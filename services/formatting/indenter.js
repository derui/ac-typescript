var Formatting;
(function (Formatting) {
    var Indenter = (function () {
        function Indenter(logger, tree, snapshot, languageHostIndentation, editorOptions, firstToken, smartIndent) {
            this.logger = logger;
            this.tree = tree;
            this.snapshot = snapshot;
            this.languageHostIndentation = languageHostIndentation;
            this.editorOptions = editorOptions;
            this.firstToken = firstToken;
            this.smartIndent = smartIndent;
            this.indentationBag = new Formatting.IndentationBag(this.snapshot);
            this.scriptBlockBeginLineNumber = -1;
            this.offsetIndentationDeltas = new Formatting.Dictionary_int_int();
            this.tree.Root.SetIndentationOverride("");
            this.ApplyScriptBlockIndentation(this.languageHostIndentation, this.tree);
            this.FillInheritedIndentation(this.tree);
        }
        Indenter.prototype.GetIndentationEdits = function (token, nextToken, node, sameLineIndent) {
            if(this.logger.information()) {
                this.logger.log("GetIndentationEdits(" + "t1=[" + token.Span.startPosition() + "," + token.Span.endPosition() + "], " + "t2=[" + (nextToken == null ? "null" : (nextToken.Span.startPosition() + "," + nextToken.Span.endPosition())) + "]" + ")");
            }
            var result = this.GetIndentationEditsWorker(token, nextToken, node, sameLineIndent);
            if(this.logger.information()) {
                for(var i = 0; i < result.count(); i++) {
                    var edit = result.get(i);
                    this.logger.log("edit: minChar=" + edit.position + ", limChar=" + (edit.position + edit.length) + ", text=\"" + TypeScript.stringToLiteral(edit.replaceWith, 30) + "\"");
                }
            }
            return result;
        };
        Indenter.prototype.GetIndentationEditsWorker = function (token, nextToken, node, sameLineIndent) {
            var result = new Formatting.List_TextEditInfo();
            var indentationInfo = null;
            this.AdjustStartOffsetIfNeeded(token, node);
            if(this.scriptBlockBeginLineNumber == token.lineNumber()) {
                return result;
            }
            if(!sameLineIndent && this.IsMultiLineString(token)) {
                return result;
            }
            indentationInfo = this.GetSpecialCaseIndentation(token, node);
            if(indentationInfo == null) {
                while(!node.CanIndent() && node.Parent != null && token.Span.span.start() == node.Parent.AuthorNode.Details.StartOffset) {
                    node = node.Parent;
                }
                if(node.CanIndent() && token.Span.span.start() == node.AuthorNode.Details.StartOffset) {
                    indentationInfo = node.GetEffectiveIndentation(this);
                } else {
                    if(token.Token == Formatting.AuthorTokenKind.atkIdentifier && nextToken != null && nextToken.Token == Formatting.AuthorTokenKind.atkColon) {
                        indentationInfo = node.GetEffectiveChildrenIndentation(this);
                    } else {
                        indentationInfo = this.ApplyIndentationDeltaFromParent(token, node);
                    }
                }
            }
            if(indentationInfo != null) {
                var edit = this.GetIndentEdit(indentationInfo, token.Span.startPosition(), sameLineIndent);
                if(edit != null) {
                    this.RegisterIndentation(edit, sameLineIndent);
                    result.add(edit);
                    if(token.Token == Formatting.AuthorTokenKind.atkComment) {
                        var commentEdits = this.GetCommentIndentationEdits(token);
                        commentEdits.foreach(function (item) {
                            result.add(item);
                        });
                    }
                }
            }
            return result;
        };
        Indenter.prototype.GetCommentIndentationEdits = function (token) {
            var result = new Formatting.List_TextEditInfo();
            if(token.Token != Formatting.AuthorTokenKind.atkComment) {
                return result;
            }
            var commentLastLineNumber = this.snapshot.GetLineNumberFromPosition(token.Span.endPosition());
            if(token.lineNumber() == commentLastLineNumber) {
                return result;
            }
            var commentFirstLineIndentationDelta = this.GetIndentationDelta(token.Span.startPosition(), null);
            if(commentFirstLineIndentationDelta != undefined) {
                for(var line = token.lineNumber() + 1; line <= commentLastLineNumber; line++) {
                    var lineStartPosition = this.snapshot.GetLineFromLineNumber(line).startPosition();
                    var lineIndent = this.GetLineIndentationForOffset(lineStartPosition);
                    var commentIndentationInfo = this.ApplyIndentationDelta2(lineIndent, commentFirstLineIndentationDelta);
                    if(commentIndentationInfo != null) {
                        var tokenStartPosition = lineStartPosition + lineIndent.length;
                        var commentIndentationEdit = this.GetIndentEdit(commentIndentationInfo, tokenStartPosition, false);
                        if(commentIndentationEdit != null) {
                            result.add(commentIndentationEdit);
                        }
                    }
                }
            }
            return result;
        };
        Indenter.GetIndentSizeFromIndentText = function GetIndentSizeFromIndentText(indentText, editorOptions) {
            return Indenter.GetIndentSizeFromText(indentText, editorOptions, false);
        };
        Indenter.GetIndentSizeFromText = function GetIndentSizeFromText(text, editorOptions, includeNonIndentChars) {
            var indentSize = 0;
            for(var i = 0; i < text.length; i++) {
                var c = text.charAt(i);
                if(c == '\t') {
                    indentSize = (indentSize + editorOptions.TabSize) - (indentSize % editorOptions.TabSize);
                } else if(c == ' ') {
                    indentSize += 1;
                } else {
                    if(includeNonIndentChars) {
                        indentSize += 1;
                    } else {
                        break;
                    }
                }
            }
            return indentSize;
        };
        Indenter.prototype.GetSpecialCaseIndentation = function (token, node) {
            var indentationInfo = null;
            switch(token.Token) {
                case Formatting.AuthorTokenKind.atkLCurly:
                    indentationInfo = this.GetSpecialCaseIndentationForLCurly(node);
                    return indentationInfo;
                case Formatting.AuthorTokenKind.atkElse:
                case Formatting.AuthorTokenKind.atkRBrack:
                    indentationInfo = node.GetNodeStartLineIndentation(this);
                    return indentationInfo;
                case Formatting.AuthorTokenKind.atkRCurly:
                    if(node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkBlock && node.AuthorNode.EdgeLabel == Formatting.AuthorParseNodeEdge.apneBody) {
                        node = node.Parent;
                    }
                    indentationInfo = node.GetNodeStartLineIndentation(this);
                    return indentationInfo;
                case Formatting.AuthorTokenKind.atkWhile:
                    if(node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkDoWhile) {
                        indentationInfo = node.GetNodeStartLineIndentation(this);
                        return indentationInfo;
                    }
                    return null;
                case Formatting.AuthorTokenKind.atkSColon:
                    return this.GetSpecialCaseIndentationForSemicolon(token, node);
                case Formatting.AuthorTokenKind.atkComment:
                    return this.GetSpecialCaseIndentationForComment(token, node);
                default:
                    return indentationInfo;
            }
        };
        Indenter.prototype.GetSpecialCaseIndentationForLCurly = function (node) {
            var indentationInfo = null;
            if(node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkFncDecl || node.AuthorNode.EdgeLabel == Formatting.AuthorParseNodeEdge.apneThen || node.AuthorNode.EdgeLabel == Formatting.AuthorParseNodeEdge.apneElse) {
                indentationInfo = node.GetNodeStartLineIndentation(this);
                return indentationInfo;
            } else if(node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkObject && !node.CanIndent()) {
                return null;
            }
            indentationInfo = node.GetEffectiveIndentation(this);
            return indentationInfo;
        };
        Indenter.prototype.GetSpecialCaseIndentationForSemicolon = function (token, node) {
            var indentationInfo = null;
            if(this.smartIndent) {
                indentationInfo = node.GetEffectiveChildrenIndentation(this);
                return indentationInfo;
            } else {
                if(node.AuthorNode.Details.Kind != Formatting.AuthorParseNodeKind.apnkFor) {
                    var semiColonStartSpan = new Formatting.Span(token.Span.startPosition(), 0);
                    node = Formatting.ParseTree.FindCommonParentNode(semiColonStartSpan, semiColonStartSpan, node);
                    indentationInfo = node.GetEffectiveChildrenIndentation(this);
                    return indentationInfo;
                }
            }
            return null;
        };
        Indenter.prototype.GetSpecialCaseIndentationForComment = function (token, node) {
            var indentationInfo = null;
            var twoCharSpan = token.Span.Intersection(new Formatting.Span(token.Span.startPosition(), 2));
            if(twoCharSpan != null && (twoCharSpan.GetText() == "//" || twoCharSpan.GetText() == "/*")) {
                while(node.ChildrenIndentationDelta == null && node.Parent != null) {
                    node = node.Parent;
                }
                if(this.CanIndentComment(token, node)) {
                    indentationInfo = node.GetEffectiveChildrenIndentationForComment(this);
                } else {
                    indentationInfo = this.ApplyIndentationDeltaFromParent(token, node);
                }
            }
            return indentationInfo;
        };
        Indenter.prototype.CanIndentComment = function (token, node) {
            switch(node.AuthorNode.Details.Kind) {
                case Formatting.AuthorParseNodeKind.apnkProg:
                case Formatting.AuthorParseNodeKind.apnkBlock:
                case Formatting.AuthorParseNodeKind.apnkSwitch:
                case Formatting.AuthorParseNodeKind.apnkCase:
                case Formatting.AuthorParseNodeKind.apnkDefaultCase:
                case Formatting.AuthorParseNodeKind.apnkIf:
                case Formatting.AuthorParseNodeKind.apnkFor:
                case Formatting.AuthorParseNodeKind.apnkForIn:
                case Formatting.AuthorParseNodeKind.apnkWhile:
                case Formatting.AuthorParseNodeKind.apnkWith:
                case Formatting.AuthorParseNodeKind.apnkDoWhile:
                case Formatting.AuthorParseNodeKind.apnkObject:
                    return true;
                case Formatting.AuthorParseNodeKind.apnkFncDecl:
                    var result = true;
                    var children = Formatting.ParseNodeExtensions.FindChildrenWithEdge(node, Formatting.AuthorParseNodeEdge.apneArgument);
                    children.foreach(function (argumentNode) {
                        if(token.Span.startPosition() < argumentNode.AuthorNode.Details.StartOffset) {
                            result = false;
                        }
                    });
                    return result;
            }
            return false;
        };
        Indenter.prototype.ApplyScriptBlockIndentation = function (languageHostIndentation, tree) {
            if(languageHostIndentation == null || tree.StartNodeSelf == null) {
                return;
            }
            var scriptBlockIndentation = this.ApplyIndentationLevel(languageHostIndentation, 1);
            tree.Root.SetIndentationOverride(scriptBlockIndentation);
        };
        Indenter.prototype.GetIndentEdit = function (indentInfo, tokenStartPosition, sameLineIndent) {
            var indentText = this.ApplyIndentationLevel(indentInfo.Prefix, indentInfo.Level);
            if(sameLineIndent) {
                return new Formatting.TextEditInfo(tokenStartPosition, 0, indentText);
            } else {
                var snapshotLine = this.snapshot.GetLineFromPosition(tokenStartPosition);
                var currentIndentSpan = new Formatting.Span(snapshotLine.startPosition(), tokenStartPosition - snapshotLine.startPosition());
                var currentIndentText = this.snapshot.GetText(currentIndentSpan);
                if(currentIndentText !== indentText) {
                    if(this.logger.debug()) {
                        for(var i = 0, len = currentIndentText.length; i < len; i++) {
                            var c = currentIndentText.charCodeAt(i);
                            if(!Formatting.StringUtils.IsWhiteSpace(c)) {
                                Formatting.Debug.Fail("Formatting error: Will remove user code when indenting the line: " + snapshotLine.getText());
                                break;
                            }
                        }
                    }
                    return new Formatting.TextEditInfo(currentIndentSpan.start(), currentIndentSpan.length(), indentText);
                }
            }
            return null;
        };
        Indenter.prototype.ApplyIndentationLevel = function (existingIndentation, level) {
            var indentSize = this.editorOptions.IndentSize;
            var tabSize = this.editorOptions.TabSize;
            var convertTabsToSpaces = this.editorOptions.ConvertTabsToSpaces;
            if(level < 0) {
                if(Formatting.StringUtils.IsNullOrEmpty(existingIndentation)) {
                    return "";
                }
                var totalIndent = 0;
                Formatting.StringUtils.foreach(existingIndentation, function (c) {
                    if(c == '\t') {
                        totalIndent += tabSize;
                    } else {
                        totalIndent++;
                    }
                });
                totalIndent += level * indentSize;
                if(totalIndent < 0) {
                    return "";
                } else {
                    return this.GetIndentString(null, totalIndent, tabSize, convertTabsToSpaces);
                }
            }
            var totalIndentSize = level * indentSize;
            return this.GetIndentString(existingIndentation, totalIndentSize, tabSize, convertTabsToSpaces);
        };
        Indenter.prototype.GetIndentString = function (prefix, totalIndentSize, tabSize, convertTabsToSpaces) {
            var tabString = convertTabsToSpaces ? Formatting.StringUtils.create(' ', tabSize) : "\t";
            var text = "";
            if(!Formatting.StringUtils.IsNullOrEmpty(prefix)) {
                text += prefix;
            }
            var pos = 0;
            while(pos <= totalIndentSize - tabSize) {
                text += tabString;
                pos += tabSize;
            }
            while(pos < totalIndentSize) {
                text += ' ';
                pos++;
            }
            return text;
        };
        Indenter.prototype.ApplyIndentationDeltaFromParent = function (token, node) {
            var indentationInfo = null;
            var indentableParent = node;
            while(indentableParent != null && !indentableParent.CanIndent()) {
                indentableParent = indentableParent.Parent;
            }
            if(indentableParent != null && indentableParent.AuthorNode.Details.Kind != Formatting.AuthorParseNodeKind.apnkProg) {
                var parentIndentationDeltaSize = this.GetIndentationDelta(indentableParent.AuthorNode.Details.StartOffset, token.Span.startPosition());
                if(parentIndentationDeltaSize !== undefined) {
                    indentationInfo = this.ApplyIndentationDelta1(token.Span.startPosition(), parentIndentationDeltaSize);
                }
            }
            return indentationInfo;
        };
        Indenter.prototype.ApplyIndentationDelta1 = function (tokenStartPosition, delta) {
            var snapshotLine = this.snapshot.GetLineFromPosition(tokenStartPosition);
            var currentIndentSpan = new Formatting.Span(snapshotLine.startPosition(), tokenStartPosition - snapshotLine.startPosition());
            var currentIndent = this.snapshot.GetText(currentIndentSpan);
            return this.ApplyIndentationDelta2(currentIndent, delta);
        };
        Indenter.prototype.ApplyIndentationDelta2 = function (currentIndent, delta) {
            if(delta == 0) {
                return null;
            }
            var currentIndentSize = Indenter.GetIndentSizeFromIndentText(currentIndent, this.editorOptions);
            var newIndentSize = currentIndentSize + delta;
            if(newIndentSize < 0) {
                newIndentSize = 0;
            }
            var newIndent = this.GetIndentString(null, newIndentSize, this.editorOptions.TabSize, this.editorOptions.ConvertTabsToSpaces);
            if(newIndent != null) {
                return new Formatting.IndentationInfo(newIndent, 0);
            }
            return null;
        };
        Indenter.prototype.GetIndentationDelta = function (tokenStartPosition, childTokenStartPosition) {
            Formatting.Debug.Assert(childTokenStartPosition !== undefined, "Error: caller must pass 'null' for undefined position");
            var indentationDeltaSize = this.offsetIndentationDeltas.GetValue(tokenStartPosition);
            if(indentationDeltaSize === null) {
                var indentEditInfo = this.indentationBag.FindIndent(tokenStartPosition);
                if(indentEditInfo == null) {
                    return null;
                }
                var origIndentText = this.snapshot.GetText(new Formatting.Span(indentEditInfo.OrigIndentPosition, indentEditInfo.OrigIndentLength()));
                var newIndentText = indentEditInfo.Indentation();
                var origIndentSize = Indenter.GetIndentSizeFromText(origIndentText, this.editorOptions, true);
                var newIndentSize = Indenter.GetIndentSizeFromIndentText(newIndentText, this.editorOptions);
                if(childTokenStartPosition !== null) {
                    var childTokenLineStartPosition = this.snapshot.GetLineFromPosition(childTokenStartPosition).startPosition();
                    var childIndentText = this.snapshot.GetText(new Formatting.Span(childTokenLineStartPosition, childTokenStartPosition - childTokenLineStartPosition));
                    var childIndentSize = Indenter.GetIndentSizeFromIndentText(childIndentText, this.editorOptions);
                    if(childIndentSize < origIndentSize) {
                        origIndentSize = Indenter.GetIndentSizeFromIndentText(origIndentText, this.editorOptions);
                    }
                }
                indentationDeltaSize = newIndentSize - origIndentSize;
                this.offsetIndentationDeltas.Add(tokenStartPosition, indentationDeltaSize);
            }
            return indentationDeltaSize;
        };
        Indenter.prototype.FillInheritedIndentation = function (tree) {
            var offset = -1;
            var indentNode = null;
            if(tree.StartNodeSelf != null) {
                if(!this.smartIndent && tree.StartNodePreviousSibling !== null && tree.StartNodeSelf.AuthorNode.Label == 0 && tree.StartNodePreviousSibling.Label == 0) {
                    indentNode = tree.StartNodeSelf;
                    offset = tree.StartNodePreviousSibling.Details.StartOffset;
                    var lineNum = this.snapshot.GetLineNumberFromPosition(offset);
                    var node = indentNode;
                    while(node.Parent != null && this.snapshot.GetLineNumberFromPosition(node.Parent.AuthorNode.Details.StartOffset) == lineNum) {
                        node = node.Parent;
                        if(node.CanIndent()) {
                            indentNode = node;
                            indentNode.IndentationDelta = 0;
                        }
                    }
                } else {
                    var parent;
                    if(this.smartIndent) {
                        parent = tree.StartNodeSelf;
                        while(parent != null && parent.AuthorNode.Details.StartOffset == this.firstToken.Span.startPosition()) {
                            parent = parent.Parent;
                        }
                    } else {
                        var startNodeLineNumber = this.snapshot.GetLineNumberFromPosition(tree.StartNodeSelf.AuthorNode.Details.StartOffset);
                        parent = tree.StartNodeSelf.Parent;
                        while(parent != null && startNodeLineNumber == this.snapshot.GetLineNumberFromPosition(parent.AuthorNode.Details.StartOffset)) {
                            parent = parent.Parent;
                        }
                    }
                    while(parent != null && !parent.CanIndent()) {
                        parent = parent.Parent;
                    }
                    if(parent != null && parent.AuthorNode.Details.Kind != Formatting.AuthorParseNodeKind.apnkProg) {
                        offset = parent.AuthorNode.Details.StartOffset;
                        indentNode = parent;
                    }
                }
            }
            if(indentNode != null) {
                var indentOverride = this.GetLineIndentationForOffset(offset);
                if(!this.smartIndent && tree.StartNodePreviousSibling !== null && indentNode.Parent != null) {
                    Formatting.ParseNodeExtensions.GetChildren(indentNode.Parent).foreach(function (sibling) {
                        if(sibling !== indentNode) {
                            if(sibling.CanIndent()) {
                                sibling.SetIndentationOverride(indentOverride);
                            }
                        }
                    });
                }
                var lastDelta = 0;
                var lastLine = this.snapshot.GetLineNumberFromPosition(indentNode.AuthorNode.Details.StartOffset);
                do {
                    var currentLine = this.snapshot.GetLineNumberFromPosition(indentNode.AuthorNode.Details.StartOffset);
                    if(lastLine != currentLine) {
                        lastLine = currentLine;
                        indentOverride = this.ApplyIndentationLevel(indentOverride, -lastDelta);
                        lastDelta = 0;
                    }
                    if(indentNode.CanIndent()) {
                        indentNode.SetIndentationOverride(indentOverride);
                        lastDelta = indentNode.IndentationDelta;
                    }
                    indentNode = indentNode.Parent;
                }while(indentNode != null);
            }
        };
        Indenter.prototype.GetLineIndentationForOffset = function (offset) {
            var indentationEdit;
            indentationEdit = this.indentationBag.FindIndent(offset);
            if(indentationEdit != null) {
                return indentationEdit.Indentation();
            } else {
                var line = this.snapshot.GetLineFromPosition(offset);
                var lineText = line.getText();
                var index = 0;
                while(index < lineText.length && (lineText.charAt(index) == ' ' || lineText.charAt(index) == '\t')) {
                    ++index;
                }
                return lineText.substr(0, index);
            }
        };
        Indenter.prototype.RegisterIndentation = function (indent, sameLineIndent) {
            var indentationInfo = null;
            if(sameLineIndent) {
                var lineStartPosition = this.snapshot.GetLineFromPosition(indent.Position).startPosition();
                var lineIndentLength = indent.Position - lineStartPosition;
                indentationInfo = Formatting.IndentationEditInfo.create2(indent.Position, indent.ReplaceWith, lineStartPosition, lineIndentLength);
            } else {
                indentationInfo = new Formatting.IndentationEditInfo(indent);
            }
            this.indentationBag.AddIndent(indentationInfo);
        };
        Indenter.prototype.RegisterIndentation2 = function (position, indent) {
            this.RegisterIndentation(new Formatting.TextEditInfo(position, 0, indent), false);
        };
        Indenter.prototype.AdjustStartOffsetIfNeeded = function (token, node) {
            if(token == null) {
                return;
            }
            var updateStartOffset = false;
            switch(token.Token) {
                case Formatting.AuthorTokenKind.atkFunction:
                    updateStartOffset = node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkFncDecl;
                    break;
                case Formatting.AuthorTokenKind.atkLCurly:
                    updateStartOffset = node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkObject;
                    break;
                case Formatting.AuthorTokenKind.atkLBrack:
                    updateStartOffset = node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkArray;
                    break;
            }
            if(updateStartOffset) {
                Formatting.ParseNodeExtensions.SetNodeSpan(node, token.Span.startPosition(), node.AuthorNode.Details.EndOffset);
            }
        };
        Indenter.prototype.IsMultiLineString = function (token) {
            return token.tokenID === TypeScript.TokenID.StringLiteral && this.snapshot.GetLineNumberFromPosition(token.Span.endPosition()) > this.snapshot.GetLineNumberFromPosition(token.Span.startPosition());
        };
        return Indenter;
    })();
    Formatting.Indenter = Indenter;    
})(Formatting || (Formatting = {}));
