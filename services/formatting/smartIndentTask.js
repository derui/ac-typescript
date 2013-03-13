var Formatting;
(function (Formatting) {
    var SmartIndentTask = (function () {
        function SmartIndentTask(logger, snapshotSpan, tokens, fileAuthoringProxy, editorOptions, languageHostIndentation) {
            this.logger = logger;
            this.snapshotSpan = snapshotSpan;
            this.tokens = tokens;
            this.fileAuthoringProxy = fileAuthoringProxy;
            this.editorOptions = editorOptions;
            this.languageHostIndentation = languageHostIndentation;
            this.snapshot = this.snapshotSpan.snapshot;
            this.DesiredIndentation = null;
        }
        SmartIndentTask.prototype.Run = function () {
            var _this = this;
            var tree = TypeScript.timeFunction(this.logger, "SmartIndentTask: new ParseTree()", function () {
                return new Formatting.ParseTree(_this.fileAuthoringProxy, _this.snapshotSpan.span, null, true);
            });
            if(tree.Root == null) {
                return;
            }
            Formatting.IndentationEdgeFinder.FillIndentationLevels(tree.Root);
            Formatting.ParseTree.DumpTree(this.logger, tree.Root);
            this.FindIndentation(tree);
        };
        SmartIndentTask.prototype.FindIndentation = function (tree) {
            var caretSpan = this.snapshotSpan.span;
            var context = Formatting.ParseTree.FindCommonParentNode(caretSpan, caretSpan, tree.Root);
            if(context && context.AuthorNode.Details.nodeType == TypeScript.NodeType.QString) {
                var nodeSpan = Formatting.Span.FromBounds(context.AuthorNode.Details.StartOffset, context.AuthorNode.Details.EndOffset);
                if(nodeSpan.Contains(caretSpan)) {
                    this.DesiredIndentation = "";
                    return;
                }
            }
            while(context != null && context.AuthorNode.Details.StartOffset == caretSpan.start() && !(context.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkBlock && context.AuthorNode.Details.Flags != Formatting.AuthorParseNodeFlags.apnfSyntheticNode)) {
                context = context.Parent;
            }
            if(context == null) {
                return;
            }
            var firstToken = Formatting.FirstOrDefault(this.tokens, function (item) {
                return true;
            });
            if(firstToken == null) {
                firstToken = new Formatting.TokenSpan(Formatting.AuthorTokenKind.atkSColon, TypeScript.TokenID.Semicolon, this.snapshotSpan);
            } else if(firstToken.Token != Formatting.AuthorTokenKind.atkElse && firstToken.Token != Formatting.AuthorTokenKind.atkWhile && firstToken.Token != Formatting.AuthorTokenKind.atkLCurly && firstToken.Token != Formatting.AuthorTokenKind.atkRCurly) {
                firstToken = new Formatting.TokenSpan(Formatting.AuthorTokenKind.atkSColon, TypeScript.TokenID.Semicolon, this.snapshotSpan);
            }
            if(this.CanDoSmartIndent(context, {
                token: firstToken
            })) {
                var indenter = new Formatting.Indenter(this.logger, tree, this.snapshot, this.languageHostIndentation, this.editorOptions, firstToken, true);
                var edit = Formatting.FirstOrDefault(indenter.GetIndentationEdits(firstToken, null, context, true), function (item) {
                    return true;
                });
                if(edit != null) {
                    this.DesiredIndentation = edit.ReplaceWith;
                }
            }
        };
        SmartIndentTask.prototype.CanDoSmartIndent = function (context, firstToken) {
            var node = context;
            while(node != null && node.ChildrenIndentationDelta == null) {
                node = node.Parent;
            }
            if(node == null) {
                return false;
            }
            if(Formatting.Rules.IsTypeScriptDeclWithBlockContextNode(node)) {
                return this.CanDoSmartIndentInStatementWithBlock(node, firstToken);
            }
            if(node.AuthorNode.Details.ast != null) {
                switch(node.AuthorNode.Details.ast.nodeType) {
                    case TypeScript.NodeType.ImportDeclaration:
                        return this.CanDoSmartIndentInStatement(node);
                }
            }
            switch(node.AuthorNode.Details.Kind) {
                case Formatting.AuthorParseNodeKind.apnkAsg:
                case Formatting.AuthorParseNodeKind.apnkAsgAdd:
                case Formatting.AuthorParseNodeKind.apnkAsgSub:
                case Formatting.AuthorParseNodeKind.apnkAsgMul:
                case Formatting.AuthorParseNodeKind.apnkAsgDiv:
                case Formatting.AuthorParseNodeKind.apnkAsgMod:
                case Formatting.AuthorParseNodeKind.apnkAsgAnd:
                case Formatting.AuthorParseNodeKind.apnkAsgXor:
                case Formatting.AuthorParseNodeKind.apnkAsgOr:
                case Formatting.AuthorParseNodeKind.apnkAsgLsh:
                case Formatting.AuthorParseNodeKind.apnkAsgRsh:
                case Formatting.AuthorParseNodeKind.apnkAsgRs2:
                case Formatting.AuthorParseNodeKind.apnkVarDecl:
                case Formatting.AuthorParseNodeKind.apnkVarDeclList:
                case Formatting.AuthorParseNodeKind.apnkCall:
                case Formatting.AuthorParseNodeKind.apnkArray:
                case Formatting.AuthorParseNodeKind.apnkMember:
                    return this.CanDoSmartIndentInStatement(node);
                case Formatting.AuthorParseNodeKind.apnkFor:
                    return this.CanDoSmartIndentInFor(node);
                case Formatting.AuthorParseNodeKind.apnkFncDecl:
                    return this.CanDoSmartIndentInFunction(node, firstToken);
                case Formatting.AuthorParseNodeKind.apnkTry:
                case Formatting.AuthorParseNodeKind.apnkFinally:
                    return this.CanDoSmartIndentInStatementWithBlock(node, firstToken);
                case Formatting.AuthorParseNodeKind.apnkCatch:
                case Formatting.AuthorParseNodeKind.apnkSwitch:
                    return this.CanDoSmartIndentInStatementWithParenAndBlock(node, firstToken);
                default:
                    return true;
            }
        };
        SmartIndentTask.prototype.CanDoSmartIndentInStatement = function (node) {
            var contextLine = this.snapshot.GetLineNumberFromPosition(node.AuthorNode.Details.StartOffset);
            var newLine = this.snapshot.GetLineNumberFromPosition(this.snapshotSpan.startPosition());
            return SmartIndentTask.IsEmptyRegion(this.snapshot, contextLine + 1, newLine - 1);
        };
        SmartIndentTask.prototype.CanDoSmartIndentInFunction = function (node, firstToken) {
            var astCursor = this.fileAuthoringProxy.GetASTCursor();
 {
                astCursor.SeekToOffset(node.AuthorNode.Details.StartOffset, false);
                var rightParenPos = astCursor.GetNodeProperty(Formatting.AuthorParseNodeProperty.apnpRParenMin);
                if(rightParenPos == 0 || this.snapshotSpan.startPosition() <= rightParenPos) {
                    return this.CanDoSmartIndentInStatement(node);
                }
                var leftCurlyPos = astCursor.GetNodeProperty(Formatting.AuthorParseNodeProperty.apnpLCurlyMin);
                if(leftCurlyPos == 0 || this.snapshotSpan.startPosition() <= leftCurlyPos) {
                    firstToken = {
                        token: new Formatting.TokenSpan(Formatting.AuthorTokenKind.atkLCurly, TypeScript.TokenID.OpenBrace, this.snapshotSpan)
                    };
                }
                return true;
            }
        };
        SmartIndentTask.prototype.CanDoSmartIndentInStatementWithBlock = function (node, firstToken) {
            var astCursor = this.fileAuthoringProxy.GetASTCursor();
 {
                astCursor.SeekToOffset(node.AuthorNode.Details.StartOffset, false);
                var leftCurlyPos = astCursor.GetNodeProperty(Formatting.AuthorParseNodeProperty.apnpLCurlyMin);
                if(leftCurlyPos == 0 || this.snapshotSpan.startPosition() <= leftCurlyPos) {
                    firstToken = {
                        token: new Formatting.TokenSpan(Formatting.AuthorTokenKind.atkLCurly, TypeScript.TokenID.OpenBrace, this.snapshotSpan)
                    };
                }
                return true;
            }
        };
        SmartIndentTask.prototype.CanDoSmartIndentInStatementWithParenAndBlock = function (node, firstToken) {
            var astCursor = this.fileAuthoringProxy.GetASTCursor();
 {
                astCursor.SeekToOffset(node.AuthorNode.Details.StartOffset, false);
                var rightParenPos = astCursor.GetNodeProperty(Formatting.AuthorParseNodeProperty.apnpRParenMin);
                var leftCurlyPos = astCursor.GetNodeProperty(Formatting.AuthorParseNodeProperty.apnpLCurlyMin);
                if(rightParenPos > 0 && this.snapshotSpan.startPosition() > rightParenPos && (leftCurlyPos == 0 || this.snapshotSpan.startPosition() <= leftCurlyPos)) {
                    firstToken = {
                        token: new Formatting.TokenSpan(Formatting.AuthorTokenKind.atkLCurly, TypeScript.TokenID.OpenBrace, this.snapshotSpan)
                    };
                }
                return true;
            }
        };
        SmartIndentTask.prototype.CanDoSmartIndentInFor = function (node) {
            var astCursor = this.fileAuthoringProxy.GetASTCursor();
 {
                astCursor.SeekToOffset(node.AuthorNode.Details.StartOffset, false);
                var rightParenPos = astCursor.GetNodeProperty(Formatting.AuthorParseNodeProperty.apnpRParenMin);
                if(rightParenPos == 0 || this.snapshotSpan.startPosition() <= rightParenPos) {
                    return this.CanDoSmartIndentInStatement(node);
                }
                return true;
            }
        };
        SmartIndentTask.IsEmptyRegion = function IsEmptyRegion(snapshot, startLine, endLine) {
            var empty = true;
            var lineNum = startLine;
            while(lineNum <= endLine) {
                var line = snapshot.GetLineFromLineNumber(lineNum);
                var lineText = line.getText();
                if(!SmartIndentTask.IsEmptyString(lineText)) {
                    empty = false;
                    break;
                }
                lineNum++;
            }
            return empty;
        };
        SmartIndentTask.IsEmptyString = function IsEmptyString(lineText) {
            for(var i = 0, len = lineText.length; i < len; i++) {
                if(!Formatting.EditorUtilities.IsWhitespace(lineText.charCodeAt(i))) {
                    return false;
                }
            }
            return true;
        };
        return SmartIndentTask;
    })();
    Formatting.SmartIndentTask = SmartIndentTask;    
})(Formatting || (Formatting = {}));
