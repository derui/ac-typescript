var Formatting;
(function (Formatting) {
    var ParseNode = (function () {
        function ParseNode() {
            this._children = null;
            this.blockSpan = null;
            this.indentationOverride = null;
            this.Parent = null;
            this.AuthorNode = null;
            this.IsIndentationOverrideEdge = false;
            this.IndentationDelta = null;
            this.ChildrenIndentationDelta = null;
            this.TokenTagIndex = null;
        }
        ParseNode.prototype.children = function () {
            return this._children;
        };
        ParseNode.prototype.addChildNode = function (node) {
            if(this._children === null) {
                this._children = new Formatting.List_ParseNode();
            }
            var count = this._children.count();
            if(count === 0) {
                this._children.add(node);
            } else {
                var startOffset = node.AuthorNode.Details.StartOffset;
                var maxOffset = this._children.get(count - 1).AuthorNode.Details.StartOffset;
                if(startOffset >= maxOffset) {
                    this._children.add(node);
                } else {
                    var pivot = Formatting.ParseNodeExtensions.findNodeInsertionPivot(this._children, node.AuthorNode.Details.StartOffset);
                    if(pivot < 0) {
                        this._children.insert(~pivot, node);
                    } else {
                        this._children.insert(pivot + 1, node);
                    }
                }
            }
        };
        ParseNode.prototype.CanIndent = function () {
 {
                return this.IndentationDelta != null;
            }
        };
        ParseNode.prototype.CoverSpan = function (span) {
            var details = this.AuthorNode.Details;
            return span.start() >= details.StartOffset && span.end() <= details.EndOffset;
        };
        ParseNode.prototype.SetIndentationOverride = function (newIndentationOverride) {
            this.indentationOverride = newIndentationOverride;
        };
        ParseNode.prototype.GetNodeStartLineIndentation = function (indentResolver) {
            var node = this;
            var prefix = null;
            while(node != null && !node.CanIndent() && !node.IsIndentationOverrideEdge) {
                node = node.Parent;
            }
            if(node != null) {
                if(node.indentationOverride == null) {
                    node.indentationOverride = indentResolver.GetLineIndentationForOffset(node.AuthorNode.Details.StartOffset);
                }
                prefix = node.indentationOverride;
            }
            return new Formatting.IndentationInfo(prefix, 0);
        };
        ParseNode.prototype.GetEffectiveIndentation = function (indentResolver) {
            var node = this;
            var prefix = null;
            var level = 0;
            while(node != null && !node.CanIndent() && !node.IsIndentationOverrideEdge) {
                node = node.Parent;
            }
            if(node != null) {
                if(node.indentationOverride != null) {
                    prefix = node.indentationOverride;
                } else {
                    if(node.CanIndent()) {
                        level = node.IndentationDelta;
                        if(!!node.AuthorNode.Label) {
                            level++;
                        }
                        node = node.Parent;
                        while(node != null) {
                            if(node.indentationOverride != null) {
                                prefix = node.indentationOverride;
                                break;
                            }
                            if(node.CanIndent() || node.IsIndentationOverrideEdge) {
                                node.indentationOverride = indentResolver.GetLineIndentationForOffset(node.AuthorNode.Details.StartOffset);
                                prefix = node.indentationOverride;
                                break;
                            }
                            node = node.Parent;
                        }
                    } else if(node.IsIndentationOverrideEdge) {
                        node.indentationOverride = indentResolver.GetLineIndentationForOffset(node.AuthorNode.Details.StartOffset);
                        prefix = node.indentationOverride;
                    }
                }
            }
            return new Formatting.IndentationInfo(prefix, level);
        };
        ParseNode.prototype.GetEffectiveChildrenIndentation = function (indentResolver) {
            var node = this;
            var indentation = null;
            while(node.ChildrenIndentationDelta == null && node.Parent != null) {
                node = node.Parent;
            }
            if(node.ChildrenIndentationDelta != null) {
                indentation = node.GetEffectiveIndentation(indentResolver);
                indentation.Level += node.ChildrenIndentationDelta;
            }
            return indentation;
        };
        ParseNode.prototype.GetEffectiveChildrenIndentationForComment = function (indentResolver) {
            var node = this;
            var indentation = null;
            while(node.Parent != null && (node.ChildrenIndentationDelta == null || node.IndentationDelta == null) && !ParseNode.IsNonIndentableException(node)) {
                node = node.Parent;
            }
            if(node.ChildrenIndentationDelta != null) {
                indentation = new Formatting.IndentationInfo();
                indentation.Level = node.ChildrenIndentationDelta;
                if(this.AuthorNode.Details.Kind != Formatting.AuthorParseNodeKind.apnkProg) {
                    indentation.Prefix = indentResolver.GetLineIndentationForOffset(node.AuthorNode.Details.StartOffset);
                }
            }
            return indentation;
        };
        ParseNode.IsNonIndentableException = function IsNonIndentableException(node) {
            return node.IndentationDelta == null && (node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkObject || node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkFncDecl);
        };
        ParseNode.prototype.GetBlockSpan = function (fileAuthoringProxy, tokens) {
            if(this.blockSpan != null) {
                return this.blockSpan;
            }
            var start = this.AuthorNode.Details.StartOffset;
            var end = this.AuthorNode.Details.EndOffset;
            var implicitBlockNode = null;
            if(this.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkFncDecl || this.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkSwitch) {
                implicitBlockNode = this;
            } else if(this.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkList && this.Parent != null && this.Parent.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkFncDecl) {
                implicitBlockNode = this.Parent;
            }
            if(implicitBlockNode != null) {
                if(implicitBlockNode.TokenTagIndex != null) {
                    var functionTokenIndex = implicitBlockNode.TokenTagIndex;
                    for(var i = functionTokenIndex + 1; i < tokens.count(); i++) {
                        if(tokens.get(i).Token == Formatting.AuthorTokenKind.atkLCurly && tokens.get(i).Span.startPosition() <= end) {
                            start = tokens.get(i).Span.startPosition();
                            break;
                        }
                    }
                } else {
                    var astCursor = fileAuthoringProxy.GetASTCursor();
 {
                        astCursor.MoveToEnclosingNode(implicitBlockNode.AuthorNode.Details.StartOffset, implicitBlockNode.AuthorNode.Details.EndOffset);
                        var leftCurlyPos = astCursor.GetNodeProperty(Formatting.AuthorParseNodeProperty.apnpLCurlyMin);
                        if(leftCurlyPos != 0 && leftCurlyPos <= end) {
                            start = leftCurlyPos;
                        }
                    }
                }
            } else if(this.AuthorNode.Details.Kind != Formatting.AuthorParseNodeKind.apnkBlock) {
                var found = false;
                Formatting.ParseNodeExtensions.GetChildren(this).foreach(function (child) {
                    if(child.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkBlock) {
                        if(!found) {
                            found = true;
                            start = child.AuthorNode.Details.StartOffset;
                            end = child.AuthorNode.Details.EndOffset;
                        }
                    }
                });
            }
            Formatting.Debug.Assert(start <= end, "Expecting start to be before end.");
            this.blockSpan = new Formatting.Span(start, end - start);
            return this.blockSpan;
        };
        ParseNode.prototype.toString = function () {
            var text = this.AuthorNode.Level + ": " + (Formatting.AuthorParseNodeKind)._map[this.AuthorNode.Details.Kind] + " - " + (TypeScript.NodeType)._map[this.AuthorNode.Details.nodeType] + " (" + (Formatting.AuthorParseNodeEdge)._map[this.AuthorNode.EdgeLabel] + ") -- I:" + this.IndentationDelta + ",IC:" + this.ChildrenIndentationDelta + " -- (" + this.AuthorNode.Details.StartOffset + "," + this.AuthorNode.Details.EndOffset + ") -- F:(" + this.AuthorNode.Details.Flags + ")";
            return text;
        };
        return ParseNode;
    })();
    Formatting.ParseNode = ParseNode;    
})(Formatting || (Formatting = {}));
