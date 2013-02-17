var Formatting;
(function (Formatting) {
    var FormattingContext = (function () {
        function FormattingContext(fileAuthoringProxy, snapshot, tokens, formattingRequestKind) {
            this.fileAuthoringProxy = fileAuthoringProxy;
            this.snapshot = snapshot;
            this.tokens = tokens;
            this.formattingRequestKind = formattingRequestKind;
            this.contextNode = null;
            this.tokenSpan = null;
            this.nextTokenSpan = null;
            this.contextNodeAllOnSameLine = undefined;
            this.tokensAreOnSameLine = undefined;
            this.tokensAreSiblingNodesOnSameLine = undefined;
            Formatting.Debug.Assert(this.snapshot != null, "snapshot is null");
            Formatting.Debug.Assert(this.tokens != null, "tokens is null");
        }
        FormattingContext.prototype.findTokenAtPosition = function (position) {
            var index = Formatting.BinarySearch(this.tokens, position, function (position, tokenSpan) {
                if(position < tokenSpan.Span.startPosition()) {
                    return -1;
                } else if(position < tokenSpan.Span.endPosition()) {
                    return 0;
                } else {
                    return 1;
                }
            });
            if(index < 0) {
                return null;
            } else {
                return this.tokens.get(index);
            }
        };
        FormattingContext.prototype.setContext = function (node, t1, t2) {
            Formatting.Debug.Assert(node != null, "node is null");
            Formatting.Debug.Assert(t1 != null, "t1 is null");
            Formatting.Debug.Assert(t2 != null, "t2 is null");
            this.contextNode = node;
            this.tokenSpan = t1;
            this.nextTokenSpan = t2;
            this.contextNodeAllOnSameLine = undefined;
            this.tokensAreOnSameLine = undefined;
            this.tokensAreSiblingNodesOnSameLine = undefined;
        };
        FormattingContext.prototype.ContextNodeAllOnSameLine = function () {
            if(this.contextNodeAllOnSameLine === undefined) {
                var blockSpan = this.contextNode.GetBlockSpan(this.fileAuthoringProxy, this.tokens);
                var openToken = this.findTokenAtPosition(blockSpan.start());
                if(openToken != null && openToken.tokenID == TypeScript.TokenID.OpenBrace) {
                    var closeToken = this.findTokenAtPosition(blockSpan.end() - 1);
                    if(closeToken == null || closeToken.tokenID != TypeScript.TokenID.CloseBrace) {
                        for(var position = blockSpan.end() - 2; position > openToken.Span.endPosition(); position--) {
                            closeToken = this.findTokenAtPosition(position);
                            if(closeToken != null) {
                                blockSpan = Formatting.Span.FromBounds(openToken.Span.startPosition(), closeToken.Span.endPosition());
                                break;
                            }
                        }
                    }
                }
                var startLine = this.snapshot.GetLineNumberFromPosition(blockSpan.start());
                var endLine = this.snapshot.GetLineNumberFromPosition(blockSpan.end());
                this.contextNodeAllOnSameLine = (startLine == endLine);
            }
            return this.contextNodeAllOnSameLine;
        };
        FormattingContext.prototype.TokensAreOnSameLine = function () {
            if(this.tokensAreOnSameLine === undefined) {
                var startLine = this.tokenSpan.lineNumber();
                var endLine = this.nextTokenSpan.lineNumber();
                this.tokensAreOnSameLine = (startLine == endLine);
            }
            return this.tokensAreOnSameLine;
        };
        FormattingContext.prototype.TokensAreSiblingNodesOnSameLine = function () {
            if(this.tokensAreSiblingNodesOnSameLine === undefined) {
                this.tokensAreSiblingNodesOnSameLine = this.AreTokensSiblingNodesOnSameLine();
            }
            return this.tokensAreSiblingNodesOnSameLine;
        };
        FormattingContext.prototype.AreTokensSiblingNodesOnSameLine = function () {
            if(this.contextNode.children() == null || this.contextNode.children().count() < 2) {
                return false;
            }
            var current = null;
            var sibling = null;
            var nodeIndex = Formatting.ParseNodeExtensions.TryFindNodeIndexForStartOffset(this.contextNode.children(), this.nextTokenSpan.Span.startPosition());
            if(nodeIndex < 0) {
                return false;
            }
            sibling = this.contextNode.children().get(nodeIndex);
            for(var i = nodeIndex - 1; i >= 0; --i) {
                var child = this.contextNode.children().get(i);
                if(child.AuthorNode.Details.EndOffset == this.tokenSpan.Span.endPosition()) {
                    current = child;
                    break;
                }
            }
            if(current == null) {
                return false;
            }
            var startLine = this.snapshot.GetLineNumberFromPosition(current.AuthorNode.Details.EndOffset);
            var endLine = this.snapshot.GetLineNumberFromPosition(sibling.AuthorNode.Details.StartOffset);
            return startLine == endLine;
        };
        return FormattingContext;
    })();
    Formatting.FormattingContext = FormattingContext;    
})(Formatting || (Formatting = {}));
