var Formatting;
(function (Formatting) {
    var TokenSpan = (function () {
        function TokenSpan(Token, tokenID, Span) {
            this.Token = Token;
            this.tokenID = tokenID;
            this.Span = Span;
            this._lineNumber = null;
        }
        TokenSpan.prototype.lineNumber = function () {
            if(this._lineNumber === null) {
                this._lineNumber = this.Span.snapshot.GetLineNumberFromPosition(this.Span.startPosition());
            }
            return this._lineNumber;
        };
        TokenSpan.prototype.toString = function () {
            var result = "[tokenKind=" + (Formatting.AuthorTokenKind)._map[this.Token] + ", " + "tokenID=" + (TypeScript.TokenID)._map[this.tokenID] + ", " + "lineNumber=" + this._lineNumber + ", " + "span=" + this.Span + "]";
            return result;
        };
        return TokenSpan;
    })();
    Formatting.TokenSpan = TokenSpan;    
})(Formatting || (Formatting = {}));
