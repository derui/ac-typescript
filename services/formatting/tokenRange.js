var Formatting;
(function (Formatting) {
    (function (Shared) {
        var TokenRangeAccess = (function () {
            function TokenRangeAccess(from, to, except) {
                this.tokens = new Formatting.List_AuthorTokenKind();
                for(var token = from; token <= to; token++) {
                    if(except.indexOf(token) < 0) {
                        this.tokens.add(token);
                    }
                }
            }
            TokenRangeAccess.prototype.GetTokens = function () {
                return this.tokens;
            };
            TokenRangeAccess.prototype.Contains = function (token) {
                return this.tokens.contains(token);
            };
            TokenRangeAccess.prototype.toString = function () {
                return "[tokenRangeStart=" + (Formatting.AuthorTokenKind)._map[this.tokens.get(0)] + "," + "tokenRangeEnd=" + (Formatting.AuthorTokenKind)._map[this.tokens.get(this.tokens.count() - 1)] + "]";
            };
            return TokenRangeAccess;
        })();
        Shared.TokenRangeAccess = TokenRangeAccess;        
        var TokenValuesAccess = (function () {
            function TokenValuesAccess(tks) {
                this.tokens = new Formatting.List_AuthorTokenKind();
                this.tokens.addAll(tks);
            }
            TokenValuesAccess.prototype.GetTokens = function () {
                return this.tokens;
            };
            TokenValuesAccess.prototype.Contains = function (token) {
                return this.GetTokens().contains(token);
            };
            return TokenValuesAccess;
        })();
        Shared.TokenValuesAccess = TokenValuesAccess;        
        var TokenSingleValueAccess = (function () {
            function TokenSingleValueAccess(token) {
                this.token = token;
            }
            TokenSingleValueAccess.prototype.GetTokens = function () {
                var result = new Formatting.List_AuthorTokenKind();
                result.add(this.token);
                return result;
            };
            TokenSingleValueAccess.prototype.Contains = function (tokenValue) {
                return tokenValue == this.token;
            };
            TokenSingleValueAccess.prototype.toString = function () {
                return "[singleTokenKind=" + (Formatting.AuthorTokenKind)._map[this.token] + "]";
            };
            return TokenSingleValueAccess;
        })();
        Shared.TokenSingleValueAccess = TokenSingleValueAccess;        
        var TokenAllAccess = (function () {
            function TokenAllAccess() { }
            TokenAllAccess.prototype.GetTokens = function () {
                var result = new Formatting.List_AuthorTokenKind();
                for(var token = Formatting.AuthorTokenKind.atkEnd; token < Formatting.AuthorTokenKind.Length; token++) {
                    result.add(token);
                }
                return result;
            };
            TokenAllAccess.prototype.Contains = function (tokenValue) {
                return true;
            };
            TokenAllAccess.prototype.toString = function () {
                return "[allTokens]";
            };
            return TokenAllAccess;
        })();
        Shared.TokenAllAccess = TokenAllAccess;        
        var TokenRange = (function () {
            function TokenRange(tokenAccess) {
                this.tokenAccess = tokenAccess;
            }
            TokenRange.FromToken = function FromToken(token) {
                return new TokenRange(new TokenSingleValueAccess(token));
            };
            TokenRange.FromTokens = function FromTokens(tokens) {
                return new TokenRange(new TokenValuesAccess(tokens));
            };
            TokenRange.FromRange = function FromRange(f, to, except) {
                if (typeof except === "undefined") { except = []; }
                return new TokenRange(new TokenRangeAccess(f, to, except));
            };
            TokenRange.AllTokens = function AllTokens() {
                return new TokenRange(new TokenAllAccess());
            };
            TokenRange.prototype.GetTokens = function () {
                return this.tokenAccess.GetTokens();
            };
            TokenRange.prototype.Contains = function (token) {
                return this.tokenAccess.Contains(token);
            };
            TokenRange.prototype.toString = function () {
                return this.tokenAccess.toString();
            };
            TokenRange.Any = TokenRange.AllTokens();
            TokenRange.Keywords = TokenRange.FromRange(Formatting.AuthorTokenKind.atkBreak, Formatting.AuthorTokenKind.atkWith);
            TokenRange.Operators = TokenRange.FromRange(Formatting.AuthorTokenKind.atkSColon, Formatting.AuthorTokenKind.atkScope);
            TokenRange.BinaryOperators = TokenRange.FromRange(Formatting.AuthorTokenKind.atkArrow, Formatting.AuthorTokenKind.atkPct);
            TokenRange.BinaryKeywordOperators = TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkIn, 
                Formatting.AuthorTokenKind.atkInstanceof
            ]);
            TokenRange.ReservedKeywords = TokenRange.FromRange(Formatting.AuthorTokenKind.atkImplements, Formatting.AuthorTokenKind.atkYield);
            TokenRange.UnaryPrefixOperators = TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkAdd, 
                Formatting.AuthorTokenKind.atkSub, 
                Formatting.AuthorTokenKind.atkTilde, 
                Formatting.AuthorTokenKind.atkBang
            ]);
            TokenRange.UnaryPrefixExpressions = TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkNumber, 
                Formatting.AuthorTokenKind.atkIdentifier, 
                Formatting.AuthorTokenKind.atkLParen, 
                Formatting.AuthorTokenKind.atkLBrack, 
                Formatting.AuthorTokenKind.atkLCurly, 
                Formatting.AuthorTokenKind.atkThis, 
                Formatting.AuthorTokenKind.atkNew
            ]);
            TokenRange.UnaryPreincrementExpressions = TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkIdentifier, 
                Formatting.AuthorTokenKind.atkLParen, 
                Formatting.AuthorTokenKind.atkThis, 
                Formatting.AuthorTokenKind.atkNew
            ]);
            TokenRange.UnaryPostincrementExpressions = TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkIdentifier, 
                Formatting.AuthorTokenKind.atkRParen, 
                Formatting.AuthorTokenKind.atkRBrack, 
                Formatting.AuthorTokenKind.atkNew
            ]);
            TokenRange.UnaryPredecrementExpressions = TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkIdentifier, 
                Formatting.AuthorTokenKind.atkLParen, 
                Formatting.AuthorTokenKind.atkThis, 
                Formatting.AuthorTokenKind.atkNew
            ]);
            TokenRange.UnaryPostdecrementExpressions = TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkIdentifier, 
                Formatting.AuthorTokenKind.atkRParen, 
                Formatting.AuthorTokenKind.atkRBrack, 
                Formatting.AuthorTokenKind.atkNew
            ]);
            return TokenRange;
        })();
        Shared.TokenRange = TokenRange;        
    })(Formatting.Shared || (Formatting.Shared = {}));
    var Shared = Formatting.Shared;
})(Formatting || (Formatting = {}));
