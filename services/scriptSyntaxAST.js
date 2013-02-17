var Services;
(function (Services) {
    var ScriptSyntaxAST = (function () {
        function ScriptSyntaxAST(logger, script, sourceText) {
            this.logger = logger;
            this.script = script;
            this.sourceText = sourceText;
        }
        ScriptSyntaxAST.prototype.getLogger = function () {
            return this.logger;
        };
        ScriptSyntaxAST.prototype.getScriptId = function () {
            return this.script.locationInfo.filename;
        };
        ScriptSyntaxAST.prototype.getScript = function () {
            return this.script;
        };
        ScriptSyntaxAST.prototype.getSourceText = function () {
            return this.sourceText;
        };
        ScriptSyntaxAST.prototype.getTokenStream = function (minChar, limChar) {
            if (typeof minChar === "undefined") { minChar = 0; }
            if(minChar > 0) {
                minChar = this.getTokenizationOffset(minChar);
            }
            if(!limChar) {
                limChar = this.getSourceText().getLength();
            }
            var scannerSourceText = this.getSourceText();
            if(minChar > 0 || limChar < scannerSourceText.getLength()) {
                scannerSourceText = new Services.SourceTextRange(scannerSourceText, minChar, limChar);
            }
            var scanner = new TypeScript.Scanner();
            scanner.resetComments();
            scanner.setSourceText(scannerSourceText, TypeScript.LexMode.File);
            scanner.setScanComments(true);
            var tokenStream = new TokenStream(scanner, minChar);
            return tokenStream;
        };
        ScriptSyntaxAST.prototype.getTokenizationOffset = function (position) {
            return TypeScript.getTokenizationOffset(this.script, position);
        };
        ScriptSyntaxAST.prototype.getAstPathToPosition = function (pos, options) {
            if (typeof options === "undefined") { options = TypeScript.GetAstPathOptions.Default; }
            return TypeScript.getAstPathToPosition(this.script, pos, options);
        };
        return ScriptSyntaxAST;
    })();
    Services.ScriptSyntaxAST = ScriptSyntaxAST;    
    var TokenStream = (function () {
        function TokenStream(scanner, offset) {
            this.scanner = scanner;
            this.offset = offset;
            this.currentToken = null;
        }
        TokenStream.prototype.moveNext = function () {
            this.currentToken = this.scanner.scan();
            if(this.currentToken.tokenId === TypeScript.TokenID.EndOfFile) {
                return false;
            }
            return true;
        };
        TokenStream.prototype.sourceTextOffset = function () {
            return this.offset;
        };
        TokenStream.prototype.tokenId = function () {
            return this.currentToken.tokenId;
        };
        TokenStream.prototype.tokenStartPos = function () {
            return this.offset + this.scanner.startPos;
        };
        TokenStream.prototype.tokenEndPos = function () {
            return this.offset + this.scanner.pos;
        };
        return TokenStream;
    })();
    Services.TokenStream = TokenStream;    
    var TokenStreamHelper = (function () {
        function TokenStreamHelper(stream) {
            this.stream = stream;
            this.moveNext();
        }
        TokenStreamHelper.prototype.moveNext = function () {
            do {
                if(!this.stream.moveNext()) {
                    return false;
                }
            }while(this.tokenId() === TypeScript.TokenID.Comment);
            return true;
        };
        TokenStreamHelper.prototype.expect = function (token) {
            if(this.stream.tokenId() === token) {
                this.moveNext();
                return true;
            }
            return false;
        };
        TokenStreamHelper.prototype.skipToOffset = function (pos) {
            while(this.tokenStartPos() < pos) {
                if(!this.moveNext()) {
                    return false;
                }
            }
            return true;
        };
        TokenStreamHelper.prototype.tokenId = function () {
            return this.stream.tokenId();
        };
        TokenStreamHelper.prototype.tokenStartPos = function () {
            return this.stream.tokenStartPos();
        };
        TokenStreamHelper.prototype.tokenEndPos = function () {
            return this.stream.tokenEndPos();
        };
        return TokenStreamHelper;
    })();
    Services.TokenStreamHelper = TokenStreamHelper;    
})(Services || (Services = {}));
