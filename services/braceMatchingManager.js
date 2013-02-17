var Services;
(function (Services) {
    var BraceMatchingManager = (function () {
        function BraceMatchingManager(scriptSyntaxAST) {
            this.scriptSyntaxAST = scriptSyntaxAST;
        }
        BraceMatchingManager.prototype.getBraceMatchingAtPosition = function (position) {
            var openBraces = "{([";
            var openBracesTokens = [
                TypeScript.TokenID.OpenBrace, 
                TypeScript.TokenID.OpenParen, 
                TypeScript.TokenID.OpenBracket
            ];
            var closeBraces = "})]";
            var closeBracesTokens = [
                TypeScript.TokenID.CloseBrace, 
                TypeScript.TokenID.CloseParen, 
                TypeScript.TokenID.CloseBracket
            ];
            var result = new Array();
            var character = this.scriptSyntaxAST.getSourceText().getText(position, position + 1);
            var openBraceIndex = openBraces.indexOf(character);
            if(openBraceIndex >= 0) {
                var closeBracePos = this.getMatchingBraceForward(position, openBracesTokens[openBraceIndex], closeBracesTokens[openBraceIndex]);
                if(closeBracePos >= 0) {
                    var range1 = new Services.TextRange(position, position + 1);
                    var range2 = new Services.TextRange(closeBracePos, closeBracePos + 1);
                    result.push(range1, range2);
                }
            }
            character = this.scriptSyntaxAST.getSourceText().getText(position - 1, position);
            var closeBraceIndex = closeBraces.indexOf(character);
            if(closeBraceIndex >= 0) {
                var openBracePos = this.getMatchingBraceBackward(position - 1, closeBracesTokens[closeBraceIndex], openBracesTokens[closeBraceIndex]);
                if(openBracePos >= 0) {
                    var range1 = new Services.TextRange(position - 1, position);
                    var range2 = new Services.TextRange(openBracePos, openBracePos + 1);
                    result.push(range1, range2);
                }
            }
            return result;
        };
        BraceMatchingManager.prototype.getMatchingBraceForward = function (position, openToken, closeToken) {
            var tokenStream = this.scriptSyntaxAST.getTokenStream();
            var balanceCount = 0;
            var foundOpenToken = false;
            while(tokenStream.moveNext()) {
                if(tokenStream.tokenStartPos() === position) {
                    if(tokenStream.tokenId() === openToken) {
                        foundOpenToken = true;
                    } else {
                        break;
                    }
                }
                if(foundOpenToken) {
                    if(tokenStream.tokenId() === openToken) {
                        balanceCount++;
                    } else if(tokenStream.tokenId() === closeToken) {
                        balanceCount--;
                        if(balanceCount === 0) {
                            return tokenStream.tokenStartPos();
                        }
                    }
                }
            }
            return -1;
        };
        BraceMatchingManager.prototype.getMatchingBraceBackward = function (position, closeToken, openToken) {
            var tokenStream = this.scriptSyntaxAST.getTokenStream();
            var openTokenPositions = [];
            var foundOpenToken = false;
            while(tokenStream.moveNext()) {
                if(tokenStream.tokenStartPos() > position) {
                    break;
                }
                if(tokenStream.tokenStartPos() === position && tokenStream.tokenId() === closeToken) {
                    if(openTokenPositions.length > 0) {
                        return openTokenPositions[openTokenPositions.length - 1];
                    }
                    break;
                }
                if(tokenStream.tokenId() === openToken) {
                    openTokenPositions.push(tokenStream.tokenStartPos());
                } else if(tokenStream.tokenId() === closeToken) {
                    if(openTokenPositions.length > 0) {
                        openTokenPositions.pop();
                    }
                }
            }
            return -1;
        };
        return BraceMatchingManager;
    })();
    Services.BraceMatchingManager = BraceMatchingManager;    
})(Services || (Services = {}));
