var Services;
(function (Services) {
    var Classifier = (function () {
        function Classifier(host) {
            this.host = host;
            this.scanner = new TypeScript.Scanner();
        }
        Classifier.prototype.getClassificationsForLine = function (text, lexState) {
            var result = new ClassificationResult();
            result.initialState = lexState;
            this.scanner.lexState = lexState;
            this.scanner.setText(text, TypeScript.LexMode.Line);
            var t = this.scanner.scanInLine();
            while(t.tokenId != TypeScript.TokenID.EndOfFile) {
                result.entries.push(new ClassificationInfo(this.scanner.pos, t.classification()));
                t = this.scanner.scanInLine();
            }
            result.finalLexState = this.scanner.lexState;
            return result;
        };
        return Classifier;
    })();
    Services.Classifier = Classifier;    
    var ClassificationResult = (function () {
        function ClassificationResult() {
            this.initialState = TypeScript.LexState.Start;
            this.finalLexState = TypeScript.LexState.Start;
            this.entries = [];
        }
        return ClassificationResult;
    })();
    Services.ClassificationResult = ClassificationResult;    
    var ClassificationInfo = (function () {
        function ClassificationInfo(length, classification) {
            this.length = length;
            this.classification = classification;
        }
        return ClassificationInfo;
    })();
    Services.ClassificationInfo = ClassificationInfo;    
})(Services || (Services = {}));
