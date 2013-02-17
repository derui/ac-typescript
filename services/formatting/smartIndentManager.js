var Formatting;
(function (Formatting) {
    var SmartIndentManager = (function () {
        function SmartIndentManager(scriptSyntaxAST, editorOptions) {
            this.scriptSyntaxAST = scriptSyntaxAST;
            this.editorOptions = editorOptions;
            this.logger = new TypeScript.LoggerAdapter(this.scriptSyntaxAST.getLogger());
            this.sourceText = this.scriptSyntaxAST.getSourceText();
            this.snapshot = new Formatting.TextSnapshot(this.scriptSyntaxAST.getScript(), this.sourceText);
            this.fileAuthoringProxy = new Formatting.FileAuthoringProxy(this.scriptSyntaxAST);
            this.tokenKindMap = Formatting.AuthorTokenKindMap.getInstance();
        }
        SmartIndentManager.prototype.getSmartIndentAtLineNumber = function (lineNumber) {
            var line = this.snapshot.GetLineFromLineNumber(lineNumber);
            var caretPosition = new Formatting.SnapshotSpan(this.snapshot, new Formatting.Span(line.startPosition(), 0));
            var tokensSpan = this.getPossibleTokenSpan(caretPosition);
            var tokens = this.gtTokens(tokensSpan);
            var languageHostIndentation = null;
            var task = new Formatting.SmartIndentTask(this.logger, caretPosition, tokens, this.fileAuthoringProxy, this.editorOptions, languageHostIndentation);
            task.Run();
            if(task.DesiredIndentation === null) {
                return this.getBlockIndent(line);
            } else {
                return Formatting.Indenter.GetIndentSizeFromIndentText(task.DesiredIndentation, this.editorOptions);
            }
        };
        SmartIndentManager.prototype.getPossibleTokenSpan = function (caretPosition) {
            var startPosition = caretPosition.start().GetContainingLine().startPosition();
            var endPosition = caretPosition.start().GetContainingLine().endPosition();
            endPosition = Formatting.Math.Min(endPosition, startPosition + 100);
            return new Formatting.SnapshotSpan(caretPosition.snapshot, Formatting.Span.FromBounds(startPosition, endPosition));
        };
        SmartIndentManager.prototype.gtTokens = function (span) {
            return Formatting.getTokensInSpan(this.logger, this.scriptSyntaxAST, this.tokenKindMap, span);
        };
        SmartIndentManager.prototype.getBlockIndent = function (line) {
            var previousLine = null;
            for(var lineNumber = line.lineNumber() - 1; lineNumber >= 0; --lineNumber) {
                previousLine = line.snapshot().GetLineFromLineNumber(lineNumber);
                var text = previousLine.getText();
                if(text.length > 0) {
                    return Formatting.Indenter.GetIndentSizeFromIndentText(text, this.editorOptions);
                }
            }
            return null;
        };
        return SmartIndentManager;
    })();
    Formatting.SmartIndentManager = SmartIndentManager;    
})(Formatting || (Formatting = {}));
