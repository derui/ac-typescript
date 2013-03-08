var Formatting;
(function (Formatting) {
    var FormattingManager = (function () {
        function FormattingManager(scriptSyntaxAST, rulesProvider, editorOptions) {
            this.scriptSyntaxAST = scriptSyntaxAST;
            this.rulesProvider = rulesProvider;
            this.editorOptions = editorOptions;
            this.logger = new TypeScript.LoggerAdapter(this.scriptSyntaxAST.getLogger());
            this.sourceText = this.scriptSyntaxAST.getSourceText();
            this.snapshot = new Formatting.TextSnapshot(this.scriptSyntaxAST.getScript(), this.sourceText);
            this.fileAuthoringProxy = new Formatting.FileAuthoringProxy(this.scriptSyntaxAST);
            this.tokenKindMap = Formatting.AuthorTokenKindMap.getInstance();
        }
        FormattingManager.prototype.FormatSelection = function (minChar, limChar) {
            var span = new Formatting.SnapshotSpan(this.snapshot, Formatting.Span.FromBounds(minChar, limChar));
            return this.Format(span, Formatting.FormattingRequestKind.FormatSelection, function (a, b) {
                return true;
            });
        };
        FormattingManager.prototype.FormatDocument = function (minChar, limChar) {
            var span = new Formatting.SnapshotSpan(this.snapshot, Formatting.Span.FromBounds(minChar, limChar));
            return this.Format(span, Formatting.FormattingRequestKind.FormatDocument, function (a, b) {
                return true;
            });
        };
        FormattingManager.prototype.FormatOnPaste = function (minChar, limChar) {
            var span = new Formatting.SnapshotSpan(this.snapshot, Formatting.Span.FromBounds(minChar, limChar));
            return this.Format(span, Formatting.FormattingRequestKind.FormatOnPaste, function (a, b) {
                return true;
            });
        };
        FormattingManager.prototype.CanFormatSpan = function (span) {
            return true;
        };
        FormattingManager.prototype.FormatOnSemicolon = function (caretPosition) {
            var _this = this;
            var caret = new Formatting.SnapshotPoint(this.snapshot, caretPosition);
            var semicolonPoint = caret.Add(-1);
            var mappedPoint = this.MapDownSnapshotPoint(semicolonPoint);
            if(mappedPoint !== null) {
                var span = this.FindStatementSpan(mappedPoint, Formatting.FormattingRequestKind.FormatOnSemicolon);
                if(span != null) {
                    return this.Format(span, Formatting.FormattingRequestKind.FormatOnSemicolon, function (tokens, requestKind) {
                        return !_this.IsInsideStringLiteralOrComment(mappedPoint, tokens);
                    });
                }
            }
            return [];
        };
        FormattingManager.prototype.FormatOnClosingCurlyBrace = function (caretPosition) {
            var _this = this;
            var caret = new Formatting.SnapshotPoint(this.snapshot, caretPosition);
            var closeBracePoint = caret.Add(-1);
            var mappedPoint = this.MapDownSnapshotPoint(closeBracePoint);
            if(mappedPoint !== null) {
                var span = this.FindMatchingBlockSpan(mappedPoint, Formatting.FormattingRequestKind.FormatOnClosingCurlyBrace);
                if(span != null) {
                    return this.Format(span, Formatting.FormattingRequestKind.FormatOnClosingCurlyBrace, function (tokens, requestKind) {
                        return !_this.IsInsideStringLiteralOrComment(mappedPoint, tokens);
                    });
                }
            }
            return [];
        };
        FormattingManager.prototype.FormatOnEnter = function (caretPosition) {
            var _this = this;
            var lineNumber = this.snapshot.GetLineNumberFromPosition(caretPosition);
            if(lineNumber > 0) {
                var prevLine = this.snapshot.GetLineFromLineNumber(lineNumber - 1);
                var currentLine = this.snapshot.GetLineFromLineNumber(lineNumber);
                var span = new Formatting.SnapshotSpan(this.snapshot, Formatting.Span.FromBounds(prevLine.startPosition(), currentLine.endPosition()));
                if(span != null) {
                    var caret = new Formatting.SnapshotPoint(this.snapshot, caretPosition);
                    var mappedPoint = this.MapDownSnapshotPoint(caret);
                    return this.Format(span, Formatting.FormattingRequestKind.FormatOnEnter, function (tokens, requestKind) {
                        return !_this.IsInsideStringLiteralOrComment(mappedPoint, tokens);
                    });
                }
            }
            return [];
        };
        FormattingManager.prototype.FindMatchingBlockSpan = function (bracePoint, formattingRequestKind) {
            var authoringProxy = this.fileAuthoringProxy;
            var matchingBlockTask = new Formatting.MatchingBlockFinderTask(bracePoint, authoringProxy);
            var blockSpan = matchingBlockTask.Run();
            if(blockSpan !== null) {
                return new Formatting.SnapshotSpan(bracePoint.snapshot, blockSpan);
            } else {
                return null;
            }
        };
        FormattingManager.prototype.FindStatementSpan = function (semicolonPoint, formattingRequestKind) {
            var authoringProxy = this.fileAuthoringProxy;
            var statementFinderTask = new Formatting.StatementFinderTask(this.logger, semicolonPoint, authoringProxy);
            statementFinderTask.Run();
            if(statementFinderTask.BlockSpan != null) {
                return new Formatting.SnapshotSpan(semicolonPoint.snapshot, statementFinderTask.BlockSpan);
            } else {
                return null;
            }
        };
        FormattingManager.prototype.MapDownSnapshotSpan = function (snapshotSpan) {
            return snapshotSpan;
        };
        FormattingManager.prototype.MapDownSnapshotPoint = function (snapshotPoint) {
            return snapshotPoint;
        };
        FormattingManager.prototype.GetTokens = function (span) {
            return Formatting.getTokensInSpan(this.logger, this.scriptSyntaxAST, this.tokenKindMap, span);
        };
        FormattingManager.prototype.IsInsideStringLiteralOrComment = function (point, tokens) {
            if(point !== null) {
                var span = new Formatting.Span(point.position, 1);
                for(var i = 0; i < tokens.count(); i++) {
                    var token = tokens.get(i);
                    if(token.Span.OverlapsWith(span)) {
                        return token.Token == Formatting.AuthorTokenKind.atkString || token.Token == Formatting.AuthorTokenKind.atkComment;
                    }
                }
            }
            return false;
        };
        FormattingManager.prototype.Format = function (span, formattingRequestKind, prerequisiteTokenTest) {
            var _this = this;
            if(span.IsEmpty() || !this.CanFormatSpan(span)) {
                return [];
            }
            var scriptHasErrors = false;
            if(scriptHasErrors) {
                return [];
            }
            var startLinePoint = span.start().GetContainingLine().start();
            span = new Formatting.SnapshotSpan(startLinePoint.snapshot, Formatting.Span.FromBounds(startLinePoint.position, span.endPosition()));
            var tokens = TypeScript.timeFunction(this.logger, "FormattingManager: GetTokens()", function () {
                return _this.GetTokens(span);
            });
            if(prerequisiteTokenTest != null && !prerequisiteTokenTest(tokens, formattingRequestKind)) {
                return [];
            }
            var languageHostIndentation = null;
            var editorOptions = this.editorOptions;
            var formattingTask = new Formatting.FormattingTask(this.logger, this.snapshot, span, tokens, this.fileAuthoringProxy, this.rulesProvider, editorOptions, languageHostIndentation, scriptHasErrors, formattingRequestKind);
            formattingTask.Run();
            var result = [];
            formattingTask.EditCommands.foreach(function (item) {
                var edit = new Services.TextEdit(item.position, item.position + item.Length, item.replaceWith);
                result.push(edit);
            });
            return result;
        };
        return FormattingManager;
    })();
    Formatting.FormattingManager = FormattingManager;    
})(Formatting || (Formatting = {}));
