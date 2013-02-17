var Formatting;
(function (Formatting) {
    var FormattingTask = (function () {
        function FormattingTask(logger, Snapshot, span, tokens, fileAuthoringProxy, rulesProvider, editorOptions, languageHostIndentation, scriptHasErrors, formattingRequestKind) {
            this.logger = logger;
            this.Snapshot = Snapshot;
            this.span = span;
            this.tokens = tokens;
            this.fileAuthoringProxy = fileAuthoringProxy;
            this.rulesProvider = rulesProvider;
            this.editorOptions = editorOptions;
            this.languageHostIndentation = languageHostIndentation;
            this.scriptHasErrors = scriptHasErrors;
            this.formattingRequestKind = formattingRequestKind;
            this.snapshotSpan = this.span;
            this.tokenTags = this.tokens;
            this.EditCommands = new Formatting.List_TextEditInfo();
        }
        FormattingTask.prototype.Run = function () {
            var _this = this;
            if(this.tokenTags.count() == 0) {
                return;
            }
            var tree = TypeScript.timeFunction(this.logger, "FormattingTask: new ParseTree()", function () {
                return new Formatting.ParseTree(_this.fileAuthoringProxy, _this.snapshotSpan.span, _this.tokenTags, false);
            });
            if(tree.Root == null) {
                return;
            }
            TypeScript.timeFunction(this.logger, "FormattingTask: FillIndentationLevels()", function () {
                Formatting.IndentationEdgeFinder.FillIndentationLevels(tree.Root);
            });
            TypeScript.timeFunction(this.logger, "FormattingTask: Format()", function () {
                _this.Format(tree);
            });
        };
        FormattingTask.prototype.Format = function (tree) {
            var _this = this;
            var tokenIndex = 0;
            var token1Line = 0;
            var currentLineNumber = -1;
            var context = tree.Root;
            Formatting.ParseTree.DumpTree(this.logger, tree.Root);
            var sameLineIndent = false;
            var t1 = this.tokenTags.get(0);
            token1Line = t1.lineNumber();
            var indenter = new Formatting.Indenter(this.logger, tree, this.Snapshot, this.languageHostIndentation, this.editorOptions, t1, false);
            if(!this.scriptHasErrors) {
                this.TrimWhitespaceInLineRange(t1, this.Snapshot.GetLineNumberFromPosition(this.snapshotSpan.startPosition()), token1Line - 1);
            }
            if(this.tokenTags.count() == 1) {
                context = Formatting.ParseTree.FindCommonParentNode(t1.Span.span, t1.Span.span, context);
            }
            var formattingContext = new Formatting.FormattingContext(this.fileAuthoringProxy, this.Snapshot, this.tokenTags, this.formattingRequestKind);
            for(tokenIndex = 1; tokenIndex < this.tokenTags.count(); tokenIndex++) {
                if(this.logger.information()) {
                    this.logger.log("Processing token #" + tokenIndex + ": tokenId=" + (TypeScript.TokenID)._map[t1.tokenID] + ", span=[" + t1.Span.startPosition() + "," + t1.Span.endPosition() + "]");
                }
                var t2 = this.tokenTags.get(tokenIndex);
                var token2Line = t2.lineNumber();
                context = Formatting.ParseTree.FindCommonParentNode(t1.Span.span, t2.Span.span, context);
                if(context.TokenTagIndex == null && context.AuthorNode.Details.StartOffset == t1.Span.span.start()) {
                    context.TokenTagIndex = tokenIndex - 1;
                }
                if(token1Line != currentLineNumber) {
                    var node = this.FindTokenNode(t1.Span.span, context);
                    var edits = indenter.GetIndentationEdits(t1, t2, node, sameLineIndent);
                    for(var i = 0; i < edits.count(); i++) {
                        this.EditCommands.add(edits.get(i));
                    }
                    currentLineNumber = token1Line;
                    sameLineIndent = false;
                }
                if(t1.Token == Formatting.AuthorTokenKind.atkComment || t1.Token == Formatting.AuthorTokenKind.atkString) {
                    currentLineNumber = this.Snapshot.GetLineNumberFromPosition(t1.Span.endPosition());
                }
                if(this.logger.information()) {
                    this.logger.log("Context node: " + context.toString());
                }
                if(!this.scriptHasErrors) {
                    formattingContext.setContext(context, t1, t2);
                    var rule = this.rulesProvider.getRulesMap().GetRule(formattingContext);
                    if(rule != null) {
                        this.GetRuleEdits(rule, t1, t2).foreach(function (edit) {
                            _this.EditCommands.add(edit);
                        });
                        if((rule.Operation.Action == Formatting.RuleAction.Space || rule.Operation.Action == Formatting.RuleAction.Delete) && token1Line != token2Line) {
                            var indent = indenter.GetLineIndentationForOffset(t1.Span.startPosition());
                            indenter.RegisterIndentation2(t2.Span.startPosition(), indent);
                            currentLineNumber = token2Line;
                        }
                        if(rule.Operation.Action == Formatting.RuleAction.NewLine && token1Line == token2Line) {
                            currentLineNumber = token2Line - 1;
                            sameLineIndent = true;
                        }
                    }
                    if(token1Line != token2Line) {
                        this.TrimWhitespaceInLineRange(t1, token1Line, token2Line - 1);
                    }
                }
                t1 = t2;
                token1Line = token2Line;
            }
            if(token1Line != currentLineNumber) {
                context = Formatting.ParseTree.FindCommonParentNode(t1.Span.span, t1.Span.span, context);
                indenter.GetIndentationEdits(t1, null, context, sameLineIndent).foreach(function (edit) {
                    _this.EditCommands.add(edit);
                });
            }
            if(!this.scriptHasErrors) {
                var projectionEndLineSet = this.GetProjectionLineEndPositionSet();
                if(!projectionEndLineSet.Contains(token1Line)) {
                    this.TrimWhitespace(t1);
                    var endlineNumber = this.Snapshot.GetLineNumberFromPosition(this.snapshotSpan.endPosition());
                    if(projectionEndLineSet.Contains(endlineNumber)) {
                        endlineNumber--;
                    }
                    this.TrimWhitespaceInLineRange(t1, token1Line + 1, endlineNumber);
                }
            }
        };
        FormattingTask.prototype.GetProjectionEndLines = function () {
            var result = new Formatting.List_ITextSnapshotLine();
            return result;
        };
        FormattingTask.prototype.GetProjectionLineEndPositionSet = function () {
            var projectionEndLineSet = new Formatting.HashSet_int();
            this.GetProjectionEndLines().foreach(function (line) {
                if(!projectionEndLineSet.Contains(line.lineNumber())) {
                    projectionEndLineSet.Add(line.lineNumber());
                }
            });
            return projectionEndLineSet;
        };
        FormattingTask.prototype.TrimWhitespaceInLineRange = function (token, startLine, endLine) {
            for(var lineNumber = startLine; lineNumber <= endLine; ++lineNumber) {
                var line = this.Snapshot.GetLineFromLineNumber(lineNumber);
                this.TrimWhitespace2(token, line);
            }
        };
        FormattingTask.prototype.TrimWhitespace = function (token) {
            var line = this.Snapshot.GetLineFromPosition(token.Span.startPosition());
            this.TrimWhitespace2(token, line);
        };
        FormattingTask.prototype.TrimWhitespace2 = function (token, line) {
            if(token.Token == Formatting.AuthorTokenKind.atkComment && token.Span.startPosition() <= line.endPosition() && token.Span.endPosition() >= line.endPosition()) {
                return;
            }
            var text = line.getText();
            var index = 0;
            for(index = text.length - 1; index >= 0; --index) {
                if(!Formatting.EditorUtilities.IsWhitespace(text.charCodeAt(index))) {
                    break;
                }
            }
            ++index;
            if(index < text.length) {
                var edit = new Formatting.TextEditInfo(line.startPosition() + index, line.length() - index, "");
                if(this.logger.information()) {
                    this.logger.log("TrimWhiteSpace2()");
                    this.logger.log("edit: minChar=" + edit.position + ", limChar=" + (edit.position + edit.length) + ", text=\"" + TypeScript.stringToLiteral(edit.replaceWith, 30) + "\"");
                }
                this.EditCommands.add(edit);
            }
        };
        FormattingTask.prototype.GetRuleEdits = function (rule, t1, t2) {
            if(this.logger.information()) {
                this.logger.log("GetRuleEdits(" + this.rulesProvider.getRuleName(rule) + ", " + "t1=[" + t1.Span.startPosition() + "," + t1.Span.endPosition() + "], " + "t2=[" + t2.Span.startPosition() + "," + t2.Span.endPosition() + "]" + ")");
            }
            var result = this.GetRuleEditsWorker(rule, t1, t2);
            if(this.logger.information()) {
                for(var i = 0; i < result.count(); i++) {
                    var edit = result.get(i);
                    this.logger.log("edit: minChar=" + edit.position + ", limChar=" + (edit.position + edit.length) + ", text=\"" + TypeScript.stringToLiteral(edit.replaceWith, 30) + "\"");
                }
            }
            return result;
        };
        FormattingTask.prototype.GetRuleEditsWorker = function (rule, t1, t2) {
            var emptyResult = new Formatting.List_TextEditInfo();
            if(rule.Operation.Action == Formatting.RuleAction.Ignore) {
                return emptyResult;
            }
            var betweenSpan;
            switch(rule.Operation.Action) {
                case Formatting.RuleAction.Delete:
 {
                        betweenSpan = new Formatting.Span(t1.Span.endPosition(), t2.Span.startPosition() - t1.Span.endPosition());
                        if(betweenSpan.length() > 0) {
                            return new Formatting.List_TextEditInfo(new Formatting.TextEditInfo(betweenSpan.start(), betweenSpan.length(), ""));
                        }
                    }
                    break;
                case Formatting.RuleAction.NewLine:
 {
                        if(rule.Flag == Formatting.RuleFlags.CanDeleteNewLines) {
                            betweenSpan = new Formatting.Span(t1.Span.endPosition(), t2.Span.startPosition() - t1.Span.endPosition());
                        } else {
                            var lengthBetween;
                            if(t1.lineNumber() == t2.lineNumber()) {
                                lengthBetween = t2.Span.startPosition() - t1.Span.endPosition();
                            } else {
                                lengthBetween = t1.Span.end().GetContainingLine().endIncludingLineBreakPosition() - t1.Span.endPosition();
                            }
                            betweenSpan = new Formatting.Span(t1.Span.endPosition(), Formatting.Math.Max(0, lengthBetween));
                        }
                        var doEdit = false;
                        var betweenText = this.Snapshot.GetText(betweenSpan);
                        var lineFeedLoc = Formatting.StringUtils.IndexOf(betweenText, this.editorOptions.NewLineCharacter);
                        if(lineFeedLoc < 0) {
                            doEdit = true;
                        } else {
                            lineFeedLoc = Formatting.StringUtils.IndexOf(betweenText, this.editorOptions.NewLineCharacter, lineFeedLoc + 1);
                            if(lineFeedLoc >= 0) {
                                doEdit = true;
                            }
                        }
                        if(doEdit) {
                            return new Formatting.List_TextEditInfo(new Formatting.TextEditInfo(betweenSpan.start(), betweenSpan.length(), this.editorOptions.NewLineCharacter));
                        }
                    }
                    break;
                case Formatting.RuleAction.Space:
 {
                        if(rule.Flag == Formatting.RuleFlags.CanDeleteNewLines) {
                            betweenSpan = new Formatting.Span(t1.Span.endPosition(), t2.Span.startPosition() - t1.Span.endPosition());
                        } else {
                            var lengthBetween;
                            if(t1.lineNumber() == t2.lineNumber()) {
                                lengthBetween = t2.Span.startPosition() - t1.Span.endPosition();
                            } else {
                                lengthBetween = t1.Span.end().GetContainingLine().endPosition() - t1.Span.endPosition();
                            }
                            betweenSpan = new Formatting.Span(t1.Span.endPosition(), Formatting.Math.Max(0, lengthBetween));
                        }
                        if(betweenSpan.length() > 1 || this.Snapshot.GetText(betweenSpan) != " ") {
                            return new Formatting.List_TextEditInfo(new Formatting.TextEditInfo(betweenSpan.start(), betweenSpan.length(), " "));
                        }
                    }
                    break;
            }
            return emptyResult;
        };
        FormattingTask.prototype.FindTokenNode = function (span, helperNode) {
            if(helperNode.CoverSpan(span)) {
                if(helperNode.children() != null) {
                    var child = Formatting.ParseNodeExtensions.TryFindNodeForSpan(helperNode.children(), span);
                    if(child != null) {
                        return this.FindTokenNode(span, child);
                    }
                }
                return helperNode;
            } else {
                if(helperNode.Parent == null) {
                    return helperNode;
                } else {
                    return this.FindTokenNode(span, helperNode.Parent);
                }
            }
        };
        return FormattingTask;
    })();
    Formatting.FormattingTask = FormattingTask;    
})(Formatting || (Formatting = {}));
