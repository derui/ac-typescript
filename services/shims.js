var Services;
(function (Services) {
    var LanguageServiceShimHostAdapter = (function () {
        function LanguageServiceShimHostAdapter(shimHost) {
            this.shimHost = shimHost;
        }
        LanguageServiceShimHostAdapter.prototype.information = function () {
            return this.shimHost.information();
        };
        LanguageServiceShimHostAdapter.prototype.debug = function () {
            return this.shimHost.debug();
        };
        LanguageServiceShimHostAdapter.prototype.warning = function () {
            return this.shimHost.warning();
        };
        LanguageServiceShimHostAdapter.prototype.error = function () {
            return this.shimHost.error();
        };
        LanguageServiceShimHostAdapter.prototype.fatal = function () {
            return this.shimHost.fatal();
        };
        LanguageServiceShimHostAdapter.prototype.log = function (s) {
            this.shimHost.log(s);
        };
        LanguageServiceShimHostAdapter.prototype.getCompilationSettings = function () {
            var settingsJson = this.shimHost.getCompilationSettings();
            if(settingsJson == null || settingsJson == "") {
                return null;
            }
            var settings = JSON.parse(settingsJson);
            return settings;
        };
        LanguageServiceShimHostAdapter.prototype.getScriptCount = function () {
            return this.shimHost.getScriptCount();
        };
        LanguageServiceShimHostAdapter.prototype.getScriptId = function (scriptIndex) {
            return this.shimHost.getScriptId(scriptIndex);
        };
        LanguageServiceShimHostAdapter.prototype.getScriptSourceText = function (scriptIndex, start, end) {
            return this.shimHost.getScriptSourceText(scriptIndex, start, end);
        };
        LanguageServiceShimHostAdapter.prototype.getScriptSourceLength = function (scriptIndex) {
            return this.shimHost.getScriptSourceLength(scriptIndex);
        };
        LanguageServiceShimHostAdapter.prototype.getScriptIsResident = function (scriptIndex) {
            return this.shimHost.getScriptIsResident(scriptIndex);
        };
        LanguageServiceShimHostAdapter.prototype.getScriptVersion = function (scriptIndex) {
            return this.shimHost.getScriptVersion(scriptIndex);
        };
        LanguageServiceShimHostAdapter.prototype.getScriptEditRangeSinceVersion = function (scriptIndex, scriptVersion) {
            var rangeText = this.shimHost.getScriptEditRangeSinceVersion(scriptIndex, scriptVersion);
            if(rangeText === null || rangeText === "") {
                return null;
            }
            var minLimDeltaString = rangeText.split(",");
            return new TypeScript.ScriptEditRange(parseInt(minLimDeltaString[0]), parseInt(minLimDeltaString[1]), parseInt(minLimDeltaString[2]));
        };
        return LanguageServiceShimHostAdapter;
    })();
    Services.LanguageServiceShimHostAdapter = LanguageServiceShimHostAdapter;    
    function simpleForwardCall(logger, actionDescription, action) {
        logger.log(actionDescription);
        var start = Date.now();
        var result = action();
        var end = Date.now();
        logger.log(actionDescription + " completed in " + (end - start) + " msec");
        if(typeof (result) === "string") {
            var str = result;
            logger.log("  result.length=" + str.length + ", result=\"" + TypeScript.stringToLiteral(str, 128) + (str.length > 128 ? "..." : "") + "\"");
        }
        return result;
    }
    Services.simpleForwardCall = simpleForwardCall;
    function forwardCall(logger, actionDescription, action, throwOnError) {
        if (typeof throwOnError === "undefined") { throwOnError = false; }
        try  {
            return simpleForwardCall(logger, actionDescription, action);
        } catch (err) {
            Services.logInternalError(logger, err);
            if(throwOnError) {
                throw err;
            }
            return "##ERROR##" + err.name + "##" + err.message;
        }
    }
    Services.forwardCall = forwardCall;
    function forwardJSONCall(logger, actionDescription, action) {
        try  {
            return simpleForwardCall(logger, actionDescription, action);
        } catch (err) {
            Services.logInternalError(logger, err);
            return _errorToJSON(err);
        }
    }
    Services.forwardJSONCall = forwardJSONCall;
    function _resultToJSON(result) {
        return '{"result":' + JSON.stringify(result) + "}";
    }
    function _errorToJSON(err) {
        return '{"error":' + JSON.stringify(err) + "}";
    }
    var LanguageServiceShim = (function () {
        function LanguageServiceShim(host, languageService) {
            this.host = host;
            this.languageService = languageService;
            this.logger = this.host;
        }
        LanguageServiceShim.prototype.forwardCall = function (actionDescription, action, throwOnError) {
            if (typeof throwOnError === "undefined") { throwOnError = false; }
            return Services.forwardCall(this.logger, actionDescription, action, throwOnError);
        };
        LanguageServiceShim.prototype.forwardJSONCall = function (actionDescription, action) {
            return Services.forwardJSONCall(this.logger, actionDescription, action);
        };
        LanguageServiceShim.prototype.dispose = function (dummy) {
            this.logger.log("dispose()");
            this.languageService = null;
            this.logger = null;
        };
        LanguageServiceShim.prototype.refresh = function (throwOnError) {
            var _this = this;
            this.forwardCall("refresh(" + throwOnError + ")", function () {
                _this.languageService.refresh();
                return null;
            }, throwOnError);
        };
        LanguageServiceShim.prototype.getErrors = function (maxCount) {
            var _this = this;
            return this.forwardJSONCall("getErrors(" + maxCount + ")", function () {
                var errors = _this.languageService.getErrors(maxCount);
                return _resultToJSON(errors);
            });
        };
        LanguageServiceShim.prototype.getScriptErrors = function (fileName, maxCount) {
            var _this = this;
            return this.forwardJSONCall("getScriptErrors(" + maxCount + ")", function () {
                var errors = _this.languageService.getScriptErrors(fileName, maxCount);
                return _resultToJSON(errors);
            });
        };
        LanguageServiceShim.prototype.getTypeAtPosition = function (fileName, pos) {
            var _this = this;
            return this.forwardJSONCall("getTypeAtPosition(\"" + fileName + "\", " + pos + ")", function () {
                var typeInfo = _this.languageService.getTypeAtPosition(fileName, pos);
                return _resultToJSON(typeInfo);
            });
        };
        LanguageServiceShim.prototype.getNameOrDottedNameSpan = function (fileName, startPos, endPos) {
            var _this = this;
            return this.forwardJSONCall("getNameOrDottedNameSpan(\"" + fileName + "\", " + startPos + ", " + endPos + ")", function () {
                var spanInfo = _this.languageService.getNameOrDottedNameSpan(fileName, startPos, endPos);
                return _resultToJSON(spanInfo);
            });
        };
        LanguageServiceShim.prototype.getBreakpointStatementAtPosition = function (fileName, pos) {
            var _this = this;
            return this.forwardJSONCall("getBreakpointStatementAtPosition(\"" + fileName + "\", " + pos + ")", function () {
                var spanInfo = _this.languageService.getBreakpointStatementAtPosition(fileName, pos);
                return _resultToJSON(spanInfo);
            });
        };
        LanguageServiceShim.prototype.getSignatureAtPosition = function (fileName, pos) {
            var _this = this;
            return this.forwardJSONCall("getSignatureAtPosition(\"" + fileName + "\", " + pos + ")", function () {
                var signatureInfo = _this.languageService.getSignatureAtPosition(fileName, pos);
                return _resultToJSON(signatureInfo);
            });
        };
        LanguageServiceShim.prototype.getDefinitionAtPosition = function (fileName, pos) {
            var _this = this;
            return this.forwardCall("getDefinitionAtPosition(\"" + fileName + "\", " + pos + ")", function () {
                var definition = _this.languageService.getDefinitionAtPosition(fileName, pos);
                var result = "";
                if(definition !== null) {
                    result = definition.unitIndex + '\t' + definition.minChar + '\t' + definition.limChar + '\t' + definition.kind + '\t' + definition.name + '\t' + definition.containerKind + '\t' + definition.containerName;
                }
                return result;
            });
        };
        LanguageServiceShim.prototype.getBraceMatchingAtPosition = function (fileName, pos) {
            var _this = this;
            return this.forwardJSONCall("getBraceMatchingAtPosition(\"" + fileName + "\", " + pos + ")", function () {
                var textRanges = _this.languageService.getBraceMatchingAtPosition(fileName, pos);
                return _resultToJSON(textRanges);
            });
        };
        LanguageServiceShim.prototype.getSmartIndentAtLineNumber = function (fileName, lineNumber, options) {
            var _this = this;
            return this.forwardJSONCall("getSmartIndentAtLineNumber(\"" + fileName + "\", " + lineNumber + ")", function () {
                var localOptions = JSON.parse(options);
                var columnOffset = _this.languageService.getSmartIndentAtLineNumber(fileName, lineNumber, localOptions);
                return _resultToJSON({
                    value: columnOffset
                });
            });
        };
        LanguageServiceShim.prototype.getReferencesAtPosition = function (fileName, pos) {
            var _this = this;
            return this.forwardJSONCall("getReferencesAtPosition(\"" + fileName + "\", " + pos + ")", function () {
                var entries = _this.languageService.getReferencesAtPosition(fileName, pos);
                return _this._referencesToResult(entries);
            });
        };
        LanguageServiceShim.prototype.getOccurrencesAtPosition = function (fileName, pos) {
            var _this = this;
            return this.forwardCall("getOccurrencesAtPosition(\"" + fileName + "\", " + pos + ")", function () {
                var entries = _this.languageService.getOccurrencesAtPosition(fileName, pos);
                return _this._referencesToResult(entries);
            });
        };
        LanguageServiceShim.prototype.getImplementorsAtPosition = function (fileName, pos) {
            var _this = this;
            return this.forwardJSONCall("getImplementorsAtPosition(\"" + fileName + "\", " + pos + ")", function () {
                var entries = _this.languageService.getImplementorsAtPosition(fileName, pos);
                return _this._referencesToResult(entries);
            });
        };
        LanguageServiceShim.prototype._referencesToResult = function (entries) {
            var result = "";
            for(var i = 0; i < entries.length; i++) {
                var entry = entries[i];
                result += entry.unitIndex + " " + entry.ast.minChar + " " + entry.ast.limChar + " " + entry.isWriteAccess + "\n";
            }
            return result;
        };
        LanguageServiceShim.prototype.getCompletionsAtPosition = function (fileName, pos, isMemberCompletion) {
            var _this = this;
            return this.forwardJSONCall("getCompletionsAtPosition(\"" + fileName + "\", " + pos + ", " + isMemberCompletion + ")", function () {
                var completion = _this.languageService.getCompletionsAtPosition(fileName, pos, isMemberCompletion);
                var result = _resultToJSON(completion);
                return result;
            });
        };
        LanguageServiceShim.prototype.getFormattingEditsForRange = function (fileName, minChar, limChar, options) {
            var _this = this;
            return this.forwardJSONCall("getFormattingEditsForRange(\"" + fileName + "\", " + minChar + ", " + limChar + ")", function () {
                var localOptions = JSON.parse(options);
                var edits = _this.languageService.getFormattingEditsForRange(fileName, minChar, limChar, localOptions);
                var result = _resultToJSON(edits);
                return result;
            });
        };
        LanguageServiceShim.prototype.getFormattingEditsForDocument = function (fileName, minChar, limChar, options) {
            var _this = this;
            return this.forwardJSONCall("getFormattingEditsForDocument(\"" + fileName + "\", " + minChar + ", " + limChar + ")", function () {
                var localOptions = JSON.parse(options);
                var edits = _this.languageService.getFormattingEditsForDocument(fileName, minChar, limChar, localOptions);
                var result = _resultToJSON(edits);
                return result;
            });
        };
        LanguageServiceShim.prototype.getFormattingEditsOnPaste = function (fileName, minChar, limChar, options) {
            var _this = this;
            return this.forwardJSONCall("getFormattingEditsOnPaste(\"" + fileName + "\", " + minChar + ", " + limChar + ")", function () {
                var localOptions = JSON.parse(options);
                var edits = _this.languageService.getFormattingEditsOnPaste(fileName, minChar, limChar, localOptions);
                var result = _resultToJSON(edits);
                return result;
            });
        };
        LanguageServiceShim.prototype.getFormattingEditsAfterKeystroke = function (fileName, position, key, options) {
            var _this = this;
            return this.forwardJSONCall("getFormattingEditsAfterKeystroke(\"" + fileName + "\", " + position + ", \"" + key + "\")", function () {
                var localOptions = JSON.parse(options);
                var edits = _this.languageService.getFormattingEditsAfterKeystroke(fileName, position, key, localOptions);
                var result = _resultToJSON(edits);
                return result;
            });
        };
        LanguageServiceShim.prototype.getNavigateToItems = function (searchValue) {
            var _this = this;
            return this.forwardCall("getNavigateToItems(\"" + searchValue + "\")", function () {
                var items = _this.languageService.getNavigateToItems(searchValue);
                var result = _this._navigateToItemsToString(items);
                return result;
            });
        };
        LanguageServiceShim.prototype.getScriptLexicalStructure = function (fileName) {
            var _this = this;
            return this.forwardCall("getScriptLexicalStructure(\"" + fileName + "\")", function () {
                var items = _this.languageService.getScriptLexicalStructure(fileName);
                var result = _this._navigateToItemsToString(items);
                return result;
            });
        };
        LanguageServiceShim.prototype.getOutliningRegions = function (fileName) {
            var _this = this;
            return this.forwardCall("getOutliningRegions(\"" + fileName + "\")", function () {
                var items = _this.languageService.getOutliningRegions(fileName);
                var result = _this._navigateToItemsToString(items);
                return result;
            });
        };
        LanguageServiceShim.prototype.logAST = function (fileName) {
            var _this = this;
            this.forwardCall("logAST(\"" + fileName + "\")", function () {
                _this.languageService.logAST(fileName);
                return null;
            });
        };
        LanguageServiceShim.prototype.logSyntaxAST = function (fileName) {
            var _this = this;
            this.forwardCall("logSyntaxAST(\"" + fileName + "\")", function () {
                _this.languageService.logSyntaxAST(fileName);
                return null;
            });
        };
        LanguageServiceShim.prototype.getEmitOutput = function (fileName) {
            var _this = this;
            return this.forwardJSONCall("getEmitOutput(\"" + fileName + "\")", function () {
                var output = _this.languageService.getEmitOutput(fileName);
                var result = _resultToJSON(output);
                return result;
            });
        };
        LanguageServiceShim.prototype._navigateToItemsToString = function (items) {
            var result = "";
            for(var i = 0; i < items.length; i++) {
                var item = items[i];
                result += item.name + "\t" + item.kind + "\t" + item.kindModifiers + "\t" + item.containerName + "\t" + item.containerKind + "\t" + item.matchKind + "\t" + item.unitIndex + "\t" + item.minChar + "\t" + item.limChar + "\n";
            }
            return result;
        };
        return LanguageServiceShim;
    })();
    Services.LanguageServiceShim = LanguageServiceShim;    
    var ClassifierShim = (function () {
        function ClassifierShim(host) {
            this.host = host;
            this.classifier = new Services.Classifier(this.host);
        }
        ClassifierShim.prototype.getClassificationsForLine = function (text, lexState) {
            var classification = this.classifier.getClassificationsForLine(text, lexState);
            var items = classification.entries;
            var result = "";
            for(var i = 0; i < items.length; i++) {
                result += items[i].length + "\n";
                result += items[i].classification + "\n";
            }
            result += classification.finalLexState;
            return result;
        };
        return ClassifierShim;
    })();
    Services.ClassifierShim = ClassifierShim;    
    var CoreServicesShim = (function () {
        function CoreServicesShim(host) {
            this.host = host;
            this.logger = this.host.logger;
            this.services = new Services.CoreServices(this.host);
        }
        CoreServicesShim.prototype.forwardCall = function (actionDescription, action, throwOnError) {
            if (typeof throwOnError === "undefined") { throwOnError = false; }
            return Services.forwardCall(this.logger, actionDescription, action, throwOnError);
        };
        CoreServicesShim.prototype.forwardJSONCall = function (actionDescription, action) {
            return Services.forwardJSONCall(this.logger, actionDescription, action);
        };
        CoreServicesShim.prototype.getPreProcessedFileInfo = function (scriptId, sourceText) {
            var _this = this;
            return this.forwardJSONCall("getPreProcessedFileInfo(\"" + scriptId + "\")", function () {
                var result = _this.services.getPreProcessedFileInfo(scriptId, sourceText);
                return _resultToJSON(result);
            });
        };
        CoreServicesShim.prototype.getDefaultCompilationSettings = function () {
            var _this = this;
            return this.forwardJSONCall("getDefaultCompilationSettings()", function () {
                var result = _this.services.getDefaultCompilationSettings();
                return _resultToJSON(result);
            });
        };
        CoreServicesShim.prototype.dumpMemory = function (dummy) {
            var _this = this;
            return this.forwardCall("dumpMemory()", function () {
                return _this.services.dumpMemory();
            });
        };
        CoreServicesShim.prototype.getMemoryInfo = function (dummy) {
            var _this = this;
            return this.forwardJSONCall("getMemoryInfo()", function () {
                var result = _this.services.getMemoryInfo();
                return _resultToJSON(result);
            });
        };
        return CoreServicesShim;
    })();
    Services.CoreServicesShim = CoreServicesShim;    
})(Services || (Services = {}));
