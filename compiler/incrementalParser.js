var TypeScript;
(function (TypeScript) {
    var IncrementalParser = (function () {
        function IncrementalParser(logger) {
            this.logger = logger;
            this.astLogger = new TypeScript.AstLogger(this.logger);
        }
        IncrementalParser.prototype.getEnclosingScopeContextIfSingleScopeEdit = function (previousScript, scriptId, newSourceText, editRange) {
            this.logger.log("checkEditsInsideSingleScope(\"" + scriptId + "\")");
            if(editRange === null) {
                throw new Error("editRange should be valid");
            }
            if(editRange.isUnknown()) {
                this.logger.log("  Bailing out because edit range is unknown");
                return null;
            }
            var scope1 = TypeScript.findEnclosingScopeAt(this.logger, previousScript, newSourceText, editRange.minChar, false);
            var scope2 = TypeScript.findEnclosingScopeAt(this.logger, previousScript, newSourceText, editRange.limChar, false);
            if(scope1 == null || scope2 == null) {
                this.logger.log("  Bailing out because containing scopes cannot be determined");
                return null;
            }
            if(scope1.scopeStartAST !== scope2.scopeStartAST) {
                this.logger.log("  Bailing out because edit overlaps 2 disctint scopes");
                return null;
            }
            var newScopeLength = scope1.scopeStartAST.limChar - scope1.scopeStartAST.minChar + editRange.delta;
            if(newScopeLength <= 0) {
                this.logger.log("  Bailing out because scope has been entirely removed from new source text");
                return null;
            }
            return scope1;
        };
        IncrementalParser.prototype.attemptIncrementalUpdateUnit = function (previousScript, scriptId, newSourceText, editRange) {
            this.logger.log("attemptIncrementalUpdateUnit(\"" + scriptId + "\")");
            if(editRange === null) {
                throw new Error("editRange should be valid");
            }
            var scope1 = this.getEnclosingScopeContextIfSingleScopeEdit(previousScript, scriptId, newSourceText, editRange);
            if(scope1 === null) {
                return null;
            }
            var newScopeLength = scope1.scopeStartAST.limChar - scope1.scopeStartAST.minChar + editRange.delta;
            if(newScopeLength >= newSourceText.getLength() / 2) {
                this.logger.log("  Bailing out because range of scope to reparse (" + newScopeLength + " characters) is greater than half the size of the source text");
                return null;
            }
            var parseErrors = [];
            var errorCapture = function (minChar, charLen, message, unitIndex) {
                parseErrors.push(new TypeScript.ErrorEntry(unitIndex, minChar, minChar + charLen, message));
            };
            var quickParseResult = TypeScript.quickParse(this.logger, scope1.scopeStartAST, newSourceText, scope1.scopeStartAST.minChar, scope1.scopeStartAST.minChar + newScopeLength, errorCapture);
            if(quickParseResult.endLexState != TypeScript.LexState.Start) {
                this.logger.log("  Bailing out because scope contains unterminated comment");
                return null;
            }
            var scriptFragment = quickParseResult.Script;
            if(scriptFragment.vars.members.length !== 0) {
                this.logger.log("  Bailing out because new source text defines variables");
                return null;
            }
            if(scriptFragment.bod.members.length !== 1) {
                this.logger.log("  Bailing out because new source text defines more than one scope (or none)");
                return null;
            }
            var oldScope = scope1.scopeStartAST;
            var newScope = scriptFragment.bod.members[0];
            if(oldScope.nodeType != newScope.nodeType) {
                this.logger.log("  Bailing out because new source text does not define the same scope type as the existing scope");
                return null;
            }
            if(!(oldScope).leftCurlyCount || !(oldScope).rightCurlyCount) {
                this.logger.log("  Bailing out because sopce doesn't have left/right curly count");
                return null;
            }
            if((oldScope).leftCurlyCount !== (newScope).leftCurlyCount) {
                this.logger.log("  Bailing out because new source text contains more (or fewer) left curly braces");
                return null;
            }
            if((oldScope).rightCurlyCount !== (newScope).rightCurlyCount) {
                this.logger.log("  Bailing out because new source text contains more (or fewer) right curly braces");
                return null;
            }
            if(newScope.minChar !== 0) {
                this.logger.log("  Bailing out because new function declaration does not start at position 0");
                return null;
            }
            if(newScope.limChar !== newScopeLength) {
                this.logger.log("  Bailing out because new function declaration does not end at the new end position");
                return null;
            }
            return TypeScript.UpdateUnitResult.singleScopeEdits(previousScript, scriptFragment, oldScope, newScope, editRange, parseErrors);
        };
        IncrementalParser.prototype.mergeTrees = function (updateResult) {
            var _this = this;
            TypeScript.timeFunction(this.logger, "mergeTrees()", function () {
                var editRange = new TypeScript.ScriptEditRange(updateResult.scope1.minChar, updateResult.scope1.limChar, updateResult.editRange.delta);
                _this.applyDeltaPosition(updateResult.script1, editRange.limChar, editRange.delta);
                _this.applyDeltaPosition(updateResult.script2, 0, editRange.minChar);
                _this.mergeLocationInfo(updateResult.script1, updateResult.script2, editRange);
                _this.replaceAST(updateResult.script1, updateResult.scope1, updateResult.scope2);
            });
        };
        IncrementalParser.prototype.replaceAST = function (script, oldAst, newAst) {
            var _this = this;
            var pre = function (cur, parent, walker) {
                if(cur === oldAst) {
                    newAst.preComments = cur.preComments;
                    newAst.postComments = cur.postComments;
                    _this.logger.log("replaced old AST node with new one in script AST");
                    walker.options.stopWalk();
                    return newAst;
                }
                if(TypeScript.isValidAstNode(cur)) {
                    if(cur.limChar < oldAst.minChar || cur.minChar > oldAst.limChar) {
                        walker.options.goChildren = false;
                    }
                }
                return cur;
            };
            TypeScript.getAstWalkerFactory().walk(script, pre);
        };
        IncrementalParser.prototype.mergeLocationInfo = function (script, partial, editRange) {
            var lineMap1 = script.locationInfo.lineMap;
            var lineMap2 = partial.locationInfo.lineMap;
            if(this.logger.information()) {
                this.logger.log("lineMap1 (before):");
                this.astLogger.logLinemap(lineMap1);
                this.logger.log("lineMap2 (quick parse):");
                this.astLogger.logLinemap(lineMap2);
                this.logger.log("EditRange=" + editRange);
            }
            var i1 = 2;
            var i2 = 2;
            var len1 = lineMap1.length;
            var len2 = lineMap2.length;
            while(i1 < len1) {
                if(lineMap1[i1] <= editRange.minChar) {
                    i1++;
                } else if(lineMap1[i1] >= editRange.limChar) {
                    lineMap1[i1] += editRange.delta;
                    i1++;
                } else {
                    if(i2 < len2) {
                        lineMap1.splice(i1, 0, lineMap2[i2] + editRange.minChar);
                        i1++;
                        len1++;
                        i2++;
                    } else {
                        lineMap1.splice(i1, 1);
                        len1--;
                    }
                }
            }
            if(i2 < len2) {
                if(lineMap1[len1 - 1] >= (lineMap2[i2] + editRange.minChar)) {
                    i1 = 2;
                    while(i1 < len1 && i2 < len2) {
                        if(lineMap1[i1] < (lineMap2[i2] + editRange.minChar)) {
                            i1++;
                        } else {
                            lineMap1.splice(i1, 0, lineMap2[i2] + editRange.minChar);
                            i1++;
                            len1++;
                            i2++;
                        }
                    }
                }
                for(; i2 < len2; i2++) {
                    lineMap1.push(lineMap2[i2] + editRange.minChar);
                }
            }
            if(this.logger.information()) {
                this.logger.log("lineMap1 (after merge):");
                this.astLogger.logLinemap(lineMap1);
            }
        };
        IncrementalParser.prototype.applyDeltaPosition = function (ast, start, delta) {
            var applyDelta = function (ast) {
                if(ast.minChar !== -1 && ast.minChar >= start) {
                    ast.minChar += delta;
                }
                if(ast.limChar !== -1 && ast.limChar >= start) {
                    ast.limChar += delta;
                }
            };
            var applyDeltaToComments = function (comments) {
                if(comments && comments.length > 0) {
                    for(var i = 0; i < comments.length; i++) {
                        applyDelta(comments[i]);
                    }
                }
            };
            var pre = function (cur, parent, walker) {
                if(cur.limChar !== -1 && cur.limChar < start) {
                    walker.options.goChildren = false;
                }
                applyDelta(cur);
                applyDeltaToComments(cur.preComments);
                applyDeltaToComments(cur.postComments);
                return cur;
            };
            TypeScript.getAstWalkerFactory().walk(ast, pre);
        };
        return IncrementalParser;
    })();
    TypeScript.IncrementalParser = IncrementalParser;    
})(TypeScript || (TypeScript = {}));
