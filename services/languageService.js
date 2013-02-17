var __extends = this.__extends || function (d, b) {
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var Services;
(function (Services) {
    function logInternalError(logger, err) {
        logger.log("*INTERNAL ERROR* - Exception in typescript services: " + err.message);
    }
    Services.logInternalError = logInternalError;
    var SourceTextAdapter = (function () {
        function SourceTextAdapter(host, scriptIndex) {
            this.host = host;
            this.scriptIndex = scriptIndex;
        }
        SourceTextAdapter.prototype.getText = function (start, end) {
            return this.host.getScriptSourceText(this.scriptIndex, start, end);
        };
        SourceTextAdapter.prototype.getLength = function () {
            return this.host.getScriptSourceLength(this.scriptIndex);
        };
        return SourceTextAdapter;
    })();
    Services.SourceTextAdapter = SourceTextAdapter;    
    var CachedSourceTextAdapter = (function () {
        function CachedSourceTextAdapter(host, scriptIndex) {
            this.length = host.getScriptSourceLength(scriptIndex);
            this.text = host.getScriptSourceText(scriptIndex, 0, this.length);
        }
        CachedSourceTextAdapter.prototype.getText = function (start, end) {
            return this.text.substring(start, end);
        };
        CachedSourceTextAdapter.prototype.getLength = function () {
            return this.length;
        };
        return CachedSourceTextAdapter;
    })();
    Services.CachedSourceTextAdapter = CachedSourceTextAdapter;    
    var SourceTextRange = (function () {
        function SourceTextRange(sourceText, minChar, limChar) {
            this.sourceText = sourceText;
            this.minChar = minChar;
            this.limChar = limChar;
        }
        SourceTextRange.prototype.getText = function (start, end) {
            var actualStart = this.minChar + start;
            var actualEnd = this.minChar + end;
            if(actualEnd > this.limChar) {
                actualEnd = this.limChar;
            }
            return this.sourceText.getText(actualStart, actualEnd);
        };
        SourceTextRange.prototype.getLength = function () {
            return this.limChar - this.minChar;
        };
        return SourceTextRange;
    })();
    Services.SourceTextRange = SourceTextRange;    
    var ReferenceEntry = (function () {
        function ReferenceEntry(unitIndex, ast, isWriteAccess) {
            this.unitIndex = unitIndex;
            this.ast = ast;
            this.isWriteAccess = isWriteAccess;
        }
        ReferenceEntry.prototype.getHashCode = function () {
            return TypeScript.combineHashes(TypeScript.numberHashFn(this.unitIndex), TypeScript.combineHashes(TypeScript.numberHashFn(this.ast.minChar), TypeScript.numberHashFn(this.ast.limChar)));
        };
        ReferenceEntry.prototype.equals = function (other) {
            if(other === null || other === undefined) {
                return false;
            }
            return (this.unitIndex === other.unitIndex) && (this.ast.minChar === other.ast.minChar) && (this.ast.limChar === other.ast.limChar);
        };
        return ReferenceEntry;
    })();
    Services.ReferenceEntry = ReferenceEntry;    
    var ReferenceEntrySet = (function () {
        function ReferenceEntrySet() {
            this.entries = [];
            this.hashTable = new TypeScript.HashTable(101, function (r) {
                return r.getHashCode();
            }, function (r1, r2) {
                return r1.equals(r2);
            });
        }
        ReferenceEntrySet.prototype.getEntries = function () {
            return this.entries;
        };
        ReferenceEntrySet.prototype.addAst = function (unitIndex, ast, isWriteAccess) {
            var reference = new ReferenceEntry(unitIndex, ast, isWriteAccess);
            if(this.hashTable.lookup(reference) !== null) {
                return;
            }
            this.hashTable.add(reference, reference);
            this.entries.push(reference);
        };
        ReferenceEntrySet.prototype.addSymbol = function (sym) {
            var unitIndex = sym.unitIndex;
            if(unitIndex < 0) {
                return;
            }
            var ast = sym.declAST;
            if(ast == null) {
                return;
            }
            var symbolLocation;
            switch(ast.nodeType) {
                case TypeScript.NodeType.InterfaceDeclaration:
                    symbolLocation = (ast).name;
                    break;
                case TypeScript.NodeType.ClassDeclaration:
                    symbolLocation = (ast).name;
                    break;
                case TypeScript.NodeType.ModuleDeclaration:
                    symbolLocation = (ast).name;
                    break;
                case TypeScript.NodeType.VarDecl:
                    symbolLocation = (ast).id;
                    break;
                case TypeScript.NodeType.FuncDecl:
                    symbolLocation = (ast).name;
                    break;
                default:
                    symbolLocation = ast;
            }
            if(symbolLocation === null) {
                symbolLocation = ast;
            }
            this.addAst(unitIndex, symbolLocation, false);
        };
        return ReferenceEntrySet;
    })();
    Services.ReferenceEntrySet = ReferenceEntrySet;    
    var NavigateToItem = (function () {
        function NavigateToItem() {
            this.name = "";
            this.kind = "";
            this.kindModifiers = "";
            this.matchKind = "";
            this.unitIndex = -1;
            this.minChar = -1;
            this.limChar = -1;
            this.containerName = "";
            this.containerKind = "";
        }
        return NavigateToItem;
    })();
    Services.NavigateToItem = NavigateToItem;    
    var NavigateToContext = (function () {
        function NavigateToContext() {
            this.options = new TypeScript.AstWalkOptions();
            this.unitIndex = 0;
            this.containerSymbols = [];
            this.containerKinds = [];
            this.containerASTs = [];
            this.path = new TypeScript.AstPath();
            this.result = [];
        }
        return NavigateToContext;
    })();
    Services.NavigateToContext = NavigateToContext;    
    var TextRange = (function () {
        function TextRange(minChar, limChar) {
            this.minChar = minChar;
            this.limChar = limChar;
        }
        return TextRange;
    })();
    Services.TextRange = TextRange;    
    var TextEdit = (function () {
        function TextEdit(minChar, limChar, text) {
            this.minChar = minChar;
            this.limChar = limChar;
            this.text = text;
        }
        TextEdit.createInsert = function createInsert(pos, text) {
            return new TextEdit(pos, pos, text);
        };
        TextEdit.createDelete = function createDelete(minChar, limChar) {
            return new TextEdit(minChar, limChar, "");
        };
        TextEdit.createReplace = function createReplace(minChar, limChar, text) {
            return new TextEdit(minChar, limChar, text);
        };
        return TextEdit;
    })();
    Services.TextEdit = TextEdit;    
    var EditorOptions = (function () {
        function EditorOptions() {
            this.IndentSize = 4;
            this.TabSize = 4;
            this.NewLineCharacter = "\r\n";
            this.ConvertTabsToSpaces = true;
        }
        return EditorOptions;
    })();
    Services.EditorOptions = EditorOptions;    
    var FormatCodeOptions = (function (_super) {
        __extends(FormatCodeOptions, _super);
        function FormatCodeOptions() {
            _super.apply(this, arguments);

            this.InsertSpaceAfterCommaDelimiter = true;
            this.InsertSpaceAfterSemicolonInForStatements = true;
            this.InsertSpaceBeforeAndAfterBinaryOperators = true;
            this.InsertSpaceAfterKeywordsInControlFlowStatements = true;
            this.InsertSpaceAfterFunctionKeywordForAnonymousFunctions = false;
            this.InsertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis = false;
            this.PlaceOpenBraceOnNewLineForFunctions = false;
            this.PlaceOpenBraceOnNewLineForControlBlocks = false;
        }
        return FormatCodeOptions;
    })(EditorOptions);
    Services.FormatCodeOptions = FormatCodeOptions;    
    var GetReferencesContext = (function () {
        function GetReferencesContext() {
            this.scope = [];
        }
        return GetReferencesContext;
    })();
    Services.GetReferencesContext = GetReferencesContext;    
    var DefinitionInfo = (function () {
        function DefinitionInfo(unitIndex, minChar, limChar, kind, name, containerKind, containerName) {
            this.unitIndex = unitIndex;
            this.minChar = minChar;
            this.limChar = limChar;
            this.kind = kind;
            this.name = name;
            this.containerKind = containerKind;
            this.containerName = containerName;
        }
        return DefinitionInfo;
    })();
    Services.DefinitionInfo = DefinitionInfo;    
    var TypeInfo = (function () {
        function TypeInfo(memberName, docComment, minChar, limChar) {
            this.memberName = memberName;
            this.docComment = docComment;
            this.minChar = minChar;
            this.limChar = limChar;
        }
        return TypeInfo;
    })();
    Services.TypeInfo = TypeInfo;    
    var SpanInfo = (function () {
        function SpanInfo(minChar, limChar, text) {
            if (typeof text === "undefined") { text = null; }
            this.minChar = minChar;
            this.limChar = limChar;
            this.text = text;
        }
        return SpanInfo;
    })();
    Services.SpanInfo = SpanInfo;    
    var SignatureInfo = (function () {
        function SignatureInfo() { }
        return SignatureInfo;
    })();
    Services.SignatureInfo = SignatureInfo;    
    var FormalSignatureInfo = (function () {
        function FormalSignatureInfo() {
            this.signatureGroup = [];
        }
        return FormalSignatureInfo;
    })();
    Services.FormalSignatureInfo = FormalSignatureInfo;    
    var FormalSignatureItemInfo = (function () {
        function FormalSignatureItemInfo() {
            this.parameters = [];
        }
        return FormalSignatureItemInfo;
    })();
    Services.FormalSignatureItemInfo = FormalSignatureItemInfo;    
    var FormalParameterInfo = (function () {
        function FormalParameterInfo() { }
        return FormalParameterInfo;
    })();
    Services.FormalParameterInfo = FormalParameterInfo;    
    var ActualSignatureInfo = (function () {
        function ActualSignatureInfo() {
            this.parameters = [];
        }
        return ActualSignatureInfo;
    })();
    Services.ActualSignatureInfo = ActualSignatureInfo;    
    var ActualParameterInfo = (function () {
        function ActualParameterInfo() { }
        return ActualParameterInfo;
    })();
    Services.ActualParameterInfo = ActualParameterInfo;    
    var CompletionInfo = (function () {
        function CompletionInfo() {
            this.maybeInaccurate = false;
            this.isMemberCompletion = false;
            this.entries = [];
        }
        return CompletionInfo;
    })();
    Services.CompletionInfo = CompletionInfo;    
    var CompletionEntry = (function () {
        function CompletionEntry() {
            this.name = "";
            this.type = "";
            this.kind = "";
            this.kindModifiers = "";
            this.docComment = "";
        }
        return CompletionEntry;
    })();
    Services.CompletionEntry = CompletionEntry;    
    var ScriptElementKind = (function () {
        function ScriptElementKind() { }
        ScriptElementKind.unknown = "";
        ScriptElementKind.keyword = "keyword";
        ScriptElementKind.scriptElement = "script";
        ScriptElementKind.moduleElement = "module";
        ScriptElementKind.classElement = "class";
        ScriptElementKind.interfaceElement = "interface";
        ScriptElementKind.enumElement = "enum";
        ScriptElementKind.variableElement = "variable";
        ScriptElementKind.functionElement = "function";
        ScriptElementKind.memberFunctionElement = "method";
        ScriptElementKind.memberGetAccessorElement = "getter";
        ScriptElementKind.memberSetAccessorElement = "setter";
        ScriptElementKind.memberVariableElement = "property";
        ScriptElementKind.constructorImplementationElement = "constructor";
        ScriptElementKind.callSignatureElement = "call";
        ScriptElementKind.indexSignatureElement = "index";
        ScriptElementKind.constructSignatureElement = "construct";
        return ScriptElementKind;
    })();
    Services.ScriptElementKind = ScriptElementKind;    
    var ScriptElementKindModifier = (function () {
        function ScriptElementKindModifier() { }
        ScriptElementKindModifier.none = "";
        ScriptElementKindModifier.publicMemberModifier = "public";
        ScriptElementKindModifier.privateMemberModifier = "private";
        ScriptElementKindModifier.exportedModifier = "export";
        ScriptElementKindModifier.ambientModifier = "declare";
        ScriptElementKindModifier.staticModifier = "static";
        return ScriptElementKindModifier;
    })();
    Services.ScriptElementKindModifier = ScriptElementKindModifier;    
    var MatchKind = (function () {
        function MatchKind() { }
        MatchKind.none = null;
        MatchKind.exact = "exact";
        MatchKind.subString = "substring";
        MatchKind.prefix = "prefix";
        return MatchKind;
    })();
    Services.MatchKind = MatchKind;    
    var ScriptSyntaxASTState = (function () {
        function ScriptSyntaxASTState() {
            this.version = -1;
            this.syntaxAST = null;
            this.fileName = null;
        }
        return ScriptSyntaxASTState;
    })();
    Services.ScriptSyntaxASTState = ScriptSyntaxASTState;    
    var LanguageService = (function () {
        function LanguageService(host) {
            this.host = host;
            this.logger = this.host;
            this.compilerState = new Services.CompilerState(this.host);
            this.syntaxASTState = new ScriptSyntaxASTState();
            this.formattingRulesProvider = new Formatting.RulesProvider(this.logger);
        }
        LanguageService.prototype.refresh = function () {
            var _this = this;
            TypeScript.timeFunction(this.logger, "refresh()", function () {
                _this.compilerState.refresh();
            });
        };
        LanguageService.prototype.minimalRefresh = function () {
            var _this = this;
            TypeScript.timeFunction(this.logger, "minimalRefresh()", function () {
                _this.compilerState.minimalRefresh();
            });
        };
        LanguageService.prototype.getSymbolTree = function () {
            this.refresh();
            return this.compilerState.getSymbolTree();
        };
        LanguageService.prototype.getScriptSyntaxAST = function (fileName) {
            this.minimalRefresh();
            return this._getScriptSyntaxAST(fileName);
        };
        LanguageService.prototype._getScriptSyntaxAST = function (fileName) {
            var _this = this;
            return TypeScript.timeFunction(this.logger, "getScriptSyntaxAST(\"" + fileName + "\")", function () {
                var version = _this.compilerState.getScriptVersion(fileName);
                var syntaxAST = _this.syntaxASTState.syntaxAST;
                if(syntaxAST === null || _this.syntaxASTState.fileName !== fileName) {
                    syntaxAST = _this.compilerState.getScriptSyntaxAST(fileName);
                } else if(_this.syntaxASTState.version !== version) {
                    syntaxAST = _this.attemptIncrementalSyntaxAST(_this.syntaxASTState);
                    if(syntaxAST === null) {
                        syntaxAST = _this.compilerState.getScriptSyntaxAST(fileName);
                    }
                }
                _this.syntaxASTState.version = version;
                _this.syntaxASTState.fileName = fileName;
                _this.syntaxASTState.syntaxAST = syntaxAST;
                return _this.syntaxASTState.syntaxAST;
            });
        };
        LanguageService.prototype.attemptIncrementalSyntaxAST = function (syntaxASTState) {
            var syntaxAST = syntaxASTState.syntaxAST;
            var fileName = syntaxAST.getScriptId();
            var newSourceText = this.compilerState.getSourceText2(fileName);
            var editRange = this.compilerState.getScriptEditRangeSinceVersion(fileName, syntaxASTState.version);
            if(editRange === null) {
                return syntaxAST;
            }
            var incrementalParser = new TypeScript.IncrementalParser(this.logger);
            var updateResult = incrementalParser.attemptIncrementalUpdateUnit(syntaxAST.getScript(), syntaxAST.getScriptId(), newSourceText, editRange);
            if(updateResult !== null && updateResult.kind === TypeScript.UpdateUnitKind.EditsInsideSingleScope) {
                incrementalParser.mergeTrees(updateResult);
                return new Services.ScriptSyntaxAST(this.logger, updateResult.script1, newSourceText);
            }
            return null;
        };
        LanguageService.prototype.getScriptAST = function (fileName) {
            this.refresh();
            return this.compilerState.getScriptAST(fileName);
        };
        LanguageService.prototype.getTypeAtPosition = function (fileName, pos) {
            this.refresh();
            var script = this.compilerState.getScriptAST(fileName);
            var sourceText = this.compilerState.getSourceText(script);
            var path = this.getAstPathToPosition(script, pos);
            if(path.count() == 0) {
                return null;
            }
            if(path.nodeType() === TypeScript.NodeType.Comment) {
                this.logger.log("The specified location is inside a comment");
                return null;
            }
            var typeInfo = this.getTypeInfoAtPosition(pos, script);
            if(typeInfo == null) {
                this.logger.log("No type found at the specified location.");
                return null;
            }
            var enclosingScopeContext = TypeScript.findEnclosingScopeAt(this.logger, script, sourceText, pos, false);
            if(enclosingScopeContext == null) {
                this.logger.log("No context found at the specified location.");
                return null;
            }
            var memberName = typeInfo.type.getScopedTypeNameEx(enclosingScopeContext.getScope());
            return new TypeInfo(memberName, TypeScript.Comment.getDocCommentText(typeInfo.type.getDocComments()), typeInfo.ast.minChar, typeInfo.ast.limChar);
        };
        LanguageService.prototype.getNameOrDottedNameSpan = function (fileName, startPos, endPos) {
            this.refresh();
            var script = this.compilerState.getScriptAST(fileName);
            var spanInfo = this.getNameOrDottedNameSpanFromPosition(startPos, script);
            if(spanInfo == null) {
                this.logger.log("No name or dotted name found at the specified location.");
                return null;
            }
            return spanInfo;
        };
        LanguageService.prototype.getBreakpointInStatement = function (pos, astSpan, verifyASTPos, existingResult, forceFirstStatement, isAst) {
            if(existingResult || !astSpan || (verifyASTPos && pos > astSpan.limChar)) {
                return existingResult;
            }
            if(!isAst) {
                return astSpan;
            }
            var ast = astSpan;
            var astList = null;
            if(ast.nodeType == TypeScript.NodeType.Block) {
                var block = ast;
                astList = block.statements;
            } else if(ast.nodeType == TypeScript.NodeType.List) {
                astList = ast;
            } else {
                return ast;
            }
            if(astList.members.length > 0) {
                var lastAST = astList.members[astList.members.length - 1];
                if(!forceFirstStatement && pos > lastAST.limChar) {
                    return lastAST;
                } else {
                    return astList.members[0];
                }
            }
            return null;
        };
        LanguageService.prototype.getBreakpointStatementAtPosition = function (fileName, pos) {
            this.refresh();
            var script = this.compilerState.getScriptAST(fileName);
            var containerASTs = [];
            var lineMap = this.compilerState.getLineMap(fileName);
            var lineCol = {
                line: -1,
                col: -1
            };
            TypeScript.getSourceLineColFromMap(lineCol, pos, lineMap);
            var pre = function (cur, parent, walker) {
                if(TypeScript.isValidAstNode(cur)) {
                    if(pos >= cur.minChar && pos <= cur.limChar) {
                        switch(cur.nodeType) {
                            case TypeScript.NodeType.ModuleDeclaration:
                            case TypeScript.NodeType.ClassDeclaration:
                            case TypeScript.NodeType.FuncDecl:
                            case TypeScript.NodeType.Break:
                            case TypeScript.NodeType.Continue:
                                containerASTs.push(cur);
                                break;
                            case TypeScript.NodeType.Script:
                            case TypeScript.NodeType.List:
                            case TypeScript.NodeType.NumberLit:
                            case TypeScript.NodeType.Regex:
                            case TypeScript.NodeType.QString:
                            case TypeScript.NodeType.ArrayLit:
                            case TypeScript.NodeType.ObjectLit:
                            case TypeScript.NodeType.TypeAssertion:
                            case TypeScript.NodeType.Pos:
                            case TypeScript.NodeType.Neg:
                            case TypeScript.NodeType.Not:
                            case TypeScript.NodeType.LogNot:
                            case TypeScript.NodeType.Block:
                                break;
                            case TypeScript.NodeType.TypeRef:
                                walker.options.goChildren = false;
                                break;
                            default:
                                if(cur.isStatementOrExpression() && (!cur.isExpression() || containerASTs.length == 0 || (!containerASTs[containerASTs.length - 1].isExpression() && containerASTs[containerASTs.length - 1].nodeType != TypeScript.NodeType.VarDecl || containerASTs[containerASTs.length - 1].nodeType == TypeScript.NodeType.ConditionalExpression))) {
                                    containerASTs.push(cur);
                                }
                                break;
                        }
                    } else {
                        walker.options.goChildren = false;
                    }
                }
                return cur;
            };
            TypeScript.getAstWalkerFactory().walk(script, pre);
            if(containerASTs.length == 0) {
                return null;
            }
            var resultAST = null;
            var cur = containerASTs[containerASTs.length - 1];
            var customSpan = null;
            switch(cur.nodeType) {
                case TypeScript.NodeType.ModuleDeclaration:
                    var moduleDecl = cur;
                    if(containerASTs.length > 1) {
                        resultAST = moduleDecl;
                    } else {
                        resultAST = this.getBreakpointInStatement(pos, moduleDecl.members, false, null, false, true);
                    }
                    customSpan = moduleDecl.endingToken;
                    break;
                case TypeScript.NodeType.FuncDecl:
                    var funcDecl = cur;
                    if(containerASTs.length > 1) {
                        resultAST = funcDecl;
                    } else {
                        resultAST = this.getBreakpointInStatement(pos, funcDecl.bod, false, null, false, true);
                    }
                    customSpan = funcDecl.endingToken;
                    break;
                case TypeScript.NodeType.ClassDeclaration:
                    var classDecl = cur;
                    if(containerASTs.length > 1) {
                        resultAST = classDecl;
                    } else {
                        resultAST = this.getBreakpointInStatement(pos, classDecl.members, false, null, false, true);
                    }
                    customSpan = classDecl.endingToken;
                    break;
                case TypeScript.NodeType.VarDecl:
                    var varDecl = cur;
                    if(varDecl.init) {
                        resultAST = varDecl;
                    }
                    break;
                case TypeScript.NodeType.If:
                    var ifStatement = cur;
                    resultAST = this.getBreakpointInStatement(pos, ifStatement.statement, true, resultAST, false, false);
                    resultAST = this.getBreakpointInStatement(pos, ifStatement.thenBod, true, resultAST, false, true);
                    resultAST = this.getBreakpointInStatement(pos, ifStatement.elseBod, false, resultAST, false, true);
                    break;
                case TypeScript.NodeType.ForIn:
                    var forInStatement = cur;
                    resultAST = this.getBreakpointInStatement(pos, forInStatement.statement, true, resultAST, false, false);
                    resultAST = this.getBreakpointInStatement(pos, forInStatement.body, false, resultAST, false, true);
                    break;
                case TypeScript.NodeType.For:
                    var forStatement = cur;
                    resultAST = this.getBreakpointInStatement(pos, forStatement.init, true, null, false, true);
                    resultAST = this.getBreakpointInStatement(pos, forStatement.cond, true, resultAST, false, true);
                    resultAST = this.getBreakpointInStatement(pos, forStatement.incr, true, resultAST, false, true);
                    resultAST = this.getBreakpointInStatement(pos, forStatement.body, false, resultAST, false, true);
                    break;
                case TypeScript.NodeType.While:
                    var whileStatement = cur;
                    resultAST = this.getBreakpointInStatement(pos, whileStatement.cond, true, null, false, true);
                    resultAST = this.getBreakpointInStatement(pos, whileStatement.body, false, resultAST, false, true);
                    break;
                case TypeScript.NodeType.DoWhile:
                    var doWhileStatement = cur;
                    resultAST = this.getBreakpointInStatement(pos, doWhileStatement.body, true, null, false, true);
                    resultAST = this.getBreakpointInStatement(pos, doWhileStatement.cond, false, resultAST, false, true);
                    break;
                case TypeScript.NodeType.Switch:
                    var switchStatement = cur;
                    resultAST = this.getBreakpointInStatement(pos, switchStatement.statement, true, resultAST, false, false);
                    var caseListCount = switchStatement.caseList.members.length;
                    if(caseListCount > 0) {
                        var lastCase = switchStatement.caseList.members[caseListCount - 1];
                        if(pos >= lastCase.limChar) {
                            var caseToUse = lastCase;
                            resultAST = this.getBreakpointInStatement(pos, caseToUse.body.members[0], false, resultAST, false, true);
                        } else {
                            var caseToUse = switchStatement.caseList.members[0];
                            resultAST = this.getBreakpointInStatement(pos, caseToUse.body.members[0], false, resultAST, true, true);
                        }
                    }
                    break;
                case TypeScript.NodeType.Case:
                    var caseStatement = cur;
                    resultAST = this.getBreakpointInStatement(pos, caseStatement.body.members[0], false, null, false, true);
                    break;
                case TypeScript.NodeType.With:
                    var withStatement = cur;
                    resultAST = this.getBreakpointInStatement(pos, withStatement.body, false, null, false, true);
                    break;
                case TypeScript.NodeType.Try:
                    var tryNode = cur;
                    resultAST = this.getBreakpointInStatement(pos, tryNode.body, false, null, false, true);
                    break;
                case TypeScript.NodeType.Catch:
                    var catchNode = cur;
                    resultAST = this.getBreakpointInStatement(pos, catchNode.statement, true, null, false, false);
                    resultAST = this.getBreakpointInStatement(pos, catchNode.body, false, resultAST, false, true);
                    break;
                case TypeScript.NodeType.Finally:
                    var finallyNode = cur;
                    resultAST = this.getBreakpointInStatement(pos, finallyNode, false, null, false, true);
                    break;
                case TypeScript.NodeType.TryCatch:
                    var tryCatch = cur;
                    resultAST = this.getBreakpointInStatement(pos, tryCatch.tryNode.body, true, null, false, true);
                    resultAST = this.getBreakpointInStatement(pos, tryCatch.catchNode.statement, true, resultAST, false, false);
                    resultAST = this.getBreakpointInStatement(pos, tryCatch.catchNode.body, false, resultAST, false, true);
                    break;
                case TypeScript.NodeType.TryFinally:
                    var tryFinally = cur;
                    if(tryFinally.nodeType == TypeScript.NodeType.Try) {
                        resultAST = this.getBreakpointInStatement(pos, (tryFinally.tryNode).body, true, null, false, true);
                    } else {
                        var tryCatch = tryFinally.tryNode;
                        resultAST = this.getBreakpointInStatement(pos, tryCatch.tryNode.body, true, null, false, true);
                        resultAST = this.getBreakpointInStatement(pos, tryCatch.catchNode.statement, true, resultAST, false, false);
                        resultAST = this.getBreakpointInStatement(pos, tryCatch.catchNode.body, true, resultAST, false, true);
                    }
                    resultAST = this.getBreakpointInStatement(pos, tryFinally.finallyNode, false, resultAST, false, true);
                    break;
                default:
                    resultAST = cur;
                    break;
            }
            if(TypeScript.isValidAstNode(customSpan) && pos >= customSpan.minChar && pos <= customSpan.limChar) {
                resultAST = customSpan;
            }
            if(resultAST) {
                var result = new SpanInfo(resultAST.minChar, resultAST.limChar);
                return result;
            }
            return null;
        };
        LanguageService.prototype.getSignatureAtPosition = function (fileName, pos) {
            var _this = this;
            this.refresh();
            var script = this.compilerState.getScriptAST(fileName);
            var atEOF = (pos === script.limChar);
            var path = this.getAstPathToPosition(script, pos);
            if(path.count() == 0) {
                return null;
            }
            if(path.nodeType() === TypeScript.NodeType.Comment) {
                this.logger.log("position is inside a comment");
                return null;
            }
            var callExpr = null;
            while(path.count() >= 2) {
                if(path.isArgumentListOfCall() || path.isArgumentListOfNew()) {
                    if(atEOF || pos > path.ast().minChar) {
                        path.pop();
                        callExpr = path.pop();
                    }
                    break;
                }
                if(pos > path.ast().minChar) {
                    if(path.isBodyOfBlock() || path.isBodyOfCase() || path.isBodyOfCatch() || path.isBodyOfDefaultCase() || path.isBodyOfDoWhile() || path.isBodyOfClass() || path.isBodyOfFinally() || path.isBodyOfFor() || path.isBodyOfForIn() || path.isBodyOfFunction() || path.isBodyOfInterface() || path.isBodyOfModule() || path.isBodyOfObjectLit() || path.isBodyOfScript() || path.isBodyOfSwitch() || path.isBodyOfTry() || path.isBodyOfWhile() || path.isBodyOfWith()) {
                        break;
                    }
                }
                path.pop();
            }
            if(!callExpr || !callExpr.target || !callExpr.target.type) {
                this.logger.log("No call expression for the given position");
                return null;
            }
            if(callExpr.target.type === this.compilerState.anyType()) {
                this.logger.log("Call expression is of type 'any'");
                return null;
            }
            var sourceText = this.compilerState.getSourceText(script);
            var enclosingScopeContext = TypeScript.findEnclosingScopeAt(this.logger, script, sourceText, pos, false);
            if(enclosingScopeContext == null) {
                this.logger.log("No context found at the specified location.");
                return null;
            }
            var getNameFromSymbol = function (symbol) {
                if(symbol != null && symbol.name != "_anonymous") {
                    return symbol.name;
                }
                return "";
            };
            var getDocCommentFromSymbol = function (symbol) {
                if(symbol != null) {
                    return TypeScript.Comment.getDocCommentText(symbol.getDocComments());
                }
                return "";
            };
            var convertSignatureGroupToSignatureInfo = function (symbol, isNew, group) {
                var result = new FormalSignatureInfo();
                result.isNew = false;
                result.name = getNameFromSymbol(symbol);
                result.docComment = getDocCommentFromSymbol(symbol);
                result.openParen = (group.flags & TypeScript.SignatureFlags.IsIndexer ? "[" : "(");
                result.closeParen = (group.flags & TypeScript.SignatureFlags.IsIndexer ? "]" : ")");
                var hasOverloads = group.signatures.length > 1;
                group.signatures.filter(function (signature) {
                    return !(hasOverloads && signature === group.definitionSignature && !_this.compilerState.getCompilationSettings().canCallDefinitionSignature);
                }).forEach(function (signature) {
                    var signatureGroupInfo = new FormalSignatureItemInfo();
                    signatureGroupInfo.docComment = (signature.declAST != null) ? TypeScript.Comment.getDocCommentText(signature.declAST.getDocComments()) : "";
                    signatureGroupInfo.returnType = (signature.returnType === null ? "any" : signature.returnType.type.getScopedTypeName(enclosingScopeContext.getScope()));
                    signature.parameters.forEach(function (p, i) {
                        var signatureParameterInfo = new FormalParameterInfo();
                        signatureParameterInfo.isVariable = (signature.hasVariableArgList) && (i === signature.parameters.length - 1);
                        signatureParameterInfo.isOptional = p.isOptional();
                        signatureParameterInfo.name = p.name;
                        signatureParameterInfo.docComment = p.getParameterDocComments();
                        signatureParameterInfo.type = p.getType().getScopedTypeName(enclosingScopeContext.getScope());
                        signatureGroupInfo.parameters.push(signatureParameterInfo);
                    });
                    result.signatureGroup.push(signatureGroupInfo);
                });
                return result;
            };
            var convertCallExprToActualSignatureInfo = function (ast, caretPosition) {
                if(!TypeScript.isValidAstNode(ast)) {
                    return null;
                }
                if(!TypeScript.isValidAstNode(ast.arguments)) {
                    return null;
                }
                var result = new ActualSignatureInfo();
                result.currentParameter = -1;
                result.openParenMinChar = ast.arguments.minChar;
                result.closeParenLimChar = Math.max(ast.arguments.minChar, ast.arguments.limChar);
                ast.arguments.members.forEach(function (arg, index) {
                    var parameter = new ActualParameterInfo();
                    parameter.minChar = arg.minChar;
                    parameter.limChar = Math.max(arg.minChar, arg.limChar);
                    result.parameters.push(parameter);
                });
                result.parameters.forEach(function (p, index) {
                    var minChar = (index == 0 ? result.openParenMinChar : result.parameters[index - 1].limChar + 1);
                    var limChar = (index == result.parameters.length - 1 ? result.closeParenLimChar : result.parameters[index + 1].minChar);
                    if(caretPosition >= minChar && (atEOF ? caretPosition <= limChar : caretPosition < limChar)) {
                        result.currentParameter = index;
                    }
                });
                return result;
            };
            var getSignatureIndex = function (ast, group) {
                if(ast == null || group == null || group.signatures == null) {
                    return -1;
                }
                return group.signatures.indexOf(ast.signature);
            };
            var getTargetSymbolWithName = function (callExpr) {
                var sym = null;
                if((callExpr.target).sym != null) {
                    sym = (callExpr.target).sym;
                } else if(callExpr.target.type.symbol !== null) {
                    var sym = callExpr.target.type.symbol;
                }
                if(sym != null) {
                    if(sym.kind() == TypeScript.SymbolKind.Type && ((sym).isMethod || (sym).isClass() || (sym).isFunction()) && (sym.name != null)) {
                        return sym;
                    } else if(sym.kind() == TypeScript.SymbolKind.Parameter) {
                        return sym;
                    } else if(sym.kind() == TypeScript.SymbolKind.Variable) {
                        return sym;
                    } else if(sym.kind() == TypeScript.SymbolKind.Field) {
                        return sym;
                    }
                }
                return null;
            };
            var symbol = getTargetSymbolWithName(callExpr);
            var result = new SignatureInfo();
            if(callExpr.nodeType === TypeScript.NodeType.Call && callExpr.target.type.call !== null) {
                result.formal = convertSignatureGroupToSignatureInfo(symbol, false, callExpr.target.type.call);
                result.actual = convertCallExprToActualSignatureInfo(callExpr, pos);
                result.activeFormal = getSignatureIndex(callExpr, callExpr.target.type.call);
            } else if(callExpr.nodeType === TypeScript.NodeType.New && callExpr.target.type.construct !== null) {
                result.formal = convertSignatureGroupToSignatureInfo(symbol, true, callExpr.target.type.construct);
                result.actual = convertCallExprToActualSignatureInfo(callExpr, pos);
                result.activeFormal = getSignatureIndex(callExpr, callExpr.target.type.construct);
            } else if(callExpr.target.nodeType === TypeScript.NodeType.Super && callExpr.target.type.symbol && callExpr.target.type.symbol.declAST) {
                var classType = callExpr.target.type.symbol.declAST.type;
                if(classType && classType.construct !== null) {
                    result.formal = convertSignatureGroupToSignatureInfo(symbol, true, classType.construct);
                    result.actual = convertCallExprToActualSignatureInfo(callExpr, pos);
                    result.activeFormal = getSignatureIndex(callExpr, classType.construct);
                } else {
                    this.logger.log("No signature group found for the target class type constructor");
                    return null;
                }
            } else {
                this.logger.log("No signature group found for the target of the call expression");
                return null;
            }
            if(result.actual == null || result.formal == null || result.activeFormal == null) {
                this.logger.log("Can't compute actual and/or formal signature of the call expression");
                return null;
            }
            return result;
        };
        LanguageService.prototype.getDefinitionAtPosition = function (fileName, pos) {
            this.refresh();
            var script = this.compilerState.getScriptAST(fileName);
            var sym = this.getSymbolAtPosition(script, pos);
            if(sym == null) {
                this.logger.log("No identifier at the specified location.");
                return null;
            }
            if(!TypeScript.isValidAstNode(sym.declAST)) {
                this.logger.log("No symbol location for identifier at the specified location.");
                return null;
            }
            var unitIndex = sym.unitIndex;
            var minChar = sym.declAST.minChar;
            var limChar = sym.declAST.limChar;
            return new DefinitionInfo(this.compilerState.mapToHostUnitIndex(unitIndex), minChar, limChar, this.getSymbolElementKind(sym), sym.name, this.getSymbolContainerKind(sym), this.getSymbolContainerName(sym));
        };
        LanguageService.prototype.getSmartIndentAtLineNumber = function (fileName, lineNumber, options) {
            this.minimalRefresh();
            var syntaxAST = this._getScriptSyntaxAST(fileName);
            var manager = new Formatting.SmartIndentManager(syntaxAST, options);
            return manager.getSmartIndentAtLineNumber(lineNumber);
        };
        LanguageService.prototype.getBraceMatchingAtPosition = function (fileName, position) {
            this.minimalRefresh();
            var syntaxAST = this._getScriptSyntaxAST(fileName);
            var manager = new Services.BraceMatchingManager(syntaxAST);
            return manager.getBraceMatchingAtPosition(position);
        };
        LanguageService.prototype.getSymbolElementKind = function (sym) {
            if(sym.declAST == null) {
                return ScriptElementKind.keyword;
            }
            return this.getDeclNodeElementKind(sym.declAST);
        };
        LanguageService.prototype.getSymbolElementKindModifiers = function (sym) {
            if(sym.declAST == null) {
                return ScriptElementKindModifier.none;
            }
            return this.getDeclNodeElementKindModifiers(sym.declAST);
        };
        LanguageService.prototype.getSymbolContainerKind = function (sym) {
            return "";
        };
        LanguageService.prototype.getSymbolContainerName = function (sym) {
            return sym.container == null ? "<global>" : sym.container.fullName();
        };
        LanguageService.prototype.getReferencesAtPosition = function (fileName, pos) {
            this.refresh();
            var context = new GetReferencesContext();
            for(var i = 0, len = this.compilerState.getScriptCount(); i < len; i++) {
                context.scope.push(i);
            }
            return this.getReferencesForSourceLocation(context, this.compilerState.getUnitIndex(fileName), pos);
        };
        LanguageService.prototype.getOccurrencesAtPosition = function (fileName, pos) {
            this.refresh();
            var unitIndex = this.compilerState.getUnitIndex(fileName);
            var context = new GetReferencesContext();
            context.scope.push(unitIndex);
            return this.getReferencesForSourceLocation(context, unitIndex, pos);
        };
        LanguageService.prototype.getImplementorsAtPosition = function (fileName, position) {
            this.refresh();
            var script = this.compilerState.getScriptAST(fileName);
            var path = this.getIdentifierPathToPosition(script, position);
            if(path === null) {
                this.logger.log("No identifier at the specified location.");
                return [];
            }
            var name = path.ast();
            var sym = name.sym;
            if(sym === null) {
                this.logger.log("No symbol annotation on the identifier AST.");
                return [];
            }
            var collector = new Services.OverridesCollector(this.getSymbolTree());
            var symbolSet = collector.findImplementors(sym);
            var references = new ReferenceEntrySet();
            symbolSet.getAll().forEach(function (x) {
                references.addSymbol(x);
            });
            return this.mapUnitIndexInReferenceEntrySet(references);
        };
        LanguageService.prototype.getReferencesForSourceLocation = function (context, unitIndex, position) {
            var script = this.compilerState.getScript(unitIndex);
            var path = this.getIdentifierPathToPosition(script, position);
            if(path === null) {
                this.logger.log("No identifier at the specified location.");
                return [];
            }
            var name = path.ast();
            var sym = name.sym;
            if(sym === null) {
                this.logger.log("No symbol annotation on the identifier AST.");
                return [];
            }
            return this.getReferencesForSymbol(context, sym);
        };
        LanguageService.prototype.isWriteAccess = function (parent, cur) {
            var write = false;
            if(parent !== null) {
                var pnt = parent.nodeType;
                switch(pnt) {
                    case TypeScript.NodeType.VarDecl:
                        if((parent).init != null) {
                            write = true;
                        }
                        break;
                    case TypeScript.NodeType.ArgDecl:
                        write = true;
                        break;
                    case TypeScript.NodeType.Asg:
                    case TypeScript.NodeType.AsgAdd:
                    case TypeScript.NodeType.AsgSub:
                    case TypeScript.NodeType.AsgMul:
                    case TypeScript.NodeType.AsgDiv:
                    case TypeScript.NodeType.AsgMod:
                    case TypeScript.NodeType.AsgOr:
                    case TypeScript.NodeType.AsgAnd:
                    case TypeScript.NodeType.AsgXor:
                    case TypeScript.NodeType.AsgLsh:
                    case TypeScript.NodeType.AsgRsh:
                    case TypeScript.NodeType.AsgRs2:
                        if((parent).operand1 === cur) {
                            write = true;
                        }
                        break;
                    case TypeScript.NodeType.IncPost:
                    case TypeScript.NodeType.IncPre:
                    case TypeScript.NodeType.DecPost:
                    case TypeScript.NodeType.DecPre:
                        write = true;
                        break;
                }
            }
            return write;
        };
        LanguageService.prototype.getReferencesForSymbol = function (context, sym) {
            var _this = this;
            var collector = new Services.OverridesCollector(this.getSymbolTree());
            var symbolSet = collector.findMemberOverrides(sym);
            var references = new ReferenceEntrySet();
            var match = function (unitIndex, parent, cur) {
                references.addAst(unitIndex, cur, _this.isWriteAccess(parent, cur));
            };
            if(sym.kind() == TypeScript.SymbolKind.Field) {
                this.logger.log("getReferencesToField");
                this.getReferencesToField(context, symbolSet, match);
            } else if(sym.kind() == TypeScript.SymbolKind.Parameter) {
                this.logger.log("getReferencesToParameter");
                this.getReferencesToParameter(context, symbolSet, match);
            } else if(sym.kind() == TypeScript.SymbolKind.Type) {
                this.logger.log("getReferencesToType");
                this.getReferencesToType(context, symbolSet, match);
            } else if(sym.kind() == TypeScript.SymbolKind.Variable) {
                this.logger.log("getReferencesToVariable");
                this.getReferencesToVariable(context, symbolSet, match);
            } else {
                this.logger.log("No recognized symbol at the specified location (" + sym.kind() + ").");
            }
            return this.mapUnitIndexInReferenceEntrySet(references);
        };
        LanguageService.prototype.mapUnitIndexInReferenceEntrySet = function (references) {
            var _this = this;
            var result = references.getEntries();
            result.forEach(function (x) {
                x.unitIndex = _this.compilerState.mapToHostUnitIndex(x.unitIndex);
            });
            return result;
        };
        LanguageService.prototype.getCompletionsAtPosition = function (fileName, pos, isMemberCompletion) {
            this.refresh();
            var result = this.getQuickCompletionsAtPosition(fileName, pos, isMemberCompletion);
            if(result == null) {
                this.refresh();
                result = this.getAccurateCompletionsAtPosition(fileName, pos, isMemberCompletion);
            }
            return result;
        };
        LanguageService.prototype.getQuickCompletionsAtPosition = function (fileName, pos, isMemberCompletion) {
            var script = this.compilerState.getScriptAST(fileName);
            var editRange = this.compilerState.getScriptEditRange(script);
            if(editRange == null) {
                this.logger.log("Full refresh required: there are no pending edits for the script. Be conservative and try again with accurate algorithm.");
                return null;
            }
            var sourceText = this.compilerState.getSourceText(script);
            var enclosingScopeContext = new TypeScript.IncrementalParser(this.logger).getEnclosingScopeContextIfSingleScopeEdit(script, fileName, sourceText, editRange);
            if(enclosingScopeContext === null) {
                this.logger.log("Full refresh required: range of edits may affect more than one scope");
                return null;
            }
            if(enclosingScopeContext.enclosingObjectLit !== null) {
                this.logger.log("Full refresh required: quick completion list does not work inside object literals, because full typecheck is required to obtain the target type of the object literal.");
                return null;
            }
            enclosingScopeContext.pos = pos;
            enclosingScopeContext.isMemberCompletion = isMemberCompletion;
            this.logger.log("Found scope context in previous script AST: " + editRange + ", pos=" + pos + ", scopePos=" + enclosingScopeContext.getScopePosition());
            var result = new CompletionInfo();
            result.maybeInaccurate = true;
            result.isMemberCompletion = isMemberCompletion;
            enclosingScopeContext.useFullAst = false;
            this.getCompletionsFromEnclosingScopeContext(enclosingScopeContext, result);
            if(result.entries.length == 0) {
                this.logger.log("Full refresh required: QuickCompletion returned an empty list. Be conservative and try again with accurate algorithm.");
                return null;
            }
            return result;
        };
        LanguageService.prototype.getAccurateCompletionsAtPosition = function (fileName, pos, isMemberCompletion) {
            var result = new CompletionInfo();
            result.maybeInaccurate = false;
            result.isMemberCompletion = isMemberCompletion;
            var script = this.compilerState.getScriptAST(fileName);
            var sourceText = this.compilerState.getSourceText(script);
            var enclosingScopeContext = TypeScript.findEnclosingScopeAt(this.logger, script, sourceText, pos, isMemberCompletion);
            if(enclosingScopeContext == null) {
                this.logger.log("No context found at the specified location.");
                return result;
            }
            this.logger.log("Found scope context in up-to-date script AST: pos=" + pos + ", scopePos=" + enclosingScopeContext.getScopePosition());
            enclosingScopeContext.useFullAst = true;
            this.getCompletionsFromEnclosingScopeContext(enclosingScopeContext, result);
            return result;
        };
        LanguageService.prototype.getCompletionsFromEnclosingScopeContext = function (enclosingScopeContext, result) {
            var _this = this;
            var getCompletions = function (isMemberCompletion) {
                result.isMemberCompletion = isMemberCompletion;
                enclosingScopeContext.isMemberCompletion = isMemberCompletion;
                var entries = _this.compilerState.getScopeEntries(enclosingScopeContext);
                entries.forEach(function (x) {
                    var entry = new CompletionEntry();
                    entry.name = x.name;
                    entry.type = x.type;
                    entry.kind = _this.getSymbolElementKind(x.sym);
                    var type = x.sym.getType();
                    if(type && type.isClass() && type.symbol.name == x.name) {
                        entry.docComment = TypeScript.Comment.getDocCommentText(type.getDocComments());
                    } else if(x.sym.declAST && x.sym.declAST.nodeType == TypeScript.NodeType.FuncDecl && type.call && type.call.signatures.length > 1) {
                        entry.docComment = TypeScript.Comment.getDocCommentTextOfSignatures(type.call.signatures);
                    } else {
                        if(x.sym.kind() == TypeScript.SymbolKind.Parameter) {
                            entry.docComment = (x.sym).getParameterDocComments();
                        } else {
                            entry.docComment = TypeScript.Comment.getDocCommentText(x.sym.getDocComments());
                        }
                    }
                    entry.kindModifiers = _this.getSymbolElementKindModifiers(x.sym);
                    result.entries.push(entry);
                });
            };
            var scriptFragment = enclosingScopeContext.getScriptFragment();
            try  {
                var path = this.getAstPathToPosition(scriptFragment, enclosingScopeContext.pos - enclosingScopeContext.getScriptFragmentPosition(), TypeScript.GetAstPathOptions.EdgeInclusive | TypeScript.GetAstPathOptions.DontPruneSearchBasedOnPosition);
                if(this.isCompletionListBlocker(path)) {
                    this.logger.log("Returning an empty list because position is inside a comment");
                } else if(this.isObjectLiteralMemberNameCompletion(enclosingScopeContext)) {
                    this.logger.log("Completion list for members of object literal");
                    return getCompletions(true);
                } else if(enclosingScopeContext.isMemberCompletion || this.isCompletionListTriggerPoint(path)) {
                    return getCompletions(enclosingScopeContext.isMemberCompletion);
                } else {
                    this.logger.log("Returning an empty list because position is not a valid position for displaying a completion list");
                }
            }finally {
                this.compilerState.cleanASTTypesForReTypeCheck(scriptFragment);
            }
        };
        LanguageService.prototype.isObjectLiteralMemberNameCompletion = function (enclosingScopeContext) {
            if(enclosingScopeContext.enclosingObjectLit === null) {
                return false;
            }
            if(enclosingScopeContext.isMemberCompletion) {
                return false;
            }
            var objectLit = enclosingScopeContext.enclosingObjectLit;
            var script = enclosingScopeContext.script;
            var pos = enclosingScopeContext.pos;
            if(!TypeScript.isValidAstNode(objectLit)) {
                return false;
            }
            if(!TypeScript.isValidAstNode(objectLit.operand)) {
                return false;
            }
            if(objectLit.operand.nodeType !== TypeScript.NodeType.List) {
                return false;
            }
            var memberList = objectLit.operand;
            var isInsideList = (memberList.minChar < pos && pos < memberList.limChar);
            if(!isInsideList) {
                return false;
            }
            if(memberList.members.length == 0) {
                return true;
            }
            var syntaxAST = new Services.ScriptSyntaxAST(this.logger, script, enclosingScopeContext.text);
            var tokenStream = new Services.TokenStreamHelper(syntaxAST.getTokenStream(memberList.minChar, memberList.limChar));
            var nameAreaMinChar = tokenStream.tokenEndPos();
            var isNameArea = false;
            var cancelSearch = false;
            if(!tokenStream.expect(TypeScript.TokenID.OpenBrace)) {
                return false;
            }
            memberList.members.forEach(function (x, i) {
                if(cancelSearch) {
                    return;
                }
                if(x.nodeType !== TypeScript.NodeType.Member) {
                    nameAreaMinChar = -1;
                    return;
                }
                var member = x;
                if(!TypeScript.isValidAstNode(member.operand1)) {
                    nameAreaMinChar = -1;
                    return;
                }
                if(nameAreaMinChar < 0) {
                    nameAreaMinChar = member.operand1.minChar;
                }
                if(!tokenStream.skipToOffset(member.operand1.limChar)) {
                    nameAreaMinChar = -1;
                    cancelSearch = true;
                    return;
                }
                if(tokenStream.tokenId() !== TypeScript.TokenID.Colon) {
                    nameAreaMinChar = -1;
                    return;
                }
                if((nameAreaMinChar) >= 0 && (nameAreaMinChar <= pos) && (pos <= tokenStream.tokenStartPos())) {
                    isNameArea = true;
                    cancelSearch = true;
                    return;
                }
                nameAreaMinChar = -1;
                if(TypeScript.isValidAstNode(member.operand2)) {
                    if(tokenStream.skipToOffset(member.operand2.limChar)) {
                        if(tokenStream.tokenId() == TypeScript.TokenID.Comma) {
                            nameAreaMinChar = tokenStream.tokenEndPos();
                            tokenStream.moveNext();
                        }
                    }
                }
            });
            if(nameAreaMinChar < 0) {
                return false;
            }
            if(isNameArea) {
                return true;
            }
            if(tokenStream.tokenId() !== TypeScript.TokenID.CloseBrace) {
                return false;
            }
            return (nameAreaMinChar <= pos) && (pos <= tokenStream.tokenStartPos());
        };
        LanguageService.prototype.isCompletionListBlocker = function (path) {
            var asts = path.asts;
            var isNodeType = function (nodeType) {
                return (path.count() >= 1) && (path.ast().nodeType === nodeType);
            };
            if(isNodeType(TypeScript.NodeType.Comment) || isNodeType(TypeScript.NodeType.Regex) || isNodeType(TypeScript.NodeType.QString)) {
                return true;
            }
            return false;
        };
        LanguageService.prototype.isCompletionListTriggerPoint = function (path) {
            var asts = path.asts;
            var isNodeType = function (nodeType) {
                return (path.count() >= 1) && (path.ast().nodeType === nodeType);
            };
            var isDecl = function (nodeType) {
                if(isNodeType(nodeType)) {
                    return true;
                }
                if(asts.length > 1 && (asts[asts.length - 2].nodeType === nodeType) && (asts[asts.length - 1].nodeType === TypeScript.NodeType.Name)) {
                    return true;
                }
                return false;
            };
            if(path.isNameOfVariable() || path.isNameOfFunction() || path.isNameOfArgument() || path.isNameOfInterface() || path.isNameOfClass() || path.isNameOfModule()) {
                return false;
            }
            if(isNodeType(TypeScript.NodeType.Member) || isNodeType(TypeScript.NodeType.TryCatch) || isNodeType(TypeScript.NodeType.Catch) || isNodeType(TypeScript.NodeType.Comment) || isNodeType(TypeScript.NodeType.Regex) || isNodeType(TypeScript.NodeType.QString)) {
                return false;
            }
            return true;
        };
        LanguageService.prototype.getFormattingEditsForRange = function (fileName, minChar, limChar, options) {
            this.minimalRefresh();
            this.formattingRulesProvider.ensureUptodate(options);
            var syntaxAST = this._getScriptSyntaxAST(fileName);
            var manager = new Formatting.FormattingManager(syntaxAST, this.formattingRulesProvider, options);
            var result = manager.FormatSelection(minChar, limChar);
            if(this.logger.information()) {
                this.logFormatCodeOptions(options);
                this.logEditResults(syntaxAST, result);
            }
            return result;
        };
        LanguageService.prototype.getFormattingEditsForDocument = function (fileName, minChar, limChar, options) {
            this.minimalRefresh();
            this.formattingRulesProvider.ensureUptodate(options);
            var syntaxAST = this._getScriptSyntaxAST(fileName);
            var manager = new Formatting.FormattingManager(syntaxAST, this.formattingRulesProvider, options);
            var result = manager.FormatDocument(minChar, limChar);
            if(this.logger.information()) {
                this.logEditResults(syntaxAST, result);
            }
            return result;
        };
        LanguageService.prototype.getFormattingEditsOnPaste = function (fileName, minChar, limChar, options) {
            this.minimalRefresh();
            this.formattingRulesProvider.ensureUptodate(options);
            var syntaxAST = this._getScriptSyntaxAST(fileName);
            var manager = new Formatting.FormattingManager(syntaxAST, this.formattingRulesProvider, options);
            var result = manager.FormatOnPaste(minChar, limChar);
            if(this.logger.information()) {
                this.logEditResults(syntaxAST, result);
            }
            return result;
        };
        LanguageService.prototype.getFormattingEditsAfterKeystroke = function (fileName, position, key, options) {
            this.minimalRefresh();
            this.formattingRulesProvider.ensureUptodate(options);
            if(key === "}") {
                var syntaxAST = this._getScriptSyntaxAST(fileName);
                var manager = new Formatting.FormattingManager(syntaxAST, this.formattingRulesProvider, options);
                return manager.FormatOnClosingCurlyBrace(position);
            } else if(key === ";") {
                var syntaxAST = this._getScriptSyntaxAST(fileName);
                var manager = new Formatting.FormattingManager(syntaxAST, this.formattingRulesProvider, options);
                return manager.FormatOnSemicolon(position);
            } else if(key === "\n") {
                var syntaxAST = this._getScriptSyntaxAST(fileName);
                var manager = new Formatting.FormattingManager(syntaxAST, this.formattingRulesProvider, options);
                return manager.FormatOnEnter(position);
            }
            return [];
        };
        LanguageService.prototype.getNavigateToItems = function (searchValue) {
            this.refresh();
            var terms = searchValue.split(" ");
            for(var i = 0; i < terms.length; i++) {
                terms[i] = terms[i].trim().toLocaleLowerCase();
            }
            var match = function (ast, parent, name) {
                name = name.toLocaleLowerCase();
                for(var i = 0; i < terms.length; i++) {
                    var term = terms[i];
                    if(name === term) {
                        return MatchKind.exact;
                    }
                    if(name.indexOf(term) == 0) {
                        return MatchKind.prefix;
                    }
                    if(name.indexOf(term) > 0) {
                        return MatchKind.subString;
                    }
                }
                return null;
            };
            var result = [];
            for(var i = 0, len = this.compilerState.getScriptCount(); i < len; i++) {
                var script = this.compilerState.getScript(i);
                var scriptId = script.locationInfo.filename;
                var matchKind = match(null, script, scriptId);
                if(matchKind != null) {
                    var item = new NavigateToItem();
                    item.name = scriptId;
                    item.matchKind = matchKind;
                    item.kind = ScriptElementKind.scriptElement;
                    item.unitIndex = this.compilerState.mapToHostUnitIndex(i);
                    item.minChar = script.minChar;
                    item.limChar = script.limChar;
                    result.push(item);
                }
                var items = this.getASTItems(i, script, match);
                for(var j = 0; j < items.length; j++) {
                    result.push(items[j]);
                }
            }
            return result;
        };
        LanguageService.prototype.getScriptLexicalStructure = function (fileName) {
            this.refresh();
            var script = this.compilerState.getScriptAST(fileName);
            return this.getASTItems(script.locationInfo.unitIndex, script, function (name) {
                return MatchKind.exact;
            });
        };
        LanguageService.prototype.getOutliningRegions = function (fileName) {
            this.refresh();
            var script = this.compilerState.getScriptAST(fileName);
            var maxLim = function (current) {
                var asts = [];
                for (var _i = 0; _i < (arguments.length - 1); _i++) {
                    asts[_i] = arguments[_i + 1];
                }
                var maxLim1 = function (current, asts) {
                    var result = current;
                    for(var i = 0; i < asts.length; i++) {
                        var ast = asts[i];
                        if(ast != null && ast.limChar != 0 && ast.limChar > result) {
                            result = ast.limChar;
                        }
                    }
                    return result;
                };
                var result = maxLim1(current, asts);
                for(var i = 0; i < asts.length; i++) {
                    var ast = asts[i];
                    if(ast != null && ast.nodeType == TypeScript.NodeType.List) {
                        result = maxLim1(result, (ast).members);
                    }
                }
                return result;
            };
            var findMinChar = function (parent, ast) {
                var result = ast.minChar;
                switch(ast.nodeType) {
                    case TypeScript.NodeType.FuncDecl:
                        result = maxLim(result, (ast).name, (ast).arguments, (ast).returnTypeAnnotation);
                        break;
                    case TypeScript.NodeType.ModuleDeclaration:
                        result = maxLim(result, (ast).name);
                        break;
                    case TypeScript.NodeType.ClassDeclaration:
                        result = maxLim(result, (ast).name, (ast).extendsList, (ast).implementsList);
                        break;
                    case TypeScript.NodeType.InterfaceDeclaration:
                        result = maxLim(result, (ast).name, (ast).extendsList, (ast).implementsList);
                        break;
                }
                return result;
            };
            var findLimChar = function (parent, ast) {
                return ast.limChar;
            };
            var match = function (parent, ast, name) {
                switch(ast.nodeType) {
                    case TypeScript.NodeType.FuncDecl:
                        if((ast).bod == null) {
                            return MatchKind.none;
                        }
                    case TypeScript.NodeType.ClassDeclaration:
                    case TypeScript.NodeType.ModuleDeclaration:
                    case TypeScript.NodeType.InterfaceDeclaration:
                        return MatchKind.exact;
                    default:
                        return null;
                }
            };
            return this.getASTItems(script.locationInfo.unitIndex, script, match, findMinChar, findLimChar);
        };
        LanguageService.prototype.logAST = function (fileName) {
            this.refresh();
            var script = this.compilerState.getScriptAST(fileName);
            new TypeScript.AstLogger(this.logger).logScript(script);
        };
        LanguageService.prototype.logSyntaxAST = function (fileName) {
            this.minimalRefresh();
            var syntaxAST = this._getScriptSyntaxAST(fileName);
            new TypeScript.AstLogger(this.logger).logScript(syntaxAST.getScript());
        };
        LanguageService.prototype.getErrors = function (maxCount) {
            this.compilerState.refresh(false);
            return this.compilerState.getErrorEntries(maxCount, function (u, e) {
                return true;
            });
        };
        LanguageService.prototype.getScriptErrors = function (fileName, maxCount) {
            this.refresh();
            var unitIndex = this.compilerState.getUnitIndex(fileName);
            return this.compilerState.getErrorEntries(maxCount, function (u, e) {
                return u === unitIndex;
            });
        };
        LanguageService.prototype.getEmitOutput = function (fileName) {
            this.refresh();
            return this.compilerState.getEmitOutput(fileName);
        };
        LanguageService.prototype.getTypeInfoAtPosition = function (pos, script) {
            var result = null;
            var resultASTs = [];
            var pre = function (cur, parent) {
                if(TypeScript.isValidAstNode(cur)) {
                    if(pos >= cur.minChar && pos < cur.limChar) {
                        var previous = resultASTs[resultASTs.length - 1];
                        if(previous == undefined || (cur.minChar >= previous.minChar && cur.limChar <= previous.limChar)) {
                            resultASTs.push(cur);
                            if((cur).sym != null && (cur).sym.getType() != null) {
                                result = {
                                    ast: cur,
                                    type: (cur).sym.getType()
                                };
                            } else if(cur.type != null) {
                                result = {
                                    ast: cur,
                                    type: cur.type
                                };
                            }
                        }
                    }
                }
                return cur;
            };
            TypeScript.getAstWalkerFactory().walk(script, pre);
            return result;
        };
        LanguageService.prototype.getNameOrDottedNameSpanFromPosition = function (pos, script) {
            var result = null;
            var pre = function (cur, parent) {
                if(TypeScript.isValidAstNode(cur)) {
                    if(pos >= cur.minChar && pos < cur.limChar) {
                        if(cur.nodeType == TypeScript.NodeType.Dot) {
                            if(result == null) {
                                result = new SpanInfo(cur.minChar, cur.limChar);
                            }
                        } else if(cur.nodeType == TypeScript.NodeType.Name) {
                            if(result == null) {
                                result = new SpanInfo(cur.minChar, cur.limChar);
                            } else {
                                result.limChar = cur.limChar;
                            }
                        } else if(cur.nodeType == TypeScript.NodeType.QString || cur.nodeType == TypeScript.NodeType.This || cur.nodeType == TypeScript.NodeType.Super) {
                            result = new SpanInfo(cur.minChar, cur.limChar);
                        }
                    }
                }
                return cur;
            };
            TypeScript.getAstWalkerFactory().walk(script, pre);
            return result;
        };
        LanguageService.prototype.getReferencesToField = function (context, symbolSet, match) {
            var fieldMatch = function (unitIndex, parent, cur) {
                if(cur.nodeType == TypeScript.NodeType.Name) {
                    match(unitIndex, parent, cur);
                }
            };
            return this.getReferencesToSymbolSet(context, symbolSet, fieldMatch);
        };
        LanguageService.prototype.getReferencesToType = function (context, symbolSet, match) {
            var typeMatch = function (unitIndex, parent, cur) {
                if(cur.nodeType == TypeScript.NodeType.Name) {
                    match(unitIndex, parent, cur);
                }
            };
            return this.getReferencesToSymbolSet(context, symbolSet, typeMatch);
        };
        LanguageService.prototype.getReferencesToParameter = function (context, symbolSet, match) {
            var parameterMatch = function (unitIndex, parent, cur) {
                if(cur.nodeType == TypeScript.NodeType.Name) {
                    match(unitIndex, parent, cur);
                }
            };
            return this.getReferencesToSymbolSet(context, symbolSet, parameterMatch);
        };
        LanguageService.prototype.getReferencesToVariable = function (context, symbolSet, match) {
            var variableMatch = function (unitIndex, parent, cur) {
                if(cur.nodeType == TypeScript.NodeType.Name) {
                    match(unitIndex, parent, cur);
                }
            };
            return this.getReferencesToSymbolSet(context, symbolSet, variableMatch);
        };
        LanguageService.prototype.getReferencesToSymbolSet = function (context, symbolSet, match) {
            var _this = this;
            var processScript = function (unitIndex) {
                var pre = function (cur, parent) {
                    if(TypeScript.isValidAstNode(cur)) {
                        var sym = (cur).sym;
                        if(sym != null && symbolSet.contains(sym)) {
                            match(unitIndex, parent, cur);
                        }
                    }
                    return cur;
                };
                TypeScript.getAstWalkerFactory().walk(_this.compilerState.getScript(unitIndex), pre);
            };
            for(var i = 0, len = context.scope.length; i < len; i++) {
                processScript(context.scope[i]);
            }
        };
        LanguageService.prototype.getDeclNodeElementKind = function (ast) {
            switch(ast.nodeType) {
                case TypeScript.NodeType.InterfaceDeclaration:
                    return ScriptElementKind.interfaceElement;
                case TypeScript.NodeType.ClassDeclaration:
                    return ScriptElementKind.classElement;
                case TypeScript.NodeType.ModuleDeclaration:
                    var moduleDecl = ast;
                    var isEnum = moduleDecl.isEnum();
                    return isEnum ? ScriptElementKind.enumElement : ScriptElementKind.moduleElement;
                case TypeScript.NodeType.VarDecl:
                    var varDecl = ast;
                    return (varDecl.isProperty() ? ScriptElementKind.memberVariableElement : ScriptElementKind.variableElement);
                case TypeScript.NodeType.ArgDecl:
                    var argDecl = ast;
                    return (argDecl.isProperty() ? ScriptElementKind.memberVariableElement : ScriptElementKind.variableElement);
                case TypeScript.NodeType.FuncDecl:
                    var funcDecl = ast;
                    if(funcDecl.isGetAccessor()) {
                        return ScriptElementKind.memberGetAccessorElement;
                    } else if(funcDecl.isSetAccessor()) {
                        return ScriptElementKind.memberSetAccessorElement;
                    } else if(funcDecl.isCallMember()) {
                        return ScriptElementKind.callSignatureElement;
                    } else if(funcDecl.isIndexerMember()) {
                        return ScriptElementKind.indexSignatureElement;
                    } else if(funcDecl.isConstructMember()) {
                        return ScriptElementKind.constructSignatureElement;
                    } else if(funcDecl.isConstructor) {
                        return ScriptElementKind.constructorImplementationElement;
                    } else if(funcDecl.isMethod()) {
                        return ScriptElementKind.memberFunctionElement;
                    } else {
                        return ScriptElementKind.functionElement;
                    }
                default:
                    if(this.logger.warning()) {
                        this.logger.log("Warning: unrecognized AST node type: " + (TypeScript.NodeType)._map[ast.nodeType]);
                    }
                    return ScriptElementKind.unknown;
            }
        };
        LanguageService.prototype.getDeclNodeElementKindModifiers = function (ast) {
            var addModifier = function (result, testValue, value) {
                if(!testValue) {
                    return result;
                }
                if(result === ScriptElementKindModifier.none) {
                    return value;
                } else {
                    return result + "," + value;
                }
            };
            var typeDeclToKindModifiers = function (decl) {
                var result = ScriptElementKindModifier.none;
                result = addModifier(result, decl.isExported(), ScriptElementKindModifier.exportedModifier);
                result = addModifier(result, decl.isAmbient(), ScriptElementKindModifier.ambientModifier);
                return result;
            };
            var classDeclToKindModifiers = function (decl) {
                var result = ScriptElementKindModifier.none;
                result = addModifier(result, decl.isExported(), ScriptElementKindModifier.exportedModifier);
                result = addModifier(result, decl.isAmbient(), ScriptElementKindModifier.ambientModifier);
                return result;
            };
            var moduleDeclToKindModifiers = function (decl) {
                var result = ScriptElementKindModifier.none;
                result = addModifier(result, decl.isExported(), ScriptElementKindModifier.exportedModifier);
                result = addModifier(result, decl.isAmbient(), ScriptElementKindModifier.ambientModifier);
                return result;
            };
            var varDeclToKindModifiers = function (decl) {
                var result = ScriptElementKindModifier.none;
                result = addModifier(result, decl.isExported(), ScriptElementKindModifier.exportedModifier);
                result = addModifier(result, decl.isAmbient(), ScriptElementKindModifier.ambientModifier);
                result = addModifier(result, decl.isPublic(), ScriptElementKindModifier.publicMemberModifier);
                result = addModifier(result, decl.isPrivate(), ScriptElementKindModifier.privateMemberModifier);
                result = addModifier(result, decl.isStatic(), ScriptElementKindModifier.staticModifier);
                return result;
            };
            var argDeclToKindModifiers = function (decl) {
                var result = ScriptElementKindModifier.none;
                result = addModifier(result, decl.isPublic(), ScriptElementKindModifier.publicMemberModifier);
                result = addModifier(result, decl.isPrivate(), ScriptElementKindModifier.privateMemberModifier);
                return result;
            };
            var funcDeclToKindModifiers = function (decl) {
                var result = ScriptElementKindModifier.none;
                result = addModifier(result, decl.isExported(), ScriptElementKindModifier.exportedModifier);
                result = addModifier(result, decl.isAmbient(), ScriptElementKindModifier.ambientModifier);
                result = addModifier(result, decl.isPublic(), ScriptElementKindModifier.publicMemberModifier);
                result = addModifier(result, decl.isPrivate(), ScriptElementKindModifier.privateMemberModifier);
                result = addModifier(result, decl.isStatic(), ScriptElementKindModifier.staticModifier);
                return result;
            };
            switch(ast.nodeType) {
                case TypeScript.NodeType.InterfaceDeclaration:
                    var typeDecl = ast;
                    return typeDeclToKindModifiers(typeDecl);
                case TypeScript.NodeType.ClassDeclaration:
                    var classDecl = ast;
                    return classDeclToKindModifiers(classDecl);
                case TypeScript.NodeType.ModuleDeclaration:
                    var moduleDecl = ast;
                    return moduleDeclToKindModifiers(moduleDecl);
                case TypeScript.NodeType.VarDecl:
                    var varDecl = ast;
                    return varDeclToKindModifiers(varDecl);
                case TypeScript.NodeType.ArgDecl:
                    var argDecl = ast;
                    return argDeclToKindModifiers(argDecl);
                case TypeScript.NodeType.FuncDecl:
                    var funcDecl = ast;
                    return funcDeclToKindModifiers(funcDecl);
                default:
                    if(this.logger.warning()) {
                        this.logger.log("Warning: unrecognized AST node type: " + (TypeScript.NodeType)._map[ast.nodeType]);
                    }
                    return ScriptElementKindModifier.none;
            }
        };
        LanguageService.prototype.getASTItems = function (unitIndex, ast, match, findMinChar, findLimChar) {
            var _this = this;
            if(findMinChar == null) {
                findMinChar = function (parent, ast) {
                    return ast.minChar;
                };
            }
            if(findLimChar == null) {
                findLimChar = function (parent, ast) {
                    return ast.limChar;
                };
            }
            var context = new NavigateToContext();
            context.unitIndex = unitIndex;
            var addItem = function (parent, ast, name, kind) {
                if(!TypeScript.isValidAstNode(ast)) {
                    return null;
                }
                var matchKind = match(parent, ast, name);
                var minChar = findMinChar(parent, ast);
                var limChar = findLimChar(parent, ast);
                if(matchKind != null && minChar >= 0 && limChar >= 0 && limChar >= minChar) {
                    var item = new NavigateToItem();
                    item.name = name;
                    item.matchKind = matchKind;
                    item.kind = kind;
                    item.kindModifiers = _this.getDeclNodeElementKindModifiers(ast);
                    item.unitIndex = _this.compilerState.mapToHostUnitIndex(context.unitIndex);
                    item.minChar = minChar;
                    item.limChar = limChar;
                    item.containerName = (TypeScript.lastOf(context.containerSymbols) === null ? "" : TypeScript.lastOf(context.containerSymbols).fullName());
                    item.containerKind = TypeScript.lastOf(context.containerKinds) === null ? "" : TypeScript.lastOf(context.containerKinds);
                    return item;
                }
                return null;
            };
            var getLimChar = function (ast) {
                return (ast == null ? -1 : ast.limChar);
            };
            var pre = function (ast, parent, walker) {
                context.path.push(ast);
                if(!TypeScript.isValidAstNode(ast)) {
                    return ast;
                }
                var item = null;
                switch(ast.nodeType) {
                    case TypeScript.NodeType.InterfaceDeclaration:
 {
                            var typeDecl = ast;
                            item = addItem(parent, typeDecl, typeDecl.name.actualText, ScriptElementKind.interfaceElement);
                            context.containerASTs.push(ast);
                            context.containerSymbols.push(typeDecl.type.symbol);
                            context.containerKinds.push("interface");
                        }
                        break;
                    case TypeScript.NodeType.ClassDeclaration:
 {
                            var classDecl = ast;
                            item = addItem(parent, classDecl, classDecl.name.actualText, ScriptElementKind.classElement);
                            context.containerASTs.push(ast);
                            context.containerSymbols.push(classDecl.type.symbol);
                            context.containerKinds.push("class");
                        }
                        break;
                    case TypeScript.NodeType.ModuleDeclaration:
 {
                            var moduleDecl = ast;
                            var isEnum = moduleDecl.isEnum();
                            var kind = isEnum ? ScriptElementKind.enumElement : ScriptElementKind.moduleElement;
                            item = addItem(parent, moduleDecl, moduleDecl.name.actualText, kind);
                            context.containerASTs.push(ast);
                            context.containerSymbols.push(moduleDecl.mod.symbol);
                            context.containerKinds.push(kind);
                        }
                        break;
                    case TypeScript.NodeType.VarDecl:
 {
                            var varDecl = ast;
                            if(varDecl.id !== null) {
                                if(varDecl.isProperty()) {
                                    item = addItem(parent, varDecl, varDecl.id.actualText, ScriptElementKind.memberVariableElement);
                                } else if(context.path.isChildOfScript() || context.path.isChildOfModule()) {
                                    item = addItem(parent, varDecl, varDecl.id.actualText, ScriptElementKind.variableElement);
                                }
                            }
                        }
                        walker.options.goChildren = false;
                        break;
                    case TypeScript.NodeType.ArgDecl:
 {
                            var argDecl = ast;
                            if(argDecl.id !== null) {
                                if(context.path.isArgumentOfClassConstructor()) {
                                    if(argDecl.isProperty()) {
                                        item = addItem(parent, argDecl, argDecl.id.actualText, ScriptElementKind.memberVariableElement);
                                    }
                                }
                            }
                        }
                        walker.options.goChildren = false;
                        break;
                    case TypeScript.NodeType.FuncDecl:
 {
                            var funcDecl = ast;
                            var kind = null;
                            var name = (funcDecl.name !== null ? funcDecl.name.actualText : null);
                            if(funcDecl.isGetAccessor()) {
                                kind = ScriptElementKind.memberGetAccessorElement;
                            } else if(funcDecl.isSetAccessor()) {
                                kind = ScriptElementKind.memberSetAccessorElement;
                            } else if(funcDecl.isCallMember()) {
                                kind = ScriptElementKind.callSignatureElement;
                                name = "()";
                            } else if(funcDecl.isIndexerMember()) {
                                kind = ScriptElementKind.indexSignatureElement;
                                name = "[]";
                            } else if(funcDecl.isConstructMember()) {
                                kind = ScriptElementKind.constructSignatureElement;
                                name = "new()";
                            } else if(funcDecl.isConstructor) {
                                kind = ScriptElementKind.constructorImplementationElement;
                                name = "constructor";
                            } else if(funcDecl.isMethod()) {
                                kind = ScriptElementKind.memberFunctionElement;
                            } else if(context.path.isChildOfScript() || context.path.isChildOfModule()) {
                                kind = ScriptElementKind.functionElement;
                            }
                            if(kind !== null && name !== null) {
                                item = addItem(parent, funcDecl, name, kind);
                            }
                        }
                        break;
                    case TypeScript.NodeType.ObjectLit:
                        walker.options.goChildren = false;
                        break;
                }
                if(item !== null) {
                    context.result.push(item);
                }
                return ast;
            };
            var post = function (ast, parent) {
                context.path.pop();
                if(ast === TypeScript.lastOf(context.containerASTs)) {
                    context.containerASTs.pop();
                    context.containerSymbols.pop();
                    context.containerKinds.pop();
                }
                return ast;
            };
            TypeScript.getAstWalkerFactory().walk(ast, pre, post);
            return context.result;
        };
        LanguageService.prototype.getAstPathToPosition = function (script, pos, options) {
            if (typeof options === "undefined") { options = TypeScript.GetAstPathOptions.Default; }
            if(this.logger.information()) {
                this.logger.log("getAstPathToPosition(" + script + ", " + pos + ")");
            }
            var path = TypeScript.getAstPathToPosition(script, pos, options);
            if(this.logger.information()) {
                if(path.count() == 0) {
                    this.logger.log("getAstPathToPosition: no ast found at position");
                } else {
                    new TypeScript.AstLogger(this.logger).logNode(script, path.ast(), 0);
                }
            }
            return path;
        };
        LanguageService.prototype.getIdentifierPathToPosition = function (script, pos) {
            this.logger.log("getIdentifierPathToPosition(" + script + ", " + pos + ")");
            var path = this.getAstPathToPosition(script, pos, TypeScript.GetAstPathOptions.EdgeInclusive);
            if(path.count() == 0) {
                return null;
            }
            if(path.nodeType() !== TypeScript.NodeType.Name) {
                return null;
            }
            return path;
        };
        LanguageService.prototype.getSymbolAtPosition = function (script, pos) {
            this.logger.log("getSymbolAtPosition(" + script + ", " + pos + ")");
            var path = this.getAstPathToPosition(script, pos);
            if(path.count() == 0) {
                return null;
            }
            var finalAST = path.ast();
            if(finalAST == null) {
                return null;
            }
            if(finalAST.sym) {
                return finalAST.sym;
            }
            if(finalAST.nodeType == TypeScript.NodeType.Dot && finalAST.operand2.sym) {
                return finalAST.operand2.sym;
            }
            if(finalAST.signature && finalAST.signature.returnType && finalAST.signature.returnType.type && finalAST.signature.returnType.type.symbol) {
                return finalAST.signature.returnType.type.symbol;
            }
            if(finalAST.type && finalAST.type.symbol) {
                return finalAST.type.symbol;
            }
            return null;
        };
        LanguageService.prototype.logFormatCodeOptions = function (options) {
            if(this.logger.information()) {
                this.logger.log("options.InsertSpaceAfterCommaDelimiter=" + options.InsertSpaceAfterCommaDelimiter);
                this.logger.log("options.InsertSpaceAfterSemicolonInForStatements=" + options.InsertSpaceAfterSemicolonInForStatements);
                this.logger.log("options.InsertSpaceBeforeAndAfterBinaryOperators=" + options.InsertSpaceBeforeAndAfterBinaryOperators);
                this.logger.log("options.InsertSpaceAfterKeywordsInControlFlowStatements=" + options.InsertSpaceAfterKeywordsInControlFlowStatements);
                this.logger.log("options.InsertSpaceAfterFunctionKeywordForAnonymousFunctions=" + options.InsertSpaceAfterFunctionKeywordForAnonymousFunctions);
                this.logger.log("options.InsertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis=" + options.InsertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis);
                this.logger.log("options.PlaceOpenBraceOnNewLineForFunctions=" + options.PlaceOpenBraceOnNewLineForFunctions);
                this.logger.log("options.PlaceOpenBraceOnNewLineForControlBlocks=" + options.PlaceOpenBraceOnNewLineForControlBlocks);
            }
        };
        LanguageService.prototype.logEditResults = function (syntaxAST, result) {
            var _this = this;
            if(this.logger.information()) {
                var logSourceText = function (text) {
                    var textLines = text.replace(/^\s+|\s+$/g, "").replace(/\r\n?/g, "\n").split(/\n/);
                    for(var i = 0; i < textLines.length; i++) {
                        var textLine = textLines[i];
                        var msg = "line #" + i + "(length=" + textLine.length + "): \"" + textLine + "\"";
                        _this.logger.log(msg);
                    }
                };
                var sourceText = syntaxAST.getSourceText();
                logSourceText(sourceText.getText(0, sourceText.getLength()));
                for(var i = 0; i < result.length; i++) {
                    var edit = result[i];
                    var oldSourceText = sourceText.getText(edit.minChar, edit.limChar);
                    var text = "edit #" + i + ": minChar=" + edit.minChar + ", " + "limChar=" + edit.limChar + ", " + "oldText=\"" + TypeScript.stringToLiteral(oldSourceText, 30) + "\", " + "textLength=" + edit.text.length + ", " + "text=\"" + TypeScript.stringToLiteral(edit.text, 30) + "\"";
                    this.logger.log(text);
                }
            }
        };
        return LanguageService;
    })();
    Services.LanguageService = LanguageService;    
})(Services || (Services = {}));
