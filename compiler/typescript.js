var TypeScript;
(function (TypeScript) {
    (function (UpdateUnitKind) {
        UpdateUnitKind._map = [];
        UpdateUnitKind._map[0] = "Unknown";
        UpdateUnitKind.Unknown = 0;
        UpdateUnitKind._map[1] = "NoEdits";
        UpdateUnitKind.NoEdits = 1;
        UpdateUnitKind._map[2] = "EditsInsideSingleScope";
        UpdateUnitKind.EditsInsideSingleScope = 2;
    })(TypeScript.UpdateUnitKind || (TypeScript.UpdateUnitKind = {}));
    var UpdateUnitKind = TypeScript.UpdateUnitKind;
    var ScriptEditRange = (function () {
        function ScriptEditRange(minChar, limChar, delta) {
            this.minChar = minChar;
            this.limChar = limChar;
            this.delta = delta;
        }
        ScriptEditRange.unknown = function unknown() {
            return new ScriptEditRange(-1, -1, -1);
        };
        ScriptEditRange.prototype.isUnknown = function () {
            return this.minChar === -1 && this.limChar === -1 && this.delta === -1;
        };
        ScriptEditRange.prototype.containsPosition = function (pos) {
            return (this.minChar <= pos && pos < this.limChar) || (this.minChar <= pos && pos < this.limChar + this.delta);
        };
        ScriptEditRange.prototype.toString = function () {
            return "editRange(minChar=" + this.minChar + ", limChar=" + this.limChar + ", delta=" + this.delta + ")";
        };
        return ScriptEditRange;
    })();
    TypeScript.ScriptEditRange = ScriptEditRange;    
    var UpdateUnitResult = (function () {
        function UpdateUnitResult(kind, unitIndex, script1, script2) {
            this.kind = kind;
            this.unitIndex = unitIndex;
            this.script1 = script1;
            this.script2 = script2;
            this.scope1 = null;
            this.scope2 = null;
            this.editRange = null;
            this.parseErrors = [];
        }
        UpdateUnitResult.noEdits = function noEdits(unitIndex) {
            return new UpdateUnitResult(UpdateUnitKind.NoEdits, unitIndex, null, null);
        };
        UpdateUnitResult.unknownEdits = function unknownEdits(script1, script2, parseErrors) {
            var result = new UpdateUnitResult(UpdateUnitKind.Unknown, script1.locationInfo.unitIndex, script1, script2);
            result.parseErrors = parseErrors;
            return result;
        };
        UpdateUnitResult.singleScopeEdits = function singleScopeEdits(script1, script2, scope1, scope2, editRange, parseErrors) {
            var result = new UpdateUnitResult(UpdateUnitKind.EditsInsideSingleScope, script1.locationInfo.unitIndex, script1, script2);
            result.scope1 = scope1;
            result.scope2 = scope2;
            result.editRange = editRange;
            result.parseErrors = parseErrors;
            return result;
        };
        return UpdateUnitResult;
    })();
    TypeScript.UpdateUnitResult = UpdateUnitResult;    
    var ErrorEntry = (function () {
        function ErrorEntry(unitIndex, minChar, limChar, message) {
            this.unitIndex = unitIndex;
            this.minChar = minChar;
            this.limChar = limChar;
            this.message = message;
        }
        return ErrorEntry;
    })();
    TypeScript.ErrorEntry = ErrorEntry;    
    TypeScript.defaultSettings = new TypeScript.CompilationSettings();
    var TypeScriptCompiler = (function () {
        function TypeScriptCompiler(errorOutput, logger, settings) {
            if (typeof logger === "undefined") { logger = new TypeScript.NullLogger(); }
            if (typeof settings === "undefined") { settings = TypeScript.defaultSettings; }
            this.errorOutput = errorOutput;
            this.logger = logger;
            this.settings = settings;
            this.parser = new TypeScript.Parser();
            this.typeFlow = null;
            this.scripts = new TypeScript.ASTList();
            this.units = new Array();
            this.errorReporter = new TypeScript.ErrorReporter(this.errorOutput);
            this.persistentTypeState = new TypeScript.PersistentGlobalTypeState(this.errorReporter);
            this.errorReporter.parser = this.parser;
            this.initTypeChecker(this.errorOutput);
            this.parser.style_requireSemi = this.settings.styleSettings.requireSemi;
            this.parser.style_funcInLoop = this.settings.styleSettings.funcInLoop;
            this.parser.inferPropertiesFromThisAssignment = this.settings.inferPropertiesFromThisAssignment;
            this.emitSettings = new TypeScript.EmitOptions(this.settings);
            TypeScript.codeGenTarget = settings.codeGenTarget;
        }
        TypeScriptCompiler.prototype.timeFunction = function (funcDescription, func) {
            return TypeScript.timeFunction(this.logger, funcDescription, func);
        };
        TypeScriptCompiler.prototype.initTypeChecker = function (errorOutput) {
            this.persistentTypeState.refreshPersistentState();
            this.typeChecker = new TypeScript.TypeChecker(this.persistentTypeState);
            this.typeChecker.errorReporter = this.errorReporter;
            this.typeChecker.checkControlFlow = this.settings.controlFlow;
            this.typeChecker.checkControlFlowUseDef = this.settings.controlFlowUseDef;
            this.typeChecker.printControlFlowGraph = this.settings.printControlFlow;
            this.typeChecker.errorsOnWith = this.settings.errorOnWith;
            this.typeChecker.styleSettings = this.settings.styleSettings;
            this.typeChecker.canCallDefinitionSignature = this.settings.canCallDefinitionSignature;
            this.errorReporter.checker = this.typeChecker;
            this.setErrorOutput(this.errorOutput);
        };
        TypeScriptCompiler.prototype.setErrorOutput = function (outerr) {
            this.errorOutput = outerr;
            this.errorReporter.setErrOut(outerr);
            this.parser.outfile = outerr;
        };
        TypeScriptCompiler.prototype.emitCommentsToOutput = function () {
            this.emitSettings = new TypeScript.EmitOptions(this.settings);
        };
        TypeScriptCompiler.prototype.setErrorCallback = function (fn) {
            this.parser.errorCallback = fn;
        };
        TypeScriptCompiler.prototype.updateUnit = function (prog, filename, setRecovery) {
            return this.updateSourceUnit(new TypeScript.StringSourceText(prog), filename, setRecovery);
        };
        TypeScriptCompiler.prototype.updateSourceUnit = function (sourceText, filename, setRecovery) {
            var _this = this;
            return this.timeFunction("updateSourceUnit(" + filename + ")", function () {
                var updateResult = _this.partialUpdateUnit(sourceText, filename, setRecovery);
                return _this.applyUpdateResult(updateResult);
            });
        };
        TypeScriptCompiler.prototype.applyUpdateResult = function (updateResult) {
            switch(updateResult.kind) {
                case UpdateUnitKind.NoEdits:
                    return false;
                case UpdateUnitKind.Unknown:
                    this.scripts.members[updateResult.unitIndex] = updateResult.script2;
                    this.units[updateResult.unitIndex] = updateResult.script2.locationInfo;
                    for(var i = 0, len = updateResult.parseErrors.length; i < len; i++) {
                        var e = updateResult.parseErrors[i];
                        if(this.parser.errorCallback) {
                            this.parser.errorCallback(e.minChar, e.limChar - e.minChar, e.message, e.unitIndex);
                        }
                    }
                    return true;
                case UpdateUnitKind.EditsInsideSingleScope:
                    new TypeScript.IncrementalParser(this.logger).mergeTrees(updateResult);
                    return true;
            }
        };
        TypeScriptCompiler.prototype.partialUpdateUnit = function (sourceText, filename, setRecovery) {
            var _this = this;
            return this.timeFunction("partialUpdateUnit(" + filename + ")", function () {
                for(var i = 0, len = _this.units.length; i < len; i++) {
                    if(_this.units[i].filename == filename) {
                        if((_this.scripts.members[i]).isResident) {
                            return UpdateUnitResult.noEdits(i);
                        }
                        if(setRecovery) {
                            _this.parser.setErrorRecovery(null);
                        }
                        var updateResult;
                        var parseErrors = [];
                        var errorCapture = function (minChar, charLen, message, unitIndex) {
                            parseErrors.push(new ErrorEntry(unitIndex, minChar, minChar + charLen, message));
                        };
                        var svErrorCallback = _this.parser.errorCallback;
                        if(svErrorCallback) {
                            _this.parser.errorCallback = errorCapture;
                        }
                        var oldScript = _this.scripts.members[i];
                        var newScript = _this.parser.parse(sourceText, filename, i);
                        if(svErrorCallback) {
                            _this.parser.errorCallback = svErrorCallback;
                        }
                        updateResult = UpdateUnitResult.unknownEdits(oldScript, newScript, parseErrors);
                        return updateResult;
                    }
                }
                throw new Error("Unknown file \"" + filename + "\"");
            });
        };
        TypeScriptCompiler.prototype.addUnit = function (prog, filename, keepResident, referencedFiles) {
            if (typeof keepResident === "undefined") { keepResident = false; }
            if (typeof referencedFiles === "undefined") { referencedFiles = []; }
            return this.addSourceUnit(new TypeScript.StringSourceText(prog), filename, keepResident, referencedFiles);
        };
        TypeScriptCompiler.prototype.addSourceUnit = function (sourceText, filename, keepResident, referencedFiles) {
            if (typeof referencedFiles === "undefined") { referencedFiles = []; }
            var _this = this;
            return this.timeFunction("addSourceUnit(" + filename + ", " + keepResident + ")", function () {
                var script = _this.parser.parse(sourceText, filename, _this.units.length, TypeScript.AllowedElements.Global);
                script.referencedFiles = referencedFiles;
                script.isResident = keepResident;
                _this.persistentTypeState.setCollectionMode(keepResident ? TypeScript.TypeCheckCollectionMode.Resident : TypeScript.TypeCheckCollectionMode.Transient);
                var index = _this.units.length;
                _this.units[index] = script.locationInfo;
                _this.typeChecker.collectTypes(script);
                _this.scripts.append(script);
                return script;
            });
        };
        TypeScriptCompiler.prototype.parseUnit = function (prog, filename) {
            return this.parseSourceUnit(new TypeScript.StringSourceText(prog), filename);
        };
        TypeScriptCompiler.prototype.parseSourceUnit = function (sourceText, filename) {
            this.parser.setErrorRecovery(this.errorOutput);
            var script = this.parser.parse(sourceText, filename, 0);
            var index = this.units.length;
            this.units[index] = script.locationInfo;
            this.typeChecker.collectTypes(script);
            this.scripts.append(script);
        };
        TypeScriptCompiler.prototype.typeCheck = function () {
            var _this = this;
            return this.timeFunction("typeCheck()", function () {
                var binder = new TypeScript.Binder(_this.typeChecker);
                _this.typeChecker.units = _this.units;
                binder.bind(_this.typeChecker.globalScope, _this.typeChecker.globals);
                binder.bind(_this.typeChecker.globalScope, _this.typeChecker.ambientGlobals);
                binder.bind(_this.typeChecker.globalScope, _this.typeChecker.globalTypes);
                binder.bind(_this.typeChecker.globalScope, _this.typeChecker.ambientGlobalTypes);
                _this.typeFlow = new TypeScript.TypeFlow(_this.logger, _this.typeChecker.globalScope, _this.parser, _this.typeChecker);
                var i = 0;
                var script = null;
                var len = _this.scripts.members.length;
                _this.persistentTypeState.setCollectionMode(TypeScript.TypeCheckCollectionMode.Resident);
                for(i = 0; i < len; i++) {
                    script = _this.scripts.members[i];
                    if(!script.isResident || script.hasBeenTypeChecked) {
                        continue;
                    }
                    _this.typeFlow.assignScopes(script);
                    _this.typeFlow.initLibs();
                }
                for(i = 0; i < len; i++) {
                    script = _this.scripts.members[i];
                    if(!script.isResident || script.hasBeenTypeChecked) {
                        continue;
                    }
                    _this.typeFlow.typeCheck(script);
                    script.hasBeenTypeChecked = true;
                }
                _this.persistentTypeState.setCollectionMode(TypeScript.TypeCheckCollectionMode.Transient);
                len = _this.scripts.members.length;
                for(i = 0; i < len; i++) {
                    script = _this.scripts.members[i];
                    if(script.isResident) {
                        continue;
                    }
                    _this.typeFlow.assignScopes(script);
                    _this.typeFlow.initLibs();
                }
                for(i = 0; i < len; i++) {
                    script = _this.scripts.members[i];
                    if(script.isResident) {
                        continue;
                    }
                    _this.typeFlow.typeCheck(script);
                }
                return null;
            });
        };
        TypeScriptCompiler.prototype.cleanASTTypesForReTypeCheck = function (ast) {
            function cleanASTType(ast, parent) {
                ast.type = null;
                if(ast.nodeType == TypeScript.NodeType.VarDecl) {
                    var vardecl = ast;
                    vardecl.sym = null;
                } else if(ast.nodeType == TypeScript.NodeType.ArgDecl) {
                    var argdecl = ast;
                    argdecl.sym = null;
                } else if(ast.nodeType == TypeScript.NodeType.Name) {
                    var name = ast;
                    name.sym = null;
                } else if(ast.nodeType == TypeScript.NodeType.FuncDecl) {
                    var funcdecl = ast;
                    funcdecl.signature = null;
                    funcdecl.freeVariables = new Array();
                    funcdecl.symbols = null;
                    funcdecl.accessorSymbol = null;
                    funcdecl.scopeType = null;
                } else if(ast.nodeType == TypeScript.NodeType.ModuleDeclaration) {
                    var modDecl = ast;
                    modDecl.mod = null;
                } else if(ast.nodeType == TypeScript.NodeType.With) {
                    (ast).withSym = null;
                } else if(ast.nodeType == TypeScript.NodeType.Catch) {
                    (ast).containedScope = null;
                } else if(ast.nodeType === TypeScript.NodeType.Script) {
                    (ast).externallyVisibleImportedSymbols = [];
                }
                return ast;
            }
            TypeScript.getAstWalkerFactory().walk(ast, cleanASTType);
        };
        TypeScriptCompiler.prototype.cleanTypesForReTypeCheck = function () {
            var _this = this;
            return this.timeFunction("cleanTypesForReTypeCheck()", function () {
                for(var i = 0, len = _this.scripts.members.length; i < len; i++) {
                    var script = _this.scripts.members[i];
                    if((script).isResident) {
                        continue;
                    }
                    _this.cleanASTTypesForReTypeCheck(script);
                    _this.typeChecker.collectTypes(script);
                }
                return null;
            });
        };
        TypeScriptCompiler.prototype.attemptIncrementalTypeCheck = function (updateResult) {
            return this.timeFunction("attemptIncrementalTypeCheck()", function () {
                return false;
            });
        };
        TypeScriptCompiler.prototype.reTypeCheck = function () {
            var _this = this;
            return this.timeFunction("reTypeCheck()", function () {
                TypeScript.CompilerDiagnostics.analysisPass++;
                _this.initTypeChecker(_this.errorOutput);
                _this.persistentTypeState.setCollectionMode(TypeScript.TypeCheckCollectionMode.Transient);
                _this.cleanTypesForReTypeCheck();
                return _this.typeCheck();
            });
        };
        TypeScriptCompiler.prototype.isDynamicModuleCompilation = function () {
            for(var i = 0, len = this.scripts.members.length; i < len; i++) {
                var script = this.scripts.members[i];
                if(!script.isDeclareFile && script.topLevelMod != null) {
                    return true;
                }
            }
            return false;
        };
        TypeScriptCompiler.prototype.updateCommonDirectoryPath = function () {
            var commonComponents = [];
            var commonComponentsLength = -1;
            for(var i = 0, len = this.scripts.members.length; i < len; i++) {
                var script = this.scripts.members[i];
                if(script.emitRequired(this.emitSettings)) {
                    var fileName = script.locationInfo.filename;
                    var fileComponents = TypeScript.filePathComponents(fileName);
                    if(commonComponentsLength == -1) {
                        commonComponents = fileComponents;
                        commonComponentsLength = commonComponents.length;
                    } else {
                        var updatedPath = false;
                        for(var j = 0; j < commonComponentsLength && j < fileComponents.length; j++) {
                            if(commonComponents[j] != fileComponents[j]) {
                                commonComponentsLength = j;
                                updatedPath = true;
                                if(j == 0) {
                                    this.errorReporter.emitterError(null, "Cannot find the common subdirectory path for the input files");
                                    return;
                                }
                                break;
                            }
                        }
                        if(!updatedPath && fileComponents.length < commonComponentsLength) {
                            commonComponentsLength = fileComponents.length;
                        }
                    }
                }
            }
            this.emitSettings.commonDirectoryPath = commonComponents.slice(0, commonComponentsLength).join("/") + "/";
            if(this.emitSettings.outputOption.charAt(this.emitSettings.outputOption.length - 1) != "/") {
                this.emitSettings.outputOption += "/";
            }
        };
        TypeScriptCompiler.prototype.parseEmitOption = function (ioHost) {
            this.emitSettings.ioHost = ioHost;
            if(this.emitSettings.outputOption == "") {
                this.emitSettings.outputMany = true;
                this.emitSettings.commonDirectoryPath = "";
                return;
            }
            this.emitSettings.outputOption = TypeScript.switchToForwardSlashes(this.emitSettings.ioHost.resolvePath(this.emitSettings.outputOption));
            if(this.emitSettings.ioHost.directoryExists(this.emitSettings.outputOption)) {
                this.emitSettings.outputMany = true;
            } else if(this.emitSettings.ioHost.fileExists(this.emitSettings.outputOption)) {
                this.emitSettings.outputMany = false;
            } else {
                this.emitSettings.outputMany = !TypeScript.isJSFile(this.emitSettings.outputOption);
            }
            if(this.isDynamicModuleCompilation() && !this.emitSettings.outputMany) {
                this.errorReporter.emitterError(null, "Cannot compile dynamic modules when emitting into single file");
            }
            if(this.emitSettings.outputMany) {
                this.updateCommonDirectoryPath();
            }
        };
        TypeScriptCompiler.prototype.useUTF8ForFile = function (script) {
            if(this.emitSettings.outputMany) {
                return this.outputScriptToUTF8(script);
            } else {
                return this.outputScriptsToUTF8((this.scripts.members));
            }
        };
        TypeScriptCompiler.mapToDTSFileName = function mapToDTSFileName(fileName, wholeFileNameReplaced) {
            return TypeScript.getDeclareFilePath(fileName);
        };
        TypeScriptCompiler.prototype.canEmitDeclarations = function (script) {
            if(!this.settings.generateDeclarationFiles) {
                return false;
            }
            if(!!script && (script.isDeclareFile || script.isResident || script.bod == null)) {
                return false;
            }
            return true;
        };
        TypeScriptCompiler.prototype.emitDeclarationsUnit = function (script, reuseEmitter, declarationEmitter) {
            if(!this.canEmitDeclarations(script)) {
                return null;
            }
            if(!declarationEmitter) {
                var declareFileName = this.emitSettings.mapOutputFileName(script.locationInfo.filename, TypeScriptCompiler.mapToDTSFileName);
                var declareFile = this.createFile(declareFileName, this.useUTF8ForFile(script));
                declarationEmitter = new TypeScript.DeclarationEmitter(this.typeChecker, this.emitSettings, this.errorReporter);
                declarationEmitter.setDeclarationFile(declareFile);
            }
            declarationEmitter.emitDeclarations(script);
            if(!reuseEmitter) {
                declarationEmitter.Close();
                return null;
            } else {
                return declarationEmitter;
            }
        };
        TypeScriptCompiler.prototype.emitDeclarations = function () {
            if(!this.canEmitDeclarations()) {
                return;
            }
            if(this.errorReporter.hasErrors) {
                return;
            }
            if(this.scripts.members.length == 0) {
                return;
            }
            var declarationEmitter = null;
            for(var i = 0, len = this.scripts.members.length; i < len; i++) {
                var script = this.scripts.members[i];
                if(this.emitSettings.outputMany || declarationEmitter == null) {
                    declarationEmitter = this.emitDeclarationsUnit(script, !this.emitSettings.outputMany);
                } else {
                    this.emitDeclarationsUnit(script, true, declarationEmitter);
                }
            }
            if(declarationEmitter) {
                declarationEmitter.Close();
            }
        };
        TypeScriptCompiler.mapToFileNameExtension = function mapToFileNameExtension(extension, fileName, wholeFileNameReplaced) {
            if(wholeFileNameReplaced) {
                return fileName;
            } else {
                var splitFname = fileName.split(".");
                splitFname.pop();
                return splitFname.join(".") + extension;
            }
        };
        TypeScriptCompiler.mapToJSFileName = function mapToJSFileName(fileName, wholeFileNameReplaced) {
            return TypeScriptCompiler.mapToFileNameExtension(".js", fileName, wholeFileNameReplaced);
        };
        TypeScriptCompiler.prototype.emitUnit = function (script, reuseEmitter, emitter, inputOutputMapper) {
            if(!script.emitRequired(this.emitSettings)) {
                return null;
            }
            var fname = script.locationInfo.filename;
            if(!emitter) {
                var outFname = this.emitSettings.mapOutputFileName(fname, TypeScriptCompiler.mapToJSFileName);
                var outFile = this.createFile(outFname, this.useUTF8ForFile(script));
                emitter = new TypeScript.Emitter(this.typeChecker, outFname, outFile, this.emitSettings, this.errorReporter);
                if(this.settings.mapSourceFiles) {
                    emitter.setSourceMappings(new TypeScript.SourceMapper(fname, outFname, outFile, this.createFile(outFname + TypeScript.SourceMapper.MapFileExtension, false), this.errorReporter, this.settings.emitFullSourceMapPath));
                }
                if(inputOutputMapper) {
                    inputOutputMapper(script.locationInfo.unitIndex, outFname);
                }
            } else if(this.settings.mapSourceFiles) {
                emitter.setSourceMappings(new TypeScript.SourceMapper(fname, emitter.emittingFileName, emitter.outfile, emitter.sourceMapper.sourceMapOut, this.errorReporter, this.settings.emitFullSourceMapPath));
            }
            this.typeChecker.locationInfo = script.locationInfo;
            emitter.emitJavascript(script, TypeScript.TokenID.Comma, false);
            if(!reuseEmitter) {
                emitter.Close();
                return null;
            } else {
                return emitter;
            }
        };
        TypeScriptCompiler.prototype.emit = function (ioHost, inputOutputMapper) {
            this.parseEmitOption(ioHost);
            var emitter = null;
            for(var i = 0, len = this.scripts.members.length; i < len; i++) {
                var script = this.scripts.members[i];
                if(this.emitSettings.outputMany || emitter == null) {
                    emitter = this.emitUnit(script, !this.emitSettings.outputMany, null, inputOutputMapper);
                } else {
                    this.emitUnit(script, true, emitter);
                }
            }
            if(emitter) {
                emitter.Close();
            }
        };
        TypeScriptCompiler.prototype.emitToOutfile = function (outputFile) {
            if(this.settings.mapSourceFiles) {
                throw Error("Cannot generate source map");
            }
            if(this.settings.generateDeclarationFiles) {
                throw Error("Cannot generate declaration files");
            }
            if(this.settings.outputOption != "") {
                throw Error("Cannot parse output option");
            }
            var emitter = emitter = new TypeScript.Emitter(this.typeChecker, "stdout", outputFile, this.emitSettings, this.errorReporter);
            ;
            for(var i = 0, len = this.scripts.members.length; i < len; i++) {
                var script = this.scripts.members[i];
                this.typeChecker.locationInfo = script.locationInfo;
                emitter.emitJavascript(script, TypeScript.TokenID.Comma, false);
            }
        };
        TypeScriptCompiler.prototype.emitAST = function (ioHost) {
            this.parseEmitOption(ioHost);
            var outFile = null;
            var context = null;
            for(var i = 0, len = this.scripts.members.length; i < len; i++) {
                var script = this.scripts.members[i];
                if(this.emitSettings.outputMany || context == null) {
                    var fname = this.units[i].filename;
                    var mapToTxtFileName = function (fileName, wholeFileNameReplaced) {
                        return TypeScriptCompiler.mapToFileNameExtension(".txt", fileName, wholeFileNameReplaced);
                    };
                    var outFname = this.emitSettings.mapOutputFileName(fname, mapToTxtFileName);
                    outFile = this.createFile(outFname, this.useUTF8ForFile(script));
                    context = new TypeScript.PrintContext(outFile, this.parser);
                }
                TypeScript.getAstWalkerFactory().walk(script, TypeScript.prePrintAST, TypeScript.postPrintAST, null, context);
                if(this.emitSettings.outputMany) {
                    try  {
                        outFile.Close();
                    } catch (e) {
                        this.errorReporter.emitterError(null, e.message);
                    }
                }
            }
            if(!this.emitSettings.outputMany) {
                try  {
                    outFile.Close();
                } catch (e) {
                    this.errorReporter.emitterError(null, e.message);
                }
            }
        };
        TypeScriptCompiler.prototype.outputScriptToUTF8 = function (script) {
            return script.containsUnicodeChar || (this.emitSettings.emitComments && script.containsUnicodeCharInComment);
        };
        TypeScriptCompiler.prototype.outputScriptsToUTF8 = function (scripts) {
            for(var i = 0, len = scripts.length; i < len; i++) {
                var script = scripts[i];
                if(this.outputScriptToUTF8(script)) {
                    return true;
                }
            }
            return false;
        };
        TypeScriptCompiler.prototype.createFile = function (fileName, useUTF8) {
            try  {
                return this.emitSettings.ioHost.createFile(fileName, useUTF8);
            } catch (ex) {
                this.errorReporter.emitterError(null, ex.message);
            }
        };
        return TypeScriptCompiler;
    })();
    TypeScript.TypeScriptCompiler = TypeScriptCompiler;    
    var ScopeEntry = (function () {
        function ScopeEntry(name, type, sym) {
            this.name = name;
            this.type = type;
            this.sym = sym;
        }
        return ScopeEntry;
    })();
    TypeScript.ScopeEntry = ScopeEntry;    
    var ScopeTraversal = (function () {
        function ScopeTraversal(compiler) {
            this.compiler = compiler;
        }
        ScopeTraversal.prototype.getScope = function (enclosingScopeContext) {
            if(enclosingScopeContext.enclosingObjectLit && enclosingScopeContext.isMemberCompletion) {
                return enclosingScopeContext.getObjectLiteralScope();
            } else if(enclosingScopeContext.isMemberCompletion) {
                if(enclosingScopeContext.useFullAst) {
                    return this.compiler.typeFlow.findMemberScopeAtFullAst(enclosingScopeContext);
                } else {
                    return this.compiler.typeFlow.findMemberScopeAt(enclosingScopeContext);
                }
            } else {
                return enclosingScopeContext.getScope();
            }
        };
        ScopeTraversal.prototype.getScopeEntries = function (enclosingScopeContext, getPrettyTypeName) {
            var scope = this.getScope(enclosingScopeContext);
            if(scope == null) {
                return [];
            }
            var inScopeNames = new TypeScript.StringHashTable();
            var allSymbolNames = scope.getAllSymbolNames(enclosingScopeContext.isMemberCompletion);
            for(var i = 0; i < allSymbolNames.length; i++) {
                var name = allSymbolNames[i];
                if(name == TypeScript.globalId || name == "_Core" || name == "_element") {
                    continue;
                }
                inScopeNames.add(name, "");
            }
            var svModuleDecl = this.compiler.typeChecker.currentModDecl;
            this.compiler.typeChecker.currentModDecl = enclosingScopeContext.deepestModuleDecl;
            var result = this.getTypeNamesForNames(enclosingScopeContext, inScopeNames.getAllKeys(), scope, getPrettyTypeName);
            this.compiler.typeChecker.currentModDecl = svModuleDecl;
            return result;
        };
        ScopeTraversal.prototype.getTypeNamesForNames = function (enclosingScopeContext, allNames, scope, getPrettyTypeName) {
            var result = [];
            var enclosingScope = enclosingScopeContext.getScope();
            for(var i = 0; i < allNames.length; i++) {
                var name = allNames[i];
                var publicsOnly = enclosingScopeContext.publicsOnly && enclosingScopeContext.isMemberCompletion;
                var symbol = scope.find(name, publicsOnly, false);
                if(symbol == null) {
                    symbol = scope.find(name, publicsOnly, true);
                }
                var displayThisMember = symbol && symbol.flags & TypeScript.SymbolFlags.Private ? symbol.container == scope.container : true;
                if(symbol) {
                    if(displayThisMember && !TypeScript.isQuoted(symbol.name) && !TypeScript.isRelative(symbol.name)) {
                        var getPrettyOverload = getPrettyTypeName && symbol.declAST && symbol.declAST.nodeType == TypeScript.NodeType.FuncDecl;
                        var type = symbol.getType();
                        var typeName = type ? type.getScopedTypeName(enclosingScope, getPrettyOverload) : "";
                        result.push(new ScopeEntry(name, typeName, symbol));
                    }
                } else {
                    if(name == "true" || name == "false") {
                        result.push(new ScopeEntry(name, "bool", this.compiler.typeChecker.booleanType.symbol));
                    }
                }
            }
            return result;
        };
        return ScopeTraversal;
    })();
    TypeScript.ScopeTraversal = ScopeTraversal;    
})(TypeScript || (TypeScript = {}));
