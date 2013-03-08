var Services;
(function (Services) {
    var ScriptMap = (function () {
        function ScriptMap() {
            this.map = new TypeScript.StringHashTable();
        }
        ScriptMap.prototype.setEntry = function (id, isResident, version) {
            var entry = this.map.lookup(id);
            if(entry == null) {
                entry = new ScriptMapEntry(isResident, version);
                this.map.add(id, entry);
            } else {
                entry.isResident = isResident;
                entry.version = version;
            }
        };
        ScriptMap.prototype.getEntry = function (id) {
            return this.map.lookup(id);
        };
        return ScriptMap;
    })();
    Services.ScriptMap = ScriptMap;    
    var ScriptMapEntry = (function () {
        function ScriptMapEntry(isResident, version) {
            this.isResident = isResident;
            this.version = version;
        }
        return ScriptMapEntry;
    })();
    Services.ScriptMapEntry = ScriptMapEntry;    
    var HostCacheEntry = (function () {
        function HostCacheEntry(host, hostUnitIndex, id, version, isResident) {
            this.host = host;
            this.hostUnitIndex = hostUnitIndex;
            this.id = id;
            this.version = version;
            this.isResident = isResident;
            this._cachedSourceText = null;
            this._sourceText = null;
        }
        HostCacheEntry.prototype.getSourceText = function (cached) {
            if(cached) {
                if(this._cachedSourceText === null) {
                    this._cachedSourceText = new Services.CachedSourceTextAdapter(this.host, this.hostUnitIndex);
                }
                return this._cachedSourceText;
            } else {
                if(this._sourceText === null) {
                    this._sourceText = new Services.SourceTextAdapter(this.host, this.hostUnitIndex);
                }
                return this._sourceText;
            }
        };
        return HostCacheEntry;
    })();
    Services.HostCacheEntry = HostCacheEntry;    
    var HostCache = (function () {
        function HostCache(host) {
            this.host = host;
            this.map = new TypeScript.StringHashTable();
            this.array = [];
            this.init();
        }
        HostCache.prototype.init = function () {
            for(var i = 0, len = this.host.getScriptCount(); i < len; i++) {
                var scriptId = this.host.getScriptId(i);
                this.map.add(scriptId, i);
                this.array[i] = new HostCacheEntry(this.host, i, scriptId, this.host.getScriptVersion(i), this.host.getScriptIsResident(i));
            }
        };
        HostCache.prototype.count = function () {
            return this.map.count();
        };
        HostCache.prototype.getUnitIndex = function (scriptId) {
            var result = this.map.lookup(scriptId);
            if(result == null) {
                return -1;
            }
            return result;
        };
        HostCache.prototype.getVersion = function (scriptIndex) {
            return this.array[scriptIndex].version;
        };
        HostCache.prototype.getIsResident = function (scriptIndex) {
            return this.array[scriptIndex].isResident;
        };
        HostCache.prototype.getScriptId = function (scriptIndex) {
            return this.array[scriptIndex].id;
        };
        HostCache.prototype.getSourceText = function (scriptIndex, cached) {
            if (typeof cached === "undefined") { cached = false; }
            return this.array[scriptIndex].getSourceText(cached);
        };
        return HostCache;
    })();
    Services.HostCache = HostCache;    
    var CompilerCache = (function () {
        function CompilerCache(compiler) {
            this.compiler = compiler;
            this.map = new TypeScript.StringHashTable();
            this.init();
        }
        CompilerCache.prototype.init = function () {
            for(var i = 0, len = this.compiler.units.length; i < len; i++) {
                this.map.add(this.compiler.units[i].filename, i);
            }
        };
        CompilerCache.prototype.getUnitIndex = function (scriptId) {
            var result = this.map.lookup(scriptId);
            if(result == null) {
                return -1;
            }
            return result;
        };
        return CompilerCache;
    })();
    Services.CompilerCache = CompilerCache;    
    var UnitErrors = (function () {
        function UnitErrors() {
            this.parseErrors = [];
            this.typeCheckErrors = [];
        }
        return UnitErrors;
    })();
    Services.UnitErrors = UnitErrors;    
    var CompilerErrorCollector = (function () {
        function CompilerErrorCollector(logger) {
            this.logger = logger;
            this.parseMode = false;
            this.fileMap = [];
        }
        CompilerErrorCollector.prototype.startParsing = function (unitIndex) {
            this.parseMode = true;
            var errors = this.fileMap[unitIndex];
            if(errors !== undefined) {
                errors.parseErrors.length = 0;
            }
        };
        CompilerErrorCollector.prototype.startTypeChecking = function () {
            this.parseMode = false;
            for(var i = 0; i < this.fileMap.length; i++) {
                var errors = this.fileMap[i];
                if(errors !== undefined) {
                    errors.typeCheckErrors.length = 0;
                }
            }
        };
        CompilerErrorCollector.prototype.reportError = function (pos, len, message, unitIndex) {
            var entry = new TypeScript.ErrorEntry(unitIndex, pos, pos + len, message);
            var unitErrors = this.fileMap[unitIndex];
            if(unitErrors == undefined) {
                unitErrors = new UnitErrors();
                this.fileMap[unitIndex] = unitErrors;
            }
            if(this.parseMode) {
                unitErrors.parseErrors.push(entry);
            } else {
                unitErrors.typeCheckErrors.push(entry);
            }
        };
        return CompilerErrorCollector;
    })();
    Services.CompilerErrorCollector = CompilerErrorCollector;    
    var TextWriter = (function () {
        function TextWriter(name, useUTF8encoding) {
            this.name = name;
            this.useUTF8encoding = useUTF8encoding;
            this.text = "";
        }
        TextWriter.prototype.Write = function (s) {
            this.text += s;
        };
        TextWriter.prototype.WriteLine = function (s) {
            this.text += s + '\n';
        };
        TextWriter.prototype.Close = function () {
        };
        return TextWriter;
    })();    
    var CompilerState = (function () {
        function CompilerState(host) {
            this.host = host;
            this.logger = this.host;
            this.compiler = null;
            this.errorCollector = null;
            this.unitIndexMap = [];
            this.scriptMap = null;
            this.hostCache = null;
            this.compilerCache = null;
            this.symbolTree = null;
            this.compilationSettings = null;
        }
        CompilerState.prototype.getCompilationSettings = function () {
            return this.compilationSettings;
        };
        CompilerState.prototype.setUnitMapping = function (unitIndex, hostUnitIndex) {
            this.scriptMap.setEntry(this.hostCache.getScriptId(hostUnitIndex), this.hostCache.getIsResident(hostUnitIndex), this.hostCache.getVersion(hostUnitIndex));
            this.setUnitIndexMapping(unitIndex, hostUnitIndex);
        };
        CompilerState.prototype.setUnitIndexMapping = function (unitIndex, hostUnitIndex) {
            this.unitIndexMap[unitIndex] = hostUnitIndex;
        };
        CompilerState.prototype.onTypeCheckStarting = function () {
            this.errorCollector.startTypeChecking();
            this.symbolTree = new Services.SymbolTree(this);
        };
        CompilerState.prototype.getSymbolTree = function () {
            return this.symbolTree;
        };
        CompilerState.prototype.mapToHostUnitIndex = function (unitIndex) {
            return this.unitIndexMap[unitIndex];
        };
        CompilerState.prototype.anyType = function () {
            return this.compiler.typeFlow.anyType;
        };
        CompilerState.prototype.getScriptCount = function () {
            return this.compiler.scripts.members.length;
        };
        CompilerState.prototype.getScript = function (index) {
            return this.compiler.scripts.members[index];
        };
        CompilerState.prototype.getScripts = function () {
            return this.compiler.scripts.members;
        };
        CompilerState.prototype.getUnitIndex = function (fileName) {
            return this.compilerCache.getUnitIndex(fileName);
        };
        CompilerState.prototype.getScriptVersion = function (fileName) {
            return this.hostCache.getVersion(this.hostCache.getUnitIndex(fileName));
        };
        CompilerState.prototype.addCompilerUnit = function (compiler, hostUnitIndex) {
            var newUnitIndex = compiler.units.length;
            this.errorCollector.startParsing(newUnitIndex);
            this.setUnitMapping(newUnitIndex, hostUnitIndex);
            var newScript = compiler.addSourceUnit(this.hostCache.getSourceText(hostUnitIndex), this.hostCache.getScriptId(hostUnitIndex), this.hostCache.getIsResident(hostUnitIndex));
        };
        CompilerState.prototype.updateCompilerUnit = function (compiler, hostUnitIndex, unitIndex) {
            var scriptId = this.hostCache.getScriptId(hostUnitIndex);
            this.setUnitIndexMapping(unitIndex, hostUnitIndex);
            var previousEntry = this.scriptMap.getEntry(scriptId);
            var isResident = this.hostCache.getIsResident(hostUnitIndex);
            if(isResident) {
                return TypeScript.UpdateUnitResult.noEdits(unitIndex);
            }
            var version = this.hostCache.getVersion(hostUnitIndex);
            if(previousEntry.version === version) {
                return TypeScript.UpdateUnitResult.noEdits(unitIndex);
            }
            var result = this.attemptIncrementalUpdateUnit(scriptId);
            if(result != null) {
                return result;
            }
            var sourceText = this.hostCache.getSourceText(hostUnitIndex);
            this.setUnitMapping(unitIndex, hostUnitIndex);
            return compiler.partialUpdateUnit(sourceText, scriptId, true);
        };
        CompilerState.prototype.attemptIncrementalUpdateUnit = function (scriptId) {
            var previousScript = this.getScriptAST(scriptId);
            var newSourceText = this.getSourceText(previousScript, false);
            var editRange = this.getScriptEditRange(previousScript);
            var result = new TypeScript.IncrementalParser(this.logger).attemptIncrementalUpdateUnit(previousScript, scriptId, newSourceText, editRange);
            if(result == null) {
                return null;
            }
            if(result.kind === TypeScript.UpdateUnitKind.EditsInsideSingleScope) {
                if(result.scope1.nodeType != TypeScript.NodeType.FuncDecl) {
                    this.logger.log("  Bailing out because containing scope is not a function");
                    return null;
                }
            }
            if(true) {
                this.logger.log("  Bailing out because incremental typecheck is not implemented yet");
                return null;
            } else {
                return result;
            }
        };
        CompilerState.prototype.getHostCompilationSettings = function () {
            var settings = this.host.getCompilationSettings();
            if(settings !== null) {
                return settings;
            }
            settings = new TypeScript.CompilationSettings();
            settings.codeGenTarget = TypeScript.CodeGenTarget.ES5;
            return settings;
        };
        CompilerState.prototype.createCompiler = function () {
            var _this = this;
            var outerr = {
                Write: function (s) {
                },
                WriteLine: function (s) {
                },
                Close: function () {
                }
            };
            this.logger.log("Initializing compiler");
            this.compilationSettings = new TypeScript.CompilationSettings();
            Services.copyDataObject(this.compilationSettings, this.getHostCompilationSettings());
            this.compiler = new TypeScript.TypeScriptCompiler(outerr, this.logger, this.compilationSettings);
            this.scriptMap = new ScriptMap();
            this.unitIndexMap = [];
            this.errorCollector = new CompilerErrorCollector(this.logger);
            this.compiler.setErrorCallback(function (a, b, c, d) {
                _this.errorCollector.reportError(a, b, c, d);
            });
            this.compiler.parser.errorRecovery = true;
            for(var i = 0, length = this.host.getScriptCount(); i < length; i++) {
                this.addCompilerUnit(this.compiler, i);
            }
            this.compilerCache = new CompilerCache(this.compiler);
            this.onTypeCheckStarting();
            this.compiler.typeCheck();
        };
        CompilerState.prototype.minimalRefresh = function () {
            this.hostCache = new HostCache(this.host);
        };
        CompilerState.prototype.refresh = function (throwOnError) {
            if (typeof throwOnError === "undefined") { throwOnError = true; }
            try  {
                this.hostCache = new HostCache(this.host);
                if(!this.fullRefresh()) {
                    this.partialRefresh();
                }
                if(this.logger.information()) {
                    for(var i = 0; i < this.compiler.units.length; i++) {
                        this.logger.log("compiler unit[" + i + "].filename='" + this.compiler.units[i].filename + "'");
                    }
                    for(var i = 0; i < this.hostCache.count(); i++) {
                        this.logger.log("host script[" + i + "].filename='" + this.hostCache.getScriptId(i) + "', version=" + this.hostCache.getVersion(i));
                    }
                    for(var i = 0; i < this.unitIndexMap.length; i++) {
                        this.logger.log("unitIndexMap[" + i + "] = " + this.unitIndexMap[i]);
                    }
                }
            } catch (err) {
                var lastUnitIndex = 0;
                if(this.compiler != null) {
                    lastUnitIndex = this.compiler.units.length - 1;
                }
                this.compiler = null;
                this.logger.log("WARNING: PERF: Internal error during \"Refresh\":");
                Services.logInternalError(this.logger, err);
                this.logger.log("WARNING: PERF:    Compiler state is lost and will be re-initiliazed during next call.");
                this.errorCollector.reportError(0, 1, "Internal error: " + err.message, lastUnitIndex);
                this.errorCollector.reportError(0, 1, "Internal error: IntelliSense features are disabled. Try making edits to source files to restore a valid compilation state.", lastUnitIndex);
                if(throwOnError) {
                    throw err;
                }
            }
        };
        CompilerState.prototype.fullRefresh = function () {
            if(this.compiler == null) {
                this.logger.log("Creating new compiler instance because there is no currently active instance");
                this.createCompiler();
                return true;
            }
            if(!Services.compareDataObjects(this.compilationSettings, this.getHostCompilationSettings())) {
                this.logger.log("Creating new compiler instance because compilation settings have changed.");
                this.createCompiler();
                return true;
            }
            for(var unitIndex = 0, len = this.compiler.units.length; unitIndex < len; unitIndex++) {
                var fileName = this.compiler.units[unitIndex].filename;
                var hostUnitIndex = this.hostCache.getUnitIndex(fileName);
                if(hostUnitIndex < 0) {
                    this.logger.log("Creating new compiler instance because of unit is not part of program anymore: " + unitIndex + "-" + fileName);
                    this.createCompiler();
                    return true;
                }
            }
            for(var unitIndex = 0, len = this.compiler.units.length; unitIndex < len; unitIndex++) {
                var fileName = this.compiler.units[unitIndex].filename;
                var isResident = (this.compiler.scripts.members[unitIndex]).isResident;
                var hostUnitIndex = this.hostCache.getUnitIndex(fileName);
                if(this.hostCache.getIsResident(hostUnitIndex) != isResident) {
                    this.logger.log("Creating new compiler instance because of unit 'isResident' status has changed: " + unitIndex + "-" + fileName);
                    this.createCompiler();
                    return true;
                }
            }
            return false;
        };
        CompilerState.prototype.partialRefresh = function () {
            this.logger.log("Updating files...");
            this.compilerCache = new CompilerCache(this.compiler);
            var updateResults = [];
            function getSingleFunctionEdit(updateResults) {
                var result = null;
                for(var i = 0, len = updateResults.length; i < len; i++) {
                    var entry = updateResults[i];
                    if(entry.kind == TypeScript.UpdateUnitKind.EditsInsideSingleScope) {
                        if(result === null) {
                            result = entry;
                        } else {
                            result = null;
                            break;
                        }
                    } else if(entry.kind == TypeScript.UpdateUnitKind.Unknown) {
                        result = null;
                        break;
                    }
                }
                return result;
            }
            var fileAdded = false;
            for(var hostUnitIndex = 0, len = this.host.getScriptCount(); hostUnitIndex < len; hostUnitIndex++) {
                var fileName = this.hostCache.getScriptId(hostUnitIndex);
                var unitIndex = this.compilerCache.getUnitIndex(fileName);
                if(unitIndex >= 0) {
                    var updateResult = this.updateCompilerUnit(this.compiler, hostUnitIndex, unitIndex);
                    updateResults.push(updateResult);
                } else {
                    this.addCompilerUnit(this.compiler, hostUnitIndex);
                    fileAdded = true;
                }
            }
            var incrementalTypeCheckSuccessful = false;
            var singleEdit = getSingleFunctionEdit(updateResults);
            if(fileAdded === false && singleEdit !== null) {
                this.logger.log("Attempting incremental type check because there was a single edit to the function \"" + (singleEdit.scope1).name.actualText + "\"");
                incrementalTypeCheckSuccessful = this.attemptIncrementalTypeCheck(singleEdit);
            }
            if(!incrementalTypeCheckSuccessful) {
                var anythingUpdated = false;
                for(var i = 0, len = updateResults.length; i < len; i++) {
                    var entry = updateResults[i];
                    if(this.applyUpdateResult(entry)) {
                        anythingUpdated = true;
                    }
                }
                if(anythingUpdated) {
                    this.logger.log("Incremental type check not applicable, processing unit updates");
                    this.onTypeCheckStarting();
                    this.compiler.reTypeCheck();
                } else {
                    this.logger.log("No updates to source files, no typecheck needed");
                }
            }
        };
        CompilerState.prototype.attemptIncrementalTypeCheck = function (updateResult) {
            var success = this.compiler.attemptIncrementalTypeCheck(updateResult);
            if(success) {
                this.applyUpdateResult(updateResult);
            }
            return success;
        };
        CompilerState.prototype.applyUpdateResult = function (updateResult) {
            switch(updateResult.kind) {
                case TypeScript.UpdateUnitKind.NoEdits:
                    return false;
                case TypeScript.UpdateUnitKind.Unknown:
                case TypeScript.UpdateUnitKind.EditsInsideSingleScope:
                    this.errorCollector.startParsing(updateResult.unitIndex);
                    return this.compiler.applyUpdateResult(updateResult);
            }
        };
        CompilerState.prototype.getScriptAST = function (fileName) {
            var unitIndex = this.compilerCache.getUnitIndex(fileName);
            if(unitIndex < 0) {
                throw new Error("Interal error: No AST found for file \"" + fileName + "\".");
            }
            return this.compiler.scripts.members[unitIndex];
        };
        CompilerState.prototype.getLineMap = function (fileName) {
            var unitIndex = this.compilerCache.getUnitIndex(fileName);
            if(unitIndex < 0) {
                throw new Error("Interal error: No AST found for file \"" + fileName + "\".");
            }
            return this.compiler.units[unitIndex].lineMap;
        };
        CompilerState.prototype.getScopeEntries = function (enclosingScopeContext, getPrettyTypeName) {
            return new TypeScript.ScopeTraversal(this.compiler).getScopeEntries(enclosingScopeContext, getPrettyTypeName);
        };
        CompilerState.prototype.getErrorEntries = function (maxCount, filter) {
            var entries = [];
            var count = 0;
            var addError = function (error) {
                entries.push(error);
                count++;
                return (count < maxCount);
            };
            for(var unitIndex = 0, len = this.errorCollector.fileMap.length; unitIndex < len; unitIndex++) {
                var errors = this.errorCollector.fileMap[unitIndex];
                if(errors !== undefined) {
                    for(var i = 0; i < errors.parseErrors.length; i++) {
                        var error = errors.parseErrors[i];
                        if(filter(unitIndex, error)) {
                            if(!addError(error)) {
                                break;
                            }
                        }
                    }
                    for(var i = 0; i < errors.typeCheckErrors.length; i++) {
                        var error = errors.typeCheckErrors[i];
                        if(filter(unitIndex, error)) {
                            if(!addError(error)) {
                                break;
                            }
                        }
                    }
                }
            }
            var result = [];
            for(var i = 0; i < entries.length; i++) {
                var e = entries[i];
                var ne = new TypeScript.ErrorEntry(this.mapToHostUnitIndex(e.unitIndex), e.minChar, e.limChar, e.message);
                result.push(ne);
            }
            return result;
        };
        CompilerState.prototype.cleanASTTypesForReTypeCheck = function (ast) {
            this.compiler.cleanASTTypesForReTypeCheck(ast);
        };
        CompilerState.prototype.getScriptEditRange = function (script) {
            var lastKnownVersion = this.scriptMap.getEntry(script.locationInfo.filename).version;
            return this.getScriptEditRangeSinceVersion(script.locationInfo.filename, lastKnownVersion);
        };
        CompilerState.prototype.getScriptEditRangeSinceVersion = function (fileName, lastKnownVersion) {
            var hostUnitIndex = this.hostCache.getUnitIndex(fileName);
            var currentVersion = this.hostCache.getVersion(hostUnitIndex);
            if(lastKnownVersion === currentVersion) {
                return null;
            }
            return this.host.getScriptEditRangeSinceVersion(hostUnitIndex, lastKnownVersion);
        };
        CompilerState.prototype.getSourceText = function (script, cached) {
            if (typeof cached === "undefined") { cached = false; }
            return this.hostCache.getSourceText(this.hostCache.getUnitIndex(script.locationInfo.filename), cached);
        };
        CompilerState.prototype.getSourceText2 = function (fileName, cached) {
            if (typeof cached === "undefined") { cached = false; }
            return this.hostCache.getSourceText(this.hostCache.getUnitIndex(fileName), cached);
        };
        CompilerState.prototype.getScriptSyntaxAST = function (fileName) {
            var sourceText = this.hostCache.getSourceText(this.hostCache.getUnitIndex(fileName), true);
            var parser = new TypeScript.Parser();
            parser.setErrorRecovery(null);
            parser.errorCallback = function (a, b, c, d) {
            };
            var script = parser.parse(sourceText, fileName, 0);
            return new Services.ScriptSyntaxAST(this.logger, script, sourceText);
        };
        CompilerState.prototype.getEmitOutput = function (fileName) {
            var unitIndex = this.compilerCache.getUnitIndex(fileName);
            if(unitIndex < 0) {
                throw new Error("Interal error: No AST found for file \"" + fileName + "\".");
            }
            var result = [];
            var errors = this.errorCollector.fileMap[unitIndex];
            if(errors !== undefined && errors.parseErrors.length > 0) {
                return result;
            }
            var emitterIOHost = {
                createFile: function (fileName, useUTF8encoding) {
                    if (typeof useUTF8encoding === "undefined") { useUTF8encoding = false; }
                    var outputFile = new TextWriter(fileName, useUTF8encoding);
                    result.push(outputFile);
                    return outputFile;
                },
                directoryExists: function (fname) {
                    return true;
                },
                fileExists: function (fname) {
                    return false;
                },
                resolvePath: function (fname) {
                    return fname;
                }
            };
            var script = this.compiler.scripts.members[unitIndex];
            this.compiler.parseEmitOption(emitterIOHost);
            this.compiler.emitUnit(script);
            if(errors == undefined || errors.typeCheckErrors.length == 0) {
                this.compiler.emitDeclarationsUnit(script);
            }
            return result;
        };
        return CompilerState;
    })();
    Services.CompilerState = CompilerState;    
})(Services || (Services = {}));
