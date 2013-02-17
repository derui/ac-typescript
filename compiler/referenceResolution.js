var TypeScript;
(function (TypeScript) {
    var SourceUnit = (function () {
        function SourceUnit(path, content) {
            this.path = path;
            this.content = content;
            this.referencedFiles = null;
        }
        SourceUnit.prototype.getText = function (start, end) {
            return this.content.substring(start, end);
        };
        SourceUnit.prototype.getLength = function () {
            return this.content.length;
        };
        return SourceUnit;
    })();
    TypeScript.SourceUnit = SourceUnit;    
    var CompilationEnvironment = (function () {
        function CompilationEnvironment(compilationSettings, ioHost) {
            this.compilationSettings = compilationSettings;
            this.ioHost = ioHost;
            this.residentCode = [];
            this.code = [];
        }
        return CompilationEnvironment;
    })();
    TypeScript.CompilationEnvironment = CompilationEnvironment;    
    var CodeResolver = (function () {
        function CodeResolver(environment) {
            this.environment = environment;
            this.visited = {
            };
        }
        CodeResolver.prototype.resolveCode = function (referencePath, parentPath, performSearch, resolutionDispatcher) {
            var resolvedFile = {
                content: null,
                path: referencePath
            };
            var ioHost = this.environment.ioHost;
            var isRelativePath = TypeScript.isRelative(referencePath);
            var isRootedPath = isRelativePath ? false : TypeScript.isRooted(referencePath);
            var normalizedPath = isRelativePath ? ioHost.resolvePath(parentPath + "/" + referencePath) : (isRootedPath || !parentPath || performSearch ? referencePath : parentPath + "/" + referencePath);
            if(!TypeScript.isSTRFile(normalizedPath) && !TypeScript.isTSFile(normalizedPath)) {
                normalizedPath += ".ts";
            }
            normalizedPath = TypeScript.switchToForwardSlashes(TypeScript.stripQuotes(normalizedPath));
            var absoluteModuleID = this.environment.compilationSettings.useCaseSensitiveFileResolution ? normalizedPath : normalizedPath.toLocaleUpperCase();
            if(!this.visited[absoluteModuleID]) {
                if(isRelativePath || isRootedPath || !performSearch) {
                    try  {
                        TypeScript.CompilerDiagnostics.debugPrint("   Reading code from " + normalizedPath);
                        try  {
                            resolvedFile.content = ioHost.readFile(normalizedPath);
                        } catch (err) {
                            try  {
                                if(TypeScript.isSTRFile(normalizedPath)) {
                                    normalizedPath = TypeScript.changePathToTS(normalizedPath);
                                } else if(TypeScript.isTSFile(normalizedPath)) {
                                    normalizedPath = TypeScript.changePathToSTR(normalizedPath);
                                }
                                TypeScript.CompilerDiagnostics.debugPrint("   Reading code from " + normalizedPath);
                                resolvedFile.content = ioHost.readFile(normalizedPath);
                            } catch (err) {
                                normalizedPath = TypeScript.changePathToDSTR(normalizedPath);
                                TypeScript.CompilerDiagnostics.debugPrint("   Reading code from " + normalizedPath);
                                try  {
                                    resolvedFile.content = ioHost.readFile(normalizedPath);
                                } catch (err) {
                                    normalizedPath = TypeScript.changePathToDTS(normalizedPath);
                                    TypeScript.CompilerDiagnostics.debugPrint("   Reading code from " + normalizedPath);
                                    resolvedFile.content = ioHost.readFile(normalizedPath);
                                }
                            }
                        }
                        TypeScript.CompilerDiagnostics.debugPrint("   Found code at " + normalizedPath);
                        resolvedFile.path = normalizedPath;
                        this.visited[absoluteModuleID] = true;
                    } catch (err) {
                        TypeScript.CompilerDiagnostics.debugPrint("   Did not find code for " + referencePath);
                    }
                } else {
                    resolvedFile = ioHost.findFile(parentPath, normalizedPath);
                    if(!resolvedFile) {
                        if(TypeScript.isSTRFile(normalizedPath)) {
                            normalizedPath = TypeScript.changePathToTS(normalizedPath);
                        } else if(TypeScript.isTSFile(normalizedPath)) {
                            normalizedPath = TypeScript.changePathToSTR(normalizedPath);
                        }
                        resolvedFile = ioHost.findFile(parentPath, normalizedPath);
                    }
                    if(!resolvedFile) {
                        normalizedPath = TypeScript.changePathToDTS(normalizedPath);
                        resolvedFile = ioHost.findFile(parentPath, normalizedPath);
                        if(!resolvedFile) {
                            normalizedPath = TypeScript.changePathToDSTR(normalizedPath);
                            resolvedFile = ioHost.findFile(parentPath, normalizedPath);
                        }
                    }
                    if(resolvedFile) {
                        resolvedFile.path = TypeScript.switchToForwardSlashes(TypeScript.stripQuotes(resolvedFile.path));
                        TypeScript.CompilerDiagnostics.debugPrint(referencePath + " resolved to: " + resolvedFile.path);
                        resolvedFile.content = resolvedFile.content;
                        this.visited[absoluteModuleID] = true;
                    } else {
                        TypeScript.CompilerDiagnostics.debugPrint("Could not find " + referencePath);
                    }
                }
                if(resolvedFile && resolvedFile.content != null) {
                    var rootDir = ioHost.dirName(resolvedFile.path);
                    var sourceUnit = new SourceUnit(resolvedFile.path, resolvedFile.content);
                    var preProcessedFileInfo = TypeScript.preProcessFile(sourceUnit, this.environment.compilationSettings);
                    sourceUnit.referencedFiles = preProcessedFileInfo.referencedFiles;
                    for(var i = 0; i < preProcessedFileInfo.referencedFiles.length; i++) {
                        var referencedFile = preProcessedFileInfo.referencedFiles[i];
                        var normalizedPath = TypeScript.isRooted(referencedFile.path) ? referencedFile.path : rootDir + "/" + referencedFile.path;
                        normalizedPath = ioHost.resolvePath(normalizedPath);
                        if(referencePath == normalizedPath) {
                            resolutionDispatcher.postResolutionError(normalizedPath, "File contains reference to itself", null);
                            continue;
                        }
                        this.resolveCode(referencedFile.path, rootDir, false, resolutionDispatcher);
                    }
                    for(var i = 0; i < preProcessedFileInfo.importedFiles.length; i++) {
                        this.resolveCode(preProcessedFileInfo.importedFiles[i].path, rootDir, true, resolutionDispatcher);
                    }
                    resolutionDispatcher.postResolution(sourceUnit.path, sourceUnit);
                }
            }
        };
        return CodeResolver;
    })();
    TypeScript.CodeResolver = CodeResolver;    
})(TypeScript || (TypeScript = {}));
