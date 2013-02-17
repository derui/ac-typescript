var TypeScript;
(function (TypeScript) {
    var StyleSettings = (function () {
        function StyleSettings() {
            this.bitwise = false;
            this.blockInCompoundStmt = false;
            this.eqeqeq = false;
            this.forin = false;
            this.emptyBlocks = true;
            this.newMustBeUsed = false;
            this.requireSemi = false;
            this.assignmentInCond = false;
            this.eqnull = false;
            this.evalOK = true;
            this.innerScopeDeclEscape = true;
            this.funcInLoop = true;
            this.reDeclareLocal = true;
            this.literalSubscript = true;
            this.implicitAny = false;
        }
        StyleSettings.prototype.setOption = function (opt, val) {
            var optExists = this[opt];
            if(optExists !== undefined) {
                this[opt] = val;
                return true;
            } else {
                return false;
            }
        };
        StyleSettings.prototype.parseOptions = function (str) {
            var opts = str.split(";");
            for(var i = 0, len = opts.length; i < len; i++) {
                var opt = opts[i];
                var val = true;
                var colonIndex = opt.lastIndexOf(":");
                if(colonIndex >= 0) {
                    var valStr = opt.substring(colonIndex + 1);
                    opt = opt.substring(0, colonIndex);
                    if(valStr == "off") {
                        val = false;
                    }
                }
                if(!this.setOption(opt, val)) {
                    return false;
                }
            }
            return true;
        };
        return StyleSettings;
    })();
    TypeScript.StyleSettings = StyleSettings;    
    var CompilationSettings = (function () {
        function CompilationSettings() {
            this.styleSettings = new StyleSettings();
            this.propagateConstants = false;
            this.minWhitespace = false;
            this.parseOnly = false;
            this.errorRecovery = false;
            this.emitComments = false;
            this.watch = false;
            this.exec = false;
            this.resolve = true;
            this.controlFlow = false;
            this.printControlFlow = false;
            this.controlFlowUseDef = false;
            this.errorOnWith = true;
            this.preprocess = true;
            this.canCallDefinitionSignature = false;
            this.inferPropertiesFromThisAssignment = false;
            this.useDefaultLib = true;
            this.codeGenTarget = TypeScript.CodeGenTarget.ES3;
            this.moduleGenTarget = TypeScript.ModuleGenTarget.Synchronous;
            this.outputOption = "";
            this.mapSourceFiles = false;
            this.generateDeclarationFiles = false;
            this.useCaseSensitiveFileResolution = false;
        }
        CompilationSettings.prototype.setStyleOptions = function (str) {
            this.styleSettings.parseOptions(str);
        };
        return CompilationSettings;
    })();
    TypeScript.CompilationSettings = CompilationSettings;    
    function getFileReferenceFromReferencePath(comment) {
        var referencesRegEx = /^(\/\/\/\s*<reference\s+path=)('|")(.+?)\2\s*(static=('|")(.+?)\2\s*)*\/>/gim;
        var match = referencesRegEx.exec(comment);
        if(match) {
            var path = TypeScript.normalizePath(match[3]);
            var adjustedPath = TypeScript.normalizePath(path);
            var isResident = match.length >= 7 && match[6] == "true";
            if(isResident) {
                TypeScript.CompilerDiagnostics.debugPrint(path + " is resident");
            }
            return {
                minChar: 0,
                limChar: 0,
                path: TypeScript.switchToForwardSlashes(adjustedPath),
                isResident: isResident
            };
        } else {
            return null;
        }
    }
    function getAdditionalDependencyPath(comment) {
        var amdDependencyRegEx = /^(\/\/\/\s*<amd-dependency\s+path=)('|")(.+?)\2\s*(static=('|")(.+?)\2\s*)*\/>/gim;
        var match = amdDependencyRegEx.exec(comment);
        if(match) {
            var path = match[3];
            return path;
        } else {
            return null;
        }
    }
    TypeScript.getAdditionalDependencyPath = getAdditionalDependencyPath;
    function getImplicitImport(comment) {
        var implicitImportRegEx = /^(\/\/\/\s*<implicit-import\s*)*\/>/gim;
        var match = implicitImportRegEx.exec(comment);
        if(match) {
            return true;
        }
        return false;
    }
    TypeScript.getImplicitImport = getImplicitImport;
    function getStyleSettings(comment, styleSettings) {
        var styleRegEx = /^(\/\/\/\s*<style\s+)(([a-zA-Z])+=('|").+('|"))\s*\/>/gim;
        var settings = styleRegEx.exec(comment);
        if(settings) {
            var settingsRegEx = /^([a-zA-Z]+=['"]on['|"])/gim;
            settings = settingsRegEx.exec(settings[2]);
            if(settings) {
                for(var i = 0; i < settings.length; i++) {
                    var setting = (settings[i]).split("=");
                    var on = "\"on\"";
                    switch(setting[0]) {
                        case "blockInCompoundStmt":
                            styleSettings.blockInCompoundStmt = setting[1] == on;
                            break;
                        case "eqeqeq":
                            styleSettings.eqeqeq = setting[1] == on;
                            break;
                        case "forin":
                            styleSettings.forin = setting[1] == on;
                            break;
                        case "emptyBlocks":
                            styleSettings.emptyBlocks = setting[1] == on;
                            break;
                        case "newMustBeUsed":
                            styleSettings.newMustBeUsed = setting[1] == on;
                            break;
                        case "requireSemi":
                            styleSettings.requireSemi = setting[1] == on;
                            break;
                        case "assignmentInCond":
                            styleSettings.assignmentInCond = setting[1] == on;
                            break;
                        case "eqnull":
                            styleSettings.eqnull = setting[1] == on;
                            break;
                        case "evalOK":
                            styleSettings.evalOK = setting[1] == on;
                            break;
                        case "innerScopeDeclEscape":
                            styleSettings.innerScopeDeclEscape = setting[1] == on;
                            break;
                        case "funcInLoop":
                            styleSettings.funcInLoop = setting[1] == on;
                            break;
                        case "reDeclareLocal":
                            styleSettings.reDeclareLocal = setting[1] == on;
                            break;
                        case "literalSubscript":
                            styleSettings.literalSubscript = setting[1] == on;
                            break;
                        case "implicitAny":
                            styleSettings.implicitAny = setting[1] == on;
                            break;
                    }
                }
            }
        }
    }
    TypeScript.getStyleSettings = getStyleSettings;
    function getReferencedFiles(sourceText) {
        var preProcessInfo = preProcessFile(sourceText, null, false);
        return preProcessInfo.referencedFiles;
    }
    TypeScript.getReferencedFiles = getReferencedFiles;
    function preProcessFile(sourceText, options, readImportFiles) {
        if (typeof options === "undefined") { options = new CompilationSettings(); }
        if (typeof readImportFiles === "undefined") { readImportFiles = true; }
        var scanner = new TypeScript.Scanner();
        scanner.resetComments();
        scanner.setSourceText(sourceText, TypeScript.LexMode.File);
        var tok = scanner.scan();
        var comments = [];
        var comment = null;
        var leftCurlies = [];
        var settings = options;
        var referencedFiles = [];
        var importedFiles = [];
        var isLibFile = false;
        while(tok.tokenId != TypeScript.TokenID.EndOfFile) {
            if(readImportFiles && tok.tokenId == TypeScript.TokenID.Import) {
                tok = scanner.scan();
                if(tok.tokenId == TypeScript.TokenID.Identifier || TypeScript.convertTokToID(tok, false)) {
                    tok = scanner.scan();
                    if(tok.tokenId == TypeScript.TokenID.Equals) {
                        tok = scanner.scan();
                        if(tok.tokenId == TypeScript.TokenID.Module) {
                            tok = scanner.scan();
                            if(tok.tokenId == TypeScript.TokenID.OpenParen) {
                                tok = scanner.scan();
                                if(tok.tokenId == TypeScript.TokenID.StringLiteral) {
                                    var ref = {
                                        minChar: scanner.startPos,
                                        limChar: scanner.pos,
                                        path: TypeScript.stripQuotes(TypeScript.switchToForwardSlashes(tok.getText())),
                                        isResident: false
                                    };
                                    importedFiles.push(ref);
                                }
                            }
                        }
                    }
                }
            }
            if(tok.tokenId == TypeScript.TokenID.OpenBrace) {
                leftCurlies.push(tok);
            }
            if(tok.tokenId == TypeScript.TokenID.CloseBrace) {
                leftCurlies.pop();
            }
            tok = scanner.scan();
        }
        comments = scanner.getComments();
        for(var iComment = 0; iComment < comments.length; iComment++) {
            comment = comments[iComment];
            if(!comment.isBlock) {
                var referencedCode = getFileReferenceFromReferencePath(comment.getText());
                if(referencedCode) {
                    referencedCode.minChar = comment.startPos;
                    referencedCode.limChar = referencedCode.minChar + comment.value.length;
                    referencedFiles.push(referencedCode);
                }
                if(settings) {
                    getStyleSettings(comment.getText(), settings.styleSettings);
                    var isNoLibRegex = /^(\/\/\/\s*<reference\s+no-default-lib=)('|")(.+?)\2\s*\/>/gim;
                    var isNoLibMatch = isNoLibRegex.exec(comment.getText());
                    if(isNoLibMatch) {
                        isLibFile = (isNoLibMatch[3] == "true");
                    }
                }
            }
        }
        return {
            settings: settings,
            referencedFiles: referencedFiles,
            importedFiles: importedFiles,
            isLibFile: isLibFile
        };
    }
    TypeScript.preProcessFile = preProcessFile;
})(TypeScript || (TypeScript = {}));
