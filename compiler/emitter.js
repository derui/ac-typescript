var TypeScript;
(function (TypeScript) {
    (function (EmitContainer) {
        EmitContainer._map = [];
        EmitContainer._map[0] = "Prog";
        EmitContainer.Prog = 0;
        EmitContainer._map[1] = "Module";
        EmitContainer.Module = 1;
        EmitContainer._map[2] = "DynamicModule";
        EmitContainer.DynamicModule = 2;
        EmitContainer._map[3] = "Class";
        EmitContainer.Class = 3;
        EmitContainer._map[4] = "Constructor";
        EmitContainer.Constructor = 4;
        EmitContainer._map[5] = "Function";
        EmitContainer.Function = 5;
        EmitContainer._map[6] = "Args";
        EmitContainer.Args = 6;
        EmitContainer._map[7] = "Interface";
        EmitContainer.Interface = 7;
    })(TypeScript.EmitContainer || (TypeScript.EmitContainer = {}));
    var EmitContainer = TypeScript.EmitContainer;
    var EmitState = (function () {
        function EmitState() {
            this.column = 0;
            this.line = 0;
            this.pretty = false;
            this.inObjectLiteral = false;
            this.container = EmitContainer.Prog;
        }
        return EmitState;
    })();
    TypeScript.EmitState = EmitState;    
    var EmitOptions = (function () {
        function EmitOptions(settings) {
            this.ioHost = null;
            this.outputMany = true;
            this.commonDirectoryPath = "";
            this.minWhitespace = settings.minWhitespace;
            this.propagateConstants = settings.propagateConstants;
            this.emitComments = settings.emitComments;
            this.outputOption = settings.outputOption;
            this.emitFullSourceMapPath = settings.emitFullSourceMapPath;
        }
        EmitOptions.prototype.mapOutputFileName = function (fileName, extensionChanger) {
            if(this.outputMany) {
                var updatedFileName = fileName;
                if(this.outputOption != "") {
                    updatedFileName = fileName.replace(this.commonDirectoryPath, "");
                    updatedFileName = this.outputOption + updatedFileName;
                }
                return extensionChanger(updatedFileName, false);
            } else {
                return extensionChanger(this.outputOption, true);
            }
        };
        return EmitOptions;
    })();
    TypeScript.EmitOptions = EmitOptions;    
    var Indenter = (function () {
        function Indenter() {
            this.indentAmt = 0;
        }
        Indenter.indentStep = 4;
        Indenter.indentStepString = "    ";
        Indenter.indentStrings = [];
        Indenter.prototype.increaseIndent = function () {
            this.indentAmt += Indenter.indentStep;
        };
        Indenter.prototype.decreaseIndent = function () {
            this.indentAmt -= Indenter.indentStep;
        };
        Indenter.prototype.getIndent = function () {
            var indentString = Indenter.indentStrings[this.indentAmt];
            if(indentString === undefined) {
                indentString = "";
                for(var i = 0; i < this.indentAmt; i = i + Indenter.indentStep) {
                    indentString += Indenter.indentStepString;
                }
                Indenter.indentStrings[this.indentAmt] = indentString;
            }
            return indentString;
        };
        return Indenter;
    })();
    TypeScript.Indenter = Indenter;    
    var Emitter = (function () {
        function Emitter(checker, emittingFileName, outfile, emitOptions, errorReporter) {
            this.checker = checker;
            this.emittingFileName = emittingFileName;
            this.outfile = outfile;
            this.emitOptions = emitOptions;
            this.errorReporter = errorReporter;
            this.globalThisCapturePrologueEmitted = false;
            this.extendsPrologueEmitted = false;
            this.thisClassNode = null;
            this.thisFnc = null;
            this.moduleDeclList = [];
            this.moduleName = "";
            this.emitState = new EmitState();
            this.indenter = new Indenter();
            this.ambientModule = false;
            this.modAliasId = null;
            this.firstModAlias = null;
            this.allSourceMappers = [];
            this.sourceMapper = null;
            this.captureThisStmtString = "var _this = this;";
            this.varListCountStack = [
                0
            ];
        }
        Emitter.prototype.setSourceMappings = function (mapper) {
            this.allSourceMappers.push(mapper);
            this.sourceMapper = mapper;
        };
        Emitter.prototype.writeToOutput = function (s) {
            this.outfile.Write(s);
            this.emitState.column += s.length;
        };
        Emitter.prototype.writeToOutputTrimmable = function (s) {
            if(this.emitOptions.minWhitespace) {
                s = s.replace(/[\s]*/g, '');
            }
            this.writeToOutput(s);
        };
        Emitter.prototype.writeLineToOutput = function (s) {
            if(this.emitOptions.minWhitespace) {
                this.writeToOutput(s);
                var c = s.charCodeAt(s.length - 1);
                if(!((c == TypeScript.LexCodeSpace) || (c == TypeScript.LexCodeSMC) || (c == TypeScript.LexCodeLBR))) {
                    this.writeToOutput(' ');
                }
            } else {
                this.outfile.WriteLine(s);
                this.emitState.column = 0;
                this.emitState.line++;
            }
        };
        Emitter.prototype.writeCaptureThisStatement = function (ast) {
            this.emitIndent();
            this.recordSourceMappingStart(ast);
            this.writeToOutput(this.captureThisStmtString);
            this.recordSourceMappingEnd(ast);
            this.writeLineToOutput("");
        };
        Emitter.prototype.setInVarBlock = function (count) {
            this.varListCountStack[this.varListCountStack.length - 1] = count;
        };
        Emitter.prototype.setInObjectLiteral = function (val) {
            var temp = this.emitState.inObjectLiteral;
            this.emitState.inObjectLiteral = val;
            return temp;
        };
        Emitter.prototype.setContainer = function (c) {
            var temp = this.emitState.container;
            this.emitState.container = c;
            return temp;
        };
        Emitter.prototype.getIndentString = function () {
            if(this.emitOptions.minWhitespace) {
                return "";
            } else {
                return this.indenter.getIndent();
            }
        };
        Emitter.prototype.emitIndent = function () {
            this.writeToOutput(this.getIndentString());
        };
        Emitter.prototype.emitCommentInPlace = function (comment) {
            var text = comment.getText();
            var hadNewLine = false;
            if(comment.isBlockComment) {
                if(this.emitState.column == 0) {
                    this.emitIndent();
                }
                this.recordSourceMappingStart(comment);
                this.writeToOutput(text[0]);
                if(text.length > 1 || comment.endsLine) {
                    for(var i = 1; i < text.length; i++) {
                        this.writeLineToOutput("");
                        this.emitIndent();
                        this.writeToOutput(text[i]);
                    }
                    this.recordSourceMappingEnd(comment);
                    this.writeLineToOutput("");
                    hadNewLine = true;
                } else {
                    this.recordSourceMappingEnd(comment);
                }
            } else {
                if(this.emitState.column == 0) {
                    this.emitIndent();
                }
                this.recordSourceMappingStart(comment);
                this.writeToOutput(text[0]);
                this.recordSourceMappingEnd(comment);
                this.writeLineToOutput("");
                hadNewLine = true;
            }
            if(hadNewLine) {
                this.emitIndent();
            } else {
                this.writeToOutput(" ");
            }
        };
        Emitter.prototype.emitParensAndCommentsInPlace = function (ast, pre) {
            var comments = pre ? ast.preComments : ast.postComments;
            if(ast.isParenthesized && !pre) {
                this.writeToOutput(")");
            }
            if(this.emitOptions.emitComments && comments && comments.length != 0) {
                for(var i = 0; i < comments.length; i++) {
                    this.emitCommentInPlace(comments[i]);
                }
            }
            if(ast.isParenthesized && pre) {
                this.writeToOutput("(");
            }
        };
        Emitter.prototype.emitObjectLiteral = function (content) {
            this.writeLineToOutput("{");
            this.indenter.increaseIndent();
            var inObjectLiteral = this.setInObjectLiteral(true);
            this.emitJavascriptList(content, ",", TypeScript.TokenID.Comma, true, false, false);
            this.setInObjectLiteral(inObjectLiteral);
            this.indenter.decreaseIndent();
            this.emitIndent();
            this.writeToOutput("}");
        };
        Emitter.prototype.emitArrayLiteral = function (content) {
            this.writeToOutput("[");
            if(content) {
                this.writeLineToOutput("");
                this.indenter.increaseIndent();
                this.emitJavascriptList(content, ", ", TypeScript.TokenID.Comma, true, false, false);
                this.indenter.decreaseIndent();
                this.emitIndent();
            }
            this.writeToOutput("]");
        };
        Emitter.prototype.emitNew = function (target, args) {
            this.writeToOutput("new ");
            if(target.nodeType == TypeScript.NodeType.TypeRef) {
                var typeRef = target;
                if(typeRef.arrayCount) {
                    this.writeToOutput("Array()");
                } else {
                    this.emitJavascript(typeRef.term, TypeScript.TokenID.Tilde, false);
                    this.writeToOutput("()");
                }
            } else {
                this.emitJavascript(target, TypeScript.TokenID.Tilde, false);
                this.recordSourceMappingStart(args);
                this.writeToOutput("(");
                this.emitJavascriptList(args, ", ", TypeScript.TokenID.Comma, false, false, false);
                this.writeToOutput(")");
                this.recordSourceMappingEnd(args);
            }
        };
        Emitter.prototype.getConstantValue = function (init) {
            if(init) {
                if(init.nodeType === TypeScript.NodeType.NumberLit) {
                    var numLit = init;
                    return numLit.value;
                } else if(init.nodeType === TypeScript.NodeType.Lsh) {
                    var binop = init;
                    if(binop.operand1.nodeType === TypeScript.NodeType.NumberLit && binop.operand2.nodeType === TypeScript.NodeType.NumberLit) {
                        return (binop.operand1).value << (binop.operand2).value;
                    }
                } else if(init.nodeType === TypeScript.NodeType.Name) {
                    var ident = init;
                    if(ident.sym !== null && ident.sym.declAST.nodeType === TypeScript.NodeType.VarDecl) {
                        var varDecl = ident.sym.declAST;
                        return this.getConstantValue(varDecl.init);
                    }
                }
            }
            return null;
        };
        Emitter.prototype.tryEmitConstant = function (dotExpr) {
            if(!this.emitOptions.propagateConstants) {
                return false;
            }
            var propertyName = dotExpr.operand2;
            if(propertyName && propertyName.sym && propertyName.sym.isVariable()) {
                if(TypeScript.hasFlag(propertyName.sym.flags, TypeScript.SymbolFlags.Constant)) {
                    if(propertyName.sym.declAST) {
                        var boundDecl = propertyName.sym.declAST;
                        var value = this.getConstantValue(boundDecl.init);
                        if(value !== null) {
                            this.writeToOutput(value.toString());
                            var comment = " /* ";
                            comment += propertyName.actualText;
                            comment += " */ ";
                            this.writeToOutput(comment);
                            return true;
                        }
                    }
                }
            }
            return false;
        };
        Emitter.prototype.emitCall = function (callNode, target, args) {
            if(!this.emitSuperCall(callNode)) {
                if(!TypeScript.hasFlag(callNode.flags, TypeScript.ASTFlags.ClassBaseConstructorCall)) {
                    if(target.nodeType == TypeScript.NodeType.FuncDecl && !target.isParenthesized) {
                        this.writeToOutput("(");
                    }
                    if(callNode.target.nodeType == TypeScript.NodeType.Super && this.emitState.container == EmitContainer.Constructor) {
                        this.writeToOutput("_super.call");
                    } else {
                        this.emitJavascript(target, TypeScript.TokenID.OpenParen, false);
                    }
                    if(target.nodeType == TypeScript.NodeType.FuncDecl && !target.isParenthesized) {
                        this.writeToOutput(")");
                    }
                    this.recordSourceMappingStart(args);
                    this.writeToOutput("(");
                    if(callNode.target.nodeType == TypeScript.NodeType.Super && this.emitState.container == EmitContainer.Constructor) {
                        this.writeToOutput("this");
                        if(args && args.members.length) {
                            this.writeToOutput(", ");
                        }
                    }
                    this.emitJavascriptList(args, ", ", TypeScript.TokenID.Comma, false, false, false);
                    this.writeToOutput(")");
                    this.recordSourceMappingEnd(args);
                } else {
                    this.indenter.decreaseIndent();
                    this.indenter.decreaseIndent();
                    var constructorCall = new TypeScript.ASTList();
                    constructorCall.members[0] = callNode;
                    this.emitConstructorCalls(constructorCall, this.thisClassNode);
                    this.indenter.increaseIndent();
                    this.indenter.increaseIndent();
                }
            }
        };
        Emitter.prototype.emitConstructorCalls = function (bases, classDecl) {
            if(bases == null) {
                return;
            }
            var basesLen = bases.members.length;
            this.recordSourceMappingStart(classDecl);
            for(var i = 0; i < basesLen; i++) {
                var baseExpr = bases.members[i];
                var baseSymbol = null;
                if(baseExpr.nodeType == TypeScript.NodeType.Call) {
                    baseSymbol = (baseExpr).target.type.symbol;
                } else {
                    baseSymbol = baseExpr.type.symbol;
                }
                var baseName = baseSymbol.name;
                if(baseSymbol.declModule != classDecl.type.symbol.declModule) {
                    baseName = baseSymbol.fullName();
                }
                if(baseExpr.nodeType == TypeScript.NodeType.Call) {
                    this.emitIndent();
                    this.writeToOutput("_super.call(this");
                    var args = (baseExpr).arguments;
                    if(args && (args.members.length > 0)) {
                        this.writeToOutput(", ");
                        this.emitJavascriptList(args, ", ", TypeScript.TokenID.Comma, false, false, false);
                    }
                    this.writeToOutput(")");
                } else {
                    if(baseExpr.type && (baseExpr.type.isClassInstance())) {
                        this.emitIndent();
                        this.writeToOutput(classDecl.name.actualText + "._super.constructor");
                        this.writeToOutput(".call(this)");
                    }
                }
            }
            this.recordSourceMappingEnd(classDecl);
        };
        Emitter.prototype.emitInnerFunction = function (funcDecl, printName, isMember, bases, hasSelfRef, classDecl) {
            var isClassConstructor = funcDecl.isConstructor && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod);
            var hasNonObjectBaseType = isClassConstructor && TypeScript.hasFlag(this.thisClassNode.type.instanceType.typeFlags, TypeScript.TypeFlags.HasBaseType) && !TypeScript.hasFlag(this.thisClassNode.type.instanceType.typeFlags, TypeScript.TypeFlags.HasBaseTypeOfObject);
            var classPropertiesMustComeAfterSuperCall = hasNonObjectBaseType && TypeScript.hasFlag((this.thisClassNode).varFlags, TypeScript.VarFlags.ClassSuperMustBeFirstCallInConstructor);
            var shouldParenthesize = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.IsFunctionExpression) && !funcDecl.isParenthesized && !funcDecl.isAccessor() && (TypeScript.hasFlag(funcDecl.flags, TypeScript.ASTFlags.ExplicitSemicolon) || TypeScript.hasFlag(funcDecl.flags, TypeScript.ASTFlags.AutomaticSemicolon));
            this.emitParensAndCommentsInPlace(funcDecl, true);
            if(shouldParenthesize) {
                this.writeToOutput("(");
            }
            this.recordSourceMappingStart(funcDecl);
            if(!(funcDecl.isAccessor() && (funcDecl.accessorSymbol).isObjectLitField)) {
                this.writeToOutput("function ");
            }
            if(printName) {
                var id = funcDecl.getNameText();
                if(id && !funcDecl.isAccessor()) {
                    if(funcDecl.name) {
                        this.recordSourceMappingStart(funcDecl.name);
                    }
                    this.writeToOutput(id);
                    if(funcDecl.name) {
                        this.recordSourceMappingEnd(funcDecl.name);
                    }
                }
            }
            this.writeToOutput("(");
            var argsLen = 0;
            var i = 0;
            var arg;
            var defaultArgs = [];
            if(funcDecl.arguments) {
                var tempContainer = this.setContainer(EmitContainer.Args);
                argsLen = funcDecl.arguments.members.length;
                var printLen = argsLen;
                if(funcDecl.variableArgList) {
                    printLen--;
                }
                for(i = 0; i < printLen; i++) {
                    arg = funcDecl.arguments.members[i];
                    if(arg.init) {
                        defaultArgs.push(arg);
                    }
                    this.emitJavascript(arg, TypeScript.TokenID.OpenParen, false);
                    if(i < (printLen - 1)) {
                        this.writeToOutput(", ");
                    }
                }
                this.setContainer(tempContainer);
            }
            this.writeLineToOutput(") {");
            if(funcDecl.isConstructor) {
                this.recordSourceMappingNameStart("constructor");
            } else if(funcDecl.isGetAccessor()) {
                this.recordSourceMappingNameStart("get_" + funcDecl.getNameText());
            } else if(funcDecl.isSetAccessor()) {
                this.recordSourceMappingNameStart("set_" + funcDecl.getNameText());
            } else {
                this.recordSourceMappingNameStart(funcDecl.getNameText());
            }
            this.indenter.increaseIndent();
            for(i = 0; i < defaultArgs.length; i++) {
                var arg = defaultArgs[i];
                this.emitIndent();
                this.recordSourceMappingStart(arg);
                this.writeToOutput("if (typeof " + arg.id.actualText + " === \"undefined\") { ");
                this.recordSourceMappingStart(arg.id);
                this.writeToOutput(arg.id.actualText);
                this.recordSourceMappingEnd(arg.id);
                this.writeToOutput(" = ");
                this.emitJavascript(arg.init, TypeScript.TokenID.OpenParen, false);
                this.writeLineToOutput("; }");
                this.recordSourceMappingEnd(arg);
            }
            if(funcDecl.isConstructor && ((funcDecl.classDecl).varFlags & TypeScript.VarFlags.MustCaptureThis)) {
                this.writeCaptureThisStatement(funcDecl);
            }
            if(funcDecl.isConstructor && !classPropertiesMustComeAfterSuperCall) {
                if(funcDecl.arguments) {
                    argsLen = funcDecl.arguments.members.length;
                    for(i = 0; i < argsLen; i++) {
                        arg = funcDecl.arguments.members[i];
                        if((arg.varFlags & TypeScript.VarFlags.Property) != TypeScript.VarFlags.None) {
                            this.emitIndent();
                            this.recordSourceMappingStart(arg);
                            this.recordSourceMappingStart(arg.id);
                            this.writeToOutput("this." + arg.id.actualText);
                            this.recordSourceMappingEnd(arg.id);
                            this.writeToOutput(" = ");
                            this.recordSourceMappingStart(arg.id);
                            this.writeToOutput(arg.id.actualText);
                            this.recordSourceMappingEnd(arg.id);
                            this.writeLineToOutput(";");
                            this.recordSourceMappingEnd(arg);
                        }
                    }
                }
                if(!TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod)) {
                    this.emitConstructorCalls(bases, classDecl);
                }
            }
            if(hasSelfRef) {
                this.writeCaptureThisStatement(funcDecl);
            }
            if(funcDecl.variableArgList) {
                argsLen = funcDecl.arguments.members.length;
                var lastArg = funcDecl.arguments.members[argsLen - 1];
                this.emitIndent();
                this.recordSourceMappingStart(lastArg);
                this.writeToOutput("var ");
                this.recordSourceMappingStart(lastArg.id);
                this.writeToOutput(lastArg.id.actualText);
                this.recordSourceMappingEnd(lastArg.id);
                this.writeLineToOutput(" = [];");
                this.recordSourceMappingEnd(lastArg);
                this.emitIndent();
                this.writeToOutput("for (");
                this.recordSourceMappingStart(lastArg);
                this.writeToOutput("var _i = 0;");
                this.recordSourceMappingEnd(lastArg);
                this.writeToOutput(" ");
                this.recordSourceMappingStart(lastArg);
                this.writeToOutput("_i < (arguments.length - " + (argsLen - 1) + ")");
                this.recordSourceMappingEnd(lastArg);
                this.writeToOutput("; ");
                this.recordSourceMappingStart(lastArg);
                this.writeToOutput("_i++");
                this.recordSourceMappingEnd(lastArg);
                this.writeLineToOutput(") {");
                this.indenter.increaseIndent();
                this.emitIndent();
                this.recordSourceMappingStart(lastArg);
                this.writeToOutput(lastArg.id.actualText + "[_i] = arguments[_i + " + (argsLen - 1) + "];");
                this.recordSourceMappingEnd(lastArg);
                this.writeLineToOutput("");
                this.indenter.decreaseIndent();
                this.emitIndent();
                this.writeLineToOutput("}");
            }
            if(funcDecl.isConstructor && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod) && !classPropertiesMustComeAfterSuperCall) {
                var nProps = (this.thisClassNode.members).members.length;
                for(var i = 0; i < nProps; i++) {
                    if((this.thisClassNode.members).members[i].nodeType == TypeScript.NodeType.VarDecl) {
                        var varDecl = (this.thisClassNode.members).members[i];
                        if(!TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Static) && varDecl.init) {
                            this.emitIndent();
                            this.emitJavascriptVarDecl(varDecl, TypeScript.TokenID.Tilde);
                            this.writeLineToOutput("");
                        }
                    }
                }
            }
            this.emitBareJavascriptStatements(funcDecl.bod, classPropertiesMustComeAfterSuperCall);
            this.indenter.decreaseIndent();
            this.emitIndent();
            this.recordSourceMappingStart(funcDecl.endingToken);
            this.writeToOutput("}");
            this.recordSourceMappingNameEnd();
            this.recordSourceMappingEnd(funcDecl.endingToken);
            this.recordSourceMappingEnd(funcDecl);
            if(shouldParenthesize) {
                this.writeToOutput(")");
            }
            this.recordSourceMappingEnd(funcDecl);
            this.emitParensAndCommentsInPlace(funcDecl, false);
            if(!isMember && !TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.IsFunctionExpression) && (!TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Signature) || funcDecl.isConstructor)) {
                this.writeLineToOutput("");
            } else if(TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.IsFunctionExpression)) {
                if(TypeScript.hasFlag(funcDecl.flags, TypeScript.ASTFlags.ExplicitSemicolon) || TypeScript.hasFlag(funcDecl.flags, TypeScript.ASTFlags.AutomaticSemicolon)) {
                    this.writeLineToOutput(";");
                }
            }
        };
        Emitter.prototype.emitJavascriptModule = function (moduleDecl) {
            var modName = moduleDecl.name.actualText;
            if(TypeScript.isTSFile(modName)) {
                moduleDecl.name.setText(modName.substring(0, modName.length - 3));
            } else if(TypeScript.isSTRFile(modName)) {
                moduleDecl.name.setText(modName.substring(0, modName.length - 4));
            }
            if(!TypeScript.hasFlag(moduleDecl.modFlags, TypeScript.ModuleFlags.Ambient)) {
                var isDynamicMod = TypeScript.hasFlag(moduleDecl.modFlags, TypeScript.ModuleFlags.IsDynamic);
                var prevOutFile = this.outfile;
                var prevOutFileName = this.emittingFileName;
                var prevAllSourceMappers = this.allSourceMappers;
                var prevSourceMapper = this.sourceMapper;
                var prevColumn = this.emitState.column;
                var prevLine = this.emitState.line;
                var temp = this.setContainer(EmitContainer.Module);
                var svModuleName = this.moduleName;
                var isExported = TypeScript.hasFlag(moduleDecl.modFlags, TypeScript.ModuleFlags.Exported);
                this.moduleDeclList[this.moduleDeclList.length] = moduleDecl;
                var isWholeFile = TypeScript.hasFlag(moduleDecl.modFlags, TypeScript.ModuleFlags.IsWholeFile);
                this.moduleName = moduleDecl.name.actualText;
                if(isDynamicMod) {
                    var tsModFileName = TypeScript.stripQuotes(moduleDecl.name.actualText);
                    var modFilePath = TypeScript.trimModName(tsModFileName) + ".js";
                    modFilePath = this.emitOptions.mapOutputFileName(modFilePath, TypeScript.TypeScriptCompiler.mapToJSFileName);
                    if(this.emitOptions.ioHost) {
                        if(TypeScript.switchToForwardSlashes(modFilePath) != TypeScript.switchToForwardSlashes(this.emittingFileName)) {
                            this.emittingFileName = modFilePath;
                            var useUTF8InOutputfile = moduleDecl.containsUnicodeChar || (this.emitOptions.emitComments && moduleDecl.containsUnicodeCharInComment);
                            this.outfile = this.createFile(this.emittingFileName, useUTF8InOutputfile);
                            if(prevSourceMapper != null) {
                                this.allSourceMappers = [];
                                var sourceMappingFile = this.createFile(this.emittingFileName + TypeScript.SourceMapper.MapFileExtension, false);
                                this.setSourceMappings(new TypeScript.SourceMapper(tsModFileName, this.emittingFileName, this.outfile, sourceMappingFile, this.errorReporter, this.emitOptions.emitFullSourceMapPath));
                                this.emitState.column = 0;
                                this.emitState.line = 0;
                            }
                        } else {
                            TypeScript.CompilerDiagnostics.assert(this.emitOptions.outputMany, "Cannot have dynamic modules compiling into single file");
                        }
                    }
                    this.setContainer(EmitContainer.DynamicModule);
                    this.recordSourceMappingStart(moduleDecl);
                    if(TypeScript.moduleGenTarget == TypeScript.ModuleGenTarget.Asynchronous) {
                        var dependencyList = "[\"require\", \"exports\"";
                        var importList = "require, exports";
                        var importStatement = null;
                        for(var i = 0; i < (moduleDecl.mod).importedModules.length; i++) {
                            importStatement = (moduleDecl.mod).importedModules[i];
                            if(importStatement.id.sym && !(importStatement.id.sym).onlyReferencedAsTypeRef) {
                                if(i <= (moduleDecl.mod).importedModules.length - 1) {
                                    dependencyList += ", ";
                                    importList += ", ";
                                }
                                importList += "__" + importStatement.id.actualText + "__";
                                dependencyList += importStatement.firstAliasedModToString();
                            }
                        }
                        for(var i = 0; i < moduleDecl.amdDependencies.length; i++) {
                            dependencyList += ", \"" + moduleDecl.amdDependencies[i] + "\"";
                        }
                        dependencyList += "]";
                        this.writeLineToOutput("define(" + dependencyList + "," + " function(" + importList + ") {");
                    } else {
                    }
                } else {
                    if(!isExported) {
                        this.recordSourceMappingStart(moduleDecl);
                        this.writeToOutput("var ");
                        this.recordSourceMappingStart(moduleDecl.name);
                        this.writeToOutput(this.moduleName);
                        this.recordSourceMappingEnd(moduleDecl.name);
                        this.writeLineToOutput(";");
                        this.recordSourceMappingEnd(moduleDecl);
                        this.emitIndent();
                    }
                    this.writeToOutput("(");
                    this.recordSourceMappingStart(moduleDecl);
                    this.writeToOutput("function (");
                    this.recordSourceMappingStart(moduleDecl.name);
                    this.writeToOutput(this.moduleName);
                    this.recordSourceMappingEnd(moduleDecl.name);
                    this.writeLineToOutput(") {");
                }
                if(!isWholeFile) {
                    this.recordSourceMappingNameStart(this.moduleName);
                }
                if(!isDynamicMod || TypeScript.moduleGenTarget == TypeScript.ModuleGenTarget.Asynchronous) {
                    this.indenter.increaseIndent();
                }
                if(moduleDecl.modFlags & TypeScript.ModuleFlags.MustCaptureThis) {
                    this.writeCaptureThisStatement(moduleDecl);
                }
                this.emitJavascriptList(moduleDecl.members, null, TypeScript.TokenID.Semicolon, true, false, false);
                if(!isDynamicMod || TypeScript.moduleGenTarget == TypeScript.ModuleGenTarget.Asynchronous) {
                    this.indenter.decreaseIndent();
                }
                this.emitIndent();
                if(isDynamicMod) {
                    if(TypeScript.moduleGenTarget == TypeScript.ModuleGenTarget.Asynchronous) {
                        this.writeLineToOutput("})");
                    } else {
                    }
                    if(!isWholeFile) {
                        this.recordSourceMappingNameEnd();
                    }
                    this.recordSourceMappingEnd(moduleDecl);
                    if(this.outfile != prevOutFile) {
                        this.Close();
                        if(prevSourceMapper != null) {
                            this.allSourceMappers = prevAllSourceMappers;
                            this.sourceMapper = prevSourceMapper;
                            this.emitState.column = prevColumn;
                            this.emitState.line = prevLine;
                        }
                        this.outfile = prevOutFile;
                        this.emittingFileName = prevOutFileName;
                    }
                } else {
                    var containingMod = null;
                    if(moduleDecl.type && moduleDecl.type.symbol.container && moduleDecl.type.symbol.container.declAST) {
                        containingMod = moduleDecl.type.symbol.container.declAST;
                    }
                    var parentIsDynamic = containingMod && TypeScript.hasFlag(containingMod.modFlags, TypeScript.ModuleFlags.IsDynamic);
                    this.recordSourceMappingStart(moduleDecl.endingToken);
                    if(temp == EmitContainer.Prog && isExported) {
                        this.writeToOutput("}");
                        if(!isWholeFile) {
                            this.recordSourceMappingNameEnd();
                        }
                        this.recordSourceMappingEnd(moduleDecl.endingToken);
                        this.writeToOutput(")(this." + this.moduleName + " || (this." + this.moduleName + " = {}));");
                    } else if(isExported || temp == EmitContainer.Prog) {
                        var dotMod = svModuleName != "" ? (parentIsDynamic ? "exports" : svModuleName) + "." : svModuleName;
                        this.writeToOutput("}");
                        if(!isWholeFile) {
                            this.recordSourceMappingNameEnd();
                        }
                        this.recordSourceMappingEnd(moduleDecl.endingToken);
                        this.writeToOutput(")(" + dotMod + this.moduleName + " || (" + dotMod + this.moduleName + " = {}));");
                    } else if(!isExported && temp != EmitContainer.Prog) {
                        this.writeToOutput("}");
                        if(!isWholeFile) {
                            this.recordSourceMappingNameEnd();
                        }
                        this.recordSourceMappingEnd(moduleDecl.endingToken);
                        this.writeToOutput(")(" + this.moduleName + " || (" + this.moduleName + " = {}));");
                    } else {
                        this.writeToOutput("}");
                        if(!isWholeFile) {
                            this.recordSourceMappingNameEnd();
                        }
                        this.recordSourceMappingEnd(moduleDecl.endingToken);
                        this.writeToOutput(")();");
                    }
                    this.recordSourceMappingEnd(moduleDecl);
                    this.writeLineToOutput("");
                    if(temp != EmitContainer.Prog && isExported) {
                        this.emitIndent();
                        this.recordSourceMappingStart(moduleDecl);
                        if(parentIsDynamic) {
                            this.writeLineToOutput("var " + this.moduleName + " = exports." + this.moduleName + ";");
                        } else {
                            this.writeLineToOutput("var " + this.moduleName + " = " + svModuleName + "." + this.moduleName + ";");
                        }
                        this.recordSourceMappingEnd(moduleDecl);
                    }
                }
                this.setContainer(temp);
                this.moduleName = svModuleName;
                this.moduleDeclList.length--;
            }
        };
        Emitter.prototype.emitIndex = function (operand1, operand2) {
            var temp = this.setInObjectLiteral(false);
            this.emitJavascript(operand1, TypeScript.TokenID.Tilde, false);
            this.writeToOutput("[");
            this.emitJavascriptList(operand2, ", ", TypeScript.TokenID.Comma, false, false, false);
            this.writeToOutput("]");
            this.setInObjectLiteral(temp);
        };
        Emitter.prototype.emitStringLiteral = function (text) {
            this.writeToOutput(text);
        };
        Emitter.prototype.emitJavascriptFunction = function (funcDecl) {
            if(TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Signature) || funcDecl.isOverload) {
                return;
            }
            var temp;
            var tempFnc = this.thisFnc;
            this.thisFnc = funcDecl;
            if(funcDecl.isConstructor) {
                temp = this.setContainer(EmitContainer.Constructor);
            } else {
                temp = this.setContainer(EmitContainer.Function);
            }
            var bases = null;
            var hasSelfRef = false;
            var funcName = funcDecl.getNameText();
            if((this.emitState.inObjectLiteral || !funcDecl.isAccessor()) && ((temp != EmitContainer.Constructor) || ((funcDecl.fncFlags & TypeScript.FncFlags.Method) == TypeScript.FncFlags.None))) {
                var tempLit = this.setInObjectLiteral(false);
                if(this.thisClassNode) {
                    bases = this.thisClassNode.extendsList;
                }
                hasSelfRef = Emitter.shouldCaptureThis(funcDecl);
                this.recordSourceMappingStart(funcDecl);
                if(TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Exported | TypeScript.FncFlags.ClassPropertyMethodExported) && funcDecl.type.symbol.container == this.checker.gloMod && !funcDecl.isConstructor) {
                    this.writeToOutput("this." + funcName + " = ");
                    this.emitInnerFunction(funcDecl, false, false, bases, hasSelfRef, this.thisClassNode);
                } else {
                    this.emitInnerFunction(funcDecl, (funcDecl.name && !funcDecl.name.isMissing()), false, bases, hasSelfRef, this.thisClassNode);
                }
                this.setInObjectLiteral(tempLit);
            }
            this.setContainer(temp);
            this.thisFnc = tempFnc;
            if(!TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Signature)) {
                if(TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Static)) {
                    if(this.thisClassNode) {
                        if(funcDecl.isAccessor()) {
                            this.emitPropertyAccessor(funcDecl, this.thisClassNode.name.actualText, false);
                        } else {
                            this.emitIndent();
                            this.recordSourceMappingStart(funcDecl);
                            this.writeLineToOutput(this.thisClassNode.name.actualText + "." + funcName + " = " + funcName + ";");
                            this.recordSourceMappingEnd(funcDecl);
                        }
                    }
                } else if((this.emitState.container == EmitContainer.Module || this.emitState.container == EmitContainer.DynamicModule) && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Exported | TypeScript.FncFlags.ClassPropertyMethodExported)) {
                    this.emitIndent();
                    var modName = this.emitState.container == EmitContainer.Module ? this.moduleName : "exports";
                    this.recordSourceMappingStart(funcDecl);
                    this.writeLineToOutput(modName + "." + funcName + " = " + funcName + ";");
                    this.recordSourceMappingEnd(funcDecl);
                }
            }
        };
        Emitter.prototype.emitAmbientVarDecl = function (varDecl) {
            if(varDecl.init) {
                this.emitParensAndCommentsInPlace(varDecl, true);
                this.recordSourceMappingStart(varDecl);
                this.recordSourceMappingStart(varDecl.id);
                this.writeToOutput(varDecl.id.actualText);
                this.recordSourceMappingEnd(varDecl.id);
                this.writeToOutput(" = ");
                this.emitJavascript(varDecl.init, TypeScript.TokenID.Comma, false);
                this.recordSourceMappingEnd(varDecl);
                this.writeToOutput(";");
                this.emitParensAndCommentsInPlace(varDecl, false);
            }
        };
        Emitter.prototype.varListCount = function () {
            return this.varListCountStack[this.varListCountStack.length - 1];
        };
        Emitter.prototype.emitVarDeclVar = function () {
            if(this.varListCount() >= 0) {
                this.writeToOutput("var ");
                this.setInVarBlock(-this.varListCount());
            }
            return true;
        };
        Emitter.prototype.onEmitVar = function () {
            if(this.varListCount() > 0) {
                this.setInVarBlock(this.varListCount() - 1);
            } else if(this.varListCount() < 0) {
                this.setInVarBlock(this.varListCount() + 1);
            }
        };
        Emitter.prototype.emitJavascriptVarDecl = function (varDecl, tokenId) {
            if((varDecl.varFlags & TypeScript.VarFlags.Ambient) == TypeScript.VarFlags.Ambient) {
                this.emitAmbientVarDecl(varDecl);
                this.onEmitVar();
            } else {
                var sym = varDecl.sym;
                var hasInitializer = (varDecl.init != null);
                this.emitParensAndCommentsInPlace(varDecl, true);
                this.recordSourceMappingStart(varDecl);
                if(sym && sym.isMember() && sym.container && (sym.container.kind() == TypeScript.SymbolKind.Type)) {
                    var type = (sym.container).type;
                    if(type.isClass() && (!TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.ModuleMember))) {
                        if(this.emitState.container != EmitContainer.Args) {
                            if(TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Static)) {
                                this.writeToOutput(sym.container.name + ".");
                            } else {
                                this.writeToOutput("this.");
                            }
                        }
                    } else if(type.hasImplementation()) {
                        if(!TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Exported) && (sym.container == this.checker.gloMod || !TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Property))) {
                            this.emitVarDeclVar();
                        } else if(TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.LocalStatic)) {
                            this.writeToOutput(".");
                        } else {
                            if(this.emitState.container == EmitContainer.DynamicModule) {
                                this.writeToOutput("exports.");
                            } else {
                                this.writeToOutput(this.moduleName + ".");
                            }
                        }
                    } else {
                        if(tokenId != TypeScript.TokenID.OpenParen) {
                            if(TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Exported) && sym.container == this.checker.gloMod) {
                                this.writeToOutput("this.");
                            } else {
                                this.emitVarDeclVar();
                            }
                        }
                    }
                } else {
                    if(tokenId != TypeScript.TokenID.OpenParen) {
                        this.emitVarDeclVar();
                    }
                }
                this.recordSourceMappingStart(varDecl.id);
                this.writeToOutput(varDecl.id.actualText);
                this.recordSourceMappingEnd(varDecl.id);
                if(hasInitializer) {
                    this.writeToOutputTrimmable(" = ");
                    this.varListCountStack.push(0);
                    this.emitJavascript(varDecl.init, TypeScript.TokenID.Comma, false);
                    this.varListCountStack.pop();
                }
                this.onEmitVar();
                if((tokenId != TypeScript.TokenID.OpenParen)) {
                    if(this.varListCount() < 0) {
                        this.writeToOutput(", ");
                    } else if(tokenId != TypeScript.TokenID.For) {
                        this.writeToOutputTrimmable(";");
                    }
                }
                this.recordSourceMappingEnd(varDecl);
                this.emitParensAndCommentsInPlace(varDecl, false);
            }
        };
        Emitter.prototype.declEnclosed = function (moduleDecl) {
            if(moduleDecl == null) {
                return true;
            }
            for(var i = 0, len = this.moduleDeclList.length; i < len; i++) {
                if(this.moduleDeclList[i] == moduleDecl) {
                    return true;
                }
            }
            return false;
        };
        Emitter.prototype.emitJavascriptName = function (name, addThis) {
            var sym = name.sym;
            this.emitParensAndCommentsInPlace(name, true);
            this.recordSourceMappingStart(name);
            if(!name.isMissing()) {
                if(addThis && (this.emitState.container != EmitContainer.Args) && sym) {
                    if(sym.container && (sym.container.name != TypeScript.globalId)) {
                        if(TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Static) && (TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Property))) {
                            if(sym.declModule && TypeScript.hasFlag(sym.declModule.modFlags, TypeScript.ModuleFlags.IsDynamic)) {
                                this.writeToOutput("exports.");
                            } else {
                                this.writeToOutput(sym.container.name + ".");
                            }
                        } else if(sym.kind() == TypeScript.SymbolKind.Field) {
                            var fieldSym = sym;
                            if(TypeScript.hasFlag(fieldSym.flags, TypeScript.SymbolFlags.ModuleMember)) {
                                if((sym.container != this.checker.gloMod) && ((TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Property)) || TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Exported))) {
                                    if(TypeScript.hasFlag(sym.declModule.modFlags, TypeScript.ModuleFlags.IsDynamic)) {
                                        this.writeToOutput("exports.");
                                    } else {
                                        this.writeToOutput(sym.container.name + ".");
                                    }
                                }
                            } else {
                                if(sym.isInstanceProperty()) {
                                    this.emitThis();
                                    this.writeToOutput(".");
                                }
                            }
                        } else if(sym.kind() == TypeScript.SymbolKind.Type) {
                            if(sym.isInstanceProperty()) {
                                var typeSym = sym;
                                var type = typeSym.type;
                                if(type.call && !TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.ModuleMember)) {
                                    this.emitThis();
                                    this.writeToOutput(".");
                                }
                            } else if((sym.unitIndex != this.checker.locationInfo.unitIndex) || (!this.declEnclosed(sym.declModule))) {
                                this.writeToOutput(sym.container.name + ".");
                            }
                        }
                    } else if(sym.container == this.checker.gloMod && TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Exported) && !TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Ambient) && !((sym.isType() || sym.isMember()) && sym.declModule && TypeScript.hasFlag(sym.declModule.modFlags, TypeScript.ModuleFlags.Ambient)) && this.emitState.container == EmitContainer.Prog && sym.declAST.nodeType != TypeScript.NodeType.FuncDecl) {
                        this.writeToOutput("this.");
                    }
                }
                if(sym && sym.declAST && sym.declAST.nodeType == TypeScript.NodeType.ModuleDeclaration && (TypeScript.hasFlag((sym.declAST).modFlags, TypeScript.ModuleFlags.IsDynamic))) {
                    var moduleDecl = sym.declAST;
                    if(TypeScript.moduleGenTarget == TypeScript.ModuleGenTarget.Asynchronous) {
                        this.writeLineToOutput("__" + this.modAliasId + "__;");
                    } else {
                        var modPath = name.actualText;
                        var isSingleQuotedMod = TypeScript.isSingleQuoted(modPath);
                        var isAmbient = moduleDecl.mod.symbol.declAST && TypeScript.hasFlag((moduleDecl.mod.symbol.declAST).modFlags, TypeScript.ModuleFlags.Ambient);
                        modPath = isAmbient ? modPath : this.firstModAlias ? this.firstModAlias : TypeScript.quoteBaseName(modPath);
                        modPath = isAmbient ? modPath : (!TypeScript.isRelative(TypeScript.stripQuotes(modPath)) ? TypeScript.quoteStr("./" + TypeScript.stripQuotes(modPath)) : modPath);
                        if(isSingleQuotedMod) {
                            modPath = TypeScript.changeToSingleQuote(modPath);
                        }
                        this.writeToOutput("require(" + modPath + ")");
                    }
                } else {
                    this.writeToOutput(name.actualText);
                }
            }
            this.recordSourceMappingEnd(name);
            this.emitParensAndCommentsInPlace(name, false);
        };
        Emitter.prototype.emitJavascriptStatements = function (stmts, emitEmptyBod) {
            if(stmts) {
                if(stmts.nodeType != TypeScript.NodeType.Block) {
                    var hasContents = (stmts && (stmts.nodeType != TypeScript.NodeType.List || ((stmts).members.length > 0)));
                    if(emitEmptyBod || hasContents) {
                        var hasOnlyBlockStatement = ((stmts.nodeType == TypeScript.NodeType.Block) || ((stmts.nodeType == TypeScript.NodeType.List) && ((stmts).members.length == 1) && ((stmts).members[0].nodeType == TypeScript.NodeType.Block)));
                        this.recordSourceMappingStart(stmts);
                        if(!hasOnlyBlockStatement) {
                            this.writeLineToOutput(" {");
                            this.indenter.increaseIndent();
                        }
                        this.emitJavascriptList(stmts, null, TypeScript.TokenID.Semicolon, true, false, false);
                        if(!hasOnlyBlockStatement) {
                            this.writeLineToOutput("");
                            this.indenter.decreaseIndent();
                            this.emitIndent();
                            this.writeToOutput("}");
                        }
                        this.recordSourceMappingEnd(stmts);
                    }
                } else {
                    this.emitJavascript(stmts, TypeScript.TokenID.Semicolon, true);
                }
            } else if(emitEmptyBod) {
                this.writeToOutput("{ }");
            }
        };
        Emitter.prototype.emitBareJavascriptStatements = function (stmts, emitClassPropertiesAfterSuperCall) {
            if (typeof emitClassPropertiesAfterSuperCall === "undefined") { emitClassPropertiesAfterSuperCall = false; }
            if(stmts.nodeType != TypeScript.NodeType.Block) {
                if(stmts.nodeType == TypeScript.NodeType.List) {
                    var stmtList = stmts;
                    if((stmtList.members.length == 2) && (stmtList.members[0].nodeType == TypeScript.NodeType.Block) && (stmtList.members[1].nodeType == TypeScript.NodeType.EndCode)) {
                        this.emitJavascript(stmtList.members[0], TypeScript.TokenID.Semicolon, true);
                        this.writeLineToOutput("");
                    } else {
                        this.emitJavascriptList(stmts, null, TypeScript.TokenID.Semicolon, true, false, emitClassPropertiesAfterSuperCall);
                    }
                } else {
                    this.emitJavascript(stmts, TypeScript.TokenID.Semicolon, true);
                }
            } else {
                this.emitJavascript(stmts, TypeScript.TokenID.Semicolon, true);
            }
        };
        Emitter.prototype.recordSourceMappingNameStart = function (name) {
            if(this.sourceMapper) {
                var finalName = name;
                if(!name) {
                    finalName = "";
                } else if(this.sourceMapper.currentNameIndex.length > 0) {
                    finalName = this.sourceMapper.names[this.sourceMapper.currentNameIndex[this.sourceMapper.currentNameIndex.length - 1]] + "." + name;
                }
                this.sourceMapper.names.push(finalName);
                this.sourceMapper.currentNameIndex.push(this.sourceMapper.names.length - 1);
            }
        };
        Emitter.prototype.recordSourceMappingNameEnd = function () {
            if(this.sourceMapper) {
                this.sourceMapper.currentNameIndex.pop();
            }
        };
        Emitter.prototype.recordSourceMappingStart = function (ast) {
            if(this.sourceMapper && TypeScript.isValidAstNode(ast)) {
                var lineCol = {
                    line: -1,
                    col: -1
                };
                var sourceMapping = new TypeScript.SourceMapping();
                sourceMapping.start.emittedColumn = this.emitState.column;
                sourceMapping.start.emittedLine = this.emitState.line;
                TypeScript.getSourceLineColFromMap(lineCol, ast.minChar, this.checker.locationInfo.lineMap);
                sourceMapping.start.sourceColumn = lineCol.col;
                sourceMapping.start.sourceLine = lineCol.line;
                TypeScript.getSourceLineColFromMap(lineCol, ast.limChar, this.checker.locationInfo.lineMap);
                sourceMapping.end.sourceColumn = lineCol.col;
                sourceMapping.end.sourceLine = lineCol.line;
                if(this.sourceMapper.currentNameIndex.length > 0) {
                    sourceMapping.nameIndex = this.sourceMapper.currentNameIndex[this.sourceMapper.currentNameIndex.length - 1];
                }
                var siblings = this.sourceMapper.currentMappings[this.sourceMapper.currentMappings.length - 1];
                siblings.push(sourceMapping);
                this.sourceMapper.currentMappings.push(sourceMapping.childMappings);
            }
        };
        Emitter.prototype.recordSourceMappingEnd = function (ast) {
            if(this.sourceMapper && TypeScript.isValidAstNode(ast)) {
                this.sourceMapper.currentMappings.pop();
                var siblings = this.sourceMapper.currentMappings[this.sourceMapper.currentMappings.length - 1];
                var sourceMapping = siblings[siblings.length - 1];
                sourceMapping.end.emittedColumn = this.emitState.column;
                sourceMapping.end.emittedLine = this.emitState.line;
            }
        };
        Emitter.prototype.Close = function () {
            if(this.sourceMapper != null) {
                TypeScript.SourceMapper.EmitSourceMapping(this.allSourceMappers);
            }
            try  {
                this.outfile.Close();
            } catch (ex) {
                this.errorReporter.emitterError(null, ex.message);
            }
        };
        Emitter.prototype.emitJavascriptList = function (ast, delimiter, tokenId, startLine, onlyStatics, emitClassPropertiesAfterSuperCall, emitPrologue, requiresExtendsBlock) {
            if (typeof emitClassPropertiesAfterSuperCall === "undefined") { emitClassPropertiesAfterSuperCall = false; }
            if (typeof emitPrologue === "undefined") { emitPrologue = false; }
            if(ast == null) {
                return;
            } else if(ast.nodeType != TypeScript.NodeType.List) {
                this.emitPrologue(emitPrologue);
                this.emitJavascript(ast, tokenId, startLine);
            } else {
                var list = ast;
                this.emitParensAndCommentsInPlace(ast, true);
                if(list.members.length == 0) {
                    this.emitParensAndCommentsInPlace(ast, false);
                    return;
                }
                var len = list.members.length;
                for(var i = 0; i < len; i++) {
                    if(emitPrologue) {
                        if(i == 1 || !TypeScript.hasFlag(list.flags, TypeScript.ASTFlags.StrictMode)) {
                            this.emitPrologue(requiresExtendsBlock);
                            emitPrologue = false;
                        }
                    }
                    if(i == 1 && emitClassPropertiesAfterSuperCall) {
                        var constructorDecl = (this.thisClassNode).constructorDecl;
                        if(constructorDecl && constructorDecl.arguments) {
                            var argsLen = constructorDecl.arguments.members.length;
                            for(var iArg = 0; iArg < argsLen; iArg++) {
                                var arg = constructorDecl.arguments.members[iArg];
                                if((arg.varFlags & TypeScript.VarFlags.Property) != TypeScript.VarFlags.None) {
                                    this.emitIndent();
                                    this.recordSourceMappingStart(arg);
                                    this.recordSourceMappingStart(arg.id);
                                    this.writeToOutput("this." + arg.id.actualText);
                                    this.recordSourceMappingEnd(arg.id);
                                    this.writeToOutput(" = ");
                                    this.recordSourceMappingStart(arg.id);
                                    this.writeToOutput(arg.id.actualText);
                                    this.recordSourceMappingEnd(arg.id);
                                    this.writeLineToOutput(";");
                                    this.recordSourceMappingEnd(arg);
                                }
                            }
                        }
                        var nProps = (this.thisClassNode.members).members.length;
                        for(var iMember = 0; iMember < nProps; iMember++) {
                            if((this.thisClassNode.members).members[iMember].nodeType == TypeScript.NodeType.VarDecl) {
                                var varDecl = (this.thisClassNode.members).members[iMember];
                                if(!TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Static) && varDecl.init) {
                                    this.emitIndent();
                                    this.emitJavascriptVarDecl(varDecl, TypeScript.TokenID.Tilde);
                                    this.writeLineToOutput("");
                                }
                            }
                        }
                    }
                    var emitNode = list.members[i];
                    var isStaticDecl = (emitNode.nodeType == TypeScript.NodeType.FuncDecl && TypeScript.hasFlag((emitNode).fncFlags, TypeScript.FncFlags.Static)) || (emitNode.nodeType == TypeScript.NodeType.VarDecl && TypeScript.hasFlag((emitNode).varFlags, TypeScript.VarFlags.Static));
                    if(onlyStatics ? !isStaticDecl : isStaticDecl) {
                        continue;
                    }
                    this.emitJavascript(emitNode, tokenId, startLine);
                    if(delimiter && (i < (len - 1))) {
                        if(startLine) {
                            this.writeLineToOutput(delimiter);
                        } else {
                            this.writeToOutput(delimiter);
                        }
                    } else if(startLine && (emitNode.nodeType != TypeScript.NodeType.ModuleDeclaration) && (emitNode.nodeType != TypeScript.NodeType.InterfaceDeclaration) && (!((emitNode.nodeType == TypeScript.NodeType.VarDecl) && ((((emitNode).varFlags) & TypeScript.VarFlags.Ambient) == TypeScript.VarFlags.Ambient) && (((emitNode).init) == null)) && this.varListCount() >= 0) && (emitNode.nodeType != TypeScript.NodeType.Block || (emitNode).isStatementBlock) && (emitNode.nodeType != TypeScript.NodeType.EndCode) && (emitNode.nodeType != TypeScript.NodeType.FuncDecl)) {
                        this.writeLineToOutput("");
                    }
                }
                this.emitParensAndCommentsInPlace(ast, false);
            }
        };
        Emitter.prototype.emitJavascript = function (ast, tokenId, startLine) {
            if(ast == null) {
                return;
            }
            if(startLine && (this.indenter.indentAmt > 0) && (ast.nodeType != TypeScript.NodeType.List) && (ast.nodeType != TypeScript.NodeType.Block)) {
                if((ast.nodeType != TypeScript.NodeType.InterfaceDeclaration) && (!((ast.nodeType == TypeScript.NodeType.VarDecl) && ((((ast).varFlags) & TypeScript.VarFlags.Ambient) == TypeScript.VarFlags.Ambient) && (((ast).init) == null)) && this.varListCount() >= 0) && (ast.nodeType != TypeScript.NodeType.EndCode) && ((ast.nodeType != TypeScript.NodeType.FuncDecl) || (this.emitState.container != EmitContainer.Constructor))) {
                    this.emitIndent();
                }
            }
            ast.emit(this, tokenId, startLine);
            if((tokenId == TypeScript.TokenID.Semicolon) && (ast.nodeType < TypeScript.NodeType.GeneralNode)) {
                this.writeToOutput(";");
            }
        };
        Emitter.prototype.emitPropertyAccessor = function (funcDecl, className, isProto) {
            if(!(funcDecl.accessorSymbol).hasBeenEmitted) {
                var accessorSymbol = funcDecl.accessorSymbol;
                this.emitIndent();
                this.recordSourceMappingStart(funcDecl);
                this.writeLineToOutput("Object.defineProperty(" + className + (isProto ? ".prototype, \"" : ", \"") + funcDecl.name.actualText + "\"" + ", {");
                this.indenter.increaseIndent();
                if(accessorSymbol.getter) {
                    var getter = accessorSymbol.getter.declAST;
                    this.emitIndent();
                    this.recordSourceMappingStart(getter);
                    this.writeToOutput("get: ");
                    this.emitInnerFunction(getter, false, isProto, null, Emitter.shouldCaptureThis(getter), null);
                    this.writeLineToOutput(",");
                }
                if(accessorSymbol.setter) {
                    var setter = accessorSymbol.setter.declAST;
                    this.emitIndent();
                    this.recordSourceMappingStart(setter);
                    this.writeToOutput("set: ");
                    this.emitInnerFunction(setter, false, isProto, null, Emitter.shouldCaptureThis(setter), null);
                    this.writeLineToOutput(",");
                }
                this.emitIndent();
                this.writeLineToOutput("enumerable: true,");
                this.emitIndent();
                this.writeLineToOutput("configurable: true");
                this.indenter.decreaseIndent();
                this.emitIndent();
                this.writeLineToOutput("});");
                this.recordSourceMappingEnd(funcDecl);
                accessorSymbol.hasBeenEmitted = true;
            }
        };
        Emitter.prototype.emitPrototypeMember = function (member, className) {
            if(member.nodeType == TypeScript.NodeType.FuncDecl) {
                var funcDecl = member;
                if(funcDecl.isAccessor()) {
                    this.emitPropertyAccessor(funcDecl, className, true);
                } else {
                    this.emitIndent();
                    this.recordSourceMappingStart(funcDecl);
                    this.writeToOutput(className + ".prototype." + funcDecl.getNameText() + " = ");
                    this.emitInnerFunction(funcDecl, false, true, null, Emitter.shouldCaptureThis(funcDecl), null);
                    this.writeLineToOutput(";");
                }
            } else if(member.nodeType == TypeScript.NodeType.VarDecl) {
                var varDecl = member;
                if(varDecl.init) {
                    this.emitIndent();
                    this.recordSourceMappingStart(varDecl);
                    this.recordSourceMappingStart(varDecl.id);
                    this.writeToOutput(className + ".prototype." + varDecl.id.actualText);
                    this.recordSourceMappingEnd(varDecl.id);
                    this.writeToOutput(" = ");
                    this.emitJavascript(varDecl.init, TypeScript.TokenID.Equals, false);
                    this.recordSourceMappingEnd(varDecl);
                    this.writeLineToOutput(";");
                }
            }
        };
        Emitter.prototype.emitAddBaseMethods = function (className, base, classDecl) {
            if(base.members) {
                var baseSymbol = base.symbol;
                var baseName = baseSymbol.name;
                if(baseSymbol.declModule != classDecl.type.symbol.declModule) {
                    baseName = baseSymbol.fullName();
                }
                base.members.allMembers.map(function (key, s, c) {
                    var sym = s;
                    if((sym.kind() == TypeScript.SymbolKind.Type) && (sym).type.call) {
                        this.recordSourceMappingStart(sym.declAST);
                        this.writeLineToOutput(className + ".prototype." + sym.name + " = " + baseName + ".prototype." + sym.name + ";");
                        this.recordSourceMappingEnd(sym.declAST);
                    }
                }, null);
            }
            if(base.extendsList) {
                for(var i = 0, len = base.extendsList.length; i < len; i++) {
                    this.emitAddBaseMethods(className, base.extendsList[i], classDecl);
                }
            }
        };
        Emitter.prototype.emitJavascriptClass = function (classDecl) {
            if(!TypeScript.hasFlag(classDecl.varFlags, TypeScript.VarFlags.Ambient)) {
                var svClassNode = this.thisClassNode;
                var i = 0;
                this.thisClassNode = classDecl;
                var className = classDecl.name.actualText;
                this.emitParensAndCommentsInPlace(classDecl, true);
                var temp = this.setContainer(EmitContainer.Class);
                this.recordSourceMappingStart(classDecl);
                if(TypeScript.hasFlag(classDecl.varFlags, TypeScript.VarFlags.Exported) && classDecl.type.symbol.container == this.checker.gloMod) {
                    this.writeToOutput("this." + className);
                } else {
                    this.writeToOutput("var " + className);
                }
                var hasBaseClass = classDecl.extendsList && classDecl.extendsList.members.length;
                var baseNameDecl = null;
                var baseName = null;
                if(hasBaseClass) {
                    this.writeLineToOutput(" = (function (_super) {");
                } else {
                    this.writeLineToOutput(" = (function () {");
                }
                this.recordSourceMappingNameStart(className);
                this.indenter.increaseIndent();
                if(hasBaseClass) {
                    baseNameDecl = classDecl.extendsList.members[0];
                    baseName = baseNameDecl.nodeType == TypeScript.NodeType.Call ? (baseNameDecl).target : baseNameDecl;
                    this.emitIndent();
                    this.writeLineToOutput("__extends(" + className + ", _super);");
                }
                this.emitIndent();
                var constrDecl = classDecl.constructorDecl;
                if(constrDecl) {
                    this.emitJavascript(classDecl.constructorDecl, TypeScript.TokenID.OpenParen, false);
                } else {
                    var wroteProps = 0;
                    this.recordSourceMappingStart(classDecl);
                    this.indenter.increaseIndent();
                    this.writeToOutput("function " + classDecl.name.actualText + "() {");
                    this.recordSourceMappingNameStart("constructor");
                    if(hasBaseClass) {
                        this.writeLineToOutput("");
                        this.emitIndent();
                        this.writeLineToOutput("_super.apply(this, arguments);");
                        wroteProps++;
                    }
                    if(classDecl.varFlags & TypeScript.VarFlags.MustCaptureThis) {
                        this.writeCaptureThisStatement(classDecl);
                    }
                    var members = (this.thisClassNode.members).members;
                    for(var i = 0; i < members.length; i++) {
                        if(members[i].nodeType == TypeScript.NodeType.VarDecl) {
                            var varDecl = members[i];
                            if(!TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Static) && varDecl.init) {
                                this.writeLineToOutput("");
                                this.emitIndent();
                                this.emitJavascriptVarDecl(varDecl, TypeScript.TokenID.Tilde);
                                wroteProps++;
                            }
                        }
                    }
                    if(wroteProps) {
                        this.writeLineToOutput("");
                        this.indenter.decreaseIndent();
                        this.emitIndent();
                        this.writeLineToOutput("}");
                    } else {
                        this.writeLineToOutput(" }");
                        this.indenter.decreaseIndent();
                    }
                    this.recordSourceMappingNameEnd();
                    this.recordSourceMappingEnd(classDecl);
                }
                var membersLen = classDecl.members.members.length;
                for(var j = 0; j < membersLen; j++) {
                    var memberDecl = classDecl.members.members[j];
                    if(memberDecl.nodeType == TypeScript.NodeType.FuncDecl) {
                        var fn = memberDecl;
                        if(TypeScript.hasFlag(fn.fncFlags, TypeScript.FncFlags.Method) && !fn.isSignature()) {
                            if(!TypeScript.hasFlag(fn.fncFlags, TypeScript.FncFlags.Static)) {
                                this.emitPrototypeMember(fn, className);
                            } else {
                                if(fn.isAccessor()) {
                                    this.emitPropertyAccessor(fn, this.thisClassNode.name.actualText, false);
                                } else {
                                    this.emitIndent();
                                    this.recordSourceMappingStart(fn);
                                    this.writeToOutput(classDecl.name.actualText + "." + fn.name.actualText + " = ");
                                    this.emitInnerFunction(fn, (fn.name && !fn.name.isMissing()), true, null, Emitter.shouldCaptureThis(fn), null);
                                    this.writeLineToOutput(";");
                                }
                            }
                        }
                    } else if(memberDecl.nodeType == TypeScript.NodeType.VarDecl) {
                        var varDecl = memberDecl;
                        if(TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Static)) {
                            if(varDecl.init) {
                                this.emitIndent();
                                this.recordSourceMappingStart(varDecl);
                                this.writeToOutput(classDecl.name.actualText + "." + varDecl.id.actualText + " = ");
                                this.emitJavascript(varDecl.init, TypeScript.TokenID.Equals, false);
                                this.writeLineToOutput(";");
                                this.recordSourceMappingEnd(varDecl);
                            }
                        }
                    } else {
                        throw Error("We want to catch this");
                    }
                }
                this.emitIndent();
                this.recordSourceMappingStart(classDecl.endingToken);
                this.writeLineToOutput("return " + className + ";");
                this.recordSourceMappingEnd(classDecl.endingToken);
                this.indenter.decreaseIndent();
                this.emitIndent();
                this.recordSourceMappingStart(classDecl.endingToken);
                this.writeToOutput("}");
                this.recordSourceMappingNameEnd();
                this.recordSourceMappingEnd(classDecl.endingToken);
                this.recordSourceMappingStart(classDecl);
                this.writeToOutput(")(");
                if(hasBaseClass) {
                    this.emitJavascript(baseName, TypeScript.TokenID.Tilde, false);
                }
                this.writeToOutput(");");
                this.recordSourceMappingEnd(classDecl);
                if((temp == EmitContainer.Module || temp == EmitContainer.DynamicModule) && TypeScript.hasFlag(classDecl.varFlags, TypeScript.VarFlags.Exported)) {
                    this.writeLineToOutput("");
                    this.emitIndent();
                    var modName = temp == EmitContainer.Module ? this.moduleName : "exports";
                    this.recordSourceMappingStart(classDecl);
                    this.writeToOutput(modName + "." + className + " = " + className + ";");
                    this.recordSourceMappingEnd(classDecl);
                }
                this.emitIndent();
                this.recordSourceMappingEnd(classDecl);
                this.emitParensAndCommentsInPlace(classDecl, false);
                this.setContainer(temp);
                this.thisClassNode = svClassNode;
            }
        };
        Emitter.prototype.emitPrologue = function (reqInherits) {
            if(!this.extendsPrologueEmitted) {
                if(reqInherits) {
                    this.extendsPrologueEmitted = true;
                    this.writeLineToOutput("var __extends = this.__extends || function (d, b) {");
                    this.writeLineToOutput("    function __() { this.constructor = d; }");
                    this.writeLineToOutput("    __.prototype = b.prototype;");
                    this.writeLineToOutput("    d.prototype = new __();");
                    this.writeLineToOutput("};");
                }
            }
            if(!this.globalThisCapturePrologueEmitted) {
                if(this.checker.mustCaptureGlobalThis) {
                    this.globalThisCapturePrologueEmitted = true;
                    this.writeLineToOutput(this.captureThisStmtString);
                }
            }
        };
        Emitter.prototype.emitSuperReference = function () {
            this.writeToOutput("_super.prototype");
        };
        Emitter.prototype.emitSuperCall = function (callEx) {
            if(callEx.target.nodeType == TypeScript.NodeType.Dot) {
                var dotNode = callEx.target;
                if(dotNode.operand1.nodeType == TypeScript.NodeType.Super) {
                    this.emitJavascript(dotNode, TypeScript.TokenID.OpenParen, false);
                    this.writeToOutput(".call(");
                    this.emitThis();
                    if(callEx.arguments && callEx.arguments.members.length > 0) {
                        this.writeToOutput(", ");
                        this.emitJavascriptList(callEx.arguments, ", ", TypeScript.TokenID.Comma, false, false, false);
                    }
                    this.writeToOutput(")");
                    return true;
                }
            }
            return false;
        };
        Emitter.prototype.emitThis = function () {
            if(this.thisFnc && !this.thisFnc.isMethod() && (!this.thisFnc.isConstructor)) {
                this.writeToOutput("_this");
            } else {
                this.writeToOutput("this");
            }
        };
        Emitter.shouldCaptureThis = function shouldCaptureThis(func) {
            return func.hasSelfReference() || func.hasSuperReferenceInFatArrowFunction();
        };
        Emitter.prototype.createFile = function (fileName, useUTF8) {
            try  {
                return this.emitOptions.ioHost.createFile(fileName, useUTF8);
            } catch (ex) {
                this.errorReporter.emitterError(null, ex.message);
            }
        };
        return Emitter;
    })();
    TypeScript.Emitter = Emitter;    
})(TypeScript || (TypeScript = {}));
