var TypeScript;
(function (TypeScript) {
    var DeclFileWriter = (function () {
        function DeclFileWriter(declFile) {
            this.declFile = declFile;
            this.onNewLine = true;
        }
        DeclFileWriter.prototype.Write = function (s) {
            this.declFile.Write(s);
            this.onNewLine = false;
        };
        DeclFileWriter.prototype.WriteLine = function (s) {
            this.declFile.WriteLine(s);
            this.onNewLine = true;
        };
        DeclFileWriter.prototype.Close = function () {
            this.declFile.Close();
        };
        return DeclFileWriter;
    })();
    TypeScript.DeclFileWriter = DeclFileWriter;    
    var DeclarationEmitter = (function () {
        function DeclarationEmitter(checker, emitOptions, errorReporter) {
            this.checker = checker;
            this.emitOptions = emitOptions;
            this.errorReporter = errorReporter;
            this.declFile = null;
            this.indenter = new TypeScript.Indenter();
            this.declarationContainerStack = [];
            this.isDottedModuleName = [];
            this.ignoreCallbackAst = null;
            this.singleDeclFile = null;
            this.varListCount = 0;
        }
        DeclarationEmitter.prototype.getAstDeclarationContainer = function () {
            return this.declarationContainerStack[this.declarationContainerStack.length - 1];
        };
        DeclarationEmitter.prototype.emitDottedModuleName = function () {
            return (this.isDottedModuleName.length == 0) ? false : this.isDottedModuleName[this.isDottedModuleName.length - 1];
        };
        DeclarationEmitter.prototype.setDeclarationFile = function (file) {
            this.declFile = new DeclFileWriter(file);
        };
        DeclarationEmitter.prototype.Close = function () {
            try  {
                this.declFile.Close();
            } catch (ex) {
                this.errorReporter.emitterError(null, ex.message);
            }
        };
        DeclarationEmitter.prototype.emitDeclarations = function (script) {
            TypeScript.AstWalkerWithDetailCallback.walk(script, this);
        };
        DeclarationEmitter.prototype.getIndentString = function (declIndent) {
            if (typeof declIndent === "undefined") { declIndent = false; }
            if(this.emitOptions.minWhitespace) {
                return "";
            } else {
                return this.indenter.getIndent();
            }
        };
        DeclarationEmitter.prototype.emitIndent = function () {
            this.declFile.Write(this.getIndentString());
        };
        DeclarationEmitter.prototype.canEmitSignature = function (declFlags, canEmitGlobalAmbientDecl, useDeclarationContainerTop) {
            if (typeof canEmitGlobalAmbientDecl === "undefined") { canEmitGlobalAmbientDecl = true; }
            if (typeof useDeclarationContainerTop === "undefined") { useDeclarationContainerTop = true; }
            var container;
            if(useDeclarationContainerTop) {
                container = this.getAstDeclarationContainer();
            } else {
                container = this.declarationContainerStack[this.declarationContainerStack.length - 2];
            }
            if(container.nodeType == TypeScript.NodeType.ModuleDeclaration && !TypeScript.hasFlag(declFlags, TypeScript.DeclFlags.Exported)) {
                return false;
            }
            if(!canEmitGlobalAmbientDecl && container.nodeType == TypeScript.NodeType.Script && TypeScript.hasFlag(declFlags, TypeScript.DeclFlags.Ambient)) {
                return false;
            }
            return true;
        };
        DeclarationEmitter.prototype.canEmitPrePostAstSignature = function (declFlags, astWithPrePostCallback, preCallback) {
            if(this.ignoreCallbackAst) {
                TypeScript.CompilerDiagnostics.assert(this.ignoreCallbackAst != astWithPrePostCallback, "Ignore Callback AST mismatch");
                this.ignoreCallbackAst = null;
                return false;
            } else if(preCallback && !this.canEmitSignature(declFlags, true, preCallback)) {
                this.ignoreCallbackAst = astWithPrePostCallback;
                return false;
            }
            return true;
        };
        DeclarationEmitter.prototype.getDeclFlagsString = function (declFlags, typeString) {
            var result = this.getIndentString();
            var accessorString = "";
            if(TypeScript.hasFlag(declFlags, TypeScript.DeclFlags.GetAccessor)) {
                accessorString = "get ";
            } else if(TypeScript.hasFlag(declFlags, TypeScript.DeclFlags.SetAccessor)) {
                accessorString = "set ";
            }
            var container = this.getAstDeclarationContainer();
            if(container.nodeType == TypeScript.NodeType.ModuleDeclaration && TypeScript.hasFlag((container).modFlags, TypeScript.ModuleFlags.IsWholeFile) && TypeScript.hasFlag(declFlags, TypeScript.DeclFlags.Exported)) {
                result += "export ";
            }
            if(TypeScript.hasFlag(declFlags, TypeScript.DeclFlags.LocalStatic) || TypeScript.hasFlag(declFlags, TypeScript.DeclFlags.Static)) {
                if(TypeScript.hasFlag(declFlags, TypeScript.DeclFlags.Private)) {
                    result += "private ";
                }
                result += "static " + accessorString;
            } else {
                if(TypeScript.hasFlag(declFlags, TypeScript.DeclFlags.Private)) {
                    result += "private " + accessorString;
                } else if(TypeScript.hasFlag(declFlags, TypeScript.DeclFlags.Public)) {
                    result += "public " + accessorString;
                } else {
                    if(accessorString == "") {
                        result += typeString + " ";
                    } else {
                        result += accessorString;
                    }
                }
            }
            return result;
        };
        DeclarationEmitter.prototype.emitDeclFlags = function (declFlags, typeString) {
            this.declFile.Write(this.getDeclFlagsString(declFlags, typeString));
        };
        DeclarationEmitter.prototype.canEmitTypeAnnotationSignature = function (declFlag) {
            if (typeof declFlag === "undefined") { declFlag = TypeScript.DeclFlags.None; }
            return !TypeScript.hasFlag(declFlag, TypeScript.DeclFlags.Private);
        };
        DeclarationEmitter.prototype.pushDeclarationContainer = function (ast) {
            this.declarationContainerStack.push(ast);
        };
        DeclarationEmitter.prototype.popDeclarationContainer = function (ast) {
            TypeScript.CompilerDiagnostics.assert(ast != this.getAstDeclarationContainer(), 'Declaration container mismatch');
            this.declarationContainerStack.pop();
        };
        DeclarationEmitter.prototype.emitTypeNamesMember = function (memberName, emitIndent) {
            if (typeof emitIndent === "undefined") { emitIndent = false; }
            if(memberName.prefix == "{ ") {
                if(emitIndent) {
                    this.emitIndent();
                }
                this.declFile.WriteLine("{");
                this.indenter.increaseIndent();
                emitIndent = true;
            } else if(memberName.prefix != "") {
                if(emitIndent) {
                    this.emitIndent();
                }
                this.declFile.Write(memberName.prefix);
                emitIndent = false;
            }
            if(memberName.isString()) {
                if(emitIndent) {
                    this.emitIndent();
                }
                this.declFile.Write((memberName).text);
            } else {
                var ar = memberName;
                for(var index = 0; index < ar.entries.length; index++) {
                    this.emitTypeNamesMember(ar.entries[index], emitIndent);
                    if(ar.delim == "; ") {
                        this.declFile.WriteLine(";");
                    }
                }
            }
            if(memberName.suffix == "}") {
                this.indenter.decreaseIndent();
                this.emitIndent();
                this.declFile.Write(memberName.suffix);
            } else {
                this.declFile.Write(memberName.suffix);
            }
        };
        DeclarationEmitter.prototype.emitTypeSignature = function (type) {
            var containingScope = null;
            var declarationContainerAst = this.getAstDeclarationContainer();
            switch(declarationContainerAst.nodeType) {
                case TypeScript.NodeType.ModuleDeclaration:
                case TypeScript.NodeType.InterfaceDeclaration:
                case TypeScript.NodeType.FuncDecl:
                    if(declarationContainerAst.type) {
                        containingScope = declarationContainerAst.type.containedScope;
                    }
                    break;
                case TypeScript.NodeType.Script:
                    var script = declarationContainerAst;
                    if(script.bod) {
                        containingScope = script.bod.enclosingScope;
                    }
                    break;
                case TypeScript.NodeType.ClassDeclaration:
                    if(declarationContainerAst.type) {
                        containingScope = declarationContainerAst.type.instanceType.containedScope;
                    }
                    break;
                default:
                    TypeScript.CompilerDiagnostics.debugPrint("Unknown containing scope");
            }
            var typeNameMembers = type.getScopedTypeNameEx(containingScope);
            this.emitTypeNamesMember(typeNameMembers);
        };
        DeclarationEmitter.prototype.emitComment = function (comment) {
            var text = comment.getText();
            if(this.declFile.onNewLine) {
                this.emitIndent();
            } else if(!comment.isBlockComment) {
                this.declFile.WriteLine("");
                this.emitIndent();
            }
            this.declFile.Write(text[0]);
            for(var i = 1; i < text.length; i++) {
                this.declFile.WriteLine("");
                this.emitIndent();
                this.declFile.Write(text[i]);
            }
            if(comment.endsLine || !comment.isBlockComment) {
                this.declFile.WriteLine("");
            } else {
                this.declFile.Write(" ");
            }
        };
        DeclarationEmitter.prototype.emitDeclarationComments = function (astOrSymbol, endLine) {
            if (typeof endLine === "undefined") { endLine = true; }
            if(!this.emitOptions.emitComments) {
                return;
            }
            var declComments = astOrSymbol.getDocComments();
            if(declComments.length > 0) {
                for(var i = 0; i < declComments.length; i++) {
                    this.emitComment(declComments[i]);
                }
                if(endLine) {
                    if(!this.declFile.onNewLine) {
                        this.declFile.WriteLine("");
                    }
                } else {
                    if(this.declFile.onNewLine) {
                        this.emitIndent();
                    }
                }
            }
        };
        DeclarationEmitter.prototype.VarDeclCallback = function (pre, varDecl) {
            if(pre && this.canEmitSignature(TypeScript.ToDeclFlags(varDecl.varFlags), false)) {
                var interfaceMember = (this.getAstDeclarationContainer().nodeType == TypeScript.NodeType.InterfaceDeclaration);
                this.emitDeclarationComments(varDecl);
                if(!interfaceMember) {
                    if(this.varListCount >= 0) {
                        this.emitDeclFlags(TypeScript.ToDeclFlags(varDecl.varFlags), "var");
                        this.varListCount = -this.varListCount;
                    }
                    this.declFile.Write(varDecl.id.text);
                } else {
                    this.emitIndent();
                    this.declFile.Write(varDecl.id.text);
                    if(TypeScript.hasFlag(varDecl.id.flags, TypeScript.ASTFlags.OptionalName)) {
                        this.declFile.Write("?");
                    }
                }
                var type = null;
                if(varDecl.typeExpr && varDecl.typeExpr.type) {
                    type = varDecl.typeExpr.type;
                } else if(varDecl.sym) {
                    type = (varDecl.sym).getType();
                    if(type == this.checker.anyType) {
                        type = null;
                    }
                }
                if(type && this.canEmitTypeAnnotationSignature(TypeScript.ToDeclFlags(varDecl.varFlags))) {
                    this.declFile.Write(": ");
                    this.emitTypeSignature(type);
                }
                if(this.varListCount > 0) {
                    this.varListCount--;
                } else if(this.varListCount < 0) {
                    this.varListCount++;
                }
                if(this.varListCount < 0) {
                    this.declFile.Write(", ");
                } else {
                    this.declFile.WriteLine(";");
                }
            }
            return false;
        };
        DeclarationEmitter.prototype.BlockCallback = function (pre, block) {
            if(!block.isStatementBlock) {
                if(pre) {
                    this.varListCount = block.statements.members.length;
                } else {
                    this.varListCount = 0;
                }
                return true;
            }
            return false;
        };
        DeclarationEmitter.prototype.emitArgDecl = function (argDecl, funcDecl) {
            this.emitDeclarationComments(argDecl, false);
            this.declFile.Write(argDecl.id.text);
            if(argDecl.isOptionalArg()) {
                this.declFile.Write("?");
            }
            if((argDecl.typeExpr || argDecl.type != this.checker.anyType) && this.canEmitTypeAnnotationSignature(TypeScript.ToDeclFlags(funcDecl.fncFlags))) {
                this.declFile.Write(": ");
                this.emitTypeSignature(argDecl.type);
            }
        };
        DeclarationEmitter.prototype.FuncDeclCallback = function (pre, funcDecl) {
            if(!pre) {
                return false;
            }
            if(funcDecl.isAccessor()) {
                return this.emitPropertyAccessorSignature(funcDecl);
            }
            var isInterfaceMember = (this.getAstDeclarationContainer().nodeType == TypeScript.NodeType.InterfaceDeclaration);
            if(funcDecl.bod) {
                if(funcDecl.isConstructor) {
                    if(funcDecl.type.construct && funcDecl.type.construct.signatures.length > 1) {
                        return false;
                    }
                } else {
                    if(funcDecl.type.call && funcDecl.type.call.signatures.length > 1) {
                        return false;
                    }
                }
            } else if(!isInterfaceMember && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Private) && funcDecl.type.call && funcDecl.type.call.signatures.length > 1) {
                var signatures = funcDecl.type.call.signatures;
                var firstSignature = signatures[0].declAST;
                if(firstSignature.bod) {
                    firstSignature = signatures[1].declAST;
                }
                if(firstSignature != funcDecl) {
                    return false;
                }
            }
            if(!this.canEmitSignature(TypeScript.ToDeclFlags(funcDecl.fncFlags), false)) {
                return false;
            }
            this.emitDeclarationComments(funcDecl);
            if(funcDecl.isConstructor) {
                this.emitIndent();
                this.declFile.Write("constructor");
            } else {
                var id = funcDecl.getNameText();
                if(!isInterfaceMember) {
                    this.emitDeclFlags(TypeScript.ToDeclFlags(funcDecl.fncFlags), "function");
                    if(id != "__missing" || !funcDecl.name || !funcDecl.name.isMissing()) {
                        this.declFile.Write(id);
                    } else if(funcDecl.isConstructMember()) {
                        this.declFile.Write("new");
                    }
                } else {
                    this.emitIndent();
                    if(funcDecl.isConstructMember()) {
                        this.declFile.Write("new");
                    } else if(!funcDecl.isCallMember() && !funcDecl.isIndexerMember()) {
                        this.declFile.Write(id);
                        if(TypeScript.hasFlag(funcDecl.name.flags, TypeScript.ASTFlags.OptionalName)) {
                            this.declFile.Write("? ");
                        }
                    }
                }
            }
            if(!funcDecl.isIndexerMember()) {
                this.declFile.Write("(");
            } else {
                this.declFile.Write("[");
            }
            this.indenter.increaseIndent();
            if(funcDecl.arguments) {
                var argsLen = funcDecl.arguments.members.length;
                if(funcDecl.variableArgList) {
                    argsLen--;
                }
                for(var i = 0; i < argsLen; i++) {
                    var argDecl = funcDecl.arguments.members[i];
                    this.emitArgDecl(argDecl, funcDecl);
                    if(i < (argsLen - 1)) {
                        this.declFile.Write(", ");
                    }
                }
            }
            if(funcDecl.variableArgList) {
                var lastArg = funcDecl.arguments.members[funcDecl.arguments.members.length - 1];
                if(funcDecl.arguments.members.length > 1) {
                    this.declFile.Write(", ...");
                } else {
                    this.declFile.Write("...");
                }
                this.emitArgDecl(lastArg, funcDecl);
            }
            this.indenter.decreaseIndent();
            if(!funcDecl.isIndexerMember()) {
                this.declFile.Write(")");
            } else {
                this.declFile.Write("]");
            }
            if(!funcDecl.isConstructor && (funcDecl.returnTypeAnnotation || funcDecl.signature.returnType.type != this.checker.anyType) && this.canEmitTypeAnnotationSignature(TypeScript.ToDeclFlags(funcDecl.fncFlags))) {
                this.declFile.Write(": ");
                this.emitTypeSignature(funcDecl.signature.returnType.type);
            }
            this.declFile.WriteLine(";");
            return false;
        };
        DeclarationEmitter.prototype.emitBaseList = function (bases, qual) {
            if(bases && (bases.members.length > 0)) {
                this.declFile.Write(" " + qual + " ");
                var basesLen = bases.members.length;
                for(var i = 0; i < basesLen; i++) {
                    var baseExpr = bases.members[i];
                    var baseSymbol = baseExpr.type.symbol;
                    var baseType = baseExpr.type;
                    if(i > 0) {
                        this.declFile.Write(", ");
                    }
                    this.emitTypeSignature(baseType);
                }
            }
        };
        DeclarationEmitter.prototype.emitPropertyAccessorSignature = function (funcDecl) {
            var accessorSymbol = funcDecl.accessorSymbol;
            if(accessorSymbol.getter && accessorSymbol.getter.declAST != funcDecl) {
                return false;
            }
            this.emitDeclarationComments(accessorSymbol);
            this.emitDeclFlags(TypeScript.ToDeclFlags(accessorSymbol.flags), "var");
            this.declFile.Write(funcDecl.name.text);
            var propertyType = accessorSymbol.getType();
            if(this.canEmitTypeAnnotationSignature(TypeScript.ToDeclFlags(accessorSymbol.flags))) {
                this.declFile.Write(" : ");
                this.emitTypeSignature(propertyType);
            }
            this.declFile.WriteLine(";");
            return false;
        };
        DeclarationEmitter.prototype.emitClassMembersFromConstructorDefinition = function (funcDecl) {
            if(funcDecl.arguments) {
                var argsLen = funcDecl.arguments.members.length;
                if(funcDecl.variableArgList) {
                    argsLen--;
                }
                for(var i = 0; i < argsLen; i++) {
                    var argDecl = funcDecl.arguments.members[i];
                    if(TypeScript.hasFlag(argDecl.varFlags, TypeScript.VarFlags.Property)) {
                        this.emitDeclarationComments(argDecl);
                        this.emitDeclFlags(TypeScript.ToDeclFlags(argDecl.varFlags), "var");
                        this.declFile.Write(argDecl.id.text);
                        if(argDecl.typeExpr && this.canEmitTypeAnnotationSignature(TypeScript.ToDeclFlags(argDecl.varFlags))) {
                            this.declFile.Write(": ");
                            this.emitTypeSignature(argDecl.type);
                        }
                        this.declFile.WriteLine(";");
                    }
                }
            }
        };
        DeclarationEmitter.prototype.ClassDeclarationCallback = function (pre, classDecl) {
            if(!this.canEmitPrePostAstSignature(TypeScript.ToDeclFlags(classDecl.varFlags), classDecl, pre)) {
                return false;
            }
            if(pre) {
                var className = classDecl.name.text;
                this.emitDeclarationComments(classDecl);
                this.emitDeclFlags(TypeScript.ToDeclFlags(classDecl.varFlags), "class");
                this.declFile.Write(className);
                this.emitBaseList(classDecl.extendsList, "extends");
                this.emitBaseList(classDecl.implementsList, "implements");
                this.declFile.WriteLine(" {");
                this.pushDeclarationContainer(classDecl);
                this.indenter.increaseIndent();
                if(classDecl.constructorDecl) {
                    this.emitClassMembersFromConstructorDefinition(classDecl.constructorDecl);
                }
            } else {
                this.indenter.decreaseIndent();
                this.popDeclarationContainer(classDecl);
                this.emitIndent();
                this.declFile.WriteLine("}");
            }
            return true;
        };
        DeclarationEmitter.prototype.InterfaceDeclarationCallback = function (pre, interfaceDecl) {
            if(!this.canEmitPrePostAstSignature(TypeScript.ToDeclFlags(interfaceDecl.varFlags), interfaceDecl, pre)) {
                return false;
            }
            if(pre) {
                var interfaceName = interfaceDecl.name.text;
                this.emitDeclarationComments(interfaceDecl);
                this.emitDeclFlags(TypeScript.ToDeclFlags(interfaceDecl.varFlags), "interface");
                this.declFile.Write(interfaceName);
                this.emitBaseList(interfaceDecl.extendsList, "extends");
                this.declFile.WriteLine(" {");
                this.indenter.increaseIndent();
                this.pushDeclarationContainer(interfaceDecl);
            } else {
                this.indenter.decreaseIndent();
                this.popDeclarationContainer(interfaceDecl);
                this.emitIndent();
                this.declFile.WriteLine("}");
            }
            return true;
        };
        DeclarationEmitter.prototype.ImportDeclarationCallback = function (pre, importDecl) {
            if(pre) {
                if((this.declarationContainerStack[0]).isExternallyVisibleSymbol(importDecl.id.sym)) {
                    this.emitDeclarationComments(importDecl);
                    this.emitIndent();
                    this.declFile.Write("import ");
                    this.declFile.Write(importDecl.id.text + " = ");
                    if(importDecl.isDynamicImport) {
                        this.declFile.WriteLine("module (" + importDecl.getAliasName() + ");");
                    } else {
                        this.declFile.WriteLine(importDecl.getAliasName() + ";");
                    }
                }
            }
            return false;
        };
        DeclarationEmitter.prototype.emitEnumSignature = function (moduleDecl) {
            if(!this.canEmitSignature(TypeScript.ToDeclFlags(moduleDecl.modFlags))) {
                return false;
            }
            this.emitDeclarationComments(moduleDecl);
            this.emitDeclFlags(TypeScript.ToDeclFlags(moduleDecl.modFlags), "enum");
            this.declFile.WriteLine(moduleDecl.name.text + " {");
            this.indenter.increaseIndent();
            var membersLen = moduleDecl.members.members.length;
            for(var j = 1; j < membersLen; j++) {
                var memberDecl = moduleDecl.members.members[j];
                if(memberDecl.nodeType == TypeScript.NodeType.VarDecl) {
                    this.emitDeclarationComments(memberDecl);
                    this.emitIndent();
                    this.declFile.WriteLine((memberDecl).id.text + ",");
                } else {
                    TypeScript.CompilerDiagnostics.assert(memberDecl.nodeType != TypeScript.NodeType.Asg, "We want to catch this");
                }
            }
            this.indenter.decreaseIndent();
            this.emitIndent();
            this.declFile.WriteLine("}");
            return false;
        };
        DeclarationEmitter.prototype.ModuleDeclarationCallback = function (pre, moduleDecl) {
            if(TypeScript.hasFlag(moduleDecl.modFlags, TypeScript.ModuleFlags.IsWholeFile)) {
                if(TypeScript.hasFlag(moduleDecl.modFlags, TypeScript.ModuleFlags.IsDynamic)) {
                    if(pre) {
                        if(!this.emitOptions.outputMany) {
                            this.singleDeclFile = this.declFile;
                            TypeScript.CompilerDiagnostics.assert(this.indenter.indentAmt == 0, "Indent has to be 0 when outputing new file");
                            var declareFileName = this.emitOptions.mapOutputFileName(TypeScript.stripQuotes(moduleDecl.name.sym.name), TypeScript.TypeScriptCompiler.mapToDTSFileName);
                            var useUTF8InOutputfile = moduleDecl.containsUnicodeChar || (this.emitOptions.emitComments && moduleDecl.containsUnicodeCharInComment);
                            try  {
                                this.declFile = new DeclFileWriter(this.emitOptions.ioHost.createFile(declareFileName, useUTF8InOutputfile));
                            } catch (ex) {
                                this.errorReporter.emitterError(null, ex.message);
                            }
                        }
                        this.pushDeclarationContainer(moduleDecl);
                    } else {
                        if(!this.emitOptions.outputMany) {
                            TypeScript.CompilerDiagnostics.assert(this.singleDeclFile != this.declFile, "singleDeclFile cannot be null as we are going to revert back to it");
                            TypeScript.CompilerDiagnostics.assert(this.indenter.indentAmt == 0, "Indent has to be 0 when outputing new file");
                            try  {
                                this.declFile.Close();
                            } catch (ex) {
                                this.errorReporter.emitterError(null, ex.message);
                            }
                            this.declFile = this.singleDeclFile;
                        }
                        this.popDeclarationContainer(moduleDecl);
                    }
                }
                return true;
            }
            if(moduleDecl.isEnum()) {
                if(pre) {
                    this.emitEnumSignature(moduleDecl);
                }
                return false;
            }
            if(!this.canEmitPrePostAstSignature(TypeScript.ToDeclFlags(moduleDecl.modFlags), moduleDecl, pre)) {
                return false;
            }
            if(pre) {
                if(this.emitDottedModuleName()) {
                    this.dottedModuleEmit += ".";
                } else {
                    this.dottedModuleEmit = this.getDeclFlagsString(TypeScript.ToDeclFlags(moduleDecl.modFlags), "module");
                }
                this.dottedModuleEmit += moduleDecl.name.text;
                var isCurrentModuleDotted = (moduleDecl.members.members.length == 1 && moduleDecl.members.members[0].nodeType == TypeScript.NodeType.ModuleDeclaration && !(moduleDecl.members.members[0]).isEnum() && TypeScript.hasFlag((moduleDecl.members.members[0]).modFlags, TypeScript.ModuleFlags.Exported));
                var moduleDeclComments = moduleDecl.getDocComments();
                isCurrentModuleDotted = isCurrentModuleDotted && (moduleDeclComments == null || moduleDeclComments.length == 0);
                this.isDottedModuleName.push(isCurrentModuleDotted);
                this.pushDeclarationContainer(moduleDecl);
                if(!isCurrentModuleDotted) {
                    this.emitDeclarationComments(moduleDecl);
                    this.declFile.Write(this.dottedModuleEmit);
                    this.declFile.WriteLine(" {");
                    this.indenter.increaseIndent();
                }
            } else {
                if(!this.emitDottedModuleName()) {
                    this.indenter.decreaseIndent();
                    this.emitIndent();
                    this.declFile.WriteLine("}");
                }
                this.popDeclarationContainer(moduleDecl);
                this.isDottedModuleName.pop();
            }
            return true;
        };
        DeclarationEmitter.prototype.ScriptCallback = function (pre, script) {
            if(pre) {
                if(this.emitOptions.outputMany) {
                    for(var i = 0; i < script.referencedFiles.length; i++) {
                        var referencePath = script.referencedFiles[i].path;
                        var declareFileName;
                        if(TypeScript.isRooted(referencePath)) {
                            declareFileName = this.emitOptions.mapOutputFileName(referencePath, TypeScript.TypeScriptCompiler.mapToDTSFileName);
                        } else {
                            declareFileName = TypeScript.getDeclareFilePath(script.referencedFiles[i].path);
                        }
                        this.declFile.WriteLine('/// <reference path="' + declareFileName + '" />');
                    }
                }
                this.pushDeclarationContainer(script);
            } else {
                this.popDeclarationContainer(script);
            }
            return true;
        };
        DeclarationEmitter.prototype.DefaultCallback = function (pre, ast) {
            return !TypeScript.hasFlag(ast.flags, TypeScript.ASTFlags.IsStatement);
        };
        return DeclarationEmitter;
    })();
    TypeScript.DeclarationEmitter = DeclarationEmitter;    
})(TypeScript || (TypeScript = {}));
