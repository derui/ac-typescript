var TypeScript;
(function (TypeScript) {
    var Continuation = (function () {
        function Continuation(normalBlock) {
            this.normalBlock = normalBlock;
            this.exceptionBlock = -1;
        }
        return Continuation;
    })();
    TypeScript.Continuation = Continuation;    
    function getBaseTypeLinks(bases, baseTypeLinks) {
        if(bases) {
            var len = bases.members.length;
            if(baseTypeLinks == null) {
                baseTypeLinks = new Array();
            }
            for(var i = 0; i < len; i++) {
                var baseExpr = bases.members[i];
                var name = baseExpr;
                var typeLink = new TypeScript.TypeLink();
                typeLink.ast = name;
                baseTypeLinks[baseTypeLinks.length] = typeLink;
            }
        }
        return baseTypeLinks;
    }
    function getBases(type, typeDecl) {
        type.extendsTypeLinks = getBaseTypeLinks(typeDecl.extendsList, type.extendsTypeLinks);
        type.implementsTypeLinks = getBaseTypeLinks(typeDecl.implementsList, type.implementsTypeLinks);
    }
    function addPrototypeField(classType, ast, context) {
        var field = new TypeScript.ValueLocation();
        field.typeLink = new TypeScript.TypeLink();
        field.typeLink.ast = ast;
        field.typeLink.type = classType.instanceType;
        var fieldSymbol = new TypeScript.FieldSymbol("prototype", ast.minChar, context.checker.locationInfo.unitIndex, true, field);
        fieldSymbol.flags |= (TypeScript.SymbolFlags.Property | TypeScript.SymbolFlags.BuiltIn);
        field.symbol = fieldSymbol;
        fieldSymbol.declAST = ast;
        classType.members.addPublicMember("prototype", fieldSymbol);
    }
    function createNewConstructGroupForType(type) {
        var signature = new TypeScript.Signature();
        signature.returnType = new TypeScript.TypeLink();
        signature.returnType.type = type.instanceType;
        signature.parameters = [];
        type.construct = new TypeScript.SignatureGroup();
        type.construct.addSignature(signature);
    }
    TypeScript.createNewConstructGroupForType = createNewConstructGroupForType;
    function cloneParentConstructGroupForChildType(child, parent) {
        child.construct = new TypeScript.SignatureGroup();
        var sig = null;
        if(!parent.construct) {
            createNewConstructGroupForType(parent);
        }
        for(var i = 0; i < parent.construct.signatures.length; i++) {
            sig = new TypeScript.Signature();
            sig.parameters = parent.construct.signatures[i].parameters;
            sig.nonOptionalParameterCount = parent.construct.signatures[i].nonOptionalParameterCount;
            sig.typeCheckStatus = parent.construct.signatures[i].typeCheckStatus;
            sig.declAST = parent.construct.signatures[i].declAST;
            sig.returnType = new TypeScript.TypeLink();
            sig.returnType.type = child.instanceType;
            child.construct.addSignature(sig);
        }
    }
    TypeScript.cloneParentConstructGroupForChildType = cloneParentConstructGroupForChildType;
    TypeScript.globalId = "__GLO";
    function findTypeSymbolInScopeChain(name, scopeChain) {
        var symbol = scopeChain.scope.find(name, false, true);
        if(symbol == null && scopeChain.previous) {
            symbol = findTypeSymbolInScopeChain(name, scopeChain.previous);
        }
        return symbol;
    }
    function findSymbolFromAlias(alias, context) {
        var symbol = null;
        switch(alias.nodeType) {
            case TypeScript.NodeType.Name:
                var name = (alias).text;
                var isDynamic = TypeScript.isQuoted(name);
                var findSym = function (id) {
                    if(context.members) {
                        return context.members.lookup(name);
                    } else {
                        return findTypeSymbolInScopeChain(name, context.topLevelScope);
                    }
                };
                if(isDynamic) {
                    symbol = context.tcContext.checker.findSymbolForDynamicModule(name, context.tcContext.script.locationInfo.filename, findSym);
                } else {
                    symbol = findSym(name);
                }
                break;
            case TypeScript.NodeType.Dot:
                var dottedExpr = alias;
                var op1Sym = findSymbolFromAlias(dottedExpr.operand1, context);
                if(op1Sym && op1Sym.getType()) {
                    symbol = findSymbolFromAlias(dottedExpr.operand2, context);
                }
                break;
            default:
                break;
        }
        if(symbol) {
            var symType = symbol.getType();
            if(symType) {
                var members = symType.members;
                if(members) {
                    context.members = members.publicMembers;
                }
            }
        }
        return symbol;
    }
    function preCollectImportTypes(ast, parent, context) {
        var scopeChain = context.scopeChain;
        var typeSymbol = null;
        var modType = null;
        var importDecl = ast;
        var aliasedModSymbol = findSymbolFromAlias(importDecl.alias, {
            topLevelScope: scopeChain,
            members: null,
            tcContext: context
        });
        var isGlobal = context.scopeChain.container == context.checker.gloMod;
        if(aliasedModSymbol) {
            var aliasedModType = aliasedModSymbol.getType();
            if(aliasedModType) {
                modType = aliasedModType;
            }
        }
        typeSymbol = new TypeScript.TypeSymbol(importDecl.id.text, importDecl.id.minChar, importDecl.limChar - importDecl.minChar, context.checker.locationInfo.unitIndex, modType);
        typeSymbol.aliasLink = importDecl;
        if(context.scopeChain.moduleDecl) {
            typeSymbol.flags |= TypeScript.SymbolFlags.ModuleMember;
            typeSymbol.declModule = context.scopeChain.moduleDecl;
        }
        typeSymbol.declAST = importDecl;
        importDecl.id.sym = typeSymbol;
        scopeChain.scope.enter(scopeChain.container, ast, typeSymbol, context.checker.errorReporter, isGlobal, true, false);
        scopeChain.scope.enter(scopeChain.container, ast, typeSymbol, context.checker.errorReporter, isGlobal, false, false);
        return true;
    }
    TypeScript.preCollectImportTypes = preCollectImportTypes;
    function preCollectModuleTypes(ast, parent, context) {
        var scopeChain = context.scopeChain;
        var moduleDecl = ast;
        var isAmbient = TypeScript.hasFlag(moduleDecl.modFlags, TypeScript.ModuleFlags.Ambient);
        var isEnum = TypeScript.hasFlag(moduleDecl.modFlags, TypeScript.ModuleFlags.IsEnum);
        var isGlobal = context.scopeChain.container == context.checker.gloMod;
        var isExported = TypeScript.hasFlag(moduleDecl.modFlags, TypeScript.ModuleFlags.Exported);
        var modName = (moduleDecl.name).text;
        var isDynamic = TypeScript.isQuoted(modName);
        var symbol = scopeChain.scope.findLocal(modName, false, false);
        var typeSymbol = null;
        var modType = null;
        if(symbol && symbol.declAST && symbol.declAST.nodeType != TypeScript.NodeType.ModuleDeclaration) {
            context.checker.errorReporter.simpleError(moduleDecl, "Conflicting symbol name for module '" + modName + "'");
            symbol = null;
            modName = "";
        }
        if(symbol) {
            var modDeclAST = symbol.declAST;
            var modDeclASTIsExported = TypeScript.hasFlag(modDeclAST.modFlags, TypeScript.ModuleFlags.Exported);
            if((modDeclASTIsExported && !isExported) || (!modDeclASTIsExported && isExported)) {
                context.checker.errorReporter.simpleError(moduleDecl, 'All contributions to a module must be "export" or none');
            }
        }
        if((symbol == null) || (symbol.kind() != TypeScript.SymbolKind.Type)) {
            if(modType == null) {
                var enclosedTypes = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
                var ambientEnclosedTypes = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
                modType = new TypeScript.ModuleType(enclosedTypes, ambientEnclosedTypes);
                if(isEnum) {
                    modType.typeFlags |= TypeScript.TypeFlags.IsEnum;
                }
                modType.members = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
                modType.ambientMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
                modType.setHasImplementation();
            }
            typeSymbol = new TypeScript.TypeSymbol(modName, moduleDecl.name.minChar, modName.length, context.checker.locationInfo.unitIndex, modType);
            typeSymbol.isDynamic = TypeScript.isQuoted(moduleDecl.prettyName);
            if(context.scopeChain.moduleDecl) {
                typeSymbol.declModule = context.scopeChain.moduleDecl;
            }
            typeSymbol.declAST = moduleDecl;
            typeSymbol.prettyName = moduleDecl.prettyName;
            scopeChain.scope.enter(scopeChain.container, ast, typeSymbol, context.checker.errorReporter, isExported || isGlobal, true, isAmbient);
            scopeChain.scope.enter(scopeChain.container, ast, typeSymbol, context.checker.errorReporter, isExported || isGlobal, false, isAmbient);
            modType.symbol = typeSymbol;
        } else {
            typeSymbol = symbol;
            var publicEnclosedTypes = typeSymbol.type.getAllEnclosedTypes().publicMembers;
            var publicEnclosedTypesTable = (publicEnclosedTypes == null) ? new TypeScript.StringHashTable() : publicEnclosedTypes;
            var enclosedTypes = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(publicEnclosedTypesTable, new TypeScript.StringHashTable()));
            var publicEnclosedAmbientTypes = typeSymbol.type.getAllAmbientEnclosedTypes().publicMembers;
            var publicAmbientEnclosedTypesTable = (publicEnclosedAmbientTypes == null) ? new TypeScript.StringHashTable() : publicEnclosedAmbientTypes;
            var ambientEnclosedTypes = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(publicAmbientEnclosedTypesTable, new TypeScript.StringHashTable()));
            var publicMembers = typeSymbol.type.members.publicMembers;
            var publicMembersTable = (publicMembers == null) ? new TypeScript.StringHashTable() : publicMembers;
            var members = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(publicMembersTable, new TypeScript.StringHashTable()));
            var publicAmbientMembers = typeSymbol.type.ambientMembers.publicMembers;
            var publicAmbientMembersTable = (publicAmbientMembers == null) ? new TypeScript.StringHashTable() : publicAmbientMembers;
            var ambientMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(publicAmbientMembersTable, new TypeScript.StringHashTable()));
            modType = new TypeScript.ModuleType(enclosedTypes, ambientEnclosedTypes);
            if(isEnum) {
                modType.typeFlags |= TypeScript.TypeFlags.IsEnum;
            }
            modType.members = members;
            modType.ambientMembers = ambientMembers;
            modType.setHasImplementation();
            modType.symbol = typeSymbol;
            typeSymbol.addLocation(moduleDecl.minChar);
            typeSymbol.expansions.push(modType);
            typeSymbol.expansionsDeclAST.push(moduleDecl);
        }
        if(context.scopeChain.moduleDecl) {
            context.scopeChain.moduleDecl.recordNonInterface();
        }
        if(isExported) {
            typeSymbol.flags |= TypeScript.SymbolFlags.Exported;
        }
        if((context.scopeChain.moduleDecl) || (context.scopeChain.container == context.checker.gloMod)) {
            typeSymbol.flags |= TypeScript.SymbolFlags.ModuleMember;
        }
        moduleDecl.mod = modType;
        TypeScript.pushTypeCollectionScope(typeSymbol, modType.members, modType.ambientMembers, modType.enclosedTypes, modType.ambientEnclosedTypes, context, null, null, moduleDecl);
        return true;
    }
    TypeScript.preCollectModuleTypes = preCollectModuleTypes;
    function preCollectClassTypes(ast, parent, context) {
        var scopeChain = context.scopeChain;
        var classDecl = ast;
        var classType;
        var instanceType;
        var typeSymbol = null;
        var className = (classDecl.name).text;
        var alreadyInScope = false;
        var isAmbient = TypeScript.hasFlag(classDecl.varFlags, TypeScript.VarFlags.Ambient);
        var isExported = TypeScript.hasFlag(classDecl.varFlags, TypeScript.VarFlags.Exported);
        var isGlobal = context.scopeChain.container == context.checker.gloMod;
        var containerMod = scopeChain.container;
        var foundValSymbol = false;
        typeSymbol = scopeChain.scope.findLocal(className, false, true);
        if(!typeSymbol) {
            var valTypeSymbol = scopeChain.scope.findLocal(className, false, false);
            if(valTypeSymbol && valTypeSymbol.isType() && valTypeSymbol.declAST && valTypeSymbol.declAST.nodeType == TypeScript.NodeType.FuncDecl && (valTypeSymbol.declAST).isSignature()) {
                typeSymbol = valTypeSymbol;
                foundValSymbol = true;
                if(isExported) {
                    typeSymbol.flags |= TypeScript.SymbolFlags.Exported;
                }
                if(isAmbient) {
                    typeSymbol.flags |= TypeScript.SymbolFlags.Ambient;
                }
                context.scopeChain.scope.enter(context.scopeChain.container, ast, typeSymbol, context.checker.errorReporter, isExported || isGlobal, true, isAmbient);
            }
        }
        if(typeSymbol && !foundValSymbol && (typeSymbol.declAST != classDecl)) {
            typeSymbol = null;
        }
        if(typeSymbol == null) {
            var valueSymbol = scopeChain.scope.findLocal(className, false, false);
            classType = new TypeScript.Type();
            classType.setHasImplementation();
            instanceType = new TypeScript.Type();
            instanceType.setHasImplementation();
            classType.instanceType = instanceType;
            classType.members = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
            classType.ambientMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
            addPrototypeField(classType, classDecl, context);
            instanceType.members = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
            instanceType.ambientMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
            typeSymbol = new TypeScript.TypeSymbol(className, classDecl.name.minChar, className.length, context.checker.locationInfo.unitIndex, classType);
            typeSymbol.declAST = classDecl;
            typeSymbol.instanceType = instanceType;
            classType.symbol = typeSymbol;
            instanceType.symbol = typeSymbol;
            if(context.scopeChain.moduleDecl) {
                context.scopeChain.moduleDecl.recordNonInterface();
                typeSymbol.declModule = context.scopeChain.moduleDecl;
                typeSymbol.flags |= TypeScript.SymbolFlags.ModuleMember;
            }
            if(isExported) {
                typeSymbol.flags |= TypeScript.SymbolFlags.Exported;
            }
            if(isAmbient) {
                typeSymbol.flags |= TypeScript.SymbolFlags.Ambient;
            }
            ast.type = classType;
            context.scopeChain.scope.enter(context.scopeChain.container, ast, typeSymbol, context.checker.errorReporter, isExported || isGlobal, true, isAmbient);
            if(valueSymbol == null) {
                context.scopeChain.scope.enter(context.scopeChain.container, ast, typeSymbol, context.checker.errorReporter, isExported || isGlobal, false, isAmbient);
            }
        } else {
            classType = typeSymbol.type;
            if(classType.instanceType == null) {
                classType.instanceType = new TypeScript.Type();
                classType.instanceType.setHasImplementation();
                classType.instanceType.members = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
                classType.instanceType.symbol = classType.symbol;
                classType.members = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
                classType.ambientMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
            }
            instanceType = classType.instanceType;
            ast.type = classType;
        }
        if(!classDecl.constructorDecl) {
            if(typeSymbol && typeSymbol.declAST && typeSymbol.declAST.type && typeSymbol.declAST.type.call && !(typeSymbol.declAST).isOverload) {
                context.checker.errorReporter.duplicateIdentifier(typeSymbol.declAST, typeSymbol.name);
            }
            createNewConstructGroupForType(classDecl.type);
        }
        classType.typeFlags |= TypeScript.TypeFlags.IsClass;
        instanceType.typeFlags |= TypeScript.TypeFlags.IsClass;
        getBases(instanceType, classDecl);
        TypeScript.pushTypeCollectionScope(typeSymbol, instanceType.members, instanceType.ambientMembers, null, null, context, instanceType, classType, null);
        return true;
    }
    TypeScript.preCollectClassTypes = preCollectClassTypes;
    function preCollectInterfaceTypes(ast, parent, context) {
        var scopeChain = context.scopeChain;
        var interfaceDecl = ast;
        var interfaceSymbol = null;
        var interfaceType = null;
        var isExported = TypeScript.hasFlag(interfaceDecl.varFlags, TypeScript.VarFlags.Exported);
        var isGlobal = context.scopeChain.container == context.checker.gloMod;
        var alreadyInScope = true;
        alreadyInScope = false;
        var interfaceName = (interfaceDecl.name).text;
        interfaceSymbol = scopeChain.scope.findLocal(interfaceName, false, true);
        if(interfaceSymbol == null) {
            interfaceType = new TypeScript.Type();
            interfaceSymbol = new TypeScript.TypeSymbol(interfaceName, interfaceDecl.name.minChar, interfaceName.length, context.checker.locationInfo.unitIndex, interfaceType);
            interfaceType.symbol = interfaceSymbol;
            interfaceType.members = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
            interfaceType.ambientMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
            interfaceSymbol.declAST = interfaceDecl;
            interfaceSymbol.declModule = context.scopeChain.moduleDecl;
        } else {
            alreadyInScope = true;
            interfaceType = interfaceSymbol.type;
        }
        if(!interfaceType) {
            interfaceType = context.checker.anyType;
        }
        ast.type = interfaceType;
        getBases(interfaceType, interfaceDecl);
        if(isExported) {
            interfaceSymbol.flags |= TypeScript.SymbolFlags.Exported;
        }
        if(context.scopeChain.moduleDecl) {
            interfaceSymbol.flags |= TypeScript.SymbolFlags.ModuleMember;
        }
        if(!alreadyInScope) {
            context.scopeChain.scope.enter(context.scopeChain.container, ast, interfaceSymbol, context.checker.errorReporter, isGlobal || isExported, true, false);
        }
        TypeScript.pushTypeCollectionScope(interfaceSymbol, interfaceType.members, interfaceType.ambientMembers, null, null, context, interfaceType, null, null);
        return true;
    }
    TypeScript.preCollectInterfaceTypes = preCollectInterfaceTypes;
    function preCollectArgDeclTypes(ast, parent, context) {
        var scopeChain = context.scopeChain;
        var argDecl = ast;
        if(TypeScript.hasFlag(argDecl.varFlags, TypeScript.VarFlags.Public | TypeScript.VarFlags.Private)) {
            var field = new TypeScript.ValueLocation();
            var isPrivate = TypeScript.hasFlag(argDecl.varFlags, TypeScript.VarFlags.Private);
            var fieldSymbol = new TypeScript.FieldSymbol(argDecl.id.text, argDecl.id.minChar, context.checker.locationInfo.unitIndex, !TypeScript.hasFlag(argDecl.varFlags, TypeScript.VarFlags.Readonly), field);
            fieldSymbol.transferVarFlags(argDecl.varFlags);
            field.symbol = fieldSymbol;
            fieldSymbol.declAST = ast;
            argDecl.parameterPropertySym = fieldSymbol;
            context.scopeChain.scope.enter(context.scopeChain.container, ast, fieldSymbol, context.checker.errorReporter, !isPrivate, false, false);
            field.typeLink = TypeScript.getTypeLink(argDecl.typeExpr, context.checker, argDecl.init == null);
            argDecl.sym = fieldSymbol;
        }
        return false;
    }
    TypeScript.preCollectArgDeclTypes = preCollectArgDeclTypes;
    function preCollectVarDeclTypes(ast, parent, context) {
        var scopeChain = context.scopeChain;
        var varDecl = ast;
        var isAmbient = TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Ambient);
        var isExported = TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Exported);
        var isGlobal = context.scopeChain.container == context.checker.gloMod;
        var isProperty = TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Property);
        var isStatic = TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Static);
        var isPrivate = TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Private);
        var isOptional = TypeScript.hasFlag(varDecl.id.flags, TypeScript.ASTFlags.OptionalName);
        if(context.scopeChain.moduleDecl) {
            context.scopeChain.moduleDecl.recordNonInterface();
        }
        if(isProperty || isExported || (context.scopeChain.container == context.checker.gloMod) || context.scopeChain.moduleDecl) {
            if(isAmbient) {
                var existingSym = scopeChain.scope.findLocal(varDecl.id.text, false, false);
                if(existingSym) {
                    varDecl.sym = existingSym;
                    return false;
                }
            }
            if(varDecl.id == null) {
                context.checker.errorReporter.simpleError(varDecl, "Expected variable identifier at this location");
                return false;
            }
            var field = new TypeScript.ValueLocation();
            var fieldSymbol = new TypeScript.FieldSymbol(varDecl.id.text, varDecl.id.minChar, context.checker.locationInfo.unitIndex, (varDecl.varFlags & TypeScript.VarFlags.Readonly) == TypeScript.VarFlags.None, field);
            fieldSymbol.transferVarFlags(varDecl.varFlags);
            if(isOptional) {
                fieldSymbol.flags |= TypeScript.SymbolFlags.Optional;
            }
            field.symbol = fieldSymbol;
            fieldSymbol.declAST = ast;
            if((context.scopeChain.moduleDecl) || (context.scopeChain.container == context.checker.gloMod)) {
                fieldSymbol.flags |= TypeScript.SymbolFlags.ModuleMember;
                fieldSymbol.declModule = context.scopeChain.moduleDecl;
            }
            if(TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Property) && isStatic && context.scopeChain.classType) {
                if(!context.scopeChain.classType.members.publicMembers.add(varDecl.id.text, fieldSymbol)) {
                    context.checker.errorReporter.duplicateIdentifier(ast, fieldSymbol.name);
                }
                fieldSymbol.container = context.scopeChain.classType.symbol;
            } else {
                context.scopeChain.scope.enter(context.scopeChain.container, ast, fieldSymbol, context.checker.errorReporter, !isPrivate && (isProperty || isExported || isGlobal || isStatic), false, isAmbient);
            }
            if(TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Exported)) {
                fieldSymbol.flags |= TypeScript.SymbolFlags.Exported;
            }
            field.typeLink = TypeScript.getTypeLink(varDecl.typeExpr, context.checker, varDecl.init == null);
            varDecl.sym = fieldSymbol;
        }
        return false;
    }
    TypeScript.preCollectVarDeclTypes = preCollectVarDeclTypes;
    function preCollectFuncDeclTypes(ast, parent, context) {
        var scopeChain = context.scopeChain;
        if(context.scopeChain.moduleDecl) {
            context.scopeChain.moduleDecl.recordNonInterface();
        }
        var funcDecl = ast;
        var fgSym = null;
        var nameText = funcDecl.getNameText();
        var isExported = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Exported | TypeScript.FncFlags.ClassPropertyMethodExported);
        var isStatic = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Static);
        var isPrivate = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Private);
        var isConstructor = funcDecl.isConstructMember() || funcDecl.isConstructor;
        var containerSym = (((funcDecl.isMethod() && isStatic) || funcDecl.isAccessor()) && context.scopeChain.classType ? context.scopeChain.classType.symbol : context.scopeChain.container);
        var containerScope = context.scopeChain.scope;
        var isGlobal = containerSym == context.checker.gloMod;
        var isOptional = funcDecl.name && TypeScript.hasFlag(funcDecl.name.flags, TypeScript.ASTFlags.OptionalName);
        var go = false;
        var foundSymbol = false;
        if(isConstructor && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod)) {
            containerSym = containerSym.container;
            containerScope = scopeChain.previous.scope;
        }
        funcDecl.unitIndex = context.checker.locationInfo.unitIndex;
        if(!funcDecl.isConstructor && containerSym && containerSym.declAST && containerSym.declAST.nodeType == TypeScript.NodeType.FuncDecl && (containerSym.declAST).isConstructor && !funcDecl.isMethod()) {
            return go;
        }
        if(TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Signature)) {
            var instType = context.scopeChain.thisType;
            if(nameText && nameText != "__missing") {
                if(isStatic) {
                    fgSym = containerSym.type.members.allMembers.lookup(nameText);
                } else {
                    fgSym = containerScope.findLocal(nameText, false, false);
                    if(fgSym == null) {
                        fgSym = containerScope.findLocal(nameText, false, true);
                    }
                }
                if(fgSym) {
                    foundSymbol = true;
                    if(!funcDecl.isSignature() && (TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Ambient) != TypeScript.hasFlag(fgSym.flags, TypeScript.SymbolFlags.Ambient))) {
                        fgSym = null;
                    }
                }
            }
            if(fgSym == null) {
                if(!(funcDecl.isSpecialFn())) {
                    fgSym = context.checker.createFunctionSignature(funcDecl, containerSym, containerScope, null, !foundSymbol).declAST.type.symbol;
                } else {
                    fgSym = context.checker.createFunctionSignature(funcDecl, containerSym, containerScope, containerSym, false).declAST.type.symbol;
                }
                if(fgSym.declAST == null || !funcDecl.isSpecialFn()) {
                    fgSym.declAST = ast;
                }
            } else {
                if((fgSym.kind() == TypeScript.SymbolKind.Type)) {
                    fgSym = context.checker.createFunctionSignature(funcDecl, containerSym, containerScope, fgSym, false).declAST.type.symbol;
                } else {
                    context.checker.errorReporter.simpleError(funcDecl, "Function or method '" + funcDecl.name.actualText + "' already declared as a property");
                }
            }
            if(funcDecl.isSpecialFn() && !isStatic) {
                funcDecl.type = instType ? instType : fgSym.type;
            } else {
                funcDecl.type = fgSym.type;
            }
        } else {
            if(nameText) {
                if(isStatic) {
                    fgSym = containerSym.type.members.allMembers.lookup(nameText);
                } else {
                    if(funcDecl.isConstructor && context.scopeChain.previous) {
                        fgSym = context.scopeChain.previous.scope.findLocal(nameText, false, false);
                    }
                    if(fgSym == null) {
                        fgSym = containerScope.findLocal(nameText, false, false);
                    }
                }
                if(fgSym) {
                    foundSymbol = true;
                    if(!isConstructor && fgSym.declAST.nodeType == TypeScript.NodeType.FuncDecl && !(fgSym.declAST).isAccessor() && !(fgSym.declAST).isSignature()) {
                        fgSym = null;
                        foundSymbol = false;
                    }
                }
            }
            if(fgSym && !fgSym.isAccessor() && fgSym.type && fgSym.type.construct && fgSym.type.construct.signatures != [] && (fgSym.type.construct.signatures[0].declAST == null || !TypeScript.hasFlag(fgSym.type.construct.signatures[0].declAST.fncFlags, TypeScript.FncFlags.Ambient)) && !funcDecl.isConstructor) {
                context.checker.errorReporter.simpleError(funcDecl, "Functions may not have class overloads");
            }
            if(fgSym && !(fgSym.kind() == TypeScript.SymbolKind.Type) && funcDecl.isMethod() && !funcDecl.isAccessor() && !funcDecl.isConstructor) {
                context.checker.errorReporter.simpleError(funcDecl, "Function or method '" + funcDecl.name.actualText + "' already declared as a property");
                fgSym.type = context.checker.anyType;
            }
            if(fgSym && !fgSym.isAccessor() && funcDecl.isAccessor()) {
                fgSym = null;
            }
            var sig = context.checker.createFunctionSignature(funcDecl, containerSym, containerScope, fgSym, !foundSymbol);
            if(((!fgSym || fgSym.declAST.nodeType != TypeScript.NodeType.FuncDecl) && funcDecl.isAccessor()) || (fgSym && fgSym.isAccessor())) {
                funcDecl.accessorSymbol = context.checker.createAccessorSymbol(funcDecl, fgSym, containerSym.type, (funcDecl.isMethod() && isStatic), true, containerScope, containerSym);
            }
            funcDecl.type.symbol.declAST = ast;
            if(funcDecl.isConstructor) {
                go = true;
            }
            ;
        }
        if(isExported) {
            if(funcDecl.type.call) {
                funcDecl.type.symbol.flags |= TypeScript.SymbolFlags.Exported;
            }
            if(fgSym && !fgSym.isAccessor() && fgSym.kind() == TypeScript.SymbolKind.Type && fgSym.type.call) {
                fgSym.flags |= TypeScript.SymbolFlags.Exported;
            }
        }
        if(context.scopeChain.moduleDecl && !funcDecl.isSpecialFn()) {
            funcDecl.type.symbol.flags |= TypeScript.SymbolFlags.ModuleMember;
            funcDecl.type.symbol.declModule = context.scopeChain.moduleDecl;
        }
        if(fgSym && isOptional) {
            fgSym.flags |= TypeScript.SymbolFlags.Optional;
        }
        return go;
    }
    TypeScript.preCollectFuncDeclTypes = preCollectFuncDeclTypes;
    function preCollectTypes(ast, parent, walker) {
        var context = walker.state;
        var go = false;
        var scopeChain = context.scopeChain;
        if(ast.nodeType == TypeScript.NodeType.Script) {
            var script = ast;
            context.script = script;
            go = true;
        } else if(ast.nodeType == TypeScript.NodeType.List) {
            go = true;
        } else if(ast.nodeType == TypeScript.NodeType.ImportDeclaration) {
            go = preCollectImportTypes(ast, parent, context);
        } else if(ast.nodeType == TypeScript.NodeType.With) {
            go = false;
        } else if(ast.nodeType == TypeScript.NodeType.ModuleDeclaration) {
            go = preCollectModuleTypes(ast, parent, context);
        } else if(ast.nodeType == TypeScript.NodeType.ClassDeclaration) {
            go = preCollectClassTypes(ast, parent, context);
        } else if(ast.nodeType == TypeScript.NodeType.Block) {
            go = true;
        } else if(ast.nodeType == TypeScript.NodeType.InterfaceDeclaration) {
            go = preCollectInterfaceTypes(ast, parent, context);
        } else if(ast.nodeType == TypeScript.NodeType.ArgDecl) {
            go = preCollectArgDeclTypes(ast, parent, context);
        } else if(ast.nodeType == TypeScript.NodeType.VarDecl) {
            go = preCollectVarDeclTypes(ast, parent, context);
        } else if(ast.nodeType == TypeScript.NodeType.FuncDecl) {
            go = preCollectFuncDeclTypes(ast, parent, context);
        } else {
            if(ast.isStatementOrExpression() && context.scopeChain.moduleDecl) {
                context.scopeChain.moduleDecl.recordNonInterface();
            }
        }
        walker.options.goChildren = go;
        return ast;
    }
    TypeScript.preCollectTypes = preCollectTypes;
    function postCollectTypes(ast, parent, walker) {
        var context = walker.state;
        if(ast.nodeType == TypeScript.NodeType.ModuleDeclaration) {
            TypeScript.popTypeCollectionScope(context);
        } else if(ast.nodeType == TypeScript.NodeType.ClassDeclaration) {
            TypeScript.popTypeCollectionScope(context);
        } else if(ast.nodeType == TypeScript.NodeType.InterfaceDeclaration) {
            TypeScript.popTypeCollectionScope(context);
        }
        return ast;
    }
    TypeScript.postCollectTypes = postCollectTypes;
})(TypeScript || (TypeScript = {}));
