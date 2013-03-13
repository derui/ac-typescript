var TypeScript;
(function (TypeScript) {
    var ArrayCache = (function () {
        function ArrayCache() {
            this.arrayBase = null;
        }
        ArrayCache.prototype.specialize = function (arrInstType, checker) {
            if(this.arrayBase == null) {
                this.arrayBase = arrInstType.specializeType(checker.wildElm.type, this.arrayType.elementType, checker, true);
            }
            return this.arrayBase;
        };
        return ArrayCache;
    })();
    TypeScript.ArrayCache = ArrayCache;    
    var TypeComparisonInfo = (function () {
        function TypeComparisonInfo() {
            this.onlyCaptureFirstError = false;
            this.flags = TypeScript.TypeRelationshipFlags.SuccessfulComparison;
            this.message = "";
        }
        TypeComparisonInfo.prototype.addMessageToFront = function (message) {
            if(!this.onlyCaptureFirstError) {
                this.message = this.message ? message + ":\n\t" + this.message : message;
            } else {
                this.setMessage(message);
            }
        };
        TypeComparisonInfo.prototype.setMessage = function (message) {
            this.message = message;
        };
        return TypeComparisonInfo;
    })();
    TypeScript.TypeComparisonInfo = TypeComparisonInfo;    
    (function (TypeCheckCollectionMode) {
        TypeCheckCollectionMode._map = [];
        TypeCheckCollectionMode._map[0] = "Resident";
        TypeCheckCollectionMode.Resident = 0;
        TypeCheckCollectionMode._map[1] = "Transient";
        TypeCheckCollectionMode.Transient = 1;
    })(TypeScript.TypeCheckCollectionMode || (TypeScript.TypeCheckCollectionMode = {}));
    var TypeCheckCollectionMode = TypeScript.TypeCheckCollectionMode;
    var PersistentGlobalTypeState = (function () {
        function PersistentGlobalTypeState(errorReporter) {
            this.errorReporter = errorReporter;
            this.importedGlobalsTable = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
            this.importedGlobalsTypeTable = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
            this.globals = null;
            this.globalTypes = null;
            this.ambientGlobals = null;
            this.ambientGlobalTypes = null;
            this.residentGlobalValues = new TypeScript.StringHashTable();
            this.residentGlobalTypes = new TypeScript.StringHashTable();
            this.residentGlobalAmbientValues = new TypeScript.StringHashTable();
            this.residentGlobalAmbientTypes = new TypeScript.StringHashTable();
            this.residentTypeCheck = true;
            this.mod = null;
            this.gloMod = null;
            this.wildElm = null;
            this.importedGlobals = new TypeScript.SymbolScopeBuilder(null, this.importedGlobalsTable, null, this.importedGlobalsTypeTable, null, null);
            this.dualGlobalValues = new TypeScript.DualStringHashTable(this.residentGlobalValues, new TypeScript.StringHashTable());
            this.dualGlobalTypes = new TypeScript.DualStringHashTable(this.residentGlobalTypes, new TypeScript.StringHashTable());
            this.dualAmbientGlobalValues = new TypeScript.DualStringHashTable(this.residentGlobalAmbientValues, new TypeScript.StringHashTable());
            this.dualAmbientGlobalTypes = new TypeScript.DualStringHashTable(this.residentGlobalAmbientTypes, new TypeScript.StringHashTable());
            var dualGlobalScopedMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(this.dualGlobalValues, new TypeScript.StringHashTable()));
            var dualGlobalScopedAmbientMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(this.dualAmbientGlobalValues, new TypeScript.StringHashTable()));
            var dualGlobalScopedEnclosedTypes = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(this.dualGlobalTypes, new TypeScript.StringHashTable()));
            var dualGlobalScopedAmbientEnclosedTypes = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(this.dualAmbientGlobalTypes, new TypeScript.StringHashTable()));
            this.globalScope = new TypeScript.SymbolScopeBuilder(dualGlobalScopedMembers, dualGlobalScopedAmbientMembers, dualGlobalScopedEnclosedTypes, dualGlobalScopedAmbientEnclosedTypes, this.importedGlobals, null);
            this.voidType = this.enterPrimitive(TypeScript.Primitive.Void, "void");
            this.booleanType = this.enterPrimitive(TypeScript.Primitive.Boolean, "bool");
            this.doubleType = this.enterPrimitive(TypeScript.Primitive.Double, "number");
            this.importedGlobals.ambientEnclosedTypes.addPublicMember("number", this.doubleType.symbol);
            this.stringType = this.enterPrimitive(TypeScript.Primitive.String, "string");
            this.anyType = this.enterPrimitive(TypeScript.Primitive.Any, "any");
            this.nullType = this.enterPrimitive(TypeScript.Primitive.Null, "null");
            this.undefinedType = this.enterPrimitive(TypeScript.Primitive.Undefined, "undefined");
            this.setCollectionMode(TypeCheckCollectionMode.Resident);
            this.wildElm = new TypeScript.TypeSymbol("_element", -1, 0, -1, new TypeScript.Type());
            this.importedGlobalsTypeTable.addPublicMember(this.wildElm.name, this.wildElm);
            this.mod = new TypeScript.ModuleType(dualGlobalScopedEnclosedTypes, dualGlobalScopedAmbientEnclosedTypes);
            this.mod.members = dualGlobalScopedMembers;
            this.mod.ambientMembers = dualGlobalScopedAmbientMembers;
            this.mod.containedScope = this.globalScope;
            this.gloMod = new TypeScript.TypeSymbol(TypeScript.globalId, -1, 0, -1, this.mod);
            this.mod.members.addPublicMember(this.gloMod.name, this.gloMod);
            this.defineGlobalValue("undefined", this.undefinedType);
        }
        PersistentGlobalTypeState.prototype.enterPrimitive = function (flags, name) {
            var primitive = new TypeScript.Type();
            primitive.primitiveTypeClass = flags;
            var symbol = new TypeScript.TypeSymbol(name, -1, name.length, -1, primitive);
            symbol.typeCheckStatus = TypeScript.TypeCheckStatus.Finished;
            primitive.symbol = symbol;
            this.importedGlobals.enter(null, null, symbol, this.errorReporter, true, true, true);
            return primitive;
        };
        PersistentGlobalTypeState.prototype.setCollectionMode = function (mode) {
            this.residentTypeCheck = this.dualGlobalValues.insertPrimary = this.dualGlobalTypes.insertPrimary = this.dualAmbientGlobalValues.insertPrimary = this.dualAmbientGlobalTypes.insertPrimary = mode == TypeCheckCollectionMode.Resident;
        };
        PersistentGlobalTypeState.prototype.refreshPersistentState = function () {
            this.globals = new TypeScript.StringHashTable();
            this.globalTypes = new TypeScript.StringHashTable();
            this.ambientGlobals = new TypeScript.StringHashTable();
            this.ambientGlobalTypes = new TypeScript.StringHashTable();
            this.globalTypes.add(this.voidType.symbol.name, this.voidType.symbol);
            this.globalTypes.add(this.booleanType.symbol.name, this.booleanType.symbol);
            this.globalTypes.add(this.doubleType.symbol.name, this.doubleType.symbol);
            this.globalTypes.add("number", this.doubleType.symbol);
            this.globalTypes.add(this.stringType.symbol.name, this.stringType.symbol);
            this.globalTypes.add(this.anyType.symbol.name, this.anyType.symbol);
            this.globalTypes.add(this.nullType.symbol.name, this.nullType.symbol);
            this.globalTypes.add(this.undefinedType.symbol.name, this.undefinedType.symbol);
            this.dualGlobalValues.secondaryTable = this.globals;
            this.dualGlobalTypes.secondaryTable = this.globalTypes;
            this.dualAmbientGlobalValues.secondaryTable = this.ambientGlobals;
            this.dualAmbientGlobalTypes.secondaryTable = this.ambientGlobalTypes;
        };
        PersistentGlobalTypeState.prototype.defineGlobalValue = function (name, type) {
            var valueLocation = new TypeScript.ValueLocation();
            valueLocation.typeLink = new TypeScript.TypeLink();
            var sym = new TypeScript.VariableSymbol(name, 0, -1, valueLocation);
            sym.setType(type);
            sym.typeCheckStatus = TypeScript.TypeCheckStatus.Finished;
            sym.container = this.gloMod;
            this.importedGlobalsTable.addPublicMember(name, sym);
        };
        return PersistentGlobalTypeState;
    })();
    TypeScript.PersistentGlobalTypeState = PersistentGlobalTypeState;    
    var ContextualTypeContext = (function () {
        function ContextualTypeContext(contextualType, provisional, contextID) {
            this.contextualType = contextualType;
            this.provisional = provisional;
            this.contextID = contextID;
            this.targetSig = null;
            this.targetThis = null;
            this.targetAccessorType = null;
        }
        return ContextualTypeContext;
    })();
    TypeScript.ContextualTypeContext = ContextualTypeContext;    
    var ContextualTypingContextStack = (function () {
        function ContextualTypingContextStack(checker) {
            this.checker = checker;
            this.contextStack = [];
            this.hadProvisionalErrors = false;
        }
        ContextualTypingContextStack.contextID = TypeScript.TypeCheckStatus.Finished + 1;
        ContextualTypingContextStack.prototype.pushContextualType = function (type, provisional) {
            this.contextStack.push(new ContextualTypeContext(type, provisional, ContextualTypingContextStack.contextID++));
            this.checker.errorReporter.pushToErrorSink = provisional;
        };
        ContextualTypingContextStack.prototype.popContextualType = function () {
            var tc = this.contextStack.pop();
            this.checker.errorReporter.pushToErrorSink = this.isProvisional();
            this.hadProvisionalErrors = this.hadProvisionalErrors || (tc.provisional && (this.checker.errorReporter.getCapturedErrors().length));
            this.checker.errorReporter.freeCapturedErrors();
            return tc;
        };
        ContextualTypingContextStack.prototype.getContextualType = function () {
            return (!this.contextStack.length ? null : this.contextStack[this.contextStack.length - 1]);
        };
        ContextualTypingContextStack.prototype.getContextID = function () {
            return (!this.contextStack.length ? TypeScript.TypeCheckStatus.Finished : this.contextStack[this.contextStack.length - 1].contextID);
        };
        ContextualTypingContextStack.prototype.isProvisional = function () {
            return (!this.contextStack.length ? false : this.contextStack[this.contextStack.length - 1].provisional);
        };
        return ContextualTypingContextStack;
    })();
    TypeScript.ContextualTypingContextStack = ContextualTypingContextStack;    
    var TypeChecker = (function () {
        function TypeChecker(persistentState) {
            this.persistentState = persistentState;
            this.errorReporter = null;
            this.checkControlFlow = false;
            this.printControlFlowGraph = false;
            this.checkControlFlowUseDef = false;
            this.styleSettings = null;
            this.units = null;
            this.anon = "_anonymous";
            this.locationInfo = null;
            this.typeFlow = null;
            this.currentCompareA = null;
            this.currentCompareB = null;
            this.currentModDecl = null;
            this.inBind = false;
            this.inWith = false;
            this.errorsOnWith = true;
            this.currentContextualTypeContext = null;
            this.resolvingBases = false;
            this.canCallDefinitionSignature = false;
            this.assignableCache = {
            };
            this.subtypeCache = {
            };
            this.identicalCache = {
            };
            this.provisionalStartedTypecheckObjects = [];
            this.mustCaptureGlobalThis = false;
            this.voidType = this.persistentState.voidType;
            this.booleanType = this.persistentState.booleanType;
            this.numberType = this.persistentState.doubleType;
            this.stringType = this.persistentState.stringType;
            this.anyType = this.persistentState.anyType;
            this.nullType = this.persistentState.nullType;
            this.undefinedType = this.persistentState.undefinedType;
            this.globals = this.persistentState.dualGlobalValues;
            this.globalTypes = this.persistentState.dualGlobalTypes;
            this.ambientGlobals = this.persistentState.dualAmbientGlobalValues;
            this.ambientGlobalTypes = this.persistentState.dualAmbientGlobalTypes;
            this.gloModType = this.persistentState.mod;
            this.gloMod = this.persistentState.gloMod;
            this.wildElm = this.persistentState.wildElm;
            this.globalScope = this.persistentState.globalScope;
            this.typingContextStack = new ContextualTypingContextStack(this);
        }
        TypeChecker.prototype.setStyleOptions = function (style) {
            this.styleSettings = style;
        };
        TypeChecker.prototype.setContextualType = function (type, provisional) {
            this.typingContextStack.pushContextualType(type, provisional);
            this.currentContextualTypeContext = this.typingContextStack.getContextualType();
        };
        TypeChecker.prototype.unsetContextualType = function () {
            var lastTC = this.typingContextStack.popContextualType();
            this.currentContextualTypeContext = this.typingContextStack.getContextualType();
            return lastTC;
        };
        TypeChecker.prototype.hadProvisionalErrors = function () {
            return this.typingContextStack.hadProvisionalErrors;
        };
        TypeChecker.prototype.resetProvisionalErrors = function () {
            if(!this.typingContextStack.getContextualType()) {
                this.typingContextStack.hadProvisionalErrors = false;
            }
        };
        TypeChecker.prototype.typeCheckWithContextualType = function (contextType, provisional, condition, ast) {
            if(condition) {
                this.setContextualType(contextType, this.typingContextStack.isProvisional() || provisional);
            }
            this.typeFlow.typeCheck(ast);
            if(condition) {
                this.unsetContextualType();
            }
        };
        TypeChecker.prototype.resetTargetType = function () {
            this.currentContextualTypeContext = this.typingContextStack.getContextualType();
        };
        TypeChecker.prototype.killCurrentContextualType = function () {
            this.currentContextualTypeContext = null;
            this.errorReporter.pushToErrorSink = false;
        };
        TypeChecker.prototype.hasTargetType = function () {
            return this.currentContextualTypeContext && this.currentContextualTypeContext.contextualType;
        };
        TypeChecker.prototype.getTargetTypeContext = function () {
            return this.currentContextualTypeContext;
        };
        TypeChecker.prototype.inProvisionalTypecheckMode = function () {
            return this.typingContextStack.isProvisional();
        };
        TypeChecker.prototype.getTypeCheckFinishedStatus = function () {
            if(this.inProvisionalTypecheckMode()) {
                return this.typingContextStack.getContextID();
            }
            return TypeScript.TypeCheckStatus.Finished;
        };
        TypeChecker.prototype.typeStatusIsFinished = function (status) {
            return status == TypeScript.TypeCheckStatus.Finished || (this.inProvisionalTypecheckMode() && status == this.typingContextStack.getContextID());
        };
        TypeChecker.prototype.addStartedPTO = function (pto) {
            if(this.inProvisionalTypecheckMode()) {
                this.provisionalStartedTypecheckObjects[this.provisionalStartedTypecheckObjects.length] = pto;
            }
        };
        TypeChecker.prototype.cleanStartedPTO = function () {
            for(var i = 0; i < this.provisionalStartedTypecheckObjects.length; i++) {
                if(this.provisionalStartedTypecheckObjects[i].typeCheckStatus >= this.typingContextStack.getContextID()) {
                    this.provisionalStartedTypecheckObjects[i].typeCheckStatus = TypeScript.TypeCheckStatus.NotStarted;
                }
            }
            this.provisionalStartedTypecheckObjects = [];
        };
        TypeChecker.prototype.collectTypes = function (ast) {
            if(ast.nodeType == TypeScript.NodeType.Script) {
                var script = ast;
                this.locationInfo = script.locationInfo;
            }
            var globalChain = new TypeScript.ScopeChain(this.gloMod, null, this.globalScope);
            var context = new TypeScript.TypeCollectionContext(globalChain, this);
            TypeScript.getAstWalkerFactory().walk(ast, TypeScript.preCollectTypes, TypeScript.postCollectTypes, null, context);
        };
        TypeChecker.prototype.makeArrayType = function (type) {
            if(type.arrayCache == null) {
                type.arrayCache = new ArrayCache();
                type.arrayCache.arrayType = new TypeScript.Type();
                type.arrayCache.arrayType.elementType = type;
                type.arrayCache.arrayType.symbol = type.symbol;
            }
            return type.arrayCache.arrayType;
        };
        TypeChecker.prototype.getParameterList = function (funcDecl, container) {
            var args = funcDecl.arguments;
            var parameterTable = null;
            var parameterBuilder = null;
            var len = args.members.length;
            var nonOptionalParams = 0;
            var result = [];
            if(len > 0) {
                parameterTable = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
                parameterBuilder = new TypeScript.SymbolScopeBuilder(parameterTable, null, null, null, null, container);
                for(var i = 0; i < len; i++) {
                    var parameter = args.members[i];
                    var paramDef = new TypeScript.ValueLocation();
                    var parameterSymbol = new TypeScript.ParameterSymbol(parameter.id.text, parameter.minChar, this.locationInfo.unitIndex, paramDef);
                    parameterSymbol.declAST = parameter;
                    parameterSymbol.funcDecl = funcDecl;
                    parameter.id.sym = parameterSymbol;
                    parameter.sym = parameterSymbol;
                    paramDef.symbol = parameterSymbol;
                    paramDef.typeLink = TypeScript.getTypeLink(parameter.typeExpr, this, false);
                    parameterBuilder.enter(null, parameter, parameterSymbol, this.errorReporter, true, false, false);
                    result[result.length] = parameterSymbol;
                    if(!parameter.isOptionalArg()) {
                        nonOptionalParams++;
                    }
                }
            }
            return {
                parameters: result,
                nonOptionalParameterCount: nonOptionalParams
            };
        };
        TypeChecker.prototype.createFunctionSignature = function (funcDecl, container, scope, overloadGroupSym, addToScope) {
            var isExported = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Exported | TypeScript.FncFlags.ClassPropertyMethodExported) || container == this.gloMod;
            var isStatic = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Static);
            var isPrivate = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Private);
            var isDefinition = !TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Signature);
            var isAmbient = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Ambient);
            var isConstructor = funcDecl.isConstructMember() || funcDecl.isConstructor;
            var isGlobal = container == this.gloMod;
            var signature = new TypeScript.Signature();
            var isLambda = funcDecl.fncFlags & TypeScript.FncFlags.IsFunctionExpression;
            if(funcDecl.returnTypeAnnotation || isDefinition) {
                signature.returnType = TypeScript.getTypeLink(funcDecl.returnTypeAnnotation, this, false);
            } else {
                signature.returnType = new TypeScript.TypeLink();
                signature.returnType.type = this.anyType;
            }
            signature.hasVariableArgList = funcDecl.variableArgList;
            var sigData = this.getParameterList(funcDecl, container);
            signature.parameters = sigData.parameters;
            signature.nonOptionalParameterCount = sigData.nonOptionalParameterCount;
            funcDecl.signature = signature;
            signature.declAST = funcDecl;
            var useOverloadGroupSym = overloadGroupSym && overloadGroupSym.getType() && !overloadGroupSym.isAccessor() && (funcDecl.isSignature() || (isAmbient == TypeScript.hasFlag(overloadGroupSym.flags, TypeScript.SymbolFlags.Ambient)));
            if(useOverloadGroupSym && isPrivate != TypeScript.hasFlag(overloadGroupSym.flags, TypeScript.SymbolFlags.Private)) {
                this.errorReporter.simpleError(funcDecl, "Public/Private visibility of overloads does not agree");
            }
            var groupType = useOverloadGroupSym ? overloadGroupSym.getType() : new TypeScript.Type();
            if(isConstructor) {
                if(groupType.construct == null) {
                    groupType.construct = new TypeScript.SignatureGroup();
                }
                groupType.construct.addSignature(signature);
                groupType.construct.hasImplementation = !(funcDecl.isSignature());
                if(groupType.construct.hasImplementation) {
                    groupType.setHasImplementation();
                }
            } else if(funcDecl.isIndexerMember()) {
                if(groupType.index == null) {
                    groupType.index = new TypeScript.SignatureGroup();
                    groupType.index.flags |= TypeScript.SignatureFlags.IsIndexer;
                }
                groupType.index.addSignature(signature);
                groupType.index.hasImplementation = !(funcDecl.isSignature());
                if(groupType.index.hasImplementation) {
                    groupType.setHasImplementation();
                }
            } else {
                if(groupType.call == null) {
                    groupType.call = new TypeScript.SignatureGroup();
                }
                groupType.call.addSignature(signature);
                groupType.call.hasImplementation = !(funcDecl.isSignature());
                if(groupType.call.hasImplementation) {
                    groupType.setHasImplementation();
                }
            }
            var instanceType = groupType.instanceType;
            var funcName = null;
            var usedHint = false;
            if(funcDecl.name && !funcDecl.name.isMissing()) {
                funcName = funcDecl.name.text;
            } else if(funcDecl.hint) {
                funcName = funcDecl.hint;
                usedHint = true;
            }
            if(groupType.symbol == null) {
                groupType.symbol = new TypeScript.TypeSymbol(funcName ? funcName : this.anon, funcDecl.minChar, funcDecl.limChar - funcDecl.minChar, this.locationInfo.unitIndex, groupType);
                if(!useOverloadGroupSym) {
                    groupType.symbol.declAST = funcDecl;
                }
            }
            if(isStatic) {
                groupType.symbol.flags |= TypeScript.SymbolFlags.Static;
            }
            if(isAmbient) {
                groupType.symbol.flags |= TypeScript.SymbolFlags.Ambient;
            }
            if(isPrivate) {
                groupType.symbol.flags |= TypeScript.SymbolFlags.Private;
            }
            groupType.symbol.isMethod = funcDecl.isMethod();
            if(groupType.symbol.isMethod) {
                groupType.symbol.flags |= TypeScript.SymbolFlags.Property;
            }
            funcDecl.type = groupType;
            if(!isConstructor) {
                if(funcName && !isLambda && !funcDecl.isAccessor() && !usedHint) {
                    if(addToScope) {
                        if(funcDecl.isMethod() && isStatic) {
                            if(!(container).type.members.publicMembers.add(funcName, groupType.symbol)) {
                                this.errorReporter.duplicateIdentifier(funcDecl, funcName);
                            }
                            groupType.symbol.container = container;
                        } else if(overloadGroupSym == null || (overloadGroupSym.declAST && !(overloadGroupSym.declAST).isOverload && (container.isType()))) {
                            scope.enter(container, funcDecl, groupType.symbol, this.errorReporter, !isPrivate && (isExported || isStatic || isGlobal), false, isAmbient);
                        }
                    } else if(!funcDecl.isSpecialFn()) {
                        groupType.symbol.container = container;
                    }
                } else if(!funcDecl.isSpecialFn()) {
                    groupType.symbol.container = container;
                }
            }
            if(useOverloadGroupSym) {
                var overloadGroupType = overloadGroupSym ? overloadGroupSym.getType() : null;
                var classType = groupType;
                if(classType != overloadGroupType) {
                    if(classType.construct == null) {
                        if(overloadGroupType && overloadGroupType.construct) {
                            classType.construct = overloadGroupType.construct;
                        } else {
                            classType.construct = new TypeScript.SignatureGroup();
                        }
                    } else if(overloadGroupType) {
                        if(overloadGroupType.construct) {
                            classType.construct.signatures.concat(overloadGroupType.construct.signatures);
                        }
                    }
                    if(overloadGroupType) {
                        if(classType.call == null) {
                            classType.call = overloadGroupType.call;
                        } else if(overloadGroupType.call) {
                            classType.call.signatures.concat(overloadGroupType.call.signatures);
                        }
                        if(!isStatic) {
                            if(classType.instanceType == null) {
                                classType.instanceType = overloadGroupType.instanceType;
                            }
                            var instanceType = classType.instanceType;
                            if(instanceType) {
                                if(instanceType.call == null) {
                                    instanceType.call = overloadGroupType.call;
                                } else if(overloadGroupType.call) {
                                    instanceType.call.signatures.concat(overloadGroupType.call.signatures);
                                }
                            }
                        }
                        if(classType.index == null) {
                            classType.index = overloadGroupType.index;
                        } else if(overloadGroupType.index) {
                            classType.index.signatures.concat(overloadGroupType.index.signatures);
                        }
                    }
                }
            }
            return signature;
        };
        TypeChecker.prototype.createAccessorSymbol = function (funcDecl, fgSym, enclosingClass, addToMembers, isClassProperty, scope, container) {
            var accessorSym = null;
            var sig = funcDecl.signature;
            var nameText = funcDecl.name.text;
            var isStatic = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Static);
            var isPrivate = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Private);
            if(fgSym == null) {
                var field = new TypeScript.ValueLocation();
                accessorSym = new TypeScript.FieldSymbol(nameText, funcDecl.minChar, this.locationInfo.unitIndex, false, field);
                field.symbol = accessorSym;
                accessorSym.declAST = funcDecl;
                if(TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.GetAccessor)) {
                    if(accessorSym.getter) {
                        this.errorReporter.simpleError(funcDecl, "Redeclaration of property getter");
                    }
                    accessorSym.getter = sig.declAST.type.symbol;
                } else {
                    if(accessorSym.setter) {
                        this.errorReporter.simpleError(funcDecl, "Redeclaration of property setter");
                    }
                    accessorSym.setter = sig.declAST.type.symbol;
                }
                field.typeLink = TypeScript.getTypeLink(null, this, false);
                if(addToMembers) {
                    if(enclosingClass) {
                        if(!enclosingClass.members.publicMembers.add(nameText, accessorSym)) {
                            this.errorReporter.duplicateIdentifier(funcDecl, accessorSym.name);
                        }
                        accessorSym.container = enclosingClass.symbol;
                    } else {
                        this.errorReporter.simpleError(funcDecl, "Accessor property may not be added in this context");
                    }
                } else {
                    scope.enter(container, funcDecl, accessorSym, this.errorReporter, !isPrivate || isStatic, false, false);
                }
                if(isClassProperty) {
                    accessorSym.flags |= TypeScript.SymbolFlags.Property;
                }
                if(isStatic) {
                    accessorSym.flags |= TypeScript.SymbolFlags.Static;
                }
                if(isPrivate) {
                    accessorSym.flags |= TypeScript.SymbolFlags.Private;
                } else {
                    accessorSym.flags |= TypeScript.SymbolFlags.Public;
                }
            } else {
                accessorSym = (fgSym);
                if(isPrivate != TypeScript.hasFlag(accessorSym.flags, TypeScript.SymbolFlags.Private)) {
                    this.errorReporter.simpleError(funcDecl, "Getter and setter accessors do not agree in visibility");
                }
                if(TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.GetAccessor)) {
                    if(accessorSym.getter) {
                        this.errorReporter.simpleError(funcDecl, "Redeclaration of property getter");
                    }
                    accessorSym.getter = funcDecl.type.symbol;
                } else {
                    if(accessorSym.setter) {
                        this.errorReporter.simpleError(funcDecl, "Redeclaration of property setter");
                    }
                    accessorSym.setter = funcDecl.type.symbol;
                }
            }
            return accessorSym;
        };
        TypeChecker.prototype.addBases = function (resultScope, type, baseContext) {
            resultScope.addParentScope(new TypeScript.SymbolTableScope(type.members, type.ambientMembers, type.getAllEnclosedTypes(), type.getAllAmbientEnclosedTypes(), type.symbol));
            var i = 0;
            var parent;
            if(type.extendsList) {
                for(var len = type.extendsList.length; i < len; i++) {
                    parent = type.extendsList[i];
                    if(baseContext.baseId == parent.typeID) {
                        this.errorReporter.reportErrorFromSym(parent.symbol, "Type '" + baseContext.base + "' is recursively referenced as a base class of itself");
                        parent.symbol.flags |= TypeScript.SymbolFlags.RecursivelyReferenced;
                        break;
                    }
                    this.addBases(resultScope, parent, baseContext);
                }
            }
        };
        TypeChecker.prototype.scopeOf = function (type) {
            var resultScope = new TypeScript.SymbolAggregateScope(type.symbol);
            var baseContext = {
                base: type.symbol && type.symbol.name ? type.symbol.name : "{}",
                baseId: type.typeID
            };
            this.addBases(resultScope, type, baseContext);
            return resultScope;
        };
        TypeChecker.prototype.lookupMemberTypeSymbol = function (containingType, name) {
            var symbol = null;
            if(containingType.containedScope) {
                symbol = containingType.containedScope.find(name, false, true);
            } else if(containingType.members) {
                symbol = containingType.members.allMembers.lookup(name);
                if(symbol == null && containingType.ambientMembers) {
                    symbol = containingType.ambientMembers.allMembers.lookup(name);
                }
            }
            if(symbol == null || !symbol.isType()) {
                var typeMembers = containingType.getAllEnclosedTypes();
                var ambientTypeMembers = containingType.getAllAmbientEnclosedTypes();
                if(typeMembers) {
                    symbol = typeMembers.allMembers.lookup(name);
                    if(symbol == null && ambientTypeMembers) {
                        symbol = ambientTypeMembers.allMembers.lookup(name);
                    }
                }
            }
            if(symbol && symbol.isType()) {
                return symbol;
            } else {
                return null;
            }
        };
        TypeChecker.prototype.findSymbolForDynamicModule = function (idText, currentFileName, search) {
            var originalIdText = idText;
            var symbol = search(idText);
            if(symbol == null) {
                if(!symbol) {
                    idText = TypeScript.swapQuotes(originalIdText);
                    symbol = search(idText);
                }
                if(!symbol) {
                    idText = TypeScript.stripQuotes(originalIdText) + ".ts";
                    symbol = search(idText);
                }
                if(!symbol) {
                    idText = TypeScript.stripQuotes(originalIdText) + ".str";
                    symbol = search(idText);
                }
                if(!symbol) {
                    idText = TypeScript.stripQuotes(originalIdText) + ".d.ts";
                    symbol = search(idText);
                }
                if(!symbol) {
                    idText = TypeScript.stripQuotes(originalIdText) + ".d.str";
                    symbol = search(idText);
                }
                if(!symbol && !TypeScript.isRelative(originalIdText)) {
                    idText = originalIdText;
                    var strippedIdText = TypeScript.stripQuotes(idText);
                    var path = TypeScript.getRootFilePath(TypeScript.switchToForwardSlashes(currentFileName));
                    while(symbol == null && path != "") {
                        idText = TypeScript.normalizePath(path + strippedIdText + ".ts");
                        symbol = search(idText);
                        if(symbol == null) {
                            idText = TypeScript.changePathToSTR(idText);
                            symbol = search(idText);
                        }
                        if(symbol == null) {
                            idText = TypeScript.changePathToDTS(idText);
                            symbol = search(idText);
                        }
                        if(symbol == null) {
                            idText = TypeScript.changePathToDSTR(idText);
                            symbol = search(idText);
                        }
                        if(symbol == null) {
                            if(path === '/') {
                                path = '';
                            } else {
                                path = TypeScript.normalizePath(path + "..");
                                path = path && path != '/' ? path + '/' : path;
                            }
                        }
                    }
                }
            }
            return symbol;
        };
        TypeChecker.prototype.resolveTypeMember = function (scope, dotNode) {
            var lhs = dotNode.operand1;
            var rhs = dotNode.operand2;
            var resultType = this.anyType;
            var lhsType = this.anyType;
            if(lhs && rhs && (rhs.nodeType == TypeScript.NodeType.Name)) {
                if(lhs.nodeType == TypeScript.NodeType.Dot) {
                    lhsType = this.resolveTypeMember(scope, lhs);
                } else if(lhs.nodeType == TypeScript.NodeType.Name) {
                    var identifier = lhs;
                    var symbol = scope.find(identifier.text, false, true);
                    if(symbol == null) {
                        this.errorReporter.unresolvedSymbol(identifier, identifier.actualText);
                    } else if(symbol.isType()) {
                        var typeSymbol = symbol;
                        if(typeSymbol.aliasLink && !typeSymbol.type && typeSymbol.aliasLink.alias.nodeType == TypeScript.NodeType.Name) {
                            var modPath = (typeSymbol.aliasLink.alias).text;
                            var modSym = this.findSymbolForDynamicModule(modPath, this.locationInfo.filename, function (id) {
                                return scope.find(id, false, true);
                            });
                            if(modSym) {
                                typeSymbol.type = modSym.getType();
                            }
                        }
                        if(TypeScript.optimizeModuleCodeGen && symbol) {
                            var symType = symbol.getType();
                            if(symType && typeSymbol.aliasLink && typeSymbol.onlyReferencedAsTypeRef) {
                                var modDecl = symType.symbol.declAST;
                                if(modDecl && TypeScript.hasFlag(modDecl.modFlags, TypeScript.ModuleFlags.IsDynamic)) {
                                    typeSymbol.onlyReferencedAsTypeRef = !this.resolvingBases;
                                }
                            }
                        }
                        if(!symbol.visible(scope, this)) {
                            this.errorReporter.simpleError(lhs, "The symbol '" + identifier.actualText + "' is not visible at this point");
                        }
                        lhsType = symbol.getType();
                        identifier.sym = symbol;
                    } else {
                        this.errorReporter.simpleError(lhs, "Expected type");
                    }
                }
                if(!lhsType) {
                    lhsType = this.anyType;
                }
                if(lhsType != this.anyType) {
                    var rhsIdentifier = rhs;
                    var resultSymbol = this.lookupMemberTypeSymbol(lhsType, rhsIdentifier.text);
                    if(resultSymbol == null) {
                        resultType = this.anyType;
                        this.errorReporter.simpleError(dotNode, "Expected type");
                    } else {
                        resultType = resultSymbol.getType();
                        if(!resultSymbol.visible(scope, this)) {
                            this.errorReporter.simpleError(lhs, "The symbol '" + (rhs).actualText + "' is not visible at this point");
                        }
                    }
                    rhsIdentifier.sym = resultType.symbol;
                }
            }
            if(resultType.isClass()) {
                resultType = resultType.instanceType;
            }
            return resultType;
        };
        TypeChecker.prototype.resolveFuncDecl = function (funcDecl, scope, fgSym) {
            var functionGroupSymbol = this.createFunctionSignature(funcDecl, scope.container, scope, fgSym, false).declAST.type.symbol;
            var signatures;
            if(funcDecl.isConstructMember()) {
                signatures = functionGroupSymbol.type.construct.signatures;
            } else if(funcDecl.isIndexerMember()) {
                signatures = functionGroupSymbol.type.getInstanceType().index.signatures;
            } else {
                signatures = functionGroupSymbol.type.call.signatures;
            }
            var signature = signatures[signatures.length - 1];
            var len = signature.parameters.length;
            for(var i = 0; i < len; i++) {
                var paramSym = signature.parameters[i];
                this.resolveTypeLink(scope, paramSym.parameter.typeLink, true);
            }
            if(len && funcDecl.variableArgList) {
                if(!signature.parameters[len - 1].parameter.typeLink.type.elementType) {
                    this.errorReporter.simpleErrorFromSym(signature.parameters[len - 1].parameter.symbol, "... parameter must have array type");
                    signature.parameters[len - 1].parameter.typeLink.type = this.makeArrayType(signature.parameters[len - 1].parameter.typeLink.type);
                }
            }
            this.resolveTypeLink(scope, signature.returnType, funcDecl.isSignature());
            return functionGroupSymbol;
        };
        TypeChecker.prototype.resolveVarDecl = function (varDecl, scope) {
            var field = new TypeScript.ValueLocation();
            var fieldSymbol = new TypeScript.FieldSymbol(varDecl.id.text, varDecl.minChar, this.locationInfo.unitIndex, (varDecl.varFlags & TypeScript.VarFlags.Readonly) == TypeScript.VarFlags.None, field);
            fieldSymbol.transferVarFlags(varDecl.varFlags);
            field.symbol = fieldSymbol;
            fieldSymbol.declAST = varDecl;
            field.typeLink = TypeScript.getTypeLink(varDecl.typeExpr, this, varDecl.init == null);
            this.resolveTypeLink(scope, field.typeLink, true);
            varDecl.sym = fieldSymbol;
            varDecl.type = field.typeLink.type;
            return fieldSymbol;
        };
        TypeChecker.prototype.resolveTypeLink = function (scope, typeLink, supplyVar) {
            var arrayCount = 0;
            if(typeLink.type == null) {
                var ast = typeLink.ast;
                if(ast) {
                    while(typeLink.type == null) {
                        switch(ast.nodeType) {
                            case TypeScript.NodeType.Name:
                                var identifier = ast;
                                var symbol = scope.find(identifier.text, false, true);
                                if(symbol == null) {
                                    typeLink.type = this.anyType;
                                    this.errorReporter.unresolvedSymbol(identifier, identifier.actualText);
                                } else if(symbol.isType()) {
                                    if(!symbol.visible(scope, this)) {
                                        this.errorReporter.simpleError(ast, "The symbol '" + identifier.actualText + "' is not visible at this point");
                                    }
                                    identifier.sym = symbol;
                                    typeLink.type = symbol.getType();
                                    if(typeLink.type) {
                                        if(typeLink.type.isClass()) {
                                            typeLink.type = typeLink.type.instanceType;
                                        }
                                    } else {
                                        typeLink.type = this.anyType;
                                    }
                                } else {
                                    typeLink.type = this.anyType;
                                    this.errorReporter.simpleError(ast, "Expected type");
                                }
                                break;
                            case TypeScript.NodeType.Dot:
                                typeLink.type = this.resolveTypeMember(scope, ast);
                                break;
                            case TypeScript.NodeType.TypeRef:
                                var typeRef = ast;
                                arrayCount = typeRef.arrayCount;
                                ast = typeRef.term;
                                if(ast == null) {
                                    typeLink.type = this.anyType;
                                }
                                break;
                            case TypeScript.NodeType.InterfaceDeclaration:
                                var interfaceDecl = ast;
                                var interfaceType = new TypeScript.Type();
                                var interfaceSymbol = new TypeScript.TypeSymbol((interfaceDecl.name).text, ast.minChar, ast.limChar - ast.minChar, this.locationInfo.unitIndex, interfaceType);
                                interfaceType.symbol = interfaceSymbol;
                                interfaceType.members = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
                                interfaceType.containedScope = new TypeScript.SymbolTableScope(interfaceType.members, null, null, null, interfaceSymbol);
                                interfaceType.containedScope.container = interfaceSymbol;
                                interfaceType.memberScope = interfaceType.containedScope;
                                var memberList = interfaceDecl.members;
                                var props = memberList.members;
                                var propsLen = props.length;
                                for(var j = 0; j < propsLen; j++) {
                                    var propDecl = props[j];
                                    var propSym = null;
                                    var addMember = true;
                                    var id = null;
                                    if(propDecl.nodeType == TypeScript.NodeType.FuncDecl) {
                                        var funcDecl = propDecl;
                                        id = funcDecl.name;
                                        propSym = interfaceType.members.allMembers.lookup(funcDecl.getNameText());
                                        addMember = (propSym == null);
                                        if(funcDecl.isSpecialFn()) {
                                            addMember = false;
                                            propSym = this.resolveFuncDecl(funcDecl, scope, interfaceSymbol);
                                        } else {
                                            propSym = this.resolveFuncDecl(funcDecl, scope, propSym);
                                        }
                                        funcDecl.type = (propSym).type;
                                    } else {
                                        id = (propDecl).id;
                                        propSym = this.resolveVarDecl(propDecl, scope);
                                        addMember = !id.isMissing();
                                    }
                                    if(addMember) {
                                        if(id && TypeScript.hasFlag(id.flags, TypeScript.ASTFlags.OptionalName)) {
                                            propSym.flags |= TypeScript.SymbolFlags.Optional;
                                        }
                                        if(!interfaceType.members.allMembers.add(propSym.name, propSym)) {
                                            this.errorReporter.duplicateIdentifier(ast, propSym.name);
                                        }
                                    }
                                }
                                ast.type = interfaceType;
                                typeLink.type = interfaceType;
                                break;
                            case TypeScript.NodeType.FuncDecl:
                                var tsym = this.resolveFuncDecl(ast, scope, null);
                                typeLink.type = tsym.type;
                                break;
                            default:
                                typeLink.type = this.anyType;
                                this.errorReporter.simpleError(ast, "Expected type");
                                break;
                        }
                    }
                }
                for(var count = arrayCount; count > 0; count--) {
                    typeLink.type = this.makeArrayType(typeLink.type);
                }
                if(supplyVar && (typeLink.type == null)) {
                    typeLink.type = this.anyType;
                }
                if(typeLink.ast) {
                    typeLink.ast.type = typeLink.type;
                }
            }
        };
        TypeChecker.prototype.resolveBaseTypeLink = function (typeLink, scope) {
            this.resolvingBases = true;
            this.resolveTypeLink(scope, typeLink, true);
            this.resolvingBases = false;
            var extendsType = null;
            if(typeLink.type.isClass()) {
                extendsType = typeLink.type.instanceType;
            } else {
                extendsType = typeLink.type;
            }
            return extendsType;
        };
        TypeChecker.prototype.findMostApplicableSignature = function (signatures, args) {
            if(signatures.length == 1) {
                return {
                    sig: signatures[0].signature,
                    ambiguous: false
                };
            }
            var best = signatures[0];
            var Q = null;
            var AType = null;
            var PType = null;
            var QType = null;
            var ambiguous = false;
            for(var qSig = 1; qSig < signatures.length; qSig++) {
                Q = signatures[qSig];
                var i = 0;
                for(i = 0; args && i < args.members.length; i++) {
                    AType = args.members[i].type;
                    PType = i < best.signature.parameters.length ? best.signature.parameters[i].getType() : best.signature.parameters[best.signature.parameters.length - 1].getType().elementType;
                    QType = i < Q.signature.parameters.length ? Q.signature.parameters[i].getType() : Q.signature.parameters[Q.signature.parameters.length - 1].getType().elementType;
                    if(this.typesAreIdentical(PType, QType)) {
                        continue;
                    } else if(this.typesAreIdentical(AType, PType)) {
                        break;
                    } else if(this.typesAreIdentical(AType, QType)) {
                        best = Q;
                        break;
                    } else if(this.sourceIsSubtypeOfTarget(PType, QType)) {
                        break;
                    } else if(this.sourceIsSubtypeOfTarget(QType, PType)) {
                        best = Q;
                        break;
                    } else if(Q.hadProvisionalErrors) {
                        break;
                    } else if(best.hadProvisionalErrors) {
                        best = Q;
                        break;
                    }
                }
                if(!args || i == args.members.length) {
                    var collection = {
                        getLength: function () {
                            return 2;
                        },
                        setTypeAtIndex: function (index, type) {
                        },
                        getTypeAtIndex: function (index) {
                            return index ? Q.signature.returnType.type : best.signature.returnType.type;
                        }
                    };
                    var bct = this.findBestCommonType(best.signature.returnType.type, null, collection, true);
                    ambiguous = !bct;
                } else {
                    ambiguous = false;
                }
            }
            return {
                sig: best.signature,
                ambiguous: ambiguous
            };
        };
        TypeChecker.prototype.getApplicableSignatures = function (signatures, args, comparisonInfo) {
            var applicableSigs = [];
            var memberType = null;
            var miss = false;
            var cxt = null;
            var hadProvisionalErrors = false;
            for(var i = 0; i < signatures.length; i++) {
                miss = false;
                for(var j = 0; j < args.members.length; j++) {
                    if(j >= signatures[i].parameters.length) {
                        continue;
                    }
                    memberType = signatures[i].parameters[j].getType();
                    if(signatures[i].declAST.variableArgList && (j >= signatures[i].nonOptionalParameterCount - 1) && memberType.isArray()) {
                        memberType = memberType.elementType;
                    }
                    if(memberType == this.anyType) {
                        continue;
                    } else if(args.members[j].nodeType == TypeScript.NodeType.FuncDecl) {
                        if(this.typeFlow.functionInterfaceType && memberType == this.typeFlow.functionInterfaceType) {
                            continue;
                        }
                        if(!this.canContextuallyTypeFunction(memberType, args.members[j], true)) {
                            if(this.canContextuallyTypeFunction(memberType, args.members[j], false)) {
                                this.typeFlow.typeCheck(args.members[j]);
                                if(!this.sourceIsAssignableToTarget(args.members[j].type, memberType, comparisonInfo)) {
                                    break;
                                }
                            } else {
                                break;
                            }
                        } else {
                            this.typeCheckWithContextualType(memberType, true, true, args.members[j]);
                            this.cleanStartedPTO();
                            hadProvisionalErrors = this.hadProvisionalErrors();
                            if(!this.sourceIsAssignableToTarget(args.members[j].type, memberType, comparisonInfo)) {
                                if(comparisonInfo) {
                                    comparisonInfo.setMessage("Could not apply type '" + memberType.getTypeName() + "' to argument " + (j + 1) + ", which is of type '" + args.members[j].type.getTypeName() + "'");
                                }
                                miss = true;
                            }
                            this.resetProvisionalErrors();
                            if(miss) {
                                break;
                            }
                        }
                    } else if(args.members[j].nodeType == TypeScript.NodeType.ObjectLit) {
                        if(this.typeFlow.objectInterfaceType && memberType == this.typeFlow.objectInterfaceType) {
                            continue;
                        }
                        this.typeCheckWithContextualType(memberType, true, true, args.members[j]);
                        this.cleanStartedPTO();
                        hadProvisionalErrors = this.hadProvisionalErrors();
                        if(!this.sourceIsAssignableToTarget(args.members[j].type, memberType, comparisonInfo)) {
                            if(comparisonInfo) {
                                comparisonInfo.setMessage("Could not apply type '" + memberType.getTypeName() + "' to argument " + (j + 1) + ", which is of type '" + args.members[j].type.getTypeName() + "'");
                            }
                            miss = true;
                        }
                        this.resetProvisionalErrors();
                        if(miss) {
                            break;
                        }
                    } else if(args.members[j].nodeType == TypeScript.NodeType.ArrayLit) {
                        if(this.typeFlow.arrayInterfaceType && memberType == this.typeFlow.arrayInterfaceType) {
                            continue;
                        }
                        this.typeCheckWithContextualType(memberType, true, true, args.members[j]);
                        this.cleanStartedPTO();
                        hadProvisionalErrors = this.hadProvisionalErrors();
                        if(!this.sourceIsAssignableToTarget(args.members[j].type, memberType, comparisonInfo)) {
                            if(comparisonInfo) {
                                comparisonInfo.setMessage("Could not apply type '" + memberType.getTypeName() + "' to argument " + (j + 1) + ", which is of type '" + args.members[j].type.getTypeName() + "'");
                            }
                            break;
                        }
                        this.resetProvisionalErrors();
                        if(miss) {
                            break;
                        }
                    }
                }
                if(j == args.members.length) {
                    applicableSigs[applicableSigs.length] = {
                        signature: signatures[i],
                        hadProvisionalErrors: hadProvisionalErrors
                    };
                }
                hadProvisionalErrors = false;
            }
            return applicableSigs;
        };
        TypeChecker.prototype.canContextuallyTypeFunction = function (candidateType, funcDecl, beStringent) {
            if(funcDecl.isParenthesized || funcDecl.isMethod() || beStringent && funcDecl.returnTypeAnnotation || funcDecl.isInlineCallLiteral) {
                return false;
            }
            beStringent = beStringent || (this.typeFlow.functionInterfaceType == candidateType);
            if(!beStringent) {
                return true;
            }
            if(!funcDecl.signature) {
                this.createFunctionSignature(funcDecl, this.typeFlow.scope.container, this.typeFlow.scope, null, null);
                this.typeFlow.typeCheck(funcDecl);
            }
            var signature = funcDecl.signature;
            var paramLen = signature.parameters.length;
            for(var i = 0; i < paramLen; i++) {
                var param = signature.parameters[i];
                var symbol = param;
                var argDecl = symbol.declAST;
                if(beStringent && argDecl.typeExpr) {
                    return false;
                }
            }
            if(candidateType.construct && candidateType.call) {
                return false;
            }
            var candidateSigs = candidateType.construct ? candidateType.construct : candidateType.call;
            if(!candidateSigs || candidateSigs.signatures.length > 1) {
                return false;
            }
            return true;
        };
        TypeChecker.prototype.canContextuallyTypeObjectLiteral = function (targetType, objectLit) {
            if(targetType == this.typeFlow.objectInterfaceType) {
                return true;
            }
            var memberDecls = objectLit.operand;
            if(!(memberDecls && targetType.memberScope)) {
                return false;
            }
            var id = null;
            var targetMember = null;
            var text = "";
            var foundSyms = {
            };
            for(var i = 0; i < memberDecls.members.length; i++) {
                id = (memberDecls.members[i]).operand1;
                if(id.nodeType == TypeScript.NodeType.Name) {
                    text = (id).text;
                } else if(id.nodeType == TypeScript.NodeType.QString) {
                    var idText = (id).text;
                    text = idText.substring(1, idText.length - 1);
                } else {
                    return false;
                }
                targetMember = targetType.memberScope.find(text, true, false);
                if(!targetMember) {
                    return false;
                }
                foundSyms[text] = true;
            }
            var targetMembers = targetType.memberScope.getAllValueSymbolNames(true);
            for(var i = 0; i < targetMembers.length; i++) {
                var memberName = targetMembers[i];
                var memberSym = targetType.memberScope.find(memberName, true, false);
                if(!foundSyms[targetMembers[i]] && !TypeScript.hasFlag(memberSym.flags, TypeScript.SymbolFlags.Optional)) {
                    return false;
                }
            }
            return true;
        };
        TypeChecker.prototype.widenType = function (t) {
            if(t == this.undefinedType || t == this.nullType) {
                return this.anyType;
            }
            return t;
        };
        TypeChecker.prototype.isNullOrUndefinedType = function (t) {
            return t == this.undefinedType || t == this.nullType;
        };
        TypeChecker.prototype.findBestCommonType = function (initialType, targetType, collection, acceptVoid, comparisonInfo) {
            var i = 0;
            var len = collection.getLength();
            var nlastChecked = 0;
            var bestCommonType = initialType;
            if(targetType) {
                bestCommonType = bestCommonType ? bestCommonType.mergeOrdered(targetType, this, acceptVoid) : targetType;
            }
            var convergenceType = bestCommonType;
            while(nlastChecked < len) {
                for(i = 0; i < len; i++) {
                    if(i == nlastChecked) {
                        continue;
                    }
                    if(convergenceType && (bestCommonType = convergenceType.mergeOrdered(collection.getTypeAtIndex(i), this, acceptVoid, comparisonInfo))) {
                        convergenceType = bestCommonType;
                    }
                    if(bestCommonType == this.anyType || bestCommonType == null) {
                        break;
                    } else if(targetType) {
                        collection.setTypeAtIndex(i, targetType);
                    }
                }
                if(convergenceType && bestCommonType) {
                    break;
                }
                nlastChecked++;
                if(nlastChecked < len) {
                    convergenceType = collection.getTypeAtIndex(nlastChecked);
                }
            }
            return acceptVoid ? bestCommonType : (bestCommonType == this.voidType ? null : bestCommonType);
        };
        TypeChecker.prototype.typesAreIdentical = function (t1, t2) {
            if(t1 == t2) {
                return true;
            }
            if(!t1 || !t2) {
                return false;
            }
            if(t1.isClass() || t1.isClassInstance()) {
                return false;
            }
            var comboId = (t2.typeID << 16) | t1.typeID;
            if(this.identicalCache[comboId]) {
                return true;
            }
            if((t1.typeFlags & TypeScript.TypeFlags.IsEnum) || (t2.typeFlags & TypeScript.TypeFlags.IsEnum)) {
                return false;
            }
            if(t1.isArray() || t2.isArray()) {
                if(!(t1.isArray() && t2.isArray())) {
                    return false;
                }
                this.identicalCache[comboId] = false;
                var ret = this.typesAreIdentical(t1.elementType, t2.elementType);
                if(ret) {
                    this.subtypeCache[comboId] = true;
                } else {
                    this.subtypeCache[comboId] = undefined;
                }
                return ret;
            }
            if(t1.primitiveTypeClass != t2.primitiveTypeClass) {
                return false;
            }
            this.identicalCache[comboId] = false;
            if(t1.memberScope && t2.memberScope) {
                var t1MemberKeys = t1.memberScope.getAllValueSymbolNames(true).sort();
                var t2MemberKeys = t2.memberScope.getAllValueSymbolNames(true).sort();
                if(t1MemberKeys.length != t2MemberKeys.length) {
                    this.identicalCache[comboId] = undefined;
                    return false;
                }
                var t1MemberSymbol = null;
                var t2MemberSymbol = null;
                var t1MemberType = null;
                var t2MemberType = null;
                for(var iMember = 0; iMember < t1MemberKeys.length; iMember++) {
                    if(t1MemberKeys[iMember] != t2MemberKeys[iMember]) {
                        this.identicalCache[comboId] = undefined;
                        return false;
                    }
                    t1MemberSymbol = t1.memberScope.find(t1MemberKeys[iMember], false, false);
                    t2MemberSymbol = t2.memberScope.find(t2MemberKeys[iMember], false, false);
                    if((t1MemberSymbol.flags & TypeScript.SymbolFlags.Optional) != (t2MemberSymbol.flags & TypeScript.SymbolFlags.Optional)) {
                        this.identicalCache[comboId] = undefined;
                        return false;
                    }
                    t1MemberType = t1MemberSymbol.getType();
                    t2MemberType = t2MemberSymbol.getType();
                    if(t1MemberType && t2MemberType && (this.identicalCache[(t2MemberType.typeID << 16) | t1MemberType.typeID] != undefined)) {
                        continue;
                    }
                    if(!this.typesAreIdentical(t1MemberType, t2MemberType)) {
                        this.identicalCache[comboId] = undefined;
                        return false;
                    }
                }
            } else if(t1.memberScope || t2.memberScope) {
                this.identicalCache[comboId] = undefined;
                return false;
            }
            if(!this.signatureGroupsAreIdentical(t1.call, t2.call)) {
                this.identicalCache[comboId] = undefined;
                return false;
            }
            if(!this.signatureGroupsAreIdentical(t1.construct, t2.construct)) {
                this.identicalCache[comboId] = undefined;
                return false;
            }
            if(!this.signatureGroupsAreIdentical(t1.index, t2.index)) {
                this.identicalCache[comboId] = undefined;
                return false;
            }
            this.identicalCache[comboId] = true;
            return true;
        };
        TypeChecker.prototype.signatureGroupsAreIdentical = function (sg1, sg2) {
            if(sg1 == sg2) {
                return true;
            }
            if(!sg1 || !sg2) {
                return false;
            }
            if(sg1.signatures.length != sg2.signatures.length) {
                return false;
            }
            var sig1 = null;
            var sig2 = null;
            var sigsMatch = false;
            for(var iSig1 = 0; iSig1 < sg1.signatures.length; iSig1++) {
                sig1 = sg1.signatures[iSig1];
                for(var iSig2 = 0; iSig2 < sg2.signatures.length; iSig2++) {
                    sig2 = sg2.signatures[iSig2];
                    if(this.signaturesAreIdentical(sig1, sig2)) {
                        sigsMatch = true;
                        break;
                    }
                }
                if(sigsMatch) {
                    sigsMatch = false;
                    continue;
                }
                return false;
            }
            return true;
        };
        TypeChecker.prototype.signaturesAreIdentical = function (s1, s2) {
            if(s1.hasVariableArgList != s2.hasVariableArgList) {
                return false;
            }
            if(s1.nonOptionalParameterCount != s2.nonOptionalParameterCount) {
                return false;
            }
            if(s1.parameters.length != s2.parameters.length) {
                return false;
            }
            if(!this.typesAreIdentical(s1.returnType.type, s2.returnType.type)) {
                return false;
            }
            for(var iParam = 0; iParam < s1.parameters.length; iParam++) {
                if(!this.typesAreIdentical(s1.parameters[iParam].parameter.typeLink.type, s2.parameters[iParam].parameter.typeLink.type)) {
                    return false;
                }
            }
            return true;
        };
        TypeChecker.prototype.sourceIsSubtypeOfTarget = function (source, target, comparisonInfo) {
            return this.sourceIsRelatableToTarget(source, target, false, this.subtypeCache, comparisonInfo);
        };
        TypeChecker.prototype.signatureGroupIsSubtypeOfTarget = function (sg1, sg2, comparisonInfo) {
            return this.signatureGroupIsRelatableToTarget(sg1, sg2, false, this.subtypeCache, comparisonInfo);
        };
        TypeChecker.prototype.signatureIsSubtypeOfTarget = function (s1, s2, comparisonInfo) {
            return this.signatureIsRelatableToTarget(s1, s2, false, this.subtypeCache, comparisonInfo);
        };
        TypeChecker.prototype.sourceIsAssignableToTarget = function (source, target, comparisonInfo) {
            return this.sourceIsRelatableToTarget(source, target, true, this.assignableCache, comparisonInfo);
        };
        TypeChecker.prototype.signatureGroupIsAssignableToTarget = function (sg1, sg2, comparisonInfo) {
            return this.signatureGroupIsRelatableToTarget(sg1, sg2, true, this.assignableCache, comparisonInfo);
        };
        TypeChecker.prototype.signatureIsAssignableToTarget = function (s1, s2, comparisonInfo) {
            return this.signatureIsRelatableToTarget(s1, s2, true, this.assignableCache, comparisonInfo);
        };
        TypeChecker.prototype.sourceIsRelatableToTarget = function (source, target, assignableTo, comparisonCache, comparisonInfo) {
            if(source == target) {
                return true;
            }
            if(!(source && target)) {
                return true;
            }
            var comboId = (source.typeID << 16) | target.typeID;
            if(comparisonCache[comboId] != undefined) {
                return true;
            }
            if(assignableTo) {
                if(source == this.anyType || target == this.anyType) {
                    return true;
                }
            } else {
                if(target == this.anyType) {
                    return true;
                }
            }
            if(source == this.undefinedType) {
                return true;
            }
            if((source == this.nullType) && (target != this.undefinedType && target != this.voidType)) {
                return true;
            }
            if(target == this.numberType && (source.typeFlags & TypeScript.TypeFlags.IsEnum)) {
                return true;
            }
            if(source == this.numberType && (target.typeFlags & TypeScript.TypeFlags.IsEnum)) {
                return true;
            }
            if((source.typeFlags & TypeScript.TypeFlags.IsEnum) || (target.typeFlags & TypeScript.TypeFlags.IsEnum)) {
                return false;
            }
            if(source.isArray() || target.isArray()) {
                if(!(source.isArray() && target.isArray())) {
                    return false;
                }
                comparisonCache[comboId] = false;
                var ret = this.sourceIsRelatableToTarget(source.elementType, target.elementType, assignableTo, comparisonCache, comparisonInfo);
                if(ret) {
                    comparisonCache[comboId] = true;
                } else {
                    comparisonCache[comboId] = undefined;
                }
                return ret;
            }
            if(source.primitiveTypeClass != target.primitiveTypeClass) {
                if(target.primitiveTypeClass == TypeScript.Primitive.None) {
                    if(source == this.numberType && this.typeFlow.numberInterfaceType) {
                        source = this.typeFlow.numberInterfaceType;
                    } else if(source == this.stringType && this.typeFlow.stringInterfaceType) {
                        source = this.typeFlow.stringInterfaceType;
                    } else if(source == this.booleanType && this.typeFlow.booleanInterfaceType) {
                        source = this.typeFlow.booleanInterfaceType;
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            comparisonCache[comboId] = false;
            if(source.hasBase(target)) {
                comparisonCache[comboId] = true;
                return true;
            }
            if(this.typeFlow.objectInterfaceType && target == this.typeFlow.objectInterfaceType) {
                return true;
            }
            if(this.typeFlow.functionInterfaceType && (source.call || source.construct) && target == this.typeFlow.functionInterfaceType) {
                return true;
            }
            if(target.isClass() || target.isClassInstance()) {
                comparisonCache[comboId] = undefined;
                return false;
            }
            if(target.memberScope && source.memberScope) {
                var mPropKeys = target.memberScope.getAllValueSymbolNames(true);
                var mProp = null;
                var nProp = null;
                var mPropType = null;
                var nPropType = null;
                var inferenceSymbol = null;
                for(var iMProp = 0; iMProp < mPropKeys.length; iMProp++) {
                    mProp = target.memberScope.find(mPropKeys[iMProp], false, false);
                    nProp = source.memberScope.find(mPropKeys[iMProp], false, false);
                    if(mProp.name == "arguments" && this.typeFlow.iargumentsInterfaceType && (this.typeFlow.iargumentsInterfaceType.symbol.flags & TypeScript.SymbolFlags.CompilerGenerated) && mProp.kind() == TypeScript.SymbolKind.Variable && (mProp).variable.typeLink.type == this.typeFlow.iargumentsInterfaceType) {
                        continue;
                    }
                    if(mProp.isInferenceSymbol()) {
                        inferenceSymbol = mProp;
                        if(inferenceSymbol.typeCheckStatus == TypeScript.TypeCheckStatus.NotStarted) {
                            this.typeFlow.typeCheck(mProp.declAST);
                        }
                    }
                    mPropType = mProp.getType();
                    if(!nProp) {
                        if(this.typeFlow.objectInterfaceType) {
                            nProp = this.typeFlow.objectInterfaceType.memberScope.find(mPropKeys[iMProp], false, false);
                        }
                        if(!nProp) {
                            if(this.typeFlow.functionInterfaceType && (mPropType.call || mPropType.construct)) {
                                nProp = this.typeFlow.functionInterfaceType.memberScope.find(mPropKeys[iMProp], false, false);
                            }
                            if(!nProp) {
                                if(!(mProp.flags & TypeScript.SymbolFlags.Optional)) {
                                    comparisonCache[comboId] = undefined;
                                    if(comparisonInfo) {
                                        comparisonInfo.flags |= TypeScript.TypeRelationshipFlags.RequiredPropertyIsMissing;
                                        comparisonInfo.addMessageToFront("Type '" + source.getTypeName() + "' is missing property '" + mPropKeys[iMProp] + "' from type '" + target.getTypeName() + "'");
                                    }
                                    return false;
                                } else {
                                    continue;
                                }
                            }
                        }
                    }
                    if(nProp.isInferenceSymbol()) {
                        inferenceSymbol = nProp;
                        if(inferenceSymbol.typeCheckStatus == TypeScript.TypeCheckStatus.NotStarted) {
                            this.typeFlow.typeCheck(nProp.declAST);
                        }
                    }
                    nPropType = nProp.getType();
                    if(mPropType && nPropType && (comparisonCache[(nPropType.typeID << 16) | mPropType.typeID] != undefined)) {
                        continue;
                    }
                    if(!this.sourceIsRelatableToTarget(nPropType, mPropType, assignableTo, comparisonCache, comparisonInfo)) {
                        comparisonCache[comboId] = undefined;
                        if(comparisonInfo) {
                            comparisonInfo.flags |= TypeScript.TypeRelationshipFlags.IncompatiblePropertyTypes;
                            comparisonInfo.addMessageToFront("Types of property '" + mProp.name + "' of types '" + source.getTypeName() + "' and '" + target.getTypeName() + "' are incompatible");
                        }
                        return false;
                    }
                }
            }
            if(source.call || target.call) {
                if(!this.signatureGroupIsRelatableToTarget(source.call, target.call, assignableTo, comparisonCache, comparisonInfo)) {
                    if(comparisonInfo) {
                        if(source.call && target.call) {
                            comparisonInfo.addMessageToFront("Call signatures of types '" + source.getTypeName() + "' and '" + target.getTypeName() + "' are incompatible");
                        } else {
                            var hasSig = target.call ? target.getTypeName() : source.getTypeName();
                            var lacksSig = !target.call ? target.getTypeName() : source.getTypeName();
                            comparisonInfo.setMessage("Type '" + hasSig + "' requires a call signature, but Type '" + lacksSig + "' lacks one");
                        }
                        comparisonInfo.flags |= TypeScript.TypeRelationshipFlags.IncompatibleSignatures;
                    }
                    comparisonCache[comboId] = undefined;
                    return false;
                }
            }
            if(source.construct || target.construct) {
                if(!this.signatureGroupIsRelatableToTarget(source.construct, target.construct, assignableTo, comparisonCache, comparisonInfo)) {
                    if(comparisonInfo) {
                        if(source.construct && target.construct) {
                            comparisonInfo.addMessageToFront("Construct signatures of types '" + source.getTypeName() + "' and '" + target.getTypeName() + "' are incompatible");
                        } else {
                            var hasSig = target.construct ? target.getTypeName() : source.getTypeName();
                            var lacksSig = !target.construct ? target.getTypeName() : source.getTypeName();
                            comparisonInfo.setMessage("Type '" + hasSig + "' requires a construct signature, but Type '" + lacksSig + "' lacks one");
                        }
                        comparisonInfo.flags |= TypeScript.TypeRelationshipFlags.IncompatibleSignatures;
                    }
                    comparisonCache[comboId] = undefined;
                    return false;
                }
            }
            if(target.index) {
                var targetIndex = !target.index && this.typeFlow.objectInterfaceType ? this.typeFlow.objectInterfaceType.index : target.index;
                var sourceIndex = !source.index && this.typeFlow.objectInterfaceType ? this.typeFlow.objectInterfaceType.index : source.index;
                if(!this.signatureGroupIsRelatableToTarget(sourceIndex, targetIndex, assignableTo, comparisonCache, comparisonInfo)) {
                    if(comparisonInfo) {
                        comparisonInfo.addMessageToFront("Index signatures of types '" + source.getTypeName() + "' and '" + target.getTypeName() + "' are incompatible");
                        comparisonInfo.flags |= TypeScript.TypeRelationshipFlags.IncompatibleSignatures;
                    }
                    comparisonCache[comboId] = undefined;
                    return false;
                }
            }
            comparisonCache[comboId] = true;
            return true;
        };
        TypeChecker.prototype.signatureGroupIsRelatableToTarget = function (sourceSG, targetSG, assignableTo, comparisonCache, comparisonInfo) {
            if(sourceSG == targetSG) {
                return true;
            }
            if(!(sourceSG && targetSG)) {
                return false;
            }
            var mSig = null;
            var nSig = null;
            var foundMatch = false;
            for(var iMSig = 0; iMSig < targetSG.signatures.length; iMSig++) {
                mSig = targetSG.signatures[iMSig];
                for(var iNSig = 0; iNSig < sourceSG.signatures.length; iNSig++) {
                    nSig = sourceSG.signatures[iNSig];
                    if(this.signatureIsRelatableToTarget(nSig, mSig, assignableTo, comparisonCache, comparisonInfo)) {
                        foundMatch = true;
                        break;
                    }
                }
                if(foundMatch) {
                    foundMatch = false;
                    continue;
                }
                return false;
            }
            return true;
        };
        TypeChecker.prototype.signatureIsRelatableToTarget = function (sourceSig, targetSig, assignableTo, comparisonCache, comparisonInfo) {
            if(!sourceSig.parameters || !targetSig.parameters) {
                return false;
            }
            var targetVarArgCount = targetSig.hasVariableArgList ? targetSig.nonOptionalParameterCount - 1 : targetSig.nonOptionalParameterCount;
            var sourceVarArgCount = sourceSig.hasVariableArgList ? sourceSig.nonOptionalParameterCount - 1 : sourceSig.nonOptionalParameterCount;
            if(sourceVarArgCount > targetVarArgCount && !targetSig.hasVariableArgList) {
                if(comparisonInfo) {
                    comparisonInfo.flags |= TypeScript.TypeRelationshipFlags.SourceSignatureHasTooManyParameters;
                    comparisonInfo.addMessageToFront("Call signature expects " + targetVarArgCount + " or fewer parameters");
                }
                return false;
            }
            var sourceReturnType = sourceSig.returnType.type;
            var targetReturnType = targetSig.returnType.type;
            if(targetReturnType != this.voidType) {
                if(!this.sourceIsRelatableToTarget(sourceReturnType, targetReturnType, assignableTo, comparisonCache, comparisonInfo)) {
                    if(comparisonInfo) {
                        comparisonInfo.flags |= TypeScript.TypeRelationshipFlags.IncompatibleReturnTypes;
                    }
                    return false;
                }
            }
            var len = (sourceVarArgCount < targetVarArgCount && sourceSig.hasVariableArgList) ? targetVarArgCount : sourceVarArgCount;
            var sourceParamType = null;
            var targetParamType = null;
            var sourceParamName = "";
            var targetParamName = "";
            for(var iSource = 0, iTarget = 0; iSource < len; iSource++ , iTarget++) {
                if(!sourceSig.hasVariableArgList || iSource < sourceVarArgCount) {
                    sourceParamType = (sourceSig.parameters[iSource]).parameter.typeLink.type;
                    sourceParamName = (sourceSig.parameters[iSource]).parameter.symbol.name;
                } else if(iSource == sourceVarArgCount) {
                    sourceParamType = (sourceSig.parameters[iSource]).parameter.typeLink.type;
                    if(sourceParamType.elementType) {
                        sourceParamType = sourceParamType.elementType;
                    }
                    sourceParamName = (sourceSig.parameters[iSource]).parameter.symbol.name;
                }
                if(iTarget < targetSig.parameters.length && iTarget < targetVarArgCount) {
                    targetParamType = (targetSig.parameters[iTarget]).parameter.typeLink.type;
                    targetParamName = (targetSig.parameters[iTarget]).parameter.symbol.name;
                } else if(targetSig.hasVariableArgList && iTarget == targetVarArgCount) {
                    targetParamType = (targetSig.parameters[iTarget]).parameter.typeLink.type;
                    if(targetParamType.elementType) {
                        targetParamType = targetParamType.elementType;
                    }
                    targetParamName = (targetSig.parameters[iTarget]).parameter.symbol.name;
                }
                if(!(this.sourceIsRelatableToTarget(sourceParamType, targetParamType, assignableTo, comparisonCache, comparisonInfo) || this.sourceIsRelatableToTarget(targetParamType, sourceParamType, assignableTo, comparisonCache, comparisonInfo))) {
                    if(comparisonInfo) {
                        comparisonInfo.flags |= TypeScript.TypeRelationshipFlags.IncompatibleParameterTypes;
                    }
                    return false;
                }
            }
            return true;
        };
        return TypeChecker;
    })();
    TypeScript.TypeChecker = TypeChecker;    
})(TypeScript || (TypeScript = {}));
