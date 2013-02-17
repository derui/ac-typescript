var TypeScript;
(function (TypeScript) {
    var Binder = (function () {
        function Binder(checker) {
            this.checker = checker;
        }
        Binder.prototype.resolveBaseTypeLinks = function (typeLinks, scope) {
            var extendsList = null;
            if(typeLinks) {
                extendsList = new Array();
                for(var i = 0, len = typeLinks.length; i < len; i++) {
                    extendsList[i] = this.checker.resolveBaseTypeLink(typeLinks[i], scope);
                }
            }
            return extendsList;
        };
        Binder.prototype.resolveBases = function (scope, type) {
            type.extendsList = this.resolveBaseTypeLinks(type.extendsTypeLinks, scope);
            var i = 0, len = type.extendsList.length;
            var derivedIsClass = type.isClassInstance();
            for(; i < len; i++) {
                var baseIsClass = type.extendsList[i].isClassInstance();
                if(type.extendsList[i] != this.checker.anyType) {
                    var baseRef = type.extendsTypeLinks[i].ast;
                    if(derivedIsClass) {
                        if(!baseIsClass) {
                            this.checker.errorReporter.simpleError(baseRef, "A class may only extend other classes, " + type.extendsList[i].symbol.fullName() + " is not a class.");
                        }
                    } else {
                        if(baseIsClass) {
                            this.checker.errorReporter.simpleError(baseRef, "An interface may only extend other interfaces, " + type.extendsList[i].symbol.fullName() + " is a class.");
                        }
                    }
                }
            }
            type.implementsList = this.resolveBaseTypeLinks(type.implementsTypeLinks, scope);
            if(type.implementsList) {
                for(i = 0 , len = type.implementsList.length; i < len; i++) {
                    var iface = type.implementsList[i];
                    var baseRef = type.implementsTypeLinks[i].ast;
                    if(iface.isClassInstance()) {
                        if(derivedIsClass) {
                            this.checker.errorReporter.simpleError(baseRef, "A class may only implement an interface; " + iface.symbol.fullName() + " is a class.");
                        }
                    }
                }
            }
        };
        Binder.prototype.resolveSignatureGroup = function (signatureGroup, scope, instanceType) {
            var supplyVar = !(signatureGroup.hasImplementation);
            for(var i = 0, len = signatureGroup.signatures.length; i < len; i++) {
                var signature = signatureGroup.signatures[i];
                if(instanceType) {
                    signature.returnType.type = instanceType;
                } else {
                    this.checker.resolveTypeLink(scope, signature.returnType, supplyVar);
                }
                var paramLen = signature.parameters.length;
                for(var j = 0; j < paramLen; j++) {
                    this.bindSymbol(scope, signature.parameters[j]);
                }
                if(signature.hasVariableArgList) {
                    var lastParam = signature.parameters[paramLen - 1];
                    lastParam.argsOffset = paramLen - 1;
                    if(!lastParam.getType().isArray()) {
                        this.checker.errorReporter.simpleErrorFromSym(lastParam, "... parameter must have array type");
                        lastParam.parameter.typeLink.type = this.checker.makeArrayType(lastParam.parameter.typeLink.type);
                    }
                }
            }
        };
        Binder.prototype.bindType = function (scope, type, instanceType) {
            if(instanceType) {
                this.bindType(scope, instanceType, null);
            }
            if(type.hasMembers()) {
                var members = type.members;
                var ambientMembers = type.ambientMembers;
                var typeMembers = type.getAllEnclosedTypes();
                var ambientTypeMembers = type.getAllAmbientEnclosedTypes();
                var memberScope = new TypeScript.SymbolTableScope(members, ambientMembers, typeMembers, ambientTypeMembers, type.symbol);
                var agg = new TypeScript.SymbolAggregateScope(type.symbol);
                var prevCurrentModDecl = this.checker.currentModDecl;
                var prevBindStatus = this.checker.inBind;
                agg.addParentScope(memberScope);
                agg.addParentScope(scope);
                if(type.isModuleType()) {
                    this.checker.currentModDecl = type.symbol.declAST;
                    this.checker.inBind = true;
                }
                if(members) {
                    this.bind(agg, type.members.allMembers);
                }
                if(typeMembers) {
                    this.bind(agg, typeMembers.allMembers);
                }
                if(ambientMembers) {
                    this.bind(agg, ambientMembers.allMembers);
                }
                if(ambientTypeMembers) {
                    this.bind(agg, ambientTypeMembers.allMembers);
                }
                this.checker.currentModDecl = prevCurrentModDecl;
                this.checker.inBind = prevBindStatus;
            }
            if(type.extendsTypeLinks) {
                this.resolveBases(scope, type);
            }
            if(type.construct) {
                this.resolveSignatureGroup(type.construct, scope, instanceType);
            }
            if(type.call) {
                this.resolveSignatureGroup(type.call, scope, null);
            }
            if(type.index) {
                this.resolveSignatureGroup(type.index, scope, null);
            }
            if(type.elementType) {
                this.bindType(scope, type.elementType, null);
            }
        };
        Binder.prototype.bindSymbol = function (scope, symbol) {
            if(!symbol.bound) {
                var prevLocationInfo = this.checker.locationInfo;
                if((this.checker.units) && (symbol.unitIndex >= 0) && (symbol.unitIndex < this.checker.units.length)) {
                    this.checker.locationInfo = this.checker.units[symbol.unitIndex];
                }
                switch(symbol.kind()) {
                    case TypeScript.SymbolKind.Type:
                        if(symbol.flags & TypeScript.SymbolFlags.Bound) {
                            break;
                        }
                        var typeSymbol = symbol;
                        typeSymbol.flags |= TypeScript.SymbolFlags.Bound;
                        if(typeSymbol.aliasLink && !typeSymbol.type && typeSymbol.aliasLink.alias.nodeType == TypeScript.NodeType.Name) {
                            var modPath = (typeSymbol.aliasLink.alias).text;
                            var modSym = this.checker.findSymbolForDynamicModule(modPath, this.checker.locationInfo.filename, function (id) {
                                return scope.find(id, false, true);
                            });
                            if(modSym) {
                                typeSymbol.type = modSym.getType();
                            }
                        }
                        if(typeSymbol.type && typeSymbol.type != this.checker.gloModType) {
                            this.bindType(scope, typeSymbol.type, typeSymbol.instanceType);
                            if(typeSymbol.type.isModuleType()) {
                                for(var i = 0; i < typeSymbol.expansions.length; i++) {
                                    this.bindType(scope, typeSymbol.expansions[i], typeSymbol.instanceType);
                                }
                            }
                        }
                        break;
                    case TypeScript.SymbolKind.Field:
                        this.checker.resolveTypeLink(scope, (symbol).field.typeLink, false);
                        break;
                    case TypeScript.SymbolKind.Parameter:
                        this.checker.resolveTypeLink(scope, (symbol).parameter.typeLink, true);
                        break;
                }
                this.checker.locationInfo = prevLocationInfo;
            }
            symbol.bound = true;
        };
        Binder.prototype.bind = function (scope, table) {
            table.map(function (key, sym, binder) {
                binder.bindSymbol(scope, sym);
            }, this);
        };
        return Binder;
    })();
    TypeScript.Binder = Binder;    
})(TypeScript || (TypeScript = {}));
