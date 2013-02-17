var Services;
(function (Services) {
    var SymbolTree = (function () {
        function SymbolTree(host) {
            this.host = host;
            this._allTypes = null;
        }
        SymbolTree.prototype.findBaseTypesTransitiveClosure = function (sym) {
            var closure = new Services.SymbolSet();
            var lastSet = new Services.SymbolSet();
            lastSet.add(sym);
            while(!lastSet.isEmpty()) {
                closure.union(lastSet);
                lastSet = this.findBaseTypes(closure, lastSet);
            }
            return closure;
        };
        SymbolTree.prototype.findDerivedTypesTransitiveClosure = function (sym) {
            var closure = new Services.SymbolSet();
            var lastSet = new Services.SymbolSet();
            lastSet.add(sym);
            while(!lastSet.isEmpty()) {
                closure.union(lastSet);
                lastSet = this.findDerivedTypes(closure, lastSet);
            }
            return closure;
        };
        SymbolTree.prototype.getOverride = function (container, memberSym) {
            var members = null;
            if(this.isClass(container)) {
                members = container.type.instanceType.members;
            } else if(this.isInterface(container)) {
                members = container.type.members;
            }
            if(members == null) {
                return null;
            }
            var override = members.allMembers.lookup(memberSym.name);
            if(override == null) {
                return null;
            }
            if((this.isMethod(memberSym) === this.isMethod(override)) && (this.isField(memberSym) === this.isField(override)) && (this.isStatic(memberSym) === this.isStatic(override))) {
                return override;
            }
            return null;
        };
        SymbolTree.prototype.getAllTypes = function () {
            var _this = this;
            if(this._allTypes === null) {
                var result = new Services.SymbolSet();
                this.host.getScripts().forEach(function (script) {
                    TypeScript.walkAST(script, function (path, walker) {
                        if(path.isNameOfClass() || path.isNameOfInterface()) {
                            var sym = (path.ast()).sym;
                            if(sym != null) {
                                if(sym.kind() === TypeScript.SymbolKind.Type) {
                                    var typeSym = sym;
                                    if(_this.isClass(typeSym) || _this.isInterface(typeSym)) {
                                        result.add(typeSym);
                                    }
                                }
                            }
                        }
                        if(path.isBodyOfFunction()) {
                            walker.options.goChildren = false;
                        }
                    });
                });
                this._allTypes = result.getAll();
            }
            return this._allTypes;
        };
        SymbolTree.prototype.findBaseTypes = function (closure, lastSet) {
            var _this = this;
            var result = new Services.SymbolSet();
            var symsArray = lastSet.getAll();
            symsArray.forEach(function (sym) {
                if(sym.kind() === TypeScript.SymbolKind.Type) {
                    var type = (sym).type;
                    if(type !== null) {
                        if(type.instanceType != null) {
                            type = type.instanceType;
                        }
                        _this.addBaseTypes(closure, result, type.implementsList);
                        _this.addBaseTypes(closure, result, type.extendsList);
                    }
                }
            });
            return result;
        };
        SymbolTree.prototype.findDerivedTypes = function (alreadyFound, baseSymbols) {
            var _this = this;
            var result = new Services.SymbolSet();
            this.getAllTypes().forEach(function (candidate) {
                if(!alreadyFound.contains(candidate)) {
                    if(candidate.kind() === TypeScript.SymbolKind.Type) {
                        var type = (candidate).type;
                        if(type !== null) {
                            if(type.instanceType != null) {
                                type = type.instanceType;
                            }
                            var emptySet = new Services.SymbolSet();
                            var baseTypes = new Services.SymbolSet();
                            _this.addBaseTypes(emptySet, baseTypes, type.implementsList);
                            _this.addBaseTypes(emptySet, baseTypes, type.extendsList);
                            baseTypes.getAll().forEach(function (baseType) {
                                if(baseSymbols.contains(baseType)) {
                                    result.add(candidate);
                                }
                            });
                        }
                    }
                }
            });
            return result;
        };
        SymbolTree.prototype.addBaseTypes = function (closure, syms, bases) {
            var _this = this;
            if(bases == null) {
                return;
            }
            bases.forEach(function (base) {
                if(base.symbol !== null) {
                    if(!closure.contains(base.symbol)) {
                        if(_this.isDefinition(base.symbol)) {
                            syms.add(base.symbol);
                        }
                    }
                }
            });
        };
        SymbolTree.prototype.isDefinition = function (sym) {
            return this.isClass(sym) || this.isInterface(sym);
        };
        SymbolTree.prototype.isClass = function (sym) {
            return sym != null && sym.kind() == TypeScript.SymbolKind.Type && (sym).isClass();
        };
        SymbolTree.prototype.isInterface = function (sym) {
            return sym != null && sym.kind() == TypeScript.SymbolKind.Type && sym.declAST != null && sym.declAST.nodeType === TypeScript.NodeType.InterfaceDeclaration;
        };
        SymbolTree.prototype.isMethod = function (sym) {
            return sym != null && sym.kind() === TypeScript.SymbolKind.Type && (sym).isMethod;
        };
        SymbolTree.prototype.isField = function (sym) {
            return sym != null && sym.kind() === TypeScript.SymbolKind.Field;
        };
        SymbolTree.prototype.isStatic = function (sym) {
            return sym != null && sym.isStatic();
        };
        return SymbolTree;
    })();
    Services.SymbolTree = SymbolTree;    
})(Services || (Services = {}));
