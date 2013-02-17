var __extends = this.__extends || function (d, b) {
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var TypeScript;
(function (TypeScript) {
    var ScopedMembers = (function () {
        function ScopedMembers(dualMembers) {
            this.dualMembers = dualMembers;
            this.allMembers = this.dualMembers;
            this.publicMembers = this.dualMembers.primaryTable;
            this.privateMembers = this.dualMembers.secondaryTable;
        }
        ScopedMembers.prototype.addPublicMember = function (key, data) {
            return this.dualMembers.primaryTable.add(key, data);
        };
        ScopedMembers.prototype.addPrivateMember = function (key, data) {
            return this.dualMembers.secondaryTable.add(key, data);
        };
        return ScopedMembers;
    })();
    TypeScript.ScopedMembers = ScopedMembers;    
    (function (SymbolKind) {
        SymbolKind._map = [];
        SymbolKind._map[0] = "None";
        SymbolKind.None = 0;
        SymbolKind._map[1] = "Type";
        SymbolKind.Type = 1;
        SymbolKind._map[2] = "Field";
        SymbolKind.Field = 2;
        SymbolKind._map[3] = "Parameter";
        SymbolKind.Parameter = 3;
        SymbolKind._map[4] = "Variable";
        SymbolKind.Variable = 4;
    })(TypeScript.SymbolKind || (TypeScript.SymbolKind = {}));
    var SymbolKind = TypeScript.SymbolKind;
    var SymbolScope = (function () {
        function SymbolScope(container) {
            this.container = container;
        }
        SymbolScope.prototype.printLabel = function () {
            return "base";
        };
        SymbolScope.prototype.getAllSymbolNames = function (members) {
            return [
                "please", 
                "implement", 
                "in", 
                "derived", 
                "classes"
            ];
        };
        SymbolScope.prototype.getAllTypeSymbolNames = function (members) {
            return [
                "please", 
                "implement", 
                "in", 
                "derived", 
                "classes"
            ];
        };
        SymbolScope.prototype.getAllValueSymbolNames = function (members) {
            return [
                "please", 
                "implement", 
                "in", 
                "derived", 
                "classes"
            ];
        };
        SymbolScope.prototype.search = function (filter, name, publicOnly, typespace) {
            return null;
        };
        SymbolScope.prototype.findLocal = function (name, publicOnly, typespace) {
            return null;
        };
        SymbolScope.prototype.find = function (name, publicOnly, typespace) {
            return null;
        };
        SymbolScope.prototype.findImplementation = function (name, publicOnly, typespace) {
            return null;
        };
        SymbolScope.prototype.findAmbient = function (name, publicOnly, typespace) {
            return null;
        };
        SymbolScope.prototype.print = function (outfile) {
            if(this.container) {
                outfile.WriteLine(this.printLabel() + " scope with container: " + this.container.name + "...");
            } else {
                outfile.WriteLine(this.printLabel() + " scope...");
            }
        };
        SymbolScope.prototype.enter = function (container, ast, symbol, errorReporter, publicOnly, typespace, ambient) {
            throw new Error("please implement in derived class");
        };
        SymbolScope.prototype.getTable = function () {
            throw new Error("please implement in derived class");
        };
        return SymbolScope;
    })();
    TypeScript.SymbolScope = SymbolScope;    
    function symbolCanBeUsed(sym, publicOnly) {
        return publicOnly ? !(TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Private) || (sym.declAST && sym.declAST.nodeType == TypeScript.NodeType.FuncDecl && TypeScript.hasFlag((sym.declAST).fncFlags, TypeScript.FncFlags.Private))) : true;
    }
    var SymbolAggregateScope = (function (_super) {
        __extends(SymbolAggregateScope, _super);
        function SymbolAggregateScope(container) {
                _super.call(this, container);
            this.valueCache = null;
            this.valueImplCache = null;
            this.valueAmbientCache = null;
            this.typeCache = null;
            this.typeImplCache = null;
            this.typeAmbientCache = null;
            this.parents = null;
            this.container = container;
        }
        SymbolAggregateScope.prototype.printLabel = function () {
            return "agg";
        };
        SymbolAggregateScope.prototype.search = function (filter, name, publicOnly, typespace) {
            if(this.parents) {
                for(var i = 0; i < this.parents.length; i++) {
                    var sym = this.parents[i].search(filter, name, publicOnly, typespace);
                    if(sym) {
                        if(filter.update(sym)) {
                            return sym;
                        }
                    }
                }
            }
            return filter.result;
        };
        SymbolAggregateScope.prototype.getAllSymbolNames = function (members) {
            var result = [];
            if(this.parents) {
                for(var i = 0; i < this.parents.length; i++) {
                    var parentResult = this.parents[i].getAllSymbolNames(members);
                    if(parentResult) {
                        result = result.concat(parentResult);
                    }
                }
            }
            return result;
        };
        SymbolAggregateScope.prototype.getAllTypeSymbolNames = function (members) {
            var result = [];
            if(this.parents) {
                for(var i = 0; i < this.parents.length; i++) {
                    var parentResult = this.parents[i].getAllTypeSymbolNames(members);
                    if(parentResult) {
                        result = result.concat(parentResult);
                    }
                }
            }
            return result;
        };
        SymbolAggregateScope.prototype.getAllValueSymbolNames = function (members) {
            var result = [];
            if(this.parents) {
                for(var i = 0; i < this.parents.length; i++) {
                    var parentResult = this.parents[i].getAllValueSymbolNames(members);
                    if(parentResult) {
                        result = result.concat(parentResult);
                    }
                }
            }
            return result;
        };
        SymbolAggregateScope.prototype.print = function (outfile) {
            _super.prototype.print.call(this, outfile);
            if(this.parents) {
                for(var i = 0; i < this.parents.length; i++) {
                    this.parents[i].print(outfile);
                }
            }
        };
        SymbolAggregateScope.prototype.findImplementation = function (name, publicOnly, typespace) {
            var sym = null;
            var i = 0;
            var implCache = this.valueImplCache;
            if(typespace) {
                implCache = this.typeImplCache;
            }
            if(implCache && ((sym = implCache.lookup(name)) != null) && (publicOnly ? !(TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Private) || (sym.declAST && sym.declAST.nodeType == TypeScript.NodeType.FuncDecl && TypeScript.hasFlag((sym.declAST).fncFlags, TypeScript.FncFlags.Private))) : true)) {
                return sym;
            }
            if(this.parents) {
                for(i = 0; i < this.parents.length; i++) {
                    sym = this.parents[i].findImplementation(name, publicOnly, typespace);
                    if(sym) {
                        break;
                    }
                }
            }
            if(implCache) {
                if(typespace) {
                    this.typeImplCache = new TypeScript.StringHashTable();
                    implCache = this.typeImplCache;
                } else {
                    this.valueImplCache = new TypeScript.StringHashTable();
                    implCache = this.valueImplCache;
                }
            }
            implCache.add(name, sym);
            return sym;
        };
        SymbolAggregateScope.prototype.find = function (name, publicOnly, typespace) {
            var sym = null;
            var i = 0;
            var cache = this.valueCache;
            if(typespace) {
                cache = this.typeCache;
            }
            if(cache && ((sym = cache.lookup(name)) != null) && (publicOnly ? !(TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.Private) || (sym.declAST && sym.declAST.nodeType == TypeScript.NodeType.FuncDecl && TypeScript.hasFlag((sym.declAST).fncFlags, TypeScript.FncFlags.Private))) : true)) {
                return sym;
            }
            if(this.parents) {
                for(i = 0; i < this.parents.length; i++) {
                    sym = this.parents[i].find(name, publicOnly, typespace);
                    if(sym) {
                        break;
                    }
                }
            }
            if(cache == null) {
                if(typespace) {
                    this.typeCache = new TypeScript.StringHashTable();
                    cache = this.typeCache;
                } else {
                    this.valueCache = new TypeScript.StringHashTable();
                    cache = this.valueCache;
                }
            }
            cache.add(name, sym);
            return sym;
        };
        SymbolAggregateScope.prototype.findAmbient = function (name, publicOnly, typespace) {
            var sym = null;
            var i = 0;
            var cache = this.valueAmbientCache;
            if(typespace) {
                cache = this.typeAmbientCache;
            }
            if(cache && ((sym = cache.lookup(name)) != null)) {
                return sym;
            }
            if(this.parents) {
                for(i = 0; i < this.parents.length; i++) {
                    sym = this.parents[i].findAmbient(name, publicOnly, typespace);
                    if(sym) {
                        break;
                    }
                }
            }
            if(cache == null) {
                if(typespace) {
                    this.typeAmbientCache = new TypeScript.StringHashTable();
                    cache = this.typeAmbientCache;
                } else {
                    this.valueAmbientCache = new TypeScript.StringHashTable();
                    cache = this.valueAmbientCache;
                }
            }
            cache.add(name, sym);
            return sym;
        };
        SymbolAggregateScope.prototype.addParentScope = function (parent) {
            if(this.parents == null) {
                this.parents = new Array();
            }
            this.parents[this.parents.length] = parent;
        };
        return SymbolAggregateScope;
    })(SymbolScope);
    TypeScript.SymbolAggregateScope = SymbolAggregateScope;    
    var SymbolTableScope = (function (_super) {
        __extends(SymbolTableScope, _super);
        function SymbolTableScope(valueMembers, ambientValueMembers, enclosedTypes, ambientEnclosedTypes, container) {
                _super.call(this, container);
            this.valueMembers = valueMembers;
            this.ambientValueMembers = ambientValueMembers;
            this.enclosedTypes = enclosedTypes;
            this.ambientEnclosedTypes = ambientEnclosedTypes;
            this.container = container;
        }
        SymbolTableScope.prototype.printLabel = function () {
            return "table";
        };
        SymbolTableScope.prototype.getAllSymbolNames = function (members) {
            var result = this.getAllTypeSymbolNames(members);
            return result.concat(this.getAllValueSymbolNames(members));
        };
        SymbolTableScope.prototype.getAllTypeSymbolNames = function (members) {
            var result = [];
            if(this.ambientEnclosedTypes) {
                result = result.concat(this.ambientEnclosedTypes.allMembers.getAllKeys());
            }
            if(this.enclosedTypes) {
                result = result.concat(this.enclosedTypes.allMembers.getAllKeys());
            }
            return result;
        };
        SymbolTableScope.prototype.getAllValueSymbolNames = function (members) {
            var result = [];
            if(this.ambientValueMembers) {
                result = result.concat(this.ambientValueMembers.allMembers.getAllKeys());
            }
            if(this.valueMembers) {
                result = result.concat(this.valueMembers.allMembers.getAllKeys());
            }
            return result;
        };
        SymbolTableScope.prototype.search = function (filter, name, publicOnly, typespace) {
            var sym = this.find(name, publicOnly, typespace);
            filter.update(sym);
            return filter.result;
        };
        SymbolTableScope.prototype.find = function (name, publicOnly, typespace) {
            var table = null;
            var ambientTable = null;
            if(typespace) {
                table = (this.enclosedTypes == null) ? null : publicOnly ? this.enclosedTypes.publicMembers : this.enclosedTypes.allMembers;
                ambientTable = (this.ambientEnclosedTypes == null) ? null : publicOnly ? this.ambientEnclosedTypes.publicMembers : this.ambientEnclosedTypes.allMembers;
            } else {
                table = (this.valueMembers == null) ? null : publicOnly ? this.valueMembers.publicMembers : this.valueMembers.allMembers;
                ambientTable = (this.ambientValueMembers == null) ? null : publicOnly ? this.ambientValueMembers.publicMembers : this.ambientValueMembers.allMembers;
            }
            if(ambientTable) {
                var s = ambientTable.lookup(name);
                if(s) {
                    return s;
                }
            }
            if(table) {
                var s = table.lookup(name);
                if(s) {
                    return s;
                }
            }
            return null;
        };
        SymbolTableScope.prototype.findAmbient = function (name, publicOnly, typespace) {
            var ambientTable = (this.ambientValueMembers == null) ? null : publicOnly ? this.ambientValueMembers.publicMembers : this.ambientValueMembers.allMembers;
            if(typespace) {
                ambientTable = (this.ambientEnclosedTypes == null) ? null : publicOnly ? this.ambientEnclosedTypes.publicMembers : this.ambientEnclosedTypes.allMembers;
            }
            if(ambientTable) {
                var s = ambientTable.lookup(name);
                if(s) {
                    return s;
                }
            }
            return null;
        };
        SymbolTableScope.prototype.print = function (outfile) {
            _super.prototype.print.call(this, outfile);
            if(this.ambientValueMembers) {
                this.ambientValueMembers.allMembers.map(function (key, sym, context) {
                    outfile.WriteLine("  " + key);
                }, null);
            }
            if(this.valueMembers) {
                this.valueMembers.allMembers.map(function (key, sym, context) {
                    outfile.WriteLine("  " + key);
                }, null);
            }
            if(this.ambientEnclosedTypes) {
                this.ambientEnclosedTypes.allMembers.map(function (key, sym, context) {
                    outfile.WriteLine("  " + key);
                }, null);
            }
            if(this.enclosedTypes) {
                this.enclosedTypes.allMembers.map(function (key, sym, context) {
                    outfile.WriteLine("  " + key);
                }, null);
            }
        };
        SymbolTableScope.prototype.findImplementation = function (name, publicOnly, typespace) {
            var sym = this.find(name, publicOnly, typespace);
            if(sym) {
                if(sym.kind() == SymbolKind.Type) {
                    var typeSym = sym;
                    if(!typeSym.type.hasImplementation()) {
                        sym = null;
                    }
                } else if(sym.container) {
                    if(sym.container.kind() == SymbolKind.Type) {
                        var ctypeSym = sym.container;
                        if(!ctypeSym.type.hasImplementation()) {
                            sym = null;
                        }
                    }
                }
            }
            return sym;
        };
        SymbolTableScope.prototype.getTable = function () {
            return this.valueMembers.publicMembers;
        };
        return SymbolTableScope;
    })(SymbolScope);
    TypeScript.SymbolTableScope = SymbolTableScope;    
    var SymbolScopeBuilder = (function (_super) {
        __extends(SymbolScopeBuilder, _super);
        function SymbolScopeBuilder(valueMembers, ambientValueMembers, enclosedTypes, ambientEnclosedTypes, parent, container) {
                _super.call(this, container);
            this.valueMembers = valueMembers;
            this.ambientValueMembers = ambientValueMembers;
            this.enclosedTypes = enclosedTypes;
            this.ambientEnclosedTypes = ambientEnclosedTypes;
            this.parent = parent;
            this.container = container;
        }
        SymbolScopeBuilder.prototype.printLabel = function () {
            return "builder";
        };
        SymbolScopeBuilder.prototype.getAllSymbolNames = function (members) {
            var result = this.getAllTypeSymbolNames(members);
            return result.concat(this.getAllValueSymbolNames(members));
        };
        SymbolScopeBuilder.prototype.getAllTypeSymbolNames = function (members) {
            var result = [];
            if(this.ambientEnclosedTypes) {
                result = result.concat(this.ambientEnclosedTypes.allMembers.getAllKeys());
            }
            if(this.enclosedTypes) {
                result = result.concat(this.enclosedTypes.allMembers.getAllKeys());
            }
            if(!members && this.parent) {
                var parentResult = this.parent.getAllTypeSymbolNames(members);
                if(parentResult) {
                    result = result.concat(parentResult);
                }
            }
            return result;
        };
        SymbolScopeBuilder.prototype.getAllValueSymbolNames = function (members) {
            var result = [];
            if(this.ambientValueMembers) {
                result = result.concat(this.ambientValueMembers.allMembers.getAllKeys());
            }
            if(this.valueMembers) {
                result = result.concat(this.valueMembers.allMembers.getAllKeys());
            }
            if(!members && this.parent) {
                var parentResult = this.parent.getAllValueSymbolNames(members);
                if(parentResult) {
                    result = result.concat(parentResult);
                }
            }
            return result;
        };
        SymbolScopeBuilder.prototype.search = function (filter, name, publicOnly, typespace) {
            var sym = null;
            var table = (this.valueMembers == null) ? null : publicOnly ? this.valueMembers.publicMembers : this.valueMembers.allMembers;
            var ambientTable = (this.ambientValueMembers == null) ? null : publicOnly ? this.ambientValueMembers.publicMembers : this.ambientValueMembers.allMembers;
            if(typespace) {
                table = (this.enclosedTypes == null) ? null : publicOnly ? this.enclosedTypes.publicMembers : this.enclosedTypes.allMembers;
                ambientTable = (this.ambientEnclosedTypes == null) ? null : publicOnly ? this.ambientEnclosedTypes.publicMembers : this.ambientEnclosedTypes.allMembers;
            }
            if(ambientTable) {
                if((sym = ambientTable.lookup(name)) != null) {
                    if(filter.update(sym)) {
                        return sym;
                    }
                }
            }
            if(table) {
                if((sym = table.lookup(name)) != null) {
                    if(filter.update(sym)) {
                        return sym;
                    }
                }
            }
            if(this.parent) {
                sym = this.parent.search(filter, name, publicOnly, typespace);
                if(sym) {
                    if(filter.update(sym)) {
                        return sym;
                    }
                }
            }
            return filter.result;
        };
        SymbolScopeBuilder.prototype.print = function (outfile) {
            _super.prototype.print.call(this, outfile);
            if(this.ambientValueMembers) {
                this.ambientValueMembers.allMembers.map(function (key, s, context) {
                    var sym = s;
                    outfile.WriteLine("  " + key);
                }, null);
            }
            if(this.valueMembers) {
                this.valueMembers.allMembers.map(function (key, s, context) {
                    var sym = s;
                    outfile.WriteLine("  " + key);
                }, null);
            }
            if(this.ambientEnclosedTypes) {
                this.ambientEnclosedTypes.allMembers.map(function (key, s, context) {
                    var sym = s;
                    outfile.WriteLine("  " + key);
                }, null);
            }
            if(this.enclosedTypes) {
                this.enclosedTypes.allMembers.map(function (key, s, context) {
                    var sym = s;
                    outfile.WriteLine("  " + key);
                }, null);
            }
            if(this.parent) {
                this.parent.print(outfile);
            }
        };
        SymbolScopeBuilder.prototype.find = function (name, publicOnly, typespace) {
            var sym = null;
            var table = (this.valueMembers == null) ? null : publicOnly ? this.valueMembers.publicMembers : this.valueMembers.allMembers;
            var ambientTable = (this.ambientValueMembers == null) ? null : publicOnly ? this.ambientValueMembers.publicMembers : this.ambientValueMembers.allMembers;
            if(typespace) {
                table = (this.enclosedTypes == null) ? null : publicOnly ? this.enclosedTypes.publicMembers : this.enclosedTypes.allMembers;
                ambientTable = (this.ambientEnclosedTypes == null) ? null : publicOnly ? this.ambientEnclosedTypes.publicMembers : this.ambientEnclosedTypes.allMembers;
            }
            if(ambientTable && ((sym = ambientTable.lookup(name)) != null)) {
                return sym;
            }
            if(table && ((sym = table.lookup(name)) != null)) {
                return sym;
            }
            if(this.parent) {
                return this.parent.find(name, publicOnly, typespace);
            }
            return null;
        };
        SymbolScopeBuilder.prototype.findAmbient = function (name, publicOnly, typespace) {
            var sym = null;
            var ambientTable = (this.ambientValueMembers == null) ? null : publicOnly ? this.ambientValueMembers.publicMembers : this.ambientValueMembers.allMembers;
            if(typespace) {
                ambientTable = (this.ambientEnclosedTypes == null) ? null : publicOnly ? this.ambientEnclosedTypes.publicMembers : this.ambientEnclosedTypes.allMembers;
            }
            if(ambientTable && ((sym = ambientTable.lookup(name)) != null)) {
                return sym;
            }
            if(this.parent) {
                return this.parent.findAmbient(name, publicOnly, typespace);
            }
            return null;
        };
        SymbolScopeBuilder.prototype.findLocal = function (name, publicOnly, typespace) {
            var sym = null;
            var table = (this.valueMembers == null) ? null : publicOnly ? this.valueMembers.publicMembers : this.valueMembers.allMembers;
            var ambientTable = (this.ambientValueMembers == null) ? null : publicOnly ? this.ambientValueMembers.publicMembers : this.ambientValueMembers.allMembers;
            if(typespace) {
                table = (this.enclosedTypes == null) ? null : publicOnly ? this.enclosedTypes.publicMembers : this.enclosedTypes.allMembers;
                ambientTable = (this.ambientEnclosedTypes == null) ? null : publicOnly ? this.ambientEnclosedTypes.publicMembers : this.ambientEnclosedTypes.allMembers;
            }
            if(table) {
                if((sym = table.lookup(name)) != null) {
                    if(sym) {
                        return sym;
                    }
                }
            }
            if(ambientTable) {
                if((sym = ambientTable.lookup(name)) != null) {
                    if(sym) {
                        return sym;
                    }
                }
            }
            return null;
        };
        SymbolScopeBuilder.prototype.enter = function (container, ast, symbol, errorReporter, insertAsPublic, typespace, ambient) {
            var table = null;
            if(ambient) {
                if(typespace) {
                    table = (this.ambientEnclosedTypes == null) ? null : insertAsPublic ? this.ambientEnclosedTypes.publicMembers : this.ambientEnclosedTypes.privateMembers;
                } else {
                    table = (this.ambientValueMembers == null) ? null : insertAsPublic ? this.ambientValueMembers.publicMembers : this.ambientValueMembers.privateMembers;
                }
            } else {
                if(typespace) {
                    table = (this.enclosedTypes == null) ? null : insertAsPublic ? this.enclosedTypes.publicMembers : this.enclosedTypes.privateMembers;
                } else {
                    table = (this.valueMembers == null) ? null : insertAsPublic ? this.valueMembers.publicMembers : this.valueMembers.privateMembers;
                }
            }
            if(table) {
                if(!table.add(symbol.name, symbol)) {
                    errorReporter.duplicateIdentifier(ast, symbol.name);
                }
            } else {
                TypeScript.CompilerDiagnostics.Alert("YYYYY");
            }
            symbol.container = container;
        };
        SymbolScopeBuilder.prototype.getTable = function () {
            return this.valueMembers.allMembers;
        };
        return SymbolScopeBuilder;
    })(SymbolScope);
    TypeScript.SymbolScopeBuilder = SymbolScopeBuilder;    
    var FilteredSymbolScope = (function (_super) {
        __extends(FilteredSymbolScope, _super);
        function FilteredSymbolScope(scope, container, filter) {
                _super.call(this, container);
            this.scope = scope;
            this.filter = filter;
        }
        FilteredSymbolScope.prototype.print = function (outfile) {
            this.scope.print(outfile);
        };
        FilteredSymbolScope.prototype.find = function (name, publicOnly, typespace) {
            this.filter.reset();
            return this.scope.search(this.filter, name, publicOnly, typespace);
        };
        FilteredSymbolScope.prototype.findLocal = function (name, publicOnly, typespace) {
            return this.scope.findLocal(name, publicOnly, typespace);
        };
        return FilteredSymbolScope;
    })(SymbolScope);
    TypeScript.FilteredSymbolScope = FilteredSymbolScope;    
    var FilteredSymbolScopeBuilder = (function (_super) {
        __extends(FilteredSymbolScopeBuilder, _super);
        function FilteredSymbolScopeBuilder(valueMembers, parent, container, filter) {
                _super.call(this, valueMembers, null, null, null, parent, container);
            this.filter = filter;
        }
        FilteredSymbolScopeBuilder.prototype.findLocal = function (name, publicOnly, typespace) {
            var sym = _super.prototype.findLocal.call(this, name, publicOnly, typespace);
            if(sym) {
                if(!this.filter(sym)) {
                    return null;
                }
            }
            return sym;
        };
        FilteredSymbolScopeBuilder.prototype.search = function (filter, name, publicOnly, typespace) {
            throw new Error("please implement");
        };
        FilteredSymbolScopeBuilder.prototype.find = function (name, publicOnly, typespace) {
            var sym = _super.prototype.findLocal.call(this, name, publicOnly, typespace);
            if(sym) {
                if(!this.filter(sym)) {
                    return null;
                }
            }
            return _super.prototype.find.call(this, name, publicOnly, typespace);
        };
        return FilteredSymbolScopeBuilder;
    })(SymbolScopeBuilder);
    TypeScript.FilteredSymbolScopeBuilder = FilteredSymbolScopeBuilder;    
})(TypeScript || (TypeScript = {}));
