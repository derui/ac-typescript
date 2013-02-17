var Services;
(function (Services) {
    var OverridesCollector = (function () {
        function OverridesCollector(symbolTree) {
            this.symbolTree = symbolTree;
        }
        OverridesCollector.prototype.findMemberOverrides = function (memberSym) {
            return this.findMemberOverridesImpl(memberSym, true, true);
        };
        OverridesCollector.prototype.findImplementors = function (sym) {
            if(this.symbolTree.isClass(sym) || this.symbolTree.isInterface(sym)) {
                return this.symbolTree.findDerivedTypesTransitiveClosure(sym);
            } else if(this.symbolTree.isMethod(sym) || this.symbolTree.isField(sym)) {
                return this.findMemberOverridesImpl(sym, false, true);
            } else {
                return new Services.SymbolSet();
            }
        };
        OverridesCollector.prototype.findMemberOverridesImpl = function (memberSym, lookInBases, lookInDerived) {
            var _this = this;
            var result = new Services.SymbolSet();
            result.add(memberSym);
            if(memberSym.container === null) {
                return result;
            }
            var baseTypes = (lookInBases ? this.symbolTree.findBaseTypesTransitiveClosure(memberSym.container) : new Services.SymbolSet());
            var derivedTypes = (lookInDerived ? this.symbolTree.findDerivedTypesTransitiveClosure(memberSym.container) : new Services.SymbolSet());
            var allTypes = new Services.SymbolSet();
            allTypes.add(memberSym.container);
            allTypes.union(baseTypes);
            allTypes.union(derivedTypes);
            allTypes.getAll().forEach(function (x) {
                var override = _this.symbolTree.getOverride(x, memberSym);
                if(override !== null) {
                    result.add(override);
                }
            });
            return result;
        };
        return OverridesCollector;
    })();
    Services.OverridesCollector = OverridesCollector;    
})(Services || (Services = {}));
