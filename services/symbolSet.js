var Services;
(function (Services) {
    var SymbolArraySet = (function () {
        function SymbolArraySet() {
            this.rtti = SymbolArraySet.rtti_id;
            this.values = [];
        }
        SymbolArraySet.rtti_id = {
            id: "Services.SymbolArraySet"
        };
        SymbolArraySet.prototype.add = function (sym) {
            if(this.contains(sym)) {
                return false;
            }
            this.values.push(sym);
            return true;
        };
        SymbolArraySet.prototype.contains = function (sym) {
            return this.values.indexOf(sym) >= 0;
        };
        SymbolArraySet.prototype.forEach = function (callback) {
            this.values.forEach(callback);
        };
        SymbolArraySet.prototype.getAll = function () {
            return this.values;
        };
        return SymbolArraySet;
    })();    
    var SymbolSet = (function () {
        function SymbolSet() {
            this.table = new TypeScript.StringHashTable();
        }
        SymbolSet.prototype.isSymbolArraySet = function (value) {
            return value.rtti === SymbolArraySet.rtti_id;
        };
        SymbolSet.prototype.add = function (sym) {
            var key = sym.name;
            var element = this.table.lookup(key);
            if(element === null) {
                this.table.add(key, sym);
                return true;
            } else if(this.isSymbolArraySet(element)) {
                return (element).add(sym);
            } else {
                var value = element;
                if(value === sym) {
                    return false;
                }
                var arraySet = new SymbolArraySet();
                arraySet.add(value);
                arraySet.add(sym);
                this.table.addOrUpdate(key, arraySet);
                return true;
            }
        };
        SymbolSet.prototype.contains = function (sym) {
            var key = sym.name;
            var element = this.table.lookup(key);
            if(element === null) {
                return false;
            } else if(this.isSymbolArraySet(element)) {
                return (element).contains(sym);
            } else {
                var value = element;
                return (value === sym);
            }
        };
        SymbolSet.prototype.isEmpty = function () {
            return this.table.count() === 0;
        };
        SymbolSet.prototype.getAll = function () {
            var result = [];
            this.forEach(function (x) {
                result.push(x);
            });
            return result;
        };
        SymbolSet.prototype.forEach = function (callback) {
            var _this = this;
            this.table.map(function (key, element, ctx) {
                if(element === null) {
                } else if(_this.isSymbolArraySet(element)) {
                    (element).forEach(callback);
                } else {
                    callback(element);
                }
            }, null);
        };
        SymbolSet.prototype.union = function (other) {
            var _this = this;
            other.getAll().forEach(function (x) {
                _this.add(x);
            });
        };
        return SymbolSet;
    })();
    Services.SymbolSet = SymbolSet;    
})(Services || (Services = {}));
