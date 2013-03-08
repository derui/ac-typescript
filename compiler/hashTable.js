var TypeScript;
(function (TypeScript) {
    var BlockIntrinsics = (function () {
        function BlockIntrinsics() {
            this.prototype = undefined;
            this.toString = undefined;
            this.toLocaleString = undefined;
            this.valueOf = undefined;
            this.hasOwnProperty = undefined;
            this.propertyIsEnumerable = undefined;
            this.isPrototypeOf = undefined;
            this["constructor"] = undefined;
        }
        return BlockIntrinsics;
    })();
    TypeScript.BlockIntrinsics = BlockIntrinsics;    
    var StringHashTable = (function () {
        function StringHashTable() {
            this.itemCount = 0;
            this.table = (new BlockIntrinsics());
        }
        StringHashTable.prototype.getAllKeys = function () {
            var result = [];
            for(var k in this.table) {
                if(this.table[k] != undefined) {
                    result[result.length] = k;
                }
            }
            return result;
        };
        StringHashTable.prototype.add = function (key, data) {
            if(this.table[key] != undefined) {
                return false;
            }
            this.table[key] = data;
            this.itemCount++;
            return true;
        };
        StringHashTable.prototype.addOrUpdate = function (key, data) {
            if(this.table[key] != undefined) {
                this.table[key] = data;
                return false;
            }
            this.table[key] = data;
            this.itemCount++;
            return true;
        };
        StringHashTable.prototype.map = function (fn, context) {
            for(var k in this.table) {
                var data = this.table[k];
                if(data != undefined) {
                    fn(k, this.table[k], context);
                }
            }
        };
        StringHashTable.prototype.every = function (fn, context) {
            for(var k in this.table) {
                var data = this.table[k];
                if(data != undefined) {
                    if(!fn(k, this.table[k], context)) {
                        return false;
                    }
                }
            }
            return true;
        };
        StringHashTable.prototype.some = function (fn, context) {
            for(var k in this.table) {
                var data = this.table[k];
                if(data != undefined) {
                    if(fn(k, this.table[k], context)) {
                        return true;
                    }
                }
            }
            return false;
        };
        StringHashTable.prototype.count = function () {
            return this.itemCount;
        };
        StringHashTable.prototype.lookup = function (key) {
            var data = this.table[key];
            if(data != undefined) {
                return data;
            } else {
                return (null);
            }
        };
        return StringHashTable;
    })();
    TypeScript.StringHashTable = StringHashTable;    
    var DualStringHashTable = (function () {
        function DualStringHashTable(primaryTable, secondaryTable) {
            this.primaryTable = primaryTable;
            this.secondaryTable = secondaryTable;
            this.insertPrimary = true;
        }
        DualStringHashTable.prototype.getAllKeys = function () {
            return this.primaryTable.getAllKeys().concat(this.secondaryTable.getAllKeys());
        };
        DualStringHashTable.prototype.add = function (key, data) {
            if(this.insertPrimary) {
                return this.primaryTable.add(key, data);
            } else {
                return this.secondaryTable.add(key, data);
            }
        };
        DualStringHashTable.prototype.addOrUpdate = function (key, data) {
            if(this.insertPrimary) {
                return this.primaryTable.addOrUpdate(key, data);
            } else {
                return this.secondaryTable.addOrUpdate(key, data);
            }
        };
        DualStringHashTable.prototype.map = function (fn, context) {
            this.primaryTable.map(fn, context);
            this.secondaryTable.map(fn, context);
        };
        DualStringHashTable.prototype.every = function (fn, context) {
            return this.primaryTable.every(fn, context) && this.secondaryTable.every(fn, context);
        };
        DualStringHashTable.prototype.some = function (fn, context) {
            return this.primaryTable.some(fn, context) || this.secondaryTable.some(fn, context);
        };
        DualStringHashTable.prototype.count = function () {
            return this.primaryTable.count() + this.secondaryTable.count();
        };
        DualStringHashTable.prototype.lookup = function (key) {
            var data = this.primaryTable.lookup(key);
            if(data != undefined) {
                return data;
            } else {
                return this.secondaryTable.lookup(key);
            }
        };
        return DualStringHashTable;
    })();
    TypeScript.DualStringHashTable = DualStringHashTable;    
    function numberHashFn(key) {
        var c2 = 0x27d4eb2d;
        key = (key ^ 61) ^ (key >>> 16);
        key = key + (key << 3);
        key = key ^ (key >>> 4);
        key = key * c2;
        key = key ^ (key >>> 15);
        return key;
    }
    TypeScript.numberHashFn = numberHashFn;
    function combineHashes(key1, key2) {
        return key2 ^ ((key1 >> 5) + key1);
    }
    TypeScript.combineHashes = combineHashes;
    var HashEntry = (function () {
        function HashEntry(key, data) {
            this.key = key;
            this.data = data;
        }
        return HashEntry;
    })();
    TypeScript.HashEntry = HashEntry;    
    var HashTable = (function () {
        function HashTable(size, hashFn, equalsFn) {
            this.size = size;
            this.hashFn = hashFn;
            this.equalsFn = equalsFn;
            this.itemCount = 0;
            this.table = new Array();
            for(var i = 0; i < this.size; i++) {
                this.table[i] = null;
            }
        }
        HashTable.prototype.add = function (key, data) {
            var current;
            var entry = new HashEntry(key, data);
            var val = this.hashFn(key);
            val = val % this.size;
            for(current = this.table[val]; current != null; current = current.next) {
                if(this.equalsFn(key, current.key)) {
                    return false;
                }
            }
            entry.next = this.table[val];
            this.table[val] = entry;
            this.itemCount++;
            return true;
        };
        HashTable.prototype.remove = function (key) {
            var current;
            var val = this.hashFn(key);
            val = val % this.size;
            var result = null;
            var prevEntry = null;
            for(current = this.table[val]; current != null; current = current.next) {
                if(this.equalsFn(key, current.key)) {
                    result = current.data;
                    this.itemCount--;
                    if(prevEntry) {
                        prevEntry.next = current.next;
                    } else {
                        this.table[val] = current.next;
                    }
                    break;
                }
                prevEntry = current;
            }
            return result;
        };
        HashTable.prototype.count = function () {
            return this.itemCount;
        };
        HashTable.prototype.lookup = function (key) {
            var current;
            var val = this.hashFn(key);
            val = val % this.size;
            for(current = this.table[val]; current != null; current = current.next) {
                if(this.equalsFn(key, current.key)) {
                    return (current.data);
                }
            }
            return (null);
        };
        return HashTable;
    })();
    TypeScript.HashTable = HashTable;    
    var SimpleHashTable = (function () {
        function SimpleHashTable() {
            this.keys = [];
            this.values = [];
        }
        SimpleHashTable.prototype.lookup = function (key, findValue) {
            var searchArray = this.keys;
            if(findValue) {
                searchArray = this.values;
            }
            for(var i = 0; i < searchArray.length; i++) {
                if(searchArray[i] == key) {
                    return {
                        key: this.keys[i],
                        data: this.values[i]
                    };
                }
            }
            return null;
        };
        SimpleHashTable.prototype.add = function (key, data) {
            var lookupData = this.lookup(key);
            if(lookupData) {
                return false;
            }
            this.keys[this.keys.length] = key;
            this.values[this.values.length] = data;
            return true;
        };
        return SimpleHashTable;
    })();
    TypeScript.SimpleHashTable = SimpleHashTable;    
})(TypeScript || (TypeScript = {}));
