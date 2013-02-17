var __extends = this.__extends || function (d, b) {
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var TypeScript;
(function (TypeScript) {
    (function (Primitive) {
        Primitive._map = [];
        Primitive.None = 0;
        Primitive.Void = 1;
        Primitive.Double = 2;
        Primitive.String = 4;
        Primitive.Boolean = 8;
        Primitive.Any = 16;
        Primitive.Null = 32;
        Primitive.Undefined = 64;
    })(TypeScript.Primitive || (TypeScript.Primitive = {}));
    var Primitive = TypeScript.Primitive;
    var MemberName = (function () {
        function MemberName() {
            this.prefix = "";
            this.suffix = "";
        }
        MemberName.prototype.isString = function () {
            return false;
        };
        MemberName.prototype.isArray = function () {
            return false;
        };
        MemberName.prototype.toString = function () {
            return MemberName.memberNameToString(this);
        };
        MemberName.memberNameToString = function memberNameToString(memberName) {
            var result = memberName.prefix;
            if(memberName.isString()) {
                result += (memberName).text;
            } else {
                var ar = memberName;
                for(var index = 0; index < ar.entries.length; index++) {
                    result += MemberName.memberNameToString(ar.entries[index]);
                    result += ar.delim;
                }
            }
            result += memberName.suffix;
            return result;
        };
        MemberName.create = function create(arg1, arg2, arg3) {
            if(typeof arg1 == "string") {
                return new MemberNameString(arg1);
            } else {
                var result = new MemberNameArray();
                if(arg2) {
                    result.prefix = arg2;
                }
                if(arg3) {
                    result.suffix = arg3;
                }
                result.entries.push(arg1);
                return result;
            }
        };
        return MemberName;
    })();
    TypeScript.MemberName = MemberName;    
    var MemberNameString = (function (_super) {
        __extends(MemberNameString, _super);
        function MemberNameString(text) {
                _super.call(this);
            this.text = text;
        }
        MemberNameString.prototype.isString = function () {
            return true;
        };
        return MemberNameString;
    })(MemberName);
    TypeScript.MemberNameString = MemberNameString;    
    var MemberNameArray = (function (_super) {
        __extends(MemberNameArray, _super);
        function MemberNameArray() {
            _super.apply(this, arguments);

            this.delim = "";
            this.entries = [];
        }
        MemberNameArray.prototype.isArray = function () {
            return true;
        };
        MemberNameArray.prototype.add = function (entry) {
            this.entries.push(entry);
        };
        MemberNameArray.prototype.addAll = function (entries) {
            for(var i = 0; i < entries.length; i++) {
                this.entries.push(entries[i]);
            }
        };
        return MemberNameArray;
    })(MemberName);
    TypeScript.MemberNameArray = MemberNameArray;    
    var currentTypeID = -1;
    var Type = (function () {
        function Type() {
            this.typeID = currentTypeID++;
            this.construct = null;
            this.call = null;
            this.index = null;
            this.passTypeCreated = TypeScript.CompilerDiagnostics.analysisPass;
            this.primitiveTypeClass = Primitive.None;
            this.typeFlags = TypeScript.TypeFlags.None;
        }
        Type.prototype.baseClass = function () {
            if(this.extendsList && (this.extendsList.length > 0)) {
                return this.extendsList[0];
            } else {
                return null;
            }
        };
        Type.prototype.getArrayBase = function (arrInstType, checker) {
            return this.arrayCache.specialize(arrInstType, checker);
        };
        Type.prototype.isClass = function () {
            return this.instanceType != null;
        };
        Type.prototype.isArray = function () {
            return this.elementType != null;
        };
        Type.prototype.isClassInstance = function () {
            return this.symbol && !this.elementType && (this.symbol).type.isClass();
        };
        Type.prototype.getInstanceType = function () {
            if(this.isClass()) {
                return this.instanceType;
            } else {
                return this;
            }
        };
        Type.prototype.hasImplementation = function () {
            return TypeScript.hasFlag(this.typeFlags, TypeScript.TypeFlags.HasImplementation);
        };
        Type.prototype.setHasImplementation = function () {
            this.typeFlags |= TypeScript.TypeFlags.HasImplementation;
        };
        Type.prototype.isDouble = function () {
            return TypeScript.hasFlag(this.primitiveTypeClass, Primitive.Double);
        };
        Type.prototype.isString = function () {
            return TypeScript.hasFlag(this.primitiveTypeClass, Primitive.String);
        };
        Type.prototype.isBoolean = function () {
            return TypeScript.hasFlag(this.primitiveTypeClass, Primitive.Boolean);
        };
        Type.prototype.isNull = function () {
            return TypeScript.hasFlag(this.primitiveTypeClass, Primitive.Null);
        };
        Type.prototype.getTypeName = function () {
            return this.getMemberTypeName("", true, false, null);
        };
        Type.prototype.getScopedTypeName = function (scope) {
            return this.getMemberTypeName("", true, false, scope);
        };
        Type.prototype.getScopedTypeNameEx = function (scope) {
            return this.getMemberTypeNameEx("", true, false, scope);
        };
        Type.prototype.callCount = function () {
            var total = 0;
            if(this.call) {
                total += this.call.signatures.length;
            }
            if(this.construct) {
                total += this.construct.signatures.length;
            }
            if(this.index) {
                total += this.index.signatures.length;
            }
            return total;
        };
        Type.prototype.getMemberTypeName = function (prefix, topLevel, isElementType, scope) {
            var memberName = this.getMemberTypeNameEx(prefix, topLevel, isElementType, scope);
            return memberName.toString();
        };
        Type.prototype.getMemberTypeNameEx = function (prefix, topLevel, isElementType, scope) {
            if(this.elementType) {
                return MemberName.create(this.elementType.getMemberTypeNameEx(prefix, false, true, scope), "", "[]");
            } else if(this.symbol && this.symbol.name && this.symbol.name != "_anonymous" && (((this.call == null) && (this.construct == null) && (this.index == null)) || (TypeScript.hasFlag(this.typeFlags, TypeScript.TypeFlags.BuildingName)) || (this.members && (!this.isClass())))) {
                var tn = this.symbol.scopeRelativeName(scope);
                return MemberName.create(tn == "null" ? "any" : tn);
            } else {
                if(this.members || this.call || this.construct) {
                    if(TypeScript.hasFlag(this.typeFlags, TypeScript.TypeFlags.BuildingName)) {
                        return MemberName.create("this");
                    }
                    this.typeFlags |= TypeScript.TypeFlags.BuildingName;
                    var builder = "";
                    var allMemberNames = new MemberNameArray();
                    var curlies = isElementType || this.index != null;
                    var memCount = 0;
                    var delim = "; ";
                    if(this.members) {
                        this.members.allMembers.map(function (key, s, unused) {
                            var sym = s;
                            if(!TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.BuiltIn)) {
                                var typeNameMember = sym.getTypeNameEx(scope);
                                if(typeNameMember.isArray() && (typeNameMember).delim == delim) {
                                    allMemberNames.addAll((typeNameMember).entries);
                                } else {
                                    allMemberNames.add(typeNameMember);
                                }
                                memCount++;
                                curlies = true;
                            }
                        }, null);
                    }
                    var signatureCount = this.callCount();
                    var j;
                    var len = 0;
                    var shortform = !curlies && signatureCount == 1 && topLevel;
                    if(this.call) {
                        allMemberNames.addAll(this.call.toStrings(prefix, shortform, scope));
                    }
                    if(this.construct) {
                        allMemberNames.addAll(this.construct.toStrings("new", shortform, scope));
                    }
                    if(this.index) {
                        allMemberNames.addAll(this.index.toStrings("", shortform, scope));
                    }
                    if((curlies) || ((signatureCount > 1) && topLevel)) {
                        allMemberNames.prefix = "{ ";
                        allMemberNames.suffix = "}";
                        allMemberNames.delim = delim;
                    } else if(allMemberNames.entries.length > 1) {
                        allMemberNames.delim = delim;
                    }
                    this.typeFlags &= (~TypeScript.TypeFlags.BuildingName);
                    if((signatureCount == 0) && (memCount == 0)) {
                        return MemberName.create("{}");
                    } else {
                        return allMemberNames;
                    }
                } else {
                    return MemberName.create("{}");
                }
            }
        };
        Type.prototype.checkDecl = function (checker) {
            if(this.isClassInstance() || this.isClass()) {
                if(this.symbol.declAST) {
                    checker.typeFlow.inScopeTypeCheckDecl(this.symbol.declAST);
                }
            }
        };
        Type.prototype.getMemberScope = function (flow) {
            if(this == flow.anyType) {
                return null;
            } else if(this.isDouble()) {
                if(flow.numberInterfaceType) {
                    return flow.numberInterfaceType.memberScope;
                } else {
                    return null;
                }
            } else if(this.isBoolean()) {
                if(flow.booleanInterfaceType) {
                    return flow.booleanInterfaceType.memberScope;
                } else {
                    return null;
                }
            } else if(this == flow.stringType) {
                if(flow.stringInterfaceType) {
                    return flow.stringInterfaceType.memberScope;
                } else {
                    return null;
                }
            } else if(this.elementType) {
                if(flow.arrayInterfaceType) {
                    var arrInstType = this.elementType.getArrayBase(flow.arrayInterfaceType, flow.checker);
                    return arrInstType.memberScope;
                } else {
                    return null;
                }
            } else {
                return this.memberScope;
            }
        };
        Type.prototype.isReferenceType = function () {
            return this.members || this.extendsList || this.construct || this.call || this.index || this.elementType;
        };
        Type.prototype.specializeType = function (pattern, replacement, checker, membersOnly) {
            if(pattern == this) {
                return replacement;
            }
            var result = this;
            if(membersOnly) {
                if(this.isReferenceType()) {
                    result = new Type();
                    if(this.members) {
                        result.members = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
                        this.members.publicMembers.map(function (key, s, unused) {
                            var sym = s;
                            var bSym = sym.specializeType(pattern, replacement, checker);
                            result.members.addPublicMember(bSym.name, bSym);
                        }, null);
                        this.members.privateMembers.map(function (key, s, unused) {
                            var sym = s;
                            var bSym = sym.specializeType(pattern, replacement, checker);
                            result.members.addPrivateMember(bSym.name, bSym);
                        }, null);
                    }
                    if(this.ambientMembers) {
                        result.ambientMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
                        this.ambientMembers.publicMembers.map(function (key, s, unused) {
                            var sym = s;
                            var bSym = sym.specializeType(pattern, replacement, checker);
                            result.ambientMembers.addPublicMember(bSym.name, bSym);
                        }, null);
                        this.ambientMembers.privateMembers.map(function (key, s, unused) {
                            var sym = s;
                            var bSym = sym.specializeType(pattern, replacement, checker);
                            result.ambientMembers.addPrivateMember(bSym.name, bSym);
                        }, null);
                    }
                    result.containedScope = checker.scopeOf(result);
                    result.memberScope = result.containedScope;
                }
            } else {
                if(this.elementType) {
                    if(this.elementType == pattern) {
                        result = checker.makeArrayType(replacement);
                    } else {
                        if(this.elementType.elementType == pattern) {
                            result = checker.makeArrayType(checker.makeArrayType(replacement));
                        }
                    }
                } else if(this.call) {
                    result = new Type();
                    result.call = this.call.specializeType(pattern, replacement, checker);
                }
            }
            return result;
        };
        Type.prototype.hasBase = function (baseType) {
            if(baseType == this) {
                return true;
            } else {
                if(this.extendsList) {
                    for(var i = 0, len = this.extendsList.length; i < len; i++) {
                        if(this.extendsList[i].hasBase(baseType)) {
                            return true;
                        }
                    }
                }
            }
            return false;
        };
        Type.prototype.mergeOrdered = function (b, checker, acceptVoid, comparisonInfo) {
            if((this == checker.anyType) || (b == checker.anyType)) {
                return checker.anyType;
            } else if(this == b) {
                return this;
            } else if((b == checker.nullType) && this != checker.nullType) {
                return this;
            } else if((this == checker.nullType) && (b != checker.nullType)) {
                return b;
            } else if(acceptVoid && (b == checker.voidType) && this != checker.voidType) {
                return this;
            } else if(acceptVoid && (this == checker.voidType) && (b != checker.voidType)) {
                return b;
            } else if((b == checker.undefinedType) && this != checker.undefinedType) {
                return this;
            } else if((this == checker.undefinedType) && (b != checker.undefinedType)) {
                return b;
            } else if(this.elementType && b.elementType) {
                if(this.elementType == b.elementType) {
                    return this;
                } else {
                    var mergedET = this.elementType.mergeOrdered(b.elementType, checker, acceptVoid, comparisonInfo);
                    if(mergedET == null) {
                        return checker.makeArrayType(checker.anyType);
                    } else {
                        return checker.makeArrayType(mergedET);
                    }
                }
            } else if(checker.sourceIsSubtypeOfTarget(this, b, comparisonInfo)) {
                return b;
            } else if(checker.sourceIsSubtypeOfTarget(b, this, comparisonInfo)) {
                return this;
            } else {
                return null;
            }
        };
        Type.prototype.isModuleType = function () {
            return false;
        };
        Type.prototype.hasMembers = function () {
            return this.members != null;
        };
        Type.prototype.getAllEnclosedTypes = function () {
            return null;
        };
        Type.prototype.getAllAmbientEnclosedTypes = function () {
            return null;
        };
        Type.prototype.getPublicEnclosedTypes = function () {
            return null;
        };
        Type.prototype.getpublicAmbientEnclosedTypes = function () {
            return null;
        };
        Type.prototype.getDocComments = function () {
            if(this.elementType || !this.symbol) {
                return [];
            }
            if(this.isClassInstance() || this.isClass()) {
                if(this.symbol.declAST.nodeType == TypeScript.NodeType.FuncDecl) {
                    return (this.symbol.declAST).classDecl.getDocComments();
                } else {
                    return this.symbol.getDocComments();
                }
            }
            if(this.symbol.name && this.symbol.name != "_anonymous" && (((this.call == null) && (this.construct == null) && (this.index == null)) || this.members)) {
                return this.symbol.getDocComments();
            }
            return [];
        };
        return Type;
    })();
    TypeScript.Type = Type;    
    var ModuleType = (function (_super) {
        __extends(ModuleType, _super);
        function ModuleType(enclosedTypes, ambientEnclosedTypes) {
                _super.call(this);
            this.enclosedTypes = enclosedTypes;
            this.ambientEnclosedTypes = ambientEnclosedTypes;
            this.importedModules = [];
        }
        ModuleType.prototype.isModuleType = function () {
            return true;
        };
        ModuleType.prototype.hasMembers = function () {
            return this.members != null || this.enclosedTypes != null;
        };
        ModuleType.prototype.getAllEnclosedTypes = function () {
            return this.enclosedTypes;
        };
        ModuleType.prototype.getAllAmbientEnclosedTypes = function () {
            return this.ambientEnclosedTypes;
        };
        ModuleType.prototype.getPublicEnclosedTypes = function () {
            return null;
        };
        ModuleType.prototype.getpublicAmbientEnclosedTypes = function () {
            return null;
        };
        ModuleType.findDynamicModuleNameInHashTable = function findDynamicModuleNameInHashTable(moduleType, members) {
            var moduleName = null;
            members.map(function (key, s, c) {
                if(moduleName == null && !TypeScript.isQuoted(key)) {
                    var symbol = s;
                    var type = symbol.getType();
                    if(type == moduleType) {
                        moduleName = {
                            name: key,
                            symbol: symbol
                        };
                    }
                }
            }, null);
            return moduleName;
        };
        ModuleType.prototype.findDynamicModuleName = function (moduleType) {
            var moduleName = null;
            moduleName = ModuleType.findDynamicModuleNameInHashTable(moduleType, this.members.allMembers);
            if(moduleName == null) {
                moduleName = ModuleType.findDynamicModuleNameInHashTable(moduleType, this.ambientMembers.allMembers);
            }
            return moduleName;
        };
        return ModuleType;
    })(Type);
    TypeScript.ModuleType = ModuleType;    
    var TypeLink = (function () {
        function TypeLink() {
            this.type = null;
            this.ast = null;
        }
        return TypeLink;
    })();
    TypeScript.TypeLink = TypeLink;    
    function getTypeLink(ast, checker, autoVar) {
        var result = new TypeLink();
        result.ast = ast;
        if((ast == null) && (autoVar)) {
            result.type = checker.anyType;
        } else {
            result.type = null;
        }
        return result;
    }
    TypeScript.getTypeLink = getTypeLink;
})(TypeScript || (TypeScript = {}));
