var __extends = this.__extends || function (d, b) {
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var TypeScript;
(function (TypeScript) {
    (function (TypeCheckStatus) {
        TypeCheckStatus._map = [];
        TypeCheckStatus._map[0] = "NotStarted";
        TypeCheckStatus.NotStarted = 0;
        TypeCheckStatus._map[1] = "Started";
        TypeCheckStatus.Started = 1;
        TypeCheckStatus._map[2] = "Finished";
        TypeCheckStatus.Finished = 2;
    })(TypeScript.TypeCheckStatus || (TypeScript.TypeCheckStatus = {}));
    var TypeCheckStatus = TypeScript.TypeCheckStatus;
    function aLexicallyEnclosesB(a, b) {
        if(a.declAST && b && b.declAST && a.declAST.nodeType == TypeScript.NodeType.FuncDecl) {
            return a.declAST.minChar <= b.declAST.minChar && a.declAST.limChar >= b.declAST.limChar;
        } else {
            return false;
        }
    }
    TypeScript.aLexicallyEnclosesB = aLexicallyEnclosesB;
    function aEnclosesB(a, b) {
        while(a.container) {
            if(a == b || aLexicallyEnclosesB(a.container, b)) {
                return true;
            }
            a = a.container;
        }
        return false;
    }
    TypeScript.aEnclosesB = aEnclosesB;
    var Symbol = (function () {
        function Symbol(name, location, length, unitIndex) {
            this.name = name;
            this.location = location;
            this.length = length;
            this.unitIndex = unitIndex;
            this.bound = false;
            this.flags = TypeScript.SymbolFlags.None;
            this.isObjectLitField = false;
            this.declAST = null;
            this.declModule = null;
            this.passSymbolCreated = TypeScript.CompilerDiagnostics.analysisPass;
        }
        Symbol.prototype.instanceScope = function () {
            return null;
        };
        Symbol.prototype.isVariable = function () {
            return false;
        };
        Symbol.prototype.isMember = function () {
            return false;
        };
        Symbol.prototype.isInferenceSymbol = function () {
            return false;
        };
        Symbol.prototype.isWith = function () {
            return false;
        };
        Symbol.prototype.writeable = function () {
            return false;
        };
        Symbol.prototype.isType = function () {
            return false;
        };
        Symbol.prototype.getType = function () {
            return null;
        };
        Symbol.prototype.isAccessor = function () {
            return false;
        };
        Symbol.prototype.isInstanceProperty = function () {
            return TypeScript.hasFlag(this.flags, TypeScript.SymbolFlags.Property) && (!TypeScript.hasFlag(this.flags, TypeScript.SymbolFlags.ModuleMember));
        };
        Symbol.prototype.getTypeName = function (scope) {
            return this.getTypeNameEx(scope).toString();
        };
        Symbol.prototype.getTypeNameEx = function (scope) {
            return TypeScript.MemberName.create(this.toString());
        };
        Symbol.prototype.getOptionalNameString = function () {
            return TypeScript.hasFlag(this.flags, TypeScript.SymbolFlags.Optional) ? "?" : "";
        };
        Symbol.prototype.pathToRoot = function () {
            var path = new Array();
            var node = this;
            while(node && (node.name != TypeScript.globalId)) {
                path[path.length] = node;
                node = node.container;
            }
            return path;
        };
        Symbol.prototype.findCommonAncestorPath = function (b) {
            if(this.container == null) {
                return new Array();
            }
            var aPath = this.container.pathToRoot();
            var bPath;
            if(b) {
                bPath = b.pathToRoot();
            } else {
                bPath = new Array();
            }
            var commonNodeIndex = -1;
            for(var i = 0, aLen = aPath.length; i < aLen; i++) {
                var aNode = aPath[i];
                for(var j = 0, bLen = bPath.length; j < bLen; j++) {
                    var bNode = bPath[j];
                    if(aNode == bNode) {
                        commonNodeIndex = i;
                        break;
                    }
                }
                if(commonNodeIndex >= 0) {
                    break;
                }
            }
            if(commonNodeIndex >= 0) {
                return aPath.slice(0, commonNodeIndex);
            } else {
                return aPath;
            }
        };
        Symbol.prototype.getPrettyName = function (scopeSymbol) {
            return this.name;
        };
        Symbol.prototype.scopeRelativeName = function (scope) {
            if(scope == null) {
                return this.getPrettyName(null) + this.getOptionalNameString();
            }
            var lca = this.findCommonAncestorPath(scope.container);
            var builder = "";
            for(var i = 0, len = lca.length; i < len; i++) {
                var prettyName = lca[i].getPrettyName(i == len - 1 ? scope.container : lca[i + 1]);
                builder = prettyName + "." + builder;
            }
            builder += this.getPrettyName(len == 0 ? scope.container : lca[0]) + this.getOptionalNameString();
            return builder;
        };
        Symbol.prototype.fullName = function () {
            var builder = this.name;
            var ancestor = this.container;
            while(ancestor && (ancestor.name != TypeScript.globalId)) {
                builder = ancestor.name + "." + builder;
                ancestor = ancestor.container;
            }
            return builder;
        };
        Symbol.prototype.isExternallyVisible = function (checker) {
            if(this == checker.gloMod) {
                return true;
            }
            if(TypeScript.hasFlag(this.flags, TypeScript.SymbolFlags.Private)) {
                return false;
            }
            if(!TypeScript.hasFlag(this.flags, TypeScript.SymbolFlags.Exported)) {
                return this.container == checker.gloMod;
            }
            return this.container.isExternallyVisible(checker);
        };
        Symbol.prototype.visible = function (scope, checker) {
            if(checker == null || this.container == checker.gloMod) {
                return true;
            }
            if(TypeScript.hasFlag(this.flags, TypeScript.SymbolFlags.ModuleMember)) {
                if(TypeScript.hasFlag(this.flags, TypeScript.SymbolFlags.Exported)) {
                    if(!TypeScript.hasFlag(this.flags, TypeScript.SymbolFlags.Private)) {
                        return true;
                    } else {
                        return aEnclosesB(this, scope.container);
                    }
                } else {
                    return checker && (checker.currentModDecl == this.declModule) || (checker.currentModDecl && checker.currentModDecl.mod && checker.currentModDecl.mod.symbol && this.declModule && this.declModule.mod && this.declModule.mod.symbol && aEnclosesB(checker.currentModDecl.mod.symbol, this.declModule.mod.symbol));
                }
            } else {
                var isFunction = this.declAST && this.declAST.nodeType == TypeScript.NodeType.FuncDecl;
                var isMethod = isFunction && (this.declAST).isMethod();
                var isStaticFunction = isFunction && TypeScript.hasFlag((this.declAST).fncFlags, TypeScript.FncFlags.Static);
                var isPrivateMethod = isMethod && TypeScript.hasFlag((this.declAST).fncFlags, TypeScript.FncFlags.Private);
                var isAlias = this.isType() && (this).aliasLink;
                if(this.isMember() || isMethod || isStaticFunction || isAlias) {
                    if(TypeScript.hasFlag(this.flags, TypeScript.SymbolFlags.Private) || isPrivateMethod) {
                        if(scope.container == null && this.container != scope.container) {
                            return false;
                        } else {
                            return this.container == null ? true : aEnclosesB(scope.container, this.container);
                        }
                    } else {
                        return true;
                    }
                } else if(this.container) {
                    return aEnclosesB(this, scope.container);
                } else {
                    return true;
                }
            }
        };
        Symbol.prototype.addRef = function (identifier) {
            if(!this.refs) {
                this.refs = [];
            }
            this.refs[this.refs.length] = identifier;
        };
        Symbol.prototype.toString = function () {
            if(this.name) {
                return this.name;
            } else {
                return "_anonymous";
            }
        };
        Symbol.prototype.print = function (outfile) {
            outfile.Write(this.toString());
        };
        Symbol.prototype.specializeType = function (pattern, replacement, checker) {
            throw new Error("please implement in derived class");
        };
        Symbol.prototype.setType = function (type) {
            throw new Error("please implement in derived class");
        };
        Symbol.prototype.kind = function () {
            throw new Error("please implement in derived class");
        };
        Symbol.prototype.getInterfaceDeclFromSymbol = function (checker) {
            if(this.declAST != null) {
                if(this.declAST.nodeType == TypeScript.NodeType.InterfaceDeclaration) {
                    return this.declAST;
                } else if(this.container != null && this.container != checker.gloMod && this.container.declAST.nodeType == TypeScript.NodeType.InterfaceDeclaration) {
                    return this.container.declAST;
                }
            }
            return null;
        };
        Symbol.prototype.getVarDeclFromSymbol = function () {
            if(this.declAST != null && this.declAST.nodeType == TypeScript.NodeType.VarDecl) {
                return this.declAST;
            }
            return null;
        };
        Symbol.prototype.getDocComments = function () {
            if(this.declAST != null) {
                return this.declAST.getDocComments();
            }
            return [];
        };
        Symbol.prototype.isStatic = function () {
            return TypeScript.hasFlag(this.flags, TypeScript.SymbolFlags.Static);
        };
        return Symbol;
    })();
    TypeScript.Symbol = Symbol;    
    var ValueLocation = (function () {
        function ValueLocation() { }
        return ValueLocation;
    })();
    TypeScript.ValueLocation = ValueLocation;    
    var InferenceSymbol = (function (_super) {
        __extends(InferenceSymbol, _super);
        function InferenceSymbol(name, location, length, unitIndex) {
                _super.call(this, name, location, length, unitIndex);
            this.typeCheckStatus = TypeCheckStatus.NotStarted;
        }
        InferenceSymbol.prototype.isInferenceSymbol = function () {
            return true;
        };
        InferenceSymbol.prototype.transferVarFlags = function (varFlags) {
            if(TypeScript.hasFlag(varFlags, TypeScript.VarFlags.Ambient)) {
                this.flags |= TypeScript.SymbolFlags.Ambient;
            }
            if(TypeScript.hasFlag(varFlags, TypeScript.VarFlags.Constant)) {
                this.flags |= TypeScript.SymbolFlags.Constant;
            }
            if(TypeScript.hasFlag(varFlags, TypeScript.VarFlags.Static)) {
                this.flags |= TypeScript.SymbolFlags.Static;
            }
            if(TypeScript.hasFlag(varFlags, TypeScript.VarFlags.Property)) {
                this.flags |= TypeScript.SymbolFlags.Property;
            }
            if(TypeScript.hasFlag(varFlags, TypeScript.VarFlags.Private)) {
                this.flags |= TypeScript.SymbolFlags.Private;
            }
            if(TypeScript.hasFlag(varFlags, TypeScript.VarFlags.Public)) {
                this.flags |= TypeScript.SymbolFlags.Public;
            }
            if(TypeScript.hasFlag(varFlags, TypeScript.VarFlags.Readonly)) {
                this.flags |= TypeScript.SymbolFlags.Readonly;
            }
            if(TypeScript.hasFlag(varFlags, TypeScript.VarFlags.Exported)) {
                this.flags |= TypeScript.SymbolFlags.Exported;
            }
        };
        return InferenceSymbol;
    })(Symbol);
    TypeScript.InferenceSymbol = InferenceSymbol;    
    var TypeSymbol = (function (_super) {
        __extends(TypeSymbol, _super);
        function TypeSymbol(locName, location, length, unitIndex, type) {
                _super.call(this, locName, location, length, unitIndex);
            this.type = type;
            this.expansions = [];
            this.expansionsDeclAST = [];
            this.isDynamic = false;
            this.isMethod = false;
            this.aliasLink = null;
            this.onlyReferencedAsTypeRef = TypeScript.optimizeModuleCodeGen;
            this.prettyName = this.name;
        }
        TypeSymbol.prototype.addLocation = function (loc) {
            if(this.additionalLocations == null) {
                this.additionalLocations = [];
            }
            this.additionalLocations[this.additionalLocations.length] = loc;
        };
        TypeSymbol.prototype.kind = function () {
            return TypeScript.SymbolKind.Type;
        };
        TypeSymbol.prototype.isType = function () {
            return true;
        };
        TypeSymbol.prototype.getType = function () {
            return this.type;
        };
        TypeSymbol.prototype.getTypeNameEx = function (scope) {
            return this.type.getMemberTypeNameEx(this.name ? this.name + this.getOptionalNameString() : "", false, false, scope);
        };
        TypeSymbol.prototype.instanceScope = function () {
            if(!(this.type.typeFlags & TypeScript.TypeFlags.IsClass) && this.type.isClass()) {
                return this.type.instanceType.constructorScope;
            } else {
                return this.type.containedScope;
            }
        };
        TypeSymbol.prototype.toString = function () {
            var result = this.type.getTypeName();
            if(this.name) {
                result = this.name + ":" + result;
            }
            return result;
        };
        TypeSymbol.prototype.isClass = function () {
            return this.instanceType != null;
        };
        TypeSymbol.prototype.isFunction = function () {
            return this.declAST != null && this.declAST.nodeType == TypeScript.NodeType.FuncDecl;
        };
        TypeSymbol.prototype.specializeType = function (pattern, replacement, checker) {
            if(this.type == pattern) {
                return replacement.symbol;
            } else {
                var replType = this.type.specializeType(pattern, replacement, checker, false);
                if(replType != this.type) {
                    var result = new TypeSymbol(this.name, -1, 0, -1, replType);
                    return result;
                } else {
                    return this;
                }
            }
        };
        TypeSymbol.prototype.getPrettyName = function (scopeSymbol) {
            if(!!scopeSymbol && TypeScript.isQuoted(this.prettyName) && this.type.isModuleType()) {
                var symbolPath = scopeSymbol.pathToRoot();
                var prettyName = this.getPrettyNameOfDynamicModule(symbolPath);
                if(prettyName != null) {
                    return prettyName.name;
                }
            }
            return this.prettyName;
        };
        TypeSymbol.prototype.getPrettyNameOfDynamicModule = function (scopeSymbolPath) {
            var scopeSymbolPathLength = scopeSymbolPath.length;
            var externalSymbol = null;
            if(scopeSymbolPath.length > 0 && scopeSymbolPath[scopeSymbolPathLength - 1].getType().isModuleType() && (scopeSymbolPath[scopeSymbolPathLength - 1]).isDynamic) {
                if(scopeSymbolPathLength > 1 && scopeSymbolPath[scopeSymbolPathLength - 2].getType().isModuleType() && (scopeSymbolPath[scopeSymbolPathLength - 2]).isDynamic) {
                    var moduleType = scopeSymbolPath[scopeSymbolPathLength - 2].getType();
                    externalSymbol = moduleType.findDynamicModuleName(this.type);
                }
                if(externalSymbol == null) {
                    var moduleType = scopeSymbolPath[scopeSymbolPathLength - 1].getType();
                    externalSymbol = moduleType.findDynamicModuleName(this.type);
                }
            }
            return externalSymbol;
        };
        TypeSymbol.prototype.getDocComments = function () {
            var comments = [];
            if(this.declAST != null) {
                comments = comments.concat(this.declAST.getDocComments());
            }
            for(var i = 0; i < this.expansionsDeclAST.length; i++) {
                comments = comments.concat(this.expansionsDeclAST[i].getDocComments());
            }
            return comments;
        };
        return TypeSymbol;
    })(InferenceSymbol);
    TypeScript.TypeSymbol = TypeSymbol;    
    var WithSymbol = (function (_super) {
        __extends(WithSymbol, _super);
        function WithSymbol(location, unitIndex, withType) {
                _super.call(this, "with", location, 4, unitIndex, withType);
        }
        WithSymbol.prototype.isWith = function () {
            return true;
        };
        return WithSymbol;
    })(TypeSymbol);
    TypeScript.WithSymbol = WithSymbol;    
    var FieldSymbol = (function (_super) {
        __extends(FieldSymbol, _super);
        function FieldSymbol(name, location, unitIndex, canWrite, field) {
                _super.call(this, name, location, name.length, unitIndex);
            this.canWrite = canWrite;
            this.field = field;
            this.getter = null;
            this.setter = null;
            this.hasBeenEmitted = false;
            this.name = name;
            this.location = location;
        }
        FieldSymbol.prototype.kind = function () {
            return TypeScript.SymbolKind.Field;
        };
        FieldSymbol.prototype.writeable = function () {
            return this.isAccessor() ? this.setter != null : this.canWrite;
        };
        FieldSymbol.prototype.getType = function () {
            return this.field.typeLink.type;
        };
        FieldSymbol.prototype.getTypeNameEx = function (scope) {
            return TypeScript.MemberName.create(this.field.typeLink.type.getScopedTypeNameEx(scope), this.name + this.getOptionalNameString() + ": ", "");
        };
        FieldSymbol.prototype.isMember = function () {
            return true;
        };
        FieldSymbol.prototype.setType = function (type) {
            this.field.typeLink.type = type;
        };
        FieldSymbol.prototype.isAccessor = function () {
            return this.getter != null || this.setter != null;
        };
        FieldSymbol.prototype.isVariable = function () {
            return true;
        };
        FieldSymbol.prototype.toString = function () {
            return this.getTypeNameEx(null).toString();
        };
        FieldSymbol.prototype.specializeType = function (pattern, replacement, checker) {
            var rType = this.field.typeLink.type.specializeType(pattern, replacement, checker, false);
            if(rType != this.field.typeLink.type) {
                var fieldDef = new ValueLocation();
                var result = new FieldSymbol(this.name, 0, checker.locationInfo.unitIndex, this.canWrite, fieldDef);
                result.flags = this.flags;
                fieldDef.symbol = result;
                fieldDef.typeLink = new TypeScript.TypeLink();
                result.setType(rType);
                result.typeCheckStatus = TypeCheckStatus.Finished;
                return result;
            } else {
                return this;
            }
        };
        FieldSymbol.prototype.getDocComments = function () {
            if(this.getter != null || this.setter != null) {
                var comments = [];
                if(this.getter != null) {
                    comments = comments.concat(this.getter.getDocComments());
                }
                if(this.setter != null) {
                    comments = comments.concat(this.setter.getDocComments());
                }
                return comments;
            } else if(this.declAST != null) {
                return this.declAST.getDocComments();
            }
            return [];
        };
        return FieldSymbol;
    })(InferenceSymbol);
    TypeScript.FieldSymbol = FieldSymbol;    
    var ParameterSymbol = (function (_super) {
        __extends(ParameterSymbol, _super);
        function ParameterSymbol(name, location, unitIndex, parameter) {
                _super.call(this, name, location, name.length, unitIndex);
            this.parameter = parameter;
            this.paramDocComment = null;
            this.funcDecl = null;
            this.argsOffset = (-1);
            this.name = name;
            this.location = location;
        }
        ParameterSymbol.prototype.kind = function () {
            return TypeScript.SymbolKind.Parameter;
        };
        ParameterSymbol.prototype.writeable = function () {
            return true;
        };
        ParameterSymbol.prototype.getType = function () {
            return this.parameter.typeLink.type;
        };
        ParameterSymbol.prototype.setType = function (type) {
            this.parameter.typeLink.type = type;
        };
        ParameterSymbol.prototype.isVariable = function () {
            return true;
        };
        ParameterSymbol.prototype.isOptional = function () {
            if(this.parameter && this.parameter.symbol && this.parameter.symbol.declAST) {
                return (this.parameter.symbol.declAST).isOptional;
            } else {
                return false;
            }
        };
        ParameterSymbol.prototype.getTypeNameEx = function (scope) {
            return TypeScript.MemberName.create(this.getType().getScopedTypeNameEx(scope), this.name + (this.isOptional() ? "?" : "") + ": ", "");
        };
        ParameterSymbol.prototype.toString = function () {
            return this.getTypeNameEx(null).toString();
        };
        ParameterSymbol.prototype.specializeType = function (pattern, replacement, checker) {
            var rType = this.parameter.typeLink.type.specializeType(pattern, replacement, checker, false);
            if(this.parameter.typeLink.type != rType) {
                var paramDef = new ValueLocation();
                var result = new ParameterSymbol(this.name, 0, checker.locationInfo.unitIndex, paramDef);
                paramDef.symbol = result;
                result.setType(rType);
                return result;
            } else {
                return this;
            }
        };
        ParameterSymbol.prototype.getParameterDocComments = function () {
            if(!this.paramDocComment) {
                var parameterComments = [];
                if(this.funcDecl) {
                    var fncDocComments = this.funcDecl.getDocComments();
                    var paramComment = TypeScript.Comment.getParameterDocCommentText(this.name, fncDocComments);
                    if(paramComment != "") {
                        parameterComments.push(paramComment);
                    }
                }
                var docComments = TypeScript.Comment.getDocCommentText(this.getDocComments());
                if(docComments != "") {
                    parameterComments.push(docComments);
                }
                this.paramDocComment = parameterComments.join("\n");
            }
            return this.paramDocComment;
        };
        return ParameterSymbol;
    })(InferenceSymbol);
    TypeScript.ParameterSymbol = ParameterSymbol;    
    var VariableSymbol = (function (_super) {
        __extends(VariableSymbol, _super);
        function VariableSymbol(name, location, unitIndex, variable) {
                _super.call(this, name, location, name.length, unitIndex);
            this.variable = variable;
        }
        VariableSymbol.prototype.kind = function () {
            return TypeScript.SymbolKind.Variable;
        };
        VariableSymbol.prototype.writeable = function () {
            return true;
        };
        VariableSymbol.prototype.getType = function () {
            return this.variable.typeLink.type;
        };
        VariableSymbol.prototype.getTypeNameEx = function (scope) {
            return TypeScript.MemberName.create(this.getType().getScopedTypeNameEx(scope), this.name + ": ", "");
        };
        VariableSymbol.prototype.setType = function (type) {
            this.variable.typeLink.type = type;
        };
        VariableSymbol.prototype.isVariable = function () {
            return true;
        };
        return VariableSymbol;
    })(InferenceSymbol);
    TypeScript.VariableSymbol = VariableSymbol;    
})(TypeScript || (TypeScript = {}));
