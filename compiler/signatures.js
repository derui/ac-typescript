var TypeScript;
(function (TypeScript) {
    var Signature = (function () {
        function Signature() {
            this.hasVariableArgList = false;
            this.parameters = null;
            this.declAST = null;
            this.typeCheckStatus = TypeScript.TypeCheckStatus.NotStarted;
            this.nonOptionalParameterCount = 0;
        }
        Signature.prototype.specializeType = function (pattern, replacement, checker) {
            var result = new Signature();
            if(this.hasVariableArgList) {
                result.hasVariableArgList = true;
            }
            result.returnType = new TypeScript.TypeLink();
            if(this.returnType.type) {
                result.returnType.type = this.returnType.type.specializeType(pattern, replacement, checker, false);
            } else {
                result.returnType.type = checker.anyType;
            }
            if(this.parameters) {
                result.parameters = [];
                for(var i = 0, len = this.parameters.length; i < len; i++) {
                    var oldSym = this.parameters[i];
                    var paramDef = new TypeScript.ValueLocation();
                    var paramSym = new TypeScript.ParameterSymbol(oldSym.name, oldSym.location, checker.locationInfo.unitIndex, paramDef);
                    paramSym.declAST = this.declAST;
                    paramDef.symbol = paramSym;
                    paramDef.typeLink = new TypeScript.TypeLink();
                    result.parameters[i] = paramSym;
                    var oldType = oldSym.getType();
                    if(oldType) {
                        paramDef.typeLink.type = oldType.specializeType(pattern, replacement, checker, false);
                    } else {
                        paramDef.typeLink.type = checker.anyType;
                    }
                }
            }
            result.nonOptionalParameterCount = this.nonOptionalParameterCount;
            result.declAST = this.declAST;
            return result;
        };
        Signature.prototype.toString = function () {
            return this.toStringHelper(false, false, null);
        };
        Signature.prototype.toStringHelper = function (shortform, brackets, scope) {
            return this.toStringHelperEx(shortform, brackets, scope).toString();
        };
        Signature.prototype.toStringHelperEx = function (shortform, brackets, scope, prefix) {
            if (typeof prefix === "undefined") { prefix = ""; }
            var builder = new TypeScript.MemberNameArray();
            if(brackets) {
                builder.prefix = prefix + "[";
            } else {
                builder.prefix = prefix + "(";
            }
            var paramLen = this.parameters.length;
            var len = this.hasVariableArgList ? paramLen - 1 : paramLen;
            for(var i = 0; i < len; i++) {
                builder.add(TypeScript.MemberName.create(this.parameters[i].name + (this.parameters[i].isOptional() ? "?" : "") + ": "));
                builder.add(this.parameters[i].getType().getScopedTypeNameEx(scope));
                if(i < paramLen - 1) {
                    builder.add(TypeScript.MemberName.create(", "));
                }
            }
            if(this.hasVariableArgList) {
                builder.add(TypeScript.MemberName.create("..." + this.parameters[i].name + ": "));
                builder.add(this.parameters[i].getType().getScopedTypeNameEx(scope));
            }
            if(shortform) {
                if(brackets) {
                    builder.add(TypeScript.MemberName.create("] => "));
                } else {
                    builder.add(TypeScript.MemberName.create(") => "));
                }
            } else {
                if(brackets) {
                    builder.add(TypeScript.MemberName.create("]: "));
                } else {
                    builder.add(TypeScript.MemberName.create("): "));
                }
            }
            if(this.returnType.type) {
                builder.add(this.returnType.type.getScopedTypeNameEx(scope));
            } else {
                builder.add(TypeScript.MemberName.create("any"));
            }
            return builder;
        };
        return Signature;
    })();
    TypeScript.Signature = Signature;    
    var SignatureGroup = (function () {
        function SignatureGroup() {
            this.signatures = [];
            this.hasImplementation = true;
            this.definitionSignature = null;
            this.hasBeenTypechecked = false;
            this.flags = TypeScript.SignatureFlags.None;
        }
        SignatureGroup.prototype.addSignature = function (signature) {
            if(this.signatures == null) {
                this.signatures = new Array();
            }
            this.signatures[this.signatures.length] = signature;
            if(signature.declAST && !signature.declAST.isOverload && !signature.declAST.isSignature() && !TypeScript.hasFlag(signature.declAST.fncFlags, TypeScript.FncFlags.Ambient) && !TypeScript.hasFlag(signature.declAST.fncFlags, TypeScript.FncFlags.Signature)) {
                this.definitionSignature = signature;
            }
        };
        SignatureGroup.prototype.toString = function () {
            return this.signatures.toString();
        };
        SignatureGroup.prototype.toStrings = function (prefix, shortform, scope, getPrettyTypeName, useSignature) {
            var _this = this;
            var result = [];
            var len = this.signatures.length;
            if(!getPrettyTypeName && len > 1) {
                shortform = false;
            }
            var getMemberNameOfSignature = function (signature) {
                if(_this.flags & TypeScript.SignatureFlags.IsIndexer) {
                    return signature.toStringHelperEx(shortform, true, scope);
                } else {
                    return signature.toStringHelperEx(shortform, false, scope, prefix);
                }
            };
            if(useSignature) {
                result.push(getMemberNameOfSignature(useSignature));
            } else {
                for(var i = 0; i < len; i++) {
                    if(len > 1 && this.signatures[i] == this.definitionSignature) {
                        continue;
                    }
                    result.push(getMemberNameOfSignature(this.signatures[i]));
                    if(getPrettyTypeName) {
                        break;
                    }
                }
            }
            if(getPrettyTypeName && len > 1) {
                var lastMemberName = result[result.length - 1];
                var overloadString = " (+ " + ((this.definitionSignature != null) ? len - 2 : len - 1) + " overload(s))";
                lastMemberName.add(TypeScript.MemberName.create(overloadString));
            }
            return result;
        };
        SignatureGroup.prototype.specializeType = function (pattern, replacement, checker) {
            var result = new SignatureGroup();
            if(this.signatures) {
                for(var i = 0, len = this.signatures.length; i < len; i++) {
                    result.addSignature(this.signatures[i].specializeType(pattern, replacement, checker));
                }
            }
            return result;
        };
        SignatureGroup.prototype.verifySignatures = function (checker) {
            var len = 0;
            if(this.signatures && ((len = this.signatures.length) > 0)) {
                for(var i = 0; i < len; i++) {
                    for(var j = i + 1; j < len; j++) {
                        if(this.signatures[i].declAST && this.signatures[j].declAST && (TypeScript.hasFlag(this.signatures[i].declAST.fncFlags, TypeScript.FncFlags.Signature) && TypeScript.hasFlag(this.signatures[j].declAST.fncFlags, TypeScript.FncFlags.Signature)) && checker.signaturesAreIdentical(this.signatures[i], this.signatures[j])) {
                            checker.errorReporter.simpleError(this.signatures[i].declAST, (this.signatures[i].declAST && this.signatures[i].declAST.name) ? "Signature for '" + this.signatures[i].declAST.name.actualText + "' is duplicated" : "Signature is duplicated");
                        }
                    }
                    if(this.definitionSignature) {
                        if(!checker.signatureIsAssignableToTarget(this.definitionSignature, this.signatures[i])) {
                            checker.errorReporter.simpleError(this.signatures[i].declAST, "Overload signature is not compatible with function definition");
                        }
                    }
                }
            }
        };
        SignatureGroup.prototype.typeCheck = function (checker, ast, hasConstruct) {
            if(this.hasBeenTypechecked) {
                return;
            }
            this.hasBeenTypechecked = true;
            var len = 0;
            if(this.signatures && ((len = this.signatures.length) > 0)) {
                for(var i = 0; i < len; i++) {
                    if(!hasConstruct && !this.definitionSignature && this.signatures[i].declAST && this.signatures[i].declAST.isOverload && !TypeScript.hasFlag(this.signatures[i].declAST.fncFlags, TypeScript.FncFlags.Ambient)) {
                        checker.errorReporter.simpleError(this.signatures[i].declAST, "Overload declaration lacks definition");
                    }
                    if(this.signatures[i].declAST && this.signatures[i].declAST.isConstructor && this.signatures[i].declAST.classDecl && this.signatures[i].declAST.classDecl.type.symbol.typeCheckStatus == TypeScript.TypeCheckStatus.NotStarted) {
                        checker.typeFlow.typeCheck(this.signatures[i].declAST.classDecl);
                    }
                    checker.typeFlow.typeCheck(this.signatures[i].declAST);
                }
                this.verifySignatures(checker);
            }
        };
        return SignatureGroup;
    })();
    TypeScript.SignatureGroup = SignatureGroup;    
})(TypeScript || (TypeScript = {}));
