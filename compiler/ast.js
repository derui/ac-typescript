var __extends = this.__extends || function (d, b) {
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var TypeScript;
(function (TypeScript) {
    var ASTSpan = (function () {
        function ASTSpan() {
            this.minChar = -1;
            this.limChar = -1;
        }
        return ASTSpan;
    })();
    TypeScript.ASTSpan = ASTSpan;    
    var AST = (function (_super) {
        __extends(AST, _super);
        function AST(nodeType) {
                _super.call(this);
            this.nodeType = nodeType;
            this.type = null;
            this.flags = TypeScript.ASTFlags.Writeable;
            this.passCreated = TypeScript.CompilerDiagnostics.analysisPass;
            this.preComments = null;
            this.postComments = null;
            this.docComments = null;
            this.isParenthesized = false;
        }
        AST.prototype.isExpression = function () {
            return false;
        };
        AST.prototype.isStatementOrExpression = function () {
            return false;
        };
        AST.prototype.isCompoundStatement = function () {
            return false;
        };
        AST.prototype.isLeaf = function () {
            return this.isStatementOrExpression() && (!this.isCompoundStatement());
        };
        AST.prototype.isDeclaration = function () {
            return false;
        };
        AST.prototype.typeCheck = function (typeFlow) {
            switch(this.nodeType) {
                case TypeScript.NodeType.Error:
                case TypeScript.NodeType.EmptyExpr:
                    this.type = typeFlow.anyType;
                    break;
                case TypeScript.NodeType.This:
                    return typeFlow.typeCheckThis(this);
                case TypeScript.NodeType.Null:
                    this.type = typeFlow.nullType;
                    break;
                case TypeScript.NodeType.False:
                case TypeScript.NodeType.True:
                    this.type = typeFlow.booleanType;
                    break;
                case TypeScript.NodeType.Super:
                    return typeFlow.typeCheckSuper(this);
                case TypeScript.NodeType.EndCode:
                case TypeScript.NodeType.Empty:
                case TypeScript.NodeType.Void:
                    this.type = typeFlow.voidType;
                    break;
                default:
                    throw new Error("please implement in derived class");
            }
            return this;
        };
        AST.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            switch(this.nodeType) {
                case TypeScript.NodeType.This:
                    emitter.recordSourceMappingStart(this);
                    if(emitter.thisFnc && (TypeScript.hasFlag(emitter.thisFnc.fncFlags, TypeScript.FncFlags.IsFatArrowFunction))) {
                        emitter.writeToOutput("_this");
                    } else {
                        emitter.writeToOutput("this");
                    }
                    emitter.recordSourceMappingEnd(this);
                    break;
                case TypeScript.NodeType.Null:
                    emitter.recordSourceMappingStart(this);
                    emitter.writeToOutput("null");
                    emitter.recordSourceMappingEnd(this);
                    break;
                case TypeScript.NodeType.False:
                    emitter.recordSourceMappingStart(this);
                    emitter.writeToOutput("false");
                    emitter.recordSourceMappingEnd(this);
                    break;
                case TypeScript.NodeType.True:
                    emitter.recordSourceMappingStart(this);
                    emitter.writeToOutput("true");
                    emitter.recordSourceMappingEnd(this);
                    break;
                case TypeScript.NodeType.Super:
                    emitter.recordSourceMappingStart(this);
                    emitter.emitSuperReference();
                    emitter.recordSourceMappingEnd(this);
                    break;
                case TypeScript.NodeType.EndCode:
                case TypeScript.NodeType.Error:
                case TypeScript.NodeType.EmptyExpr:
                    break;
                case TypeScript.NodeType.Empty:
                    emitter.recordSourceMappingStart(this);
                    emitter.recordSourceMappingEnd(this);
                    break;
                case TypeScript.NodeType.Void:
                    emitter.recordSourceMappingStart(this);
                    emitter.writeToOutput("void ");
                    emitter.recordSourceMappingEnd(this);
                    break;
                default:
                    throw new Error("please implement in derived class");
            }
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        AST.prototype.print = function (context) {
            context.startLine();
            var lineCol = {
                line: -1,
                col: -1
            };
            var limLineCol = {
                line: -1,
                col: -1
            };
            if(context.parser !== null) {
                context.parser.getSourceLineCol(lineCol, this.minChar);
                context.parser.getSourceLineCol(limLineCol, this.limChar);
                context.write("(" + lineCol.line + "," + lineCol.col + ")--" + "(" + limLineCol.line + "," + limLineCol.col + "): ");
            }
            var lab = this.printLabel();
            if(TypeScript.hasFlag(this.flags, TypeScript.ASTFlags.Error)) {
                lab += " (Error)";
            }
            context.writeLine(lab);
        };
        AST.prototype.printLabel = function () {
            if(TypeScript.nodeTypeTable[this.nodeType] !== undefined) {
                return TypeScript.nodeTypeTable[this.nodeType];
            } else {
                return (TypeScript.NodeType)._map[this.nodeType];
            }
        };
        AST.prototype.addToControlFlow = function (context) {
            context.walker.options.goChildren = false;
            context.addContent(this);
        };
        AST.prototype.netFreeUses = function (container, freeUses) {
        };
        AST.prototype.treeViewLabel = function () {
            return (TypeScript.NodeType)._map[this.nodeType];
        };
        AST.getResolvedIdentifierName = function getResolvedIdentifierName(name) {
            if(!name) {
                return "";
            }
            var resolved = "";
            var start = 0;
            var i = 0;
            while(i <= name.length - 6) {
                if(name.charAt(i) == '\\' && name.charAt(i + 1) == 'u') {
                    var charCode = parseInt(name.substr(i + 2, 4), 16);
                    resolved += name.substr(start, i - start);
                    resolved += String.fromCharCode(charCode);
                    i += 6;
                    start = i;
                    continue;
                }
                i++;
            }
            resolved += name.substring(start);
            return resolved;
        };
        AST.prototype.getDocComments = function () {
            if(!this.isDeclaration() || !this.preComments || this.preComments.length == 0) {
                return [];
            }
            if(!this.docComments) {
                var preCommentsLength = this.preComments.length;
                var docComments = [];
                for(var i = preCommentsLength - 1; i >= 0; i--) {
                    if(this.preComments[i].isDocComment()) {
                        var prevDocComment = docComments.length > 0 ? docComments[docComments.length - 1] : null;
                        if(prevDocComment == null || (this.preComments[i].limLine == prevDocComment.minLine || this.preComments[i].limLine + 1 == prevDocComment.minLine)) {
                            docComments.push(this.preComments[i]);
                            continue;
                        }
                    }
                    break;
                }
                this.docComments = docComments.reverse();
            }
            return this.docComments;
        };
        return AST;
    })(ASTSpan);
    TypeScript.AST = AST;    
    var IncompleteAST = (function (_super) {
        __extends(IncompleteAST, _super);
        function IncompleteAST(min, lim) {
                _super.call(this, TypeScript.NodeType.Error);
            this.minChar = min;
            this.limChar = lim;
        }
        return IncompleteAST;
    })(AST);
    TypeScript.IncompleteAST = IncompleteAST;    
    var ASTList = (function (_super) {
        __extends(ASTList, _super);
        function ASTList() {
                _super.call(this, TypeScript.NodeType.List);
            this.enclosingScope = null;
            this.members = new Array();
        }
        ASTList.prototype.addToControlFlow = function (context) {
            var len = this.members.length;
            for(var i = 0; i < len; i++) {
                if(context.noContinuation) {
                    context.addUnreachable(this.members[i]);
                    break;
                } else {
                    this.members[i] = context.walk(this.members[i], this);
                }
            }
            context.walker.options.goChildren = false;
        };
        ASTList.prototype.append = function (ast) {
            this.members[this.members.length] = ast;
            return this;
        };
        ASTList.prototype.appendAll = function (ast) {
            if(ast.nodeType == TypeScript.NodeType.List) {
                var list = ast;
                for(var i = 0, len = list.members.length; i < len; i++) {
                    this.append(list.members[i]);
                }
            } else {
                this.append(ast);
            }
            return this;
        };
        ASTList.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.recordSourceMappingStart(this);
            emitter.emitJavascriptList(this, null, TypeScript.TokenID.Semicolon, startLine, false, false);
            emitter.recordSourceMappingEnd(this);
        };
        ASTList.prototype.typeCheck = function (typeFlow) {
            var len = this.members.length;
            typeFlow.nestingLevel++;
            for(var i = 0; i < len; i++) {
                if(this.members[i]) {
                    this.members[i] = this.members[i].typeCheck(typeFlow);
                }
            }
            typeFlow.nestingLevel--;
            return this;
        };
        return ASTList;
    })(AST);
    TypeScript.ASTList = ASTList;    
    var Identifier = (function (_super) {
        __extends(Identifier, _super);
        function Identifier(actualText, hasEscapeSequence) {
                _super.call(this, TypeScript.NodeType.Name);
            this.actualText = actualText;
            this.hasEscapeSequence = hasEscapeSequence;
            this.sym = null;
            this.cloId = -1;
            this.setText(actualText, hasEscapeSequence);
        }
        Identifier.prototype.setText = function (actualText, hasEscapeSequence) {
            this.actualText = actualText;
            if(hasEscapeSequence) {
                this.text = AST.getResolvedIdentifierName(actualText);
            } else {
                this.text = actualText;
            }
        };
        Identifier.prototype.isMissing = function () {
            return false;
        };
        Identifier.prototype.isLeaf = function () {
            return true;
        };
        Identifier.prototype.treeViewLabel = function () {
            return "id: " + this.actualText;
        };
        Identifier.prototype.printLabel = function () {
            if(this.actualText) {
                return "id: " + this.actualText;
            } else {
                return "name node";
            }
        };
        Identifier.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckName(this);
        };
        Identifier.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitJavascriptName(this, true);
        };
        Identifier.fromToken = function fromToken(token) {
            return new Identifier(token.getText(), (token).hasEscapeSequence);
        };
        return Identifier;
    })(AST);
    TypeScript.Identifier = Identifier;    
    var MissingIdentifier = (function (_super) {
        __extends(MissingIdentifier, _super);
        function MissingIdentifier() {
                _super.call(this, "__missing");
        }
        MissingIdentifier.prototype.isMissing = function () {
            return true;
        };
        MissingIdentifier.prototype.emit = function (emitter, tokenId, startLine) {
        };
        return MissingIdentifier;
    })(Identifier);
    TypeScript.MissingIdentifier = MissingIdentifier;    
    var Label = (function (_super) {
        __extends(Label, _super);
        function Label(id) {
                _super.call(this, TypeScript.NodeType.Label);
            this.id = id;
        }
        Label.prototype.printLabel = function () {
            return this.id.actualText + ":";
        };
        Label.prototype.typeCheck = function (typeFlow) {
            this.type = typeFlow.voidType;
            return this;
        };
        Label.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            emitter.recordSourceMappingStart(this.id);
            emitter.writeToOutput(this.id.actualText);
            emitter.recordSourceMappingEnd(this.id);
            emitter.writeLineToOutput(":");
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        return Label;
    })(AST);
    TypeScript.Label = Label;    
    var Expression = (function (_super) {
        __extends(Expression, _super);
        function Expression(nodeType) {
                _super.call(this, nodeType);
        }
        Expression.prototype.isExpression = function () {
            return true;
        };
        Expression.prototype.isStatementOrExpression = function () {
            return true;
        };
        return Expression;
    })(AST);
    TypeScript.Expression = Expression;    
    var UnaryExpression = (function (_super) {
        __extends(UnaryExpression, _super);
        function UnaryExpression(nodeType, operand) {
                _super.call(this, nodeType);
            this.operand = operand;
            this.targetType = null;
            this.castTerm = null;
        }
        UnaryExpression.prototype.addToControlFlow = function (context) {
            _super.prototype.addToControlFlow.call(this, context);
            if(this.nodeType == TypeScript.NodeType.Throw) {
                context.returnStmt();
            }
        };
        UnaryExpression.prototype.typeCheck = function (typeFlow) {
            switch(this.nodeType) {
                case TypeScript.NodeType.Not:
                    return typeFlow.typeCheckBitNot(this);
                case TypeScript.NodeType.LogNot:
                    return typeFlow.typeCheckLogNot(this);
                case TypeScript.NodeType.Pos:
                case TypeScript.NodeType.Neg:
                    return typeFlow.typeCheckUnaryNumberOperator(this);
                case TypeScript.NodeType.IncPost:
                case TypeScript.NodeType.IncPre:
                case TypeScript.NodeType.DecPost:
                case TypeScript.NodeType.DecPre:
                    return typeFlow.typeCheckIncOrDec(this);
                case TypeScript.NodeType.ArrayLit:
                    typeFlow.typeCheckArrayLit(this);
                    return this;
                case TypeScript.NodeType.ObjectLit:
                    typeFlow.typeCheckObjectLit(this);
                    return this;
                case TypeScript.NodeType.Throw:
                    this.operand = typeFlow.typeCheck(this.operand);
                    this.type = typeFlow.voidType;
                    return this;
                case TypeScript.NodeType.Typeof:
                    this.operand = typeFlow.typeCheck(this.operand);
                    this.type = typeFlow.stringType;
                    return this;
                case TypeScript.NodeType.Delete:
                    this.operand = typeFlow.typeCheck(this.operand);
                    this.type = typeFlow.booleanType;
                    break;
                case TypeScript.NodeType.TypeAssertion:
                    this.castTerm = typeFlow.typeCheck(this.castTerm);
                    var applyTargetType = !this.operand.isParenthesized;
                    var targetType = applyTargetType ? this.castTerm.type : null;
                    typeFlow.checker.typeCheckWithContextualType(targetType, typeFlow.checker.inProvisionalTypecheckMode(), true, this.operand);
                    typeFlow.castWithCoercion(this.operand, this.castTerm.type, false, true);
                    this.type = this.castTerm.type;
                    return this;
                case TypeScript.NodeType.Void:
                    this.operand = typeFlow.typeCheck(this.operand);
                    this.type = typeFlow.checker.undefinedType;
                    break;
                default:
                    throw new Error("please implement in derived class");
            }
            return this;
        };
        UnaryExpression.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            switch(this.nodeType) {
                case TypeScript.NodeType.IncPost:
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.PlusPlus, false);
                    emitter.writeToOutput("++");
                    break;
                case TypeScript.NodeType.LogNot:
                    emitter.writeToOutput("!");
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.Exclamation, false);
                    break;
                case TypeScript.NodeType.DecPost:
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.MinusMinus, false);
                    emitter.writeToOutput("--");
                    break;
                case TypeScript.NodeType.ObjectLit:
                    emitter.emitObjectLiteral(this.operand);
                    break;
                case TypeScript.NodeType.ArrayLit:
                    emitter.emitArrayLiteral(this.operand);
                    break;
                case TypeScript.NodeType.Not:
                    emitter.writeToOutput("~");
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.Tilde, false);
                    break;
                case TypeScript.NodeType.Neg:
                    emitter.writeToOutput("-");
                    if(this.operand.nodeType == TypeScript.NodeType.Neg) {
                        this.operand.isParenthesized = true;
                    }
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.Minus, false);
                    break;
                case TypeScript.NodeType.Pos:
                    emitter.writeToOutput("+");
                    if(this.operand.nodeType == TypeScript.NodeType.Pos) {
                        this.operand.isParenthesized = true;
                    }
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.Plus, false);
                    break;
                case TypeScript.NodeType.IncPre:
                    emitter.writeToOutput("++");
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.PlusPlus, false);
                    break;
                case TypeScript.NodeType.DecPre:
                    emitter.writeToOutput("--");
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.MinusMinus, false);
                    break;
                case TypeScript.NodeType.Throw:
                    emitter.writeToOutput("throw ");
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.Tilde, false);
                    emitter.writeToOutput(";");
                    break;
                case TypeScript.NodeType.Typeof:
                    emitter.writeToOutput("typeof ");
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.Tilde, false);
                    break;
                case TypeScript.NodeType.Delete:
                    emitter.writeToOutput("delete ");
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.Tilde, false);
                    break;
                case TypeScript.NodeType.Void:
                    emitter.writeToOutput("void ");
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.Tilde, false);
                    break;
                case TypeScript.NodeType.TypeAssertion:
                    emitter.emitJavascript(this.operand, TypeScript.TokenID.Tilde, false);
                    break;
                default:
                    throw new Error("please implement in derived class");
            }
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        return UnaryExpression;
    })(Expression);
    TypeScript.UnaryExpression = UnaryExpression;    
    var CallExpression = (function (_super) {
        __extends(CallExpression, _super);
        function CallExpression(nodeType, target, arguments) {
                _super.call(this, nodeType);
            this.target = target;
            this.arguments = arguments;
            this.signature = null;
            this.minChar = this.target.minChar;
        }
        CallExpression.prototype.typeCheck = function (typeFlow) {
            if(this.nodeType == TypeScript.NodeType.New) {
                return typeFlow.typeCheckNew(this);
            } else {
                return typeFlow.typeCheckCall(this);
            }
        };
        CallExpression.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            if(this.nodeType == TypeScript.NodeType.New) {
                emitter.emitNew(this.target, this.arguments);
            } else {
                emitter.emitCall(this, this.target, this.arguments);
            }
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        return CallExpression;
    })(Expression);
    TypeScript.CallExpression = CallExpression;    
    var BinaryExpression = (function (_super) {
        __extends(BinaryExpression, _super);
        function BinaryExpression(nodeType, operand1, operand2) {
                _super.call(this, nodeType);
            this.operand1 = operand1;
            this.operand2 = operand2;
        }
        BinaryExpression.prototype.typeCheck = function (typeFlow) {
            switch(this.nodeType) {
                case TypeScript.NodeType.Dot:
                    return typeFlow.typeCheckDotOperator(this);
                case TypeScript.NodeType.Asg:
                    return typeFlow.typeCheckAsgOperator(this);
                case TypeScript.NodeType.Add:
                case TypeScript.NodeType.Sub:
                case TypeScript.NodeType.Mul:
                case TypeScript.NodeType.Div:
                case TypeScript.NodeType.Mod:
                case TypeScript.NodeType.Or:
                case TypeScript.NodeType.And:
                    return typeFlow.typeCheckArithmeticOperator(this, false);
                case TypeScript.NodeType.Xor:
                    return typeFlow.typeCheckBitwiseOperator(this, false);
                case TypeScript.NodeType.Ne:
                case TypeScript.NodeType.Eq:
                    var text;
                    if(typeFlow.checker.styleSettings.eqeqeq) {
                        text = TypeScript.nodeTypeTable[this.nodeType];
                        typeFlow.checker.errorReporter.styleError(this, "use of " + text);
                    } else if(typeFlow.checker.styleSettings.eqnull) {
                        text = TypeScript.nodeTypeTable[this.nodeType];
                        if((this.operand2 !== null) && (this.operand2.nodeType == TypeScript.NodeType.Null)) {
                            typeFlow.checker.errorReporter.styleError(this, "use of " + text + " to compare with null");
                        }
                    }
                case TypeScript.NodeType.Eqv:
                case TypeScript.NodeType.NEqv:
                case TypeScript.NodeType.Lt:
                case TypeScript.NodeType.Le:
                case TypeScript.NodeType.Ge:
                case TypeScript.NodeType.Gt:
                    return typeFlow.typeCheckBooleanOperator(this);
                case TypeScript.NodeType.Index:
                    return typeFlow.typeCheckIndex(this);
                case TypeScript.NodeType.Member:
                    this.type = typeFlow.voidType;
                    return this;
                case TypeScript.NodeType.LogOr:
                    return typeFlow.typeCheckLogOr(this);
                case TypeScript.NodeType.LogAnd:
                    return typeFlow.typeCheckLogAnd(this);
                case TypeScript.NodeType.AsgAdd:
                case TypeScript.NodeType.AsgSub:
                case TypeScript.NodeType.AsgMul:
                case TypeScript.NodeType.AsgDiv:
                case TypeScript.NodeType.AsgMod:
                case TypeScript.NodeType.AsgOr:
                case TypeScript.NodeType.AsgAnd:
                    return typeFlow.typeCheckArithmeticOperator(this, true);
                case TypeScript.NodeType.AsgXor:
                    return typeFlow.typeCheckBitwiseOperator(this, true);
                case TypeScript.NodeType.Lsh:
                case TypeScript.NodeType.Rsh:
                case TypeScript.NodeType.Rs2:
                    return typeFlow.typeCheckShift(this, false);
                case TypeScript.NodeType.AsgLsh:
                case TypeScript.NodeType.AsgRsh:
                case TypeScript.NodeType.AsgRs2:
                    return typeFlow.typeCheckShift(this, true);
                case TypeScript.NodeType.Comma:
                    return typeFlow.typeCheckCommaOperator(this);
                case TypeScript.NodeType.InstOf:
                    return typeFlow.typeCheckInstOf(this);
                case TypeScript.NodeType.In:
                    return typeFlow.typeCheckInOperator(this);
                case TypeScript.NodeType.From:
                    typeFlow.checker.errorReporter.simpleError(this, "Illegal use of 'from' keyword in binary expression");
                    break;
                default:
                    throw new Error("please implement in derived class");
            }
            return this;
        };
        BinaryExpression.prototype.emit = function (emitter, tokenId, startLine) {
            var binTokenId = TypeScript.nodeTypeToTokTable[this.nodeType];
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            if(binTokenId != undefined) {
                emitter.emitJavascript(this.operand1, binTokenId, false);
                if(TypeScript.tokenTable[binTokenId].text == "instanceof") {
                    emitter.writeToOutput(" instanceof ");
                } else if(TypeScript.tokenTable[binTokenId].text == "in") {
                    emitter.writeToOutput(" in ");
                } else {
                    emitter.writeToOutputTrimmable(" " + TypeScript.tokenTable[binTokenId].text + " ");
                }
                emitter.emitJavascript(this.operand2, binTokenId, false);
            } else {
                switch(this.nodeType) {
                    case TypeScript.NodeType.Dot:
                        if(!emitter.tryEmitConstant(this)) {
                            emitter.emitJavascript(this.operand1, TypeScript.TokenID.Dot, false);
                            emitter.writeToOutput(".");
                            emitter.emitJavascriptName(this.operand2, false);
                        }
                        break;
                    case TypeScript.NodeType.Index:
                        emitter.emitIndex(this.operand1, this.operand2);
                        break;
                    case TypeScript.NodeType.Member:
                        if(this.operand2.nodeType == TypeScript.NodeType.FuncDecl && (this.operand2).isAccessor()) {
                            var funcDecl = this.operand2;
                            if(TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.GetAccessor)) {
                                emitter.writeToOutput("get ");
                            } else {
                                emitter.writeToOutput("set ");
                            }
                            emitter.emitJavascript(this.operand1, TypeScript.TokenID.Colon, false);
                        } else {
                            emitter.emitJavascript(this.operand1, TypeScript.TokenID.Colon, false);
                            emitter.writeToOutputTrimmable(": ");
                        }
                        emitter.emitJavascript(this.operand2, TypeScript.TokenID.Comma, false);
                        break;
                    case TypeScript.NodeType.Comma:
                        emitter.emitJavascript(this.operand1, TypeScript.TokenID.Comma, false);
                        if(emitter.emitState.inObjectLiteral) {
                            emitter.writeLineToOutput(", ");
                        } else {
                            emitter.writeToOutput(",");
                        }
                        emitter.emitJavascript(this.operand2, TypeScript.TokenID.Comma, false);
                        break;
                    case TypeScript.NodeType.Is:
                        throw new Error("should be de-sugared during type check");
                    default:
                        throw new Error("please implement in derived class");
                }
            }
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        return BinaryExpression;
    })(Expression);
    TypeScript.BinaryExpression = BinaryExpression;    
    var ConditionalExpression = (function (_super) {
        __extends(ConditionalExpression, _super);
        function ConditionalExpression(operand1, operand2, operand3) {
                _super.call(this, TypeScript.NodeType.ConditionalExpression);
            this.operand1 = operand1;
            this.operand2 = operand2;
            this.operand3 = operand3;
        }
        ConditionalExpression.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckQMark(this);
        };
        ConditionalExpression.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            emitter.emitJavascript(this.operand1, TypeScript.TokenID.Question, false);
            emitter.writeToOutput(" ? ");
            emitter.emitJavascript(this.operand2, TypeScript.TokenID.Question, false);
            emitter.writeToOutput(" : ");
            emitter.emitJavascript(this.operand3, TypeScript.TokenID.Question, false);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        return ConditionalExpression;
    })(Expression);
    TypeScript.ConditionalExpression = ConditionalExpression;    
    var NumberLiteral = (function (_super) {
        __extends(NumberLiteral, _super);
        function NumberLiteral(value, hasEmptyFraction) {
                _super.call(this, TypeScript.NodeType.NumberLit);
            this.value = value;
            this.hasEmptyFraction = hasEmptyFraction;
            this.isNegativeZero = false;
        }
        NumberLiteral.prototype.typeCheck = function (typeFlow) {
            this.type = typeFlow.doubleType;
            return this;
        };
        NumberLiteral.prototype.treeViewLabel = function () {
            return "num: " + this.printLabel();
        };
        NumberLiteral.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            if(this.isNegativeZero) {
                emitter.writeToOutput("-");
            }
            emitter.writeToOutput(this.value.toString());
            if(this.hasEmptyFraction) {
                emitter.writeToOutput(".0");
            }
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        NumberLiteral.prototype.printLabel = function () {
            if(Math.floor(this.value) != this.value) {
                return this.value.toFixed(2).toString();
            } else if(this.hasEmptyFraction) {
                return this.value.toString() + ".0";
            } else {
                return this.value.toString();
            }
        };
        return NumberLiteral;
    })(Expression);
    TypeScript.NumberLiteral = NumberLiteral;    
    var RegexLiteral = (function (_super) {
        __extends(RegexLiteral, _super);
        function RegexLiteral(regex) {
                _super.call(this, TypeScript.NodeType.Regex);
            this.regex = regex;
        }
        RegexLiteral.prototype.typeCheck = function (typeFlow) {
            this.type = typeFlow.regexType;
            return this;
        };
        RegexLiteral.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            emitter.writeToOutput(this.regex.toString());
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        return RegexLiteral;
    })(Expression);
    TypeScript.RegexLiteral = RegexLiteral;    
    var StringLiteral = (function (_super) {
        __extends(StringLiteral, _super);
        function StringLiteral(text) {
                _super.call(this, TypeScript.NodeType.QString);
            this.text = text;
        }
        StringLiteral.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            emitter.emitStringLiteral(this.text);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        StringLiteral.prototype.typeCheck = function (typeFlow) {
            this.type = typeFlow.stringType;
            return this;
        };
        StringLiteral.prototype.treeViewLabel = function () {
            return "st: " + this.text;
        };
        StringLiteral.prototype.printLabel = function () {
            return this.text;
        };
        return StringLiteral;
    })(Expression);
    TypeScript.StringLiteral = StringLiteral;    
    var ModuleElement = (function (_super) {
        __extends(ModuleElement, _super);
        function ModuleElement(nodeType) {
                _super.call(this, nodeType);
        }
        return ModuleElement;
    })(AST);
    TypeScript.ModuleElement = ModuleElement;    
    var ImportDeclaration = (function (_super) {
        __extends(ImportDeclaration, _super);
        function ImportDeclaration(id, alias) {
                _super.call(this, TypeScript.NodeType.ImportDeclaration);
            this.id = id;
            this.alias = alias;
            this.varFlags = TypeScript.VarFlags.None;
            this.isDynamicImport = false;
        }
        ImportDeclaration.prototype.isStatementOrExpression = function () {
            return true;
        };
        ImportDeclaration.prototype.isDeclaration = function () {
            return true;
        };
        ImportDeclaration.prototype.emit = function (emitter, tokenId, startLine) {
            var mod = this.alias.type;
            if(!this.isDynamicImport || (this.id.sym && !(this.id.sym).onlyReferencedAsTypeRef)) {
                var prevModAliasId = emitter.modAliasId;
                var prevFirstModAlias = emitter.firstModAlias;
                emitter.recordSourceMappingStart(this);
                emitter.emitParensAndCommentsInPlace(this, true);
                emitter.writeToOutput("var " + this.id.actualText + " = ");
                emitter.modAliasId = this.id.actualText;
                emitter.firstModAlias = this.firstAliasedModToString();
                emitter.emitJavascript(this.alias, TypeScript.TokenID.Tilde, false);
                if(!this.isDynamicImport) {
                    emitter.writeToOutput(";");
                }
                emitter.emitParensAndCommentsInPlace(this, false);
                emitter.recordSourceMappingEnd(this);
                emitter.modAliasId = prevModAliasId;
                emitter.firstModAlias = prevFirstModAlias;
            }
        };
        ImportDeclaration.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckImportDecl(this);
        };
        ImportDeclaration.prototype.getAliasName = function (aliasAST) {
            if (typeof aliasAST === "undefined") { aliasAST = this.alias; }
            if(aliasAST.nodeType == TypeScript.NodeType.Name) {
                return (aliasAST).actualText;
            } else {
                var dotExpr = aliasAST;
                return this.getAliasName(dotExpr.operand1) + "." + this.getAliasName(dotExpr.operand2);
            }
        };
        ImportDeclaration.prototype.firstAliasedModToString = function () {
            if(this.alias.nodeType == TypeScript.NodeType.Name) {
                return (this.alias).actualText;
            } else {
                var dotExpr = this.alias;
                var firstMod = dotExpr.operand1;
                return firstMod.actualText;
            }
        };
        return ImportDeclaration;
    })(ModuleElement);
    TypeScript.ImportDeclaration = ImportDeclaration;    
    var BoundDecl = (function (_super) {
        __extends(BoundDecl, _super);
        function BoundDecl(id, nodeType, nestingLevel) {
                _super.call(this, nodeType);
            this.id = id;
            this.nestingLevel = nestingLevel;
            this.init = null;
            this.typeExpr = null;
            this.varFlags = TypeScript.VarFlags.None;
            this.sym = null;
        }
        BoundDecl.prototype.isDeclaration = function () {
            return true;
        };
        BoundDecl.prototype.isStatementOrExpression = function () {
            return true;
        };
        BoundDecl.prototype.isPrivate = function () {
            return TypeScript.hasFlag(this.varFlags, TypeScript.VarFlags.Private);
        };
        BoundDecl.prototype.isPublic = function () {
            return TypeScript.hasFlag(this.varFlags, TypeScript.VarFlags.Public);
        };
        BoundDecl.prototype.isProperty = function () {
            return TypeScript.hasFlag(this.varFlags, TypeScript.VarFlags.Property);
        };
        BoundDecl.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckBoundDecl(this);
        };
        BoundDecl.prototype.printLabel = function () {
            return this.treeViewLabel();
        };
        return BoundDecl;
    })(AST);
    TypeScript.BoundDecl = BoundDecl;    
    var VarDecl = (function (_super) {
        __extends(VarDecl, _super);
        function VarDecl(id, nest) {
                _super.call(this, id, TypeScript.NodeType.VarDecl, nest);
        }
        VarDecl.prototype.isAmbient = function () {
            return TypeScript.hasFlag(this.varFlags, TypeScript.VarFlags.Ambient);
        };
        VarDecl.prototype.isExported = function () {
            return TypeScript.hasFlag(this.varFlags, TypeScript.VarFlags.Exported);
        };
        VarDecl.prototype.isStatic = function () {
            return TypeScript.hasFlag(this.varFlags, TypeScript.VarFlags.Static);
        };
        VarDecl.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitJavascriptVarDecl(this, tokenId);
        };
        VarDecl.prototype.treeViewLabel = function () {
            return "var " + this.id.actualText;
        };
        return VarDecl;
    })(BoundDecl);
    TypeScript.VarDecl = VarDecl;    
    var ArgDecl = (function (_super) {
        __extends(ArgDecl, _super);
        function ArgDecl(id) {
                _super.call(this, id, TypeScript.NodeType.ArgDecl, 0);
            this.isOptional = false;
            this.parameterPropertySym = null;
        }
        ArgDecl.prototype.isOptionalArg = function () {
            return this.isOptional || this.init;
        };
        ArgDecl.prototype.treeViewLabel = function () {
            return "arg: " + this.id.actualText;
        };
        ArgDecl.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            emitter.writeToOutput(this.id.actualText);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        return ArgDecl;
    })(BoundDecl);
    TypeScript.ArgDecl = ArgDecl;    
    var internalId = 0;
    var FuncDecl = (function (_super) {
        __extends(FuncDecl, _super);
        function FuncDecl(name, bod, isConstructor, arguments, vars, scopes, statics, nodeType) {
                _super.call(this, nodeType);
            this.name = name;
            this.bod = bod;
            this.isConstructor = isConstructor;
            this.arguments = arguments;
            this.vars = vars;
            this.scopes = scopes;
            this.statics = statics;
            this.hint = null;
            this.fncFlags = TypeScript.FncFlags.None;
            this.returnTypeAnnotation = null;
            this.variableArgList = false;
            this.jumpRefs = null;
            this.internalNameCache = null;
            this.tmp1Declared = false;
            this.enclosingFnc = null;
            this.freeVariables = [];
            this.unitIndex = -1;
            this.classDecl = null;
            this.boundToProperty = null;
            this.isOverload = false;
            this.innerStaticFuncs = [];
            this.isTargetTypedAsMethod = false;
            this.isInlineCallLiteral = false;
            this.accessorSymbol = null;
            this.leftCurlyCount = 0;
            this.rightCurlyCount = 0;
            this.returnStatementsWithExpressions = [];
            this.scopeType = null;
            this.endingToken = null;
        }
        FuncDecl.prototype.isDeclaration = function () {
            return true;
        };
        FuncDecl.prototype.internalName = function () {
            if(this.internalNameCache == null) {
                var extName = this.getNameText();
                if(extName) {
                    this.internalNameCache = "_internal_" + extName;
                } else {
                    this.internalNameCache = "_internal_" + internalId++;
                }
            }
            return this.internalNameCache;
        };
        FuncDecl.prototype.hasSelfReference = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.HasSelfReference);
        };
        FuncDecl.prototype.setHasSelfReference = function () {
            this.fncFlags |= TypeScript.FncFlags.HasSelfReference;
        };
        FuncDecl.prototype.hasSuperReferenceInFatArrowFunction = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.HasSuperReferenceInFatArrowFunction);
        };
        FuncDecl.prototype.setHasSuperReferenceInFatArrowFunction = function () {
            this.fncFlags |= TypeScript.FncFlags.HasSuperReferenceInFatArrowFunction;
        };
        FuncDecl.prototype.addCloRef = function (id, sym) {
            if(this.envids == null) {
                this.envids = new Array();
            }
            this.envids[this.envids.length] = id;
            var outerFnc = this.enclosingFnc;
            if(sym) {
                while(outerFnc && (outerFnc.type.symbol != sym.container)) {
                    outerFnc.addJumpRef(sym);
                    outerFnc = outerFnc.enclosingFnc;
                }
            }
            return this.envids.length - 1;
        };
        FuncDecl.prototype.addJumpRef = function (sym) {
            if(this.jumpRefs == null) {
                this.jumpRefs = new Array();
            }
            var id = new Identifier(sym.name);
            this.jumpRefs[this.jumpRefs.length] = id;
            id.sym = sym;
            id.cloId = this.addCloRef(id, null);
        };
        FuncDecl.prototype.buildControlFlow = function () {
            var entry = new TypeScript.BasicBlock();
            var exit = new TypeScript.BasicBlock();
            var context = new TypeScript.ControlFlowContext(entry, exit);
            var controlFlowPrefix = function (ast, parent, walker) {
                ast.addToControlFlow(walker.state);
                return ast;
            };
            var walker = TypeScript.getAstWalkerFactory().getWalker(controlFlowPrefix, null, null, context);
            context.walker = walker;
            walker.walk(this.bod, this);
            return context;
        };
        FuncDecl.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckFunction(this);
        };
        FuncDecl.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitJavascriptFunction(this);
        };
        FuncDecl.prototype.getNameText = function () {
            if(this.name) {
                return this.name.actualText;
            } else {
                return this.hint;
            }
        };
        FuncDecl.prototype.isMethod = function () {
            return (this.fncFlags & TypeScript.FncFlags.Method) != TypeScript.FncFlags.None;
        };
        FuncDecl.prototype.isCallMember = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.CallMember);
        };
        FuncDecl.prototype.isConstructMember = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.ConstructMember);
        };
        FuncDecl.prototype.isIndexerMember = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.IndexerMember);
        };
        FuncDecl.prototype.isSpecialFn = function () {
            return this.isCallMember() || this.isIndexerMember() || this.isConstructMember();
        };
        FuncDecl.prototype.isAnonymousFn = function () {
            return this.name === null;
        };
        FuncDecl.prototype.isAccessor = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.GetAccessor) || TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.SetAccessor);
        };
        FuncDecl.prototype.isGetAccessor = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.GetAccessor);
        };
        FuncDecl.prototype.isSetAccessor = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.SetAccessor);
        };
        FuncDecl.prototype.isAmbient = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.Ambient);
        };
        FuncDecl.prototype.isExported = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.Exported);
        };
        FuncDecl.prototype.isPrivate = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.Private);
        };
        FuncDecl.prototype.isPublic = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.Public);
        };
        FuncDecl.prototype.isStatic = function () {
            return TypeScript.hasFlag(this.fncFlags, TypeScript.FncFlags.Static);
        };
        FuncDecl.prototype.treeViewLabel = function () {
            if(this.name == null) {
                return "funcExpr";
            } else {
                return "func: " + this.name.actualText;
            }
        };
        FuncDecl.prototype.ClearFlags = function () {
            this.fncFlags = TypeScript.FncFlags.None;
        };
        FuncDecl.prototype.isSignature = function () {
            return (this.fncFlags & TypeScript.FncFlags.Signature) != TypeScript.FncFlags.None;
        };
        return FuncDecl;
    })(AST);
    TypeScript.FuncDecl = FuncDecl;    
    var LocationInfo = (function () {
        function LocationInfo(filename, lineMap, unitIndex) {
            this.filename = filename;
            this.lineMap = lineMap;
            this.unitIndex = unitIndex;
        }
        return LocationInfo;
    })();
    TypeScript.LocationInfo = LocationInfo;    
    TypeScript.unknownLocationInfo = new LocationInfo("unknown", null, -1);
    var Script = (function (_super) {
        __extends(Script, _super);
        function Script(vars, scopes) {
                _super.call(this, new Identifier("script"), null, false, null, vars, scopes, null, TypeScript.NodeType.Script);
            this.locationInfo = null;
            this.referencedFiles = [];
            this.requiresGlobal = false;
            this.requiresExtendsBlock = false;
            this.isResident = false;
            this.isDeclareFile = false;
            this.hasBeenTypeChecked = false;
            this.topLevelMod = null;
            this.leftCurlyCount = 0;
            this.rightCurlyCount = 0;
            this.containsUnicodeChar = false;
            this.containsUnicodeCharInComment = false;
            this.externallyVisibleImportedSymbols = [];
            this.vars = vars;
            this.scopes = scopes;
        }
        Script.prototype.setCachedEmitRequired = function (value) {
            this.cachedEmitRequired = value;
            return this.cachedEmitRequired;
        };
        Script.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckScript(this);
        };
        Script.prototype.treeViewLabel = function () {
            return "Script";
        };
        Script.prototype.emitRequired = function (emitOptions) {
            if(this.cachedEmitRequired != undefined) {
                return this.cachedEmitRequired;
            }
            if(!this.isDeclareFile && !this.isResident && this.bod) {
                if(this.bod.members.length == 0) {
                    return this.setCachedEmitRequired(true);
                }
                for(var i = 0, len = this.bod.members.length; i < len; i++) {
                    var stmt = this.bod.members[i];
                    if(stmt.nodeType == TypeScript.NodeType.ModuleDeclaration) {
                        if(!TypeScript.hasFlag((stmt).modFlags, TypeScript.ModuleFlags.ShouldEmitModuleDecl | TypeScript.ModuleFlags.Ambient)) {
                            return this.setCachedEmitRequired(true);
                        }
                    } else if(stmt.nodeType == TypeScript.NodeType.ClassDeclaration) {
                        if(!TypeScript.hasFlag((stmt).varFlags, TypeScript.VarFlags.Ambient)) {
                            return this.setCachedEmitRequired(true);
                        }
                    } else if(stmt.nodeType == TypeScript.NodeType.VarDecl) {
                        if(!TypeScript.hasFlag((stmt).varFlags, TypeScript.VarFlags.Ambient)) {
                            return this.setCachedEmitRequired(true);
                        }
                    } else if(stmt.nodeType == TypeScript.NodeType.FuncDecl) {
                        if(!(stmt).isSignature()) {
                            return this.setCachedEmitRequired(true);
                        }
                    } else if(stmt.nodeType != TypeScript.NodeType.InterfaceDeclaration && stmt.nodeType != TypeScript.NodeType.Empty) {
                        return this.setCachedEmitRequired(true);
                    }
                }
                if(emitOptions.emitComments && ((this.bod.preComments && this.bod.preComments.length > 0) || (this.bod.postComments && this.bod.postComments.length > 0))) {
                    return this.setCachedEmitRequired(true);
                }
            }
            return this.setCachedEmitRequired(false);
        };
        Script.prototype.emit = function (emitter, tokenId, startLine) {
            if(this.emitRequired(emitter.emitOptions)) {
                emitter.emitParensAndCommentsInPlace(this.bod, true);
                emitter.emitJavascriptList(this.bod, null, TypeScript.TokenID.Semicolon, true, false, false, true, this.requiresExtendsBlock);
                emitter.emitParensAndCommentsInPlace(this.bod, false);
            }
        };
        Script.prototype.AddExternallyVisibleImportedSymbol = function (symbol, checker) {
            if(this.isExternallyVisibleSymbol(symbol)) {
                return;
            }
            if(!symbol.getType().symbol.isExternallyVisible(checker)) {
                var quotes = "";
                var moduleName = symbol.getType().symbol.prettyName;
                if(!TypeScript.isQuoted(moduleName)) {
                    quotes = "'";
                }
                checker.errorReporter.simpleError(symbol.declAST, "Externally visible import statement uses non exported module " + quotes + moduleName + quotes);
            }
            this.externallyVisibleImportedSymbols.push(symbol);
        };
        Script.prototype.isExternallyVisibleSymbol = function (symbol) {
            for(var i = 0; i < this.externallyVisibleImportedSymbols.length; i++) {
                if(this.externallyVisibleImportedSymbols[i] == symbol) {
                    return true;
                }
            }
            return false;
        };
        return Script;
    })(FuncDecl);
    TypeScript.Script = Script;    
    var NamedDeclaration = (function (_super) {
        __extends(NamedDeclaration, _super);
        function NamedDeclaration(nodeType, name, members) {
                _super.call(this, nodeType);
            this.name = name;
            this.members = members;
            this.leftCurlyCount = 0;
            this.rightCurlyCount = 0;
        }
        NamedDeclaration.prototype.isDeclaration = function () {
            return true;
        };
        return NamedDeclaration;
    })(ModuleElement);
    TypeScript.NamedDeclaration = NamedDeclaration;    
    var ModuleDeclaration = (function (_super) {
        __extends(ModuleDeclaration, _super);
        function ModuleDeclaration(name, members, vars, scopes, endingToken) {
                _super.call(this, TypeScript.NodeType.ModuleDeclaration, name, members);
            this.endingToken = endingToken;
            this.modFlags = TypeScript.ModuleFlags.ShouldEmitModuleDecl;
            this.amdDependencies = [];
            this.containsUnicodeChar = false;
            this.containsUnicodeCharInComment = false;
            this.vars = vars;
            this.scopes = scopes;
            this.prettyName = this.name.actualText;
        }
        ModuleDeclaration.prototype.isExported = function () {
            return TypeScript.hasFlag(this.modFlags, TypeScript.ModuleFlags.Exported);
        };
        ModuleDeclaration.prototype.isAmbient = function () {
            return TypeScript.hasFlag(this.modFlags, TypeScript.ModuleFlags.Ambient);
        };
        ModuleDeclaration.prototype.isEnum = function () {
            return TypeScript.hasFlag(this.modFlags, TypeScript.ModuleFlags.IsEnum);
        };
        ModuleDeclaration.prototype.recordNonInterface = function () {
            this.modFlags &= ~TypeScript.ModuleFlags.ShouldEmitModuleDecl;
        };
        ModuleDeclaration.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckModule(this);
        };
        ModuleDeclaration.prototype.emit = function (emitter, tokenId, startLine) {
            if(!TypeScript.hasFlag(this.modFlags, TypeScript.ModuleFlags.ShouldEmitModuleDecl)) {
                emitter.emitParensAndCommentsInPlace(this, true);
                emitter.emitJavascriptModule(this);
                emitter.emitParensAndCommentsInPlace(this, false);
            }
        };
        return ModuleDeclaration;
    })(NamedDeclaration);
    TypeScript.ModuleDeclaration = ModuleDeclaration;    
    var TypeDeclaration = (function (_super) {
        __extends(TypeDeclaration, _super);
        function TypeDeclaration(nodeType, name, extendsList, implementsList, members) {
                _super.call(this, nodeType, name, members);
            this.extendsList = extendsList;
            this.implementsList = implementsList;
            this.varFlags = TypeScript.VarFlags.None;
        }
        TypeDeclaration.prototype.isExported = function () {
            return TypeScript.hasFlag(this.varFlags, TypeScript.VarFlags.Exported);
        };
        TypeDeclaration.prototype.isAmbient = function () {
            return TypeScript.hasFlag(this.varFlags, TypeScript.VarFlags.Ambient);
        };
        return TypeDeclaration;
    })(NamedDeclaration);
    TypeScript.TypeDeclaration = TypeDeclaration;    
    var ClassDeclaration = (function (_super) {
        __extends(ClassDeclaration, _super);
        function ClassDeclaration(name, members, extendsList, implementsList) {
                _super.call(this, TypeScript.NodeType.ClassDeclaration, name, extendsList, implementsList, members);
            this.knownMemberNames = {
            };
            this.constructorDecl = null;
            this.constructorNestingLevel = 0;
            this.endingToken = null;
        }
        ClassDeclaration.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckClass(this);
        };
        ClassDeclaration.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitJavascriptClass(this);
        };
        return ClassDeclaration;
    })(TypeDeclaration);
    TypeScript.ClassDeclaration = ClassDeclaration;    
    var InterfaceDeclaration = (function (_super) {
        __extends(InterfaceDeclaration, _super);
        function InterfaceDeclaration(name, members, extendsList, implementsList) {
                _super.call(this, TypeScript.NodeType.InterfaceDeclaration, name, extendsList, implementsList, members);
        }
        InterfaceDeclaration.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckInterface(this);
        };
        InterfaceDeclaration.prototype.emit = function (emitter, tokenId, startLine) {
        };
        return InterfaceDeclaration;
    })(TypeDeclaration);
    TypeScript.InterfaceDeclaration = InterfaceDeclaration;    
    var Statement = (function (_super) {
        __extends(Statement, _super);
        function Statement(nodeType) {
                _super.call(this, nodeType);
            this.flags |= TypeScript.ASTFlags.IsStatement;
        }
        Statement.prototype.isLoop = function () {
            return false;
        };
        Statement.prototype.isStatementOrExpression = function () {
            return true;
        };
        Statement.prototype.isCompoundStatement = function () {
            return this.isLoop();
        };
        Statement.prototype.typeCheck = function (typeFlow) {
            this.type = typeFlow.voidType;
            return this;
        };
        return Statement;
    })(ModuleElement);
    TypeScript.Statement = Statement;    
    var LabeledStatement = (function (_super) {
        __extends(LabeledStatement, _super);
        function LabeledStatement(labels, stmt) {
                _super.call(this, TypeScript.NodeType.LabeledStatement);
            this.labels = labels;
            this.stmt = stmt;
        }
        LabeledStatement.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            if(this.labels) {
                var labelsLen = this.labels.members.length;
                for(var i = 0; i < labelsLen; i++) {
                    this.labels.members[i].emit(emitter, tokenId, startLine);
                }
            }
            this.stmt.emit(emitter, tokenId, true);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        LabeledStatement.prototype.typeCheck = function (typeFlow) {
            typeFlow.typeCheck(this.labels);
            this.stmt = this.stmt.typeCheck(typeFlow);
            return this;
        };
        LabeledStatement.prototype.addToControlFlow = function (context) {
            var beforeBB = context.current;
            var bb = new TypeScript.BasicBlock();
            context.current = bb;
            beforeBB.addSuccessor(bb);
        };
        return LabeledStatement;
    })(Statement);
    TypeScript.LabeledStatement = LabeledStatement;    
    var Block = (function (_super) {
        __extends(Block, _super);
        function Block(statements, isStatementBlock) {
                _super.call(this, TypeScript.NodeType.Block);
            this.statements = statements;
            this.isStatementBlock = isStatementBlock;
        }
        Block.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            if(this.isStatementBlock) {
                emitter.writeLineToOutput(" {");
                emitter.indenter.increaseIndent();
            } else {
                emitter.setInVarBlock(this.statements.members.length);
            }
            var temp = emitter.setInObjectLiteral(false);
            if(this.statements) {
                emitter.emitJavascriptList(this.statements, null, TypeScript.TokenID.Semicolon, true, false, false);
            }
            if(this.isStatementBlock) {
                emitter.indenter.decreaseIndent();
                emitter.emitIndent();
                emitter.writeToOutput("}");
            }
            emitter.setInObjectLiteral(temp);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        Block.prototype.addToControlFlow = function (context) {
            var afterIfNeeded = new TypeScript.BasicBlock();
            context.pushStatement(this, context.current, afterIfNeeded);
            if(this.statements) {
                context.walk(this.statements, this);
            }
            context.walker.options.goChildren = false;
            context.popStatement();
            if(afterIfNeeded.predecessors.length > 0) {
                context.current.addSuccessor(afterIfNeeded);
                context.current = afterIfNeeded;
            }
        };
        Block.prototype.typeCheck = function (typeFlow) {
            if(!typeFlow.checker.styleSettings.emptyBlocks) {
                if((this.statements === null) || (this.statements.members.length == 0)) {
                    typeFlow.checker.errorReporter.styleError(this, "empty block");
                }
            }
            typeFlow.typeCheck(this.statements);
            return this;
        };
        return Block;
    })(Statement);
    TypeScript.Block = Block;    
    var Jump = (function (_super) {
        __extends(Jump, _super);
        function Jump(nodeType) {
                _super.call(this, nodeType);
            this.target = null;
            this.resolvedTarget = null;
        }
        Jump.prototype.hasExplicitTarget = function () {
            return (this.target);
        };
        Jump.prototype.setResolvedTarget = function (parser, stmt) {
            if(stmt.isLoop()) {
                this.resolvedTarget = stmt;
                return true;
            }
            if(this.nodeType === TypeScript.NodeType.Continue) {
                parser.reportParseError("continue statement applies only to loops");
                return false;
            } else {
                if((stmt.nodeType == TypeScript.NodeType.Switch) || this.hasExplicitTarget()) {
                    this.resolvedTarget = stmt;
                    return true;
                } else {
                    parser.reportParseError("break statement with no label can apply only to a loop or switch statement");
                    return false;
                }
            }
        };
        Jump.prototype.addToControlFlow = function (context) {
            _super.prototype.addToControlFlow.call(this, context);
            context.unconditionalBranch(this.resolvedTarget, (this.nodeType == TypeScript.NodeType.Continue));
        };
        Jump.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            if(this.nodeType == TypeScript.NodeType.Break) {
                emitter.writeToOutput("break");
            } else {
                emitter.writeToOutput("continue");
            }
            if(this.hasExplicitTarget()) {
                emitter.writeToOutput(" " + this.target);
            }
            emitter.recordSourceMappingEnd(this);
            emitter.writeToOutput(";");
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        return Jump;
    })(Statement);
    TypeScript.Jump = Jump;    
    var WhileStatement = (function (_super) {
        __extends(WhileStatement, _super);
        function WhileStatement(cond) {
                _super.call(this, TypeScript.NodeType.While);
            this.cond = cond;
            this.body = null;
        }
        WhileStatement.prototype.isLoop = function () {
            return true;
        };
        WhileStatement.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            var temp = emitter.setInObjectLiteral(false);
            emitter.writeToOutput("while(");
            emitter.emitJavascript(this.cond, TypeScript.TokenID.While, false);
            emitter.writeToOutput(")");
            emitter.emitJavascriptStatements(this.body, false);
            emitter.setInObjectLiteral(temp);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        WhileStatement.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckWhile(this);
        };
        WhileStatement.prototype.addToControlFlow = function (context) {
            var loopHeader = context.current;
            var loopStart = new TypeScript.BasicBlock();
            var afterLoop = new TypeScript.BasicBlock();
            loopHeader.addSuccessor(loopStart);
            context.current = loopStart;
            context.addContent(this.cond);
            var condBlock = context.current;
            var targetInfo = null;
            if(this.body) {
                context.current = new TypeScript.BasicBlock();
                condBlock.addSuccessor(context.current);
                context.pushStatement(this, loopStart, afterLoop);
                context.walk(this.body, this);
                targetInfo = context.popStatement();
            }
            if(!(context.noContinuation)) {
                var loopEnd = context.current;
                loopEnd.addSuccessor(loopStart);
            }
            context.current = afterLoop;
            condBlock.addSuccessor(afterLoop);
            context.noContinuation = false;
            context.walker.options.goChildren = false;
        };
        return WhileStatement;
    })(Statement);
    TypeScript.WhileStatement = WhileStatement;    
    var DoWhileStatement = (function (_super) {
        __extends(DoWhileStatement, _super);
        function DoWhileStatement() {
                _super.call(this, TypeScript.NodeType.DoWhile);
            this.body = null;
            this.whileAST = null;
            this.cond = null;
        }
        DoWhileStatement.prototype.isLoop = function () {
            return true;
        };
        DoWhileStatement.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            var temp = emitter.setInObjectLiteral(false);
            emitter.writeToOutput("do");
            emitter.emitJavascriptStatements(this.body, true);
            emitter.recordSourceMappingStart(this.whileAST);
            emitter.writeToOutput("while");
            emitter.recordSourceMappingEnd(this.whileAST);
            emitter.writeToOutput('(');
            emitter.emitJavascript(this.cond, TypeScript.TokenID.CloseParen, false);
            emitter.writeToOutput(")");
            emitter.setInObjectLiteral(temp);
            emitter.recordSourceMappingEnd(this);
            emitter.writeToOutput(";");
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        DoWhileStatement.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckDoWhile(this);
        };
        DoWhileStatement.prototype.addToControlFlow = function (context) {
            var loopHeader = context.current;
            var loopStart = new TypeScript.BasicBlock();
            var afterLoop = new TypeScript.BasicBlock();
            loopHeader.addSuccessor(loopStart);
            context.current = loopStart;
            var targetInfo = null;
            if(this.body) {
                context.pushStatement(this, loopStart, afterLoop);
                context.walk(this.body, this);
                targetInfo = context.popStatement();
            }
            if(!(context.noContinuation)) {
                var loopEnd = context.current;
                loopEnd.addSuccessor(loopStart);
                context.addContent(this.cond);
                context.current = afterLoop;
                loopEnd.addSuccessor(afterLoop);
            } else {
                context.addUnreachable(this.cond);
            }
            context.walker.options.goChildren = false;
        };
        return DoWhileStatement;
    })(Statement);
    TypeScript.DoWhileStatement = DoWhileStatement;    
    var IfStatement = (function (_super) {
        __extends(IfStatement, _super);
        function IfStatement(cond) {
                _super.call(this, TypeScript.NodeType.If);
            this.cond = cond;
            this.elseBod = null;
            this.statement = new ASTSpan();
        }
        IfStatement.prototype.isCompoundStatement = function () {
            return true;
        };
        IfStatement.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            var temp = emitter.setInObjectLiteral(false);
            emitter.recordSourceMappingStart(this.statement);
            emitter.writeToOutput("if(");
            emitter.emitJavascript(this.cond, TypeScript.TokenID.If, false);
            emitter.writeToOutput(")");
            emitter.recordSourceMappingEnd(this.statement);
            emitter.emitJavascriptStatements(this.thenBod, true);
            if(this.elseBod) {
                if(this.elseBod.nodeType === TypeScript.NodeType.If) {
                    emitter.writeToOutput(" else ");
                    this.elseBod.emit(emitter, tokenId, false);
                } else {
                    emitter.writeToOutput(" else");
                    emitter.emitJavascriptStatements(this.elseBod, true);
                }
            }
            emitter.setInObjectLiteral(temp);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        IfStatement.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckIf(this);
        };
        IfStatement.prototype.addToControlFlow = function (context) {
            this.cond.addToControlFlow(context);
            var afterIf = new TypeScript.BasicBlock();
            var beforeIf = context.current;
            context.pushStatement(this, beforeIf, afterIf);
            var hasContinuation = false;
            context.current = new TypeScript.BasicBlock();
            beforeIf.addSuccessor(context.current);
            context.walk(this.thenBod, this);
            if(!context.noContinuation) {
                hasContinuation = true;
                context.current.addSuccessor(afterIf);
            }
            if(this.elseBod) {
                context.current = new TypeScript.BasicBlock();
                context.noContinuation = false;
                beforeIf.addSuccessor(context.current);
                context.walk(this.elseBod, this);
                if(!context.noContinuation) {
                    hasContinuation = true;
                    context.current.addSuccessor(afterIf);
                } else {
                    if(hasContinuation) {
                        context.noContinuation = false;
                    }
                }
            } else {
                beforeIf.addSuccessor(afterIf);
                context.noContinuation = false;
                hasContinuation = true;
            }
            var targetInfo = context.popStatement();
            if(afterIf.predecessors.length > 0) {
                context.noContinuation = false;
                hasContinuation = true;
            }
            if(hasContinuation) {
                context.current = afterIf;
            }
            context.walker.options.goChildren = false;
        };
        return IfStatement;
    })(Statement);
    TypeScript.IfStatement = IfStatement;    
    var ReturnStatement = (function (_super) {
        __extends(ReturnStatement, _super);
        function ReturnStatement() {
                _super.call(this, TypeScript.NodeType.Return);
            this.returnExpression = null;
        }
        ReturnStatement.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            var temp = emitter.setInObjectLiteral(false);
            if(this.returnExpression) {
                emitter.writeToOutput("return ");
                emitter.emitJavascript(this.returnExpression, TypeScript.TokenID.Semicolon, false);
                if(this.returnExpression.nodeType === TypeScript.NodeType.FuncDecl) {
                    emitter.writeToOutput(";");
                }
            } else {
                emitter.writeToOutput("return;");
            }
            emitter.setInObjectLiteral(temp);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        ReturnStatement.prototype.addToControlFlow = function (context) {
            _super.prototype.addToControlFlow.call(this, context);
            context.returnStmt();
        };
        ReturnStatement.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckReturn(this);
        };
        return ReturnStatement;
    })(Statement);
    TypeScript.ReturnStatement = ReturnStatement;    
    var EndCode = (function (_super) {
        __extends(EndCode, _super);
        function EndCode() {
                _super.call(this, TypeScript.NodeType.EndCode);
        }
        return EndCode;
    })(AST);
    TypeScript.EndCode = EndCode;    
    var ForInStatement = (function (_super) {
        __extends(ForInStatement, _super);
        function ForInStatement(lval, obj) {
                _super.call(this, TypeScript.NodeType.ForIn);
            this.lval = lval;
            this.obj = obj;
            this.statement = new ASTSpan();
            if(this.lval && (this.lval.nodeType == TypeScript.NodeType.VarDecl)) {
                (this.lval).varFlags |= TypeScript.VarFlags.AutoInit;
            }
        }
        ForInStatement.prototype.isLoop = function () {
            return true;
        };
        ForInStatement.prototype.isFiltered = function () {
            if(this.body) {
                var singleItem = null;
                if(this.body.nodeType == TypeScript.NodeType.List) {
                    var stmts = this.body;
                    if(stmts.members.length == 1) {
                        singleItem = stmts.members[0];
                    }
                } else {
                    singleItem = this.body;
                }
                if(singleItem !== null) {
                    if(singleItem.nodeType == TypeScript.NodeType.Block) {
                        var block = singleItem;
                        if((block.statements !== null) && (block.statements.members.length == 1)) {
                            singleItem = block.statements.members[0];
                        }
                    }
                    if(singleItem.nodeType == TypeScript.NodeType.If) {
                        var cond = (singleItem).cond;
                        if(cond.nodeType == TypeScript.NodeType.Call) {
                            var target = (cond).target;
                            if(target.nodeType == TypeScript.NodeType.Dot) {
                                var binex = target;
                                if((binex.operand1.nodeType == TypeScript.NodeType.Name) && (this.obj.nodeType == TypeScript.NodeType.Name) && ((binex.operand1).actualText == (this.obj).actualText)) {
                                    var prop = binex.operand2;
                                    if(prop.actualText == "hasOwnProperty") {
                                        var args = (cond).arguments;
                                        if((args !== null) && (args.members.length == 1)) {
                                            var arg = args.members[0];
                                            if((arg.nodeType == TypeScript.NodeType.Name) && (this.lval.nodeType == TypeScript.NodeType.Name)) {
                                                if(((this.lval).actualText) == (arg).actualText) {
                                                    return true;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return false;
        };
        ForInStatement.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            var temp = emitter.setInObjectLiteral(false);
            emitter.recordSourceMappingStart(this.statement);
            emitter.writeToOutput("for(");
            emitter.emitJavascript(this.lval, TypeScript.TokenID.For, false);
            emitter.writeToOutput(" in ");
            emitter.emitJavascript(this.obj, TypeScript.TokenID.For, false);
            emitter.writeToOutput(")");
            emitter.recordSourceMappingEnd(this.statement);
            emitter.emitJavascriptStatements(this.body, true);
            emitter.setInObjectLiteral(temp);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        ForInStatement.prototype.typeCheck = function (typeFlow) {
            if(typeFlow.checker.styleSettings.forin) {
                if(!this.isFiltered()) {
                    typeFlow.checker.errorReporter.styleError(this, "no hasOwnProperty filter");
                }
            }
            return typeFlow.typeCheckForIn(this);
        };
        ForInStatement.prototype.addToControlFlow = function (context) {
            if(this.lval) {
                context.addContent(this.lval);
            }
            if(this.obj) {
                context.addContent(this.obj);
            }
            var loopHeader = context.current;
            var loopStart = new TypeScript.BasicBlock();
            var afterLoop = new TypeScript.BasicBlock();
            loopHeader.addSuccessor(loopStart);
            context.current = loopStart;
            if(this.body) {
                context.pushStatement(this, loopStart, afterLoop);
                context.walk(this.body, this);
                context.popStatement();
            }
            if(!(context.noContinuation)) {
                var loopEnd = context.current;
                loopEnd.addSuccessor(loopStart);
            }
            context.current = afterLoop;
            context.noContinuation = false;
            loopHeader.addSuccessor(afterLoop);
            context.walker.options.goChildren = false;
        };
        return ForInStatement;
    })(Statement);
    TypeScript.ForInStatement = ForInStatement;    
    var ForStatement = (function (_super) {
        __extends(ForStatement, _super);
        function ForStatement(init) {
                _super.call(this, TypeScript.NodeType.For);
            this.init = init;
        }
        ForStatement.prototype.isLoop = function () {
            return true;
        };
        ForStatement.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            var temp = emitter.setInObjectLiteral(false);
            emitter.writeToOutput("for(");
            if(this.init) {
                if(this.init.nodeType != TypeScript.NodeType.List) {
                    emitter.emitJavascript(this.init, TypeScript.TokenID.For, false);
                } else {
                    emitter.setInVarBlock((this.init).members.length);
                    emitter.emitJavascriptList(this.init, null, TypeScript.TokenID.For, false, false, false);
                }
            }
            emitter.writeToOutput("; ");
            emitter.emitJavascript(this.cond, TypeScript.TokenID.For, false);
            emitter.writeToOutput("; ");
            emitter.emitJavascript(this.incr, TypeScript.TokenID.For, false);
            emitter.writeToOutput(")");
            emitter.emitJavascriptStatements(this.body, true);
            emitter.setInObjectLiteral(temp);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        ForStatement.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckFor(this);
        };
        ForStatement.prototype.addToControlFlow = function (context) {
            if(this.init) {
                context.addContent(this.init);
            }
            var loopHeader = context.current;
            var loopStart = new TypeScript.BasicBlock();
            var afterLoop = new TypeScript.BasicBlock();
            loopHeader.addSuccessor(loopStart);
            context.current = loopStart;
            var condBlock = null;
            var continueTarget = loopStart;
            var incrBB = null;
            if(this.incr) {
                incrBB = new TypeScript.BasicBlock();
                continueTarget = incrBB;
            }
            if(this.cond) {
                condBlock = context.current;
                context.addContent(this.cond);
                context.current = new TypeScript.BasicBlock();
                condBlock.addSuccessor(context.current);
            }
            var targetInfo = null;
            if(this.body) {
                context.pushStatement(this, continueTarget, afterLoop);
                context.walk(this.body, this);
                targetInfo = context.popStatement();
            }
            if(this.incr) {
                if(context.noContinuation) {
                    if(incrBB.predecessors.length == 0) {
                        context.addUnreachable(this.incr);
                    }
                } else {
                    context.current.addSuccessor(incrBB);
                    context.current = incrBB;
                    context.addContent(this.incr);
                }
            }
            var loopEnd = context.current;
            if(!(context.noContinuation)) {
                loopEnd.addSuccessor(loopStart);
            }
            if(condBlock) {
                condBlock.addSuccessor(afterLoop);
                context.noContinuation = false;
            }
            if(afterLoop.predecessors.length > 0) {
                context.noContinuation = false;
                context.current = afterLoop;
            }
            context.walker.options.goChildren = false;
        };
        return ForStatement;
    })(Statement);
    TypeScript.ForStatement = ForStatement;    
    var WithStatement = (function (_super) {
        __extends(WithStatement, _super);
        function WithStatement(expr) {
                _super.call(this, TypeScript.NodeType.With);
            this.expr = expr;
            this.withSym = null;
        }
        WithStatement.prototype.isCompoundStatement = function () {
            return true;
        };
        WithStatement.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            emitter.writeToOutput("with (");
            if(this.expr) {
                emitter.emitJavascript(this.expr, TypeScript.TokenID.With, false);
            }
            emitter.writeToOutput(")");
            emitter.emitJavascriptStatements(this.body, true);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        WithStatement.prototype.typeCheck = function (typeFlow) {
            return typeFlow.typeCheckWith(this);
        };
        return WithStatement;
    })(Statement);
    TypeScript.WithStatement = WithStatement;    
    var SwitchStatement = (function (_super) {
        __extends(SwitchStatement, _super);
        function SwitchStatement(val) {
                _super.call(this, TypeScript.NodeType.Switch);
            this.val = val;
            this.defaultCase = null;
            this.statement = new ASTSpan();
        }
        SwitchStatement.prototype.isCompoundStatement = function () {
            return true;
        };
        SwitchStatement.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            var temp = emitter.setInObjectLiteral(false);
            emitter.recordSourceMappingStart(this.statement);
            emitter.writeToOutput("switch(");
            emitter.emitJavascript(this.val, TypeScript.TokenID.Identifier, false);
            emitter.writeToOutput(")");
            emitter.recordSourceMappingEnd(this.statement);
            emitter.writeLineToOutput(" {");
            emitter.indenter.increaseIndent();
            var casesLen = this.caseList.members.length;
            for(var i = 0; i < casesLen; i++) {
                var caseExpr = this.caseList.members[i];
                emitter.emitJavascript(caseExpr, TypeScript.TokenID.Case, true);
            }
            emitter.indenter.decreaseIndent();
            emitter.emitIndent();
            emitter.writeToOutput("}");
            emitter.setInObjectLiteral(temp);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        SwitchStatement.prototype.typeCheck = function (typeFlow) {
            var len = this.caseList.members.length;
            this.val = typeFlow.typeCheck(this.val);
            for(var i = 0; i < len; i++) {
                this.caseList.members[i] = typeFlow.typeCheck(this.caseList.members[i]);
            }
            this.defaultCase = typeFlow.typeCheck(this.defaultCase);
            this.type = typeFlow.voidType;
            return this;
        };
        SwitchStatement.prototype.addToControlFlow = function (context) {
            var condBlock = context.current;
            context.addContent(this.val);
            var execBlock = new TypeScript.BasicBlock();
            var afterSwitch = new TypeScript.BasicBlock();
            condBlock.addSuccessor(execBlock);
            context.pushSwitch(execBlock);
            context.current = execBlock;
            context.pushStatement(this, execBlock, afterSwitch);
            context.walk(this.caseList, this);
            context.popSwitch();
            var targetInfo = context.popStatement();
            var hasCondContinuation = (this.defaultCase == null);
            if(this.defaultCase == null) {
                condBlock.addSuccessor(afterSwitch);
            }
            if(afterSwitch.predecessors.length > 0) {
                context.noContinuation = false;
                context.current = afterSwitch;
            } else {
                context.noContinuation = true;
            }
            context.walker.options.goChildren = false;
        };
        return SwitchStatement;
    })(Statement);
    TypeScript.SwitchStatement = SwitchStatement;    
    var CaseStatement = (function (_super) {
        __extends(CaseStatement, _super);
        function CaseStatement() {
                _super.call(this, TypeScript.NodeType.Case);
            this.expr = null;
        }
        CaseStatement.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            if(this.expr) {
                emitter.writeToOutput("case ");
                emitter.emitJavascript(this.expr, TypeScript.TokenID.Identifier, false);
            } else {
                emitter.writeToOutput("default");
            }
            emitter.writeToOutput(":");
            if(this.body.members.length == 1 && this.body.members[0].nodeType == TypeScript.NodeType.Block) {
                emitter.emitJavascriptStatements(this.body, false);
            } else {
                emitter.writeLineToOutput("");
                emitter.indenter.increaseIndent();
                emitter.emitBareJavascriptStatements(this.body);
                emitter.indenter.decreaseIndent();
            }
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        CaseStatement.prototype.typeCheck = function (typeFlow) {
            this.expr = typeFlow.typeCheck(this.expr);
            typeFlow.typeCheck(this.body);
            this.type = typeFlow.voidType;
            return this;
        };
        CaseStatement.prototype.addToControlFlow = function (context) {
            var execBlock = new TypeScript.BasicBlock();
            var sw = context.currentSwitch[context.currentSwitch.length - 1];
            if(this.expr) {
                var exprBlock = new TypeScript.BasicBlock();
                context.current = exprBlock;
                sw.addSuccessor(exprBlock);
                context.addContent(this.expr);
                exprBlock.addSuccessor(execBlock);
            } else {
                sw.addSuccessor(execBlock);
            }
            context.current = execBlock;
            if(this.body) {
                context.walk(this.body, this);
            }
            context.noContinuation = false;
            context.walker.options.goChildren = false;
        };
        return CaseStatement;
    })(Statement);
    TypeScript.CaseStatement = CaseStatement;    
    var TypeReference = (function (_super) {
        __extends(TypeReference, _super);
        function TypeReference(term, arrayCount) {
                _super.call(this, TypeScript.NodeType.TypeRef);
            this.term = term;
            this.arrayCount = arrayCount;
        }
        TypeReference.prototype.emit = function (emitter, tokenId, startLine) {
            throw new Error("should not emit a type ref");
        };
        TypeReference.prototype.typeCheck = function (typeFlow) {
            var prevInTCTR = typeFlow.inTypeRefTypeCheck;
            typeFlow.inTypeRefTypeCheck = true;
            var typeLink = TypeScript.getTypeLink(this, typeFlow.checker, true);
            typeFlow.checker.resolveTypeLink(typeFlow.scope, typeLink, false);
            if(this.term) {
                typeFlow.typeCheck(this.term);
            }
            typeFlow.checkForVoidConstructor(typeLink.type, this);
            this.type = typeLink.type;
            if(this.term) {
                this.term.type = this.type;
            }
            typeFlow.inTypeRefTypeCheck = prevInTCTR;
            return this;
        };
        return TypeReference;
    })(AST);
    TypeScript.TypeReference = TypeReference;    
    var TryFinally = (function (_super) {
        __extends(TryFinally, _super);
        function TryFinally(tryNode, finallyNode) {
                _super.call(this, TypeScript.NodeType.TryFinally);
            this.tryNode = tryNode;
            this.finallyNode = finallyNode;
        }
        TryFinally.prototype.isCompoundStatement = function () {
            return true;
        };
        TryFinally.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.recordSourceMappingStart(this);
            emitter.emitJavascript(this.tryNode, TypeScript.TokenID.Try, false);
            emitter.emitJavascript(this.finallyNode, TypeScript.TokenID.Finally, false);
            emitter.recordSourceMappingEnd(this);
        };
        TryFinally.prototype.typeCheck = function (typeFlow) {
            this.tryNode = typeFlow.typeCheck(this.tryNode);
            this.finallyNode = typeFlow.typeCheck(this.finallyNode);
            this.type = typeFlow.voidType;
            return this;
        };
        TryFinally.prototype.addToControlFlow = function (context) {
            var afterFinally = new TypeScript.BasicBlock();
            context.walk(this.tryNode, this);
            var finBlock = new TypeScript.BasicBlock();
            if(context.current) {
                context.current.addSuccessor(finBlock);
            }
            context.current = finBlock;
            context.pushStatement(this, null, afterFinally);
            context.walk(this.finallyNode, this);
            if(!context.noContinuation && context.current) {
                context.current.addSuccessor(afterFinally);
            }
            if(afterFinally.predecessors.length > 0) {
                context.current = afterFinally;
            } else {
                context.noContinuation = true;
            }
            context.popStatement();
            context.walker.options.goChildren = false;
        };
        return TryFinally;
    })(Statement);
    TypeScript.TryFinally = TryFinally;    
    var TryCatch = (function (_super) {
        __extends(TryCatch, _super);
        function TryCatch(tryNode, catchNode) {
                _super.call(this, TypeScript.NodeType.TryCatch);
            this.tryNode = tryNode;
            this.catchNode = catchNode;
        }
        TryCatch.prototype.isCompoundStatement = function () {
            return true;
        };
        TryCatch.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            emitter.emitJavascript(this.tryNode, TypeScript.TokenID.Try, false);
            emitter.emitJavascript(this.catchNode, TypeScript.TokenID.Catch, false);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        TryCatch.prototype.addToControlFlow = function (context) {
            var beforeTry = context.current;
            var tryBlock = new TypeScript.BasicBlock();
            beforeTry.addSuccessor(tryBlock);
            context.current = tryBlock;
            var afterTryCatch = new TypeScript.BasicBlock();
            context.pushStatement(this, null, afterTryCatch);
            context.walk(this.tryNode, this);
            if(!context.noContinuation) {
                if(context.current) {
                    context.current.addSuccessor(afterTryCatch);
                }
            }
            context.current = new TypeScript.BasicBlock();
            beforeTry.addSuccessor(context.current);
            context.walk(this.catchNode, this);
            context.popStatement();
            if(!context.noContinuation) {
                if(context.current) {
                    context.current.addSuccessor(afterTryCatch);
                }
            }
            context.current = afterTryCatch;
            context.walker.options.goChildren = false;
        };
        TryCatch.prototype.typeCheck = function (typeFlow) {
            this.tryNode = typeFlow.typeCheck(this.tryNode);
            this.catchNode = typeFlow.typeCheck(this.catchNode);
            this.type = typeFlow.voidType;
            return this;
        };
        return TryCatch;
    })(Statement);
    TypeScript.TryCatch = TryCatch;    
    var Try = (function (_super) {
        __extends(Try, _super);
        function Try(body) {
                _super.call(this, TypeScript.NodeType.Try);
            this.body = body;
        }
        Try.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            emitter.writeToOutput("try ");
            emitter.emitJavascript(this.body, TypeScript.TokenID.Try, false);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        Try.prototype.typeCheck = function (typeFlow) {
            this.body = typeFlow.typeCheck(this.body);
            return this;
        };
        Try.prototype.addToControlFlow = function (context) {
            if(this.body) {
                context.walk(this.body, this);
            }
            context.walker.options.goChildren = false;
            context.noContinuation = false;
        };
        return Try;
    })(Statement);
    TypeScript.Try = Try;    
    var Catch = (function (_super) {
        __extends(Catch, _super);
        function Catch(param, body) {
                _super.call(this, TypeScript.NodeType.Catch);
            this.param = param;
            this.body = body;
            this.statement = new ASTSpan();
            this.containedScope = null;
            if(this.param) {
                this.param.varFlags |= TypeScript.VarFlags.AutoInit;
            }
        }
        Catch.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            emitter.writeToOutput(" ");
            emitter.recordSourceMappingStart(this.statement);
            emitter.writeToOutput("catch (");
            emitter.emitJavascript(this.param, TypeScript.TokenID.OpenParen, false);
            emitter.writeToOutput(")");
            emitter.recordSourceMappingEnd(this.statement);
            emitter.emitJavascript(this.body, TypeScript.TokenID.Catch, false);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        Catch.prototype.addToControlFlow = function (context) {
            if(this.param) {
                context.addContent(this.param);
                var bodBlock = new TypeScript.BasicBlock();
                context.current.addSuccessor(bodBlock);
                context.current = bodBlock;
            }
            if(this.body) {
                context.walk(this.body, this);
            }
            context.noContinuation = false;
            context.walker.options.goChildren = false;
        };
        Catch.prototype.typeCheck = function (typeFlow) {
            var prevScope = typeFlow.scope;
            typeFlow.scope = this.containedScope;
            this.param = typeFlow.typeCheck(this.param);
            var exceptVar = new TypeScript.ValueLocation();
            var varSym = new TypeScript.VariableSymbol((this.param).id.text, this.param.minChar, typeFlow.checker.locationInfo.unitIndex, exceptVar);
            exceptVar.symbol = varSym;
            exceptVar.typeLink = new TypeScript.TypeLink();
            exceptVar.typeLink.type = typeFlow.anyType;
            var thisFnc = typeFlow.thisFnc;
            if(thisFnc && thisFnc.type) {
                exceptVar.symbol.container = thisFnc.type.symbol;
            } else {
                exceptVar.symbol.container = null;
            }
            this.param.sym = exceptVar.symbol;
            typeFlow.scope.enter(exceptVar.symbol.container, this.param, exceptVar.symbol, typeFlow.checker.errorReporter, false, false, false);
            this.body = typeFlow.typeCheck(this.body);
            if(typeFlow.checker.inProvisionalTypecheckMode()) {
                var table = typeFlow.scope.getTable();
                (table).secondaryTable.table[exceptVar.symbol.name] = undefined;
            }
            this.type = typeFlow.voidType;
            typeFlow.scope = prevScope;
            return this;
        };
        return Catch;
    })(Statement);
    TypeScript.Catch = Catch;    
    var Finally = (function (_super) {
        __extends(Finally, _super);
        function Finally(body) {
                _super.call(this, TypeScript.NodeType.Finally);
            this.body = body;
        }
        Finally.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            emitter.writeToOutput("finally");
            emitter.emitJavascript(this.body, TypeScript.TokenID.Finally, false);
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        Finally.prototype.addToControlFlow = function (context) {
            if(this.body) {
                context.walk(this.body, this);
            }
            context.walker.options.goChildren = false;
            context.noContinuation = false;
        };
        Finally.prototype.typeCheck = function (typeFlow) {
            this.body = typeFlow.typeCheck(this.body);
            return this;
        };
        return Finally;
    })(Statement);
    TypeScript.Finally = Finally;    
    var Comment = (function (_super) {
        __extends(Comment, _super);
        function Comment(content, isBlockComment, endsLine) {
                _super.call(this, TypeScript.NodeType.Comment);
            this.content = content;
            this.isBlockComment = isBlockComment;
            this.endsLine = endsLine;
            this.text = null;
            this.docCommentText = null;
        }
        Comment.prototype.getText = function () {
            if(this.text == null) {
                if(this.isBlockComment) {
                    this.text = this.content.split("\n");
                    for(var i = 0; i < this.text.length; i++) {
                        this.text[i] = this.text[i].replace(/^\s+|\s+$/g, '');
                    }
                } else {
                    this.text = [
                        (this.content.replace(/^\s+|\s+$/g, ''))
                    ];
                }
            }
            return this.text;
        };
        Comment.prototype.isDocComment = function () {
            if(this.isBlockComment) {
                return this.content.charAt(2) == "*";
            }
            return false;
        };
        Comment.prototype.getDocCommentText = function () {
            if(this.docCommentText == null) {
                this.docCommentText = Comment.cleanJSDocComment(this.content);
            }
            return this.docCommentText;
        };
        Comment.consumeLeadingSpace = function consumeLeadingSpace(line, startIndex, maxSpacesToRemove) {
            var endIndex = line.length;
            if(maxSpacesToRemove != undefined) {
                endIndex = TypeScript.min(startIndex + maxSpacesToRemove, endIndex);
            }
            for(; startIndex < endIndex; startIndex++) {
                var charCode = line.charCodeAt(startIndex);
                if(charCode != TypeScript.LexCodeSpace && charCode != TypeScript.LexCodeTAB) {
                    return startIndex;
                }
            }
            if(endIndex != line.length) {
                return endIndex;
            }
            return -1;
        };
        Comment.isSpaceChar = function isSpaceChar(line, index) {
            var length = line.length;
            if(index < length) {
                var charCode = line.charCodeAt(index);
                return charCode == TypeScript.LexCodeSpace || charCode == TypeScript.LexCodeTAB;
            }
            return index == length;
        };
        Comment.cleanDocCommentLine = function cleanDocCommentLine(line, jsDocStyleComment, jsDocLineSpaceToRemove) {
            var nonSpaceIndex = Comment.consumeLeadingSpace(line, 0);
            if(nonSpaceIndex != -1) {
                var jsDocSpacesRemoved = nonSpaceIndex;
                if(jsDocStyleComment && line.charAt(nonSpaceIndex) == '*') {
                    var startIndex = nonSpaceIndex + 1;
                    nonSpaceIndex = Comment.consumeLeadingSpace(line, startIndex, jsDocLineSpaceToRemove);
                    if(nonSpaceIndex != -1) {
                        jsDocSpacesRemoved = nonSpaceIndex - startIndex;
                    } else {
                        return null;
                    }
                }
                return {
                    minChar: nonSpaceIndex,
                    limChar: line.charAt(line.length - 1) == "\r" ? line.length - 1 : line.length,
                    jsDocSpacesRemoved: jsDocSpacesRemoved
                };
            }
            return null;
        };
        Comment.cleanJSDocComment = function cleanJSDocComment(content, spacesToRemove) {
            var docCommentLines = [];
            content = content.replace("/**", "");
            if(content.length >= 2 && content.charAt(content.length - 1) == "/" && content.charAt(content.length - 2) == "*") {
                content = content.substring(0, content.length - 2);
            }
            var lines = content.split("\n");
            var inParamTag = false;
            for(var l = 0; l < lines.length; l++) {
                var line = lines[l];
                var cleanLinePos = Comment.cleanDocCommentLine(line, true, spacesToRemove);
                if(!cleanLinePos) {
                    continue;
                }
                var docCommentText = "";
                var prevPos = cleanLinePos.minChar;
                for(var i = line.indexOf("@", cleanLinePos.minChar); 0 <= i && i < cleanLinePos.limChar; i = line.indexOf("@", i + 1)) {
                    var wasInParamtag = inParamTag;
                    if(line.indexOf("param", i + 1) == i + 1 && Comment.isSpaceChar(line, i + 6)) {
                        if(!wasInParamtag) {
                            docCommentText += line.substring(prevPos, i);
                        }
                        prevPos = i;
                        inParamTag = true;
                    } else if(wasInParamtag) {
                        prevPos = i;
                        inParamTag = false;
                    }
                }
                if(!inParamTag) {
                    docCommentText += line.substring(prevPos, cleanLinePos.limChar);
                }
                var newCleanPos = Comment.cleanDocCommentLine(docCommentText, false);
                if(newCleanPos) {
                    if(spacesToRemove == undefined) {
                        spacesToRemove = cleanLinePos.jsDocSpacesRemoved;
                    }
                    docCommentLines.push(docCommentText);
                }
            }
            return docCommentLines.join("\n");
        };
        Comment.getDocCommentText = function getDocCommentText(comments) {
            var docCommentText = [];
            for(var c = 0; c < comments.length; c++) {
                var commentText = comments[c].getDocCommentText();
                if(commentText != "") {
                    docCommentText.push(commentText);
                }
            }
            return docCommentText.join("\n");
        };
        Comment.getParameterDocCommentText = function getParameterDocCommentText(param, fncDocComments) {
            if(fncDocComments.length == 0 || !fncDocComments[0].isBlockComment) {
                return "";
            }
            for(var i = 0; i < fncDocComments.length; i++) {
                var commentContents = fncDocComments[i].content;
                for(var j = commentContents.indexOf("@param", 0); 0 <= j; j = commentContents.indexOf("@param", j)) {
                    j += 6;
                    if(!Comment.isSpaceChar(commentContents, j)) {
                        continue;
                    }
                    j = Comment.consumeLeadingSpace(commentContents, j);
                    if(j == -1) {
                        break;
                    }
                    if(commentContents.charCodeAt(j) == TypeScript.LexCodeLC) {
                        j++;
                        var charCode = 0;
                        for(var curlies = 1; j < commentContents.length; j++) {
                            charCode = commentContents.charCodeAt(j);
                            if(charCode == TypeScript.LexCodeLC) {
                                curlies++;
                                continue;
                            }
                            if(charCode == TypeScript.LexCodeRC) {
                                curlies--;
                                if(curlies == 0) {
                                    break;
                                } else {
                                    continue;
                                }
                            }
                            if(charCode == TypeScript.LexCodeAtSign) {
                                break;
                            }
                        }
                        if(j == commentContents.length) {
                            break;
                        }
                        if(charCode == TypeScript.LexCodeAtSign) {
                            continue;
                        }
                        j = Comment.consumeLeadingSpace(commentContents, j + 1);
                        if(j == -1) {
                            break;
                        }
                    }
                    if(param != commentContents.substr(j, param.length) || !Comment.isSpaceChar(commentContents, j + param.length)) {
                        continue;
                    }
                    j = Comment.consumeLeadingSpace(commentContents, j + param.length);
                    if(j == -1) {
                        return "";
                    }
                    var endOfParam = commentContents.indexOf("@", j);
                    var paramHelpString = commentContents.substring(j, endOfParam < 0 ? commentContents.length : endOfParam);
                    var paramSpacesToRemove = undefined;
                    var paramLineIndex = commentContents.substring(0, j).lastIndexOf("\n") + 1;
                    if(paramLineIndex != 0) {
                        if(paramLineIndex < j && commentContents.charAt(paramLineIndex + 1) == "\r") {
                            paramLineIndex++;
                        }
                    }
                    var startSpaceRemovalIndex = Comment.consumeLeadingSpace(commentContents, paramLineIndex);
                    if(startSpaceRemovalIndex != j && commentContents.charAt(startSpaceRemovalIndex) == "*") {
                        paramSpacesToRemove = j - startSpaceRemovalIndex - 1;
                    }
                    return Comment.cleanJSDocComment(paramHelpString, paramSpacesToRemove);
                }
            }
            return "";
        };
        Comment.getDocCommentTextOfSignatures = function getDocCommentTextOfSignatures(signatures) {
            var comments = [];
            for(var i = 0; i < signatures.length; i++) {
                var signatureDocComment = TypeScript.Comment.getDocCommentText(signatures[i].declAST.getDocComments());
                if(signatureDocComment != "") {
                    comments.push(signatureDocComment);
                }
            }
            return comments.join("\n");
        };
        return Comment;
    })(AST);
    TypeScript.Comment = Comment;    
    var DebuggerStatement = (function (_super) {
        __extends(DebuggerStatement, _super);
        function DebuggerStatement() {
                _super.call(this, TypeScript.NodeType.Debugger);
        }
        DebuggerStatement.prototype.emit = function (emitter, tokenId, startLine) {
            emitter.emitParensAndCommentsInPlace(this, true);
            emitter.recordSourceMappingStart(this);
            emitter.writeLineToOutput("debugger;");
            emitter.recordSourceMappingEnd(this);
            emitter.emitParensAndCommentsInPlace(this, false);
        };
        return DebuggerStatement;
    })(Statement);
    TypeScript.DebuggerStatement = DebuggerStatement;    
})(TypeScript || (TypeScript = {}));
