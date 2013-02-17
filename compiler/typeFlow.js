var TypeScript;
(function (TypeScript) {
    var ScopeChain = (function () {
        function ScopeChain(container, previous, scope) {
            this.container = container;
            this.previous = previous;
            this.scope = scope;
        }
        return ScopeChain;
    })();
    TypeScript.ScopeChain = ScopeChain;    
    var BBUseDefInfo = (function () {
        function BBUseDefInfo(bb) {
            this.bb = bb;
            this.defsBySymbol = new Array();
            this.useIndexBySymbol = new Array();
        }
        BBUseDefInfo.prototype.updateTop = function () {
            var temp = new BitVector(this.top.bitCount);
            for(var i = 0, succLen = this.bb.successors.length; i < succLen; i++) {
                var succ = this.bb.successors[i];
                if(succ.useDef) {
                    temp.union(succ.useDef.top);
                }
            }
            temp.difference(this.kill);
            temp.union(this.gen);
            var changed = temp.notEq(this.top);
            this.top = temp;
            return changed;
        };
        BBUseDefInfo.prototype.initialize = function (useDefContext) {
            var _this = this;
            var defSym = function (sym, context) {
                if(context.isLocalSym(sym)) {
                    var index = context.getSymbolIndex(sym);
                    _this.useIndexBySymbol[index] = new Array();
                    _this.defsBySymbol[index] = true;
                }
            };
            var useSym = function (sym, context, ast) {
                if(context.isLocalSym(sym)) {
                    var symIndex = context.getSymbolIndex(sym);
                    if(_this.useIndexBySymbol[symIndex] == undefined) {
                        _this.useIndexBySymbol[symIndex] = new Array();
                    }
                    var symUses = _this.useIndexBySymbol[symIndex];
                    var astIndex = context.getUseIndex(ast);
                    context.addUse(symIndex, astIndex);
                    symUses.push(astIndex);
                }
            };
            function initUseDefPre(cur, parent, walker) {
                var context = walker.state;
                if(cur == null) {
                    cur = null;
                }
                if(cur.nodeType == TypeScript.NodeType.VarDecl) {
                    var varDecl = cur;
                    if(varDecl.init || TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.AutoInit)) {
                        defSym(varDecl.sym, context);
                    }
                } else if(cur.nodeType == TypeScript.NodeType.Name) {
                    if(parent) {
                        if(parent.nodeType == TypeScript.NodeType.Asg) {
                            var asg = parent;
                            if(asg.operand1 == cur) {
                                return cur;
                            }
                        } else if(parent.nodeType == TypeScript.NodeType.VarDecl) {
                            var parentDecl = parent;
                            if(parentDecl.id == cur) {
                                return cur;
                            }
                        }
                    }
                    var id = cur;
                    useSym(id.sym, context, cur);
                } else if((cur.nodeType >= TypeScript.NodeType.Asg) && (cur.nodeType <= TypeScript.NodeType.LastAsg)) {
                    var asg = cur;
                    if(asg.operand1 && (asg.operand1.nodeType == TypeScript.NodeType.Name)) {
                        var id = asg.operand1;
                        defSym(id.sym, context);
                    }
                } else if(cur.nodeType == TypeScript.NodeType.FuncDecl) {
                    walker.options.goChildren = false;
                }
                return cur;
            }
            var options = new TypeScript.AstWalkOptions();
            options.reverseSiblings = true;
            TypeScript.getAstWalkerFactory().walk(this.bb.content, initUseDefPre, null, options, useDefContext);
        };
        BBUseDefInfo.prototype.initializeGen = function (useDefContext) {
            var symbolLen = this.useIndexBySymbol.length;
            var bitCount = useDefContext.uses.length;
            this.gen = new BitVector(bitCount);
            for(var s = 0; s < symbolLen; s++) {
                var symUses = this.useIndexBySymbol[s];
                if((symUses != undefined) && (symUses.length > 0)) {
                    for(var u = 0, uLen = symUses.length; u < uLen; u++) {
                        this.gen.set(symUses[u], true);
                    }
                }
            }
            this.top = this.gen;
        };
        BBUseDefInfo.prototype.initializeKill = function (useDefContext) {
            this.kill = new BitVector(this.gen.bitCount);
            for(var s = 0, symbolLen = this.defsBySymbol.length; s < symbolLen; s++) {
                if(this.defsBySymbol[s]) {
                    var globalSymUses = useDefContext.useIndexBySymbol[s];
                    if(globalSymUses) {
                        for(var u = 0, useLen = globalSymUses.length; u < useLen; u++) {
                            this.kill.set(globalSymUses[u], true);
                        }
                    }
                }
            }
        };
        return BBUseDefInfo;
    })();
    TypeScript.BBUseDefInfo = BBUseDefInfo;    
    var UseDefContext = (function () {
        function UseDefContext() {
            this.useIndexBySymbol = new Array();
            this.uses = new Array();
            this.symbols = new Array();
            this.symbolMap = new TypeScript.StringHashTable();
            this.symbolCount = 0;
        }
        UseDefContext.prototype.getSymbolIndex = function (sym) {
            var name = sym.name;
            var index = (this.symbolMap.lookup(name));
            if(index == null) {
                index = this.symbolCount++;
                this.symbols[index] = sym;
                this.symbolMap.add(name, index);
            }
            return index;
        };
        UseDefContext.prototype.addUse = function (symIndex, astIndex) {
            var useBySym = this.useIndexBySymbol[symIndex];
            if(useBySym == undefined) {
                useBySym = new Array();
                this.useIndexBySymbol[symIndex] = useBySym;
            }
            useBySym[useBySym.length] = astIndex;
        };
        UseDefContext.prototype.getUseIndex = function (ast) {
            this.uses[this.uses.length] = ast;
            return this.uses.length - 1;
        };
        UseDefContext.prototype.isLocalSym = function (sym) {
            return (sym && (sym.container == this.func) && (sym.kind() == TypeScript.SymbolKind.Variable));
        };
        UseDefContext.prototype.killSymbol = function (sym, bbUses) {
            var index = this.symbolMap.lookup(sym.name);
            var usesOfSym = this.useIndexBySymbol[index];
            for(var k = 0, len = usesOfSym.length; k < len; k++) {
                bbUses.set(usesOfSym[k], true);
            }
        };
        return UseDefContext;
    })();
    TypeScript.UseDefContext = UseDefContext;    
    var BitVector = (function () {
        function BitVector(bitCount) {
            this.bitCount = bitCount;
            this.firstBits = 0;
            this.restOfBits = null;
            if(this.bitCount > BitVector.packBits) {
                this.restOfBits = new Array();
                var len = Math.floor(this.bitCount / BitVector.packBits);
                for(var i = 0; i < len; i++) {
                    this.restOfBits[i] = 0;
                }
            }
        }
        BitVector.packBits = 30;
        BitVector.prototype.set = function (bitIndex, value) {
            if(bitIndex < BitVector.packBits) {
                if(value) {
                    this.firstBits |= (1 << bitIndex);
                } else {
                    this.firstBits &= (~(1 << bitIndex));
                }
            } else {
                var offset = Math.floor(bitIndex / BitVector.packBits) - 1;
                var localIndex = bitIndex % BitVector.packBits;
                if(value) {
                    this.restOfBits[offset] |= (1 << localIndex);
                } else {
                    this.restOfBits[offset] &= (~(1 << localIndex));
                }
            }
        };
        BitVector.prototype.map = function (fn) {
            var k;
            for(k = 0; k < BitVector.packBits; k++) {
                if(k == this.bitCount) {
                    return;
                }
                if(((1 << k) & this.firstBits) != 0) {
                    fn(k);
                }
            }
            if(this.restOfBits) {
                var len;
                var cumu = BitVector.packBits;
                for(k = 0 , len = this.restOfBits.length; k < len; k++) {
                    var myBits = this.restOfBits[k];
                    for(var j = 0; j < BitVector.packBits; j++) {
                        if(((1 << j) & myBits) != 0) {
                            fn(cumu);
                        }
                        cumu++;
                        if(cumu == this.bitCount) {
                            return;
                        }
                    }
                }
            }
        };
        BitVector.prototype.union = function (b) {
            this.firstBits |= b.firstBits;
            if(this.restOfBits) {
                for(var k = 0, len = this.restOfBits.length; k < len; k++) {
                    var myBits = this.restOfBits[k];
                    var bBits = b.restOfBits[k];
                    this.restOfBits[k] = myBits | bBits;
                }
            }
        };
        BitVector.prototype.intersection = function (b) {
            this.firstBits &= b.firstBits;
            if(this.restOfBits) {
                for(var k = 0, len = this.restOfBits.length; k < len; k++) {
                    var myBits = this.restOfBits[k];
                    var bBits = b.restOfBits[k];
                    this.restOfBits[k] = myBits & bBits;
                }
            }
        };
        BitVector.prototype.notEq = function (b) {
            if(this.firstBits != b.firstBits) {
                return true;
            }
            if(this.restOfBits) {
                for(var k = 0, len = this.restOfBits.length; k < len; k++) {
                    var myBits = this.restOfBits[k];
                    var bBits = b.restOfBits[k];
                    if(myBits != bBits) {
                        return true;
                    }
                }
            }
            return false;
        };
        BitVector.prototype.difference = function (b) {
            var oldFirstBits = this.firstBits;
            this.firstBits &= (~b.firstBits);
            if(this.restOfBits) {
                for(var k = 0, len = this.restOfBits.length; k < len; k++) {
                    var myBits = this.restOfBits[k];
                    var bBits = b.restOfBits[k];
                    this.restOfBits[k] &= (~bBits);
                }
            }
        };
        return BitVector;
    })();
    TypeScript.BitVector = BitVector;    
    var BasicBlock = (function () {
        function BasicBlock() {
            this.predecessors = new Array();
            this.index = -1;
            this.markValue = 0;
            this.successors = new Array();
            this.useDef = null;
            this.content = new TypeScript.ASTList();
        }
        BasicBlock.prototype.marked = function (markBase) {
            return this.markValue > markBase;
        };
        BasicBlock.prototype.mark = function () {
            this.markValue++;
        };
        BasicBlock.prototype.addSuccessor = function (successor) {
            this.successors[this.successors.length] = successor;
            successor.predecessors[successor.predecessors.length] = this;
        };
        return BasicBlock;
    })();
    TypeScript.BasicBlock = BasicBlock;    
    var ControlFlowContext = (function () {
        function ControlFlowContext(current, exit) {
            this.current = current;
            this.exit = exit;
            this.entry = null;
            this.unreachable = null;
            this.noContinuation = false;
            this.statementStack = new Array();
            this.currentSwitch = new Array();
            this.markBase = 0;
            this.linearBBs = new Array();
            this.entry = this.current;
        }
        ControlFlowContext.prototype.walk = function (ast, parent) {
            return this.walker.walk(ast, parent);
        };
        ControlFlowContext.prototype.pushSwitch = function (bb) {
            this.currentSwitch.push(bb);
        };
        ControlFlowContext.prototype.popSwitch = function () {
            return this.currentSwitch.pop();
        };
        ControlFlowContext.prototype.reportUnreachable = function (er) {
            if(this.unreachable && (this.unreachable.length > 0)) {
                var len = this.unreachable.length;
                for(var i = 0; i < len; i++) {
                    var unreachableAST = this.unreachable[i];
                    if(unreachableAST.nodeType != TypeScript.NodeType.EndCode) {
                        er.simpleError(unreachableAST, "unreachable code");
                    }
                }
            }
        };
        ControlFlowContext.prototype.printAST = function (ast, outfile) {
            var printContext = new TypeScript.PrintContext(outfile, null);
            printContext.increaseIndent();
            TypeScript.getAstWalkerFactory().walk(ast, TypeScript.prePrintAST, TypeScript.postPrintAST, null, printContext);
            printContext.decreaseIndent();
        };
        ControlFlowContext.prototype.printBlockContent = function (bb, outfile) {
            var content = bb.content;
            for(var i = 0, len = content.members.length; i < len; i++) {
                var ast = content.members[i];
                this.printAST(ast, outfile);
            }
        };
        ControlFlowContext.prototype.bfs = function (nodeFunc, edgeFunc, preEdges, postEdges) {
            var markValue = this.markBase++;
            var q = new Array();
            q[q.length] = this.entry;
            while(q.length > 0) {
                var bb = q.pop();
                if(!(bb.marked(markValue))) {
                    bb.mark();
                    if(nodeFunc) {
                        nodeFunc(bb);
                    }
                    var succLen = bb.successors.length;
                    if(succLen > 0) {
                        if(preEdges) {
                            preEdges();
                        }
                        for(var j = succLen - 1; j >= 0; j--) {
                            var successor = bb.successors[j];
                            if(!(successor.marked(this.markBase))) {
                                if(edgeFunc) {
                                    edgeFunc(bb, successor);
                                }
                                q[q.length] = successor;
                            }
                        }
                        if(postEdges) {
                            postEdges();
                        }
                    }
                }
            }
        };
        ControlFlowContext.prototype.useDef = function (er, funcSym) {
            var _this = this;
            var useDefContext = new UseDefContext();
            useDefContext.func = funcSym;
            var useDefInit = function (bb) {
                bb.useDef = new BBUseDefInfo(bb);
                bb.useDef.initialize(useDefContext);
                _this.linearBBs[_this.linearBBs.length] = bb;
            };
            this.bfs(useDefInit, null, null, null);
            var i, bbLen;
            for(i = 0 , bbLen = this.linearBBs.length; i < bbLen; i++) {
                this.linearBBs[i].useDef.initializeGen(useDefContext);
                this.linearBBs[i].useDef.initializeKill(useDefContext);
            }
            var changed = true;
            while(changed) {
                changed = false;
                for(i = 0; i < bbLen; i++) {
                    changed = this.linearBBs[i].useDef.updateTop() || changed;
                }
            }
            var top = this.entry.useDef.top;
            top.map(function (index) {
                var ast = useDefContext.uses[index];
                er.simpleError(ast, "use of variable '" + ast.actualText + "' that is not definitely assigned");
            });
        };
        ControlFlowContext.prototype.print = function (outfile) {
            var _this = this;
            var index = 0;
            var node = function (bb) {
                if(bb.index < 0) {
                    bb.index = index++;
                }
                if(bb == _this.exit) {
                    outfile.WriteLine("Exit block with index " + bb.index);
                } else {
                    outfile.WriteLine("Basic block with index " + bb.index);
                    _this.printBlockContent(bb, outfile);
                }
            };
            function preEdges() {
                outfile.Write("  Branches to ");
            }
            function postEdges() {
                outfile.WriteLine("");
            }
            function edge(node1, node2) {
                if(node2.index < 0) {
                    node2.index = index++;
                }
                outfile.Write(node2.index + " ");
            }
            this.bfs(node, edge, preEdges, postEdges);
            if(this.unreachable != null) {
                for(var i = 0, len = this.unreachable.length; i < len; i++) {
                    outfile.WriteLine("Unreachable basic block ...");
                    this.printAST(this.unreachable[i], outfile);
                }
            }
        };
        ControlFlowContext.prototype.pushStatement = function (stmt, continueBB, breakBB) {
            this.statementStack.push({
                stmt: stmt,
                continueBB: continueBB,
                breakBB: breakBB
            });
        };
        ControlFlowContext.prototype.popStatement = function () {
            return this.statementStack.pop();
        };
        ControlFlowContext.prototype.returnStmt = function () {
            this.current.addSuccessor(this.exit);
            this.setUnreachable();
        };
        ControlFlowContext.prototype.setUnreachable = function () {
            this.current = null;
            this.noContinuation = true;
        };
        ControlFlowContext.prototype.addUnreachable = function (ast) {
            if(this.unreachable === null) {
                this.unreachable = new Array();
            }
            this.unreachable[this.unreachable.length] = ast;
        };
        ControlFlowContext.prototype.unconditionalBranch = function (target, isContinue) {
            var targetBB = null;
            for(var i = 0, len = this.statementStack.length; i < len; i++) {
                var targetInfo = this.statementStack[i];
                if(targetInfo.stmt == target) {
                    if(isContinue) {
                        targetBB = targetInfo.continueBB;
                    } else {
                        targetBB = targetInfo.breakBB;
                    }
                    break;
                }
            }
            if(targetBB) {
                this.current.addSuccessor(targetBB);
            }
            this.setUnreachable();
        };
        ControlFlowContext.prototype.addContent = function (ast) {
            if(this.current) {
                this.current.content.append(ast);
            }
        };
        return ControlFlowContext;
    })();
    TypeScript.ControlFlowContext = ControlFlowContext;    
    var ResolutionDataCache = (function () {
        function ResolutionDataCache() {
            this.cacheSize = 16;
            this.rdCache = [];
            this.nextUp = 0;
            for(var i = 0; i < this.cacheSize; i++) {
                this.rdCache[i] = {
                    actuals: new Array(),
                    exactCandidates: new Array(),
                    conversionCandidates: new Array(),
                    id: i
                };
            }
        }
        ResolutionDataCache.prototype.getResolutionData = function () {
            var rd = null;
            if(this.nextUp < this.cacheSize) {
                rd = this.rdCache[this.nextUp];
            }
            if(rd == null) {
                this.cacheSize++;
                rd = {
                    actuals: new Array(),
                    exactCandidates: new Array(),
                    conversionCandidates: new Array(),
                    id: this.cacheSize
                };
                this.rdCache[this.cacheSize] = rd;
            }
            this.nextUp++;
            return rd;
        };
        ResolutionDataCache.prototype.returnResolutionData = function (rd) {
            rd.actuals.length = 0;
            rd.exactCandidates.length = 0;
            rd.conversionCandidates.length = 0;
            this.nextUp = rd.id;
        };
        return ResolutionDataCache;
    })();
    TypeScript.ResolutionDataCache = ResolutionDataCache;    
    var TypeFlow = (function () {
        function TypeFlow(logger, initScope, parser, checker) {
            this.logger = logger;
            this.initScope = initScope;
            this.parser = parser;
            this.checker = checker;
            this.thisFnc = null;
            this.thisClassNode = null;
            this.enclosingFncIsMethod = false;
            this.arrayInterfaceType = null;
            this.stringInterfaceType = null;
            this.objectInterfaceType = null;
            this.functionInterfaceType = null;
            this.numberInterfaceType = null;
            this.booleanInterfaceType = null;
            this.iargumentsInterfaceType = null;
            this.currentScript = null;
            this.inImportTypeCheck = false;
            this.inTypeRefTypeCheck = false;
            this.inArrayElementTypeCheck = false;
            this.resolutionDataCache = new ResolutionDataCache();
            this.nestingLevel = 0;
            this.inSuperCall = false;
            this.checker.typeFlow = this;
            this.scope = this.initScope;
            this.globalScope = this.initScope;
            this.doubleType = this.checker.numberType;
            this.booleanType = this.checker.booleanType;
            this.stringType = this.checker.stringType;
            this.anyType = this.checker.anyType;
            this.regexType = this.anyType;
            this.nullType = this.checker.nullType;
            this.voidType = this.checker.voidType;
            this.arrayAnyType = this.checker.makeArrayType(this.anyType);
        }
        TypeFlow.prototype.initLibs = function () {
            var arraySym = this.globalScope.find("Array", false, true);
            if(arraySym && (arraySym.kind() == TypeScript.SymbolKind.Type)) {
                this.arrayInterfaceType = (arraySym).type;
            }
            var stringSym = this.globalScope.find("String", false, true);
            if(stringSym && (stringSym.kind() == TypeScript.SymbolKind.Type)) {
                this.stringInterfaceType = (stringSym).type;
            }
            var objectSym = this.globalScope.find("Object", false, true);
            if(objectSym && (objectSym.kind() == TypeScript.SymbolKind.Type)) {
                this.objectInterfaceType = (objectSym).type;
            }
            var fnSym = this.globalScope.find("Function", false, true);
            if(fnSym && (fnSym.kind() == TypeScript.SymbolKind.Type)) {
                this.functionInterfaceType = (fnSym).type;
            }
            var numberSym = this.globalScope.find("Number", false, true);
            if(numberSym && (numberSym.kind() == TypeScript.SymbolKind.Type)) {
                this.numberInterfaceType = (numberSym).type;
            }
            var booleanSym = this.globalScope.find("Boolean", false, true);
            if(booleanSym && (booleanSym.kind() == TypeScript.SymbolKind.Type)) {
                this.booleanInterfaceType = (booleanSym).type;
            }
            var regexSym = this.globalScope.find("RegExp", false, true);
            if(regexSym && (regexSym.kind() == TypeScript.SymbolKind.Type)) {
                this.regexType = (regexSym).type;
            }
        };
        TypeFlow.prototype.cast = function (ast, type) {
            return this.castWithCoercion(ast, type, true, false);
        };
        TypeFlow.prototype.castWithCoercion = function (ast, type, applyCoercion, typeAssertion) {
            var comparisonInfo = new TypeScript.TypeComparisonInfo();
            if(this.checker.sourceIsAssignableToTarget(ast.type, type, comparisonInfo) || (typeAssertion && this.checker.sourceIsAssignableToTarget(type, ast.type, comparisonInfo))) {
                if(applyCoercion) {
                    if(type == null) {
                        ast.type = this.anyType;
                    } else if(type.isClass()) {
                        ast.type = type.instanceType;
                    } else {
                        ast.type = type;
                    }
                }
                return ast;
            } else {
                this.checker.errorReporter.incompatibleTypes(ast, ast.type, type, null, this.scope, comparisonInfo);
                return ast;
            }
        };
        TypeFlow.prototype.inScopeTypeCheck = function (ast, enclosingScope) {
            var prevScope = this.scope;
            this.scope = enclosingScope;
            var svThisFnc = this.thisFnc;
            var svThisType = this.thisType;
            var svThisClassNode = this.thisClassNode;
            var svCurrentModDecl = this.checker.currentModDecl;
            var prevMethodStatus = this.enclosingFncIsMethod;
            var container = this.scope.container;
            var fnc = null;
            while(container) {
                if(container.kind() == TypeScript.SymbolKind.Type) {
                    var typeSym = container;
                    var type = typeSym.type;
                    if(type.call) {
                        if(fnc == null) {
                            this.enclosingFncIsMethod = typeSym.isMethod;
                            fnc = container.declAST;
                        }
                    }
                    if(type.isClass()) {
                        this.thisType = type.instanceType;
                        if(typeSym.declAST && (typeSym.declAST.nodeType == TypeScript.NodeType.ClassDeclaration)) {
                            this.thisClassNode = typeSym.declAST;
                        }
                        break;
                    }
                    if(type.isModuleType()) {
                        this.checker.currentModDecl = typeSym.declAST;
                        break;
                    }
                }
                container = container.container;
            }
            this.thisFnc = fnc;
            var updated = this.typeCheck(ast);
            this.thisFnc = svThisFnc;
            this.thisType = svThisType;
            this.thisClassNode = svThisClassNode;
            this.checker.currentModDecl = svCurrentModDecl;
            this.enclosingFncIsMethod = prevMethodStatus;
            this.scope = prevScope;
            return updated;
        };
        TypeFlow.prototype.typeCheck = function (ast) {
            if(ast) {
                return ast.typeCheck(this);
            } else {
                return null;
            }
        };
        TypeFlow.prototype.inScopeTypeCheckDecl = function (ast) {
            if(ast.nodeType == TypeScript.NodeType.VarDecl || ast.nodeType == TypeScript.NodeType.ArgDecl) {
                this.inScopeTypeCheckBoundDecl(ast);
            } else if(ast.nodeType == TypeScript.NodeType.FuncDecl) {
                var funcDecl = ast;
                if(funcDecl.isAccessor()) {
                    this.typeCheckFunction(funcDecl);
                }
            }
        };
        TypeFlow.prototype.inScopeTypeCheckBoundDecl = function (varDecl) {
            var sym = varDecl.sym;
            var svThisFnc = this.thisFnc;
            var svThisType = this.thisType;
            var prevMethodStatus = this.enclosingFncIsMethod;
            var prevLocationInfo = this.checker.locationInfo;
            if(sym && sym.container) {
                var instanceScope = TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.ClassConstructorProperty) ? sym.container.getType().constructorScope : sym.container.instanceScope();
                if(TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Property) && sym.container.declAST.nodeType == TypeScript.NodeType.FuncDecl) {
                    this.thisFnc = sym.container.declAST;
                }
                if(instanceScope) {
                    var prevScope = this.scope;
                    this.scope = instanceScope;
                    var container = sym.container;
                    var svCurrentModDecl = this.checker.currentModDecl;
                    if(this.checker.units && (sym.unitIndex >= 0) && (sym.unitIndex < this.checker.units.length)) {
                        this.checker.locationInfo = this.checker.units[sym.unitIndex];
                    } else {
                        this.checker.locationInfo = TypeScript.unknownLocationInfo;
                    }
                    while(container) {
                        if(container.kind() == TypeScript.SymbolKind.Type) {
                            var typeSym = container;
                            var type = typeSym.type;
                            if(type.call) {
                                this.enclosingFncIsMethod = typeSym.isMethod;
                            }
                            if(type.isClass()) {
                                this.thisType = type.instanceType;
                            }
                            if(type.isModuleType()) {
                                this.checker.currentModDecl = container.declAST;
                                break;
                            }
                        }
                        container = container.container;
                    }
                    this.typeCheckBoundDecl(varDecl);
                    this.checker.currentModDecl = svCurrentModDecl;
                    this.scope = prevScope;
                }
            }
            this.thisFnc = svThisFnc;
            this.thisType = svThisType;
            this.checker.locationInfo = prevLocationInfo;
            this.enclosingFncIsMethod = prevMethodStatus;
        };
        TypeFlow.prototype.resolveBoundDecl = function (varDecl) {
            if(varDecl.typeExpr) {
                if(varDecl.typeExpr.type == null || (varDecl.typeExpr.type && varDecl.typeExpr.type == this.anyType && this.scope) || varDecl.typeExpr.type.symbol == null || !this.checker.typeStatusIsFinished(varDecl.typeExpr.type.symbol.typeCheckStatus)) {
                    this.typeCheck(varDecl.typeExpr);
                }
                varDecl.type = varDecl.typeExpr.type;
                if(varDecl.sym) {
                    varDecl.sym.setType(varDecl.type);
                }
            } else if(varDecl.init == null) {
                if(this.checker.styleSettings.implicitAny) {
                    this.checker.errorReporter.styleError(varDecl, "type implicitly set to 'any'");
                }
                varDecl.type = this.anyType;
                if(varDecl.sym) {
                    if(varDecl.sym.isType()) {
                        var tsym = varDecl.sym;
                        if(tsym.isMethod) {
                            this.checker.errorReporter.simpleError(varDecl, "Cannot bind method group to variable.  (Did you mean to use 'declare function' instead of 'declare var'?)");
                            return;
                        } else {
                            this.checker.errorReporter.simpleError(varDecl, "Cannot bind type to variable");
                            return;
                        }
                    }
                    varDecl.sym.setType(varDecl.type);
                }
            }
        };
        TypeFlow.prototype.typeCheckBoundDecl = function (varDecl) {
            var _this = this;
            var infSym = varDecl.sym;
            if(infSym == null) {
                if(varDecl.init) {
                    varDecl.init = this.typeCheck(varDecl.init);
                    varDecl.type = this.checker.widenType(varDecl.init.type);
                } else {
                    if(this.checker.styleSettings.implicitAny) {
                        this.checker.errorReporter.styleError(varDecl, "type implicitly set to 'any'");
                    }
                    varDecl.type = this.anyType;
                }
            } else {
                if(infSym.typeCheckStatus == TypeScript.TypeCheckStatus.Started) {
                    if(this.checker.styleSettings.implicitAny) {
                        this.checker.errorReporter.styleError(varDecl, "type implicitly set to 'any'");
                    }
                    varDecl.type = this.anyType;
                    infSym.setType(this.anyType);
                } else if(infSym.typeCheckStatus == TypeScript.TypeCheckStatus.NotStarted) {
                    infSym.typeCheckStatus = TypeScript.TypeCheckStatus.Started;
                    this.checker.addStartedPTO(infSym);
                    var resolved = false;
                    if(varDecl.type == null) {
                        if(varDecl.typeExpr) {
                            this.resolveBoundDecl(varDecl);
                            resolved = true;
                            varDecl.type = varDecl.typeExpr.type;
                            infSym.typeCheckStatus = this.checker.getTypeCheckFinishedStatus();
                        }
                    }
                    if(varDecl.init) {
                        var isLocalStatic = TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.LocalStatic);
                        var prevScope = this.scope;
                        var applyTargetType = !varDecl.init.isParenthesized;
                        if(isLocalStatic) {
                            this.scope = varDecl.sym.container.getType().memberScope;
                        }
                        if(TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Property) && this.thisClassNode) {
                            TypeScript.getAstWalkerFactory().walk(varDecl.init, function (ast, parent, walker) {
                                if(ast && ast.nodeType == TypeScript.NodeType.FuncDecl) {
                                    if(TypeScript.hasFlag((ast).fncFlags, TypeScript.FncFlags.IsFatArrowFunction)) {
                                        (ast).fncFlags |= TypeScript.FncFlags.IsPropertyBound;
                                    }
                                    walker.options.goChildren = false;
                                }
                                return ast;
                            });
                        }
                        this.checker.typeCheckWithContextualType(varDecl.type, this.checker.inProvisionalTypecheckMode(), applyTargetType, varDecl.init);
                        this.scope = prevScope;
                        if(varDecl.type) {
                            var preserveScope = false;
                            var preservedContainedScope = null;
                            if(varDecl.init.type) {
                                preservedContainedScope = varDecl.init.type.containedScope;
                                preserveScope = true;
                                if(varDecl.init.type == this.voidType) {
                                    this.checker.errorReporter.simpleError(varDecl, "Cannot assign type 'void' to variable '" + varDecl.id.actualText + "'");
                                }
                            }
                            varDecl.init = this.castWithCoercion(varDecl.init, varDecl.type, applyTargetType && !this.checker.inProvisionalTypecheckMode(), false);
                            if(preserveScope && varDecl.init.type.containedScope == null) {
                                varDecl.init.type.containedScope = preservedContainedScope;
                            }
                        } else {
                            varDecl.type = this.checker.widenType(varDecl.init.type);
                            if(varDecl.type == this.voidType) {
                                this.checker.errorReporter.simpleError(varDecl, "Cannot assign type 'void' to variable '" + varDecl.id.actualText + "'");
                                varDecl.type = this.anyType;
                            }
                        }
                        infSym.setType(varDecl.type);
                    } else {
                        if(!resolved) {
                            this.resolveBoundDecl(varDecl);
                        }
                    }
                    infSym.typeCheckStatus = this.checker.getTypeCheckFinishedStatus();
                } else if(this.checker.typeStatusIsFinished(infSym.typeCheckStatus) && (infSym.declAST != varDecl)) {
                    if(varDecl.init) {
                        varDecl.init = this.typeCheck(varDecl.init);
                        varDecl.type = infSym.getType();
                        varDecl.init = this.cast(varDecl.init, varDecl.type);
                    }
                }
            }
            if(varDecl.id && varDecl.sym) {
                varDecl.id.sym = varDecl.sym;
            }
            if(varDecl.sym && varDecl.sym.container) {
                this.checkTypePrivacy(varDecl.sym.getType(), varDecl.sym, function (typeName, isModuleName) {
                    return _this.varPrivacyErrorReporter(varDecl, typeName, isModuleName);
                });
            }
            return varDecl;
        };
        TypeFlow.prototype.varPrivacyErrorReporter = function (varDecl, typeName, isModuleName) {
            var typestring = "";
            if(isModuleName) {
                var quotestring = "";
                if(!TypeScript.isQuoted(typeName)) {
                    quotestring = "'";
                }
                typestring = " is using inaccessible module " + quotestring + typeName + quotestring;
            } else {
                typestring = " has or is using private type '" + typeName + "'";
            }
            if(TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Public)) {
                if(varDecl.sym.container.declAST.nodeType == TypeScript.NodeType.InterfaceDeclaration) {
                    this.checker.errorReporter.simpleError(varDecl, "property '" + varDecl.sym.name + "' of exported interface" + typestring);
                } else {
                    this.checker.errorReporter.simpleError(varDecl, "public member '" + varDecl.sym.name + "' of exported class" + typestring);
                }
            } else {
                this.checker.errorReporter.simpleError(varDecl, "exported variable '" + varDecl.sym.name + "'" + typestring);
            }
        };
        TypeFlow.prototype.typeCheckSuper = function (ast) {
            if(this.thisType && (this.enclosingFncIsMethod && !this.thisFnc.isStatic()) && this.thisType.baseClass()) {
                ast.type = this.thisType.baseClass();
            } else {
                if(!this.enclosingFncIsMethod && this.thisType && this.thisType.baseClass() && this.thisFnc && TypeScript.hasFlag(this.thisFnc.fncFlags, TypeScript.FncFlags.IsFatArrowFunction)) {
                    var enclosingFnc = this.thisFnc.enclosingFnc;
                    while(TypeScript.hasFlag(enclosingFnc.fncFlags, TypeScript.FncFlags.IsFatArrowFunction)) {
                        enclosingFnc = enclosingFnc.enclosingFnc;
                    }
                    if(enclosingFnc && (enclosingFnc.isMethod() || enclosingFnc.isConstructor) && !enclosingFnc.isStatic()) {
                        ast.type = this.thisType.baseClass();
                        enclosingFnc.setHasSuperReferenceInFatArrowFunction();
                        return ast;
                    }
                }
                ast.type = this.anyType;
                this.checker.errorReporter.invalidSuperReference(ast);
            }
            return ast;
        };
        TypeFlow.prototype.typeCheckThis = function (ast) {
            ast.type = this.anyType;
            var illegalThisRef = false;
            if(this.thisFnc == null) {
                if(this.thisType) {
                    if(this.thisClassNode && this.thisClassNode.nodeType == TypeScript.NodeType.ClassDeclaration) {
                        illegalThisRef = true;
                    } else {
                        ast.type = this.thisType;
                    }
                } else if(this.checker.currentModDecl) {
                    this.checker.errorReporter.simpleError(ast, "'this' may not be referenced within module bodies");
                }
            } else {
                if(this.thisClassNode && (TypeScript.hasFlag(this.thisFnc.fncFlags, TypeScript.FncFlags.IsPropertyBound) || (this.inSuperCall && TypeScript.hasFlag((this.thisClassNode).varFlags, TypeScript.VarFlags.ClassSuperMustBeFirstCallInConstructor)))) {
                    illegalThisRef = true;
                }
                if(this.thisFnc.isMethod() || this.thisFnc.isConstructor || this.thisFnc.isTargetTypedAsMethod) {
                    if(this.thisType && !(this.thisFnc.fncFlags & TypeScript.FncFlags.Static)) {
                        ast.type = this.thisType;
                    }
                }
            }
            if(!this.enclosingFncIsMethod && this.thisFnc && TypeScript.hasFlag(this.thisFnc.fncFlags, TypeScript.FncFlags.IsFatArrowFunction)) {
                if(this.thisFnc.boundToProperty) {
                    var container = this.thisFnc.boundToProperty.sym.container;
                    if(container.declAST.nodeType == TypeScript.NodeType.FuncDecl) {
                        (container.declAST).setHasSelfReference();
                    }
                } else {
                    var encFnc = this.thisFnc.enclosingFnc;
                    var firstEncFnc = encFnc;
                    while(encFnc) {
                        if(this.thisClassNode && TypeScript.hasFlag(encFnc.fncFlags, TypeScript.FncFlags.IsPropertyBound)) {
                            illegalThisRef = true;
                        }
                        if(!TypeScript.hasFlag(encFnc.fncFlags, TypeScript.FncFlags.IsFatArrowFunction) || encFnc.hasSelfReference()) {
                            encFnc.setHasSelfReference();
                            break;
                        }
                        encFnc = encFnc.enclosingFnc;
                    }
                    if(!encFnc && firstEncFnc) {
                        encFnc = firstEncFnc;
                        encFnc.setHasSelfReference();
                    } else if(!encFnc) {
                        if(this.thisClassNode) {
                            (this.thisClassNode).varFlags |= TypeScript.VarFlags.MustCaptureThis;
                        } else if(this.checker.currentModDecl) {
                            this.checker.currentModDecl.modFlags |= TypeScript.ModuleFlags.MustCaptureThis;
                        } else {
                            this.checker.mustCaptureGlobalThis = true;
                        }
                    }
                    if(encFnc && (encFnc.isMethod() || encFnc.isConstructor) && this.thisType && !TypeScript.hasFlag(encFnc.fncFlags, TypeScript.FncFlags.Static)) {
                        ast.type = this.thisType;
                    }
                }
            }
            if(illegalThisRef) {
                this.checker.errorReporter.simpleError(ast, "Keyword 'this' cannot be referenced in initializers in a class body, or in super constructor calls");
            }
            return ast;
        };
        TypeFlow.prototype.setTypeFromSymbol = function (ast, symbol) {
            if(symbol.isVariable()) {
                if(symbol.isInferenceSymbol()) {
                    var infSym = symbol;
                    if(infSym.declAST && !this.checker.typeStatusIsFinished(infSym.typeCheckStatus)) {
                        this.inScopeTypeCheckDecl(infSym.declAST);
                    }
                    if(!this.checker.styleSettings.innerScopeDeclEscape) {
                        if(infSym.declAST && (infSym.declAST.nodeType == TypeScript.NodeType.VarDecl)) {
                            if(this.nestingLevel < (infSym.declAST).nestingLevel) {
                                this.checker.errorReporter.styleError(ast, "Illegal reference to a variable defined in more nested scope");
                            }
                        }
                    }
                }
                ast.type = symbol.getType();
                if(!symbol.writeable()) {
                    ast.flags = ast.flags & (~(TypeScript.ASTFlags.Writeable));
                }
            } else if(symbol.isType()) {
                ast.type = symbol.getType();
                ast.flags = ast.flags & (~(TypeScript.ASTFlags.Writeable));
            } else {
                ast.type = this.anyType;
                this.checker.errorReporter.symbolDoesNotReferToAValue(ast, symbol.name);
            }
        };
        TypeFlow.prototype.typeCheckName = function (ast) {
            var _this = this;
            var identifier = ast;
            if(this.checker.inWith) {
                identifier.type = this.anyType;
            } else {
                var typespace = this.inTypeRefTypeCheck;
                var idText = identifier.text;
                var originalIdText = idText;
                var isDynamicModuleName = TypeScript.isQuoted(identifier.text);
                var symbol = this.scope.find(idText, false, typespace);
                if(symbol == null && isDynamicModuleName) {
                    symbol = this.checker.findSymbolForDynamicModule(idText, this.currentScript.locationInfo.filename, function (id) {
                        return _this.scope.find(id, false, typespace);
                    });
                }
                if(!symbol) {
                    if(!identifier.isMissing()) {
                        this.checker.errorReporter.unresolvedSymbol(identifier, identifier.text);
                    }
                    identifier.type = this.anyType;
                } else {
                    if(TypeScript.optimizeModuleCodeGen && symbol && symbol.isType()) {
                        var symType = symbol.getType();
                        if(symType && (symbol).aliasLink && (symbol).onlyReferencedAsTypeRef) {
                            var modDecl = symType.symbol.declAST;
                            if(modDecl && TypeScript.hasFlag(modDecl.modFlags, TypeScript.ModuleFlags.IsDynamic)) {
                                (symbol).onlyReferencedAsTypeRef = this.inTypeRefTypeCheck;
                            }
                        }
                    }
                    if(symbol.declAST && symbol.declAST.nodeType == TypeScript.NodeType.FuncDecl && !(symbol.declAST).returnTypeAnnotation && (symbol.declAST).signature.typeCheckStatus == TypeScript.TypeCheckStatus.Started) {
                        (symbol.declAST).type.symbol.flags |= TypeScript.SymbolFlags.RecursivelyReferenced;
                        (symbol.declAST).signature.returnType.type = this.anyType;
                    }
                    this.setTypeFromSymbol(ast, symbol);
                    identifier.sym = symbol;
                    if(this.thisFnc) {
                        if(this.thisFnc.type && symbol.container != this.thisFnc.type.symbol) {
                            this.thisFnc.freeVariables[this.thisFnc.freeVariables.length] = symbol;
                        }
                    }
                }
            }
            return ast;
        };
        TypeFlow.prototype.typeCheckScript = function (script) {
            this.checker.locationInfo = script.locationInfo;
            this.scope = this.checker.globalScope;
            if(!script.topLevelMod) {
                this.addLocalsFromScope(this.scope, this.checker.gloMod, script.vars, this.checker.globals, true);
            }
            this.currentScript = script;
            script.bod = this.typeCheck(script.bod);
            this.currentScript = null;
            return script;
        };
        TypeFlow.prototype.typeCheckBitNot = function (ast) {
            var unex = ast;
            unex.operand = this.typeCheck(unex.operand);
            unex.type = this.doubleType;
            return unex;
        };
        TypeFlow.prototype.typeCheckUnaryNumberOperator = function (ast) {
            var unex = ast;
            unex.operand = this.typeCheck(unex.operand);
            unex.type = this.doubleType;
            return ast;
        };
        TypeFlow.prototype.typeCheckLogNot = function (ast) {
            var unex = ast;
            unex.operand = this.typeCheck(unex.operand);
            unex.type = this.booleanType;
            return unex;
        };
        TypeFlow.prototype.astIsWriteable = function (ast) {
            return TypeScript.hasFlag(ast.flags, TypeScript.ASTFlags.Writeable);
        };
        TypeFlow.prototype.typeCheckIncOrDec = function (ast) {
            var unex = ast;
            var lval = unex.operand;
            if(!this.astIsWriteable(unex)) {
                this.checker.errorReporter.valueCannotBeModified(unex);
                unex.type = this.doubleType;
            } else {
                unex = this.typeCheckUnaryNumberOperator(ast);
                if(unex.operand.type != this.checker.numberType && unex.operand.type != this.checker.anyType && !(unex.operand.type.typeFlags & TypeScript.TypeFlags.IsEnum)) {
                    this.checker.errorReporter.simpleError(ast, "'++' and '--' may only be applied to operands of type 'number' or 'any'");
                }
            }
            return unex;
        };
        TypeFlow.prototype.typeCheckBitwiseOperator = function (ast, assignment) {
            var binex = ast;
            var resultType = null;
            binex.operand1 = this.typeCheck(binex.operand1);
            binex.operand2 = this.typeCheck(binex.operand2);
            var leftType = binex.operand1.type;
            var rightType = binex.operand2.type;
            if(assignment && (!this.astIsWriteable(binex))) {
                this.checker.errorReporter.valueCannotBeModified(binex);
            }
            if(this.checker.styleSettings.bitwise) {
                this.checker.errorReporter.styleError(ast, "use of " + TypeScript.nodeTypeTable[binex.nodeType]);
            }
            if(this.checker.sourceIsSubtypeOfTarget(leftType, this.doubleType) && (this.checker.sourceIsSubtypeOfTarget(rightType, this.doubleType))) {
                resultType = this.doubleType;
            } else if((leftType == this.booleanType) && (rightType == this.booleanType)) {
                resultType = this.booleanType;
            } else if(leftType == this.anyType) {
                if((rightType == this.anyType) || (rightType == this.doubleType) || (rightType == this.booleanType)) {
                    resultType = this.anyType;
                }
            } else if(rightType == this.anyType) {
                if((leftType == this.anyType) || (leftType == this.doubleType) || (leftType == this.booleanType)) {
                    resultType = this.anyType;
                }
            }
            if(resultType == null) {
                resultType = this.anyType;
                this.checker.errorReporter.incompatibleTypes(binex, leftType, rightType, binex.printLabel(), this.scope);
            }
            binex.type = resultType;
            return binex;
        };
        TypeFlow.prototype.typeCheckArithmeticOperator = function (ast, assignment) {
            var binex = ast;
            binex.operand1 = this.typeCheck(binex.operand1);
            binex.operand2 = this.typeCheck(binex.operand2);
            var leftType = binex.operand1.type;
            var rightType = binex.operand2.type;
            if(assignment && (!this.astIsWriteable(binex.operand1))) {
                this.checker.errorReporter.valueCannotBeModified(binex);
            }
            if(this.checker.styleSettings.bitwise && ((binex.nodeType == TypeScript.NodeType.And) || (binex.nodeType == TypeScript.NodeType.Or) || (binex.nodeType == TypeScript.NodeType.AsgAnd) || (binex.nodeType == TypeScript.NodeType.AsgOr))) {
                this.checker.errorReporter.styleError(ast, "use of " + TypeScript.nodeTypeTable[binex.nodeType]);
            }
            if(leftType == null || rightType == null) {
                this.checker.errorReporter.simpleError(binex, "Could not typecheck arithmetic operation.  Possible recursive typecheck error?");
                binex.type = this.anyType;
                return binex;
            }
            var nodeType = binex.nodeType;
            if(this.checker.isNullOrUndefinedType(leftType)) {
                leftType = rightType;
            }
            if(this.checker.isNullOrUndefinedType(rightType)) {
                rightType = leftType;
            }
            leftType = this.checker.widenType(leftType);
            rightType = this.checker.widenType(rightType);
            if(nodeType == TypeScript.NodeType.Add || nodeType == TypeScript.NodeType.AsgAdd) {
                if(leftType == this.checker.stringType || rightType == this.checker.stringType) {
                    binex.type = this.checker.stringType;
                } else if(leftType == this.checker.numberType && rightType == this.checker.numberType) {
                    binex.type = this.checker.numberType;
                } else if(this.checker.sourceIsSubtypeOfTarget(leftType, this.checker.numberType) && this.checker.sourceIsSubtypeOfTarget(rightType, this.checker.numberType)) {
                    binex.type = this.checker.numberType;
                } else if(leftType == this.checker.anyType || rightType == this.checker.anyType) {
                    binex.type = this.checker.anyType;
                } else {
                    binex.type = this.anyType;
                    this.checker.errorReporter.incompatibleTypes(binex, leftType, rightType, binex.printLabel(), this.scope);
                }
            } else {
                if(leftType == this.checker.numberType && rightType == this.checker.numberType) {
                    binex.type = this.checker.numberType;
                } else if(this.checker.sourceIsSubtypeOfTarget(leftType, this.checker.numberType) && this.checker.sourceIsSubtypeOfTarget(rightType, this.checker.numberType)) {
                    binex.type = this.checker.numberType;
                } else if(leftType == this.checker.anyType || rightType == this.checker.anyType) {
                    binex.type = this.checker.numberType;
                } else {
                    binex.type = this.anyType;
                    this.checker.errorReporter.incompatibleTypes(binex, leftType, rightType, binex.printLabel(), this.scope);
                }
            }
            return binex;
        };
        TypeFlow.prototype.typeCheckDotOperator = function (ast) {
            var binex = ast;
            var leftIsFnc = false;
            binex.operand1 = this.typeCheck(binex.operand1);
            var leftType = binex.operand1.type;
            var leftScope = null;
            if(leftType) {
                if(leftType == this.anyType) {
                    binex.type = this.anyType;
                    return binex;
                } else if(leftType == this.stringType) {
                    if(this.stringInterfaceType) {
                        leftScope = this.stringInterfaceType.memberScope;
                    } else {
                        binex.type = this.anyType;
                        return binex;
                    }
                } else if(leftType == this.doubleType) {
                    if(this.numberInterfaceType) {
                        leftScope = this.numberInterfaceType.memberScope;
                    } else {
                        binex.type = this.anyType;
                        return binex;
                    }
                } else if(leftType == this.booleanType) {
                    if(this.booleanInterfaceType) {
                        leftScope = this.booleanInterfaceType.memberScope;
                    } else {
                        binex.type = this.anyType;
                        return binex;
                    }
                } else if((leftType.call || leftType.construct) && leftType.members == null) {
                    if(this.functionInterfaceType) {
                        leftScope = this.functionInterfaceType.memberScope;
                    } else {
                        binex.type = this.anyType;
                        return binex;
                    }
                } else if(leftType.elementType) {
                    if(this.arrayInterfaceType) {
                        var arrInstType = leftType.elementType.getArrayBase(this.arrayInterfaceType, this.checker);
                        leftScope = arrInstType.memberScope;
                    } else {
                        binex.type = this.anyType;
                        return binex;
                    }
                } else {
                    leftScope = leftType.memberScope;
                }
            }
            if(leftScope == null) {
                this.checker.errorReporter.expectedClassOrInterface(binex);
                binex.type = this.anyType;
            } else {
                var propertyName = binex.operand2;
                var lhsIsEnclosingType = (this.thisClassNode && binex.operand1.type == this.thisClassNode.type.instanceType) || this.inTypeRefTypeCheck;
                var symbol = leftScope.find(propertyName.text, !lhsIsEnclosingType, this.inTypeRefTypeCheck);
                if(!symbol) {
                    if(this.objectInterfaceType && leftType) {
                        if(leftType.isReferenceType()) {
                            symbol = this.objectInterfaceType.memberScope.find(propertyName.text, false, this.inTypeRefTypeCheck);
                        }
                        if(!symbol) {
                            if(this.functionInterfaceType && (leftType.call || leftType.construct)) {
                                symbol = this.functionInterfaceType.memberScope.find(propertyName.text, false, this.inTypeRefTypeCheck);
                            }
                        }
                    }
                }
                if(!symbol || (!symbol.visible(leftScope, this.checker))) {
                    binex.type = this.anyType;
                    if(symbol == null) {
                        this.checker.errorReporter.simpleError(propertyName, "The property '" + propertyName.actualText + "' does not exist on value of type '" + leftType.getScopedTypeName(this.scope) + "'");
                    } else if(!this.inTypeRefTypeCheck) {
                        this.checker.errorReporter.simpleError(binex, "The property '" + propertyName.actualText + " on type '" + leftType.getScopedTypeName(this.scope) + "' is not visible");
                    }
                } else {
                    if(symbol.isVariable()) {
                        if(symbol.isInferenceSymbol()) {
                            var infSym = symbol;
                            if(infSym.declAST && !this.checker.typeStatusIsFinished(infSym.typeCheckStatus)) {
                                this.inScopeTypeCheckDecl(infSym.declAST);
                            }
                        }
                    }
                    propertyName.sym = symbol;
                    binex.type = symbol.getType();
                }
            }
            if(binex.type == null) {
                binex.type = this.anyType;
            }
            return binex;
        };
        TypeFlow.prototype.typeCheckBooleanOperator = function (ast) {
            var binex = ast;
            binex.operand1 = this.typeCheck(binex.operand1);
            binex.operand2 = this.typeCheck(binex.operand2);
            var leftType = binex.operand1.type;
            var rightType = binex.operand2.type;
            if((!(this.checker.sourceIsAssignableToTarget(leftType, rightType))) && (!(this.checker.sourceIsAssignableToTarget(rightType, leftType)))) {
                this.checker.errorReporter.incompatibleTypes(binex, leftType, rightType, binex.printLabel(), this.scope);
            }
            binex.type = this.booleanType;
            return binex;
        };
        TypeFlow.prototype.typeCheckAsgOperator = function (ast) {
            var binex = ast;
            var applyTargetType = !binex.operand2.isParenthesized;
            binex.operand1 = this.typeCheck(binex.operand1);
            this.checker.typeCheckWithContextualType(binex.operand1.type, this.checker.inProvisionalTypecheckMode(), applyTargetType, binex.operand2);
            var leftType = binex.operand1.type;
            var rightType = binex.operand2.type;
            if(!(this.astIsWriteable(binex.operand1))) {
                this.checker.errorReporter.valueCannotBeModified(binex);
            }
            if(binex.operand1.nodeType == TypeScript.NodeType.Call) {
                var callEx = binex.operand1;
            }
            var preserveScope = false;
            var preservedContainedScope = null;
            if(binex.operand2.type) {
                preservedContainedScope = binex.operand2.type.containedScope;
                preserveScope = true;
            }
            binex.operand2 = this.castWithCoercion(binex.operand2, leftType, applyTargetType && !this.checker.inProvisionalTypecheckMode(), false);
            if(preserveScope && binex.operand2.type.containedScope == null) {
                binex.operand2.type.containedScope = preservedContainedScope;
            }
            binex.type = rightType;
            return binex;
        };
        TypeFlow.prototype.typeCheckIndex = function (ast) {
            var binex = ast;
            binex.operand1 = this.typeCheck(binex.operand1);
            binex.operand2 = this.typeCheck(binex.operand2);
            if(!this.checker.styleSettings.literalSubscript) {
                if(binex.operand2.nodeType == TypeScript.NodeType.QString) {
                    this.checker.errorReporter.styleError(ast, "use literal subscript ('.') notation instead)");
                }
            }
            var objExprType = binex.operand1.type;
            var indexExprType = binex.operand2.type;
            if(objExprType.elementType) {
                if(indexExprType == this.checker.anyType || indexExprType == this.checker.numberType || TypeScript.hasFlag(indexExprType.typeFlags, TypeScript.TypeFlags.IsEnum)) {
                    binex.type = objExprType.elementType;
                } else if(indexExprType == this.checker.stringType) {
                    binex.type = this.checker.anyType;
                } else {
                    this.checker.errorReporter.simpleError(binex, "Illegal property access");
                    binex.type = this.checker.anyType;
                }
            } else if(objExprType.index) {
                if(indexExprType == this.checker.anyType || !((objExprType.index.flags & TypeScript.SignatureFlags.IsStringIndexer) || (objExprType.index.flags & TypeScript.SignatureFlags.IsNumberIndexer)) || ((objExprType.index.flags & TypeScript.SignatureFlags.IsStringIndexer) && indexExprType == this.checker.stringType) || ((objExprType.index.flags & TypeScript.SignatureFlags.IsNumberIndexer) && (indexExprType == this.checker.numberType || TypeScript.hasFlag(indexExprType.typeFlags, TypeScript.TypeFlags.IsEnum)))) {
                    var sig = this.resolveOverload(ast, objExprType.index);
                    if(sig) {
                        binex.type = sig.returnType.type;
                    } else {
                        binex.type = this.checker.anyType;
                    }
                } else if(indexExprType == this.checker.stringType) {
                    binex.type = this.checker.anyType;
                } else {
                    this.checker.errorReporter.simpleError(binex, "Illegal property access");
                    binex.type = this.checker.anyType;
                }
            } else if((objExprType == this.checker.anyType || objExprType == this.checker.stringType || objExprType == this.checker.numberType || objExprType == this.checker.booleanType || objExprType.isReferenceType()) && (indexExprType == this.checker.anyType || indexExprType == this.checker.stringType || (indexExprType == this.checker.numberType || TypeScript.hasFlag(indexExprType.typeFlags, TypeScript.TypeFlags.IsEnum)))) {
                binex.type = this.checker.anyType;
            } else {
                this.checker.errorReporter.simpleError(binex, "Illegal property access");
                binex.type = this.checker.anyType;
            }
            return binex;
        };
        TypeFlow.prototype.typeCheckInOperator = function (binex) {
            binex.operand1 = this.cast(this.typeCheck(binex.operand1), this.stringType);
            binex.operand2 = this.typeCheck(binex.operand2);
            if(!((binex.operand1.type == this.checker.anyType || binex.operand1.type == this.checker.stringType) && (binex.operand2.type == this.anyType || this.checker.sourceIsSubtypeOfTarget(binex.operand2.type, this.objectInterfaceType)))) {
                this.checker.errorReporter.simpleError(binex, "The in operator requires the left operand to be of type Any or the String primitive type, and the right operand to be of type Any or an object type");
            }
            binex.type = this.booleanType;
            return binex;
        };
        TypeFlow.prototype.typeCheckShift = function (binex, assignment) {
            binex.operand1 = this.cast(this.typeCheck(binex.operand1), this.doubleType);
            binex.operand2 = this.cast(this.typeCheck(binex.operand2), this.doubleType);
            if(assignment && (!(this.astIsWriteable(binex.operand1)))) {
                this.checker.errorReporter.valueCannotBeModified(binex);
            }
            binex.type = this.doubleType;
            return binex;
        };
        TypeFlow.prototype.typeCheckQMark = function (trinex) {
            trinex.operand1 = this.typeCheck(trinex.operand1);
            trinex.operand2 = this.typeCheck(trinex.operand2);
            trinex.operand3 = this.typeCheck(trinex.operand3);
            var leftType = trinex.operand2.type;
            var rightType = trinex.operand3.type;
            if(leftType == rightType) {
                trinex.type = leftType;
            } else {
                if(this.checker.sourceIsSubtypeOfTarget(leftType, rightType)) {
                    trinex.type = rightType;
                } else if(this.checker.sourceIsSubtypeOfTarget(rightType, leftType)) {
                    trinex.type = leftType;
                } else {
                    trinex.type = this.anyType;
                    this.checker.errorReporter.incompatibleTypes(trinex, leftType, rightType, trinex.printLabel(), this.scope);
                }
            }
            return trinex;
        };
        TypeFlow.prototype.addFormals = function (container, signature, table) {
            var len = signature.parameters.length;
            for(var i = 0; i < len; i++) {
                var symbol = signature.parameters[i];
                symbol.container = container;
                table.add(symbol.name, symbol);
            }
        };
        TypeFlow.prototype.addLocalsFromScope = function (scope, container, vars, table, isModContainer) {
            var len = vars.members.length;
            var hasArgsDef = false;
            for(var i = 0; i < len; i++) {
                var local = vars.members[i];
                if(((local.sym == null) || (local.sym.kind() != TypeScript.SymbolKind.Field))) {
                    var result = null;
                    if((result = table.lookup(local.id.text)) == null) {
                        var localVar = new TypeScript.ValueLocation();
                        localVar.typeLink = new TypeScript.TypeLink();
                        var varSym = null;
                        if(TypeScript.hasFlag(local.varFlags, TypeScript.VarFlags.Static)) {
                            local.varFlags |= TypeScript.VarFlags.LocalStatic;
                            varSym = new TypeScript.FieldSymbol(local.id.text, local.minChar, this.checker.locationInfo.unitIndex, true, localVar);
                        } else {
                            varSym = new TypeScript.VariableSymbol(local.id.text, local.minChar, this.checker.locationInfo.unitIndex, localVar);
                        }
                        varSym.transferVarFlags(local.varFlags);
                        localVar.symbol = varSym;
                        varSym.declAST = local;
                        localVar.typeLink.ast = local.typeExpr;
                        this.checker.resolveTypeLink(scope, localVar.typeLink, false);
                        if((local.type == null) && (local.init == null)) {
                            local.type = this.anyType;
                        }
                        localVar.typeLink.type = local.type;
                        localVar.symbol.container = container;
                        local.sym = localVar.symbol;
                        table.add(local.id.text, varSym);
                        if(local.id.text == "arguments") {
                            hasArgsDef = true;
                        }
                    } else {
                        local.type = result.getType();
                        local.sym = result;
                    }
                }
            }
            if(!isModContainer) {
                if(!hasArgsDef) {
                    var argLoc = new TypeScript.ValueLocation();
                    argLoc.typeLink = new TypeScript.TypeLink();
                    var theArgSym = new TypeScript.VariableSymbol("arguments", vars.minChar, this.checker.locationInfo.unitIndex, argLoc);
                    if(!this.iargumentsInterfaceType) {
                        var argumentsSym = scope.find("IArguments", false, true);
                        if(argumentsSym) {
                            argumentsSym.flags |= TypeScript.SymbolFlags.CompilerGenerated;
                            this.iargumentsInterfaceType = argumentsSym.getType();
                        } else {
                            this.iargumentsInterfaceType = this.anyType;
                        }
                    }
                    argLoc.typeLink.type = this.iargumentsInterfaceType;
                    table.add("arguments", theArgSym);
                }
            }
        };
        TypeFlow.prototype.addConstructorLocalArgs = function (container, args, table, isClass) {
            if(args) {
                var len = args.members.length;
                for(var i = 0; i < len; i++) {
                    var local = args.members[i];
                    if((local.sym == null) || (isClass || (local.sym.kind() != TypeScript.SymbolKind.Field))) {
                        var result = null;
                        if((result = table.lookup(local.id.text)) == null) {
                            this.resolveBoundDecl(local);
                            var localVar = new TypeScript.ValueLocation();
                            localVar.typeLink = new TypeScript.TypeLink();
                            var varSym = new TypeScript.ParameterSymbol(local.id.text, local.minChar, this.checker.locationInfo.unitIndex, localVar);
                            varSym.declAST = local;
                            localVar.symbol = varSym;
                            localVar.typeLink.type = local.type;
                            localVar.symbol.container = container;
                            local.sym = localVar.symbol;
                            table.add(local.id.text, varSym);
                        } else {
                            local.type = result.getType();
                            local.sym = result;
                        }
                    }
                }
            }
        };
        TypeFlow.prototype.checkInitSelf = function (funcDecl) {
            if(!funcDecl.isMethod()) {
                var freeVars = funcDecl.freeVariables;
                for(var k = 0, len = freeVars.length; k < len; k++) {
                    var sym = freeVars[k];
                    if(sym.isInstanceProperty()) {
                        return true;
                    }
                }
            }
            var fns = funcDecl.scopes;
            var fnsLen = fns.members.length;
            for(var j = 0; j < fnsLen; j++) {
                var fn = fns.members[j];
                if(this.checkInitSelf(fn)) {
                    return true;
                }
            }
            return false;
        };
        TypeFlow.prototype.checkPromoteFreeVars = function (funcDecl, constructorSym) {
            var freeVars = funcDecl.freeVariables;
            for(var k = 0, len = freeVars.length; k < len; k++) {
                var sym = freeVars[k];
                if((!sym.isInstanceProperty()) && (sym.container == constructorSym)) {
                    TypeScript.instanceFilter.reset();
                    if(this.scope.search(TypeScript.instanceFilter, sym.name, false, false)) {
                        this.checker.errorReporter.simpleError(funcDecl, "Constructor-local variable shadows class property '" + sym.name + "'. To access the class property, use 'self." + sym.name + "'");
                    }
                    this.checker.errorReporter.simpleError(funcDecl, "Constructor-local variables may not be accessed from instance method bodies. Consider changing local variable '" + sym.name + "' to a class property");
                }
            }
        };
        TypeFlow.prototype.allReturnsAreVoid = function (funcDecl) {
            var allReturnsAreVoid = true;
            if(funcDecl.signature.returnType.type == null) {
                var preFindReturnExpressionTypes = function (ast, parent, walker) {
                    var go = true;
                    switch(ast.nodeType) {
                        case TypeScript.NodeType.FuncDecl:
                            go = false;
                            break;
                        case TypeScript.NodeType.Return:
                            var returnStmt = ast;
                            if(returnStmt.returnExpression) {
                                allReturnsAreVoid = false;
                                go = false;
                            }
                        default:
                            break;
                    }
                    walker.options.goChildren = go;
                    walker.options.goNextSibling = go;
                    return ast;
                };
                TypeScript.getAstWalkerFactory().walk(funcDecl.bod, preFindReturnExpressionTypes);
            }
            return allReturnsAreVoid;
        };
        TypeFlow.prototype.classConstructorHasSuperCall = function (funcDecl) {
            var foundSuper = false;
            var preFindSuperCall = function (ast, parent, walker) {
                var go = true;
                switch(ast.nodeType) {
                    case TypeScript.NodeType.FuncDecl:
                        go = false;
                        break;
                    case TypeScript.NodeType.Call:
                        var call = ast;
                        if(call.target.nodeType == TypeScript.NodeType.Super) {
                            go = false;
                            foundSuper = true;
                            break;
                        }
                        break;
                    default:
                        break;
                }
                walker.options.goChildren = go;
                return ast;
            };
            TypeScript.getAstWalkerFactory().walk(funcDecl.bod, preFindSuperCall);
            return foundSuper;
        };
        TypeFlow.prototype.baseListPrivacyErrorReporter = function (bases, i, declSymbol, extendsList, typeName, isModuleName) {
            var baseSymbol = bases.members[i].type.symbol;
            var declTypeString = (declSymbol.declAST.nodeType == TypeScript.NodeType.InterfaceDeclaration) ? "interface" : "class";
            var baseListTypeString = extendsList ? "extends" : "implements";
            var baseTypeString = (baseSymbol.declAST.nodeType == TypeScript.NodeType.InterfaceDeclaration) ? "interface" : "class";
            var typestring = "";
            if(isModuleName) {
                var quotestring = "";
                if(!TypeScript.isQuoted(typeName)) {
                    quotestring = "'";
                }
                typestring = " is using inaccessible module ";
                baseTypeString = " " + baseTypeString + " from private module " + quotestring + typeName + quotestring;
            } else {
                baseTypeString = " private " + baseTypeString + " '" + typeName + "'";
            }
            this.checker.errorReporter.simpleError(bases.members[i], "exported " + declTypeString + " '" + declSymbol.name + "' " + baseListTypeString + baseTypeString);
        };
        TypeFlow.prototype.typeCheckBaseListPrivacy = function (bases, declSymbol, extendsList) {
            var _this = this;
            if(bases) {
                var basesLen = bases.members.length;
                for(var i = 0; i < basesLen; i++) {
                    if(!bases.members[i].type || bases.members[i].type == this.checker.anyType) {
                        continue;
                    }
                    this.checkSymbolPrivacy(bases.members[i].type.symbol, declSymbol, function (typeName, isModuleName) {
                        return _this.baseListPrivacyErrorReporter(bases, i, declSymbol, extendsList, typeName, isModuleName);
                    });
                }
            }
        };
        TypeFlow.prototype.checkSymbolPrivacy = function (typeSymbol, declSymbol, errorCallback) {
            var externalModuleSymbol = null;
            var declSymbolPath = null;
            if(typeSymbol.isExternallyVisible(this.checker)) {
                var typeSymbolPath = typeSymbol.pathToRoot();
                declSymbolPath = declSymbol.pathToRoot();
                var typeSymbolLength = typeSymbolPath.length;
                var declSymbolPathLength = declSymbolPath.length;
                if(typeSymbolLength > 0) {
                    if(typeSymbolPath[typeSymbolLength - 1].getType().isModuleType() && (typeSymbolPath[typeSymbolLength - 1]).isDynamic && typeSymbolPath[typeSymbolLength - 1] != declSymbolPath[declSymbolPathLength - 1]) {
                        externalModuleSymbol = typeSymbolPath[typeSymbolLength - 1];
                    } else if(typeSymbolLength > 1) {
                        if(typeSymbolPath[typeSymbolLength - 2].getType().isModuleType() && (typeSymbolPath[typeSymbolLength - 2]).isDynamic && (declSymbolPathLength == 1 || typeSymbolPath[typeSymbolLength - 2] != declSymbolPath[declSymbolPathLength - 2])) {
                            externalModuleSymbol = typeSymbolPath[typeSymbolLength - 2];
                        }
                    }
                }
                if(externalModuleSymbol == null) {
                    return;
                }
            }
            var interfaceDecl = declSymbol.getInterfaceDeclFromSymbol(this.checker);
            if(interfaceDecl && !TypeScript.hasFlag(interfaceDecl.varFlags, TypeScript.VarFlags.Exported)) {
                return;
            }
            var checkVisibilitySymbol = declSymbol;
            var varDecl = declSymbol.getVarDeclFromSymbol();
            if(varDecl) {
                if(TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Private)) {
                    return;
                } else if(TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Public)) {
                    checkVisibilitySymbol = declSymbol.container;
                }
            }
            if(checkVisibilitySymbol.isExternallyVisible(this.checker)) {
                var privateSymbolName = typeSymbol.name;
                if(externalModuleSymbol != null) {
                    var prettyName = externalModuleSymbol.getPrettyNameOfDynamicModule(declSymbolPath);
                    if(prettyName != null) {
                        this.currentScript.AddExternallyVisibleImportedSymbol(prettyName.symbol, this.checker);
                        return;
                    } else {
                        privateSymbolName = externalModuleSymbol.prettyName;
                    }
                }
                errorCallback(privateSymbolName, typeSymbol.name != privateSymbolName);
            }
        };
        TypeFlow.prototype.checkTypePrivacy = function (type, declSymbol, errorCallback) {
            var _this = this;
            if(!(type && type.primitiveTypeClass == TypeScript.Primitive.None)) {
                return;
            }
            if(type.isArray()) {
                return this.checkTypePrivacy(type.elementType, declSymbol, errorCallback);
            }
            if(type.symbol && type.symbol.name && type.symbol.name != "_anonymous" && (((type.call == null) && (type.construct == null) && (type.index == null)) || (type.members && (!type.isClass())))) {
                return this.checkSymbolPrivacy(type.symbol, declSymbol, errorCallback);
            }
            if(type.members) {
                type.members.allMembers.map(function (key, s, unused) {
                    var sym = s;
                    if(!TypeScript.hasFlag(sym.flags, TypeScript.SymbolFlags.BuiltIn)) {
                        _this.checkTypePrivacy(sym.getType(), declSymbol, errorCallback);
                    }
                }, null);
            }
            this.checkSignatureGroupPrivacy(type.call, declSymbol, errorCallback);
            this.checkSignatureGroupPrivacy(type.construct, declSymbol, errorCallback);
            this.checkSignatureGroupPrivacy(type.index, declSymbol, errorCallback);
        };
        TypeFlow.prototype.checkSignatureGroupPrivacy = function (sgroup, declSymbol, errorCallback) {
            if(sgroup) {
                var len = sgroup.signatures.length;
                for(var i = 0; i < sgroup.signatures.length; i++) {
                    var signature = sgroup.signatures[i];
                    if(len > 1 && signature == sgroup.definitionSignature) {
                        continue;
                    }
                    if(signature.returnType) {
                        this.checkTypePrivacy(signature.returnType.type, declSymbol, errorCallback);
                    }
                    var paramLen = signature.parameters.length;
                    for(var j = 0; j < paramLen; j++) {
                        var param = signature.parameters[j];
                        this.checkTypePrivacy(param.getType(), declSymbol, errorCallback);
                    }
                }
            }
        };
        TypeFlow.prototype.functionArgumentPrivacyErrorReporter = function (funcDecl, p, paramSymbol, typeName, isModuleName) {
            var isGetter = funcDecl.isAccessor() && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.GetAccessor);
            var isSetter = funcDecl.isAccessor() && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.SetAccessor);
            var isPublicFunc = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Public);
            var isContainerInterface = funcDecl.type.symbol.getInterfaceDeclFromSymbol(this.checker) != null;
            var typestring = "";
            if(isModuleName) {
                var quotestring = "";
                if(!TypeScript.isQuoted(typeName)) {
                    quotestring = "'";
                }
                typestring = " is using inaccessible module " + quotestring + typeName + quotestring;
            } else {
                typestring = " has or is using private type '" + typeName + "'";
            }
            if(!isContainerInterface) {
                if(funcDecl.isConstructor) {
                    this.checker.errorReporter.simpleError(funcDecl.arguments.members[p], "exported class's constructor parameter '" + paramSymbol.name + "'" + typestring);
                } else if(isSetter) {
                    this.checker.errorReporter.simpleError(funcDecl.arguments.members[p], (isPublicFunc ? "public" : "exported") + " setter parameter '" + paramSymbol.name + "'" + typestring);
                } else if(!isGetter) {
                    this.checker.errorReporter.simpleError(funcDecl.arguments.members[p], (isPublicFunc ? "public" : "exported") + " function parameter '" + paramSymbol.name + "'" + typestring);
                }
            } else {
                if(funcDecl.isConstructMember()) {
                    this.checker.errorReporter.simpleError(funcDecl.arguments.members[p], "exported interface's constructor parameter '" + paramSymbol.name + "'" + typestring);
                } else if(funcDecl.isCallMember()) {
                    this.checker.errorReporter.simpleError(funcDecl.arguments.members[p], "exported interface's call parameter '" + paramSymbol.name + "'" + typestring);
                } else if(!funcDecl.isIndexerMember()) {
                    this.checker.errorReporter.simpleError(funcDecl.arguments.members[p], "exported interface's function parameter '" + paramSymbol.name + "'" + typestring);
                }
            }
        };
        TypeFlow.prototype.returnTypePrivacyError = function (astError, funcDecl, typeName, isModuleName) {
            var isGetter = funcDecl.isAccessor() && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.GetAccessor);
            var isSetter = funcDecl.isAccessor() && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.SetAccessor);
            var isPublicFunc = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Public);
            var isContainerInterface = funcDecl.type.symbol.getInterfaceDeclFromSymbol(this.checker) != null;
            var typestring = "";
            if(isModuleName) {
                var quotestring = "";
                if(!TypeScript.isQuoted(typeName)) {
                    quotestring = "'";
                }
                typestring = " is using inaccessible module " + quotestring + typeName + quotestring;
            } else {
                typestring = " has or is using private type '" + typeName + "'";
            }
            if(!isContainerInterface) {
                if(isGetter) {
                    this.checker.errorReporter.simpleError(astError, (isPublicFunc ? "public" : "exported") + " getter return type" + typestring);
                } else if(!isSetter) {
                    this.checker.errorReporter.simpleError(astError, (isPublicFunc ? "public" : "exported") + " function return type" + typestring);
                }
            } else {
                if(funcDecl.isConstructMember()) {
                    this.checker.errorReporter.simpleError(astError, "exported interface's constructor return type" + typestring);
                } else if(funcDecl.isCallMember()) {
                    this.checker.errorReporter.simpleError(astError, "exported interface's call return type" + typestring);
                } else if(funcDecl.isIndexerMember()) {
                    this.checker.errorReporter.simpleError(astError, "exported interface's indexer return type" + typestring);
                } else {
                    this.checker.errorReporter.simpleError(astError, "exported interface's function return type" + typestring);
                }
            }
        };
        TypeFlow.prototype.functionReturnTypePrivacyErrorReporter = function (funcDecl, signature, typeName, isModuleName) {
            var reportOnFuncDecl = false;
            if(funcDecl.returnTypeAnnotation != null && funcDecl.returnTypeAnnotation.type == signature.returnType.type) {
                this.returnTypePrivacyError(funcDecl.returnTypeAnnotation, funcDecl, typeName, isModuleName);
            }
            for(var i = 0; i < funcDecl.returnStatementsWithExpressions.length; i++) {
                if(funcDecl.returnStatementsWithExpressions[i].type == signature.returnType.type) {
                    this.returnTypePrivacyError(funcDecl.returnStatementsWithExpressions[i], funcDecl, typeName, isModuleName);
                } else {
                    reportOnFuncDecl = true;
                }
            }
            if(reportOnFuncDecl) {
                this.returnTypePrivacyError(funcDecl, funcDecl, typeName, isModuleName);
            }
        };
        TypeFlow.prototype.typeCheckFunction = function (funcDecl) {
            var _this = this;
            this.nestingLevel = 0;
            var fnType = funcDecl.type;
            var fgSym = fnType.symbol;
            var signature = funcDecl.signature;
            if(this.checker.typeStatusIsFinished(signature.typeCheckStatus)) {
                return funcDecl;
            } else if(signature.typeCheckStatus == TypeScript.TypeCheckStatus.Started) {
                if(!funcDecl.returnTypeAnnotation && funcDecl.bod && !funcDecl.isSignature() && !(funcDecl.isConstructor) && this.allReturnsAreVoid(funcDecl)) {
                    signature.returnType.type = this.voidType;
                    return funcDecl;
                } else {
                    if(funcDecl.returnTypeAnnotation == null) {
                        if(this.checker.styleSettings.implicitAny) {
                            this.checker.errorReporter.styleError(funcDecl, "type implicitly set to 'any'");
                        }
                        signature.returnType.type = this.anyType;
                        fgSym.flags |= TypeScript.SymbolFlags.RecursivelyReferenced;
                    }
                    return funcDecl;
                }
            }
            signature.typeCheckStatus = TypeScript.TypeCheckStatus.Started;
            this.checker.addStartedPTO(signature);
            var prevScope = this.scope;
            var prevFnc = this.thisFnc;
            var prevMethodStatus = this.enclosingFncIsMethod;
            var prevClassNode = this.thisClassNode;
            this.enclosingFncIsMethod = funcDecl.isMethod() || funcDecl.isConstructor;
            this.thisFnc = funcDecl;
            var container = funcDecl.type.symbol;
            var prevThisType = this.thisType;
            var prevLocationInfo = this.checker.locationInfo;
            var funcTable = null;
            var acceptedContextualType = false;
            var targetParams = null;
            var targetReturnType = null;
            var isGetter = funcDecl.isAccessor() && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.GetAccessor);
            var isSetter = funcDecl.isAccessor() && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.SetAccessor);
            var accessorType = (isGetter || isSetter) && funcDecl.accessorSymbol ? funcDecl.accessorSymbol.getType() : null;
            var prevModDecl = this.checker.currentModDecl;
            if(funcDecl.isConstructor && !funcDecl.isOverload) {
                if(fnType.instanceType == null) {
                    this.checker.errorReporter.simpleError(funcDecl, "Malformed function body (is this a class named the same as an existing interface?)");
                    return funcDecl;
                }
                this.scope = fnType.instanceType.constructorScope;
                var ssb = this.scope;
                funcTable = ssb.valueMembers.allMembers;
            } else if((funcDecl.isSpecialFn() && !(funcDecl.fncFlags & TypeScript.FncFlags.Signature)) || funcDecl.isOverload) {
                funcTable = funcDecl.symbols;
                if(!TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Static) && fnType.containedScope) {
                    this.scope = fnType.containedScope;
                }
            } else {
                if(funcDecl.bod) {
                    this.scope = fnType.containedScope;
                }
                var ssb = this.scope;
                if(ssb && ssb.valueMembers) {
                    funcTable = ssb.valueMembers.allMembers;
                }
            }
            if(funcDecl.isConstructor && funcDecl.bod && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod)) {
                var hasBaseType = TypeScript.hasFlag(funcDecl.classDecl.type.instanceType.typeFlags, TypeScript.TypeFlags.HasBaseType);
                var noSuperCallAllowed = !hasBaseType || TypeScript.hasFlag(funcDecl.classDecl.type.instanceType.typeFlags, TypeScript.TypeFlags.HasBaseTypeOfObject);
                var superCallMustBeFirst = TypeScript.hasFlag((funcDecl.classDecl).varFlags, TypeScript.VarFlags.ClassSuperMustBeFirstCallInConstructor);
                if(noSuperCallAllowed && this.classConstructorHasSuperCall(funcDecl)) {
                    this.checker.errorReporter.simpleError(funcDecl, "Calls to 'super' constructor are not allowed in classes that either inherit directly from 'Object' or have no base class");
                } else if(hasBaseType) {
                    if(superCallMustBeFirst) {
                        if(!funcDecl.bod || !funcDecl.bod.members.length || !((funcDecl.bod.members[0].nodeType == TypeScript.NodeType.Call && (funcDecl.bod.members[0]).target.nodeType == TypeScript.NodeType.Super) || (TypeScript.hasFlag(funcDecl.bod.flags, TypeScript.ASTFlags.StrictMode) && funcDecl.bod.members.length > 1 && funcDecl.bod.members[1].nodeType == TypeScript.NodeType.Call && (funcDecl.bod.members[1]).target.nodeType == TypeScript.NodeType.Super))) {
                            this.checker.errorReporter.simpleError(funcDecl, "If a derived class contains initialized properties or constructor parameter properties, the first statement in the constructor body must be a call to the super constructor");
                        }
                    } else if(!this.classConstructorHasSuperCall(funcDecl)) {
                        this.checker.errorReporter.simpleError(funcDecl, "Constructors for derived classes must contain a call to the class's 'super' constructor");
                    }
                }
            }
            if(funcDecl.isMethod() && funcDecl.type.enclosingType) {
                var enclosingClassNode = null;
                if(funcDecl.type.enclosingType.symbol.declAST.nodeType == TypeScript.NodeType.FuncDecl) {
                    enclosingClassNode = (funcDecl.type.enclosingType.symbol.declAST).classDecl;
                } else if(funcDecl.type.enclosingType.symbol.declAST.nodeType == TypeScript.NodeType.ClassDeclaration) {
                    enclosingClassNode = funcDecl.type.enclosingType.symbol.declAST;
                }
                if(enclosingClassNode) {
                    this.thisClassNode = enclosingClassNode;
                }
            }
            if(fnType.enclosingType) {
                ;
                var enclosingSym = fnType.symbol.container;
                if(enclosingSym && enclosingSym.isType() && enclosingSym.getType().isClass()) {
                    enclosingSym = enclosingSym.container;
                }
                if(enclosingSym && enclosingSym.declAST && enclosingSym.declAST.nodeType == TypeScript.NodeType.ModuleDeclaration) {
                    this.checker.currentModDecl = enclosingSym.declAST;
                }
            }
            if(funcDecl.unitIndex > 0) {
                if(this.checker.units && (funcDecl.unitIndex < this.checker.units.length)) {
                    this.checker.locationInfo = this.checker.units[funcDecl.unitIndex];
                } else {
                    this.checker.locationInfo = TypeScript.unknownLocationInfo;
                }
            }
            if(fnType.enclosingType) {
                this.thisType = fnType.enclosingType;
            } else {
                this.thisType = prevThisType;
            }
            var paramLen = signature.parameters.length;
            if(!funcDecl.isConstructor && funcDecl.bod && !funcDecl.isSignature()) {
                var tmpParamScope = this.scope;
                var ssb = this.scope;
                if(!funcDecl.isMethod() && funcDecl.returnTypeAnnotation == null) {
                    if(prevScope && funcDecl.name && !funcDecl.name.isMissing()) {
                        var considerSym = prevScope.findAmbient(funcDecl.name.text, false, false);
                        if(considerSym && considerSym.declAST && considerSym.declAST.type) {
                            this.checker.setContextualType(considerSym.declAST.type, false);
                        }
                    }
                    if(this.checker.hasTargetType()) {
                        var candidateTypeContext = this.checker.getTargetTypeContext();
                        var candidateType = candidateTypeContext.contextualType;
                        if(this.checker.canContextuallyTypeFunction(candidateType, funcDecl, true)) {
                            var candidateSigs = candidateType.construct ? candidateType.construct : candidateType.call;
                            candidateTypeContext.targetSig = candidateSigs.signatures[0];
                            var candidateParams = candidateTypeContext.targetSig.parameters;
                            targetParams = candidateParams;
                            targetReturnType = candidateTypeContext.targetSig.returnType.type;
                            if(candidateTypeContext.targetSig.declAST) {
                                if(candidateTypeContext.targetSig.declAST.isConstructor) {
                                    funcDecl.isTargetTypedAsMethod = true;
                                } else if(candidateTypeContext.targetSig.declAST.isMethod()) {
                                    funcDecl.isTargetTypedAsMethod = true;
                                }
                            }
                            fgSym.type = candidateTypeContext.contextualType;
                            acceptedContextualType = true;
                        } else if(candidateType && funcDecl.isAccessor()) {
                            accessorType = candidateType;
                            candidateTypeContext.targetAccessorType = accessorType;
                        } else {
                            this.checker.killCurrentContextualType();
                        }
                    }
                }
                var paramTable = ssb.valueMembers;
                this.scope = new TypeScript.SymbolScopeBuilder(paramTable, null, null, null, prevScope, container);
                for(var p = 0; p < paramLen; p++) {
                    var symbol = signature.parameters[p];
                    var ast = symbol.declAST;
                    if(this.checker.hasTargetType() && (targetParams && (this.checker.getTargetTypeContext().targetSig.hasVariableArgList || p < targetParams.length))) {
                        var candidateTypeContext = this.checker.getTargetTypeContext();
                        var hasVarArgList = candidateTypeContext.targetSig.hasVariableArgList;
                        ast.type = hasVarArgList && p >= targetParams.length - 1 ? targetParams[targetParams.length - 1].getType().elementType : targetParams[p].getType();
                        ast.sym.setType(ast.type);
                        (ast.sym).typeCheckStatus = this.checker.getTypeCheckFinishedStatus();
                    } else {
                        this.typeCheck(ast);
                    }
                    if(isSetter && accessorType) {
                        ast = this.cast(ast, accessorType);
                    }
                    symbol.container = container;
                    this.checkTypePrivacy(symbol.getType(), container, function (typeName, isModuleName) {
                        return _this.functionArgumentPrivacyErrorReporter(funcDecl, p, symbol, typeName, isModuleName);
                    });
                    paramTable.publicMembers.add(symbol.name, symbol);
                }
                this.scope = tmpParamScope;
            } else {
                this.typeCheck(funcDecl.arguments);
                for(var p = 0; p < paramLen; p++) {
                    signature.parameters[p].parameter.typeLink.type = funcDecl.arguments.members[p].type;
                    this.checkTypePrivacy(signature.parameters[p].getType(), container, function (typeName, isModuleName) {
                        return _this.functionArgumentPrivacyErrorReporter(funcDecl, p, signature.parameters[p], typeName, isModuleName);
                    });
                    if((funcDecl.arguments.members[p]).parameterPropertySym) {
                        (funcDecl.arguments.members[p]).parameterPropertySym.setType(funcDecl.arguments.members[p].type);
                    }
                }
                if((funcDecl.fncFlags & TypeScript.FncFlags.IndexerMember)) {
                    if(!paramLen || paramLen > 1) {
                        this.checker.errorReporter.simpleError(funcDecl, "Index signatures may take one and only one parameter");
                    } else if(funcDecl.arguments.members[0].type == this.checker.numberType) {
                        fnType.index.flags |= TypeScript.SignatureFlags.IsNumberIndexer;
                    } else if(funcDecl.arguments.members[0].type == this.checker.stringType) {
                        fnType.index.flags |= TypeScript.SignatureFlags.IsStringIndexer;
                    } else {
                        this.checker.errorReporter.simpleError(funcDecl.arguments.members[0], "Index signatures may only take 'string' or 'number' as their parameter");
                    }
                }
            }
            if(funcDecl.bod && (!funcDecl.isSignature())) {
                if(!(funcDecl.isConstructor)) {
                    this.addFormals(container, signature, funcTable);
                } else {
                    this.addConstructorLocalArgs(funcDecl.type.symbol, funcDecl.arguments, funcTable, TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod));
                    if(this.thisClassNode && this.thisClassNode.extendsList) {
                        var tmpScope = this.scope;
                        var funcMembers = new TypeScript.ScopedMembers(funcTable);
                        this.scope = new TypeScript.FilteredSymbolScopeBuilder(funcMembers, prevScope, funcDecl.type.symbol, function (sym) {
                            return sym.kind() == TypeScript.SymbolKind.Parameter;
                        });
                        this.typeCheckBaseCalls(this.thisClassNode.extendsList);
                        this.scope = tmpScope;
                    }
                }
                var prevMod = this.checker.currentModDecl;
                if(funcDecl.type && funcDecl.type.symbol && !funcDecl.isMethod() && funcDecl.type.symbol.declModule) {
                    this.checker.currentModDecl = funcDecl.type.symbol.declModule;
                }
                if(acceptedContextualType) {
                    this.checker.setContextualType(null, this.checker.inProvisionalTypecheckMode());
                }
                this.typeCheck(funcDecl.bod);
                if(acceptedContextualType) {
                    this.checker.unsetContextualType();
                }
                this.checker.currentModDecl = prevMod;
                if(this.checker.checkControlFlow) {
                    var cfg = funcDecl.buildControlFlow();
                    if(this.checker.printControlFlowGraph) {
                        cfg.print(this.checker.errorReporter.outfile);
                    }
                    cfg.reportUnreachable(this.checker.errorReporter);
                    if(this.checker.checkControlFlowUseDef) {
                        cfg.useDef(this.checker.errorReporter, funcDecl.type.symbol);
                    }
                }
                if(funcDecl.isConstructor) {
                    var fns = funcDecl.scopes;
                    var fnsLen = fns.members.length;
                    var freeVars;
                    var sym;
                    var j = 0;
                    for(; j < fnsLen; j++) {
                        var fn = fns.members[j];
                        if(!fn.isSignature()) {
                            if(TypeScript.hasFlag(fn.fncFlags, TypeScript.FncFlags.Method) && (!TypeScript.hasFlag(fn.fncFlags, TypeScript.FncFlags.Static))) {
                                this.checkPromoteFreeVars(fn, funcDecl.type.symbol);
                            }
                        }
                    }
                }
            }
            this.scope = prevScope;
            this.thisFnc = prevFnc;
            this.thisClassNode = prevClassNode;
            this.enclosingFncIsMethod = prevMethodStatus;
            this.thisType = prevThisType;
            this.checker.locationInfo = prevLocationInfo;
            this.checker.currentModDecl = prevModDecl;
            signature.typeCheckStatus = this.checker.getTypeCheckFinishedStatus();
            if(funcDecl.returnTypeAnnotation) {
                this.checkForVoidConstructor(funcDecl.returnTypeAnnotation.type, funcDecl.returnTypeAnnotation);
                if(signature.returnType.type == null) {
                    this.checker.resolveTypeLink(this.scope, signature.returnType, false);
                }
            } else if(targetReturnType) {
                signature.returnType.type = targetReturnType;
            }
            if(!(fgSym.flags & TypeScript.SymbolFlags.RecursivelyReferenced) && funcDecl.returnStatementsWithExpressions.length > 0) {
                var collection = {
                    getLength: function () {
                        return funcDecl.returnStatementsWithExpressions.length;
                    },
                    setTypeAtIndex: function (index, type) {
                        funcDecl.returnStatementsWithExpressions[index].type = type;
                    },
                    getTypeAtIndex: function (index) {
                        return funcDecl.returnStatementsWithExpressions[index].type;
                    }
                };
                var bestCommonReturnType = funcDecl.returnStatementsWithExpressions[0].type;
                bestCommonReturnType = this.checker.findBestCommonType(bestCommonReturnType, null, collection, true);
                if(bestCommonReturnType) {
                    signature.returnType.type = this.checker.widenType(bestCommonReturnType);
                } else {
                    for(var i = 0; i < funcDecl.returnStatementsWithExpressions.length; i++) {
                        this.checker.errorReporter.simpleError(funcDecl.returnStatementsWithExpressions[i], "Incompatible return type");
                    }
                    signature.returnType.type = this.anyType;
                }
            }
            var onlyHasThrow = false;
            if(signature.returnType.type == null) {
                if(TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.HasReturnExpression)) {
                    if(this.checker.styleSettings.implicitAny) {
                        this.checker.errorReporter.styleError(funcDecl, "type implicitly set to 'any'");
                    }
                    signature.returnType.type = this.anyType;
                } else {
                    signature.returnType.type = this.voidType;
                }
            } else if(signature.returnType.type == this.nullType || signature.returnType.type == this.checker.undefinedType) {
                signature.returnType.type = this.anyType;
            } else if((signature.returnType.type != this.voidType && signature.returnType.type != this.checker.undefinedType && signature.returnType.type != this.anyType)) {
                if(!funcDecl.isSignature() && !funcDecl.isConstructor && !TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.HasReturnExpression) && !TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.IsFatArrowFunction)) {
                    onlyHasThrow = (funcDecl.bod.members.length > 0) && (funcDecl.bod.members[0].nodeType == TypeScript.NodeType.Throw);
                    if(!onlyHasThrow) {
                        this.checker.errorReporter.simpleError(funcDecl.returnTypeAnnotation || funcDecl, "Function declared a non-void return type, but has no return expression");
                    }
                }
                this.checkTypePrivacy(signature.returnType.type, container, function (typeName, isModuleName) {
                    return _this.functionReturnTypePrivacyErrorReporter(funcDecl, signature, typeName, isModuleName);
                });
            }
            if(funcDecl.accessorSymbol) {
                var accessorType = funcDecl.accessorSymbol.getType();
                if(!onlyHasThrow && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.GetAccessor) && !TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.HasReturnExpression)) {
                    this.checker.errorReporter.simpleError(funcDecl, "Getters must return a value");
                }
                if(accessorType) {
                    if((TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.GetAccessor) && accessorType != signature.returnType.type) || (funcDecl.arguments.members.length > 0 && accessorType != funcDecl.arguments.members[0].type)) {
                        this.checker.errorReporter.simpleError(funcDecl, "Getter and setter types do not agree");
                    }
                } else {
                    if(TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.GetAccessor)) {
                        funcDecl.accessorSymbol.setType(signature.returnType.type);
                    } else {
                        if(funcDecl.arguments.members.length != 1) {
                            this.checker.errorReporter.simpleError(funcDecl, "Setters may have one and only one argument");
                        } else {
                            funcDecl.accessorSymbol.setType(funcDecl.arguments.members[0].type);
                        }
                    }
                }
            }
            this.typeCheckOverloadSignatures(fnType, funcDecl);
            return funcDecl;
        };
        TypeFlow.prototype.typeCheckBases = function (type) {
            var seenInterface = false;
            var bases = type.extendsList;
            var baseLinks = type.extendsTypeLinks;
            if(bases) {
                var len = bases.length;
                if(len > 0) {
                    type.typeFlags |= TypeScript.TypeFlags.HasBaseType;
                }
                for(var i = 0; i < len; i++) {
                    if(bases[i] == this.checker.anyType) {
                        baseLinks[i].type = null;
                        var oldErrors = this.checker.errorReporter.getCapturedErrors();
                        TypeScript.CompilerDiagnostics.assert(oldErrors.length == 0, "There shouldnt be any contextual errors when typechecking base type names");
                        this.checker.errorReporter.pushToErrorSink = true;
                        bases[i] = this.checker.resolveBaseTypeLink(baseLinks[i], type.containedScope);
                        this.checker.errorReporter.pushToErrorSink = false;
                        this.checker.errorReporter.freeCapturedErrors();
                    }
                    var base = bases[i];
                    var baseRef = baseLinks[i].ast;
                    var baseTypeOfObject = base.symbol && base.symbol.name == "Object" && base.symbol.container == this.checker.gloMod;
                    if(baseTypeOfObject) {
                        type.typeFlags |= TypeScript.TypeFlags.HasBaseTypeOfObject;
                    }
                    if(base.isClassInstance()) {
                        if(!(type.isClassInstance())) {
                            this.checker.errorReporter.simpleError(baseRef, "Interface base type must be interface");
                        } else {
                            if(seenInterface) {
                                this.checker.errorReporter.simpleError(baseRef, "Class may not follow interface as base type");
                            }
                        }
                    } else if(base.isModuleType()) {
                        this.checker.errorReporter.simpleError(baseRef, "Types may not be derived from module types");
                    } else if(base.members) {
                        if(!seenInterface) {
                            seenInterface = true;
                        }
                    } else {
                        if(!(type.isClassInstance())) {
                            this.checker.errorReporter.simpleError(baseRef, "Interface base type must be interface");
                        } else {
                            this.checker.errorReporter.simpleError(baseRef, "Base type must be interface or class");
                        }
                        break;
                    }
                }
            }
        };
        TypeFlow.prototype.checkMembersImplementInterfaces = function (implementingType) {
            var instanceType = implementingType.getInstanceType();
            if(instanceType.implementsList) {
                var len = instanceType.implementsList.length;
                for(var i = 0; i < len; i++) {
                    var interfaceType = instanceType.implementsList[i];
                    var comparisonInfo = new TypeScript.TypeComparisonInfo();
                    if(!this.checker.sourceIsSubtypeOfTarget(instanceType, interfaceType, comparisonInfo)) {
                        var emsg = "Class '" + instanceType.getTypeName() + "' declares interface '" + interfaceType.getTypeName() + "' but does not implement it";
                        if(!comparisonInfo.message) {
                            this.checker.errorReporter.simpleErrorFromSym(instanceType.symbol, emsg);
                        } else {
                            this.checker.errorReporter.simpleErrorFromSym(instanceType.symbol, emsg + ": " + comparisonInfo.message);
                        }
                    }
                }
            }
        };
        TypeFlow.prototype.typeCheckBaseCalls = function (bases) {
            if(bases == null) {
                return;
            }
            var basesLen = bases.members.length;
            for(var i = 0; i < basesLen; i++) {
                var baseExpr = bases.members[i];
                var baseSymbol = null;
                if(baseExpr.nodeType == TypeScript.NodeType.Call) {
                    this.typeCheckNew(baseExpr);
                }
            }
        };
        TypeFlow.prototype.assertUniqueNamesInBaseTypes = function (names, type, classDecl, checkUnique) {
            var _this = this;
            if(type) {
                if(type.members) {
                    type.members.publicMembers.map(function (key, s, c) {
                        var sym = s;
                        var dup = names.lookup(sym.name);
                        if(dup) {
                            if(checkUnique) {
                                _this.checker.errorReporter.simpleError(classDecl, "duplicate member name in bases for " + classDecl.name.actualText + ": " + type.symbol.name + " and " + dup.container.name + " both contain member with name " + sym.name);
                            }
                        } else {
                            names.add(sym.name, sym);
                        }
                    }, null);
                }
                if(type.extendsList) {
                    var len = type.extendsList.length;
                    for(var i = 0; i < len; i++) {
                        if(!(type.extendsList[i].symbol.flags & TypeScript.SymbolFlags.RecursivelyReferenced)) {
                            this.assertUniqueNamesInBaseTypes(names, type.extendsList[i], classDecl, checkUnique);
                        }
                    }
                }
            }
        };
        TypeFlow.prototype.checkBaseTypeMemberInheritance = function (derivedType, derivedTypeDecl) {
            var _this = this;
            var instanceType = derivedType.getInstanceType();
            if(instanceType.extendsList == null) {
                return;
            }
            var len = instanceType.extendsList.length;
            if(len > 0) {
                var names = new TypeScript.StringHashTable();
                if(instanceType.isClassInstance()) {
                    for(var i = 0; i < len; i++) {
                        this.assertUniqueNamesInBaseTypes(names, instanceType.extendsList[i], derivedTypeDecl, i > 0);
                    }
                }
                if(instanceType.members) {
                    instanceType.members.publicMembers.map(function (key, s, c) {
                        var sym = s;
                        for(var j = 0; j < len; j++) {
                            var base = instanceType.extendsList[j];
                            if(base.memberScope == null) {
                                _this.checker.errorReporter.simpleError(derivedTypeDecl, "Base type '" + base.symbol.name + "' lacks an implementation.");
                            } else {
                                var bSym = base.memberScope.find(sym.name, false, false);
                                if(bSym) {
                                    var aType = sym.getType();
                                    var bType = bSym.getType();
                                    if(!(_this.checker.sourceIsSubtypeOfTarget(aType, bType))) {
                                        _this.checker.errorReporter.simpleErrorFromSym(sym, "Type of overridden member '" + sym.name + "' is not subtype of original member defined by type '" + bSym.container.name + "'");
                                    } else if((sym.kind() == TypeScript.SymbolKind.Type) && (bSym.kind() == TypeScript.SymbolKind.Field)) {
                                        _this.checker.errorReporter.simpleErrorFromSym(sym, "Cannot override field '" + sym.name + "' with method");
                                    }
                                }
                            }
                        }
                    }, null);
                }
            }
        };
        TypeFlow.prototype.typeCheckClass = function (classDecl) {
            var typeSymbol = classDecl.type.symbol;
            if(typeSymbol.typeCheckStatus == TypeScript.TypeCheckStatus.Finished) {
                return classDecl;
            } else if(typeSymbol.typeCheckStatus == TypeScript.TypeCheckStatus.Started) {
                return classDecl;
            } else {
                typeSymbol.typeCheckStatus = TypeScript.TypeCheckStatus.Started;
                this.checker.addStartedPTO(typeSymbol);
            }
            var prevScope = this.scope;
            var svClassNode = this.thisClassNode;
            this.thisClassNode = classDecl;
            var classType = classDecl.type;
            this.typeCheckBases(classType.instanceType);
            this.typeCheckBaseListPrivacy(classDecl.extendsList, typeSymbol, true);
            this.typeCheckBaseListPrivacy(classDecl.implementsList, typeSymbol, false);
            var prevThisType = this.thisType;
            this.thisType = classType.instanceType;
            this.scope = classType.instanceType.containedScope;
            if(classDecl.constructorDecl) {
                this.scope = classType.instanceType.constructorScope;
                var ssb = this.scope;
                var funcTable = ssb.valueMembers.allMembers;
                this.addConstructorLocalArgs(classDecl.constructorDecl.type.symbol, classDecl.constructorDecl.arguments, funcTable, true);
            }
            this.typeCheck(classDecl.members);
            typeSymbol.typeCheckStatus = TypeScript.TypeCheckStatus.Finished;
            this.checkBaseTypeMemberInheritance(classType, classDecl);
            this.checkMembersImplementInterfaces(classType);
            this.typeCheckOverloadSignatures(classType, classDecl);
            this.typeCheckOverloadSignatures(classType.instanceType, classDecl);
            if(!classDecl.constructorDecl) {
                if(classDecl.extendsList && classDecl.extendsList.members.length && classDecl.extendsList.members[0].type && classDecl.extendsList.members[0].type.symbol.type.isClass()) {
                    TypeScript.cloneParentConstructGroupForChildType(classDecl.type, classDecl.extendsList.members[0].type.symbol.type);
                }
            }
            this.thisType = prevThisType;
            this.thisClassNode = svClassNode;
            this.scope = prevScope;
            return classDecl;
        };
        TypeFlow.prototype.typeCheckOverloadSignatures = function (type, ast) {
            if(type.call) {
                type.call.typeCheck(this.checker, ast, type.construct != null);
            }
            if(type.construct) {
                type.construct.typeCheck(this.checker, ast, false);
            }
            if(type.index) {
                type.index.typeCheck(this.checker, ast, false);
            }
        };
        TypeFlow.prototype.typeCheckInterface = function (interfaceDecl) {
            this.typeCheckBases(interfaceDecl.type);
            this.typeCheckBaseListPrivacy(interfaceDecl.extendsList, interfaceDecl.type.symbol, true);
            this.typeCheck(interfaceDecl.members);
            this.checkBaseTypeMemberInheritance(interfaceDecl.type, interfaceDecl);
            if(interfaceDecl.extendsList) {
                for(var i = 0; i < interfaceDecl.extendsList.members.length; i++) {
                    if(interfaceDecl.extendsList.members[i].type.call) {
                        if(interfaceDecl.type.call) {
                            interfaceDecl.type.call.signatures = interfaceDecl.type.call.signatures.concat(interfaceDecl.extendsList.members[i].type.call.signatures);
                        } else {
                            interfaceDecl.type.call = interfaceDecl.extendsList.members[i].type.call;
                        }
                    }
                    if(interfaceDecl.extendsList.members[i].type.construct) {
                        if(interfaceDecl.type.construct) {
                            interfaceDecl.type.construct.signatures = interfaceDecl.type.construct.signatures.concat(interfaceDecl.extendsList.members[i].type.construct.signatures);
                        } else {
                            interfaceDecl.type.construct = interfaceDecl.extendsList.members[i].type.construct;
                        }
                    }
                    if(interfaceDecl.extendsList.members[i].type.index) {
                        if(interfaceDecl.type.index) {
                            interfaceDecl.type.index.signatures = interfaceDecl.type.index.signatures.concat(interfaceDecl.extendsList.members[i].type.index.signatures);
                        } else {
                            interfaceDecl.type.index = interfaceDecl.extendsList.members[i].type.index;
                        }
                    }
                }
            }
            return interfaceDecl;
        };
        TypeFlow.prototype.typeCheckImportDecl = function (importDecl) {
            var mod = importDecl.alias.type;
            var sym = null;
            var prevInImportTC = this.inImportTypeCheck;
            this.inImportTypeCheck = true;
            this.typeCheck(importDecl.alias);
            mod = importDecl.alias.type;
            if(mod == null) {
                this.checker.errorReporter.simpleError(importDecl.alias, "Could not resolve module alias '" + importDecl.id.actualText + "'");
                mod = this.checker.anyType;
                (importDecl.id.sym).type = mod;
            }
            importDecl.id.type = mod;
            sym = mod.symbol;
            if(!mod.isModuleType()) {
                this.checker.errorReporter.simpleError(importDecl.alias, "A module cannot be aliased to a non-module type");
            } else {
                sym.type = mod;
                if(this.checker.typeFlow.currentScript && this.checker.typeFlow.currentScript.topLevelMod && this.checker.typeFlow.currentScript.topLevelMod.mod) {
                    this.checker.typeFlow.currentScript.topLevelMod.mod.importedModules.push(importDecl);
                }
                (importDecl.id.sym).type = mod;
                if(mod.symbol && mod.symbol.declAST) {
                    (mod.symbol.declAST).modFlags &= ~TypeScript.ModuleFlags.ShouldEmitModuleDecl;
                }
            }
            this.inImportTypeCheck = prevInImportTC;
            return importDecl;
        };
        TypeFlow.prototype.typeCheckModule = function (moduleDecl) {
            if(!moduleDecl.mod) {
                return moduleDecl;
            }
            if(this.currentScript) {
                this.currentScript.requiresGlobal = true;
            }
            var mod = moduleDecl.mod;
            var sym = null;
            var prevScope = this.scope;
            var prevThisType = this.thisType;
            var prevCurrentModDecl = this.checker.currentModDecl;
            this.checker.currentModDecl = moduleDecl;
            this.thisType = null;
            this.scope = mod.containedScope;
            this.typeCheck(moduleDecl.members);
            sym = mod.symbol;
            this.checker.currentModDecl = prevCurrentModDecl;
            this.thisType = prevThisType;
            this.scope = prevScope;
            moduleDecl.type = mod;
            if(sym) {
                sym.typeCheckStatus = TypeScript.TypeCheckStatus.Finished;
            }
            return moduleDecl;
        };
        TypeFlow.prototype.typeCheckFor = function (forStmt) {
            forStmt.init = this.typeCheck(forStmt.init);
            this.nestingLevel++;
            forStmt.cond = this.typeCheck(forStmt.cond);
            this.typeCheckCondExpr(forStmt.cond);
            forStmt.incr = this.typeCheck(forStmt.incr);
            this.nestingLevel--;
            forStmt.body = this.typeCheck(forStmt.body);
            this.typeCheckCompoundStmtBlock(forStmt.body, "for statement");
            forStmt.type = this.voidType;
            return forStmt;
        };
        TypeFlow.prototype.typeCheckWith = function (withStmt) {
            if(this.checker.errorsOnWith) {
                this.checker.errorReporter.simpleError(withStmt.expr, "All symbols within a 'with' block will be typed as 'any'");
            }
            withStmt.expr = this.typeCheck(withStmt.expr);
            this.checker.inWith = true;
            withStmt.body = this.typeCheck(withStmt.body);
            this.typeCheckCompoundStmtBlock(withStmt.body, "with statement");
            this.checker.inWith = false;
            return withStmt;
        };
        TypeFlow.prototype.typeCheckForIn = function (forInStmt) {
            forInStmt.obj = this.typeCheck(forInStmt.obj);
            forInStmt.lval = this.cast(this.typeCheck(forInStmt.lval), this.checker.stringType);
            if(forInStmt.lval.nodeType == TypeScript.NodeType.VarDecl) {
                var varDecl = forInStmt.lval;
                if(varDecl.typeExpr) {
                    this.checker.errorReporter.simpleError(varDecl, "Variable declarations for for/in expressions may not contain a type annotation");
                }
                if(varDecl.sym) {
                    varDecl.sym.setType(this.checker.stringType);
                }
            }
            forInStmt.body = this.typeCheck(forInStmt.body);
            this.typeCheckCompoundStmtBlock(forInStmt.body, "for in statement");
            return forInStmt;
        };
        TypeFlow.prototype.typeCheckWhile = function (whileStmt) {
            whileStmt.cond = this.typeCheck(whileStmt.cond);
            this.typeCheckCondExpr(whileStmt.cond);
            whileStmt.body = this.typeCheck(whileStmt.body);
            this.typeCheckCompoundStmtBlock(whileStmt.body, "while statement");
            whileStmt.type = this.voidType;
            return whileStmt;
        };
        TypeFlow.prototype.typeCheckDoWhile = function (doWhileStmt) {
            doWhileStmt.cond = this.typeCheck(doWhileStmt.cond);
            this.typeCheckCondExpr(doWhileStmt.cond);
            doWhileStmt.body = this.typeCheck(doWhileStmt.body);
            this.typeCheckCompoundStmtBlock(doWhileStmt.body, "do while statement");
            doWhileStmt.type = this.voidType;
            return doWhileStmt;
        };
        TypeFlow.prototype.typeCheckCondExpr = function (cond) {
            if(this.checker.styleSettings.assignmentInCond) {
                if((cond !== null) && (cond.nodeType >= TypeScript.NodeType.Asg) && (cond.nodeType <= TypeScript.NodeType.LastAsg)) {
                    this.checker.errorReporter.simpleError(cond, "top-level assignment statement in conditional expression");
                }
            }
        };
        TypeFlow.prototype.typeCheckCompoundStmtBlock = function (stmts, stmtType) {
            if(this.checker.styleSettings.blockInCompoundStmt && stmts) {
                if(stmts.nodeType != TypeScript.NodeType.Block) {
                    this.checker.errorReporter.styleError(stmts, stmtType + " requires a block");
                }
            }
        };
        TypeFlow.prototype.typeCheckIf = function (ifStmt) {
            ifStmt.cond = this.typeCheck(ifStmt.cond);
            this.typeCheckCondExpr(ifStmt.cond);
            ifStmt.thenBod = this.typeCheck(ifStmt.thenBod);
            ifStmt.elseBod = this.typeCheck(ifStmt.elseBod);
            this.typeCheckCompoundStmtBlock(ifStmt.thenBod, "if statement");
            this.typeCheckCompoundStmtBlock(ifStmt.elseBod, "if statement");
            ifStmt.type = this.voidType;
            return ifStmt;
        };
        TypeFlow.prototype.typeFromAccessorFuncDecl = function (funcDecl) {
            if(!funcDecl.isAccessor()) {
                return null;
            }
            if(TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.GetAccessor)) {
                return funcDecl.type.call.signatures[0].returnType.type;
            } else {
                return funcDecl.type.call.signatures[0].parameters[0].getType();
            }
        };
        TypeFlow.prototype.typeCheckObjectLit = function (objectLit) {
            var resultType = new TypeScript.Type();
            resultType.symbol = new TypeScript.TypeSymbol(this.checker.anon, objectLit.minChar, objectLit.limChar - objectLit.minChar, this.checker.locationInfo.unitIndex, resultType);
            resultType.members = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
            resultType.memberScope = new TypeScript.SymbolTableScope(resultType.members, null, null, null, null);
            var aggScope = new TypeScript.SymbolAggregateScope(resultType.symbol);
            aggScope.addParentScope(resultType.memberScope);
            aggScope.addParentScope(this.scope);
            resultType.containedScope = aggScope;
            var memberDecls = objectLit.operand;
            var prevThisType = this.thisType;
            var acceptTargetType = false;
            var targetType = null;
            if(this.checker.hasTargetType()) {
                targetType = this.checker.getTargetTypeContext().contextualType;
                if(targetType && targetType.symbol && !this.checker.typeStatusIsFinished(targetType.symbol.typeCheckStatus)) {
                    if(targetType.symbol.declAST) {
                        this.typeCheck(targetType.symbol.declAST);
                    }
                }
                acceptTargetType = true;
            }
            if(memberDecls) {
                for(var i = 0, len = memberDecls.members.length; i < len; i++) {
                    var binex = memberDecls.members[i];
                    var id = binex.operand1;
                    var text;
                    var targetMember = null;
                    var fieldSymbol = null;
                    if(id.nodeType == TypeScript.NodeType.Name) {
                        text = (id).text;
                    } else if(id.nodeType == TypeScript.NodeType.QString) {
                        var idText = (id).text;
                        text = idText.substring(1, idText.length - 1);
                    } else {
                        this.checker.errorReporter.simpleError(objectLit, "malformed object literal");
                        resultType = this.anyType;
                        break;
                    }
                    if(acceptTargetType && targetType.memberScope) {
                        targetMember = targetType.memberScope.find(text, false, false);
                    }
                    if(binex.operand2.nodeType == TypeScript.NodeType.FuncDecl && (binex.operand2).isAccessor()) {
                        var funcDecl = binex.operand2;
                        var accessorSym = resultType.members.publicMembers.lookup(text);
                        accessorSym = this.checker.createAccessorSymbol(funcDecl, accessorSym, resultType, true, false, resultType.memberScope, null);
                        funcDecl.accessorSymbol = accessorSym;
                        fieldSymbol = accessorSym;
                        if(id.nodeType == TypeScript.NodeType.Name) {
                            (id).sym = accessorSym;
                        }
                    }
                    this.checker.typeCheckWithContextualType(acceptTargetType && targetMember ? targetMember.getType() : null, false, acceptTargetType, binex.operand2);
                    if(acceptTargetType && targetMember) {
                        if((binex.operand2.type == this.anyType || this.checker.sourceIsAssignableToTarget(binex.operand2.type, targetMember.getType())) || (binex.operand2.nodeType == TypeScript.NodeType.FuncDecl && (binex.operand2).isAccessor() && this.typeFromAccessorFuncDecl(binex.operand2) == targetMember.getType())) {
                            binex.operand1.type = targetMember.getType();
                        }
                    } else {
                        binex.operand2.type = binex.operand2.type == this.checker.undefinedType ? this.anyType : binex.operand2.type;
                    }
                    if(fieldSymbol == null) {
                        var memberType = binex.operand2.type;
                        var field = new TypeScript.ValueLocation();
                        fieldSymbol = new TypeScript.FieldSymbol(text, id.minChar, this.checker.locationInfo.unitIndex, true, field);
                        fieldSymbol.flags |= TypeScript.SymbolFlags.Property;
                        field.symbol = fieldSymbol;
                        fieldSymbol.typeCheckStatus = this.checker.getTypeCheckFinishedStatus();
                        field.typeLink = new TypeScript.TypeLink();
                        field.typeLink.type = memberType;
                        resultType.members.publicMembers.add(text, fieldSymbol);
                    }
                    fieldSymbol.isObjectLitField = true;
                }
            }
            this.thisType = prevThisType;
            objectLit.type = resultType;
            if(targetType) {
                objectLit.targetType = targetType;
            }
        };
        TypeFlow.prototype.typeCheckArrayLit = function (arrayLit) {
            var elements = arrayLit.operand;
            var elementType = this.anyType;
            var targetElementType = null;
            var comparisonInfo = new TypeScript.TypeComparisonInfo();
            comparisonInfo.onlyCaptureFirstError = true;
            if(this.checker.hasTargetType()) {
                var targetType = this.checker.getTargetTypeContext().contextualType;
                if(targetType.elementType) {
                    targetElementType = targetType.elementType;
                }
            }
            if(elements) {
                var prevInArrayElemTypeCheck = this.inArrayElementTypeCheck;
                this.inArrayElementTypeCheck = true;
                this.checker.typeCheckWithContextualType(targetElementType, this.checker.inProvisionalTypecheckMode(), targetElementType != null, elements);
                this.inArrayElementTypeCheck = prevInArrayElemTypeCheck;
                elementType = elements.members[0].type;
                var collection = {
                    getLength: function () {
                        return elements.members.length;
                    },
                    setTypeAtIndex: function (index, type) {
                        elements.members[index].type = type;
                    },
                    getTypeAtIndex: function (index) {
                        return elements.members[index].type;
                    }
                };
                elementType = this.checker.findBestCommonType(elementType, targetElementType, collection, false, comparisonInfo);
                if(elementType == this.checker.undefinedType || (!prevInArrayElemTypeCheck && elementType == this.nullType)) {
                    elementType = this.anyType;
                }
            }
            if(!elementType) {
                var emsg = "Incompatible types in array literal expression";
                if(!comparisonInfo.message) {
                    this.checker.errorReporter.simpleError(arrayLit, emsg);
                } else {
                    this.checker.errorReporter.simpleError(arrayLit, emsg + ": " + comparisonInfo.message);
                }
                elementType = this.anyType;
            } else if(targetElementType) {
                if(this.checker.sourceIsAssignableToTarget(elementType, targetElementType)) {
                    elementType = targetElementType;
                }
            }
            arrayLit.type = this.checker.makeArrayType(elementType);
        };
        TypeFlow.prototype.checkForVoidConstructor = function (type, ast) {
            if(type && type.construct && type.construct.signatures.length > 0) {
                for(var i = 0; i < type.construct.signatures.length; i++) {
                    if(type.construct.signatures[i].returnType.type == this.checker.voidType) {
                        this.checker.errorReporter.simpleError(ast, "Constructors may not have a return type of 'void'");
                        break;
                    }
                }
            }
        };
        TypeFlow.prototype.typeCheckReturn = function (returnStmt) {
            if(this.thisFnc) {
                var targetType = null;
                if(this.checker.hasTargetType()) {
                    var tcContext = this.checker.getTargetTypeContext();
                    var accessorType = tcContext.targetAccessorType;
                    if(accessorType) {
                        targetType = accessorType;
                    } else {
                        var targetSig = this.checker.getTargetTypeContext().targetSig;
                        if(targetSig && targetSig.returnType.type != this.voidType) {
                            targetType = targetSig.returnType.type;
                        }
                    }
                }
                if(returnStmt.returnExpression) {
                    this.thisFnc.fncFlags |= TypeScript.FncFlags.HasReturnExpression;
                    if(targetType == null && this.thisFnc.returnTypeAnnotation && this.thisFnc.returnTypeAnnotation.type && this.thisFnc.returnTypeAnnotation.type != this.voidType) {
                        targetType = this.thisFnc.returnTypeAnnotation.type;
                    }
                    this.checker.typeCheckWithContextualType(targetType, this.checker.inProvisionalTypecheckMode(), targetType != null, returnStmt.returnExpression);
                    var expectedReturnType = (this.thisFnc.returnTypeAnnotation && this.thisFnc.returnTypeAnnotation.type) ? this.thisFnc.returnTypeAnnotation.type : targetType;
                    if(expectedReturnType) {
                        if(expectedReturnType == this.voidType && returnStmt.returnExpression.type != this.voidType) {
                            this.checker.errorReporter.simpleError(returnStmt, "Return with value expression in void function");
                            returnStmt.type = returnStmt.returnExpression.type;
                        } else {
                            returnStmt.returnExpression = this.cast(returnStmt.returnExpression, expectedReturnType);
                            returnStmt.type = expectedReturnType;
                        }
                    } else {
                        if(targetType) {
                            if(returnStmt.returnExpression.type != this.voidType) {
                                returnStmt.returnExpression = this.cast(returnStmt.returnExpression, targetType);
                            } else {
                                returnStmt.returnExpression.type = targetType;
                            }
                        }
                        returnStmt.type = returnStmt.returnExpression.type;
                    }
                    this.thisFnc.returnStatementsWithExpressions[this.thisFnc.returnStatementsWithExpressions.length] = returnStmt;
                } else {
                    returnStmt.type = targetType == null ? this.checker.voidType : targetType;
                }
            }
            return returnStmt;
        };
        TypeFlow.prototype.typeCheckInstOf = function (ast) {
            var binex = ast;
            binex.operand1 = this.typeCheck(binex.operand1);
            binex.operand2 = this.typeCheck(binex.operand2);
            if(!((binex.operand1.type == this.checker.anyType || this.checker.sourceIsSubtypeOfTarget(binex.operand1.type, this.objectInterfaceType)) && (binex.operand2.type == this.anyType || this.checker.sourceIsSubtypeOfTarget(binex.operand2.type, this.functionInterfaceType)))) {
                this.checker.errorReporter.simpleError(ast, "The instanceof operator requires the left operand to be of type Any or an object type, and the right operand to be of type Any or a subtype of the Function interface type");
            }
            binex.type = this.booleanType;
            return binex;
        };
        TypeFlow.prototype.typeCheckCommaOperator = function (ast) {
            var binex = ast;
            binex.operand1 = this.typeCheck(binex.operand1);
            binex.operand2 = this.typeCheck(binex.operand2);
            binex.type = binex.operand2.type;
            return binex;
        };
        TypeFlow.prototype.typeCheckLogOr = function (binex) {
            binex.operand1 = this.typeCheck(binex.operand1);
            binex.operand2 = this.typeCheck(binex.operand2);
            var leftType = binex.operand1.type;
            var rightType = binex.operand2.type;
            if(leftType == this.checker.anyType || rightType == this.checker.anyType) {
                binex.type = this.checker.anyType;
            } else if(leftType == this.checker.booleanType) {
                if(rightType == this.checker.booleanType) {
                    binex.type = this.checker.booleanType;
                } else {
                    binex.type = this.checker.anyType;
                }
            } else if(leftType == this.checker.numberType) {
                if(rightType == this.checker.numberType) {
                    binex.type = this.checker.numberType;
                } else {
                    binex.type = this.checker.anyType;
                }
            } else if(leftType == this.checker.stringType) {
                if(rightType == this.checker.stringType) {
                    binex.type = this.checker.stringType;
                } else {
                    binex.type = this.checker.anyType;
                }
            } else {
                if(this.checker.sourceIsSubtypeOfTarget(leftType, rightType)) {
                    binex.type = rightType;
                } else if(this.checker.sourceIsSubtypeOfTarget(rightType, leftType)) {
                    binex.type = leftType;
                } else {
                    binex.type = this.checker.anyType;
                }
            }
            return binex;
        };
        TypeFlow.prototype.typeCheckLogAnd = function (binex) {
            binex.operand1 = this.typeCheck(binex.operand1);
            binex.operand2 = this.typeCheck(binex.operand2);
            binex.type = binex.operand2.type;
            return binex;
        };
        TypeFlow.prototype.tryAddCandidates = function (signature, actuals, exactCandidates, conversionCandidates, comparisonInfo) {
            var lowerBound = signature.nonOptionalParameterCount;
            var upperBound = signature.parameters.length;
            var formalLen = lowerBound;
            var acceptable = false;
            if((actuals.length >= lowerBound) && (signature.hasVariableArgList || actuals.length <= upperBound)) {
                formalLen = (signature.hasVariableArgList ? signature.parameters.length : actuals.length);
                acceptable = true;
            }
            var repeatType = null;
            if(acceptable || signature.hasVariableArgList) {
                if(signature.hasVariableArgList) {
                    formalLen -= 1;
                    repeatType = (signature.parameters[formalLen]).parameter.typeLink.type;
                    repeatType = repeatType.elementType;
                    acceptable = actuals.length >= formalLen;
                }
                var len = actuals.length;
                var exact = acceptable;
                var convert = acceptable;
                for(var i = 0; i < len; i++) {
                    var typeA;
                    if(i < formalLen) {
                        typeA = (signature.parameters[i]).parameter.typeLink.type;
                    } else {
                        typeA = repeatType;
                    }
                    var typeB = actuals[i];
                    if(!typeA || !typeB || !(this.checker.typesAreIdentical(typeA, typeB))) {
                        exact = false;
                    }
                    if(!this.checker.sourceIsAssignableToTarget(typeB, typeA, comparisonInfo)) {
                        convert = false;
                    }
                    if(!(exact || convert)) {
                        break;
                    }
                }
                if(exact) {
                    exactCandidates[exactCandidates.length] = signature;
                } else if(convert && (exactCandidates.length == 0)) {
                    conversionCandidates[conversionCandidates.length] = signature;
                }
            }
        };
        TypeFlow.prototype.resolveOverload = function (application, group) {
            var rd = this.resolutionDataCache.getResolutionData();
            var actuals = rd.actuals;
            var exactCandidates = rd.exactCandidates;
            var conversionCandidates = rd.conversionCandidates;
            var candidate = null;
            var hasOverloads = group.signatures.length > 1;
            var comparisonInfo = new TypeScript.TypeComparisonInfo();
            var args = null;
            var target = null;
            if(application.nodeType == TypeScript.NodeType.Call || application.nodeType == TypeScript.NodeType.New) {
                var callEx = application;
                args = callEx.arguments;
                target = callEx.target;
                if(callEx.arguments) {
                    var len = callEx.arguments.members.length;
                    for(var i = 0; i < len; i++) {
                        actuals[i] = callEx.arguments.members[i].type;
                    }
                }
            } else if(application.nodeType == TypeScript.NodeType.Index) {
                var binExp = application;
                target = binExp.operand1;
                args = new TypeScript.ASTList();
                args.members[0] = binExp.operand2;
                actuals[0] = binExp.operand2.type;
            }
            for(var j = 0, groupLen = group.signatures.length; j < groupLen; j++) {
                var signature = group.signatures[j];
                if(hasOverloads && signature == group.definitionSignature && !this.checker.canCallDefinitionSignature) {
                    continue;
                }
                if(!signature.returnType.type && signature.declAST && (signature.typeCheckStatus != TypeScript.TypeCheckStatus.Finished)) {
                    this.typeCheckFunction(signature.declAST);
                }
                this.tryAddCandidates(signature, actuals, exactCandidates, conversionCandidates, comparisonInfo);
            }
            if(exactCandidates.length == 0) {
                var applicableCandidates = this.checker.getApplicableSignatures(conversionCandidates, args, comparisonInfo);
                if(applicableCandidates.length > 0) {
                    var candidateInfo = this.checker.findMostApplicableSignature(applicableCandidates, args);
                    if(candidateInfo.ambiguous) {
                        this.checker.errorReporter.simpleError(target, "Ambiguous call expression - could not choose overload");
                    }
                    candidate = candidateInfo.sig;
                } else {
                    var emsg = "Supplied parameters do not match any signature of call target";
                    if(comparisonInfo.message) {
                        this.checker.errorReporter.simpleError(target, emsg + ":\n\t" + comparisonInfo.message);
                    } else {
                        this.checker.errorReporter.simpleError(target, emsg);
                    }
                }
            } else {
                if(exactCandidates.length > 1) {
                    var applicableSigs = [];
                    for(var i = 0; i < exactCandidates.length; i++) {
                        applicableSigs[i] = {
                            signature: exactCandidates[i],
                            hadProvisionalErrors: false
                        };
                    }
                    var candidateInfo = this.checker.findMostApplicableSignature(applicableSigs, args);
                    if(candidateInfo.ambiguous) {
                        this.checker.errorReporter.simpleError(target, "Ambiguous call expression - could not choose overload");
                    }
                    candidate = candidateInfo.sig;
                } else {
                    candidate = exactCandidates[0];
                }
            }
            this.resolutionDataCache.returnResolutionData(rd);
            return candidate;
        };
        TypeFlow.prototype.typeCheckNew = function (ast) {
            var callEx = ast;
            callEx.target = this.typeCheck(callEx.target);
            var target = callEx.target;
            if(target.type.construct || target.type.call) {
                this.preTypeCheckCallArgs(callEx.arguments);
            } else {
                callEx.arguments = this.typeCheck(callEx.arguments);
            }
            if(target.type == this.anyType) {
                callEx.type = this.anyType;
                callEx.arguments = this.typeCheck(callEx.arguments);
            } else {
                if(target.type.construct) {
                    var signature = this.resolveOverload(callEx, target.type.construct);
                    if(signature == null) {
                        callEx.type = this.anyType;
                    } else if(signature.returnType.type == this.voidType) {
                        callEx.type = this.anyType;
                        callEx.signature = signature;
                    } else {
                        callEx.type = signature.returnType.type;
                        callEx.signature = signature;
                    }
                } else if(target.type.call) {
                    var signature = this.resolveOverload(callEx, target.type.call);
                    if(signature == null) {
                        callEx.type = this.anyType;
                    } else if((signature.returnType.type == this.voidType) || (signature.returnType.type == this.anyType)) {
                        callEx.type = this.anyType;
                        callEx.signature = signature;
                    } else {
                        this.checker.errorReporter.simpleError(callEx.target, "new expression only valid on constructors");
                    }
                } else if(target.type.elementType) {
                    callEx.type = target.type;
                } else {
                    this.checker.errorReporter.invalidCall(callEx, callEx.nodeType, this.scope);
                    callEx.type = this.anyType;
                }
            }
            this.postTypeCheckCallArgs(callEx);
            return callEx;
        };
        TypeFlow.prototype.preTypeCheckCallArgs = function (args) {
            if(!args) {
                return;
            }
            for(var i = 0; i < args.members.length; i++) {
                switch(args.members[i].nodeType) {
                    case TypeScript.NodeType.FuncDecl:
                    case TypeScript.NodeType.ObjectLit:
                    case TypeScript.NodeType.ArrayLit:
                        continue;
                    default:
                        this.typeCheck(args.members[i]);
                        break;
                }
            }
        };
        TypeFlow.prototype.postTypeCheckCallArgs = function (callEx) {
            var acceptedTargetType = false;
            var i = 0;
            if(callEx.target && callEx.target.type && callEx.signature && callEx.arguments) {
                var sig = callEx.signature;
                if(sig && callEx.arguments.members.length >= sig.nonOptionalParameterCount) {
                    acceptedTargetType = true;
                    var targetType = null;
                    var nonVarArgFormalParamLength = sig.hasVariableArgList ? sig.parameters.length - 1 : sig.parameters.length;
                    var nonVarArgActualParamLength = callEx.arguments.members.length < nonVarArgFormalParamLength ? callEx.arguments.members.length : nonVarArgFormalParamLength;
                    for(i = 0; i < nonVarArgActualParamLength; i++) {
                        targetType = sig.parameters[i].getType();
                        switch(callEx.arguments.members[i].nodeType) {
                            case TypeScript.NodeType.FuncDecl:
                            case TypeScript.NodeType.ObjectLit:
                            case TypeScript.NodeType.ArrayLit:
                                this.checker.typeCheckWithContextualType(targetType, this.checker.inProvisionalTypecheckMode(), !sig.parameters[i].declAST.isParenthesized, callEx.arguments.members[i]);
                                break;
                        }
                    }
                    if(sig.hasVariableArgList) {
                        var varArgParamIndex = sig.nonOptionalParameterCount - 1;
                        targetType = sig.parameters[varArgParamIndex].getType();
                        if(targetType) {
                            targetType = targetType.elementType;
                        }
                        var isParenthesized = !sig.parameters[varArgParamIndex].declAST.isParenthesized;
                        for(i = nonVarArgActualParamLength; i < callEx.arguments.members.length; i++) {
                            switch(callEx.arguments.members[i].nodeType) {
                                case TypeScript.NodeType.FuncDecl:
                                case TypeScript.NodeType.ObjectLit:
                                case TypeScript.NodeType.ArrayLit:
                                    this.checker.typeCheckWithContextualType(targetType, this.checker.inProvisionalTypecheckMode(), isParenthesized, callEx.arguments.members[i]);
                                    break;
                            }
                        }
                    }
                }
            }
            if(!acceptedTargetType && callEx.arguments) {
                this.checker.killCurrentContextualType();
                for(i = 0; i < callEx.arguments.members.length; i++) {
                    switch(callEx.arguments.members[i].nodeType) {
                        case TypeScript.NodeType.FuncDecl:
                        case TypeScript.NodeType.ObjectLit:
                        case TypeScript.NodeType.ArrayLit:
                            this.typeCheck(callEx.arguments.members[i]);
                            break;
                        default:
                            continue;
                    }
                }
            }
        };
        TypeFlow.prototype.typeCheckCall = function (ast) {
            var callEx = ast;
            if(this.checker.styleSettings.newMustBeUsed && (ast.nodeType == TypeScript.NodeType.New)) {
                if(TypeScript.hasFlag(ast.flags, TypeScript.ASTFlags.IsStatement)) {
                    this.checker.errorReporter.styleError(ast, "use of new expression as a statement");
                }
            } else if((!this.checker.styleSettings.evalOK) && (ast.nodeType == TypeScript.NodeType.Call)) {
                if((callEx.target.nodeType == TypeScript.NodeType.Name) && ((callEx.target).text == "eval")) {
                    this.checker.errorReporter.styleError(callEx, "eval not permitted");
                }
            }
            if(callEx.target.nodeType == TypeScript.NodeType.FuncDecl) {
                (callEx.target).isInlineCallLiteral = true;
            }
            var prevInSuperCall = this.inSuperCall;
            if(callEx.target.nodeType == TypeScript.NodeType.Super) {
                this.inSuperCall = true;
            }
            callEx.target = this.typeCheck(callEx.target);
            this.preTypeCheckCallArgs(callEx.arguments);
            var target = callEx.target;
            if((target.type == null) || (target.type == this.anyType) || (this.functionInterfaceType && target.type == this.functionInterfaceType)) {
                callEx.type = this.anyType;
            } else {
                var fnType = target.type;
                if(fnType.call) {
                    var signature = this.resolveOverload(callEx, fnType.call);
                    if(signature == null) {
                        callEx.type = this.anyType;
                    } else {
                        callEx.type = signature.returnType.type;
                        callEx.signature = signature;
                    }
                } else {
                    if(callEx.target.nodeType == TypeScript.NodeType.Super && this.thisFnc && this.thisFnc.isConstructor && TypeScript.hasFlag(this.thisFnc.fncFlags, TypeScript.FncFlags.ClassMethod)) {
                        var signature = fnType.symbol.type.construct ? this.resolveOverload(callEx, fnType.symbol.type.construct) : null;
                        if(signature == null) {
                            callEx.type = this.anyType;
                        } else {
                            callEx.flags |= TypeScript.ASTFlags.ClassBaseConstructorCall;
                            callEx.type = signature.returnType.type;
                            callEx.signature = signature;
                        }
                    } else {
                        callEx.type = this.anyType;
                        this.checker.errorReporter.invalidCall(callEx, callEx.nodeType, this.scope);
                    }
                }
            }
            this.postTypeCheckCallArgs(callEx);
            this.inSuperCall = prevInSuperCall;
            return callEx;
        };
        TypeFlow.prototype.assignScopes = function (ast) {
            var script = ast;
            this.checker.locationInfo = script.locationInfo;
            var globalChain = new ScopeChain(this.checker.gloMod, null, this.globalScope);
            var context = new TypeScript.AssignScopeContext(globalChain, this, [
                this.checker.currentModDecl
            ]);
            TypeScript.getAstWalkerFactory().walk(ast, TypeScript.preAssignScopes, TypeScript.postAssignScopes, null, context);
        };
        TypeFlow.prototype.findMemberScope = function (enclosingScopeContext, matchFlag) {
            var enclosingScope = enclosingScopeContext.getScope();
            var pos = enclosingScopeContext.pos - enclosingScopeContext.getScriptFragmentPosition();
            var scriptFragment = enclosingScopeContext.getScriptFragment();
            var memContext = new TypeScript.MemberScopeContext(this, pos, matchFlag);
            memContext.scope = enclosingScope;
            if(scriptFragment.nodeType == TypeScript.NodeType.Name) {
                return scriptFragment.type.getMemberScope(this);
            } else {
                TypeScript.getAstWalkerFactory().walk(scriptFragment, TypeScript.preFindMemberScope, null, null, memContext);
                if(memContext.ast && enclosingScopeContext.enclosingClassDecl && memContext.ast.type == enclosingScopeContext.enclosingClassDecl.type.instanceType) {
                    enclosingScopeContext.publicsOnly = false;
                }
                if(memContext.type) {
                    return memContext.type.getMemberScope(this);
                } else {
                    return null;
                }
            }
        };
        TypeFlow.prototype.findMemberScopeAt = function (enclosingScopeContext) {
            return this.findMemberScope(enclosingScopeContext, TypeScript.ASTFlags.DotLHS);
        };
        TypeFlow.prototype.findMemberScopeAtFullAst = function (enclosingScopeContext) {
            var matchFlag = TypeScript.ASTFlags.DotLHS;
            var pos = enclosingScopeContext.pos;
            var astResult = null;
            var preFindMemberScopeFullAst = function (ast, parent, walker) {
                if(TypeScript.isValidAstNode(ast)) {
                    if(TypeScript.hasFlag(ast.flags, matchFlag) && (pos == ast.limChar || (pos - 1) == ast.limChar)) {
                        astResult = ast;
                        walker.options.stopWalk();
                    }
                    walker.options.goChildren = (ast.minChar <= pos) && (pos <= ast.limChar);
                }
                return ast;
            };
            var preFindMemberScopeFullAstFuzy = function (ast, parent, walker) {
                if(TypeScript.isValidAstNode(ast)) {
                    if(TypeScript.hasFlag(ast.flags, matchFlag) && ((ast.minChar < pos) && (pos <= ast.limChar))) {
                        astResult = ast;
                    }
                    walker.options.goChildren = (ast.minChar <= pos) && (pos <= ast.limChar);
                }
                return ast;
            };
            TypeScript.getAstWalkerFactory().walk(enclosingScopeContext.script, preFindMemberScopeFullAst);
            if(astResult == null) {
                TypeScript.getAstWalkerFactory().walk(enclosingScopeContext.script, preFindMemberScopeFullAstFuzy);
            }
            if(astResult && enclosingScopeContext.enclosingClassDecl && astResult.type == enclosingScopeContext.enclosingClassDecl.type.instanceType) {
                enclosingScopeContext.publicsOnly = false;
            }
            if(astResult && astResult.type) {
                return astResult.type.getMemberScope(this);
            } else {
                return null;
            }
        };
        return TypeFlow;
    })();
    TypeScript.TypeFlow = TypeFlow;    
})(TypeScript || (TypeScript = {}));
