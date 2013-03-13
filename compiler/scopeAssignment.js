var TypeScript;
(function (TypeScript) {
    var AssignScopeContext = (function () {
        function AssignScopeContext(scopeChain, typeFlow, modDeclChain) {
            this.scopeChain = scopeChain;
            this.typeFlow = typeFlow;
            this.modDeclChain = modDeclChain;
        }
        return AssignScopeContext;
    })();
    TypeScript.AssignScopeContext = AssignScopeContext;    
    function pushAssignScope(scope, context, type, classType, fnc) {
        var chain = new TypeScript.ScopeChain(null, context.scopeChain, scope);
        chain.thisType = type;
        chain.classType = classType;
        chain.fnc = fnc;
        context.scopeChain = chain;
    }
    TypeScript.pushAssignScope = pushAssignScope;
    function popAssignScope(context) {
        context.scopeChain = context.scopeChain.previous;
    }
    TypeScript.popAssignScope = popAssignScope;
    function instanceCompare(a, b) {
        if(((a == null) || (!a.isInstanceProperty()))) {
            return b;
        } else {
            return a;
        }
    }
    TypeScript.instanceCompare = instanceCompare;
    function instanceFilterStop(s) {
        return s.isInstanceProperty();
    }
    TypeScript.instanceFilterStop = instanceFilterStop;
    var ScopeSearchFilter = (function () {
        function ScopeSearchFilter(select, stop) {
            this.select = select;
            this.stop = stop;
            this.result = null;
        }
        ScopeSearchFilter.prototype.reset = function () {
            this.result = null;
        };
        ScopeSearchFilter.prototype.update = function (b) {
            this.result = this.select(this.result, b);
            if(this.result) {
                return this.stop(this.result);
            } else {
                return false;
            }
        };
        return ScopeSearchFilter;
    })();
    TypeScript.ScopeSearchFilter = ScopeSearchFilter;    
    TypeScript.instanceFilter = new ScopeSearchFilter(instanceCompare, instanceFilterStop);
    function preAssignModuleScopes(ast, context) {
        var moduleDecl = ast;
        var memberScope = null;
        var aggScope = null;
        if(moduleDecl.name && moduleDecl.mod) {
            moduleDecl.name.sym = moduleDecl.mod.symbol;
        }
        var mod = moduleDecl.mod;
        if(!mod) {
            return;
        }
        memberScope = new TypeScript.SymbolTableScope(mod.members, mod.ambientMembers, mod.enclosedTypes, mod.ambientEnclosedTypes, mod.symbol);
        mod.memberScope = memberScope;
        context.modDeclChain.push(moduleDecl);
        context.typeFlow.checker.currentModDecl = moduleDecl;
        aggScope = new TypeScript.SymbolAggregateScope(mod.symbol);
        aggScope.addParentScope(memberScope);
        aggScope.addParentScope(context.scopeChain.scope);
        pushAssignScope(aggScope, context, null, null, null);
        mod.containedScope = aggScope;
        if(mod.symbol) {
            context.typeFlow.addLocalsFromScope(mod.containedScope, mod.symbol, moduleDecl.vars, mod.members.privateMembers, true);
        }
    }
    TypeScript.preAssignModuleScopes = preAssignModuleScopes;
    function preAssignClassScopes(ast, context) {
        var classDecl = ast;
        var memberScope = null;
        var aggScope = null;
        if(classDecl.name && classDecl.type) {
            classDecl.name.sym = classDecl.type.symbol;
        }
        var classType = ast.type;
        if(classType) {
            var classSym = classType.symbol;
            memberScope = context.typeFlow.checker.scopeOf(classType);
            aggScope = new TypeScript.SymbolAggregateScope(classType.symbol);
            aggScope.addParentScope(memberScope);
            aggScope.addParentScope(context.scopeChain.scope);
            classType.containedScope = aggScope;
            classType.memberScope = memberScope;
            var instanceType = classType.instanceType;
            memberScope = context.typeFlow.checker.scopeOf(instanceType);
            instanceType.memberScope = memberScope;
            aggScope = new TypeScript.SymbolAggregateScope(instanceType.symbol);
            aggScope.addParentScope(context.scopeChain.scope);
            pushAssignScope(aggScope, context, instanceType, classType, null);
            instanceType.containedScope = aggScope;
        } else {
            ast.type = context.typeFlow.anyType;
        }
    }
    TypeScript.preAssignClassScopes = preAssignClassScopes;
    function preAssignInterfaceScopes(ast, context) {
        var interfaceDecl = ast;
        var memberScope = null;
        var aggScope = null;
        if(interfaceDecl.name && interfaceDecl.type) {
            interfaceDecl.name.sym = interfaceDecl.type.symbol;
        }
        var interfaceType = ast.type;
        memberScope = context.typeFlow.checker.scopeOf(interfaceType);
        interfaceType.memberScope = memberScope;
        aggScope = new TypeScript.SymbolAggregateScope(interfaceType.symbol);
        aggScope.addParentScope(memberScope);
        aggScope.addParentScope(context.scopeChain.scope);
        pushAssignScope(aggScope, context, null, null, null);
        interfaceType.containedScope = aggScope;
    }
    TypeScript.preAssignInterfaceScopes = preAssignInterfaceScopes;
    function preAssignWithScopes(ast, context) {
        var withStmt = ast;
        var withType = withStmt.type;
        var members = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
        var ambientMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
        var withType = new TypeScript.Type();
        var withSymbol = new TypeScript.WithSymbol(withStmt.minChar, context.typeFlow.checker.locationInfo.unitIndex, withType);
        withType.members = members;
        withType.ambientMembers = ambientMembers;
        withType.symbol = withSymbol;
        withType.setHasImplementation();
        withStmt.type = withType;
        var withScope = new TypeScript.SymbolScopeBuilder(withType.members, withType.ambientMembers, null, null, context.scopeChain.scope, withType.symbol);
        pushAssignScope(withScope, context, null, null, null);
        withType.containedScope = withScope;
    }
    TypeScript.preAssignWithScopes = preAssignWithScopes;
    function preAssignFuncDeclScopes(ast, context) {
        var funcDecl = ast;
        var container = null;
        var localContainer = null;
        if(funcDecl.type) {
            localContainer = ast.type.symbol;
        }
        var isStatic = TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.Static);
        var isInnerStatic = isStatic && context.scopeChain.fnc != null;
        var parentScope = isInnerStatic ? context.scopeChain.fnc.type.memberScope : context.scopeChain.scope;
        if(context.scopeChain.thisType && (!funcDecl.isConstructor || TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod))) {
            var instType = context.scopeChain.thisType;
            if(!(instType.typeFlags & TypeScript.TypeFlags.IsClass) && !TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod)) {
                if(!funcDecl.isMethod() || isStatic) {
                    parentScope = instType.constructorScope;
                } else {
                    parentScope = instType.containedScope;
                }
            } else {
                if(context.scopeChain.previous.scope.container && context.scopeChain.previous.scope.container.declAST && context.scopeChain.previous.scope.container.declAST.nodeType == TypeScript.NodeType.FuncDecl && (context.scopeChain.previous.scope.container.declAST).isConstructor) {
                    parentScope = instType.constructorScope;
                } else if(isStatic && context.scopeChain.classType) {
                    parentScope = context.scopeChain.classType.containedScope;
                } else {
                    parentScope = instType.containedScope;
                }
            }
            container = instType.symbol;
        } else if(funcDecl.isConstructor && context.scopeChain.thisType) {
            container = context.scopeChain.thisType.symbol;
        }
        if(funcDecl.type == null || TypeScript.hasFlag(funcDecl.type.symbol.flags, TypeScript.SymbolFlags.TypeSetDuringScopeAssignment)) {
            if(context.scopeChain.fnc && context.scopeChain.fnc.type) {
                container = context.scopeChain.fnc.type.symbol;
            }
            var funcScope = null;
            var outerFnc = context.scopeChain.fnc;
            var nameText = funcDecl.name ? funcDecl.name.actualText : null;
            var fgSym = null;
            if(isStatic) {
                if(outerFnc.type.members == null && container.getType().memberScope) {
                    outerFnc.type.members = ((container).type.memberScope).valueMembers;
                }
                funcScope = context.scopeChain.fnc.type.memberScope;
                outerFnc.innerStaticFuncs[outerFnc.innerStaticFuncs.length] = funcDecl;
            } else {
                funcScope = context.scopeChain.scope;
            }
            if(nameText && nameText != "__missing" && !funcDecl.isAccessor()) {
                if(isStatic) {
                    fgSym = funcScope.findLocal(nameText, false, false);
                } else {
                    fgSym = funcScope.findLocal(nameText, false, false);
                }
            }
            context.typeFlow.checker.createFunctionSignature(funcDecl, container, funcScope, fgSym, fgSym == null);
            if(!funcDecl.accessorSymbol && (funcDecl.fncFlags & TypeScript.FncFlags.ClassMethod) && container && ((!fgSym || fgSym.declAST.nodeType != TypeScript.NodeType.FuncDecl) && funcDecl.isAccessor()) || (fgSym && fgSym.isAccessor())) {
                funcDecl.accessorSymbol = context.typeFlow.checker.createAccessorSymbol(funcDecl, fgSym, container.getType(), (funcDecl.isMethod() && isStatic), true, funcScope, container);
            }
            funcDecl.type.symbol.flags |= TypeScript.SymbolFlags.TypeSetDuringScopeAssignment;
        }
        if(funcDecl.name && funcDecl.type) {
            funcDecl.name.sym = funcDecl.type.symbol;
        }
        funcDecl.scopeType = funcDecl.type;
        if(funcDecl.isOverload) {
            return;
        }
        var funcTable = new TypeScript.StringHashTable();
        var funcMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(funcTable, new TypeScript.StringHashTable()));
        var ambientFuncTable = new TypeScript.StringHashTable();
        var ambientFuncMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(ambientFuncTable, new TypeScript.StringHashTable()));
        var funcStaticTable = new TypeScript.StringHashTable();
        var funcStaticMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(funcStaticTable, new TypeScript.StringHashTable()));
        var ambientFuncStaticTable = new TypeScript.StringHashTable();
        var ambientFuncStaticMembers = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(ambientFuncStaticTable, new TypeScript.StringHashTable()));
        funcDecl.unitIndex = context.typeFlow.checker.locationInfo.unitIndex;
        var locals = new TypeScript.SymbolScopeBuilder(funcMembers, ambientFuncMembers, null, null, parentScope, localContainer);
        var statics = new TypeScript.SymbolScopeBuilder(funcStaticMembers, ambientFuncStaticMembers, null, null, parentScope, null);
        if(funcDecl.isConstructor && context.scopeChain.thisType) {
            context.scopeChain.thisType.constructorScope = locals;
        }
        funcDecl.symbols = funcTable;
        if(!funcDecl.isSpecialFn()) {
            var group = funcDecl.type;
            var signature = funcDecl.signature;
            if(!funcDecl.isConstructor) {
                group.containedScope = locals;
                locals.container = group.symbol;
                group.memberScope = statics;
                statics.container = group.symbol;
            }
            funcDecl.enclosingFnc = context.scopeChain.fnc;
            group.enclosingType = isStatic ? context.scopeChain.classType : context.scopeChain.thisType;
            var fgSym = ast.type.symbol;
            if(((funcDecl.fncFlags & TypeScript.FncFlags.Signature) == TypeScript.FncFlags.None) && funcDecl.vars) {
                context.typeFlow.addLocalsFromScope(locals, fgSym, funcDecl.vars, funcTable, false);
                context.typeFlow.addLocalsFromScope(statics, fgSym, funcDecl.statics, funcStaticTable, false);
            }
            if(signature.parameters) {
                var len = signature.parameters.length;
                for(var i = 0; i < len; i++) {
                    var paramSym = signature.parameters[i];
                    context.typeFlow.checker.resolveTypeLink(locals, paramSym.parameter.typeLink, true);
                }
            }
            context.typeFlow.checker.resolveTypeLink(locals, signature.returnType, funcDecl.isSignature());
        }
        if(!funcDecl.isConstructor || TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod)) {
            var thisType = (funcDecl.isConstructor && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod)) ? context.scopeChain.thisType : null;
            pushAssignScope(locals, context, thisType, null, funcDecl);
        }
        if(funcDecl.name && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.IsFunctionExpression) && !funcDecl.isAccessor()) {
            if(funcDecl.name.sym) {
                funcTable.add(funcDecl.name.actualText, funcDecl.name.sym);
            }
        }
    }
    TypeScript.preAssignFuncDeclScopes = preAssignFuncDeclScopes;
    function preAssignCatchScopes(ast, context) {
        var catchBlock = ast;
        if(catchBlock.param) {
            var catchTable = new TypeScript.ScopedMembers(new TypeScript.DualStringHashTable(new TypeScript.StringHashTable(), new TypeScript.StringHashTable()));
            var catchLocals = new TypeScript.SymbolScopeBuilder(catchTable, null, null, null, context.scopeChain.scope, context.scopeChain.scope.container);
            catchBlock.containedScope = catchLocals;
            pushAssignScope(catchLocals, context, context.scopeChain.thisType, context.scopeChain.classType, context.scopeChain.fnc);
        }
    }
    TypeScript.preAssignCatchScopes = preAssignCatchScopes;
    function preAssignScopes(ast, parent, walker) {
        var context = walker.state;
        var go = true;
        if(ast) {
            if(ast.nodeType == TypeScript.NodeType.List) {
                var list = ast;
                list.enclosingScope = context.scopeChain.scope;
            } else if(ast.nodeType == TypeScript.NodeType.ModuleDeclaration) {
                preAssignModuleScopes(ast, context);
            } else if(ast.nodeType == TypeScript.NodeType.ClassDeclaration) {
                preAssignClassScopes(ast, context);
            } else if(ast.nodeType == TypeScript.NodeType.InterfaceDeclaration) {
                preAssignInterfaceScopes(ast, context);
            } else if(ast.nodeType == TypeScript.NodeType.With) {
                preAssignWithScopes(ast, context);
            } else if(ast.nodeType == TypeScript.NodeType.FuncDecl) {
                preAssignFuncDeclScopes(ast, context);
            } else if(ast.nodeType == TypeScript.NodeType.Catch) {
                preAssignCatchScopes(ast, context);
            } else if(ast.nodeType == TypeScript.NodeType.TypeRef) {
                go = false;
            }
        }
        walker.options.goChildren = go;
        return ast;
    }
    TypeScript.preAssignScopes = preAssignScopes;
    function postAssignScopes(ast, parent, walker) {
        var context = walker.state;
        var go = true;
        if(ast) {
            if(ast.nodeType == TypeScript.NodeType.ModuleDeclaration) {
                var prevModDecl = ast;
                popAssignScope(context);
                context.modDeclChain.pop();
                if(context.modDeclChain.length >= 1) {
                    context.typeFlow.checker.currentModDecl = context.modDeclChain[context.modDeclChain.length - 1];
                }
            } else if(ast.nodeType == TypeScript.NodeType.ClassDeclaration) {
                popAssignScope(context);
            } else if(ast.nodeType == TypeScript.NodeType.InterfaceDeclaration) {
                popAssignScope(context);
            } else if(ast.nodeType == TypeScript.NodeType.With) {
                popAssignScope(context);
            } else if(ast.nodeType == TypeScript.NodeType.FuncDecl) {
                var funcDecl = ast;
                if((!funcDecl.isConstructor || TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod)) && !funcDecl.isOverload) {
                    popAssignScope(context);
                }
            } else if(ast.nodeType == TypeScript.NodeType.Catch) {
                var catchBlock = ast;
                if(catchBlock.param) {
                    popAssignScope(context);
                }
            } else {
                go = false;
            }
        }
        walker.options.goChildren = go;
        return ast;
    }
    TypeScript.postAssignScopes = postAssignScopes;
})(TypeScript || (TypeScript = {}));
