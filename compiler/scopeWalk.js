var TypeScript;
(function (TypeScript) {
    var TypeCollectionContext = (function () {
        function TypeCollectionContext(scopeChain, checker) {
            this.scopeChain = scopeChain;
            this.checker = checker;
            this.script = null;
        }
        return TypeCollectionContext;
    })();
    TypeScript.TypeCollectionContext = TypeCollectionContext;    
    var MemberScopeContext = (function () {
        function MemberScopeContext(flow, pos, matchFlag) {
            this.flow = flow;
            this.pos = pos;
            this.matchFlag = matchFlag;
            this.type = null;
            this.ast = null;
            this.options = new TypeScript.AstWalkOptions();
        }
        return MemberScopeContext;
    })();
    TypeScript.MemberScopeContext = MemberScopeContext;    
    var EnclosingScopeContext = (function () {
        function EnclosingScopeContext(logger, script, text, pos, isMemberCompletion) {
            this.logger = logger;
            this.script = script;
            this.text = text;
            this.pos = pos;
            this.isMemberCompletion = isMemberCompletion;
            this.scopeGetter = null;
            this.objectLiteralScopeGetter = null;
            this.scopeStartAST = null;
            this.skipNextFuncDeclForClass = false;
            this.deepestModuleDecl = null;
            this.enclosingClassDecl = null;
            this.enclosingObjectLit = null;
            this.publicsOnly = true;
            this.useFullAst = false;
        }
        EnclosingScopeContext.prototype.getScope = function () {
            return this.scopeGetter();
        };
        EnclosingScopeContext.prototype.getObjectLiteralScope = function () {
            return this.objectLiteralScopeGetter();
        };
        EnclosingScopeContext.prototype.getScopeAST = function () {
            return this.scopeStartAST;
        };
        EnclosingScopeContext.prototype.getScopePosition = function () {
            return this.scopeStartAST.minChar;
        };
        EnclosingScopeContext.prototype.getScriptFragmentStartAST = function () {
            return this.scopeStartAST;
        };
        EnclosingScopeContext.prototype.getScriptFragmentPosition = function () {
            return this.getScriptFragmentStartAST().minChar;
        };
        EnclosingScopeContext.prototype.getScriptFragment = function () {
            if(this.scriptFragment == null) {
                var ast = this.getScriptFragmentStartAST();
                var minChar = ast.minChar;
                var limChar = (this.isMemberCompletion ? this.pos : this.pos + 1);
                this.scriptFragment = TypeScript.quickParse(this.logger, ast, this.text, minChar, limChar, null).Script;
            }
            return this.scriptFragment;
        };
        return EnclosingScopeContext;
    })();
    TypeScript.EnclosingScopeContext = EnclosingScopeContext;    
    function preFindMemberScope(ast, parent, walker) {
        var memScope = walker.state;
        if(TypeScript.hasFlag(ast.flags, memScope.matchFlag) && ((memScope.pos < 0) || (memScope.pos == ast.limChar))) {
            memScope.ast = ast;
            if((ast.type == null) && (memScope.pos >= 0)) {
                memScope.flow.inScopeTypeCheck(ast, memScope.scope);
            }
            memScope.type = ast.type;
            memScope.options.stopWalk();
        }
        return ast;
    }
    TypeScript.preFindMemberScope = preFindMemberScope;
    function pushTypeCollectionScope(container, valueMembers, ambientValueMembers, enclosedTypes, ambientEnclosedTypes, context, thisType, classType, moduleDecl) {
        var builder = new TypeScript.SymbolScopeBuilder(valueMembers, ambientValueMembers, enclosedTypes, ambientEnclosedTypes, null, container);
        var chain = new TypeScript.ScopeChain(container, context.scopeChain, builder);
        chain.thisType = thisType;
        chain.classType = classType;
        chain.moduleDecl = moduleDecl;
        context.scopeChain = chain;
    }
    TypeScript.pushTypeCollectionScope = pushTypeCollectionScope;
    function popTypeCollectionScope(context) {
        context.scopeChain = context.scopeChain.previous;
    }
    TypeScript.popTypeCollectionScope = popTypeCollectionScope;
    function preFindEnclosingScope(ast, parent, walker) {
        var context = walker.state;
        var minChar = ast.minChar;
        var limChar = ast.limChar;
        if(ast.nodeType == TypeScript.NodeType.Script && context.pos > limChar) {
            limChar = context.pos;
        }
        if((minChar <= context.pos) && (limChar >= context.pos)) {
            switch(ast.nodeType) {
                case TypeScript.NodeType.Script:
                    var script = ast;
                    context.scopeGetter = function () {
                        return script.bod === null ? null : script.bod.enclosingScope;
                    };
                    context.scopeStartAST = script;
                    break;
                case TypeScript.NodeType.ClassDeclaration:
                    context.scopeGetter = function () {
                        return (ast.type === null || ast.type.instanceType.containedScope === null) ? null : ast.type.instanceType.containedScope;
                    };
                    context.scopeStartAST = ast;
                    context.enclosingClassDecl = ast;
                    break;
                case TypeScript.NodeType.ObjectLit:
                    var objectLit = ast;
                    if(objectLit.targetType) {
                        context.scopeGetter = function () {
                            return objectLit.targetType.containedScope;
                        };
                        context.objectLiteralScopeGetter = function () {
                            return objectLit.targetType.memberScope;
                        };
                        context.enclosingObjectLit = objectLit;
                    }
                    break;
                case TypeScript.NodeType.ModuleDeclaration:
                    context.deepestModuleDecl = ast;
                    context.scopeGetter = function () {
                        return ast.type === null ? null : ast.type.containedScope;
                    };
                    context.scopeStartAST = ast;
                    break;
                case TypeScript.NodeType.InterfaceDeclaration:
                    context.scopeGetter = function () {
                        return (ast.type === null) ? null : ast.type.containedScope;
                    };
                    context.scopeStartAST = ast;
                    break;
                case TypeScript.NodeType.FuncDecl:
 {
                        var funcDecl = ast;
                        if(context.skipNextFuncDeclForClass) {
                            context.skipNextFuncDeclForClass = false;
                        } else {
                            context.scopeGetter = function () {
                                if(funcDecl.isConstructor && TypeScript.hasFlag(funcDecl.fncFlags, TypeScript.FncFlags.ClassMethod)) {
                                    if(ast.type && ast.type.enclosingType) {
                                        return ast.type.enclosingType.constructorScope;
                                    }
                                }
                                if(funcDecl.scopeType) {
                                    return funcDecl.scopeType.containedScope;
                                }
                                if(funcDecl.type) {
                                    return funcDecl.type.containedScope;
                                }
                                return null;
                            };
                            context.scopeStartAST = ast;
                        }
                    }
                    break;
            }
            walker.options.goChildren = true;
        } else {
            walker.options.goChildren = false;
        }
        return ast;
    }
    TypeScript.preFindEnclosingScope = preFindEnclosingScope;
    function findEnclosingScopeAt(logger, script, text, pos, isMemberCompletion) {
        var context = new EnclosingScopeContext(logger, script, text, pos, isMemberCompletion);
        TypeScript.getAstWalkerFactory().walk(script, preFindEnclosingScope, null, null, context);
        if(context.scopeStartAST === null) {
            return null;
        }
        return context;
    }
    TypeScript.findEnclosingScopeAt = findEnclosingScopeAt;
})(TypeScript || (TypeScript = {}));
