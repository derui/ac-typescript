var TypeScript;
(function (TypeScript) {
    (function (TypeContext) {
        TypeContext._map = [];
        TypeContext.NoTypes = 0;
        TypeContext.ArraySuffix = 1;
        TypeContext.Primitive = 2;
        TypeContext.Named = 4;
        TypeContext.AllSimpleTypes = TypeContext.Primitive | TypeContext.Named;
        TypeContext.AllTypes = TypeContext.Primitive | TypeContext.Named | TypeContext.ArraySuffix;
    })(TypeScript.TypeContext || (TypeScript.TypeContext = {}));
    var TypeContext = TypeScript.TypeContext;
    var QuickParseResult = (function () {
        function QuickParseResult(Script, endLexState) {
            this.Script = Script;
            this.endLexState = endLexState;
        }
        return QuickParseResult;
    })();
    TypeScript.QuickParseResult = QuickParseResult;    
    var Parser = (function () {
        function Parser() {
            this.varLists = [];
            this.scopeLists = [];
            this.staticsLists = [];
            this.scanner = new TypeScript.Scanner();
            this.currentToken = null;
            this.needTerminator = false;
            this.inFunction = false;
            this.inInterfaceDecl = false;
            this.currentClassDecl = null;
            this.inFncDecl = false;
            this.anonId = new TypeScript.Identifier("_anonymous");
            this.style_requireSemi = false;
            this.style_funcInLoop = true;
            this.incremental = false;
            this.errorRecovery = false;
            this.outfile = undefined;
            this.errorCallback = null;
            this.ambientModule = false;
            this.ambientClass = false;
            this.topLevel = true;
            this.allowImportDeclaration = true;
            this.currentUnitIndex = (-1);
            this.prevIDTok = null;
            this.statementInfoStack = new Array();
            this.hasTopLevelImportOrExport = false;
            this.strictMode = false;
            this.nestingLevel = 0;
            this.prevExpr = null;
            this.currentClassDefinition = null;
            this.parsingClassConstructorDefinition = false;
            this.parsingDeclareFile = false;
            this.amdDependencies = [];
            this.inferPropertiesFromThisAssignment = false;
            this.requiresExtendsBlock = false;
            this.fname = "";
        }
        Parser.prototype.resetStmtStack = function () {
            this.statementInfoStack = new Array();
        };
        Parser.prototype.inLoop = function () {
            for(var j = this.statementInfoStack.length - 1; j >= 0; j--) {
                if(this.statementInfoStack[j].stmt.isLoop()) {
                    return true;
                }
            }
            return false;
        };
        Parser.prototype.pushStmt = function (stmt, labels) {
            var info = {
                stmt: stmt,
                labels: labels
            };
            this.statementInfoStack.push(info);
        };
        Parser.prototype.popStmt = function () {
            return this.statementInfoStack.pop();
        };
        Parser.prototype.resolveJumpTarget = function (jump) {
            var resolvedTarget = TypeScript.AST.getResolvedIdentifierName(jump.target);
            var len = this.statementInfoStack.length;
            for(var i = len - 1; i >= 0; i--) {
                var info = this.statementInfoStack[i];
                if(jump.target) {
                    if(info.labels && (info.labels.members.length > 0)) {
                        for(var j = 0, labLen = info.labels.members.length; j < labLen; j++) {
                            var label = info.labels.members[j];
                            if(label.id.text == resolvedTarget) {
                                jump.setResolvedTarget(this, info.stmt);
                                return;
                            }
                        }
                    }
                } else {
                    if(info.stmt.isLoop()) {
                        jump.setResolvedTarget(this, info.stmt);
                        return;
                    } else if((info.stmt.nodeType == TypeScript.NodeType.Switch) && (jump.nodeType == TypeScript.NodeType.Break)) {
                        jump.setResolvedTarget(this, info.stmt);
                        return;
                    }
                }
            }
            if(jump.target) {
                this.reportParseError("could not find enclosing statement with label " + jump.target);
            } else {
                if(jump.nodeType == TypeScript.NodeType.Break) {
                    this.reportParseError("break statement requires enclosing loop or switch");
                } else {
                    this.reportParseError("continue statement requires enclosing loop");
                }
            }
        };
        Parser.prototype.setErrorRecovery = function (outfile) {
            this.outfile = outfile;
            this.errorRecovery = true;
        };
        Parser.prototype.getSourceLineCol = function (lineCol, minChar) {
            TypeScript.getSourceLineColFromMap(lineCol, minChar, this.scanner.lineMap);
        };
        Parser.prototype.createRef = function (text, hasEscapeSequence, minChar) {
            var id = new TypeScript.Identifier(text, hasEscapeSequence);
            id.minChar = minChar;
            return id;
        };
        Parser.prototype.reportParseStyleError = function (message) {
            this.reportParseError("STYLE: " + message);
        };
        Parser.prototype.reportParseError = function (message, startPos, pos) {
            if (typeof startPos === "undefined") { startPos = this.scanner.startPos; }
            if (typeof pos === "undefined") { pos = this.scanner.pos; }
            var len = Math.max(1, pos - startPos);
            if(this.errorCallback) {
                this.errorCallback(startPos, len, message, this.currentUnitIndex);
            } else if(this.errorRecovery) {
                var lineCol = {
                    line: -1,
                    col: -1
                };
                this.getSourceLineCol(lineCol, startPos);
                if(this.outfile) {
                    this.outfile.WriteLine("// " + this.fname + " (" + lineCol.line + "," + lineCol.col + "): " + message);
                }
            } else {
                throw new SyntaxError(this.fname + " (" + this.scanner.line + "," + this.scanner.col + "): " + message);
            }
        };
        Parser.prototype.checkNextToken = function (tokenId, errorRecoverySet, errorText) {
            if (typeof errorText === "undefined") { errorText = null; }
            this.currentToken = this.scanner.scan();
            this.checkCurrentToken(tokenId, errorRecoverySet, errorText);
        };
        Parser.prototype.skip = function (errorRecoverySet) {
            errorRecoverySet |= TypeScript.ErrorRecoverySet.EOF;
            var ersTok = TypeScript.ErrorRecoverySet.None;
            var tokenInfo = TypeScript.lookupToken(this.currentToken.tokenId);
            if(tokenInfo != undefined) {
                ersTok = tokenInfo.ers;
            }
            var pendingRightCurlies = 0;
            while(((ersTok & errorRecoverySet) == TypeScript.ErrorRecoverySet.None) || (this.currentToken.tokenId == TypeScript.TokenID.CloseBrace) && (pendingRightCurlies > 0)) {
                if(this.currentToken.tokenId == TypeScript.TokenID.OpenBrace) {
                    pendingRightCurlies++;
                } else if(this.currentToken.tokenId == TypeScript.TokenID.CloseBrace) {
                    pendingRightCurlies--;
                }
                this.currentToken = this.scanner.scan();
                ersTok = TypeScript.ErrorRecoverySet.None;
                tokenInfo = TypeScript.lookupToken(this.currentToken.tokenId);
                if(tokenInfo != undefined) {
                    ersTok = tokenInfo.ers;
                }
            }
        };
        Parser.prototype.checkCurrentToken = function (tokenId, errorRecoverySet, errorText) {
            if (typeof errorText === "undefined") { errorText = null; }
            if(this.currentToken.tokenId != tokenId) {
                errorText = errorText == null ? ("Expected '" + TypeScript.tokenTable[tokenId].text + "'") : errorText;
                this.reportParseError(errorText);
                if(this.errorRecovery) {
                    this.skip(errorRecoverySet);
                }
            } else {
                this.currentToken = this.scanner.scan();
            }
        };
        Parser.prototype.pushDeclLists = function () {
            this.staticsLists.push(new TypeScript.ASTList());
            this.varLists.push(new TypeScript.ASTList());
            this.scopeLists.push(new TypeScript.ASTList());
        };
        Parser.prototype.popDeclLists = function () {
            this.staticsLists.pop();
            this.varLists.pop();
            this.scopeLists.pop();
        };
        Parser.prototype.topVarList = function () {
            return this.varLists[this.varLists.length - 1];
        };
        Parser.prototype.topScopeList = function () {
            return this.scopeLists[this.scopeLists.length - 1];
        };
        Parser.prototype.topStaticsList = function () {
            return this.staticsLists[this.staticsLists.length - 1];
        };
        Parser.prototype.parseComment = function (comment) {
            if(comment) {
                var c = new TypeScript.Comment(comment.value, comment.isBlock, comment.endsLine);
                c.minChar = comment.startPos;
                c.limChar = comment.startPos + comment.value.length;
                var lineCol = {
                    line: -1,
                    col: -1
                };
                this.getSourceLineCol(lineCol, c.minChar);
                c.minLine = lineCol.line;
                this.getSourceLineCol(lineCol, c.limChar);
                c.limLine = lineCol.line;
                if(!comment.isBlock && comment.value.length > 3 && comment.value.substring(0, 3) == "///") {
                    var dependencyPath = TypeScript.getAdditionalDependencyPath(comment.value);
                    if(dependencyPath) {
                        this.amdDependencies.push(dependencyPath);
                    }
                    if(TypeScript.getImplicitImport(comment.value)) {
                        this.hasTopLevelImportOrExport = true;
                    }
                }
                return c;
            } else {
                return null;
            }
        };
        Parser.prototype.parseCommentsInner = function (comments) {
            if(comments) {
                var commentASTs = new Array();
                for(var i = 0; i < comments.length; i++) {
                    commentASTs.push(this.parseComment(comments[i]));
                }
                return commentASTs;
            } else {
                return null;
            }
        };
        Parser.prototype.parseComments = function () {
            var comments = this.scanner.getComments();
            return this.parseCommentsInner(comments);
        };
        Parser.prototype.parseCommentsForLine = function (line) {
            var comments = this.scanner.getCommentsForLine(line);
            return this.parseCommentsInner(comments);
        };
        Parser.prototype.combineComments = function (comment1, comment2) {
            if(comment1 == null) {
                return comment2;
            } else if(comment2 == null) {
                return comment1;
            } else {
                return comment1.concat(comment2);
            }
        };
        Parser.prototype.parseEnumDecl = function (errorRecoverySet, modifiers) {
            var leftCurlyCount = this.scanner.leftCurlyCount;
            var rightCurlyCount = this.scanner.rightCurlyCount;
            var name = null;
            if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                name = TypeScript.Identifier.fromToken(this.currentToken);
                name.minChar = this.scanner.startPos;
                name.limChar = this.scanner.pos;
                this.currentToken = this.scanner.scan();
            } else {
                this.reportParseError("Enum declaration requires identifier");
                if(this.errorRecovery) {
                    name = new TypeScript.MissingIdentifier();
                    name.minChar = this.scanner.startPos;
                    name.limChar = this.scanner.startPos;
                    name.flags |= TypeScript.ASTFlags.Error;
                }
            }
            var membersMinChar = this.scanner.startPos;
            this.checkCurrentToken(TypeScript.TokenID.OpenBrace, errorRecoverySet | TypeScript.ErrorRecoverySet.ID);
            this.pushDeclLists();
            var members = new TypeScript.ASTList();
            members.minChar = membersMinChar;
            var mapDecl = new TypeScript.VarDecl(new TypeScript.Identifier("_map"), 0);
            mapDecl.varFlags |= TypeScript.VarFlags.Exported;
            mapDecl.varFlags |= TypeScript.VarFlags.Private;
            mapDecl.varFlags |= (TypeScript.VarFlags.Property | TypeScript.VarFlags.Public);
            mapDecl.init = new TypeScript.UnaryExpression(TypeScript.NodeType.ArrayLit, null);
            members.append(mapDecl);
            var lastValue = null;
            var memberNames = [];
            for(; ; ) {
                var minChar = this.scanner.startPos;
                var limChar;
                var memberName = null;
                var memberValue = null;
                var preComments = null;
                var postComments = null;
                if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToIDName(this.currentToken)) {
                    memberName = TypeScript.Identifier.fromToken(this.currentToken);
                    memberName.minChar = this.scanner.startPos;
                    memberName.limChar = this.scanner.pos;
                    memberNames.push(memberName);
                } else if(this.currentToken.tokenId == TypeScript.TokenID.CloseBrace) {
                    break;
                } else {
                    this.reportParseError("Expected identifer of enum member");
                    if(this.errorRecovery) {
                        memberName = new TypeScript.MissingIdentifier();
                        memberName.minChar = this.scanner.startPos;
                        memberName.limChar = this.scanner.startPos;
                        memberName.flags |= TypeScript.ASTFlags.Error;
                    }
                }
                limChar = this.scanner.pos;
                preComments = this.parseComments();
                this.currentToken = this.scanner.scan();
                postComments = this.parseComments();
                if(this.currentToken.tokenId == TypeScript.TokenID.Equals) {
                    this.currentToken = this.scanner.scan();
                    memberValue = this.parseExpr(errorRecoverySet, TypeScript.OperatorPrecedence.Comma, true, TypeContext.NoTypes);
                    lastValue = memberValue;
                    limChar = memberValue.limChar;
                } else {
                    if(lastValue == null) {
                        memberValue = new TypeScript.NumberLiteral(0, "0");
                        lastValue = memberValue;
                    } else {
                        var nextValue = lastValue.value + 1;
                        memberValue = new TypeScript.NumberLiteral(nextValue, nextValue.toString());
                        lastValue = memberValue;
                    }
                    var map = new TypeScript.BinaryExpression(TypeScript.NodeType.Asg, new TypeScript.BinaryExpression(TypeScript.NodeType.Index, new TypeScript.Identifier("_map"), memberValue), new TypeScript.StringLiteral('"' + memberName.actualText + '"'));
                    members.append(map);
                }
                var member = new TypeScript.VarDecl(memberName, this.nestingLevel);
                member.minChar = minChar;
                member.limChar = limChar;
                member.init = memberValue;
                member.typeExpr = new TypeScript.TypeReference(this.createRef(name.actualText, name.hasEscapeSequence, -1), 0);
                member.varFlags |= (TypeScript.VarFlags.Readonly | TypeScript.VarFlags.Property);
                if(memberValue.nodeType == TypeScript.NodeType.NumberLit) {
                    member.varFlags |= TypeScript.VarFlags.Constant;
                } else if(memberValue.nodeType === TypeScript.NodeType.Lsh) {
                    var binop = memberValue;
                    if(binop.operand1.nodeType === TypeScript.NodeType.NumberLit && binop.operand2.nodeType === TypeScript.NodeType.NumberLit) {
                        member.varFlags |= TypeScript.VarFlags.Constant;
                    }
                } else if(memberValue.nodeType === TypeScript.NodeType.Name) {
                    var nameNode = memberValue;
                    for(var i = 0; i < memberNames.length; i++) {
                        var memberName = memberNames[i];
                        if(memberName.text === nameNode.text) {
                            member.varFlags |= TypeScript.VarFlags.Constant;
                            break;
                        }
                    }
                }
                member.preComments = preComments;
                members.append(member);
                member.postComments = postComments;
                member.varFlags |= TypeScript.VarFlags.Exported;
                if(this.currentToken.tokenId == TypeScript.TokenID.Comma) {
                    this.currentToken = this.scanner.scan();
                    member.postComments = this.combineComments(member.postComments, this.parseCommentsForLine(this.scanner.prevLine));
                    if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || (TypeScript.convertTokToIDName(this.currentToken))) {
                        continue;
                    }
                }
                break;
            }
            var endingToken = new TypeScript.ASTSpan();
            endingToken.minChar = this.scanner.startPos;
            endingToken.limChar = this.scanner.pos;
            this.checkCurrentToken(TypeScript.TokenID.CloseBrace, errorRecoverySet);
            members.limChar = this.scanner.lastTokenLimChar();
            var modDecl = new TypeScript.ModuleDeclaration(name, members, this.topVarList(), endingToken);
            modDecl.modFlags |= TypeScript.ModuleFlags.IsEnum;
            this.popDeclLists();
            modDecl.leftCurlyCount = this.scanner.leftCurlyCount - leftCurlyCount;
            modDecl.rightCurlyCount = this.scanner.rightCurlyCount - rightCurlyCount;
            return modDecl;
        };
        Parser.prototype.parseDottedName = function (enclosedList) {
            this.currentToken = this.scanner.scan();
            if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                var id = TypeScript.Identifier.fromToken(this.currentToken);
                id.preComments = this.parseComments();
                enclosedList[enclosedList.length] = id;
                id.minChar = this.scanner.startPos;
                id.limChar = this.scanner.pos;
                this.currentToken = this.scanner.scan();
                if(this.currentToken.tokenId == TypeScript.TokenID.Dot) {
                    this.parseDottedName(enclosedList);
                }
            } else {
                this.reportParseError("need identifier after '.'");
            }
        };
        Parser.prototype.isValidImportPath = function (importPath) {
            importPath = TypeScript.stripQuotes(importPath);
            if(!importPath || importPath.indexOf(':') != -1 || importPath.indexOf('\\') != -1 || importPath.charAt(0) == '/') {
                return false;
            }
            return true;
        };
        Parser.prototype.parseImportDeclaration = function (errorRecoverySet, modifiers) {
            var name = null;
            var alias = null;
            var importDecl = null;
            var minChar = this.scanner.startPos;
            var isDynamicImport = false;
            this.currentToken = this.scanner.scan();
            if(this.currentToken.tokenId == TypeScript.TokenID.Identifier || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                name = TypeScript.Identifier.fromToken(this.currentToken);
            } else {
                this.reportParseError("Expected identifer after 'import'");
                name = new TypeScript.MissingIdentifier();
            }
            name.minChar = this.scanner.startPos;
            name.limChar = this.scanner.pos;
            this.currentToken = this.scanner.scan();
            this.checkCurrentToken(TypeScript.TokenID.Equals, errorRecoverySet | TypeScript.ErrorRecoverySet.ID);
            var aliasPreComments = this.parseComments();
            var limChar;
            if(this.currentToken.tokenId == TypeScript.TokenID.Identifier || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                if(this.currentToken.tokenId == TypeScript.TokenID.Module) {
                    limChar = this.scanner.pos;
                    this.currentToken = this.scanner.scan();
                    if(this.currentToken.tokenId == TypeScript.TokenID.OpenParen) {
                        this.currentToken = this.scanner.scan();
                        if(this.currentToken.tokenId == TypeScript.TokenID.StringLiteral || this.currentToken.tokenId == TypeScript.TokenID.Identifier || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                            if(this.currentToken.tokenId == TypeScript.TokenID.StringLiteral) {
                                if(this.topLevel) {
                                    this.hasTopLevelImportOrExport = true;
                                } else if(!this.allowImportDeclaration) {
                                    this.reportParseError("Import declaration of external module is permitted only in global or top level dynamic modules");
                                }
                                var aliasText = this.currentToken.getText();
                                alias = TypeScript.Identifier.fromToken(this.currentToken);
                                alias.minChar = this.scanner.startPos;
                                alias.limChar = this.scanner.pos;
                                if(!this.isValidImportPath((alias).text)) {
                                    this.reportParseError("Invalid import path");
                                }
                                isDynamicImport = true;
                                this.currentToken = this.scanner.scan();
                                alias.preComments = aliasPreComments;
                            } else {
                                alias = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon, TypeScript.OperatorPrecedence.Assignment, true, TypeContext.NoTypes);
                                alias.preComments = aliasPreComments;
                            }
                        }
                        limChar = this.scanner.pos;
                        this.checkCurrentToken(TypeScript.TokenID.CloseParen, errorRecoverySet | TypeScript.ErrorRecoverySet.ID);
                        if(alias) {
                            alias.postComments = this.parseComments();
                        }
                    }
                } else {
                    alias = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon, TypeScript.OperatorPrecedence.Assignment, true, TypeContext.NoTypes);
                    limChar = this.scanner.pos;
                }
            } else {
                this.reportParseError("Expected module name");
                alias = new TypeScript.MissingIdentifier();
                alias.minChar = this.scanner.startPos;
                if(this.currentToken.tokenId == TypeScript.TokenID.Semicolon) {
                    alias.limChar = this.scanner.startPos;
                } else {
                    alias.limChar = this.scanner.pos;
                    this.currentToken = this.scanner.scan();
                }
                alias.flags |= TypeScript.ASTFlags.Error;
                limChar = alias.limChar;
            }
            importDecl = new TypeScript.ImportDeclaration(name, alias);
            importDecl.isDynamicImport = isDynamicImport;
            importDecl.minChar = minChar;
            importDecl.limChar = limChar;
            return importDecl;
        };
        Parser.prototype.parseModuleDecl = function (errorRecoverySet, modifiers, preComments) {
            var leftCurlyCount = this.scanner.leftCurlyCount;
            var rightCurlyCount = this.scanner.rightCurlyCount;
            var svAmbient = this.ambientModule;
            var svTopLevel = this.topLevel;
            this.topLevel = false;
            if(this.parsingDeclareFile || svAmbient || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient)) {
                this.ambientModule = true;
            }
            this.currentToken = this.scanner.scan();
            var name = null;
            var enclosedList = null;
            this.pushDeclLists();
            var minChar = this.scanner.startPos;
            var isDynamicMod = false;
            if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || (this.currentToken.tokenId == TypeScript.TokenID.StringLiteral) || (!TypeScript.isPrimitiveTypeToken(this.currentToken) && TypeScript.convertTokToID(this.currentToken, this.strictMode))) {
                var nameText = this.currentToken.getText();
                if(this.currentToken.tokenId == TypeScript.TokenID.StringLiteral) {
                    isDynamicMod = true;
                    if(!this.ambientModule) {
                        this.reportParseError("Only ambient dynamic modules may have string literal names");
                    }
                    if(!svTopLevel) {
                        this.reportParseError("Dynamic modules may not be nested within other modules");
                    }
                }
                name = TypeScript.Identifier.fromToken(this.currentToken);
                name.minChar = this.scanner.startPos;
                name.limChar = this.scanner.pos;
                this.currentToken = this.scanner.scan();
            } else if(this.currentToken.tokenId == TypeScript.TokenID.OpenBrace) {
                this.reportParseError("Module name missing");
                name = new TypeScript.Identifier("");
                name.minChar = minChar;
                name.limChar = minChar;
            }
            if(this.currentToken.tokenId == TypeScript.TokenID.Dot) {
                enclosedList = new Array();
                this.parseDottedName(enclosedList);
            }
            if(name == null) {
                name = new TypeScript.MissingIdentifier();
            }
            var moduleBody = new TypeScript.ASTList();
            var bodyMinChar = this.scanner.startPos;
            this.checkCurrentToken(TypeScript.TokenID.OpenBrace, errorRecoverySet | TypeScript.ErrorRecoverySet.ID);
            if(svTopLevel && isDynamicMod) {
                this.allowImportDeclaration = true;
            } else {
                this.allowImportDeclaration = false;
            }
            this.parseStatementList(errorRecoverySet | TypeScript.ErrorRecoverySet.RCurly, moduleBody, true, true, TypeScript.AllowedElements.Global, modifiers);
            moduleBody.minChar = bodyMinChar;
            moduleBody.limChar = this.scanner.pos;
            var endingToken = new TypeScript.ASTSpan();
            endingToken.minChar = this.scanner.startPos;
            endingToken.limChar = this.scanner.pos;
            this.checkCurrentToken(TypeScript.TokenID.CloseBrace, errorRecoverySet);
            var limChar = this.scanner.lastTokenLimChar();
            var moduleDecl;
            this.allowImportDeclaration = svTopLevel;
            if(enclosedList && (enclosedList.length > 0)) {
                var len = enclosedList.length;
                var innerName = enclosedList[len - 1];
                var innerDecl = new TypeScript.ModuleDeclaration(innerName, moduleBody, this.topVarList(), endingToken);
                innerDecl.preComments = preComments;
                if(this.parsingDeclareFile || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient)) {
                    innerDecl.modFlags |= TypeScript.ModuleFlags.Ambient;
                }
                innerDecl.modFlags |= TypeScript.ModuleFlags.Exported;
                innerDecl.minChar = minChar;
                innerDecl.limChar = limChar;
                this.popDeclLists();
                var outerModBod;
                for(var i = len - 2; i >= 0; i--) {
                    outerModBod = new TypeScript.ASTList();
                    outerModBod.append(innerDecl);
                    innerName = enclosedList[i];
                    innerDecl = new TypeScript.ModuleDeclaration(innerName, outerModBod, new TypeScript.ASTList(), endingToken);
                    outerModBod.minChar = innerDecl.minChar = minChar;
                    outerModBod.limChar = innerDecl.limChar = limChar;
                    if(this.parsingDeclareFile || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient)) {
                        innerDecl.modFlags |= TypeScript.ModuleFlags.Ambient;
                    }
                    innerDecl.modFlags |= TypeScript.ModuleFlags.Exported;
                }
                outerModBod = new TypeScript.ASTList();
                outerModBod.append(innerDecl);
                outerModBod.minChar = minChar;
                outerModBod.limChar = limChar;
                moduleDecl = new TypeScript.ModuleDeclaration(name, outerModBod, new TypeScript.ASTList(), endingToken);
            } else {
                moduleDecl = new TypeScript.ModuleDeclaration(name, moduleBody, this.topVarList(), endingToken);
                moduleDecl.preComments = preComments;
                this.popDeclLists();
            }
            if(this.parsingDeclareFile || svAmbient || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient)) {
                moduleDecl.modFlags |= TypeScript.ModuleFlags.Ambient;
            }
            if(svAmbient || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Exported)) {
                moduleDecl.modFlags |= TypeScript.ModuleFlags.Exported;
            }
            if(isDynamicMod) {
                moduleDecl.modFlags |= TypeScript.ModuleFlags.IsDynamic;
            }
            this.ambientModule = svAmbient;
            this.topLevel = svTopLevel;
            moduleDecl.leftCurlyCount = this.scanner.leftCurlyCount - leftCurlyCount;
            moduleDecl.rightCurlyCount = this.scanner.rightCurlyCount - rightCurlyCount;
            moduleDecl.limChar = moduleBody.limChar;
            return moduleDecl;
        };
        Parser.prototype.parseTypeReferenceTail = function (errorRecoverySet, minChar, term) {
            var result = new TypeScript.TypeReference(term, 0);
            result.minChar = minChar;
            while(this.currentToken.tokenId == TypeScript.TokenID.OpenBracket) {
                this.currentToken = this.scanner.scan();
                result.arrayCount++;
                this.checkCurrentToken(TypeScript.TokenID.CloseBracket, errorRecoverySet | TypeScript.ErrorRecoverySet.LBrack);
            }
            result.limChar = this.scanner.lastTokenLimChar();
            return result;
        };
        Parser.prototype.parseNamedType = function (errorRecoverySet, minChar, term, tail) {
            this.currentToken = this.scanner.scan();
            if(this.currentToken.tokenId == TypeScript.TokenID.Dot) {
                var curpos = this.scanner.pos;
                this.currentToken = this.scanner.scan();
                if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || ((!this.errorRecovery || !this.scanner.lastTokenHadNewline()) && TypeScript.convertTokToID(this.currentToken, this.strictMode))) {
                    var op2 = TypeScript.Identifier.fromToken(this.currentToken);
                    op2.minChar = this.scanner.startPos;
                    op2.limChar = this.scanner.pos;
                    var dotNode = new TypeScript.BinaryExpression(TypeScript.NodeType.Dot, term, op2);
                    dotNode.minChar = term.minChar;
                    dotNode.limChar = op2.limChar;
                    return this.parseNamedType(errorRecoverySet, minChar, dotNode, tail);
                } else {
                    this.reportParseError("need identifier after '.'");
                    if(this.errorRecovery) {
                        term.flags |= TypeScript.ASTFlags.DotLHS;
                        term.limChar = this.scanner.lastTokenLimChar();
                        return term;
                    } else {
                        var eop2 = new TypeScript.MissingIdentifier();
                        eop2.minChar = this.scanner.pos;
                        eop2.limChar = this.scanner.pos;
                        var edotNode = new TypeScript.BinaryExpression(TypeScript.NodeType.Dot, term, eop2);
                        edotNode.flags |= TypeScript.ASTFlags.Error;
                        edotNode.minChar = term.minChar;
                        edotNode.limChar = eop2.limChar;
                        return this.parseNamedType(errorRecoverySet, minChar, edotNode, tail);
                    }
                }
            } else {
                if(tail) {
                    return this.parseTypeReferenceTail(errorRecoverySet, minChar, term);
                } else {
                    return term;
                }
            }
        };
        Parser.prototype.parseTypeReference = function (errorRecoverySet, allowVoid) {
            var minChar = this.scanner.startPos;
            var isConstructorMember = false;
            switch(this.currentToken.tokenId) {
                case TypeScript.TokenID.Void:
                    if(!allowVoid) {
                        this.reportParseError("void not a valid type in this context");
                    }
                case TypeScript.TokenID.Any:
                case TypeScript.TokenID.Number:
                case TypeScript.TokenID.Bool:
                case TypeScript.TokenID.String: {
                    var text = TypeScript.tokenTable[this.currentToken.tokenId].text;
                    var predefinedIdentifier = new TypeScript.Identifier(text);
                    predefinedIdentifier.minChar = minChar;
                    predefinedIdentifier.limChar = this.scanner.pos;
                    this.currentToken = this.scanner.scan();
                    return this.parseTypeReferenceTail(errorRecoverySet, minChar, predefinedIdentifier);
                }
                case TypeScript.TokenID.Identifier:
                    var ident = this.createRef(this.currentToken.getText(), (this.currentToken).hasEscapeSequence, minChar);
                    ident.limChar = this.scanner.pos;
                    return this.parseNamedType(errorRecoverySet, minChar, ident, true);
                case TypeScript.TokenID.OpenBrace:
                    return this.parseObjectType(minChar, errorRecoverySet);
                case TypeScript.TokenID.New:
                    this.currentToken = this.scanner.scan();
                    if(this.currentToken.tokenId != TypeScript.TokenID.OpenParen) {
                        this.reportParseError("Expected '('");
                    } else {
                        isConstructorMember = true;
                    }
                case TypeScript.TokenID.OpenParen: {
                    var formals = new TypeScript.ASTList();
                    var variableArgList = this.parseFormalParameterList(errorRecoverySet | TypeScript.ErrorRecoverySet.RParen, formals, false, true, false, false, false, false, null, true);
                    this.checkCurrentToken(TypeScript.TokenID.EqualsGreaterThan, errorRecoverySet);
                    var returnType = this.parseTypeReference(errorRecoverySet, true);
                    var funcDecl = new TypeScript.FuncDecl(null, null, false, formals, null, null, null, TypeScript.NodeType.FuncDecl);
                    funcDecl.returnTypeAnnotation = returnType;
                    funcDecl.variableArgList = variableArgList;
                    funcDecl.fncFlags |= TypeScript.FncFlags.Signature;
                    if(isConstructorMember) {
                        funcDecl.fncFlags |= TypeScript.FncFlags.ConstructMember;
                        funcDecl.hint = "_construct";
                        funcDecl.classDecl = null;
                    }
                    funcDecl.minChar = minChar;
                    return this.parseTypeReferenceTail(errorRecoverySet, minChar, funcDecl);
                }
                default:
                    this.reportParseError("Expected type name");
                    var etr = new TypeScript.TypeReference(null, 0);
                    etr.flags |= TypeScript.ASTFlags.Error;
                    etr.minChar = this.scanner.pos;
                    etr.limChar = this.scanner.pos;
                    return etr;
            }
        };
        Parser.prototype.parseObjectType = function (minChar, errorRecoverySet) {
            this.currentToken = this.scanner.scan();
            var members = new TypeScript.ASTList();
            members.minChar = minChar;
            var prevInInterfaceDecl = this.inInterfaceDecl;
            this.inInterfaceDecl = true;
            this.parseTypeMemberList(errorRecoverySet | TypeScript.ErrorRecoverySet.RCurly, members);
            this.inInterfaceDecl = prevInInterfaceDecl;
            this.checkCurrentToken(TypeScript.TokenID.CloseBrace, errorRecoverySet);
            var interfaceDecl = new TypeScript.InterfaceDeclaration(this.anonId, members, null, null);
            interfaceDecl.minChar = minChar;
            interfaceDecl.limChar = members.limChar;
            return this.parseTypeReferenceTail(errorRecoverySet, minChar, interfaceDecl);
        };
        Parser.prototype.parseFunctionBlock = function (errorRecoverySet, allowedElements, parentModifiers, bod, bodMinChar) {
            this.checkCurrentToken(TypeScript.TokenID.OpenBrace, errorRecoverySet | TypeScript.ErrorRecoverySet.StmtStart);
            var savedInFunction = this.inFunction;
            this.inFunction = true;
            this.parseStatementList(errorRecoverySet | TypeScript.ErrorRecoverySet.RCurly | TypeScript.ErrorRecoverySet.StmtStart, bod, true, false, allowedElements, parentModifiers);
            bod.minChar = bodMinChar;
            bod.limChar = this.scanner.pos;
            this.inFunction = savedInFunction;
            if(bod.limChar > bod.minChar) {
                var ec = new TypeScript.EndCode();
                ec.minChar = bod.limChar;
                ec.limChar = ec.minChar;
                bod.append(ec);
            }
        };
        Parser.prototype.parseFunctionStatements = function (errorRecoverySet, name, isConstructor, isMethod, args, allowedElements, minChar, requiresSignature, parentModifiers) {
            this.pushDeclLists();
            var svStmtStack = this.statementInfoStack;
            this.resetStmtStack();
            var bod = null;
            var wasShorthand = false;
            var isAnonLambda = false;
            var limChar;
            if(requiresSignature) {
                limChar = this.scanner.pos;
                if(this.currentToken.tokenId === TypeScript.TokenID.OpenBrace) {
                    this.reportParseError("Function declarations are not permitted within interfaces, ambient modules or classes");
                    bod = new TypeScript.ASTList();
                    var bodMinChar = this.scanner.startPos;
                    this.parseFunctionBlock(errorRecoverySet, allowedElements, parentModifiers, bod, bodMinChar);
                    this.checkCurrentToken(TypeScript.TokenID.CloseBrace, errorRecoverySet);
                    if(this.currentToken.tokenId === TypeScript.TokenID.Semicolon) {
                        this.currentToken = this.scanner.scan();
                    }
                } else {
                    this.checkCurrentToken(TypeScript.TokenID.Semicolon, errorRecoverySet, "Expected ';'");
                }
            } else {
                bod = new TypeScript.ASTList();
                var bodMinChar = this.scanner.startPos;
                if(this.currentToken.tokenId == TypeScript.TokenID.EqualsGreaterThan) {
                    if(isMethod) {
                        this.reportParseError("'=>' may not be used for class methods");
                    }
                    wasShorthand = true;
                    this.currentToken = this.scanner.scan();
                }
                if(wasShorthand && this.currentToken.tokenId != TypeScript.TokenID.OpenBrace) {
                    var retExpr = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon, TypeScript.OperatorPrecedence.Assignment, true, TypeContext.NoTypes);
                    var retStmt = new TypeScript.ReturnStatement();
                    retStmt.returnExpression = retExpr;
                    retStmt.minChar = retExpr.minChar;
                    retStmt.limChar = retExpr.limChar;
                    bod.minChar = bodMinChar;
                    bod.limChar = retExpr.limChar;
                    bod.append(retStmt);
                } else if(this.currentToken.tokenId != TypeScript.TokenID.EndOfFile) {
                    isAnonLambda = wasShorthand;
                    this.parseFunctionBlock(errorRecoverySet, allowedElements, parentModifiers, bod, bodMinChar);
                }
                limChar = this.scanner.pos;
            }
            var funcDecl = new TypeScript.FuncDecl(name, bod, isConstructor, args, this.topVarList(), this.topScopeList(), this.topStaticsList(), TypeScript.NodeType.FuncDecl);
            this.popDeclLists();
            var scopeList = this.topScopeList();
            scopeList.append(funcDecl);
            var staticFuncDecl = false;
            if(!requiresSignature) {
                if(!wasShorthand || isAnonLambda) {
                    funcDecl.endingToken = new TypeScript.ASTSpan();
                    funcDecl.endingToken.minChar = this.scanner.startPos;
                    funcDecl.endingToken.limChar = this.scanner.pos;
                    this.checkCurrentToken(TypeScript.TokenID.CloseBrace, errorRecoverySet);
                    if(isAnonLambda) {
                        funcDecl.fncFlags |= TypeScript.FncFlags.IsFatArrowFunction;
                    }
                } else {
                    funcDecl.fncFlags |= TypeScript.FncFlags.IsFatArrowFunction;
                    funcDecl.endingToken = new TypeScript.ASTSpan();
                    funcDecl.endingToken.minChar = bod.members[0].minChar;
                    funcDecl.endingToken.limChar = bod.members[0].limChar;
                }
            }
            funcDecl.minChar = minChar;
            funcDecl.limChar = limChar;
            if(requiresSignature) {
                funcDecl.fncFlags |= TypeScript.FncFlags.Signature;
            }
            this.statementInfoStack = svStmtStack;
            return funcDecl;
        };
        Parser.prototype.transformAnonymousArgsIntoFormals = function (formals, argList) {
            var _this = this;
            var translateBinExOperand = function (operand) {
                if(operand.nodeType == TypeScript.NodeType.Comma) {
                    return _this.transformAnonymousArgsIntoFormals(formals, operand);
                } else if(operand.nodeType == TypeScript.NodeType.Name || operand.nodeType == TypeScript.NodeType.Asg) {
                    var opArg = operand.nodeType == TypeScript.NodeType.Asg ? (operand).operand1 : operand;
                    opArg.isParenthesized = false;
                    var arg = new TypeScript.ArgDecl(opArg);
                    arg.preComments = opArg.preComments;
                    arg.postComments = opArg.postComments;
                    arg.minChar = operand.minChar;
                    arg.limChar = operand.limChar;
                    if(TypeScript.hasFlag(opArg.flags, TypeScript.ASTFlags.PossibleOptionalParameter)) {
                        arg.isOptional = true;
                    }
                    if(operand.nodeType == TypeScript.NodeType.Asg) {
                        arg.init = (operand).operand2;
                    }
                    formals.append(arg);
                    return arg.isOptional || arg.init;
                } else {
                    _this.reportParseError("Invalid lambda argument");
                }
                return false;
            };
            if(argList) {
                if(argList.nodeType == TypeScript.NodeType.Comma) {
                    var commaList = argList;
                    if(commaList.operand1.isParenthesized) {
                        this.reportParseError("Invalid lambda argument", commaList.operand1.minChar, commaList.operand1.limChar);
                    }
                    if(commaList.operand2.isParenthesized) {
                        this.reportParseError("Invalid lambda argument", commaList.operand2.minChar, commaList.operand2.limChar);
                    }
                    var isOptional = translateBinExOperand(commaList.operand1);
                    isOptional = translateBinExOperand(commaList.operand2) || isOptional;
                    return isOptional;
                } else {
                    return translateBinExOperand(argList);
                }
            }
        };
        Parser.prototype.parseFormalParameterList = function (errorRecoverySet, formals, isClassConstr, isSig, isIndexer, isGetter, isSetter, isLambda, preProcessedLambdaArgs, expectClosingRParen) {
            formals.minChar = this.scanner.startPos;
            if(isIndexer) {
                this.currentToken = this.scanner.scan();
            } else if(!isLambda) {
                this.checkCurrentToken(TypeScript.TokenID.OpenParen, errorRecoverySet | TypeScript.ErrorRecoverySet.RParen);
            }
            var sawEllipsis = false;
            var firstArg = true;
            var hasOptional = false;
            var haveFirstArgID = false;
            if(isLambda && preProcessedLambdaArgs && preProcessedLambdaArgs.nodeType != TypeScript.NodeType.EmptyExpr) {
                hasOptional = this.transformAnonymousArgsIntoFormals(formals, preProcessedLambdaArgs);
                formals.minChar = preProcessedLambdaArgs.minChar;
                haveFirstArgID = true;
            }
            while(true) {
                var munchedArg = false;
                var argFlags = TypeScript.VarFlags.None;
                var argMinChar = this.scanner.startPos;
                if(this.inferPropertiesFromThisAssignment && this.currentToken.tokenId == TypeScript.TokenID.This) {
                    if(!isClassConstr) {
                        this.reportParseError("Instance property declarations using 'this' may only be used in class constructors");
                    }
                    this.currentToken = this.scanner.scan();
                    argFlags |= (TypeScript.VarFlags.Public | TypeScript.VarFlags.Property);
                    if(this.currentClassDefinition) {
                        this.currentClassDefinition.varFlags |= TypeScript.VarFlags.ClassSuperMustBeFirstCallInConstructor;
                    }
                }
                if(this.currentToken.tokenId == TypeScript.TokenID.Public) {
                    argFlags |= (TypeScript.VarFlags.Public | TypeScript.VarFlags.Property);
                    if(this.currentClassDefinition) {
                        this.currentClassDefinition.varFlags |= TypeScript.VarFlags.ClassSuperMustBeFirstCallInConstructor;
                    }
                } else if(this.currentToken.tokenId == TypeScript.TokenID.Private) {
                    argFlags |= (TypeScript.VarFlags.Private | TypeScript.VarFlags.Property);
                    if(this.currentClassDefinition) {
                        this.currentClassDefinition.varFlags |= TypeScript.VarFlags.ClassSuperMustBeFirstCallInConstructor;
                    }
                } else if(this.currentToken.tokenId == TypeScript.TokenID.Static && isClassConstr) {
                    this.reportParseError("Static properties can not be declared as parameter properties");
                    this.currentToken = this.scanner.scan();
                }
                if(argFlags != TypeScript.VarFlags.None) {
                    if(!isClassConstr) {
                        this.reportParseError("only constructor parameters can be properties");
                    }
                    this.currentToken = this.scanner.scan();
                    if(TypeScript.isModifier(this.currentToken)) {
                        this.reportParseError("Multiple modifiers may not be applied to parameters");
                        this.currentToken = this.scanner.scan();
                    }
                    if(this.inferPropertiesFromThisAssignment && this.currentToken.tokenId == TypeScript.TokenID.This) {
                        if(!isClassConstr) {
                            this.reportParseError("Instance property declarations using 'this' may only be used in class constructors");
                        }
                        this.currentToken = this.scanner.scan();
                        this.currentToken = this.scanner.scan();
                    }
                } else if(this.currentToken.tokenId == TypeScript.TokenID.DotDotDot) {
                    sawEllipsis = true;
                    this.currentToken = this.scanner.scan();
                    if(!(this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                        this.reportParseError("'...' parameters require both a parameter name and an array type annotation to be specified");
                        sawEllipsis = false;
                    }
                }
                var argId = null;
                if(!haveFirstArgID && (this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                    argId = TypeScript.Identifier.fromToken(this.currentToken);
                    argId.minChar = this.scanner.startPos;
                    argId.limChar = this.scanner.pos;
                }
                if(haveFirstArgID || argId) {
                    munchedArg = true;
                    var type = null;
                    var arg = null;
                    if(haveFirstArgID && formals.members.length) {
                        arg = formals.members[formals.members.length - 1];
                        if(arg.isOptional) {
                            hasOptional = true;
                        }
                    } else {
                        arg = new TypeScript.ArgDecl(argId);
                        if(isGetter) {
                            this.reportParseError("Property getters may not take any arguments");
                        }
                        if(isSetter && !firstArg) {
                            this.reportParseError("Property setters may only take one argument");
                        }
                        arg.minChar = argMinChar;
                        arg.preComments = this.parseComments();
                        this.currentToken = this.scanner.scan();
                    }
                    if(this.currentToken.tokenId == TypeScript.TokenID.Question) {
                        arg.isOptional = true;
                        hasOptional = true;
                        this.currentToken = this.scanner.scan();
                    }
                    if(this.currentToken.tokenId == TypeScript.TokenID.Colon) {
                        this.currentToken = this.scanner.scan();
                        type = this.parseTypeReference(errorRecoverySet, false);
                    }
                    if(this.currentToken.tokenId == TypeScript.TokenID.Equals) {
                        if(isSig) {
                            this.reportParseError("Arguments in signatures may not have default values");
                        }
                        hasOptional = true;
                        this.currentToken = this.scanner.scan();
                        arg.init = this.parseExpr(TypeScript.ErrorRecoverySet.Comma | errorRecoverySet, TypeScript.OperatorPrecedence.Comma, false, TypeContext.NoTypes);
                    }
                    if(hasOptional && !arg.isOptionalArg() && !sawEllipsis) {
                        this.reportParseError("Optional parameters may only be followed by other optional parameters");
                    }
                    if(sawEllipsis && arg.isOptionalArg()) {
                        this.reportParseError("Varargs may not be optional or have default parameters");
                    }
                    if(sawEllipsis && !type) {
                        this.reportParseError("'...' parameters require both a parameter name and an array type annotation to be specified");
                    }
                    arg.postComments = this.parseComments();
                    arg.typeExpr = type;
                    arg.limChar = this.scanner.lastTokenLimChar();
                    arg.varFlags |= argFlags;
                    if(!haveFirstArgID) {
                        formals.append(arg);
                    } else {
                        haveFirstArgID = false;
                    }
                }
                firstArg = false;
                if(this.currentToken.tokenId == TypeScript.TokenID.Comma) {
                    if((munchedArg) && (!sawEllipsis)) {
                        this.currentToken = this.scanner.scan();
                        continue;
                    } else {
                        this.reportParseError("Unexpected ',' in argument list");
                        if(this.errorRecovery) {
                            this.currentToken = this.scanner.scan();
                            continue;
                        }
                    }
                } else {
                    break;
                }
            }
            if(isIndexer) {
                this.checkCurrentToken(TypeScript.TokenID.CloseBracket, errorRecoverySet | TypeScript.ErrorRecoverySet.LCurly | TypeScript.ErrorRecoverySet.SColon);
            } else if(expectClosingRParen) {
                this.checkCurrentToken(TypeScript.TokenID.CloseParen, errorRecoverySet | TypeScript.ErrorRecoverySet.LCurly | TypeScript.ErrorRecoverySet.SColon);
            }
            formals.limChar = this.currentToken.tokenId == TypeScript.TokenID.EndOfFile ? this.scanner.pos : this.scanner.lastTokenLimChar();
            return sawEllipsis;
        };
        Parser.prototype.parseFncDecl = function (errorRecoverySet, isDecl, requiresSignature, isMethod, methodName, indexer, isStatic, markedAsAmbient, modifiers, lambdaArgContext, expectClosingRParen) {
            var leftCurlyCount = this.scanner.leftCurlyCount;
            var rightCurlyCount = this.scanner.rightCurlyCount;
            var prevInConstr = this.parsingClassConstructorDefinition;
            this.parsingClassConstructorDefinition = false;
            var name = null;
            var fnMin = this.scanner.startPos;
            var minChar = this.scanner.pos;
            var prevNestingLevel = this.nestingLevel;
            var preComments = this.parseComments();
            var isLambda = !!lambdaArgContext;
            this.nestingLevel = 0;
            if((!this.style_funcInLoop) && this.inLoop()) {
                this.reportParseStyleError("function declaration in loop");
            }
            if(!isMethod && !isStatic && !indexer && !lambdaArgContext && !methodName) {
                this.currentToken = this.scanner.scan();
                if((this.currentToken.tokenId != TypeScript.TokenID.Identifier) && (!TypeScript.convertTokToID(this.currentToken, this.strictMode))) {
                    if(isDecl) {
                        this.reportParseError("Function declaration must include identifier");
                        this.nestingLevel = prevNestingLevel;
                        return new TypeScript.IncompleteAST(fnMin, this.scanner.pos);
                    }
                } else {
                    name = TypeScript.Identifier.fromToken(this.currentToken);
                    name.minChar = this.scanner.startPos;
                    name.limChar = this.scanner.pos;
                    this.currentToken = this.scanner.scan();
                }
            } else {
                if(methodName) {
                    name = methodName;
                }
            }
            var args = new TypeScript.ASTList();
            var variableArgList = false;
            var isOverload = false;
            var isGetter = TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Getter);
            var isSetter = TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Setter);
            if((this.currentToken.tokenId == TypeScript.TokenID.OpenParen) || (indexer && (this.currentToken.tokenId == TypeScript.TokenID.OpenBracket)) || (lambdaArgContext && (lambdaArgContext.preProcessedLambdaArgs || this.currentToken.tokenId == TypeScript.TokenID.DotDotDot))) {
                variableArgList = this.parseFormalParameterList(errorRecoverySet, args, false, requiresSignature, indexer, isGetter, isSetter, isLambda, lambdaArgContext ? lambdaArgContext.preProcessedLambdaArgs : null, expectClosingRParen);
            }
            var returnType = null;
            if(this.currentToken.tokenId == TypeScript.TokenID.Colon) {
                this.currentToken = this.scanner.scan();
                if(TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Setter)) {
                    this.reportParseError("Property setters may not declare a return type");
                }
                returnType = this.parseTypeReference(errorRecoverySet, true);
            }
            if(indexer && args.members.length == 0) {
                this.reportParseError("Index signatures require a parameter type to be specified");
            }
            if(isLambda && this.currentToken.tokenId != TypeScript.TokenID.EqualsGreaterThan) {
                this.reportParseError("Expected '=>'");
            }
            if(isDecl && !(this.parsingDeclareFile || markedAsAmbient) && !this.ambientModule && !this.ambientClass && !this.inInterfaceDecl && this.currentToken.tokenId == TypeScript.TokenID.Semicolon) {
                isOverload = true;
                isDecl = false;
                requiresSignature = true;
            }
            var svInFncDecl = this.inFncDecl;
            this.inFncDecl = true;
            var funcDecl = this.parseFunctionStatements(errorRecoverySet | TypeScript.ErrorRecoverySet.RCurly, name, false, isMethod, args, TypeScript.AllowedElements.None, minChar, requiresSignature, TypeScript.Modifiers.None);
            this.inFncDecl = svInFncDecl;
            funcDecl.variableArgList = variableArgList;
            funcDecl.isOverload = isOverload;
            if(isStatic) {
                funcDecl.fncFlags |= TypeScript.FncFlags.Static;
            }
            if(requiresSignature) {
                funcDecl.fncFlags |= TypeScript.FncFlags.Signature;
            }
            if(indexer) {
                funcDecl.fncFlags |= TypeScript.FncFlags.IndexerMember;
            }
            funcDecl.returnTypeAnnotation = returnType;
            if(isMethod) {
                funcDecl.fncFlags |= TypeScript.FncFlags.Method;
                funcDecl.fncFlags |= TypeScript.FncFlags.ClassPropertyMethodExported;
            }
            funcDecl.leftCurlyCount = this.scanner.leftCurlyCount - leftCurlyCount;
            funcDecl.rightCurlyCount = this.scanner.rightCurlyCount - rightCurlyCount;
            this.nestingLevel = prevNestingLevel;
            this.parsingClassConstructorDefinition = prevInConstr;
            funcDecl.preComments = preComments;
            return funcDecl;
        };
        Parser.prototype.convertToTypeReference = function (ast) {
            var result;
            switch(ast.nodeType) {
                case TypeScript.NodeType.TypeRef:
                    return ast;
                case TypeScript.NodeType.Name:
                    result = new TypeScript.TypeReference(ast, 0);
                    result.minChar = ast.minChar;
                    result.limChar = ast.limChar;
                    return result;
                case TypeScript.NodeType.Index: {
                    var expr = ast;
                    result = this.convertToTypeReference(expr.operand1);
                    if(result) {
                        result.arrayCount++;
                        result.minChar = expr.minChar;
                        result.limChar = expr.limChar;
                        return result;
                    } else {
                        var etr = new TypeScript.AST(TypeScript.NodeType.Error);
                        return etr;
                    }
                }
            }
            return null;
        };
        Parser.prototype.parseArgList = function (errorRecoverySet) {
            var args = new TypeScript.ASTList();
            args.minChar = this.scanner.startPos;
            this.currentToken = this.scanner.scan();
            if(this.currentToken.tokenId !== TypeScript.TokenID.CloseParen) {
                while(true) {
                    if(args.members.length > 0xffff) {
                        this.reportParseError("max number of args exceeded");
                        break;
                    }
                    var arg = this.parseExpr(TypeScript.ErrorRecoverySet.Comma | errorRecoverySet, TypeScript.OperatorPrecedence.Comma, true, TypeContext.NoTypes);
                    args.append(arg);
                    if(this.currentToken.tokenId != TypeScript.TokenID.Comma) {
                        break;
                    }
                    this.currentToken = this.scanner.scan();
                }
            }
            args.limChar = this.scanner.pos;
            return args;
        };
        Parser.prototype.parseBaseList = function (extendsList, implementsList, errorRecoverySet, isClass) {
            var keyword = true;
            var currentList = extendsList;
            for(; ; ) {
                if(keyword) {
                    if(this.currentToken.tokenId === TypeScript.TokenID.Implements) {
                        currentList = implementsList;
                    } else if(this.currentToken.tokenId == TypeScript.TokenID.Extends && !this.requiresExtendsBlock) {
                        this.requiresExtendsBlock = isClass;
                    }
                    this.currentToken = this.scanner.scan();
                    keyword = false;
                }
                var baseName = null;
                if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                    var minChar = this.scanner.startPos;
                    baseName = TypeScript.Identifier.fromToken(this.currentToken);
                    baseName.minChar = minChar;
                    baseName.limChar = this.scanner.pos;
                    baseName = this.parseNamedType(errorRecoverySet | TypeScript.ErrorRecoverySet.LCurly, minChar, baseName, false);
                } else {
                    this.reportParseError("Expected base name");
                    if(this.errorRecovery) {
                        baseName = new TypeScript.MissingIdentifier();
                        baseName.minChar = this.scanner.pos;
                        baseName.limChar = this.scanner.pos;
                        baseName.flags |= TypeScript.ASTFlags.Error;
                    }
                }
                if(this.currentToken.tokenId == TypeScript.TokenID.OpenParen) {
                    if(isClass) {
                        this.reportParseError("Base classes may only be initialized via a 'super' call within the constructor body");
                    } else {
                        this.reportParseError("Interfaces may not be extended with a call expression");
                    }
                } else {
                    currentList.append(baseName);
                }
                if(isClass && currentList == extendsList && extendsList.members.length > 1) {
                    this.reportParseError("A class may only extend one other class");
                }
                if(this.currentToken.tokenId == TypeScript.TokenID.Comma) {
                    this.currentToken = this.scanner.scan();
                    continue;
                } else if((this.currentToken.tokenId == TypeScript.TokenID.Extends) || (this.currentToken.tokenId == TypeScript.TokenID.Implements)) {
                    if(this.currentToken.tokenId == TypeScript.TokenID.Extends && !this.requiresExtendsBlock) {
                        this.requiresExtendsBlock = isClass;
                    }
                    currentList = extendsList;
                    keyword = true;
                    continue;
                }
                break;
            }
        };
        Parser.prototype.parseClassDecl = function (errorRecoverySet, minChar, modifiers) {
            var leftCurlyCount = this.scanner.leftCurlyCount;
            var rightCurlyCount = this.scanner.rightCurlyCount;
            if((modifiers & TypeScript.Modifiers.Readonly) != TypeScript.Modifiers.None) {
                this.reportParseError("const modifier is implicit for class");
            }
            if(this.parsingDeclareFile || this.ambientModule) {
                modifiers |= TypeScript.Modifiers.Ambient;
                modifiers |= TypeScript.Modifiers.Exported;
            }
            var classIsMarkedAsAmbient = this.parsingDeclareFile || (modifiers & TypeScript.Modifiers.Ambient) != TypeScript.Modifiers.None;
            var svAmbientClass = this.ambientClass;
            this.ambientClass = classIsMarkedAsAmbient;
            this.currentToken = this.scanner.scan();
            var name = null;
            if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || (!TypeScript.isPrimitiveTypeToken(this.currentToken) && TypeScript.convertTokToID(this.currentToken, this.strictMode))) {
                name = TypeScript.Identifier.fromToken(this.currentToken);
                name.minChar = this.scanner.startPos;
                name.limChar = this.scanner.pos;
                this.currentToken = this.scanner.scan();
            } else {
                this.reportParseError("class missing name");
                if(this.errorRecovery) {
                    name = new TypeScript.MissingIdentifier();
                    name.minChar = this.scanner.pos;
                    name.limChar = this.scanner.pos;
                    name.flags |= TypeScript.ASTFlags.Error;
                }
            }
            var extendsList = null;
            var implementsList = null;
            var requiresSignature = false;
            if((this.currentToken.tokenId == TypeScript.TokenID.Extends) || (this.currentToken.tokenId == TypeScript.TokenID.Implements)) {
                extendsList = new TypeScript.ASTList();
                implementsList = new TypeScript.ASTList();
                this.parseBaseList(extendsList, implementsList, errorRecoverySet, true);
            }
            var classDecl = new TypeScript.ClassDeclaration(name, new TypeScript.ASTList(), extendsList, implementsList);
            this.currentClassDefinition = classDecl;
            this.parseClassElements(classDecl, errorRecoverySet, modifiers);
            if(this.ambientModule || this.parsingDeclareFile || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Exported)) {
                classDecl.varFlags |= TypeScript.VarFlags.Exported;
            }
            if(this.ambientModule || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient)) {
                classDecl.varFlags |= TypeScript.VarFlags.Ambient;
            }
            classDecl.varFlags |= TypeScript.VarFlags.Class;
            this.ambientClass = svAmbientClass;
            classDecl.leftCurlyCount = this.scanner.leftCurlyCount - leftCurlyCount;
            classDecl.rightCurlyCount = this.scanner.rightCurlyCount - rightCurlyCount;
            return classDecl;
        };
        Parser.prototype.parseClassElements = function (classDecl, errorRecoverySet, parentModifiers) {
            var modifiers = parentModifiers;
            var resetModifiers = false;
            var membersMinChar = this.scanner.startPos;
            this.checkCurrentToken(TypeScript.TokenID.OpenBrace, errorRecoverySet);
            this.nestingLevel++;
            var currentMemberMinChar = this.scanner.startPos;
            var wasGetOrSetId = false;
            while(!(this.currentToken.tokenId == TypeScript.TokenID.CloseBrace || this.currentToken.tokenId == TypeScript.TokenID.EndOfFile)) {
                var scanNext = true;
                var publicOrPrivateFlags = TypeScript.Modifiers.Public | TypeScript.Modifiers.Private;
                if(this.currentToken.tokenId == TypeScript.TokenID.Get) {
                    if(modifiers & TypeScript.Modifiers.Getter) {
                        this.reportParseError("Duplicate 'get' declaration in class body");
                    }
                    if(modifiers & TypeScript.Modifiers.Setter) {
                        this.reportParseError("Getter already marked as a setter");
                    }
                    modifiers |= TypeScript.Modifiers.Getter;
                } else if(this.currentToken.tokenId == TypeScript.TokenID.Set) {
                    if(modifiers & TypeScript.Modifiers.Setter) {
                        this.reportParseError("Duplicate 'set' declaration in class body");
                    }
                    if(modifiers & TypeScript.Modifiers.Getter) {
                        this.reportParseError("Setter already marked as a getter");
                    }
                    modifiers |= TypeScript.Modifiers.Setter;
                } else if(this.currentToken.tokenId == TypeScript.TokenID.Private) {
                    if(modifiers & publicOrPrivateFlags) {
                        this.reportParseError("Multiple modifiers may not be applied to class members");
                    }
                    modifiers |= TypeScript.Modifiers.Private;
                } else if(this.currentToken.tokenId == TypeScript.TokenID.Public) {
                    if(modifiers & publicOrPrivateFlags) {
                        this.reportParseError("Multiple modifiers may not be applied to class members");
                    }
                    modifiers |= TypeScript.Modifiers.Public;
                } else if(this.currentToken.tokenId == TypeScript.TokenID.Static) {
                    if(modifiers & TypeScript.Modifiers.Static) {
                        this.reportParseError("Multiple modifiers may not be applied to class members");
                    }
                    modifiers |= TypeScript.Modifiers.Static;
                } else if(this.currentToken.tokenId == TypeScript.TokenID.Constructor) {
                    if(modifiers != parentModifiers) {
                        this.reportParseError("Constructors may not have modifiers");
                    }
                    this.parseClassConstructorDeclaration(currentMemberMinChar, errorRecoverySet, modifiers);
                    scanNext = false;
                    resetModifiers = true;
                } else if(wasGetOrSetId || this.currentToken.tokenId == TypeScript.TokenID.Identifier || TypeScript.convertTokToIDName(this.currentToken)) {
                    var idText = wasGetOrSetId ? ((modifiers & TypeScript.Modifiers.Getter) ? "get" : "set") : this.currentToken.getText();
                    var id = wasGetOrSetId ? new TypeScript.Identifier(idText) : TypeScript.Identifier.fromToken(this.currentToken);
                    id.minChar = this.scanner.startPos;
                    id.limChar = this.scanner.pos;
                    if(wasGetOrSetId) {
                        modifiers = modifiers ^ ((modifiers & TypeScript.Modifiers.Getter) ? TypeScript.Modifiers.Getter : TypeScript.Modifiers.Setter);
                        wasGetOrSetId = false;
                    } else {
                        this.currentToken = this.scanner.scan();
                    }
                    if(this.currentToken.tokenId == TypeScript.TokenID.OpenParen) {
                        this.parseClassMemberFunctionDeclaration(id, currentMemberMinChar, errorRecoverySet, modifiers);
                        scanNext = false;
                    } else {
                        if(modifiers & TypeScript.Modifiers.Getter || modifiers & TypeScript.Modifiers.Setter) {
                            this.reportParseError("Property accessors must be functions");
                        }
                        var varDecl = this.parseClassMemberVariableDeclaration(id, currentMemberMinChar, false, errorRecoverySet, modifiers);
                        if(varDecl.init && varDecl.init.nodeType == TypeScript.NodeType.FuncDecl) {
                            if(this.currentToken.tokenId == TypeScript.TokenID.CloseBrace) {
                                scanNext = false;
                            }
                        } else if(varDecl.init && varDecl.init.nodeType == TypeScript.NodeType.ObjectLit && this.currentToken.tokenId != TypeScript.TokenID.Semicolon) {
                            scanNext = false;
                            varDecl.init.flags |= TypeScript.ASTFlags.AutomaticSemicolon;
                        } else if(this.currentToken.tokenId != TypeScript.TokenID.Semicolon) {
                            this.reportParseError("Expected ';'");
                            scanNext = false;
                        }
                    }
                    resetModifiers = true;
                } else if(this.currentToken.tokenId == TypeScript.TokenID.Super) {
                    this.reportParseError("Base class initializers must be the first statement in a class definition");
                } else if(!wasGetOrSetId && ((modifiers & TypeScript.Modifiers.Getter) || (modifiers & TypeScript.Modifiers.Setter)) && ((this.currentToken.tokenId == TypeScript.TokenID.OpenParen) || (this.currentToken.tokenId == TypeScript.TokenID.Equals) || (this.currentToken.tokenId == TypeScript.TokenID.Colon) || (this.currentToken.tokenId == TypeScript.TokenID.Semicolon))) {
                    wasGetOrSetId = true;
                    scanNext = false;
                } else if(this.currentToken.tokenId != TypeScript.TokenID.Semicolon) {
                    this.reportParseError("Unexpected '" + this.currentToken.getText() + "' in class definition");
                    resetModifiers = true;
                }
                if(scanNext) {
                    this.currentToken = this.scanner.scan();
                }
                if(resetModifiers) {
                    modifiers = parentModifiers;
                    currentMemberMinChar = this.scanner.startPos;
                    resetModifiers = false;
                }
            }
            var membersLimChar = this.scanner.pos;
            if(this.currentToken.tokenId == TypeScript.TokenID.CloseBrace) {
                classDecl.endingToken = new TypeScript.ASTSpan();
                classDecl.endingToken.minChar = this.scanner.startPos;
                classDecl.endingToken.limChar = this.scanner.pos;
                if(!this.currentClassDefinition.members.members.length) {
                    this.currentClassDefinition.preComments = this.parseComments();
                }
                this.currentToken = this.scanner.scan();
            }
            this.nestingLevel--;
            this.currentClassDefinition.members.minChar = membersMinChar;
            this.currentClassDefinition.members.limChar = membersLimChar;
            this.currentClassDefinition.limChar = membersLimChar;
            this.currentClassDefinition = null;
        };
        Parser.prototype.parseClassConstructorDeclaration = function (minChar, errorRecoverySet, modifiers) {
            this.parsingClassConstructorDefinition = true;
            var isAmbient = this.parsingDeclareFile || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient);
            var args = new TypeScript.ASTList();
            var variableArgList = false;
            var preComments = this.parseComments();
            var constructorSpan = new TypeScript.ASTSpan();
            constructorSpan.minChar = this.scanner.startPos;
            constructorSpan.limChar = this.scanner.pos;
            this.currentToken = this.scanner.scan();
            if(this.currentToken.tokenId == TypeScript.TokenID.OpenParen) {
                variableArgList = this.parseFormalParameterList(errorRecoverySet, args, true, isAmbient, false, false, false, false, null, true);
                if(args.members.length > 0) {
                    var lastArg = args.members[args.members.length - 1];
                }
            }
            var requiresSignature = isAmbient || this.currentToken.tokenId == TypeScript.TokenID.Semicolon;
            if(requiresSignature) {
                for(var i = 0; i < args.members.length; i++) {
                    var arg = args.members[i];
                    if(TypeScript.hasFlag(arg.varFlags, TypeScript.VarFlags.Property)) {
                        this.reportParseError("Overload or ambient signatures may not specify parameter properties", arg.minChar, arg.limChar);
                    }
                }
            }
            if(!requiresSignature) {
                this.currentClassDefinition.constructorNestingLevel = this.nestingLevel + 1;
            }
            var constructorFuncDecl = this.parseFunctionStatements(errorRecoverySet | TypeScript.ErrorRecoverySet.RCurly, this.currentClassDefinition.name, true, false, args, TypeScript.AllowedElements.Properties, minChar, requiresSignature, modifiers);
            constructorFuncDecl.constructorSpan = constructorSpan;
            constructorFuncDecl.preComments = preComments;
            if(requiresSignature && !isAmbient) {
                constructorFuncDecl.isOverload = true;
            }
            constructorFuncDecl.variableArgList = variableArgList;
            this.currentClassDecl = null;
            constructorFuncDecl.returnTypeAnnotation = this.convertToTypeReference(this.currentClassDefinition.name);
            constructorFuncDecl.classDecl = this.currentClassDefinition;
            if(isAmbient) {
                constructorFuncDecl.fncFlags |= TypeScript.FncFlags.Ambient;
            }
            if(requiresSignature) {
                constructorFuncDecl.fncFlags |= TypeScript.FncFlags.Signature;
            }
            if(this.ambientModule || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Exported)) {
                constructorFuncDecl.fncFlags |= TypeScript.FncFlags.Exported;
            }
            if(this.currentClassDefinition.constructorDecl) {
                if(!isAmbient && !this.currentClassDefinition.constructorDecl.isSignature() && !constructorFuncDecl.isSignature()) {
                    this.reportParseError("Duplicate constructor definition");
                }
            }
            if(isAmbient || !constructorFuncDecl.isSignature()) {
                this.currentClassDefinition.constructorDecl = constructorFuncDecl;
            }
            constructorFuncDecl.fncFlags |= TypeScript.FncFlags.ClassMethod;
            this.currentClassDefinition.members.members[this.currentClassDefinition.members.members.length] = constructorFuncDecl;
            this.parsingClassConstructorDefinition = false;
            return constructorFuncDecl;
        };
        Parser.prototype.parseClassMemberVariableDeclaration = function (text, minChar, isDeclaredInConstructor, errorRecoverySet, modifiers) {
            var varDecl = new TypeScript.VarDecl(text, this.nestingLevel);
            varDecl.minChar = minChar;
            var isStatic = false;
            varDecl.preComments = this.parseComments();
            if(this.currentToken.tokenId == TypeScript.TokenID.Colon) {
                this.currentToken = this.scanner.scan();
                varDecl.typeExpr = this.parseTypeReference(errorRecoverySet | TypeScript.ErrorRecoverySet.Asg | TypeScript.ErrorRecoverySet.Comma, false);
                if(varDecl.typeExpr && varDecl.typeExpr.nodeType == TypeScript.NodeType.TypeRef) {
                    var typeExpr = (varDecl.typeExpr);
                    if(typeExpr.term && typeExpr.term.nodeType == TypeScript.NodeType.FuncDecl) {
                        typeExpr.term.preComments = varDecl.preComments;
                    }
                }
            }
            if(this.currentToken.tokenId == TypeScript.TokenID.Equals) {
                if(this.parsingDeclareFile || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient)) {
                    this.reportParseError("context does not permit variable initializer");
                    if(this.errorRecovery) {
                        this.skip(errorRecoverySet);
                        varDecl.flags |= TypeScript.ASTFlags.Error;
                        varDecl.limChar = this.scanner.lastTokenLimChar();
                        return varDecl;
                    }
                }
                this.currentToken = this.scanner.scan();
                varDecl.init = this.parseExpr(TypeScript.ErrorRecoverySet.Comma | errorRecoverySet, TypeScript.OperatorPrecedence.Comma, true, TypeContext.NoTypes);
                varDecl.limChar = varDecl.init.limChar;
                if(!(modifiers & TypeScript.Modifiers.Static)) {
                    this.currentClassDefinition.varFlags |= TypeScript.VarFlags.ClassSuperMustBeFirstCallInConstructor;
                }
            } else {
                varDecl.limChar = this.scanner.pos;
            }
            if(modifiers & TypeScript.Modifiers.Static) {
                varDecl.varFlags |= TypeScript.VarFlags.Static;
                isStatic = true;
            }
            if((modifiers & TypeScript.Modifiers.Private) != TypeScript.Modifiers.None) {
                varDecl.varFlags |= TypeScript.VarFlags.Private;
            } else {
                varDecl.varFlags |= TypeScript.VarFlags.Public;
            }
            varDecl.varFlags |= TypeScript.VarFlags.Property;
            if(isDeclaredInConstructor) {
                varDecl.varFlags |= TypeScript.VarFlags.ClassConstructorProperty;
            }
            if(!isDeclaredInConstructor && !isStatic) {
                varDecl.varFlags |= TypeScript.VarFlags.ClassBodyProperty;
            }
            this.currentClassDefinition.knownMemberNames[text.actualText] = true;
            if(!isDeclaredInConstructor) {
                this.currentClassDefinition.members.members[this.currentClassDefinition.members.members.length] = varDecl;
            }
            varDecl.postComments = this.parseComments();
            return varDecl;
        };
        Parser.prototype.parseClassMemberFunctionDeclaration = function (methodName, minChar, errorRecoverySet, modifiers) {
            var wasAccessorID = this.prevIDTok != null;
            var isAccessor = TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Getter) || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Setter);
            var isStatic = TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Static);
            var isAmbient = this.ambientModule || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient);
            errorRecoverySet |= TypeScript.ErrorRecoverySet.RParen;
            if(isAccessor && (modifiers & TypeScript.Modifiers.Ambient)) {
                this.reportParseError("Property accessors may not be declared in ambient classes");
            }
            var ast = this.parseFncDecl(errorRecoverySet, true, isAmbient, true, methodName, false, isStatic, isAmbient, modifiers, null, true);
            if(ast.nodeType == TypeScript.NodeType.Error) {
                return ast;
            }
            var funcDecl = ast;
            funcDecl.minChar = minChar;
            if(funcDecl.bod !== null) {
                funcDecl.limChar = funcDecl.bod.limChar;
            }
            if(modifiers & TypeScript.Modifiers.Private) {
                funcDecl.fncFlags |= TypeScript.FncFlags.Private;
            } else {
                funcDecl.fncFlags |= TypeScript.FncFlags.Public;
            }
            if(isStatic) {
                funcDecl.fncFlags |= TypeScript.FncFlags.Static;
            }
            if(isAccessor) {
                if(TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Getter)) {
                    funcDecl.fncFlags |= TypeScript.FncFlags.GetAccessor;
                    funcDecl.hint = "get" + funcDecl.name.actualText;
                } else {
                    funcDecl.fncFlags |= TypeScript.FncFlags.SetAccessor;
                    funcDecl.hint = "set" + funcDecl.name.actualText;
                }
                funcDecl.fncFlags |= TypeScript.FncFlags.IsFunctionExpression;
                if(TypeScript.codeGenTarget < TypeScript.CodeGenTarget.ES5) {
                    this.reportParseError("Property accessors are only available when targeting ES5 or greater", funcDecl.minChar, funcDecl.limChar);
                }
            }
            funcDecl.fncFlags |= TypeScript.FncFlags.ClassMethod;
            this.currentClassDefinition.knownMemberNames[methodName.actualText] = true;
            this.currentClassDefinition.members.members[this.currentClassDefinition.members.members.length] = funcDecl;
            return funcDecl;
        };
        Parser.prototype.parseTypeMember = function (errorRecoverySet) {
            var minChar = this.scanner.startPos;
            var propertyDecl = this.parsePropertyDeclaration(errorRecoverySet, TypeScript.Modifiers.Public, true, false);
            if(propertyDecl) {
                propertyDecl.minChar = minChar;
                if(propertyDecl.nodeType == TypeScript.NodeType.VarDecl) {
                    this.checkCurrentToken(TypeScript.TokenID.Semicolon, errorRecoverySet);
                }
            }
            return propertyDecl;
        };
        Parser.prototype.parseTypeMemberList = function (errorRecoverySet, members) {
            errorRecoverySet |= TypeScript.ErrorRecoverySet.TypeScriptS;
            while(true) {
                switch(this.currentToken.tokenId) {
                    case TypeScript.TokenID.CloseBrace:
                    case TypeScript.TokenID.EndOfFile:
                        members.limChar = this.scanner.pos;
                        return;
                }
                var element = this.parseTypeMember(errorRecoverySet);
                if(element) {
                    members.append(element);
                }
            }
        };
        Parser.prototype.parseInterfaceDecl = function (errorRecoverySet, modifiers) {
            var leftCurlyCount = this.scanner.leftCurlyCount;
            var rightCurlyCount = this.scanner.rightCurlyCount;
            this.currentToken = this.scanner.scan();
            var minChar = this.scanner.pos;
            var name = null;
            if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || (!TypeScript.isPrimitiveTypeToken(this.currentToken) && TypeScript.convertTokToID(this.currentToken, this.strictMode))) {
                name = TypeScript.Identifier.fromToken(this.currentToken);
                name.minChar = this.scanner.startPos;
                name.limChar = this.scanner.pos;
                this.currentToken = this.scanner.scan();
            } else {
                this.reportParseError("interface missing name");
                if(this.errorRecovery) {
                    name = new TypeScript.MissingIdentifier();
                    name.minChar = this.scanner.pos;
                    name.limChar = this.scanner.pos;
                    name.flags |= TypeScript.ASTFlags.Error;
                }
            }
            var extendsList = null;
            var implementsList = null;
            if(this.currentToken.tokenId === TypeScript.TokenID.Extends || this.currentToken.tokenId === TypeScript.TokenID.Implements) {
                if(this.currentToken.tokenId === TypeScript.TokenID.Implements) {
                    this.reportParseError("Expected 'extends'");
                }
                extendsList = new TypeScript.ASTList();
                implementsList = new TypeScript.ASTList();
                extendsList.minChar = this.scanner.startPos;
                this.parseBaseList(extendsList, implementsList, errorRecoverySet, false);
            }
            var membersMinChar = this.scanner.startPos;
            this.checkCurrentToken(TypeScript.TokenID.OpenBrace, errorRecoverySet | TypeScript.ErrorRecoverySet.TypeScriptS);
            var members = new TypeScript.ASTList();
            members.minChar = membersMinChar;
            var prevInInterfaceDecl = this.inInterfaceDecl;
            this.inInterfaceDecl = true;
            this.parseTypeMemberList(errorRecoverySet | TypeScript.ErrorRecoverySet.RCurly, members);
            this.inInterfaceDecl = prevInInterfaceDecl;
            this.checkCurrentToken(TypeScript.TokenID.CloseBrace, errorRecoverySet);
            var interfaceDecl = new TypeScript.InterfaceDeclaration(name, members, extendsList, null);
            if(TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Private)) {
                interfaceDecl.varFlags |= TypeScript.VarFlags.Private;
            }
            if(TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Public)) {
                interfaceDecl.varFlags |= TypeScript.VarFlags.Public;
            }
            if(this.parsingDeclareFile || this.ambientModule || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Exported)) {
                interfaceDecl.varFlags |= TypeScript.VarFlags.Exported;
            }
            interfaceDecl.limChar = members.limChar;
            interfaceDecl.leftCurlyCount = this.scanner.leftCurlyCount - leftCurlyCount;
            interfaceDecl.rightCurlyCount = this.scanner.rightCurlyCount - rightCurlyCount;
            return interfaceDecl;
        };
        Parser.prototype.makeVarDecl = function (id, nest) {
            var varDecl = new TypeScript.VarDecl(id, nest);
            var currentVarList = this.topVarList();
            if(currentVarList) {
                currentVarList.append(varDecl);
            }
            return varDecl;
        };
        Parser.prototype.parsePropertyDeclaration = function (errorRecoverySet, modifiers, requireSignature, isStatic, unnamedAmbientFunctionOk) {
            var text = null;
            var minChar = this.scanner.startPos;
            var nameLimChar = minChar;
            var isNew = false;
            var isIndexer = false;
            var wasAccessorID = this.prevIDTok != null;
            var isAccessor = TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Getter) || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Setter);
            if(this.parsingDeclareFile || this.ambientModule || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient)) {
                requireSignature = true;
            }
            if(this.currentToken.tokenId == TypeScript.TokenID.OpenParen && !wasAccessorID) {
                if(!requireSignature && !isStatic) {
                    this.reportParseError("Expected identifier in property declaration");
                    if(this.errorRecovery) {
                        this.skip(errorRecoverySet);
                        text = new TypeScript.MissingIdentifier();
                    }
                }
            } else if(this.currentToken.tokenId == TypeScript.TokenID.New) {
                if(requireSignature) {
                    this.currentToken = this.scanner.scan();
                    if(this.currentToken.tokenId == TypeScript.TokenID.OpenParen) {
                        isNew = true;
                    }
                }
                if(!isNew) {
                    if(!requireSignature) {
                        this.currentToken = this.scanner.scan();
                    }
                    text = new TypeScript.Identifier("new");
                    text.minChar = this.scanner.pos - 3;
                    text.limChar = this.scanner.pos;
                    nameLimChar = this.scanner.pos;
                }
            } else if((this.currentToken.tokenId == TypeScript.TokenID.OpenBracket) && requireSignature) {
                isIndexer = true;
                text = new TypeScript.Identifier("__item");
            } else if((this.currentToken.tokenId != TypeScript.TokenID.Identifier) && (!TypeScript.convertTokToIDName(this.currentToken)) && !wasAccessorID) {
                this.reportParseError("Expected identifier in property declaration");
                if(this.errorRecovery) {
                    var eminChar = this.scanner.startPos;
                    var curpos = this.scanner.pos;
                    this.skip(errorRecoverySet & (~TypeScript.ErrorRecoverySet.Comma));
                    if(this.scanner.pos == curpos) {
                        this.currentToken = this.scanner.scan();
                    }
                    var epd = new TypeScript.VarDecl(new TypeScript.MissingIdentifier(), this.nestingLevel);
                    epd.flags |= TypeScript.ASTFlags.Error;
                    epd.minChar = eminChar;
                    epd.limChar = this.scanner.lastTokenLimChar();
                    return epd;
                }
            } else {
                if(wasAccessorID) {
                    text = TypeScript.Identifier.fromToken(this.prevIDTok);
                    text.minChar = this.scanner.lastTokenLimChar() - 3;
                    text.limChar = this.scanner.lastTokenLimChar();
                    nameLimChar = text.limChar;
                    if(TypeScript.codeGenTarget < TypeScript.CodeGenTarget.ES5) {
                        this.reportParseError("Property accessors are only available when targeting ES5 or greater");
                    }
                    if(this.currentToken.getText() == text.actualText && this.currentToken != this.prevIDTok) {
                        this.currentToken = this.scanner.scan();
                    }
                    this.prevIDTok = null;
                } else {
                    text = TypeScript.Identifier.fromToken(this.currentToken);
                    text.minChar = this.scanner.startPos;
                    text.limChar = this.scanner.pos;
                    nameLimChar = this.scanner.pos;
                    this.currentToken = this.scanner.scan();
                }
            }
            if(this.currentToken.tokenId == TypeScript.TokenID.Question) {
                if(this.inInterfaceDecl && text) {
                    text.flags |= TypeScript.ASTFlags.OptionalName;
                } else {
                    this.reportParseError("Optional properties may only be declared on interface or object types");
                }
                this.currentToken = this.scanner.scan();
            }
            if((this.currentToken.tokenId == TypeScript.TokenID.OpenParen) || (isIndexer && (this.currentToken.tokenId == TypeScript.TokenID.OpenBracket))) {
                var ers = errorRecoverySet | TypeScript.ErrorRecoverySet.RParen;
                if(isIndexer) {
                    ers = errorRecoverySet | TypeScript.ErrorRecoverySet.RBrack;
                }
                if(!text && unnamedAmbientFunctionOk && !isIndexer) {
                    text = new TypeScript.MissingIdentifier();
                }
                var ast = this.parseFncDecl(ers, true, requireSignature, this.currentClassDefinition || this.inInterfaceDecl, text, isIndexer, isStatic, (this.parsingDeclareFile || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient)), modifiers, null, true);
                var funcDecl;
                if(ast.nodeType == TypeScript.NodeType.Error) {
                    return ast;
                } else {
                    funcDecl = ast;
                }
                if(funcDecl.name) {
                    funcDecl.name.minChar = minChar;
                    funcDecl.name.limChar = nameLimChar;
                }
                if((modifiers & TypeScript.Modifiers.Public) != TypeScript.Modifiers.None) {
                    funcDecl.fncFlags |= TypeScript.FncFlags.Public;
                }
                if((modifiers & TypeScript.Modifiers.Private) != TypeScript.Modifiers.None) {
                    funcDecl.fncFlags |= TypeScript.FncFlags.Private;
                }
                if(isStatic) {
                    funcDecl.fncFlags |= TypeScript.FncFlags.Static;
                }
                if(this.parsingDeclareFile || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient)) {
                    funcDecl.fncFlags |= TypeScript.FncFlags.Ambient;
                }
                if(isAccessor) {
                    if(TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Getter)) {
                        funcDecl.fncFlags |= TypeScript.FncFlags.GetAccessor;
                        funcDecl.hint = "get" + funcDecl.name.actualText;
                    } else {
                        funcDecl.fncFlags |= TypeScript.FncFlags.SetAccessor;
                        funcDecl.hint = "set" + funcDecl.name.actualText;
                    }
                    funcDecl.fncFlags |= TypeScript.FncFlags.IsFunctionExpression;
                    if(modifiers & TypeScript.Modifiers.Ambient) {
                        this.reportParseError("Property accessors may not be declared in ambient types");
                    }
                }
                if(text == null || (text.isMissing() && unnamedAmbientFunctionOk && !isIndexer)) {
                    if(isNew) {
                        funcDecl.fncFlags |= TypeScript.FncFlags.ConstructMember;
                        funcDecl.hint = "_construct";
                        funcDecl.classDecl = this.currentClassDecl;
                    } else {
                        funcDecl.hint = "_call";
                        funcDecl.fncFlags |= TypeScript.FncFlags.CallMember;
                    }
                }
                return funcDecl;
            } else {
                var varDecl = new TypeScript.VarDecl(text, this.nestingLevel);
                varDecl.preComments = this.parseComments();
                varDecl.minChar = minChar;
                if(this.currentToken.tokenId == TypeScript.TokenID.Colon) {
                    this.currentToken = this.scanner.scan();
                    varDecl.typeExpr = this.parseTypeReference(errorRecoverySet | TypeScript.ErrorRecoverySet.Asg | TypeScript.ErrorRecoverySet.Comma, false);
                    if(varDecl.typeExpr && varDecl.typeExpr.nodeType == TypeScript.NodeType.TypeRef) {
                        var typeExpr = (varDecl.typeExpr);
                        if(typeExpr.term && typeExpr.term.nodeType == TypeScript.NodeType.FuncDecl) {
                            typeExpr.term.preComments = varDecl.preComments;
                        }
                    }
                }
                if(this.currentToken.tokenId == TypeScript.TokenID.Equals) {
                    if(requireSignature) {
                        this.reportParseError("context does not permit variable initializer");
                        if(this.errorRecovery) {
                            this.skip(errorRecoverySet);
                            varDecl.flags |= TypeScript.ASTFlags.Error;
                            varDecl.limChar = this.scanner.lastTokenLimChar();
                            return varDecl;
                        }
                    }
                    this.currentToken = this.scanner.scan();
                    varDecl.init = this.parseExpr(TypeScript.ErrorRecoverySet.Comma | errorRecoverySet, TypeScript.OperatorPrecedence.Comma, true, TypeContext.NoTypes);
                    varDecl.limChar = varDecl.init.limChar;
                    if(varDecl.init.nodeType == TypeScript.NodeType.FuncDecl) {
                        var funcDecl = varDecl.init;
                        funcDecl.hint = varDecl.id.text;
                        funcDecl.boundToProperty = varDecl;
                    } else if(isAccessor) {
                        this.reportParseError("Accessors may only be functions");
                    }
                } else {
                    varDecl.limChar = this.scanner.pos;
                }
                if((modifiers & TypeScript.Modifiers.Readonly) != TypeScript.Modifiers.None) {
                    varDecl.varFlags |= TypeScript.VarFlags.Readonly;
                }
                if(isStatic) {
                    varDecl.varFlags |= TypeScript.VarFlags.Static;
                }
                if((modifiers & TypeScript.Modifiers.Public) != TypeScript.Modifiers.None) {
                    varDecl.varFlags |= TypeScript.VarFlags.Public;
                }
                if((modifiers & TypeScript.Modifiers.Private) != TypeScript.Modifiers.None) {
                    varDecl.varFlags |= TypeScript.VarFlags.Private;
                }
                varDecl.varFlags |= TypeScript.VarFlags.Property;
                return varDecl;
            }
        };
        Parser.prototype.parseVariableDeclaration = function (errorRecoverySet, modifiers, allowIn, isStatic) {
            var isConst = TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Readonly);
            var minChar = this.scanner.startPos;
            var varDecl = null;
            var declList = null;
            var multivar = false;
            this.currentToken = this.scanner.scan();
            var varDeclPreComments = this.parseComments();
            while(true) {
                if((this.currentToken.tokenId != TypeScript.TokenID.Identifier) && (!TypeScript.convertTokToID(this.currentToken, this.strictMode))) {
                    this.reportParseError("Expected identifier in variable declaration");
                    if(this.errorRecovery) {
                        varDecl = new TypeScript.VarDecl(new TypeScript.MissingIdentifier(), this.nestingLevel);
                        varDecl.minChar = minChar;
                        this.skip(errorRecoverySet);
                        varDecl.flags |= TypeScript.ASTFlags.Error;
                        varDecl.limChar = this.scanner.lastTokenLimChar();
                        return varDecl;
                    }
                }
                var varDeclName = TypeScript.Identifier.fromToken(this.currentToken);
                if(this.strictMode && (varDeclName.text == "eval")) {
                    this.reportParseError("'eval' may not name a variable in strict mode");
                }
                varDecl = this.makeVarDecl(varDeclName, this.nestingLevel);
                varDecl.id.minChar = this.scanner.startPos;
                varDecl.id.limChar = this.scanner.pos;
                varDecl.preComments = varDeclPreComments;
                if(isStatic) {
                    varDecl.varFlags |= TypeScript.VarFlags.Static;
                }
                if(TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Readonly)) {
                    varDecl.varFlags |= TypeScript.VarFlags.Readonly;
                }
                if(this.parsingDeclareFile || this.ambientModule || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient)) {
                    varDecl.varFlags |= TypeScript.VarFlags.Ambient;
                }
                if(this.parsingDeclareFile || this.ambientModule || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Exported)) {
                    varDecl.varFlags |= TypeScript.VarFlags.Exported;
                }
                varDecl.minChar = minChar;
                if(declList) {
                    declList.append(varDecl);
                }
                this.currentToken = this.scanner.scan();
                if(this.currentToken.tokenId == TypeScript.TokenID.Colon) {
                    this.currentToken = this.scanner.scan();
                    var prevInFncDecl = this.inFncDecl;
                    this.inFncDecl = false;
                    varDecl.typeExpr = this.parseTypeReference(errorRecoverySet | TypeScript.ErrorRecoverySet.Asg | TypeScript.ErrorRecoverySet.Comma, false);
                    this.inFncDecl = prevInFncDecl;
                }
                if(this.currentToken.tokenId == TypeScript.TokenID.Equals) {
                    if(TypeScript.hasFlag(varDecl.varFlags, TypeScript.VarFlags.Ambient)) {
                        this.reportParseError("Ambient variable can not have an initializer");
                    }
                    this.currentToken = this.scanner.scan();
                    varDecl.init = this.parseExpr(TypeScript.ErrorRecoverySet.Comma | errorRecoverySet, TypeScript.OperatorPrecedence.Comma, allowIn, TypeContext.NoTypes);
                    varDecl.limChar = varDecl.init.limChar;
                    if(varDecl.init.nodeType == TypeScript.NodeType.FuncDecl) {
                        var funcDecl = varDecl.init;
                        funcDecl.hint = varDecl.id.actualText;
                    }
                } else {
                    if(isConst) {
                        this.reportParseError("const declaration requires initializer");
                    }
                    varDecl.limChar = this.scanner.pos;
                }
                varDecl.postComments = this.parseCommentsForLine(this.scanner.line);
                if(this.currentToken.tokenId != TypeScript.TokenID.Comma) {
                    if(declList) {
                        declList.limChar = varDecl.limChar;
                        return declList;
                    } else {
                        return varDecl;
                    }
                }
                if(!multivar) {
                    declList = new TypeScript.ASTList();
                    declList.minChar = varDecl.minChar;
                    declList.append(varDecl);
                    multivar = true;
                }
                this.currentToken = this.scanner.scan();
                minChar = this.scanner.startPos;
            }
        };
        Parser.prototype.parseMemberList = function (errorRecoverySet) {
            var elements = new TypeScript.ASTList();
            if(this.currentToken.tokenId == TypeScript.TokenID.CloseBrace) {
                return elements;
            }
            var idHint = null;
            var memberName = null;
            var memberExpr = null;
            var member = null;
            var minChar = this.scanner.startPos;
            var isSet = false;
            var skippedTokenForGetSetId = false;
            var getSetTok = null;
            var getSetStartPos = 0;
            var getSetPos = 0;
            for(; ; ) {
                var accessorPattern = false;
                if(this.currentToken.tokenId == TypeScript.TokenID.Get || this.currentToken.tokenId == TypeScript.TokenID.Set) {
                    isSet = this.currentToken.tokenId == TypeScript.TokenID.Set;
                    getSetTok = this.currentToken;
                    getSetStartPos = this.scanner.startPos;
                    getSetPos = this.scanner.pos;
                    this.currentToken = this.scanner.scan();
                    if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToIDName(this.currentToken)) {
                        idHint = isSet ? "set" : "get";
                        idHint = idHint + this.currentToken.getText();
                        memberName = TypeScript.Identifier.fromToken(this.currentToken);
                        memberName.minChar = this.scanner.startPos;
                        accessorPattern = true;
                        if(TypeScript.codeGenTarget < TypeScript.CodeGenTarget.ES5) {
                            this.reportParseError("Property accessors are only available when targeting ES5 or greater");
                        }
                    } else if(this.currentToken.tokenId != TypeScript.TokenID.Colon) {
                        this.reportParseError("Expected identifier, string or number as accessor name");
                    } else {
                        skippedTokenForGetSetId = true;
                        memberName = TypeScript.Identifier.fromToken(getSetTok);
                        memberName.minChar = getSetStartPos;
                        memberName.limChar = getSetPos;
                    }
                } else if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToIDName(this.currentToken)) {
                    idHint = this.currentToken.getText();
                    memberName = TypeScript.Identifier.fromToken(this.currentToken);
                    memberName.minChar = this.scanner.startPos;
                    memberName.limChar = this.scanner.pos;
                } else if(this.currentToken.tokenId == TypeScript.TokenID.StringLiteral) {
                    idHint = this.currentToken.getText();
                    memberName = new TypeScript.StringLiteral(idHint);
                    memberName.minChar = this.scanner.startPos;
                    memberName.limChar = this.scanner.pos;
                } else if(this.currentToken.tokenId == TypeScript.TokenID.NumberLiteral) {
                    var ntok = this.currentToken;
                    idHint = ntok.value.toString();
                    memberName = new TypeScript.StringLiteral(idHint);
                    memberName.minChar = this.scanner.startPos;
                    memberName.limChar = this.scanner.pos;
                } else {
                    this.reportParseError("Expected identifier, string or number as member name");
                    if(this.errorRecovery) {
                        memberName = new TypeScript.MissingIdentifier();
                        memberName.minChar = this.scanner.startPos;
                        memberName.flags |= TypeScript.ASTFlags.Error;
                        this.skip(errorRecoverySet | TypeScript.ErrorRecoverySet.Comma);
                        memberName.limChar = this.scanner.lastTokenLimChar();
                    }
                }
                if(!skippedTokenForGetSetId) {
                    this.currentToken = this.scanner.scan();
                } else {
                    skippedTokenForGetSetId = false;
                }
                if(this.currentToken.tokenId == TypeScript.TokenID.Question) {
                    memberName.flags |= TypeScript.ASTFlags.OptionalName;
                    this.currentToken = this.scanner.scan();
                }
                if(accessorPattern) {
                    var args = new TypeScript.ASTList();
                    this.parseFormalParameterList(errorRecoverySet | TypeScript.ErrorRecoverySet.RParen, args, false, true, false, !isSet, isSet, false, null, true);
                    var funcDecl = this.parseFunctionStatements(errorRecoverySet | TypeScript.ErrorRecoverySet.RCurly, memberName, false, true, args, TypeScript.AllowedElements.None, this.scanner.startPos, false, TypeScript.Modifiers.None);
                    if(isSet && funcDecl.returnTypeAnnotation) {
                        this.reportParseError("Property setters may not declare a return type");
                    }
                    funcDecl.fncFlags |= isSet ? TypeScript.FncFlags.SetAccessor : TypeScript.FncFlags.GetAccessor;
                    funcDecl.fncFlags |= TypeScript.FncFlags.IsFunctionExpression;
                    funcDecl.hint = idHint;
                    memberExpr = funcDecl;
                    member = new TypeScript.BinaryExpression(TypeScript.NodeType.Member, memberName, memberExpr);
                    member.minChar = memberName.minChar;
                    if(memberExpr.nodeType == TypeScript.NodeType.FuncDecl) {
                        var funcDecl = memberExpr;
                        funcDecl.hint = idHint;
                    }
                } else if(this.currentToken.tokenId == TypeScript.TokenID.Colon) {
                    this.currentToken = this.scanner.scan();
                    memberExpr = this.parseExpr(TypeScript.ErrorRecoverySet.Comma | errorRecoverySet, TypeScript.OperatorPrecedence.Comma, true, TypeContext.NoTypes);
                    if(memberExpr.nodeType == TypeScript.NodeType.TypeRef) {
                        this.reportParseError("Expected 'new' on array declaration in member definition");
                    }
                    member = new TypeScript.BinaryExpression(TypeScript.NodeType.Member, memberName, memberExpr);
                    member.minChar = memberName.minChar;
                    if(memberExpr.nodeType == TypeScript.NodeType.FuncDecl) {
                        var funcDecl = memberExpr;
                        funcDecl.hint = idHint;
                    }
                } else {
                    this.reportParseError("Expected ':' in member definition");
                    if(this.errorRecovery) {
                        this.skip(errorRecoverySet);
                        elements.flags |= TypeScript.ASTFlags.Error;
                        elements.minChar = minChar;
                        elements.limChar = this.scanner.lastTokenLimChar();
                        return elements;
                    }
                }
                idHint = null;
                elements.append(member);
                member.limChar = this.scanner.lastTokenLimChar();
                if(this.currentToken.tokenId != TypeScript.TokenID.Comma) {
                    break;
                } else {
                    this.currentToken = this.scanner.scan();
                }
                if(this.currentToken.tokenId == TypeScript.TokenID.CloseBrace) {
                    break;
                }
            }
            if(member) {
                elements.limChar = member.limChar;
            }
            elements.minChar = minChar;
            return elements;
        };
        Parser.prototype.parseArrayList = function (errorRecoverySet) {
            var elements = null;
            if(this.currentToken.tokenId == TypeScript.TokenID.CloseBracket) {
                return elements;
            } else {
                elements = new TypeScript.ASTList();
                elements.minChar = this.scanner.startPos;
            }
            var arg;
            for(; ; ) {
                if((this.currentToken.tokenId == TypeScript.TokenID.Comma) || (this.currentToken.tokenId == TypeScript.TokenID.CloseBracket)) {
                    arg = new TypeScript.AST(TypeScript.NodeType.EmptyExpr);
                } else {
                    arg = this.parseExpr(TypeScript.ErrorRecoverySet.Comma | errorRecoverySet, TypeScript.OperatorPrecedence.Comma, true, TypeContext.NoTypes);
                }
                elements.append(arg);
                if(this.currentToken.tokenId != TypeScript.TokenID.Comma) {
                    break;
                }
                this.currentToken = this.scanner.scan();
            }
            elements.limChar = this.scanner.lastTokenLimChar();
            return elements;
        };
        Parser.prototype.parseArrayLiteral = function (errorRecoverySet) {
            var arrayLiteral = null;
            arrayLiteral = new TypeScript.UnaryExpression(TypeScript.NodeType.ArrayLit, this.parseArrayList(errorRecoverySet));
            return arrayLiteral;
        };
        Parser.prototype.parseTerm = function (errorRecoverySet, allowCall, typeContext, inCast) {
            var ast = null;
            var sawId = false;
            var inNew = false;
            var minChar = this.scanner.startPos;
            var limChar = this.scanner.pos;
            var parseAsLambda = false;
            var expectlambdaRParen = false;
            switch(this.currentToken.tokenId) {
                case TypeScript.TokenID.Number:
                case TypeScript.TokenID.Bool:
                case TypeScript.TokenID.Any:
                case TypeScript.TokenID.String:
                    var tid = new TypeScript.Identifier(TypeScript.tokenTable[this.currentToken.tokenId].text);
                    if(TypeScript.hasFlag(typeContext, TypeContext.Primitive)) {
                        ast = new TypeScript.TypeReference(tid, 0);
                        sawId = true;
                    } else {
                        ast = tid;
                        sawId = true;
                    }
                    ast.minChar = minChar;
                    this.currentToken = this.scanner.scan();
                    limChar = this.scanner.lastTokenLimChar();
                    break;
                case TypeScript.TokenID.This:
                    ast = new TypeScript.AST(TypeScript.NodeType.This);
                    ast.minChar = minChar;
                    this.currentToken = this.scanner.scan();
                    limChar = this.scanner.lastTokenLimChar();
                    break;
                case TypeScript.TokenID.Super:
                    ast = new TypeScript.AST(TypeScript.NodeType.Super);
                    ast.minChar = minChar;
                    this.currentToken = this.scanner.scan();
                    limChar = this.scanner.lastTokenLimChar();
                    break;
                case TypeScript.TokenID.True:
                    ast = new TypeScript.AST(TypeScript.NodeType.True);
                    this.currentToken = this.scanner.scan();
                    ast.minChar = minChar;
                    break;
                case TypeScript.TokenID.False:
                    ast = new TypeScript.AST(TypeScript.NodeType.False);
                    this.currentToken = this.scanner.scan();
                    ast.minChar = minChar;
                    break;
                case TypeScript.TokenID.Null:
                    ast = new TypeScript.AST(TypeScript.NodeType.Null);
                    this.currentToken = this.scanner.scan();
                    ast.minChar = minChar;
                    break;
                case TypeScript.TokenID.New:
                    this.currentToken = this.scanner.scan();
                    var target = this.parseTerm(errorRecoverySet, false, TypeContext.AllSimpleTypes, inCast);
                    if(target.nodeType == TypeScript.NodeType.Error || (target.nodeType == TypeScript.NodeType.Index && (target).operand1.nodeType == TypeScript.NodeType.TypeRef)) {
                        this.reportParseError("Cannot invoke 'new' on this expression");
                    } else {
                        ast = new TypeScript.CallExpression(TypeScript.NodeType.New, target, null);
                        ast.minChar = minChar;
                        limChar = this.currentToken.tokenId == TypeScript.TokenID.EndOfFile ? this.scanner.pos : this.scanner.lastTokenLimChar();
                        inNew = true;
                    }
                    break;
                case TypeScript.TokenID.Function:
                    minChar = this.scanner.pos;
                    ast = this.parseFncDecl(errorRecoverySet, false, false, false, null, false, false, false, TypeScript.Modifiers.None, null, true);
                    (ast).fncFlags |= TypeScript.FncFlags.IsFunctionExpression;
                    ast.minChar = minChar;
                    limChar = this.currentToken.tokenId == TypeScript.TokenID.EndOfFile ? this.scanner.pos : this.scanner.lastTokenLimChar();
                    ast.limChar = limChar;
                    break;
            }
            if(ast == null) {
                if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                    var idText = this.currentToken.getText();
                    ast = this.createRef(idText, (this.currentToken).hasEscapeSequence, minChar);
                    sawId = true;
                    ast.minChar = minChar;
                    this.currentToken = this.scanner.scan();
                    if(this.currentToken.tokenId == TypeScript.TokenID.Question) {
                        ast.flags |= TypeScript.ASTFlags.PossibleOptionalParameter;
                    }
                    limChar = this.scanner.lastTokenLimChar();
                }
            }
            if(inCast) {
                this.checkCurrentToken(TypeScript.TokenID.GreaterThan, errorRecoverySet);
            }
            if(ast == null) {
                switch(this.currentToken.tokenId) {
                    case TypeScript.TokenID.OpenParen:
                        minChar = this.scanner.pos;
                        var prevTokId = this.scanner.previousToken().tokenId;
                        this.currentToken = this.scanner.scan();
                        var couldBeLambda = prevTokId == TypeScript.TokenID.OpenParen || prevTokId == TypeScript.TokenID.Comma || prevTokId == TypeScript.TokenID.EqualsEquals || prevTokId == TypeScript.TokenID.Colon;
                        if(couldBeLambda && this.currentToken.tokenId == TypeScript.TokenID.CloseParen) {
                            parseAsLambda = true;
                            expectlambdaRParen = false;
                            this.currentToken = this.scanner.scan();
                        } else if(couldBeLambda && this.currentToken.tokenId == TypeScript.TokenID.DotDotDot) {
                            parseAsLambda = true;
                            expectlambdaRParen = true;
                        } else {
                            ast = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.RParen, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes, couldBeLambda);
                            limChar = this.scanner.lastTokenLimChar();
                            parseAsLambda = couldBeLambda && (ast.nodeType == TypeScript.NodeType.Name || ast.nodeType == TypeScript.NodeType.Comma) && (this.currentToken.tokenId == TypeScript.TokenID.Colon || this.currentToken.tokenId == TypeScript.TokenID.Question);
                            expectlambdaRParen = true;
                        }
                        if((ast && !parseAsLambda)) {
                            if(TypeScript.hasFlag(ast.flags, TypeScript.ASTFlags.SkipNextRParen)) {
                                ast.flags = ast.flags & (~(TypeScript.ASTFlags.SkipNextRParen));
                                break;
                            }
                            this.checkCurrentToken(TypeScript.TokenID.CloseParen, errorRecoverySet);
                            ast.isParenthesized = true;
                        }
                        break;
                    case TypeScript.TokenID.NumberLiteral: {
                        var numTok = this.currentToken;
                        this.currentToken = this.scanner.scan();
                        ast = new TypeScript.NumberLiteral(numTok.value, numTok.text);
                        ast.minChar = minChar;
                        limChar = this.scanner.lastTokenLimChar();
                        break;
                    }
                    case TypeScript.TokenID.StringLiteral:
                        ast = new TypeScript.StringLiteral(this.currentToken.getText());
                        this.currentToken = this.scanner.scan();
                        ast.minChar = minChar;
                        limChar = this.scanner.lastTokenLimChar();
                        break;
                    case TypeScript.TokenID.RegularExpressionLiteral: {
                        var rtok = this.currentToken;
                        ast = new TypeScript.RegexLiteral(rtok.text);
                        this.currentToken = this.scanner.scan();
                        ast.minChar = minChar;
                        limChar = this.scanner.lastTokenLimChar();
                        break;
                    }
                    case TypeScript.TokenID.OpenBracket:
                        minChar = this.scanner.startPos;
                        this.currentToken = this.scanner.scan();
                        ast = this.parseArrayLiteral(TypeScript.ErrorRecoverySet.RBrack | errorRecoverySet);
                        ast.minChar = minChar;
                        limChar = this.scanner.pos;
                        this.checkCurrentToken(TypeScript.TokenID.CloseBracket, errorRecoverySet);
                        break;
                    case TypeScript.TokenID.OpenBrace:
                        minChar = this.scanner.startPos;
                        this.currentToken = this.scanner.scan();
                        var members = this.parseMemberList(TypeScript.ErrorRecoverySet.RCurly | errorRecoverySet);
                        this.checkCurrentToken(TypeScript.TokenID.CloseBrace, errorRecoverySet);
                        ast = new TypeScript.UnaryExpression(TypeScript.NodeType.ObjectLit, members);
                        ast.minChar = minChar;
                        limChar = this.scanner.lastTokenLimChar();
                        members.minChar = minChar;
                        members.limChar = limChar;
                        break;
                    case TypeScript.TokenID.LessThan:
                        minChar = this.scanner.startPos;
                        this.currentToken = this.scanner.scan();
                        var term = this.parseTypeReference(TypeScript.ErrorRecoverySet.BinOp, false);
                        this.checkCurrentToken(TypeScript.TokenID.GreaterThan, errorRecoverySet);
                        ast = new TypeScript.UnaryExpression(TypeScript.NodeType.TypeAssertion, this.parseExpr(errorRecoverySet, TypeScript.OperatorPrecedence.Unary, false, TypeContext.NoTypes));
                        (ast).castTerm = term;
                        break;
                    default:
                        if(this.prevExpr && TypeScript.hasFlag(this.prevExpr.flags, TypeScript.ASTFlags.PossibleOptionalParameter)) {
                            parseAsLambda = true;
                            ast = this.prevExpr;
                        } else {
                            this.reportParseError("Check format of expression term");
                            if(this.errorRecovery) {
                                var ident = new TypeScript.MissingIdentifier();
                                ident.minChar = minChar;
                                ident.flags |= TypeScript.ASTFlags.Error;
                                this.skip(errorRecoverySet | TypeScript.ErrorRecoverySet.Postfix);
                                if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                                    ident.setText(this.currentToken.getText(), (this.currentToken).hasEscapeSequence);
                                    this.currentToken = this.scanner.scan();
                                    limChar = this.scanner.lastTokenLimChar();
                                } else {
                                    limChar = this.scanner.lastTokenLimChar();
                                }
                                ast = ident;
                            }
                        }
                }
            }
            if(parseAsLambda) {
                if(this.currentToken.tokenId == TypeScript.TokenID.Colon || this.currentToken.tokenId == TypeScript.TokenID.Comma || this.currentToken.tokenId == TypeScript.TokenID.CloseParen || this.currentToken.tokenId == TypeScript.TokenID.DotDotDot) {
                    ast = this.parseLambdaExpr(errorRecoverySet, ast, true, expectlambdaRParen);
                    ast.minChar = minChar;
                    limChar = this.scanner.lastTokenLimChar();
                    ast.limChar = limChar;
                } else if(ast) {
                    ast.isParenthesized = true;
                }
            }
            if(sawId && (typeContext != TypeContext.NoTypes)) {
                typeContext |= TypeContext.ArraySuffix;
            }
            var postFix = this.parsePostfixOperators(errorRecoverySet, ast, allowCall, inNew, typeContext, minChar, limChar);
            if(postFix) {
                if(sawId && (postFix.nodeType == TypeScript.NodeType.Index)) {
                    var binExpr = postFix;
                    if(binExpr.operand2 == null) {
                        postFix = this.convertToTypeReference(postFix);
                    }
                }
                postFix.minChar = minChar;
                postFix.limChar = TypeScript.max(postFix.limChar, this.scanner.lastTokenLimChar());
                return postFix;
            } else {
                return new TypeScript.AST(TypeScript.NodeType.Error);
            }
        };
        Parser.prototype.parseLambdaExpr = function (errorRecoverySet, lambdaArgs, skipNextRParen, expectClosingRParen) {
            var ast = this.parseFncDecl(errorRecoverySet, false, false, false, null, false, false, false, TypeScript.Modifiers.None, {
                preProcessedLambdaArgs: lambdaArgs
            }, expectClosingRParen);
            (ast).fncFlags |= TypeScript.FncFlags.IsFunctionExpression;
            (ast).fncFlags |= TypeScript.FncFlags.IsFatArrowFunction;
            if(!skipNextRParen) {
                ast.flags |= TypeScript.ASTFlags.SkipNextRParen;
            }
            ast.limChar = this.scanner.lastTokenLimChar();
            ;
            return ast;
        };
        Parser.prototype.parseExpr = function (errorRecoverySet, minPrecedence, allowIn, typeContext, possiblyInLambda) {
            if (typeof possiblyInLambda === "undefined") { possiblyInLambda = false; }
            var ast = null;
            var tokenInfo = TypeScript.lookupToken(this.currentToken.tokenId);
            var canAssign = true;
            var idHint = null;
            var minChar = this.scanner.startPos;
            var preComments = this.parseComments();
            var exprIsAnonLambda = false;
            if((tokenInfo != undefined) && (tokenInfo.unopNodeType != TypeScript.NodeType.None)) {
                canAssign = false;
                this.currentToken = this.scanner.scan();
                var tempExpr = this.parseExpr(TypeScript.ErrorRecoverySet.BinOp | errorRecoverySet, tokenInfo.unopPrecedence, allowIn, TypeContext.NoTypes);
                ast = new TypeScript.UnaryExpression(tokenInfo.unopNodeType, tempExpr);
                ast.limChar = tempExpr.limChar;
                ast.minChar = minChar;
            } else {
                ast = this.parseTerm(TypeScript.ErrorRecoverySet.BinOp | TypeScript.ErrorRecoverySet.AddOp | errorRecoverySet, true, typeContext, false);
                var id;
                var temp;
                if(ast.nodeType == TypeScript.NodeType.Name) {
                    id = ast;
                    idHint = id.actualText;
                } else if(ast.nodeType == TypeScript.NodeType.Dot) {
                    var subsumedExpr = false;
                    if(this.inferPropertiesFromThisAssignment && (this.currentToken.tokenId == TypeScript.TokenID.Colon || this.currentToken.tokenId == TypeScript.TokenID.Equals) && this.parsingClassConstructorDefinition && this.nestingLevel == this.currentClassDefinition.constructorNestingLevel && (ast).operand1.nodeType == TypeScript.NodeType.This) {
                        if((ast).operand2.nodeType == TypeScript.NodeType.Name) {
                            var op2ID = ((ast).operand2);
                            if(!this.currentClassDefinition.knownMemberNames[op2ID.actualText]) {
                                ast = this.parseClassMemberVariableDeclaration(op2ID, ast.minChar, true, errorRecoverySet, TypeScript.Modifiers.Public);
                                subsumedExpr = true;
                            }
                        }
                    }
                    if(!subsumedExpr) {
                        temp = ast;
                        while(temp.nodeType == TypeScript.NodeType.Dot) {
                            var binExpr = temp;
                            temp = binExpr.operand2;
                        }
                        if(temp.nodeType == TypeScript.NodeType.Name) {
                            id = temp;
                            idHint = id.actualText;
                        }
                    }
                }
                if((!this.scanner.lastTokenHadNewline()) && ((this.currentToken.tokenId == TypeScript.TokenID.PlusPlus) || (this.currentToken.tokenId == TypeScript.TokenID.MinusMinus))) {
                    canAssign = false;
                    var operand = ast;
                    ast = new TypeScript.UnaryExpression((this.currentToken.tokenId == TypeScript.TokenID.PlusPlus) ? TypeScript.NodeType.IncPost : TypeScript.NodeType.DecPost, operand);
                    ast.limChar = this.scanner.pos;
                    ast.minChar = operand.minChar;
                    this.currentToken = this.scanner.scan();
                }
            }
            for(; ; ) {
                tokenInfo = TypeScript.lookupToken(this.currentToken.tokenId);
                if((tokenInfo == undefined) || (tokenInfo.binopNodeType == TypeScript.NodeType.None)) {
                    break;
                }
                if((!allowIn) && (tokenInfo.binopNodeType == TypeScript.NodeType.In)) {
                    break;
                }
                if(tokenInfo.binopPrecedence == TypeScript.OperatorPrecedence.Assignment) {
                    if(tokenInfo.binopPrecedence < minPrecedence) {
                        break;
                    }
                    if(!canAssign) {
                        this.reportParseError("illegal assignment");
                    }
                } else if(tokenInfo.binopPrecedence <= minPrecedence) {
                    break;
                }
                if(possiblyInLambda && this.currentToken.tokenId == TypeScript.TokenID.Comma && this.scanner.getLookAheadToken().tokenId == TypeScript.TokenID.DotDotDot) {
                    exprIsAnonLambda = true;
                    canAssign = false;
                    ast = this.parseLambdaExpr(errorRecoverySet, ast, false, true);
                    break;
                }
                this.currentToken = this.scanner.scan();
                canAssign = false;
                if(tokenInfo.binopNodeType == TypeScript.NodeType.ConditionalExpression) {
                    if(possiblyInLambda && (this.currentToken.tokenId == TypeScript.TokenID.Equals || this.currentToken.tokenId == TypeScript.TokenID.Colon || this.currentToken.tokenId == TypeScript.TokenID.CloseParen || this.currentToken.tokenId == TypeScript.TokenID.Comma)) {
                        exprIsAnonLambda = true;
                        canAssign = true;
                    } else {
                        this.prevExpr = ast;
                        var whenTrue = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.Colon, TypeScript.OperatorPrecedence.Assignment, allowIn, TypeContext.NoTypes);
                        this.prevExpr = null;
                        this.checkCurrentToken(TypeScript.TokenID.Colon, errorRecoverySet | TypeScript.ErrorRecoverySet.ExprStart);
                        var whenFalse = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.BinOp, TypeScript.OperatorPrecedence.Assignment, allowIn, TypeContext.NoTypes);
                        ast = new TypeScript.ConditionalExpression(ast, whenTrue, whenFalse);
                    }
                } else {
                    var tc = TypeContext.NoTypes;
                    var binExpr2;
                    binExpr2 = new TypeScript.BinaryExpression(tokenInfo.binopNodeType, ast, this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.BinOp, tokenInfo.binopPrecedence, allowIn, TypeContext.NoTypes, possiblyInLambda));
                    if(binExpr2.operand2.nodeType == TypeScript.NodeType.FuncDecl) {
                        var funcDecl = binExpr2.operand2;
                        funcDecl.hint = idHint;
                    }
                    binExpr2.minChar = ast.minChar;
                    binExpr2.limChar = this.scanner.lastTokenLimChar();
                    idHint = null;
                    ast = binExpr2;
                }
            }
            if(canAssign) {
                ast.flags |= TypeScript.ASTFlags.Writeable;
            }
            if(!exprIsAnonLambda) {
                ast.minChar = minChar;
                ast.limChar = TypeScript.max(ast.limChar, this.scanner.lastTokenLimChar());
                if(preComments) {
                    ast.preComments = ast.preComments ? preComments.concat(ast.preComments) : preComments;
                }
                ast.postComments = this.parseCommentsForLine(this.scanner.line);
            }
            return ast;
        };
        Parser.prototype.parsePostfixOperators = function (errorRecoverySet, ast, allowCall, inNew, typeContext, lhsMinChar, lhsLimChar) {
            var count = 0;
            if(!ast) {
                ast = new TypeScript.AST(TypeScript.NodeType.EmptyExpr);
                ast.isParenthesized = true;
            }
            ast.minChar = lhsMinChar;
            ast.limChar = lhsLimChar;
            for(; ; ) {
                switch(this.currentToken.tokenId) {
                    case TypeScript.TokenID.OpenParen:
                        if(inNew) {
                            var callExpr = ast;
                            callExpr.arguments = this.parseArgList(errorRecoverySet);
                            inNew = false;
                        } else {
                            if(!allowCall) {
                                return ast;
                            }
                            ast = new TypeScript.CallExpression(TypeScript.NodeType.Call, ast, this.parseArgList(errorRecoverySet));
                            ast.minChar = lhsMinChar;
                        }
                        ast.limChar = this.scanner.pos;
                        this.checkCurrentToken(TypeScript.TokenID.CloseParen, errorRecoverySet);
                        break;
                    case TypeScript.TokenID.OpenBracket:
                        this.currentToken = this.scanner.scan();
                        if(this.currentToken.tokenId == TypeScript.TokenID.CloseBracket) {
                            if(TypeScript.hasFlag(typeContext, TypeContext.ArraySuffix)) {
                                this.currentToken = this.scanner.scan();
                                if(ast.nodeType == TypeScript.NodeType.TypeRef) {
                                    var typeRef = ast;
                                    typeRef.arrayCount++;
                                } else {
                                    ast = new TypeScript.BinaryExpression(TypeScript.NodeType.Index, ast, null);
                                }
                                ast.limChar = this.scanner.pos;
                                break;
                            }
                        }
                        ast = new TypeScript.BinaryExpression(TypeScript.NodeType.Index, ast, this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.RBrack, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes));
                        ast.minChar = lhsMinChar;
                        ast.limChar = this.scanner.pos;
                        this.checkCurrentToken(TypeScript.TokenID.CloseBracket, errorRecoverySet);
                        break;
                    case TypeScript.TokenID.Dot: {
                        var name = null;
                        var curpos = this.scanner.pos;
                        this.currentToken = this.scanner.scan();
                        if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || ((!this.errorRecovery || !this.scanner.lastTokenHadNewline()) && TypeScript.convertTokToIDName(this.currentToken))) {
                            ast.flags |= TypeScript.ASTFlags.DotLHS;
                            name = this.createRef(this.currentToken.getText(), (this.currentToken).hasEscapeSequence, this.scanner.startPos);
                            name.limChar = this.scanner.pos;
                            this.currentToken = this.scanner.scan();
                        } else {
                            this.reportParseError("Expected identifier following dot");
                            if(this.errorRecovery) {
                                this.skip(errorRecoverySet);
                                ast.flags |= (TypeScript.ASTFlags.Error | TypeScript.ASTFlags.DotLHS);
                                return ast;
                            } else {
                                name = new TypeScript.MissingIdentifier();
                            }
                        }
                        ast = new TypeScript.BinaryExpression(TypeScript.NodeType.Dot, ast, name);
                        ast.minChar = lhsMinChar;
                        ast.limChar = this.scanner.lastTokenLimChar();
                        break;
                    }
                    case TypeScript.TokenID.EqualsGreaterThan:
                        ast = this.parseFncDecl(errorRecoverySet, false, false, false, null, false, false, false, TypeScript.Modifiers.None, {
                            preProcessedLambdaArgs: ast
                        }, false);
                        (ast).fncFlags |= TypeScript.FncFlags.IsFunctionExpression;
                        ast.minChar = lhsMinChar;
                        ast.limChar = this.scanner.lastTokenLimChar();
                        break;
                    default:
                        return ast;
                }
            }
        };
        Parser.prototype.parseTry = function (tryNode, errorRecoverySet, parentModifiers) {
            var minChar = this.scanner.startPos;
            var preComments = this.parseComments();
            this.currentToken = this.scanner.scan();
            if(this.currentToken.tokenId != TypeScript.TokenID.OpenBrace) {
                this.reportParseError("Expected '{'");
                if(this.errorRecovery) {
                    var etryNode = tryNode;
                    etryNode.minChar = minChar;
                    etryNode.limChar = this.scanner.lastTokenLimChar();
                    etryNode.flags |= TypeScript.ASTFlags.Error;
                    return etryNode;
                }
            }
            tryNode.body = this.parseStatement(errorRecoverySet, TypeScript.AllowedElements.None, parentModifiers);
            tryNode.minChar = minChar;
            tryNode.limChar = tryNode.body.limChar;
            tryNode.preComments = preComments;
            tryNode.postComments = this.parseComments();
            return tryNode;
        };
        Parser.prototype.parseCatch = function (errorRecoverySet, parentModifiers) {
            var catchMinChar = this.scanner.startPos;
            var preComments = this.parseComments();
            this.currentToken = this.scanner.scan();
            this.checkCurrentToken(TypeScript.TokenID.OpenParen, errorRecoverySet | TypeScript.ErrorRecoverySet.ExprStart);
            if((this.currentToken.tokenId != TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                this.reportParseError("Expected identifier in catch header");
                if(this.errorRecovery) {
                    this.skip(errorRecoverySet);
                    var ecatch = new TypeScript.Catch(new TypeScript.VarDecl(new TypeScript.MissingIdentifier(), this.nestingLevel), new TypeScript.Statement(TypeScript.NodeType.Empty));
                    ecatch.statement.minChar = catchMinChar;
                    ecatch.statement.limChar = this.scanner.pos;
                    ecatch.minChar = this.scanner.startPos;
                    ecatch.limChar = this.scanner.pos;
                    ecatch.flags |= TypeScript.ASTFlags.Error;
                    return ecatch;
                }
            }
            var param = new TypeScript.VarDecl(TypeScript.Identifier.fromToken(this.currentToken), this.nestingLevel);
            param.id.minChar = this.scanner.startPos;
            param.id.limChar = this.scanner.pos;
            param.minChar = param.id.minChar;
            param.limChar = param.id.limChar;
            this.currentToken = this.scanner.scan();
            var statementPos = this.scanner.pos;
            this.checkCurrentToken(TypeScript.TokenID.CloseParen, errorRecoverySet | TypeScript.ErrorRecoverySet.StmtStart);
            if(this.currentToken.tokenId != TypeScript.TokenID.OpenBrace) {
                this.reportParseError("Expected '{' to start catch body");
                if(this.errorRecovery) {
                    this.skip(errorRecoverySet);
                    var ecatch = new TypeScript.Catch(new TypeScript.VarDecl(new TypeScript.MissingIdentifier(), this.nestingLevel), new TypeScript.Statement(TypeScript.NodeType.Empty));
                    ecatch.statement.minChar = catchMinChar;
                    ecatch.statement.limChar = statementPos;
                    ecatch.minChar = this.scanner.startPos;
                    ecatch.limChar = this.scanner.pos;
                    ecatch.flags |= TypeScript.ASTFlags.Error;
                    return ecatch;
                }
            }
            var catchStmt = this.parseStatement(errorRecoverySet, TypeScript.AllowedElements.None, parentModifiers);
            var catchNode = new TypeScript.Catch(param, catchStmt);
            catchNode.statement.minChar = catchMinChar;
            catchNode.statement.limChar = statementPos;
            catchNode.minChar = catchMinChar;
            catchNode.limChar = catchStmt.limChar;
            catchNode.preComments = preComments;
            catchNode.postComments = this.parseComments();
            return catchNode;
        };
        Parser.prototype.parseFinally = function (errorRecoverySet, parentModifiers) {
            var finMinChar = this.scanner.startPos;
            var preComments = this.parseComments();
            this.currentToken = this.scanner.scan();
            if(this.currentToken.tokenId != TypeScript.TokenID.OpenBrace) {
                this.reportParseError("Expected '{' to start body of finally statement");
                if(this.errorRecovery) {
                    this.skip(errorRecoverySet);
                    var efin = new TypeScript.Finally(new TypeScript.Statement(TypeScript.NodeType.Empty));
                    efin.flags |= TypeScript.ASTFlags.Error;
                    efin.minChar = this.scanner.startPos;
                    efin.limChar = this.scanner.pos;
                    return efin;
                }
            }
            var finBody = this.parseStatement(errorRecoverySet, TypeScript.AllowedElements.None, parentModifiers);
            var fin = new TypeScript.Finally(finBody);
            fin.minChar = finMinChar;
            fin.limChar = fin.body.limChar;
            fin.preComments = preComments;
            fin.postComments = this.parseComments();
            return fin;
        };
        Parser.prototype.parseTryCatchFinally = function (errorRecoverySet, parentModifiers, labelList) {
            var tryPart = new TypeScript.Try(null);
            var tryMinChar = this.scanner.startPos;
            this.pushStmt(tryPart, labelList);
            this.parseTry(tryPart, errorRecoverySet | TypeScript.ErrorRecoverySet.Catch, parentModifiers);
            this.popStmt();
            var tc = null;
            var tf = null;
            if(this.currentToken.tokenId == TypeScript.TokenID.Catch) {
                var catchPart = this.parseCatch(errorRecoverySet | TypeScript.ErrorRecoverySet.Catch, parentModifiers);
                tc = new TypeScript.TryCatch(tryPart, catchPart);
                tc.minChar = tryPart.minChar;
                tc.limChar = catchPart.limChar;
            }
            if(this.currentToken.tokenId != TypeScript.TokenID.Finally) {
                if(tc == null) {
                    this.reportParseError("try with neither catch nor finally");
                    if(this.errorRecovery) {
                        var etf = new TypeScript.TryFinally(tryPart, new TypeScript.Finally(new TypeScript.AST(TypeScript.NodeType.Empty)));
                        etf.flags |= TypeScript.ASTFlags.Error;
                        etf.minChar = this.scanner.startPos;
                        etf.limChar = this.scanner.pos;
                        return etf;
                    }
                    return new TypeScript.TryFinally(tryPart, new TypeScript.Finally(new TypeScript.AST(TypeScript.NodeType.Empty)));
                } else {
                    return tc;
                }
            } else {
                if(tc) {
                    tryPart = tc;
                }
                var finallyPart = this.parseFinally(errorRecoverySet, parentModifiers);
                tf = new TypeScript.TryFinally(tryPart, finallyPart);
                tf.minChar = tryMinChar;
                tf.limChar = finallyPart.limChar;
                return tf;
            }
        };
        Parser.prototype.parseStatement = function (errorRecoverySet, allowedElements, parentModifiers) {
            var ast = null;
            var labelList = null;
            var astList = null;
            var temp;
            var modifiers = TypeScript.Modifiers.None;
            var minChar = this.scanner.startPos;
            var forInOk = false;
            var needTerminator = false;
            var fnOrVar = null;
            var preComments = this.parseComments();
            function isAmbient() {
                return TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient) || TypeScript.hasFlag(parentModifiers, TypeScript.Modifiers.Ambient);
            }
            function mayNotBeExported() {
                if(TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Exported)) {
                    this.reportError("Statement may not be exported");
                }
            }
            for(; ; ) {
                switch(this.currentToken.tokenId) {
                    case TypeScript.TokenID.EndOfFile:
                        ast = new TypeScript.AST(TypeScript.NodeType.Error);
                        ast.minChar = minChar;
                        ast.limChar = this.scanner.pos;
                        break;
                    case TypeScript.TokenID.Function:
                        if(this.parsingDeclareFile || isAmbient() || this.ambientModule) {
                            this.currentToken = this.scanner.scan();
                            fnOrVar = this.parsePropertyDeclaration(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon, modifiers, true, false, true);
                            if(fnOrVar.nodeType == TypeScript.NodeType.VarDecl) {
                                this.reportParseError("function keyword can only introduce function declaration");
                            } else if((fnOrVar.nodeType == TypeScript.NodeType.FuncDecl) && ((fnOrVar).fncFlags , TypeScript.FncFlags.IsFatArrowFunction)) {
                                needTerminator = true;
                            }
                            ast = fnOrVar;
                            if(TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Exported) || this.parsingDeclareFile || this.ambientModule && ast.nodeType == TypeScript.NodeType.FuncDecl) {
                                (ast).fncFlags |= TypeScript.FncFlags.Exported;
                            }
                        } else {
                            ast = this.parseFncDecl(errorRecoverySet, true, false, false, null, false, false, false, modifiers, null, true);
                            if(TypeScript.hasFlag((ast).fncFlags, TypeScript.FncFlags.IsFatArrowFunction)) {
                                needTerminator = true;
                            }
                            if(this.ambientModule) {
                                this.reportParseError("function declaration not permitted within ambient module");
                            }
                            if(TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Exported)) {
                                (ast).fncFlags |= TypeScript.FncFlags.Exported;
                            }
                        }
                        break;
                    case TypeScript.TokenID.Module:
                        if((allowedElements & TypeScript.AllowedElements.ModuleDeclarations) == TypeScript.AllowedElements.None) {
                            this.reportParseError("module not allowed in this context");
                            this.currentToken = this.scanner.scan();
                            ast = new TypeScript.AST(TypeScript.NodeType.Error);
                            ast.minChar = minChar;
                            ast.limChar = this.scanner.lastTokenLimChar();
                        } else {
                            ast = this.parseModuleDecl(errorRecoverySet, modifiers, preComments);
                            preComments = null;
                        }
                        break;
                    case TypeScript.TokenID.Import:
                        if((allowedElements & TypeScript.AllowedElements.ModuleDeclarations) == TypeScript.AllowedElements.None) {
                            this.reportParseError("module not allowed in this context");
                            this.currentToken = this.scanner.scan();
                            ast = new TypeScript.AST(TypeScript.NodeType.Error);
                            ast.minChar = minChar;
                            ast.limChar = this.scanner.lastTokenLimChar();
                        } else {
                            if(TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Exported)) {
                                this.reportParseError("export keyword not permitted on import declaration");
                            }
                            ast = this.parseImportDeclaration(errorRecoverySet, modifiers);
                            needTerminator = true;
                        }
                        break;
                    case TypeScript.TokenID.Export:
                        if((allowedElements & TypeScript.AllowedElements.ModuleDeclarations) == TypeScript.AllowedElements.None) {
                            this.reportParseError("'export' statements are only allowed at the global and module levels");
                            this.currentToken = this.scanner.scan();
                            ast = new TypeScript.AST(TypeScript.NodeType.Error);
                            ast.minChar = minChar;
                            ast.limChar = this.scanner.lastTokenLimChar();
                        }
                        if(this.topLevel) {
                            this.hasTopLevelImportOrExport = true;
                        }
                        modifiers |= TypeScript.Modifiers.Exported;
                        this.currentToken = this.scanner.scan();
                        break;
                    case TypeScript.TokenID.Private:
                        modifiers |= TypeScript.Modifiers.Private;
                        this.currentToken = this.scanner.scan();
                        if(this.parsingClassConstructorDefinition) {
                            if(!this.inferPropertiesFromThisAssignment) {
                                this.reportParseError("Property declarations are not permitted within constructor bodies");
                            }
                            minChar = this.scanner.pos;
                            if(this.inferPropertiesFromThisAssignment && (this.currentToken.tokenId != TypeScript.TokenID.This || (this.currentToken = this.scanner.scan()).tokenId != TypeScript.TokenID.Dot)) {
                                this.reportParseError("Expected 'this.' for property declaration");
                                this.currentToken = this.scanner.scan();
                                ast = new TypeScript.AST(TypeScript.NodeType.Error);
                                ast.minChar = minChar;
                                ast.limChar = this.scanner.lastTokenLimChar();
                            } else {
                                this.currentToken = this.scanner.scan();
                                var id = TypeScript.Identifier.fromToken(this.currentToken);
                                id.minChar = this.scanner.startPos;
                                id.limChar = this.scanner.pos;
                                this.currentToken = this.scanner.scan();
                                ast = this.parseClassMemberVariableDeclaration(id, minChar, this.parsingClassConstructorDefinition, errorRecoverySet, modifiers);
                            }
                        } else {
                            if(this.currentToken.tokenId != TypeScript.TokenID.Interface) {
                                if(this.currentToken.tokenId == TypeScript.TokenID.Get) {
                                    this.prevIDTok = this.currentToken;
                                    this.currentToken = this.scanner.scan();
                                    if(TypeScript.codeGenTarget < TypeScript.CodeGenTarget.ES5) {
                                        this.reportParseError("Property accessors are only available when targeting ES5 or greater");
                                    }
                                    if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                                        modifiers |= TypeScript.Modifiers.Getter;
                                        this.prevIDTok = null;
                                    }
                                } else if(this.currentToken.tokenId == TypeScript.TokenID.Set) {
                                    this.prevIDTok = this.currentToken;
                                    this.currentToken = this.scanner.scan();
                                    if(TypeScript.codeGenTarget < TypeScript.CodeGenTarget.ES5) {
                                        this.reportParseError("Property accessors are only available when targeting ES5 or greater");
                                    }
                                    if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                                        modifiers |= TypeScript.Modifiers.Setter;
                                        this.prevIDTok = null;
                                    }
                                }
                                fnOrVar = this.parsePropertyDeclaration(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon, modifiers, isAmbient(), false);
                                if((fnOrVar.nodeType == TypeScript.NodeType.VarDecl) || ((fnOrVar.nodeType == TypeScript.NodeType.FuncDecl) && (TypeScript.hasFlag((fnOrVar).fncFlags, TypeScript.FncFlags.IsFatArrowFunction)))) {
                                    needTerminator = true;
                                }
                                ast = fnOrVar;
                            }
                        }
                        break;
                    case TypeScript.TokenID.Public:
                        if(this.parsingClassConstructorDefinition) {
                            if(!this.inferPropertiesFromThisAssignment) {
                                this.reportParseError("Property declarations are not permitted within constructor bodies");
                            }
                            this.currentToken = this.scanner.scan();
                            minChar = this.scanner.pos;
                            modifiers |= TypeScript.Modifiers.Public;
                            if(this.inferPropertiesFromThisAssignment && (this.currentToken.tokenId != TypeScript.TokenID.This || (this.currentToken = this.scanner.scan()).tokenId != TypeScript.TokenID.Dot)) {
                                this.reportParseError("Expected 'this.' for property declaration");
                                this.currentToken = this.scanner.scan();
                                ast = new TypeScript.AST(TypeScript.NodeType.Error);
                                ast.minChar = minChar;
                                ast.limChar = this.scanner.lastTokenLimChar();
                            } else {
                                this.currentToken = this.scanner.scan();
                                var id = TypeScript.Identifier.fromToken(this.currentToken);
                                id.minChar = this.scanner.startPos;
                                id.limChar = this.scanner.pos;
                                this.currentToken = this.scanner.scan();
                                ast = this.parseClassMemberVariableDeclaration(id, minChar, this.parsingClassConstructorDefinition, errorRecoverySet, modifiers);
                            }
                        } else {
                            if((allowedElements & TypeScript.AllowedElements.Properties) == TypeScript.AllowedElements.None) {
                                this.reportParseError("'property' statements are only allowed within classes");
                                this.currentToken = this.scanner.scan();
                                ast = new TypeScript.AST(TypeScript.NodeType.Error);
                                ast.minChar = minChar;
                                ast.limChar = this.scanner.lastTokenLimChar();
                            } else {
                                modifiers |= TypeScript.Modifiers.Public;
                                this.currentToken = this.scanner.scan();
                                if(this.currentToken.tokenId == TypeScript.TokenID.Get) {
                                    this.prevIDTok = this.currentToken;
                                    this.currentToken = this.scanner.scan();
                                    if(TypeScript.codeGenTarget < TypeScript.CodeGenTarget.ES5) {
                                        this.reportParseError("Property accessors are only available when targeting ES5 or greater");
                                    }
                                    if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                                        modifiers |= TypeScript.Modifiers.Getter;
                                        this.prevIDTok = null;
                                    }
                                } else if(this.currentToken.tokenId == TypeScript.TokenID.Set) {
                                    this.prevIDTok = this.currentToken;
                                    this.currentToken = this.scanner.scan();
                                    if(TypeScript.codeGenTarget < TypeScript.CodeGenTarget.ES5) {
                                        this.reportParseError("Property accessors are only available when targeting ES5 or greater");
                                    }
                                    if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                                        modifiers |= TypeScript.Modifiers.Setter;
                                        this.prevIDTok = null;
                                    }
                                }
                                fnOrVar = this.parsePropertyDeclaration(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon, modifiers, isAmbient(), false);
                                if((fnOrVar.nodeType == TypeScript.NodeType.VarDecl) || ((fnOrVar.nodeType == TypeScript.NodeType.FuncDecl) && TypeScript.hasFlag((fnOrVar).fncFlags, TypeScript.FncFlags.IsFatArrowFunction))) {
                                    needTerminator = true;
                                }
                                ast = fnOrVar;
                            }
                        }
                        break;
                    case TypeScript.TokenID.Declare:
                        if(!(allowedElements & TypeScript.AllowedElements.AmbientDeclarations)) {
                            this.reportParseError("Ambient declarations are only allowed at the top-level or module scopes");
                        }
                        if(!this.parsingDeclareFile && TypeScript.hasFlag(parentModifiers, TypeScript.Modifiers.Ambient)) {
                            this.reportParseError("Duplicate ambient declaration in this context. (Is the enclosing module or class already ambient?)");
                        }
                        modifiers |= TypeScript.Modifiers.Ambient;
                        this.currentToken = this.scanner.scan();
                        break;
                    case TypeScript.TokenID.Class:
                        if((allowedElements & TypeScript.AllowedElements.ClassDeclarations) == TypeScript.AllowedElements.None) {
                            this.reportParseError("class not allowed in this context");
                            this.currentToken = this.scanner.scan();
                            ast = new TypeScript.AST(TypeScript.NodeType.Error);
                            ast.minChar = minChar;
                            ast.limChar = this.scanner.lastTokenLimChar();
                        } else {
                            ast = this.parseClassDecl(errorRecoverySet, minChar, modifiers);
                        }
                        break;
                    case TypeScript.TokenID.Interface:
                        if((allowedElements & TypeScript.AllowedElements.InterfaceDeclarations) == TypeScript.AllowedElements.None) {
                            this.reportParseError("interface not allowed in this context");
                            this.currentToken = this.scanner.scan();
                            ast = new TypeScript.AST(TypeScript.NodeType.Error);
                            ast.minChar = minChar;
                            ast.limChar = this.scanner.lastTokenLimChar();
                        } else {
                            ast = this.parseInterfaceDecl(errorRecoverySet, modifiers);
                        }
                        break;
                    case TypeScript.TokenID.Var:
                        var declAst = this.parseVariableDeclaration(errorRecoverySet | TypeScript.ErrorRecoverySet.StmtStart, modifiers, true, false);
                        if(declAst.nodeType == TypeScript.NodeType.VarDecl) {
                            ast = declAst;
                        } else {
                            ast = new TypeScript.Block(declAst, false);
                        }
                        needTerminator = true;
                        break;
                    case TypeScript.TokenID.Static:
                        if(this.currentClassDecl == null) {
                            this.reportParseError("Statics may only be class members");
                        }
                        mayNotBeExported();
                        modifiers |= TypeScript.Modifiers.Public;
                        this.currentToken = this.scanner.scan();
                        if(this.currentToken.tokenId == TypeScript.TokenID.Get) {
                            this.prevIDTok = this.currentToken;
                            this.currentToken = this.scanner.scan();
                            if(TypeScript.codeGenTarget < TypeScript.CodeGenTarget.ES5) {
                                this.reportParseError("Property accessors are only available when targeting ES5 or greater");
                            }
                            if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                                modifiers |= TypeScript.Modifiers.Getter;
                                this.prevIDTok = null;
                            }
                        } else if(this.currentToken.tokenId == TypeScript.TokenID.Set) {
                            this.currentToken = this.scanner.scan();
                            if(TypeScript.codeGenTarget < TypeScript.CodeGenTarget.ES5) {
                                this.reportParseError("Property accessors are only available when targeting ES5 or greater");
                            }
                            if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) || TypeScript.convertTokToID(this.currentToken, this.strictMode)) {
                                modifiers |= TypeScript.Modifiers.Setter;
                            }
                        }
                        if(isAmbient()) {
                            modifiers |= TypeScript.Modifiers.Ambient;
                        }
                        fnOrVar = this.parsePropertyDeclaration(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon, modifiers, this.parsingDeclareFile || (modifiers & TypeScript.Modifiers.Ambient) != TypeScript.Modifiers.None, true);
                        var staticsList = this.topStaticsList();
                        if(staticsList && fnOrVar.nodeType == TypeScript.NodeType.VarDecl) {
                            staticsList.append(fnOrVar);
                        }
                        if(fnOrVar.nodeType == TypeScript.NodeType.VarDecl || ((fnOrVar.nodeType == TypeScript.NodeType.FuncDecl) && TypeScript.hasFlag((fnOrVar).fncFlags, TypeScript.FncFlags.IsFatArrowFunction))) {
                            needTerminator = true;
                        }
                        ast = fnOrVar;
                        break;
                    case TypeScript.TokenID.For:
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("syntax error: for statement does not take modifiers");
                        }
                        minChar = this.scanner.startPos;
                        this.checkNextToken(TypeScript.TokenID.OpenParen, errorRecoverySet | TypeScript.ErrorRecoverySet.ExprStart | TypeScript.ErrorRecoverySet.Var);
                        forInOk = true;
                        switch(this.currentToken.tokenId) {
                            case TypeScript.TokenID.Var:
                                temp = this.parseVariableDeclaration(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon | TypeScript.ErrorRecoverySet.In, TypeScript.Modifiers.None, false, false);
                                break;
                            case TypeScript.TokenID.Semicolon:
                                temp = null;
                                break;
                            default:
                                temp = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon | TypeScript.ErrorRecoverySet.In, TypeScript.OperatorPrecedence.None, false, TypeContext.NoTypes);
                                break;
                        }
                        if(this.currentToken.tokenId == TypeScript.TokenID.In) {
                            if((temp == null) || (!forInOk)) {
                                this.reportParseError("malformed for statement");
                                if(this.errorRecovery) {
                                    this.skip(errorRecoverySet | TypeScript.ErrorRecoverySet.StmtStart);
                                    ast = new TypeScript.AST(TypeScript.NodeType.Empty);
                                    ast.flags |= TypeScript.ASTFlags.Error;
                                }
                            } else {
                                this.currentToken = this.scanner.scan();
                                var forInStmt = new TypeScript.ForInStatement(temp, this.parseExpr(TypeScript.ErrorRecoverySet.RParen | errorRecoverySet, TypeScript.OperatorPrecedence.Comma, false, TypeContext.NoTypes));
                                forInStmt.limChar = this.scanner.pos;
                                forInStmt.statement.minChar = minChar;
                                forInStmt.statement.limChar = this.scanner.pos;
                                this.checkCurrentToken(TypeScript.TokenID.CloseParen, TypeScript.ErrorRecoverySet.StmtStart | errorRecoverySet);
                                this.pushStmt(forInStmt, labelList);
                                forInStmt.body = this.parseStatement(errorRecoverySet, allowedElements, parentModifiers);
                                this.popStmt();
                                forInStmt.minChar = minChar;
                                ast = forInStmt;
                            }
                        } else {
                            var forStmt = new TypeScript.ForStatement(temp);
                            forStmt.minChar = minChar;
                            this.checkCurrentToken(TypeScript.TokenID.Semicolon, errorRecoverySet);
                            if(this.currentToken.tokenId == TypeScript.TokenID.Semicolon) {
                                forStmt.cond = null;
                            } else {
                                forStmt.cond = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon | TypeScript.ErrorRecoverySet.RParen, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes);
                                if(this.currentToken.tokenId != TypeScript.TokenID.Semicolon) {
                                    this.skip(errorRecoverySet | TypeScript.ErrorRecoverySet.StmtStart);
                                    ast = forStmt;
                                    ast.flags |= TypeScript.ASTFlags.Error;
                                }
                            }
                            this.currentToken = this.scanner.scan();
                            if(this.currentToken.tokenId == TypeScript.TokenID.CloseParen) {
                                forStmt.incr = null;
                            } else {
                                forStmt.incr = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon | TypeScript.ErrorRecoverySet.RParen, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes);
                            }
                            this.checkCurrentToken(TypeScript.TokenID.CloseParen, errorRecoverySet | TypeScript.ErrorRecoverySet.LCurly);
                            this.pushStmt(forStmt, labelList);
                            forStmt.body = this.parseStatement(errorRecoverySet, allowedElements, parentModifiers);
                            this.popStmt();
                            forStmt.limChar = forStmt.body.limChar;
                            ast = forStmt;
                        }
                        break;
                    case TypeScript.TokenID.With:
 {
                            if(TypeScript.codeGenTarget < TypeScript.CodeGenTarget.ES5) {
                                this.reportParseError("'with' statements are only available in ES5 codegen mode or better");
                            }
                            if(this.strictMode) {
                                this.reportParseError("'with' statements are not available in strict mode");
                            }
                            mayNotBeExported();
                            if(modifiers != TypeScript.Modifiers.None) {
                                this.reportParseError("'with' statement does not take modifiers");
                            }
                            minChar = this.scanner.startPos;
                            this.checkNextToken(TypeScript.TokenID.OpenParen, errorRecoverySet | TypeScript.ErrorRecoverySet.ExprStart | TypeScript.ErrorRecoverySet.Var);
                            var expr = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.Colon, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes);
                            this.checkCurrentToken(TypeScript.TokenID.CloseParen, errorRecoverySet | TypeScript.ErrorRecoverySet.LCurly);
                            var withStmt = new TypeScript.WithStatement(expr);
                            withStmt.body = this.parseStatement(errorRecoverySet, allowedElements, parentModifiers);
                            withStmt.minChar = minChar;
                            withStmt.limChar = withStmt.body.limChar;
                            ast = withStmt;
                        }
                        break;
                    case TypeScript.TokenID.Switch: {
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("'switch' statement does not take modifiers");
                        }
                        this.checkNextToken(TypeScript.TokenID.OpenParen, errorRecoverySet | TypeScript.ErrorRecoverySet.ExprStart);
                        var switchStmt = new TypeScript.SwitchStatement(this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.RParen, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes));
                        switchStmt.statement.minChar = minChar;
                        switchStmt.statement.limChar = this.scanner.pos;
                        this.checkCurrentToken(TypeScript.TokenID.CloseParen, errorRecoverySet | TypeScript.ErrorRecoverySet.LCurly);
                        var caseListMinChar = this.scanner.startPos;
                        this.checkCurrentToken(TypeScript.TokenID.OpenBrace, errorRecoverySet | TypeScript.ErrorRecoverySet.SCase);
                        switchStmt.defaultCase = null;
                        switchStmt.caseList = new TypeScript.ASTList();
                        var caseStmt = null;
                        this.pushStmt(switchStmt, labelList);
                        for(; ; ) {
                            if((this.currentToken.tokenId == TypeScript.TokenID.Case) || (this.currentToken.tokenId == TypeScript.TokenID.Default)) {
                                var isDefault = (this.currentToken.tokenId == TypeScript.TokenID.Default);
                                caseStmt = new TypeScript.CaseStatement();
                                caseStmt.minChar = this.scanner.startPos;
                                this.currentToken = this.scanner.scan();
                                if(isDefault) {
                                    switchStmt.defaultCase = caseStmt;
                                } else {
                                    caseStmt.expr = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.Colon, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes);
                                }
                                caseStmt.colonSpan.minChar = this.scanner.startPos;
                                caseStmt.colonSpan.limChar = this.scanner.pos;
                                this.checkCurrentToken(TypeScript.TokenID.Colon, errorRecoverySet | TypeScript.ErrorRecoverySet.StmtStart);
                                caseStmt.body = new TypeScript.ASTList();
                                this.parseStatementList(errorRecoverySet | TypeScript.ErrorRecoverySet.RCurly, caseStmt.body, false, true, allowedElements, modifiers);
                                caseStmt.limChar = caseStmt.body.limChar;
                                switchStmt.caseList.append(caseStmt);
                            } else {
                                break;
                            }
                        }
                        switchStmt.caseList.minChar = caseListMinChar;
                        switchStmt.caseList.limChar = this.scanner.pos;
                        switchStmt.limChar = switchStmt.caseList.limChar;
                        this.checkCurrentToken(TypeScript.TokenID.CloseBrace, errorRecoverySet);
                        this.popStmt();
                        ast = switchStmt;
                        break;
                    }
                    case TypeScript.TokenID.While: {
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("'while' statement does not take modifiers");
                        }
                        minChar = this.scanner.startPos;
                        this.checkNextToken(TypeScript.TokenID.OpenParen, TypeScript.ErrorRecoverySet.ExprStart | errorRecoverySet);
                        var whileStmt = new TypeScript.WhileStatement(this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.RParen, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes));
                        whileStmt.minChar = minChar;
                        this.checkCurrentToken(TypeScript.TokenID.CloseParen, errorRecoverySet | TypeScript.ErrorRecoverySet.StmtStart);
                        this.pushStmt(whileStmt, labelList);
                        whileStmt.body = this.parseStatement(errorRecoverySet, allowedElements, parentModifiers);
                        whileStmt.limChar = whileStmt.body.limChar;
                        this.popStmt();
                        ast = whileStmt;
                        break;
                    }
                    case TypeScript.TokenID.Do: {
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("'do' statement does not take modifiers");
                        }
                        minChar = this.scanner.startPos;
                        this.currentToken = this.scanner.scan();
                        var doStmt = new TypeScript.DoWhileStatement();
                        doStmt.minChar = minChar;
                        this.pushStmt(doStmt, labelList);
                        doStmt.body = this.parseStatement(errorRecoverySet | TypeScript.ErrorRecoverySet.While, allowedElements, parentModifiers);
                        this.popStmt();
                        doStmt.whileAST = new TypeScript.Identifier("while");
                        doStmt.whileAST.minChar = this.scanner.startPos;
                        this.checkCurrentToken(TypeScript.TokenID.While, errorRecoverySet | TypeScript.ErrorRecoverySet.LParen);
                        doStmt.whileAST.limChar = doStmt.whileAST.minChar + 5;
                        this.checkCurrentToken(TypeScript.TokenID.OpenParen, errorRecoverySet | TypeScript.ErrorRecoverySet.ExprStart);
                        doStmt.cond = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.RParen, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes);
                        doStmt.limChar = this.scanner.pos;
                        this.checkCurrentToken(TypeScript.TokenID.CloseParen, errorRecoverySet);
                        ast = doStmt;
                        if(this.currentToken.tokenId == TypeScript.TokenID.Semicolon) {
                            this.currentToken = this.scanner.scan();
                        }
                        break;
                    }
                    case TypeScript.TokenID.If: {
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("if statement does not take modifiers");
                        }
                        minChar = this.scanner.startPos;
                        this.checkNextToken(TypeScript.TokenID.OpenParen, errorRecoverySet | TypeScript.ErrorRecoverySet.ExprStart);
                        var ifStmt = new TypeScript.IfStatement(this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.LParen, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes));
                        ifStmt.minChar = minChar;
                        ifStmt.statement.minChar = minChar;
                        ifStmt.statement.limChar = this.scanner.pos;
                        this.checkCurrentToken(TypeScript.TokenID.CloseParen, errorRecoverySet | TypeScript.ErrorRecoverySet.StmtStart);
                        this.pushStmt(ifStmt, labelList);
                        ifStmt.thenBod = this.parseStatement(TypeScript.ErrorRecoverySet.Else | errorRecoverySet, allowedElements, parentModifiers);
                        ifStmt.limChar = ifStmt.thenBod.limChar;
                        if(this.currentToken.tokenId == TypeScript.TokenID.Else) {
                            this.currentToken = this.scanner.scan();
                            ifStmt.elseBod = this.parseStatement(errorRecoverySet, allowedElements, parentModifiers);
                            ifStmt.limChar = ifStmt.elseBod.limChar;
                        }
                        this.popStmt();
                        ast = ifStmt;
                        break;
                    }
                    case TypeScript.TokenID.Try: {
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("try statement does not take modifiers");
                        }
                        minChar = this.scanner.startPos;
                        ast = this.parseTryCatchFinally(errorRecoverySet, parentModifiers, labelList);
                        break;
                    }
                    case TypeScript.TokenID.OpenBrace: {
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("block does not take modifiers");
                        }
                        minChar = this.scanner.startPos;
                        this.currentToken = this.scanner.scan();
                        var block = new TypeScript.Block(new TypeScript.ASTList(), true);
                        this.pushStmt(block, labelList);
                        this.parseStatementList(errorRecoverySet | TypeScript.ErrorRecoverySet.RCurly, block.statements, false, false, TypeScript.AllowedElements.None, modifiers);
                        this.popStmt();
                        block.statements.minChar = minChar;
                        block.statements.limChar = this.scanner.pos;
                        block.minChar = block.statements.minChar;
                        block.limChar = block.statements.limChar;
                        this.checkCurrentToken(TypeScript.TokenID.CloseBrace, errorRecoverySet);
                        ast = block;
                        break;
                    }
                    case TypeScript.TokenID.Semicolon:
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("modifier can not appear here");
                        }
                        ast = new TypeScript.AST(TypeScript.NodeType.Empty);
                        this.currentToken = this.scanner.scan();
                        break;
                    case TypeScript.TokenID.Break:
                    case TypeScript.TokenID.Continue: {
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("modifiers can not appear before jump statement");
                        }
                        var jump = new TypeScript.Jump((this.currentToken.tokenId == TypeScript.TokenID.Break) ? TypeScript.NodeType.Break : TypeScript.NodeType.Continue);
                        this.currentToken = this.scanner.scan();
                        if((this.currentToken.tokenId == TypeScript.TokenID.Identifier) && (!this.scanner.lastTokenHadNewline())) {
                            jump.target = this.currentToken.getText();
                            this.currentToken = this.scanner.scan();
                        }
                        this.resolveJumpTarget(jump);
                        ast = jump;
                        needTerminator = true;
                        break;
                    }
                    case TypeScript.TokenID.Return: {
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("modifiers can not appear before return statement");
                        }
                        if(!this.inFunction) {
                            this.reportParseError("return statement outside of function body");
                        }
                        minChar = this.scanner.startPos;
                        this.currentToken = this.scanner.scan();
                        var retStmt = new TypeScript.ReturnStatement();
                        retStmt.minChar = minChar;
                        if((this.currentToken.tokenId != TypeScript.TokenID.Semicolon) && (this.currentToken.tokenId != TypeScript.TokenID.CloseBrace) && (!(this.scanner.lastTokenHadNewline()))) {
                            retStmt.returnExpression = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes);
                        }
                        needTerminator = true;
                        retStmt.limChar = this.scanner.lastTokenLimChar();
                        ast = retStmt;
                        break;
                    }
                    case TypeScript.TokenID.Throw:
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("modifiers can not appear before a throw statement");
                        }
                        minChar = this.scanner.startPos;
                        this.currentToken = this.scanner.scan();
                        if((this.currentToken.tokenId != TypeScript.TokenID.Semicolon) && (this.currentToken.tokenId != TypeScript.TokenID.CloseBrace) && (!(this.scanner.lastTokenHadNewline()))) {
                            temp = this.parseExpr(errorRecoverySet | TypeScript.ErrorRecoverySet.SColon, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes);
                        } else {
                            this.reportParseError("throw with no target");
                            temp = null;
                        }
                        ast = new TypeScript.UnaryExpression(TypeScript.NodeType.Throw, temp);
                        ast.limChar = this.scanner.lastTokenLimChar();
                        needTerminator = true;
                        break;
                    case TypeScript.TokenID.Enum:
                        this.currentToken = this.scanner.scan();
                        ast = this.parseEnumDecl(errorRecoverySet, modifiers);
                        ast.minChar = minChar;
                        ast.limChar = this.scanner.lastTokenLimChar();
                        if(this.parsingDeclareFile || this.ambientModule || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Ambient)) {
                            (ast).modFlags |= TypeScript.ModuleFlags.Ambient;
                        }
                        if(this.parsingDeclareFile || this.ambientModule || TypeScript.hasFlag(modifiers, TypeScript.Modifiers.Exported)) {
                            (ast).modFlags |= TypeScript.ModuleFlags.Exported;
                        }
                        break;
                    case TypeScript.TokenID.Debugger:
                        mayNotBeExported();
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("modifiers can not appear before debugger statement");
                        }
                        minChar = this.scanner.startPos;
                        this.currentToken = this.scanner.scan();
                        var debuggerStmt = new TypeScript.DebuggerStatement();
                        debuggerStmt.minChar = minChar;
                        needTerminator = true;
                        debuggerStmt.limChar = this.scanner.lastTokenLimChar();
                        ast = debuggerStmt;
                        break;
                    default:
                        if(modifiers != TypeScript.Modifiers.None) {
                            this.reportParseError("modifiers can not appear before an expression statement or label");
                        }
                        minChar = this.scanner.startPos;
                        var svPos = this.scanner.pos;
                        temp = this.parseExpr(TypeScript.ErrorRecoverySet.Colon | TypeScript.ErrorRecoverySet.StmtStart | errorRecoverySet, TypeScript.OperatorPrecedence.None, true, TypeContext.NoTypes);
                        if(this.scanner.pos == svPos) {
                            this.currentToken = this.scanner.scan();
                            ast = temp;
                        } else if((this.currentToken.tokenId == TypeScript.TokenID.Colon) && (!this.scanner.lastTokenHadNewline()) && temp && (temp.nodeType == TypeScript.NodeType.Name)) {
                            if(labelList == null) {
                                labelList = new TypeScript.ASTList();
                            }
                            labelList.append(new TypeScript.Label(temp));
                            this.currentToken = this.scanner.scan();
                        } else {
                            ast = temp;
                            needTerminator = true;
                        }
                }
                if(ast) {
                    break;
                }
            }
            if(needTerminator) {
                switch(this.currentToken.tokenId) {
                    case TypeScript.TokenID.Semicolon:
                        this.currentToken = this.scanner.scan();
                        ast.flags |= TypeScript.ASTFlags.ExplicitSemicolon;
                        break;
                    case TypeScript.TokenID.EndOfFile:
                        ast.limChar = this.scanner.pos;
                    case TypeScript.TokenID.CloseBrace:
                        ast.flags |= TypeScript.ASTFlags.AutomaticSemicolon;
                        if(this.style_requireSemi) {
                            this.reportParseStyleError("no automatic semicolon");
                        }
                        break;
                    default:
                        if(!this.scanner.lastTokenHadNewline()) {
                            this.reportParseError("Expected ';'");
                        } else {
                            ast.flags |= TypeScript.ASTFlags.AutomaticSemicolon;
                            if(this.style_requireSemi) {
                                this.reportParseStyleError("no automatic semicolon");
                            }
                        }
                        break;
                }
            }
            if(labelList) {
                ast = new TypeScript.LabeledStatement(labelList, ast);
            }
            ast.minChar = minChar;
            ast.limChar = TypeScript.max(ast.limChar, this.scanner.lastTokenLimChar());
            if(preComments) {
                ast.preComments = ast.preComments ? preComments.concat(ast.preComments) : preComments;
            }
            if(this.ambientModule && (!this.okAmbientModuleMember(ast))) {
                this.reportParseError("statement not permitted within ambient module");
            }
            ast.flags |= TypeScript.ASTFlags.IsStatement;
            return ast;
        };
        Parser.prototype.okAmbientModuleMember = function (ast) {
            var nt = ast.nodeType;
            return (nt == TypeScript.NodeType.ClassDeclaration) || (nt == TypeScript.NodeType.ImportDeclaration) || (nt == TypeScript.NodeType.InterfaceDeclaration) || (nt == TypeScript.NodeType.ModuleDeclaration) || (nt == TypeScript.NodeType.Empty) || (nt == TypeScript.NodeType.VarDecl) || ((nt == TypeScript.NodeType.Block) && !(ast).isStatementBlock) || ((nt == TypeScript.NodeType.FuncDecl) && ((ast).bod == null));
        };
        Parser.prototype.parseStatementList = function (errorRecoverySet, statements, sourceElms, noLeadingCase, allowedElements, parentModifiers) {
            var directivePrologue = sourceElms;
            statements.minChar = this.scanner.startPos;
            var limChar = this.scanner.pos;
            var innerStmts = (allowedElements & TypeScript.AllowedElements.ModuleDeclarations) == TypeScript.AllowedElements.None;
            var classNope = (allowedElements & TypeScript.AllowedElements.ClassDeclarations) == TypeScript.AllowedElements.None;
            errorRecoverySet |= TypeScript.ErrorRecoverySet.TypeScriptS | TypeScript.ErrorRecoverySet.RCurly;
            var oldStrictMode = this.strictMode;
            this.nestingLevel++;
            for(; ; ) {
                if((this.currentToken.tokenId == TypeScript.TokenID.CloseBrace) || (noLeadingCase && ((this.currentToken.tokenId == TypeScript.TokenID.Case) || (this.currentToken.tokenId == TypeScript.TokenID.Default))) || (innerStmts && (this.currentToken.tokenId == TypeScript.TokenID.Export)) || (classNope && (this.currentToken.tokenId == TypeScript.TokenID.Class)) || (this.currentToken.tokenId == TypeScript.TokenID.EndOfFile)) {
                    statements.limChar = limChar;
                    if(statements.members.length == 0) {
                        statements.preComments = this.parseComments();
                    } else {
                        statements.postComments = this.parseComments();
                    }
                    this.strictMode = oldStrictMode;
                    this.nestingLevel--;
                    return;
                }
                var stmt = this.parseStatement(errorRecoverySet & (~(TypeScript.ErrorRecoverySet.Else | TypeScript.ErrorRecoverySet.RParen | TypeScript.ErrorRecoverySet.Catch | TypeScript.ErrorRecoverySet.Colon)), allowedElements, parentModifiers);
                if(stmt) {
                    stmt.postComments = this.combineComments(stmt.postComments, this.parseCommentsForLine(this.scanner.prevLine));
                    statements.append(stmt);
                    limChar = stmt.limChar;
                    if(directivePrologue) {
                        if(stmt.nodeType == TypeScript.NodeType.QString) {
                            var qstring = stmt;
                            if(qstring.text == "\"use strict\"") {
                                statements.flags |= TypeScript.ASTFlags.StrictMode;
                                this.strictMode = true;
                            } else {
                                directivePrologue = false;
                            }
                        } else {
                            directivePrologue = false;
                        }
                    }
                }
            }
        };
        Parser.prototype.quickParse = function (sourceText, filename, unitIndex) {
            var svGenTarget = TypeScript.moduleGenTarget;
            try  {
                TypeScript.moduleGenTarget = TypeScript.ModuleGenTarget.Local;
                var script = this.parse(sourceText, filename, unitIndex, TypeScript.AllowedElements.QuickParse);
                return new QuickParseResult(script, this.scanner.lexState);
            }finally {
                TypeScript.moduleGenTarget = svGenTarget;
            }
        };
        Parser.prototype.parse = function (sourceText, filename, unitIndex, allowedElements) {
            if (typeof allowedElements === "undefined") { allowedElements = TypeScript.AllowedElements.Global; }
            var _this = this;
            this.fname = filename;
            this.currentUnitIndex = unitIndex;
            this.currentToken = null;
            this.needTerminator = false;
            this.inFunction = false;
            this.inInterfaceDecl = false;
            this.inFncDecl = false;
            this.ambientModule = false;
            this.ambientClass = false;
            this.topLevel = true;
            this.allowImportDeclaration = true;
            this.prevIDTok = null;
            this.statementInfoStack = new Array();
            this.hasTopLevelImportOrExport = false;
            this.strictMode = false;
            this.nestingLevel = 0;
            this.prevExpr = null;
            this.currentClassDefinition = null;
            this.parsingClassConstructorDefinition = false;
            this.parsingDeclareFile = false;
            this.amdDependencies = [];
            this.inferPropertiesFromThisAssignment = false;
            this.requiresExtendsBlock = false;
            this.scanner.resetComments();
            this.scanner.setErrorHandler(function (message) {
                return _this.reportParseError(message);
            });
            this.scanner.setSourceText(sourceText, TypeScript.LexMode.File);
            var leftCurlyCount = this.scanner.leftCurlyCount;
            var rightCurlyCount = this.scanner.rightCurlyCount;
            var minChar = this.scanner.pos;
            this.currentToken = this.scanner.scan();
            this.pushDeclLists();
            var bod = new TypeScript.ASTList();
            bod.minChar = minChar;
            this.parsingDeclareFile = TypeScript.isDSTRFile(filename) || TypeScript.isDTSFile(filename);
            while(true) {
                this.parseStatementList(TypeScript.ErrorRecoverySet.EOF | TypeScript.ErrorRecoverySet.Func, bod, true, false, allowedElements, TypeScript.Modifiers.None);
                if(this.currentToken.tokenId === TypeScript.TokenID.EndOfFile) {
                    break;
                }
                var badToken = TypeScript.tokenTable[this.currentToken.tokenId];
                this.reportParseError("Unexpected statement block terminator '" + badToken.text + "'");
                this.currentToken = this.scanner.scan();
            }
            bod.limChar = this.scanner.pos;
            var topLevelMod = null;
            if(TypeScript.moduleGenTarget != TypeScript.ModuleGenTarget.Local && this.hasTopLevelImportOrExport) {
                var correctedFileName = TypeScript.switchToForwardSlashes(filename);
                var id = new TypeScript.Identifier(correctedFileName);
                topLevelMod = new TypeScript.ModuleDeclaration(id, bod, this.topVarList(), null);
                topLevelMod.modFlags |= TypeScript.ModuleFlags.IsDynamic;
                topLevelMod.modFlags |= TypeScript.ModuleFlags.IsWholeFile;
                topLevelMod.modFlags |= TypeScript.ModuleFlags.Exported;
                if(this.parsingDeclareFile) {
                    topLevelMod.modFlags |= TypeScript.ModuleFlags.Ambient;
                }
                topLevelMod.minChar = minChar;
                topLevelMod.limChar = this.scanner.pos;
                topLevelMod.prettyName = TypeScript.getPrettyName(correctedFileName);
                topLevelMod.containsUnicodeChar = this.scanner.seenUnicodeChar;
                topLevelMod.containsUnicodeCharInComment = this.scanner.seenUnicodeCharInComment;
                topLevelMod.amdDependencies = this.amdDependencies;
                bod = new TypeScript.ASTList();
                bod.minChar = topLevelMod.minChar;
                bod.limChar = topLevelMod.limChar;
                bod.append(topLevelMod);
            }
            var script = new TypeScript.Script(this.topVarList(), this.topScopeList());
            script.bod = bod;
            this.popDeclLists();
            script.minChar = minChar;
            script.limChar = this.scanner.pos;
            script.locationInfo = new TypeScript.LocationInfo(filename, this.scanner.lineMap, unitIndex);
            script.leftCurlyCount = this.scanner.leftCurlyCount - leftCurlyCount;
            script.rightCurlyCount = this.scanner.rightCurlyCount - rightCurlyCount;
            script.isDeclareFile = this.parsingDeclareFile;
            script.topLevelMod = topLevelMod;
            script.containsUnicodeChar = this.scanner.seenUnicodeChar;
            script.containsUnicodeCharInComment = this.scanner.seenUnicodeCharInComment;
            script.requiresExtendsBlock = this.requiresExtendsBlock;
            return script;
        };
        return Parser;
    })();
    TypeScript.Parser = Parser;    
    function quickParse(logger, scopeStartAST, sourceText, minChar, limChar, errorCapture) {
        var fragment = sourceText.getText(minChar, limChar);
        logger.log("Quick parse range (" + minChar + "," + limChar + "): \"" + TypeScript.stringToLiteral(fragment, 100) + "\"");
        var quickParser = new Parser();
        quickParser.setErrorRecovery(null);
        quickParser.errorCallback = errorCapture;
        var quickClassDecl = new TypeScript.ClassDeclaration(null, null, null, null);
        quickParser.currentClassDecl = quickClassDecl;
        var result = quickParser.quickParse(new TypeScript.StringSourceText(fragment), "", 0);
        return result;
    }
    TypeScript.quickParse = quickParse;
})(TypeScript || (TypeScript = {}));
