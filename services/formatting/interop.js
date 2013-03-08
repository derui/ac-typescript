var __extends = this.__extends || function (d, b) {
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var Formatting;
(function (Formatting) {
    var SnapshotSpan = (function () {
        function SnapshotSpan(snapshot, span) {
            this.snapshot = snapshot;
            this.span = span;
            this._startPoint = null;
            this._endPoint = null;
        }
        SnapshotSpan.prototype.start = function () {
            if(this._startPoint == null) {
                this._startPoint = new SnapshotPoint(this.snapshot, this.span.start());
            }
            return this._startPoint;
        };
        SnapshotSpan.prototype.end = function () {
            if(this._endPoint == null) {
                this._endPoint = new SnapshotPoint(this.snapshot, this.span.end());
            }
            return this._endPoint;
        };
        SnapshotSpan.prototype.startPosition = function () {
            return this.span.start();
        };
        SnapshotSpan.prototype.endPosition = function () {
            return this.span.end();
        };
        SnapshotSpan.prototype.GetText = function () {
            return this.snapshot.GetText(this.span);
        };
        SnapshotSpan.prototype.IsEmpty = function () {
            return this.span.length() === 0;
        };
        SnapshotSpan.prototype.OverlapsWith = function (spanArg) {
            return this.span.OverlapsWith(spanArg);
        };
        SnapshotSpan.prototype.Intersection = function (simpleSpan) {
            var nullable = this.span.Intersection(simpleSpan);
            if(nullable !== null) {
                return new SnapshotSpan(this.snapshot, nullable);
            }
            return null;
        };
        return SnapshotSpan;
    })();
    Formatting.SnapshotSpan = SnapshotSpan;    
    var SnapshotPoint = (function () {
        function SnapshotPoint(snapshot, position) {
            this.snapshot = snapshot;
            this.position = position;
        }
        SnapshotPoint.prototype.GetContainingLine = function () {
            return this.snapshot.GetLineFromPosition(this.position);
        };
        SnapshotPoint.prototype.Add = function (offset) {
            return new SnapshotPoint(this.snapshot, this.position + offset);
        };
        return SnapshotPoint;
    })();
    Formatting.SnapshotPoint = SnapshotPoint;    
    var FileAuthoringProxy = (function () {
        function FileAuthoringProxy(scriptSyntaxAST) {
            this.scriptSyntaxAST = scriptSyntaxAST;
        }
        FileAuthoringProxy.prototype.GetASTCursor = function () {
            return new AuthorParseNodeCursor(this);
        };
        return FileAuthoringProxy;
    })();
    Formatting.FileAuthoringProxy = FileAuthoringProxy;    
    var AuthorParseNodeCursor = (function () {
        function AuthorParseNodeCursor(fileAuthoringProxy) {
            this.fileAuthoringProxy = fileAuthoringProxy;
            this.logger = this.fileAuthoringProxy.scriptSyntaxAST.getLogger();
            this.path = new TypeScript.AstPath();
        }
        AuthorParseNodeCursor.prototype.fixupPath = function (newPath) {
            var temp = new TypeScript.AstPath();
            for(var i = 0; i < newPath.count(); i++) {
                temp.push(newPath.asts[i]);
                if(temp.isBodyOfCase()) {
                    var fakeBlock = this.mapBodyOfCase(temp.ast());
                    var previousBody = temp.pop();
                    temp.push(fakeBlock);
                    temp.push(previousBody);
                }
            }
            return temp;
        };
        AuthorParseNodeCursor.prototype.mapNodeType = function (path) {
            var nodeType = path.ast().nodeType;
            var mapList = function () {
                if(path.isBodyOfScript() || path.isBodyOfModule() || path.isBodyOfClass() || path.isBodyOfInterface() || path.isBodyOfFor() || path.isBodyOfForIn() || path.isBodyOfWhile() || path.isBodyOfDoWhile() || path.isBodyOfTry() || path.isBodyOfCatch() || path.isBodyOfFinally() || path.isBodyOfFunction() || path.isBodyOfWith() || path.isBodyOfSwitch() || false) {
                    return AuthorParseNodeKind.apnkBlock;
                } else {
                    return AuthorParseNodeKind.apnkList;
                }
            };
            switch(nodeType) {
                case TypeScript.NodeType.None:
                    return AuthorParseNodeKind.apnkEmptyNode;
                case TypeScript.NodeType.Empty:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.EmptyExpr:
                    return AuthorParseNodeKind.apnkEmptyNode;
                case TypeScript.NodeType.True:
                    return AuthorParseNodeKind.apnkTrue;
                case TypeScript.NodeType.False:
                    return AuthorParseNodeKind.apnkFalse;
                case TypeScript.NodeType.This:
                    return AuthorParseNodeKind.apnkThis;
                case TypeScript.NodeType.Super:
                    return AuthorParseNodeKind.apnkThis;
                case TypeScript.NodeType.QString:
                    return AuthorParseNodeKind.apnkStr;
                case TypeScript.NodeType.Regex:
                    return AuthorParseNodeKind.apnkRegExp;
                case TypeScript.NodeType.Null:
                    return AuthorParseNodeKind.apnkNull;
                case TypeScript.NodeType.ArrayLit:
                    return AuthorParseNodeKind.apnkArray;
                case TypeScript.NodeType.ObjectLit:
                    return AuthorParseNodeKind.apnkObject;
                case TypeScript.NodeType.Void:
                    return AuthorParseNodeKind.apnkNull;
                case TypeScript.NodeType.Comma:
                    return AuthorParseNodeKind.apnkComma;
                case TypeScript.NodeType.Pos:
                    return AuthorParseNodeKind.apnkPos;
                case TypeScript.NodeType.Neg:
                    return AuthorParseNodeKind.apnkNeg;
                case TypeScript.NodeType.Delete:
                    return AuthorParseNodeKind.apnkDelete;
                case TypeScript.NodeType.Await:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.In:
                    return AuthorParseNodeKind.apnkIn;
                case TypeScript.NodeType.Dot:
                    return AuthorParseNodeKind.apnkDot;
                case TypeScript.NodeType.From:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.Is:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.InstOf:
                    return AuthorParseNodeKind.apnkInstOf;
                case TypeScript.NodeType.Typeof:
                    return AuthorParseNodeKind.apnkTypeof;
                case TypeScript.NodeType.NumberLit:
                    return AuthorParseNodeKind.apnkInt;
                case TypeScript.NodeType.Name:
                    return AuthorParseNodeKind.apnkName;
                case TypeScript.NodeType.TypeRef:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.Index:
                    return AuthorParseNodeKind.apnkIndex;
                case TypeScript.NodeType.Call:
                    return AuthorParseNodeKind.apnkCall;
                case TypeScript.NodeType.New:
                    return AuthorParseNodeKind.apnkNew;
                case TypeScript.NodeType.Asg:
                    return AuthorParseNodeKind.apnkAsg;
                case TypeScript.NodeType.AsgAdd:
                    return AuthorParseNodeKind.apnkAsgAdd;
                case TypeScript.NodeType.AsgSub:
                    return AuthorParseNodeKind.apnkAsgDiv;
                case TypeScript.NodeType.AsgDiv:
                    return AuthorParseNodeKind.apnkAsgMul;
                case TypeScript.NodeType.AsgMul:
                    return AuthorParseNodeKind.apnkAsgMul;
                case TypeScript.NodeType.AsgMod:
                    return AuthorParseNodeKind.apnkAsgMod;
                case TypeScript.NodeType.AsgAnd:
                    return AuthorParseNodeKind.apnkAsgAnd;
                case TypeScript.NodeType.AsgXor:
                    return AuthorParseNodeKind.apnkAsgXor;
                case TypeScript.NodeType.AsgOr:
                    return AuthorParseNodeKind.apnkAsgOr;
                case TypeScript.NodeType.AsgLsh:
                    return AuthorParseNodeKind.apnkAsgLsh;
                case TypeScript.NodeType.AsgRsh:
                    return AuthorParseNodeKind.apnkAsgRsh;
                case TypeScript.NodeType.AsgRs2:
                    return AuthorParseNodeKind.apnkAsgRs2;
                case TypeScript.NodeType.ConditionalExpression:
                    return AuthorParseNodeKind.apnkQmark;
                case TypeScript.NodeType.LogOr:
                    return AuthorParseNodeKind.apnkLogOr;
                case TypeScript.NodeType.LogAnd:
                    return AuthorParseNodeKind.apnkLogAnd;
                case TypeScript.NodeType.Or:
                    return AuthorParseNodeKind.apnkOr;
                case TypeScript.NodeType.Xor:
                    return AuthorParseNodeKind.apnkXor;
                case TypeScript.NodeType.And:
                    return AuthorParseNodeKind.apnkAnd;
                case TypeScript.NodeType.Eq:
                    return AuthorParseNodeKind.apnkEq;
                case TypeScript.NodeType.Ne:
                    return AuthorParseNodeKind.apnkNe;
                case TypeScript.NodeType.Eqv:
                    return AuthorParseNodeKind.apnkEqv;
                case TypeScript.NodeType.NEqv:
                    return AuthorParseNodeKind.apnkNEqv;
                case TypeScript.NodeType.Lt:
                    return AuthorParseNodeKind.apnkLt;
                case TypeScript.NodeType.Le:
                    return AuthorParseNodeKind.apnkLe;
                case TypeScript.NodeType.Gt:
                    return AuthorParseNodeKind.apnkGt;
                case TypeScript.NodeType.Ge:
                    return AuthorParseNodeKind.apnkGe;
                case TypeScript.NodeType.Add:
                    return AuthorParseNodeKind.apnkAdd;
                case TypeScript.NodeType.Sub:
                    return AuthorParseNodeKind.apnkSub;
                case TypeScript.NodeType.Mul:
                    return AuthorParseNodeKind.apnkMul;
                case TypeScript.NodeType.Div:
                    return AuthorParseNodeKind.apnkDiv;
                case TypeScript.NodeType.Mod:
                    return AuthorParseNodeKind.apnkMod;
                case TypeScript.NodeType.Lsh:
                    return AuthorParseNodeKind.apnkLsh;
                case TypeScript.NodeType.Rsh:
                    return AuthorParseNodeKind.apnkRsh;
                case TypeScript.NodeType.Rs2:
                    return AuthorParseNodeKind.apnkRs2;
                case TypeScript.NodeType.Not:
                    return AuthorParseNodeKind.apnkNot;
                case TypeScript.NodeType.LogNot:
                    return AuthorParseNodeKind.apnkLogNot;
                case TypeScript.NodeType.IncPre:
                    return AuthorParseNodeKind.apnkIncPre;
                case TypeScript.NodeType.DecPre:
                    return AuthorParseNodeKind.apnkDecPre;
                case TypeScript.NodeType.IncPost:
                    return AuthorParseNodeKind.apnkIncPost;
                case TypeScript.NodeType.DecPost:
                    return AuthorParseNodeKind.apnkDecPost;
                case TypeScript.NodeType.TypeAssertion:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.FuncDecl:
                    return AuthorParseNodeKind.apnkFncDecl;
                case TypeScript.NodeType.Member:
                    return AuthorParseNodeKind.apnkMember;
                case TypeScript.NodeType.VarDecl:
                    return AuthorParseNodeKind.apnkVarDecl;
                case TypeScript.NodeType.ArgDecl:
                    return AuthorParseNodeKind.apnkVarDecl;
                case TypeScript.NodeType.Return:
                    return AuthorParseNodeKind.apnkReturn;
                case TypeScript.NodeType.Break:
                    return AuthorParseNodeKind.apnkBreak;
                case TypeScript.NodeType.Continue:
                    return AuthorParseNodeKind.apnkContinue;
                case TypeScript.NodeType.Throw:
                    return AuthorParseNodeKind.apnkThrow;
                case TypeScript.NodeType.For:
                    return AuthorParseNodeKind.apnkFor;
                case TypeScript.NodeType.ForIn:
                    return AuthorParseNodeKind.apnkForIn;
                case TypeScript.NodeType.If:
                    return AuthorParseNodeKind.apnkIf;
                case TypeScript.NodeType.While:
                    return AuthorParseNodeKind.apnkWhile;
                case TypeScript.NodeType.DoWhile:
                    return AuthorParseNodeKind.apnkDoWhile;
                case TypeScript.NodeType.Block:
                    return (path.ast()).isStatementBlock ? AuthorParseNodeKind.apnkBlock : AuthorParseNodeKind.apnkVarDeclList;
                case TypeScript.NodeType.Case:
                    return AuthorParseNodeKind.apnkCase;
                case TypeScript.NodeType.Switch:
                    return AuthorParseNodeKind.apnkSwitch;
                case TypeScript.NodeType.Try:
                    return AuthorParseNodeKind.apnkTry;
                case TypeScript.NodeType.TryCatch:
                    return AuthorParseNodeKind.apnkTryCatch;
                case TypeScript.NodeType.TryFinally:
                    return AuthorParseNodeKind.apnkTryFinally;
                case TypeScript.NodeType.Finally:
                    return AuthorParseNodeKind.apnkFinally;
                case TypeScript.NodeType.Catch:
                    return AuthorParseNodeKind.apnkCatch;
                case TypeScript.NodeType.List:
                    return mapList();
                case TypeScript.NodeType.Script:
                    return AuthorParseNodeKind.apnkProg;
                case TypeScript.NodeType.ClassDeclaration:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.InterfaceDeclaration:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.ModuleDeclaration:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.ImportDeclaration:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.With:
                    return AuthorParseNodeKind.apnkWith;
                case TypeScript.NodeType.Label:
                    return AuthorParseNodeKind.apnkLabel;
                case TypeScript.NodeType.LabeledStatement:
                    return AuthorParseNodeKind.apnkLabel;
                case TypeScript.NodeType.EBStart:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.GotoEB:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.EndCode:
                    return AuthorParseNodeKind.apnkEndCode;
                case TypeScript.NodeType.Error:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.Comment:
                    return AuthorParseNodeKind.apnkEmpty;
                case TypeScript.NodeType.Debugger:
                    return AuthorParseNodeKind.apnkDebugger;
                default:
                    throw new Error("Invalid node kind: " + nodeType);
            }
        };
        AuthorParseNodeCursor.prototype.mapNodeFlags = function (path) {
            var result = AuthorParseNodeFlags.apnfNone;
            if(path.isSynthesizedBlock()) {
                result = result | AuthorParseNodeFlags.apnfSyntheticNode;
            }
            return result;
        };
        AuthorParseNodeCursor.prototype.getDetails = function (path) {
            var ast = path.ast();
            var result = new AuthorParseNodeDetails();
            if(path.isListOfObjectLit()) {
                if(!path.parent().isParenthesized) {
                    Formatting.Debug.Assert(path.parent().minChar == path.ast().minChar, "Assumption about AST minChar position is not verified");
                    Formatting.Debug.Assert(path.parent().limChar == path.ast().limChar, "Assumption about AST limChar position is not verified");
                }
                result.StartOffset = ast.minChar + 1;
                result.EndOffset = ast.limChar - 1;
            } else {
                result.StartOffset = ast.minChar;
                result.EndOffset = ast.limChar;
            }
            result.Flags = this.mapNodeFlags(path);
            result.Kind = this.mapNodeType(path);
            result.nodeType = ast.nodeType;
            result.ast = ast;
            return result;
        };
        AuthorParseNodeCursor.prototype.getEdgeLabel = function (path) {
            if(path.isBodyOfScript() || path.isBodyOfModule() || path.isBodyOfClass() || path.isBodyOfInterface() || path.isBodyOfFunction() || path.isBodyOfFor() || path.isBodyOfForIn() || path.isBodyOfWhile() || path.isBodyOfDoWhile() || path.isBodyOfWith() || path.isBodyOfSwitch() || this.isBodyOfCase(path) || path.isBodyOfTry() || path.isBodyOfCatch() || path.isBodyOfFinally()) {
                return AuthorParseNodeEdge.apneBody;
            }
            if(path.isThenOfIf()) {
                return AuthorParseNodeEdge.apneThen;
            }
            if(path.isElseOfIf()) {
                return AuthorParseNodeEdge.apneElse;
            }
            if(path.isBodyOfBlock() || this.isBodyOfBlock(path)) {
                return AuthorParseNodeEdge.apneBlockBody;
            }
            if(path.isDefaultCaseOfSwitch()) {
                return AuthorParseNodeEdge.apneDefaultCase;
            }
            if(path.isCaseOfSwitch()) {
                return AuthorParseNodeEdge.apneCase;
            }
            if(path.isListOfObjectLit()) {
                return AuthorParseNodeEdge.apneMembers;
            }
            if(path.isListOfArrayLit()) {
                return AuthorParseNodeEdge.apneElements;
            }
            if(path.isMemberOfMember()) {
                return AuthorParseNodeEdge.apneMember;
            }
            if(path.isTargetOfMember()) {
                return AuthorParseNodeEdge.apneTarget;
            }
            if(path.isArgumentOfFunction()) {
                return AuthorParseNodeEdge.apneArgument;
            }
            if(path.isItemOfList()) {
                return AuthorParseNodeEdge.apneListItem;
            }
            return AuthorParseNodeEdge.apneNone;
        };
        AuthorParseNodeCursor.prototype.getEmptyNodeDetails = function () {
            var result = new AuthorParseNodeDetails();
            result.StartOffset = 0;
            result.EndOffset = 0;
            result.Flags = AuthorParseNodeFlags.apnfNone;
            result.Kind = AuthorParseNodeKind.apnkEmptyNode;
            result.nodeType = TypeScript.NodeType.Empty;
            result.ast = null;
            return result;
        };
        AuthorParseNodeCursor.prototype.getAstPath = function (position, options) {
            if (typeof options === "undefined") { options = TypeScript.GetAstPathOptions.Default; }
            var path = this.fileAuthoringProxy.scriptSyntaxAST.getAstPathToPosition(position, options);
            while(path.count() >= 1 && this.skipNode(path)) {
                path.up();
            }
            return path;
        };
        AuthorParseNodeCursor.prototype.moveToParent = function (path) {
            while(path.count() >= 1) {
                path.up();
                if(!this.skipNode(path)) {
                    break;
                }
            }
        };
        AuthorParseNodeCursor.prototype.getAuthorParseNodeDetails = function (path) {
            if(path.count() === 0) {
                return this.getEmptyNodeDetails();
            } else {
                return this.getDetails(path);
            }
        };
        AuthorParseNodeCursor.prototype.Current = function () {
            return this.getAuthorParseNodeDetails(this.path);
        };
        AuthorParseNodeCursor.prototype.Parent = function () {
            var temp = this.path.clone();
            this.moveToParent(temp);
            return this.getAuthorParseNodeDetails(temp);
        };
        AuthorParseNodeCursor.prototype.MoveToChild = function (edgeLabel, index) {
            var _this = this;
            var pushIfNotNull = function (node) {
                if(node != null) {
                    _this.path.push(node);
                    return _this.getAuthorParseNodeDetails(_this.path);
                } else {
                    return _this.getEmptyNodeDetails();
                }
            };
            if(this.path.count() >= 1) {
                var ast = this.path.ast();
                switch(ast.nodeType) {
                    case TypeScript.NodeType.ArrayLit:
 {
                            switch(edgeLabel) {
                                case AuthorParseNodeEdge.apneElements:
                                    return pushIfNotNull((ast).operand);
                            }
                        }
                        break;
                }
            }
            var empty = new TypeScript.AST(TypeScript.NodeType.Empty);
            this.path.push(empty);
            return this.getAuthorParseNodeDetails(this.path);
        };
        AuthorParseNodeCursor.prototype.MoveUp = function () {
            this.moveToParent(this.path);
            return this.getAuthorParseNodeDetails(this.path);
        };
        AuthorParseNodeCursor.prototype.SeekToOffset = function (offset, excludeEndOffset) {
            var newPath = this.getAstPath(offset, excludeEndOffset ? TypeScript.GetAstPathOptions.Default : TypeScript.GetAstPathOptions.EdgeInclusive);
            this.path = this.fixupPath(newPath);
            if(this.path.count() == 0) {
                return null;
            }
            return this.getDetails(this.path);
        };
        AuthorParseNodeCursor.prototype.MoveToEnclosingNode = function (startOffset, endOffset) {
            if(startOffset > endOffset) {
                throw new Error("Invalid offsets");
            }
            var start = this.getAstPath(startOffset);
            var end = this.getAstPath(endOffset - 1);
            if(start.count() == 0 || end.count() == 0) {
                throw new Error("No nodes enclosing span");
            }
            var startIndex = 0;
            var endIndex = 0;
            while(startIndex < start.count() && endIndex < end.count()) {
                if(start.get(startIndex) !== end.get(endIndex)) {
                    break;
                }
                startIndex++;
                endIndex++;
            }
            start.top = startIndex - 1;
            while(this.skipNode(start)) {
                start.up();
            }
            this.path = this.fixupPath(start);
            return this.getDetails(this.path);
        };
        AuthorParseNodeCursor.prototype.skipNode = function (path) {
            return path.isBodyOfSwitch() || path.isTopLevelImplicitModule() || path.isBodyOfTopLevelImplicitModule() || (path.isBodyOfBlock() && path.isSingleStatementList()) || (path.isBodyOfCase() && path.isSingleStatementList()) || (path.isArgumentListOfFunction());
        };
        AuthorParseNodeCursor.prototype.mapAstNode = function (path, depth) {
            if(!TypeScript.isValidAstNode(path.ast())) {
                return null;
            }
            if(this.skipNode(path)) {
                return null;
            }
            var result = new AuthorParseNode();
            result.Details = this.getDetails(path);
            result.Level = depth;
            result.Label = 0;
            result.Name = 0;
            result.EdgeLabel = this.getEdgeLabel(path);
            return result;
        };
        AuthorParseNodeCursor.prototype.isBodyOfBlock = function (path) {
            var result = false;
            if(path.count() >= 2) {
                if(path.ast().nodeType == TypeScript.NodeType.List && !path.isSingleStatementList() && path.isBodyOfCase()) {
                    result = true;
                }
                path.up();
                if(path.ast().nodeType == TypeScript.NodeType.List && path.isSingleStatementList() && path.isBodyOfCase()) {
                    result = true;
                }
                path.down();
            }
            return result;
        };
        AuthorParseNodeCursor.prototype.isBodyOfCase = function (path) {
            var asts = path.asts;
            var top = path.top;
            return path.count() >= 2 && asts[top - 1].nodeType === TypeScript.NodeType.Case && asts[top - 0].nodeType === TypeScript.NodeType.Block && (asts[top - 1]).body == (asts[top - 0]).statements;
        };
        AuthorParseNodeCursor.prototype.mapBodyOfCase = function (body) {
            var fakeBlock = new TypeScript.Block(body, false);
            fakeBlock.minChar = body.minChar;
            fakeBlock.limChar = body.limChar;
            return fakeBlock;
        };
        AuthorParseNodeCursor.prototype.GetSubTree = function (depth) {
            var _this = this;
            if(this.path.count() == 0) {
                return new AuthorParseNodeSet([]);
            }
            var context = {
                path: new TypeScript.AstPath(),
                nodes: [],
                curDepth: 0,
                curDepths: []
            };
            var pre = function (cur, parent, walker) {
                context.curDepths.push(context.curDepth);
                context.path.push(cur);
                if(context.path.isBodyOfCase()) {
                    var fakeBlock = _this.mapBodyOfCase(cur);
                    var previousBody = context.path.pop();
                    context.path.push(fakeBlock);
                    context.nodes.push(_this.mapAstNode(context.path, context.curDepth));
                    context.curDepth++;
                    context.path.pop();
                    context.path.push(previousBody);
                }
                var node = _this.mapAstNode(context.path, context.curDepth);
                if(node !== null) {
                    context.nodes.push(node);
                    context.curDepth++;
                }
                walker.options.goChildren = (depth < 0 || context.curDepth <= depth);
                return cur;
            };
            var post = function (cur, parent, walker) {
                context.curDepth = context.curDepths.pop();
                context.path.pop();
                return cur;
            };
            TypeScript.getAstWalkerFactory().walk(this.path.ast(), pre, post);
            if(this.logger.information()) {
                this.logger.log("getSubTree(" + depth + ")");
                for(var i = 0; i < context.nodes.length; i++) {
                    var authorNode = context.nodes[i];
                    var text = authorNode.Level + ": " + (AuthorParseNodeKind)._map[authorNode.Details.Kind] + " - " + (TypeScript.NodeType)._map[authorNode.Details.nodeType] + "(" + (AuthorParseNodeEdge)._map[authorNode.EdgeLabel] + ")" + "(" + authorNode.Details.StartOffset + "," + authorNode.Details.EndOffset + ")" + " -- F:(" + authorNode.Details.Flags + ")";
                    this.logger.log(text);
                }
            }
            return new AuthorParseNodeSet(context.nodes);
        };
        AuthorParseNodeCursor.prototype.GetNodeProperty = function (nodeProperty) {
            if(this.path.count() == 0) {
                return 0;
            }
            var authorNode = this.mapAstNode(this.path, 0);
            if(authorNode.Details.ast === null) {
                return 0;
            }
            switch(authorNode.Details.ast.nodeType) {
                case TypeScript.NodeType.FuncDecl:
 {
                        var funcDecl = (authorNode.Details.ast);
                        var bod = funcDecl.bod;
                        switch(nodeProperty) {
                            case AuthorParseNodeProperty.apnpFunctionKeywordMin:
                                return funcDecl.minChar;
                            case AuthorParseNodeProperty.apnpLCurlyMin:
                                if(bod !== null && bod.minChar > 0) {
                                    return bod.minChar;
                                } else {
                                    return 0;
                                }
                            case AuthorParseNodeProperty.apnpRCurlyMin:
                                if(bod !== null && bod.limChar > 0) {
                                    return bod.limChar - 1;
                                } else {
                                    return 0;
                                }
                            case AuthorParseNodeProperty.apnpRParenMin:
                                if(funcDecl.arguments != null && funcDecl.arguments.limChar > 0) {
                                    return funcDecl.arguments.limChar - 1;
                                }
                        }
                    }
                    break;
                case TypeScript.NodeType.ClassDeclaration:
 {
                        var classDecl = authorNode.Details.ast;
                        var bod = classDecl.members;
                        switch(nodeProperty) {
                            case AuthorParseNodeProperty.apnpLCurlyMin:
                                if(bod !== null) {
                                    return bod.minChar;
                                } else {
                                    return 0;
                                }
                            case AuthorParseNodeProperty.apnpRCurlyMin:
                                if(bod !== null) {
                                    return bod.limChar - 1;
                                } else {
                                    return 0;
                                }
                        }
                    }
                    break;
            }
            if(this.logger.warning()) {
                this.logger.log("NYI:GetNodeProperty " + "(nodeType=" + (TypeScript.NodeType)._map[authorNode.Details.ast.nodeType] + ", " + "propperty= " + (AuthorParseNodeProperty)._map[nodeProperty] + ")");
            }
            return 0;
        };
        AuthorParseNodeCursor.prototype.GetEdgeLabel = function () {
            return this.mapAstNode(this.path, 0).EdgeLabel;
        };
        return AuthorParseNodeCursor;
    })();
    Formatting.AuthorParseNodeCursor = AuthorParseNodeCursor;    
    var TextSnapshot = (function () {
        function TextSnapshot(script, sourceText) {
            this.script = script;
            this.sourceText = sourceText;
            this.lines = [];
        }
        TextSnapshot.prototype.GetText = function (span) {
            return this.sourceText.getText(span.start(), span.end());
        };
        TextSnapshot.prototype.GetLineNumberFromPosition = function (position) {
            var lineNumber = TypeScript.getLineNumberFromPosition(this.script.locationInfo.lineMap, position);
            return lineNumber - 1;
        };
        TextSnapshot.prototype.GetLineFromPosition = function (position) {
            var lineNumber = this.GetLineNumberFromPosition(position);
            return this.GetLineFromLineNumber(lineNumber);
        };
        TextSnapshot.prototype.GetLineFromLineNumber = function (lineNumber) {
            var line = this.lines[lineNumber];
            if(line === undefined) {
                line = this.GetLineFromLineNumberWorker(lineNumber);
                this.lines[lineNumber] = line;
            }
            return line;
        };
        TextSnapshot.prototype.GetLineFromLineNumberWorker = function (lineNumber) {
            var lineMap = this.script.locationInfo.lineMap;
            var lineMapIndex = lineNumber + 1;
            if(lineMapIndex < 1 || lineMapIndex >= lineMap.length) {
                throw new Error("invalid line number (" + lineMapIndex + ")");
            }
            var start = lineMap[lineMapIndex];
            var end;
            var endIncludingLineBreak;
            var lineBreak = "";
            if(lineMapIndex == lineMap.length) {
                end = endIncludingLineBreak = this.sourceText.getLength();
            } else {
                endIncludingLineBreak = (lineMapIndex >= lineMap.length - 1 ? this.sourceText.getLength() : this.script.locationInfo.lineMap[lineMapIndex + 1]);
                for(var p = endIncludingLineBreak - 1; p >= start; p--) {
                    var c = this.sourceText.getText(p, p + 1);
                    if(c != "\r" && c != "\n") {
                        break;
                    }
                }
                end = p + 1;
                lineBreak = this.sourceText.getText(end, endIncludingLineBreak);
            }
            var result = new TextSnapshotLine(this, lineNumber, start, end, lineBreak);
            return result;
        };
        return TextSnapshot;
    })();
    Formatting.TextSnapshot = TextSnapshot;    
    var TextSnapshotLine = (function () {
        function TextSnapshotLine(_snapshot, _lineNumber, _start, _end, _lineBreak) {
            this._snapshot = _snapshot;
            this._lineNumber = _lineNumber;
            this._start = _start;
            this._end = _end;
            this._lineBreak = _lineBreak;
        }
        TextSnapshotLine.prototype.snapshot = function () {
            return this._snapshot;
        };
        TextSnapshotLine.prototype.start = function () {
            return new SnapshotPoint(this._snapshot, this._start);
        };
        TextSnapshotLine.prototype.startPosition = function () {
            return this._start;
        };
        TextSnapshotLine.prototype.end = function () {
            return new SnapshotPoint(this._snapshot, this._end);
        };
        TextSnapshotLine.prototype.endPosition = function () {
            return this._end;
        };
        TextSnapshotLine.prototype.endIncludingLineBreak = function () {
            return new SnapshotPoint(this._snapshot, this._end + this._lineBreak.length);
        };
        TextSnapshotLine.prototype.endIncludingLineBreakPosition = function () {
            return this._end + this._lineBreak.length;
        };
        TextSnapshotLine.prototype.length = function () {
            return this._end - this._start;
        };
        TextSnapshotLine.prototype.lineNumber = function () {
            return this._lineNumber;
        };
        TextSnapshotLine.prototype.getText = function () {
            return this._snapshot.GetText(Span.FromBounds(this._start, this._end));
        };
        return TextSnapshotLine;
    })();
    Formatting.TextSnapshotLine = TextSnapshotLine;    
    var Span = (function () {
        function Span(_start, _length) {
            this._start = _start;
            this._length = _length;
            if(this._start < 0) {
                throw new Error("Invalid start value");
            }
            if(this._length < 0) {
                throw new Error("Invalid length value");
            }
        }
        Span.prototype.start = function () {
            return this._start;
        };
        Span.prototype.end = function () {
            return this._start + this._length;
        };
        Span.prototype.length = function () {
            return this._length;
        };
        Span.prototype.Intersection = function (span) {
            var start = Math.Max(this.start(), span.start());
            var end = Math.Min(this.end(), span.end());
            if(start <= end) {
                return Span.FromBounds(start, end);
            }
            return null;
        };
        Span.prototype.OverlapsWith = function (span) {
            var num = Math.Max(this.start(), span.start());
            var num2 = Math.Min(this.end(), span.end());
            return (num < num2);
        };
        Span.prototype.Contains = function (span) {
            return ((span.start() >= this.start()) && (span.end() <= this.end()));
        };
        Span.FromBounds = function FromBounds(start, end) {
            return new Span(start, end - start);
        };
        return Span;
    })();
    Formatting.Span = Span;    
    var ListEnumerator = (function () {
        function ListEnumerator(list) {
            this.list = list;
            this.index = -1;
        }
        ListEnumerator.prototype.MoveNext = function () {
            if(this.index < this.list.count() - 1) {
                this.index++;
                return true;
            }
            return false;
        };
        ListEnumerator.prototype.Current = function () {
            return this.list.get(this.index);
        };
        return ListEnumerator;
    })();
    Formatting.ListEnumerator = ListEnumerator;    
    var List = (function () {
        function List() {
            this.items = List.empty;
        }
        List.empty = [];
        List.prototype.copyOnWrite = function () {
            if(this.items === List.empty) {
                this.items = [];
            }
        };
        List.prototype.count = function () {
            return this.items.length;
        };
        List.prototype.get = function (index) {
            var result = this.items[index];
            if(result === undefined) {
                throw new Error("Invalid list index " + index + " (valid range is 0 to " + (this.items.length - 1) + ")");
            }
            return result;
        };
        List.prototype.add = function (item) {
            if(item === undefined) {
                throw new Error("Cannot add an undefined value in a list");
            }
            this.copyOnWrite();
            this.items.push(item);
        };
        List.prototype.addAll = function (range) {
            var _this = this;
            range.forEach(function (item) {
                _this.add(item);
            });
        };
        List.prototype.insert = function (index, item) {
            if(item === undefined) {
                throw new Error("Cannot add an undefined value in a list");
            }
            if(index < 0 || index > this.items.length) {
                throw new Error("Invalid index when inserting into array" + " (valid range is 0 to " + this.items.length + ")");
            }
            this.copyOnWrite();
            this.items.splice(index, 0, item);
        };
        List.prototype.contains = function (item) {
            var found = false;
            this.foreach(function (i) {
                if(i === item) {
                    found = true;
                }
            });
            return found;
        };
        List.prototype.foreach = function (action) {
            var list = this;
            for(var i = 0, len = list.count(); i < len; i++) {
                action(list.get(i));
            }
        };
        List.prototype.GetEnumerator = function () {
            return new ListEnumerator(this);
        };
        List.prototype.Where = function (pred) {
            var result = new List();
            this.foreach(function (item) {
                if(pred(item)) {
                    result.add(item);
                }
            });
            return result;
        };
        return List;
    })();
    Formatting.List = List;    
    var List_TokenSpan = (function (_super) {
        __extends(List_TokenSpan, _super);
        function List_TokenSpan() {
            _super.apply(this, arguments);

        }
        List_TokenSpan.prototype.get = function (index) {
            return _super.prototype.get.call(this, index);
        };
        List_TokenSpan.prototype.foreach = function (action) {
            _super.prototype.foreach.call(this, action);
        };
        return List_TokenSpan;
    })(List);
    Formatting.List_TokenSpan = List_TokenSpan;    
    var List_AuthorTokenKind = (function (_super) {
        __extends(List_AuthorTokenKind, _super);
        function List_AuthorTokenKind() {
            _super.apply(this, arguments);

        }
        List_AuthorTokenKind.prototype.get = function (index) {
            return _super.prototype.get.call(this, index);
        };
        List_AuthorTokenKind.prototype.foreach = function (action) {
            _super.prototype.foreach.call(this, action);
        };
        return List_AuthorTokenKind;
    })(List);
    Formatting.List_AuthorTokenKind = List_AuthorTokenKind;    
    var List_IndentationInfo = (function (_super) {
        __extends(List_IndentationInfo, _super);
        function List_IndentationInfo() {
            _super.apply(this, arguments);

        }
        List_IndentationInfo.prototype.get = function (index) {
            return _super.prototype.get.call(this, index);
        };
        List_IndentationInfo.prototype.foreach = function (action) {
            _super.prototype.foreach.call(this, action);
        };
        return List_IndentationInfo;
    })(List);
    Formatting.List_IndentationInfo = List_IndentationInfo;    
    var List_IndentationEditInfo = (function (_super) {
        __extends(List_IndentationEditInfo, _super);
        function List_IndentationEditInfo() {
            _super.apply(this, arguments);

        }
        List_IndentationEditInfo.prototype.get = function (index) {
            return _super.prototype.get.call(this, index);
        };
        List_IndentationEditInfo.prototype.foreach = function (action) {
            _super.prototype.foreach.call(this, action);
        };
        return List_IndentationEditInfo;
    })(List);
    Formatting.List_IndentationEditInfo = List_IndentationEditInfo;    
    var List_ParseNode = (function (_super) {
        __extends(List_ParseNode, _super);
        function List_ParseNode() {
            _super.apply(this, arguments);

        }
        List_ParseNode.prototype.get = function (index) {
            return _super.prototype.get.call(this, index);
        };
        List_ParseNode.prototype.foreach = function (action) {
            _super.prototype.foreach.call(this, action);
        };
        return List_ParseNode;
    })(List);
    Formatting.List_ParseNode = List_ParseNode;    
    var List_TextEditInfo = (function (_super) {
        __extends(List_TextEditInfo, _super);
        function List_TextEditInfo(item) {
            if (typeof item === "undefined") { item = null; }
                _super.call(this);
            if(item != null) {
                this.add(item);
            }
        }
        List_TextEditInfo.prototype.get = function (index) {
            return _super.prototype.get.call(this, index);
        };
        List_TextEditInfo.prototype.foreach = function (action) {
            _super.prototype.foreach.call(this, action);
        };
        return List_TextEditInfo;
    })(List);
    Formatting.List_TextEditInfo = List_TextEditInfo;    
    var List_Rule = (function (_super) {
        __extends(List_Rule, _super);
        function List_Rule() {
            _super.apply(this, arguments);

        }
        List_Rule.prototype.get = function (index) {
            return _super.prototype.get.call(this, index);
        };
        List_Rule.prototype.foreach = function (action) {
            _super.prototype.foreach.call(this, action);
        };
        List_Rule.prototype.Add = function (item) {
            _super.prototype.add.call(this, item);
        };
        List_Rule.prototype.AddRange = function (items) {
            var _this = this;
            items.forEach(function (item) {
                _this.Add(item);
            });
        };
        return List_Rule;
    })(List);
    Formatting.List_Rule = List_Rule;    
    var List_ITextSnapshotLine = (function (_super) {
        __extends(List_ITextSnapshotLine, _super);
        function List_ITextSnapshotLine() {
            _super.apply(this, arguments);

        }
        List_ITextSnapshotLine.prototype.get = function (index) {
            return _super.prototype.get.call(this, index);
        };
        List_ITextSnapshotLine.prototype.foreach = function (action) {
            _super.prototype.foreach.call(this, action);
        };
        return List_ITextSnapshotLine;
    })(List);
    Formatting.List_ITextSnapshotLine = List_ITextSnapshotLine;    
    var Stack = (function () {
        function Stack() {
            this.items = [];
        }
        Stack.prototype.Count = function () {
            return this.items.length;
        };
        Stack.prototype.Push = function (item) {
            if(item === undefined) {
                throw new Error("Cannot add an undefined value in a list");
            }
            this.items.push(item);
        };
        Stack.prototype.Pop = function () {
            if(this.items.length === 0) {
                throw new Error("Cannot pop from an empty stack");
            }
            return this.items.pop();
        };
        return Stack;
    })();
    Formatting.Stack = Stack;    
    var Stack_ParseNode = (function (_super) {
        __extends(Stack_ParseNode, _super);
        function Stack_ParseNode() {
            _super.apply(this, arguments);

        }
        Stack_ParseNode.prototype.Pop = function () {
            return _super.prototype.Pop.call(this);
        };
        return Stack_ParseNode;
    })(Stack);
    Formatting.Stack_ParseNode = Stack_ParseNode;    
    function BinarySearch(list, searchValue, comparer) {
        var low = 0;
        var high = list.count();
        while(low < high) {
            var median = (low + high) >> 1;
            var compareResult = comparer(searchValue, list.get(median));
            if(compareResult > 0) {
                low = median + 1;
            } else {
                high = median;
            }
        }
        Debug.Assert(low >= 0 && low <= list.count(), "Incorrect implementation of BinarySearch");
        if(low == list.count() || comparer(searchValue, list.get(low)) != 0) {
            return ~low;
        }
        return low;
    }
    Formatting.BinarySearch = BinarySearch;
    function FirstOrDefault(list, pred) {
        for(var i = 0, len = list.count(); i < len; i++) {
            if(pred(list.get(i))) {
                return list.get(i);
            }
        }
        return null;
    }
    Formatting.FirstOrDefault = FirstOrDefault;
    function LastOrDefault(list, pred) {
        for(var len = list.count(), i = len - 1; i >= 0; i--) {
            if(pred(list.get(i))) {
                return list.get(i);
            }
        }
        return null;
    }
    Formatting.LastOrDefault = LastOrDefault;
    var AuthorParseNode = (function () {
        function AuthorParseNode() {
        }
        return AuthorParseNode;
    })();
    Formatting.AuthorParseNode = AuthorParseNode;    
    var AuthorParseNodeDetails = (function () {
        function AuthorParseNodeDetails() {
        }
        AuthorParseNodeDetails.prototype.Equals = function (other) {
            if(other == null) {
                return false;
            }
            return this.Kind == other.Kind && this.nodeType == other.nodeType && this.StartOffset == other.StartOffset && this.EndOffset == other.EndOffset && this.Flags == other.Flags;
        };
        return AuthorParseNodeDetails;
    })();
    Formatting.AuthorParseNodeDetails = AuthorParseNodeDetails;    
    (function (AuthorParseNodeFlags) {
        AuthorParseNodeFlags._map = [];
        AuthorParseNodeFlags.apnfNone = 0x0000;
        AuthorParseNodeFlags.apnfSyntheticNode = 0x0100;
    })(Formatting.AuthorParseNodeFlags || (Formatting.AuthorParseNodeFlags = {}));
    var AuthorParseNodeFlags = Formatting.AuthorParseNodeFlags;
    (function (AuthorParseNodeKind) {
        AuthorParseNodeKind._map = [];
        AuthorParseNodeKind._map[0] = "apnkEmptyNode";
        AuthorParseNodeKind.apnkEmptyNode = 0;
        AuthorParseNodeKind._map[1] = "apnkNone";
        AuthorParseNodeKind.apnkNone = 1;
        AuthorParseNodeKind._map[2] = "apnkName";
        AuthorParseNodeKind.apnkName = 2;
        AuthorParseNodeKind._map[3] = "apnkInt";
        AuthorParseNodeKind.apnkInt = 3;
        AuthorParseNodeKind._map[4] = "apnkFlt";
        AuthorParseNodeKind.apnkFlt = 4;
        AuthorParseNodeKind._map[5] = "apnkStr";
        AuthorParseNodeKind.apnkStr = 5;
        AuthorParseNodeKind._map[6] = "apnkRegExp";
        AuthorParseNodeKind.apnkRegExp = 6;
        AuthorParseNodeKind._map[7] = "apnkThis";
        AuthorParseNodeKind.apnkThis = 7;
        AuthorParseNodeKind._map[8] = "apnkNull";
        AuthorParseNodeKind.apnkNull = 8;
        AuthorParseNodeKind._map[9] = "apnkFalse";
        AuthorParseNodeKind.apnkFalse = 9;
        AuthorParseNodeKind._map[10] = "apnkTrue";
        AuthorParseNodeKind.apnkTrue = 10;
        AuthorParseNodeKind._map[11] = "apnkEmpty";
        AuthorParseNodeKind.apnkEmpty = 11;
        AuthorParseNodeKind._map[12] = "apnkLdFncSlot";
        AuthorParseNodeKind.apnkLdFncSlot = 12;
        AuthorParseNodeKind._map[13] = "apnkArgRef";
        AuthorParseNodeKind.apnkArgRef = 13;
        AuthorParseNodeKind._map[14] = "apnkHelperCall3";
        AuthorParseNodeKind.apnkHelperCall3 = 14;
        AuthorParseNodeKind._map[15] = "apnkNot";
        AuthorParseNodeKind.apnkNot = 15;
        AuthorParseNodeKind._map[16] = "apnkNeg";
        AuthorParseNodeKind.apnkNeg = 16;
        AuthorParseNodeKind._map[17] = "apnkPos";
        AuthorParseNodeKind.apnkPos = 17;
        AuthorParseNodeKind._map[18] = "apnkLogNot";
        AuthorParseNodeKind.apnkLogNot = 18;
        AuthorParseNodeKind._map[19] = "apnkIncPost";
        AuthorParseNodeKind.apnkIncPost = 19;
        AuthorParseNodeKind._map[20] = "apnkDecPost";
        AuthorParseNodeKind.apnkDecPost = 20;
        AuthorParseNodeKind._map[21] = "apnkIncPre";
        AuthorParseNodeKind.apnkIncPre = 21;
        AuthorParseNodeKind._map[22] = "apnkDecPre";
        AuthorParseNodeKind.apnkDecPre = 22;
        AuthorParseNodeKind._map[23] = "apnkTypeof";
        AuthorParseNodeKind.apnkTypeof = 23;
        AuthorParseNodeKind._map[24] = "apnkVoid";
        AuthorParseNodeKind.apnkVoid = 24;
        AuthorParseNodeKind._map[25] = "apnkDelete";
        AuthorParseNodeKind.apnkDelete = 25;
        AuthorParseNodeKind._map[26] = "apnkArray";
        AuthorParseNodeKind.apnkArray = 26;
        AuthorParseNodeKind._map[27] = "apnkObject";
        AuthorParseNodeKind.apnkObject = 27;
        AuthorParseNodeKind._map[28] = "apnkTempRef";
        AuthorParseNodeKind.apnkTempRef = 28;
        AuthorParseNodeKind._map[29] = "apnkStFncSlot";
        AuthorParseNodeKind.apnkStFncSlot = 29;
        AuthorParseNodeKind._map[30] = "apnkAdd";
        AuthorParseNodeKind.apnkAdd = 30;
        AuthorParseNodeKind._map[31] = "apnkSub";
        AuthorParseNodeKind.apnkSub = 31;
        AuthorParseNodeKind._map[32] = "apnkMul";
        AuthorParseNodeKind.apnkMul = 32;
        AuthorParseNodeKind._map[33] = "apnkDiv";
        AuthorParseNodeKind.apnkDiv = 33;
        AuthorParseNodeKind._map[34] = "apnkMod";
        AuthorParseNodeKind.apnkMod = 34;
        AuthorParseNodeKind._map[35] = "apnkOr";
        AuthorParseNodeKind.apnkOr = 35;
        AuthorParseNodeKind._map[36] = "apnkXor";
        AuthorParseNodeKind.apnkXor = 36;
        AuthorParseNodeKind._map[37] = "apnkAnd";
        AuthorParseNodeKind.apnkAnd = 37;
        AuthorParseNodeKind._map[38] = "apnkEq";
        AuthorParseNodeKind.apnkEq = 38;
        AuthorParseNodeKind._map[39] = "apnkNe";
        AuthorParseNodeKind.apnkNe = 39;
        AuthorParseNodeKind._map[40] = "apnkLt";
        AuthorParseNodeKind.apnkLt = 40;
        AuthorParseNodeKind._map[41] = "apnkLe";
        AuthorParseNodeKind.apnkLe = 41;
        AuthorParseNodeKind._map[42] = "apnkGe";
        AuthorParseNodeKind.apnkGe = 42;
        AuthorParseNodeKind._map[43] = "apnkGt";
        AuthorParseNodeKind.apnkGt = 43;
        AuthorParseNodeKind._map[44] = "apnkCall";
        AuthorParseNodeKind.apnkCall = 44;
        AuthorParseNodeKind._map[45] = "apnkDot";
        AuthorParseNodeKind.apnkDot = 45;
        AuthorParseNodeKind._map[46] = "apnkAsg";
        AuthorParseNodeKind.apnkAsg = 46;
        AuthorParseNodeKind._map[47] = "apnkInstOf";
        AuthorParseNodeKind.apnkInstOf = 47;
        AuthorParseNodeKind._map[48] = "apnkIn";
        AuthorParseNodeKind.apnkIn = 48;
        AuthorParseNodeKind._map[49] = "apnkEqv";
        AuthorParseNodeKind.apnkEqv = 49;
        AuthorParseNodeKind._map[50] = "apnkNEqv";
        AuthorParseNodeKind.apnkNEqv = 50;
        AuthorParseNodeKind._map[51] = "apnkComma";
        AuthorParseNodeKind.apnkComma = 51;
        AuthorParseNodeKind._map[52] = "apnkLogOr";
        AuthorParseNodeKind.apnkLogOr = 52;
        AuthorParseNodeKind._map[53] = "apnkLogAnd";
        AuthorParseNodeKind.apnkLogAnd = 53;
        AuthorParseNodeKind._map[54] = "apnkLsh";
        AuthorParseNodeKind.apnkLsh = 54;
        AuthorParseNodeKind._map[55] = "apnkRsh";
        AuthorParseNodeKind.apnkRsh = 55;
        AuthorParseNodeKind._map[56] = "apnkRs2";
        AuthorParseNodeKind.apnkRs2 = 56;
        AuthorParseNodeKind._map[57] = "apnkNew";
        AuthorParseNodeKind.apnkNew = 57;
        AuthorParseNodeKind._map[58] = "apnkIndex";
        AuthorParseNodeKind.apnkIndex = 58;
        AuthorParseNodeKind._map[59] = "apnkQmark";
        AuthorParseNodeKind.apnkQmark = 59;
        AuthorParseNodeKind._map[60] = "apnkAsgAdd";
        AuthorParseNodeKind.apnkAsgAdd = 60;
        AuthorParseNodeKind._map[61] = "apnkAsgSub";
        AuthorParseNodeKind.apnkAsgSub = 61;
        AuthorParseNodeKind._map[62] = "apnkAsgMul";
        AuthorParseNodeKind.apnkAsgMul = 62;
        AuthorParseNodeKind._map[63] = "apnkAsgDiv";
        AuthorParseNodeKind.apnkAsgDiv = 63;
        AuthorParseNodeKind._map[64] = "apnkAsgMod";
        AuthorParseNodeKind.apnkAsgMod = 64;
        AuthorParseNodeKind._map[65] = "apnkAsgAnd";
        AuthorParseNodeKind.apnkAsgAnd = 65;
        AuthorParseNodeKind._map[66] = "apnkAsgXor";
        AuthorParseNodeKind.apnkAsgXor = 66;
        AuthorParseNodeKind._map[67] = "apnkAsgOr";
        AuthorParseNodeKind.apnkAsgOr = 67;
        AuthorParseNodeKind._map[68] = "apnkAsgLsh";
        AuthorParseNodeKind.apnkAsgLsh = 68;
        AuthorParseNodeKind._map[69] = "apnkAsgRsh";
        AuthorParseNodeKind.apnkAsgRsh = 69;
        AuthorParseNodeKind._map[70] = "apnkAsgRs2";
        AuthorParseNodeKind.apnkAsgRs2 = 70;
        AuthorParseNodeKind._map[71] = "apnkScope";
        AuthorParseNodeKind.apnkScope = 71;
        AuthorParseNodeKind._map[72] = "apnkMember";
        AuthorParseNodeKind.apnkMember = 72;
        AuthorParseNodeKind._map[73] = "apnkSetMember";
        AuthorParseNodeKind.apnkSetMember = 73;
        AuthorParseNodeKind._map[74] = "apnkGetMember";
        AuthorParseNodeKind.apnkGetMember = 74;
        AuthorParseNodeKind._map[75] = "apnkList";
        AuthorParseNodeKind.apnkList = 75;
        AuthorParseNodeKind._map[76] = "apnkVarDecl";
        AuthorParseNodeKind.apnkVarDecl = 76;
        AuthorParseNodeKind._map[77] = "apnkTemp";
        AuthorParseNodeKind.apnkTemp = 77;
        AuthorParseNodeKind._map[78] = "apnkFncDecl";
        AuthorParseNodeKind.apnkFncDecl = 78;
        AuthorParseNodeKind._map[79] = "apnkProg";
        AuthorParseNodeKind.apnkProg = 79;
        AuthorParseNodeKind._map[80] = "apnkEndCode";
        AuthorParseNodeKind.apnkEndCode = 80;
        AuthorParseNodeKind._map[81] = "apnkDebugger";
        AuthorParseNodeKind.apnkDebugger = 81;
        AuthorParseNodeKind._map[82] = "apnkFor";
        AuthorParseNodeKind.apnkFor = 82;
        AuthorParseNodeKind._map[83] = "apnkIf";
        AuthorParseNodeKind.apnkIf = 83;
        AuthorParseNodeKind._map[84] = "apnkWhile";
        AuthorParseNodeKind.apnkWhile = 84;
        AuthorParseNodeKind._map[85] = "apnkDoWhile";
        AuthorParseNodeKind.apnkDoWhile = 85;
        AuthorParseNodeKind._map[86] = "apnkForIn";
        AuthorParseNodeKind.apnkForIn = 86;
        AuthorParseNodeKind._map[87] = "apnkBlock";
        AuthorParseNodeKind.apnkBlock = 87;
        AuthorParseNodeKind._map[88] = "apnkWith";
        AuthorParseNodeKind.apnkWith = 88;
        AuthorParseNodeKind._map[89] = "apnkBreak";
        AuthorParseNodeKind.apnkBreak = 89;
        AuthorParseNodeKind._map[90] = "apnkContinue";
        AuthorParseNodeKind.apnkContinue = 90;
        AuthorParseNodeKind._map[91] = "apnkLabel";
        AuthorParseNodeKind.apnkLabel = 91;
        AuthorParseNodeKind._map[92] = "apnkSwitch";
        AuthorParseNodeKind.apnkSwitch = 92;
        AuthorParseNodeKind._map[93] = "apnkCase";
        AuthorParseNodeKind.apnkCase = 93;
        AuthorParseNodeKind._map[94] = "apnkTryCatch";
        AuthorParseNodeKind.apnkTryCatch = 94;
        AuthorParseNodeKind._map[95] = "apnkCatch";
        AuthorParseNodeKind.apnkCatch = 95;
        AuthorParseNodeKind._map[96] = "apnkReturn";
        AuthorParseNodeKind.apnkReturn = 96;
        AuthorParseNodeKind._map[97] = "apnkTry";
        AuthorParseNodeKind.apnkTry = 97;
        AuthorParseNodeKind._map[98] = "apnkThrow";
        AuthorParseNodeKind.apnkThrow = 98;
        AuthorParseNodeKind._map[99] = "apnkFinally";
        AuthorParseNodeKind.apnkFinally = 99;
        AuthorParseNodeKind._map[100] = "apnkTryFinally";
        AuthorParseNodeKind.apnkTryFinally = 100;
        AuthorParseNodeKind._map[101] = "apnkStruct";
        AuthorParseNodeKind.apnkStruct = 101;
        AuthorParseNodeKind._map[102] = "apnkEnum";
        AuthorParseNodeKind.apnkEnum = 102;
        AuthorParseNodeKind._map[103] = "apnkTyped";
        AuthorParseNodeKind.apnkTyped = 103;
        AuthorParseNodeKind._map[104] = "apnkVarDeclList";
        AuthorParseNodeKind.apnkVarDeclList = 104;
        AuthorParseNodeKind._map[105] = "apnkDefaultCase";
        AuthorParseNodeKind.apnkDefaultCase = 105;
    })(Formatting.AuthorParseNodeKind || (Formatting.AuthorParseNodeKind = {}));
    var AuthorParseNodeKind = Formatting.AuthorParseNodeKind;
    (function (AuthorTokenKind) {
        AuthorTokenKind._map = [];
        AuthorTokenKind._map[0] = "atkEnd";
        AuthorTokenKind.atkEnd = 0;
        AuthorTokenKind._map[1] = "atkText";
        AuthorTokenKind.atkText = 1;
        AuthorTokenKind._map[2] = "atkIdentifier";
        AuthorTokenKind.atkIdentifier = 2;
        AuthorTokenKind._map[3] = "atkComment";
        AuthorTokenKind.atkComment = 3;
        AuthorTokenKind._map[4] = "atkNumber";
        AuthorTokenKind.atkNumber = 4;
        AuthorTokenKind._map[5] = "atkString";
        AuthorTokenKind.atkString = 5;
        AuthorTokenKind._map[6] = "atkRegexp";
        AuthorTokenKind.atkRegexp = 6;
        AuthorTokenKind._map[7] = "atkConditionalComp";
        AuthorTokenKind.atkConditionalComp = 7;
        AuthorTokenKind._map[8] = "atkScanError";
        AuthorTokenKind.atkScanError = 8;
        AuthorTokenKind._map[9] = "atkSColon";
        AuthorTokenKind.atkSColon = 9;
        AuthorTokenKind._map[10] = "atkLParen";
        AuthorTokenKind.atkLParen = 10;
        AuthorTokenKind._map[11] = "atkRParen";
        AuthorTokenKind.atkRParen = 11;
        AuthorTokenKind._map[12] = "atkLBrack";
        AuthorTokenKind.atkLBrack = 12;
        AuthorTokenKind._map[13] = "atkRBrack";
        AuthorTokenKind.atkRBrack = 13;
        AuthorTokenKind._map[14] = "atkLCurly";
        AuthorTokenKind.atkLCurly = 14;
        AuthorTokenKind._map[15] = "atkRCurly";
        AuthorTokenKind.atkRCurly = 15;
        AuthorTokenKind._map[16] = "atkComma";
        AuthorTokenKind.atkComma = 16;
        AuthorTokenKind._map[17] = "atkArrow";
        AuthorTokenKind.atkArrow = 17;
        AuthorTokenKind._map[18] = "atkAsg";
        AuthorTokenKind.atkAsg = 18;
        AuthorTokenKind._map[19] = "atkAsgAdd";
        AuthorTokenKind.atkAsgAdd = 19;
        AuthorTokenKind._map[20] = "atkAsgSub";
        AuthorTokenKind.atkAsgSub = 20;
        AuthorTokenKind._map[21] = "atkAsgMul";
        AuthorTokenKind.atkAsgMul = 21;
        AuthorTokenKind._map[22] = "atkAsgDiv";
        AuthorTokenKind.atkAsgDiv = 22;
        AuthorTokenKind._map[23] = "atkAsgMod";
        AuthorTokenKind.atkAsgMod = 23;
        AuthorTokenKind._map[24] = "atkAsgAnd";
        AuthorTokenKind.atkAsgAnd = 24;
        AuthorTokenKind._map[25] = "atkAsgXor";
        AuthorTokenKind.atkAsgXor = 25;
        AuthorTokenKind._map[26] = "atkAsgOr";
        AuthorTokenKind.atkAsgOr = 26;
        AuthorTokenKind._map[27] = "atkAsgLsh";
        AuthorTokenKind.atkAsgLsh = 27;
        AuthorTokenKind._map[28] = "atkAsgRsh";
        AuthorTokenKind.atkAsgRsh = 28;
        AuthorTokenKind._map[29] = "atkAsgRs2";
        AuthorTokenKind.atkAsgRs2 = 29;
        AuthorTokenKind._map[30] = "atkQMark";
        AuthorTokenKind.atkQMark = 30;
        AuthorTokenKind._map[31] = "atkColon";
        AuthorTokenKind.atkColon = 31;
        AuthorTokenKind._map[32] = "atkLogOr";
        AuthorTokenKind.atkLogOr = 32;
        AuthorTokenKind._map[33] = "atkLogAnd";
        AuthorTokenKind.atkLogAnd = 33;
        AuthorTokenKind._map[34] = "atkOr";
        AuthorTokenKind.atkOr = 34;
        AuthorTokenKind._map[35] = "atkXor";
        AuthorTokenKind.atkXor = 35;
        AuthorTokenKind._map[36] = "atkAnd";
        AuthorTokenKind.atkAnd = 36;
        AuthorTokenKind._map[37] = "atkEQ";
        AuthorTokenKind.atkEQ = 37;
        AuthorTokenKind._map[38] = "atkNE";
        AuthorTokenKind.atkNE = 38;
        AuthorTokenKind._map[39] = "atkEqv";
        AuthorTokenKind.atkEqv = 39;
        AuthorTokenKind._map[40] = "atkNEqv";
        AuthorTokenKind.atkNEqv = 40;
        AuthorTokenKind._map[41] = "atkLT";
        AuthorTokenKind.atkLT = 41;
        AuthorTokenKind._map[42] = "atkLE";
        AuthorTokenKind.atkLE = 42;
        AuthorTokenKind._map[43] = "atkGT";
        AuthorTokenKind.atkGT = 43;
        AuthorTokenKind._map[44] = "atkGE";
        AuthorTokenKind.atkGE = 44;
        AuthorTokenKind._map[45] = "atkLsh";
        AuthorTokenKind.atkLsh = 45;
        AuthorTokenKind._map[46] = "atkRsh";
        AuthorTokenKind.atkRsh = 46;
        AuthorTokenKind._map[47] = "atkRs2";
        AuthorTokenKind.atkRs2 = 47;
        AuthorTokenKind._map[48] = "atkAdd";
        AuthorTokenKind.atkAdd = 48;
        AuthorTokenKind._map[49] = "atkSub";
        AuthorTokenKind.atkSub = 49;
        AuthorTokenKind._map[50] = "atkMult";
        AuthorTokenKind.atkMult = 50;
        AuthorTokenKind._map[51] = "atkDiv";
        AuthorTokenKind.atkDiv = 51;
        AuthorTokenKind._map[52] = "atkPct";
        AuthorTokenKind.atkPct = 52;
        AuthorTokenKind._map[53] = "atkTilde";
        AuthorTokenKind.atkTilde = 53;
        AuthorTokenKind._map[54] = "atkBang";
        AuthorTokenKind.atkBang = 54;
        AuthorTokenKind._map[55] = "atkInc";
        AuthorTokenKind.atkInc = 55;
        AuthorTokenKind._map[56] = "atkDec";
        AuthorTokenKind.atkDec = 56;
        AuthorTokenKind._map[57] = "atkDot";
        AuthorTokenKind.atkDot = 57;
        AuthorTokenKind._map[58] = "atkScope";
        AuthorTokenKind.atkScope = 58;
        AuthorTokenKind._map[59] = "atkEllipsis";
        AuthorTokenKind.atkEllipsis = 59;
        AuthorTokenKind._map[60] = "atkBreak";
        AuthorTokenKind.atkBreak = 60;
        AuthorTokenKind._map[61] = "atkCase";
        AuthorTokenKind.atkCase = 61;
        AuthorTokenKind._map[62] = "atkCatch";
        AuthorTokenKind.atkCatch = 62;
        AuthorTokenKind._map[63] = "atkClass";
        AuthorTokenKind.atkClass = 63;
        AuthorTokenKind._map[64] = "atkConst";
        AuthorTokenKind.atkConst = 64;
        AuthorTokenKind._map[65] = "atkContinue";
        AuthorTokenKind.atkContinue = 65;
        AuthorTokenKind._map[66] = "atkDebugger";
        AuthorTokenKind.atkDebugger = 66;
        AuthorTokenKind._map[67] = "atkDefault";
        AuthorTokenKind.atkDefault = 67;
        AuthorTokenKind._map[68] = "atkDelete";
        AuthorTokenKind.atkDelete = 68;
        AuthorTokenKind._map[69] = "atkDo";
        AuthorTokenKind.atkDo = 69;
        AuthorTokenKind._map[70] = "atkElse";
        AuthorTokenKind.atkElse = 70;
        AuthorTokenKind._map[71] = "atkEnum";
        AuthorTokenKind.atkEnum = 71;
        AuthorTokenKind._map[72] = "atkExport";
        AuthorTokenKind.atkExport = 72;
        AuthorTokenKind._map[73] = "atkExtends";
        AuthorTokenKind.atkExtends = 73;
        AuthorTokenKind._map[74] = "atkFalse";
        AuthorTokenKind.atkFalse = 74;
        AuthorTokenKind._map[75] = "atkFinally";
        AuthorTokenKind.atkFinally = 75;
        AuthorTokenKind._map[76] = "atkFor";
        AuthorTokenKind.atkFor = 76;
        AuthorTokenKind._map[77] = "atkFunction";
        AuthorTokenKind.atkFunction = 77;
        AuthorTokenKind._map[78] = "atkIf";
        AuthorTokenKind.atkIf = 78;
        AuthorTokenKind._map[79] = "atkImport";
        AuthorTokenKind.atkImport = 79;
        AuthorTokenKind._map[80] = "atkIn";
        AuthorTokenKind.atkIn = 80;
        AuthorTokenKind._map[81] = "atkInstanceof";
        AuthorTokenKind.atkInstanceof = 81;
        AuthorTokenKind._map[82] = "atkNew";
        AuthorTokenKind.atkNew = 82;
        AuthorTokenKind._map[83] = "atkNull";
        AuthorTokenKind.atkNull = 83;
        AuthorTokenKind._map[84] = "atkReturn";
        AuthorTokenKind.atkReturn = 84;
        AuthorTokenKind._map[85] = "atkSuper";
        AuthorTokenKind.atkSuper = 85;
        AuthorTokenKind._map[86] = "atkSwitch";
        AuthorTokenKind.atkSwitch = 86;
        AuthorTokenKind._map[87] = "atkThis";
        AuthorTokenKind.atkThis = 87;
        AuthorTokenKind._map[88] = "atkThrow";
        AuthorTokenKind.atkThrow = 88;
        AuthorTokenKind._map[89] = "atkTrue";
        AuthorTokenKind.atkTrue = 89;
        AuthorTokenKind._map[90] = "atkTry";
        AuthorTokenKind.atkTry = 90;
        AuthorTokenKind._map[91] = "atkTypeof";
        AuthorTokenKind.atkTypeof = 91;
        AuthorTokenKind._map[92] = "atkVar";
        AuthorTokenKind.atkVar = 92;
        AuthorTokenKind._map[93] = "atkVoid";
        AuthorTokenKind.atkVoid = 93;
        AuthorTokenKind._map[94] = "atkWhile";
        AuthorTokenKind.atkWhile = 94;
        AuthorTokenKind._map[95] = "atkWith";
        AuthorTokenKind.atkWith = 95;
        AuthorTokenKind._map[96] = "atkConstructor";
        AuthorTokenKind.atkConstructor = 96;
        AuthorTokenKind._map[97] = "atkDeclare";
        AuthorTokenKind.atkDeclare = 97;
        AuthorTokenKind._map[98] = "atkModule";
        AuthorTokenKind.atkModule = 98;
        AuthorTokenKind._map[99] = "atkGet";
        AuthorTokenKind.atkGet = 99;
        AuthorTokenKind._map[100] = "atkSet";
        AuthorTokenKind.atkSet = 100;
        AuthorTokenKind._map[101] = "atkImplements";
        AuthorTokenKind.atkImplements = 101;
        AuthorTokenKind._map[102] = "atkInterface";
        AuthorTokenKind.atkInterface = 102;
        AuthorTokenKind._map[103] = "atkLet";
        AuthorTokenKind.atkLet = 103;
        AuthorTokenKind._map[104] = "atkPackage";
        AuthorTokenKind.atkPackage = 104;
        AuthorTokenKind._map[105] = "atkPrivate";
        AuthorTokenKind.atkPrivate = 105;
        AuthorTokenKind._map[106] = "atkProtected";
        AuthorTokenKind.atkProtected = 106;
        AuthorTokenKind._map[107] = "atkPublic";
        AuthorTokenKind.atkPublic = 107;
        AuthorTokenKind._map[108] = "atkStatic";
        AuthorTokenKind.atkStatic = 108;
        AuthorTokenKind._map[109] = "atkYield";
        AuthorTokenKind.atkYield = 109;
        AuthorTokenKind._map[110] = "Length";
        AuthorTokenKind.Length = 110;
    })(Formatting.AuthorTokenKind || (Formatting.AuthorTokenKind = {}));
    var AuthorTokenKind = Formatting.AuthorTokenKind;
    (function (AuthorParseNodeEdge) {
        AuthorParseNodeEdge._map = [];
        AuthorParseNodeEdge._map[0] = "apneNone";
        AuthorParseNodeEdge.apneNone = 0;
        AuthorParseNodeEdge._map[1] = "apneOperand";
        AuthorParseNodeEdge.apneOperand = 1;
        AuthorParseNodeEdge._map[2] = "apneLeft";
        AuthorParseNodeEdge.apneLeft = 2;
        AuthorParseNodeEdge._map[3] = "apneRight";
        AuthorParseNodeEdge.apneRight = 3;
        AuthorParseNodeEdge._map[4] = "apneCondition";
        AuthorParseNodeEdge.apneCondition = 4;
        AuthorParseNodeEdge._map[5] = "apneThen";
        AuthorParseNodeEdge.apneThen = 5;
        AuthorParseNodeEdge._map[6] = "apneElse";
        AuthorParseNodeEdge.apneElse = 6;
        AuthorParseNodeEdge._map[7] = "apneInitialization";
        AuthorParseNodeEdge.apneInitialization = 7;
        AuthorParseNodeEdge._map[8] = "apneIncrement";
        AuthorParseNodeEdge.apneIncrement = 8;
        AuthorParseNodeEdge._map[9] = "apneBody";
        AuthorParseNodeEdge.apneBody = 9;
        AuthorParseNodeEdge._map[10] = "apneBlockBody";
        AuthorParseNodeEdge.apneBlockBody = 10;
        AuthorParseNodeEdge._map[11] = "apneValue";
        AuthorParseNodeEdge.apneValue = 11;
        AuthorParseNodeEdge._map[12] = "apneTarget";
        AuthorParseNodeEdge.apneTarget = 12;
        AuthorParseNodeEdge._map[13] = "apneArgument";
        AuthorParseNodeEdge.apneArgument = 13;
        AuthorParseNodeEdge._map[14] = "apneArguments";
        AuthorParseNodeEdge.apneArguments = 14;
        AuthorParseNodeEdge._map[15] = "apneMembers";
        AuthorParseNodeEdge.apneMembers = 15;
        AuthorParseNodeEdge._map[16] = "apneVariable";
        AuthorParseNodeEdge.apneVariable = 16;
        AuthorParseNodeEdge._map[17] = "apneObject";
        AuthorParseNodeEdge.apneObject = 17;
        AuthorParseNodeEdge._map[18] = "apneTry";
        AuthorParseNodeEdge.apneTry = 18;
        AuthorParseNodeEdge._map[19] = "apneCatch";
        AuthorParseNodeEdge.apneCatch = 19;
        AuthorParseNodeEdge._map[20] = "apneFinally";
        AuthorParseNodeEdge.apneFinally = 20;
        AuthorParseNodeEdge._map[21] = "apneCase";
        AuthorParseNodeEdge.apneCase = 21;
        AuthorParseNodeEdge._map[22] = "apneDefaultCase";
        AuthorParseNodeEdge.apneDefaultCase = 22;
        AuthorParseNodeEdge._map[23] = "apneElements";
        AuthorParseNodeEdge.apneElements = 23;
        AuthorParseNodeEdge._map[24] = "apneListItem";
        AuthorParseNodeEdge.apneListItem = 24;
        AuthorParseNodeEdge._map[25] = "apneMember";
        AuthorParseNodeEdge.apneMember = 25;
        AuthorParseNodeEdge._map[26] = "apneType";
        AuthorParseNodeEdge.apneType = 26;
    })(Formatting.AuthorParseNodeEdge || (Formatting.AuthorParseNodeEdge = {}));
    var AuthorParseNodeEdge = Formatting.AuthorParseNodeEdge;
    var AuthorParseNodeSet = (function () {
        function AuthorParseNodeSet(nodes) {
            this.nodes = nodes;
        }
        AuthorParseNodeSet.prototype.Count = function () {
            return this.nodes.length;
        };
        AuthorParseNodeSet.prototype.GetItems = function (startIndex, count) {
            if(startIndex == 0 && count == this.nodes.length) {
                return this.nodes;
            }
            throw new Error("Invalid call to GetItems");
        };
        return AuthorParseNodeSet;
    })();
    Formatting.AuthorParseNodeSet = AuthorParseNodeSet;    
    (function (AuthorParseNodeProperty) {
        AuthorParseNodeProperty._map = [];
        AuthorParseNodeProperty._map[0] = "apnpLCurlyMin";
        AuthorParseNodeProperty.apnpLCurlyMin = 0;
        AuthorParseNodeProperty._map[1] = "apnpRCurlyMin";
        AuthorParseNodeProperty.apnpRCurlyMin = 1;
        AuthorParseNodeProperty._map[2] = "apnpLParenMin";
        AuthorParseNodeProperty.apnpLParenMin = 2;
        AuthorParseNodeProperty._map[3] = "apnpRParenMin";
        AuthorParseNodeProperty.apnpRParenMin = 3;
        AuthorParseNodeProperty._map[4] = "apnpLBrackMin";
        AuthorParseNodeProperty.apnpLBrackMin = 4;
        AuthorParseNodeProperty._map[5] = "apnpRBrackMin";
        AuthorParseNodeProperty.apnpRBrackMin = 5;
        AuthorParseNodeProperty._map[6] = "apnpIdentifierMin";
        AuthorParseNodeProperty.apnpIdentifierMin = 6;
        AuthorParseNodeProperty._map[7] = "apnpFunctionKeywordMin";
        AuthorParseNodeProperty.apnpFunctionKeywordMin = 7;
    })(Formatting.AuthorParseNodeProperty || (Formatting.AuthorParseNodeProperty = {}));
    var AuthorParseNodeProperty = Formatting.AuthorParseNodeProperty;
    var AuthorTokenKindMap = (function () {
        function AuthorTokenKindMap() {
            this.tokenMap = [];
            this.init();
        }
        AuthorTokenKindMap.instance = null;
        AuthorTokenKindMap.getInstance = function getInstance() {
            if(AuthorTokenKindMap.instance === null) {
                AuthorTokenKindMap.instance = new AuthorTokenKindMap();
            }
            return AuthorTokenKindMap.instance;
        };
        AuthorTokenKindMap.prototype.init = function () {
            for(var i = 0, len = TypeScript.TokenID.Lim; i < len; i++) {
                this.tokenMap[i] = this.mapTokenID(i);
            }
        };
        AuthorTokenKindMap.prototype.getTokenKind = function (kind) {
            return this.tokenMap[kind];
        };
        AuthorTokenKindMap.prototype.mapTokenID = function (kind) {
            switch(kind) {
                case TypeScript.TokenID.Any:
                    return AuthorTokenKind.atkIdentifier;
                case TypeScript.TokenID.Bool:
                    return AuthorTokenKind.atkIdentifier;
                case TypeScript.TokenID.Break:
                    return AuthorTokenKind.atkBreak;
                case TypeScript.TokenID.Case:
                    return AuthorTokenKind.atkCase;
                case TypeScript.TokenID.Catch:
                    return AuthorTokenKind.atkCatch;
                case TypeScript.TokenID.Class:
                    return AuthorTokenKind.atkClass;
                case TypeScript.TokenID.Const:
                    return AuthorTokenKind.atkConst;
                case TypeScript.TokenID.Continue:
                    return AuthorTokenKind.atkContinue;
                case TypeScript.TokenID.Debugger:
                    return AuthorTokenKind.atkDebugger;
                case TypeScript.TokenID.Default:
                    return AuthorTokenKind.atkDefault;
                case TypeScript.TokenID.Delete:
                    return AuthorTokenKind.atkDelete;
                case TypeScript.TokenID.Do:
                    return AuthorTokenKind.atkDo;
                case TypeScript.TokenID.Else:
                    return AuthorTokenKind.atkElse;
                case TypeScript.TokenID.Enum:
                    return AuthorTokenKind.atkEnum;
                case TypeScript.TokenID.Export:
                    return AuthorTokenKind.atkExport;
                case TypeScript.TokenID.Extends:
                    return AuthorTokenKind.atkExtends;
                case TypeScript.TokenID.Declare:
                    return AuthorTokenKind.atkDeclare;
                case TypeScript.TokenID.False:
                    return AuthorTokenKind.atkFalse;
                case TypeScript.TokenID.Finally:
                    return AuthorTokenKind.atkFinally;
                case TypeScript.TokenID.For:
                    return AuthorTokenKind.atkFor;
                case TypeScript.TokenID.Constructor:
                    return AuthorTokenKind.atkConstructor;
                case TypeScript.TokenID.Function:
                    return AuthorTokenKind.atkFunction;
                case TypeScript.TokenID.Get:
                    return AuthorTokenKind.atkGet;
                case TypeScript.TokenID.If:
                    return AuthorTokenKind.atkIf;
                case TypeScript.TokenID.Implements:
                    return AuthorTokenKind.atkImplements;
                case TypeScript.TokenID.Import:
                    return AuthorTokenKind.atkImport;
                case TypeScript.TokenID.In:
                    return AuthorTokenKind.atkIn;
                case TypeScript.TokenID.InstanceOf:
                    return AuthorTokenKind.atkInstanceof;
                case TypeScript.TokenID.Interface:
                    return AuthorTokenKind.atkInterface;
                case TypeScript.TokenID.Let:
                    return AuthorTokenKind.atkLet;
                case TypeScript.TokenID.Module:
                    return AuthorTokenKind.atkModule;
                case TypeScript.TokenID.New:
                    return AuthorTokenKind.atkNew;
                case TypeScript.TokenID.Number:
                    return AuthorTokenKind.atkIdentifier;
                case TypeScript.TokenID.Null:
                    return AuthorTokenKind.atkNull;
                case TypeScript.TokenID.Package:
                    return AuthorTokenKind.atkPackage;
                case TypeScript.TokenID.Private:
                    return AuthorTokenKind.atkPrivate;
                case TypeScript.TokenID.Protected:
                    return AuthorTokenKind.atkProtected;
                case TypeScript.TokenID.Public:
                    return AuthorTokenKind.atkPublic;
                case TypeScript.TokenID.With:
                    return AuthorTokenKind.atkWith;
                case TypeScript.TokenID.Return:
                    return AuthorTokenKind.atkReturn;
                case TypeScript.TokenID.Set:
                    return AuthorTokenKind.atkSet;
                case TypeScript.TokenID.Static:
                    return AuthorTokenKind.atkStatic;
                case TypeScript.TokenID.String:
                    return AuthorTokenKind.atkIdentifier;
                case TypeScript.TokenID.Super:
                    return AuthorTokenKind.atkSuper;
                case TypeScript.TokenID.Switch:
                    return AuthorTokenKind.atkSwitch;
                case TypeScript.TokenID.This:
                    return AuthorTokenKind.atkThis;
                case TypeScript.TokenID.Throw:
                    return AuthorTokenKind.atkThrow;
                case TypeScript.TokenID.True:
                    return AuthorTokenKind.atkTrue;
                case TypeScript.TokenID.Try:
                    return AuthorTokenKind.atkTry;
                case TypeScript.TokenID.TypeOf:
                    return AuthorTokenKind.atkTypeof;
                case TypeScript.TokenID.Var:
                    return AuthorTokenKind.atkVar;
                case TypeScript.TokenID.Void:
                    return AuthorTokenKind.atkVoid;
                case TypeScript.TokenID.While:
                    return AuthorTokenKind.atkWhile;
                case TypeScript.TokenID.Yield:
                    return AuthorTokenKind.atkYield;
                case TypeScript.TokenID.Semicolon:
                    return AuthorTokenKind.atkSColon;
                case TypeScript.TokenID.OpenParen:
                    return AuthorTokenKind.atkLParen;
                case TypeScript.TokenID.CloseParen:
                    return AuthorTokenKind.atkRParen;
                case TypeScript.TokenID.OpenBracket:
                    return AuthorTokenKind.atkLBrack;
                case TypeScript.TokenID.CloseBracket:
                    return AuthorTokenKind.atkRBrack;
                case TypeScript.TokenID.OpenBrace:
                    return AuthorTokenKind.atkLCurly;
                case TypeScript.TokenID.CloseBrace:
                    return AuthorTokenKind.atkRCurly;
                case TypeScript.TokenID.Comma:
                    return AuthorTokenKind.atkComma;
                case TypeScript.TokenID.Equals:
                    return AuthorTokenKind.atkAsg;
                case TypeScript.TokenID.PlusEquals:
                    return AuthorTokenKind.atkAsgAdd;
                case TypeScript.TokenID.MinusEquals:
                    return AuthorTokenKind.atkAsgSub;
                case TypeScript.TokenID.AsteriskEquals:
                    return AuthorTokenKind.atkAsgMul;
                case TypeScript.TokenID.SlashEquals:
                    return AuthorTokenKind.atkAsgDiv;
                case TypeScript.TokenID.PercentEquals:
                    return AuthorTokenKind.atkAsgMod;
                case TypeScript.TokenID.AmpersandEquals:
                    return AuthorTokenKind.atkAsgAnd;
                case TypeScript.TokenID.CaretEquals:
                    return AuthorTokenKind.atkAsgXor;
                case TypeScript.TokenID.BarEquals:
                    return AuthorTokenKind.atkAsgOr;
                case TypeScript.TokenID.LessThanLessThanEquals:
                    return AuthorTokenKind.atkAsgLsh;
                case TypeScript.TokenID.GreaterThanGreaterThanEquals:
                    return AuthorTokenKind.atkAsgRsh;
                case TypeScript.TokenID.GreaterThanGreaterThanGreaterThanEquals:
                    return AuthorTokenKind.atkAsgRs2;
                case TypeScript.TokenID.Question:
                    return AuthorTokenKind.atkQMark;
                case TypeScript.TokenID.Colon:
                    return AuthorTokenKind.atkColon;
                case TypeScript.TokenID.BarBar:
                    return AuthorTokenKind.atkLogOr;
                case TypeScript.TokenID.AmpersandAmpersand:
                    return AuthorTokenKind.atkLogAnd;
                case TypeScript.TokenID.Bar:
                    return AuthorTokenKind.atkOr;
                case TypeScript.TokenID.Caret:
                    return AuthorTokenKind.atkXor;
                case TypeScript.TokenID.And:
                    return AuthorTokenKind.atkAnd;
                case TypeScript.TokenID.EqualsEquals:
                    return AuthorTokenKind.atkEQ;
                case TypeScript.TokenID.ExclamationEquals:
                    return AuthorTokenKind.atkNE;
                case TypeScript.TokenID.EqualsEqualsEquals:
                    return AuthorTokenKind.atkEqv;
                case TypeScript.TokenID.ExclamationEqualsEquals:
                    return AuthorTokenKind.atkNEqv;
                case TypeScript.TokenID.LessThan:
                    return AuthorTokenKind.atkLT;
                case TypeScript.TokenID.LessThanEquals:
                    return AuthorTokenKind.atkLE;
                case TypeScript.TokenID.GreaterThan:
                    return AuthorTokenKind.atkGT;
                case TypeScript.TokenID.GreaterThanEquals:
                    return AuthorTokenKind.atkGE;
                case TypeScript.TokenID.LessThanLessThan:
                    return AuthorTokenKind.atkLsh;
                case TypeScript.TokenID.GreaterThanGreaterThan:
                    return AuthorTokenKind.atkRsh;
                case TypeScript.TokenID.GreaterThanGreaterThanGreaterThan:
                    return AuthorTokenKind.atkRs2;
                case TypeScript.TokenID.Plus:
                    return AuthorTokenKind.atkAdd;
                case TypeScript.TokenID.Minus:
                    return AuthorTokenKind.atkSub;
                case TypeScript.TokenID.Asterisk:
                    return AuthorTokenKind.atkMult;
                case TypeScript.TokenID.Slash:
                    return AuthorTokenKind.atkDiv;
                case TypeScript.TokenID.Percent:
                    return AuthorTokenKind.atkPct;
                case TypeScript.TokenID.Tilde:
                    return AuthorTokenKind.atkTilde;
                case TypeScript.TokenID.Exclamation:
                    return AuthorTokenKind.atkBang;
                case TypeScript.TokenID.PlusPlus:
                    return AuthorTokenKind.atkInc;
                case TypeScript.TokenID.MinusMinus:
                    return AuthorTokenKind.atkDec;
                case TypeScript.TokenID.Dot:
                    return AuthorTokenKind.atkDot;
                case TypeScript.TokenID.DotDotDot:
                    return AuthorTokenKind.atkEllipsis;
                case TypeScript.TokenID.Error:
                    return AuthorTokenKind.atkIdentifier;
                case TypeScript.TokenID.EndOfFile:
                    return AuthorTokenKind.atkEnd;
                case TypeScript.TokenID.EqualsGreaterThan:
                    return AuthorTokenKind.atkArrow;
                case TypeScript.TokenID.Identifier:
                    return AuthorTokenKind.atkIdentifier;
                case TypeScript.TokenID.StringLiteral:
                    return AuthorTokenKind.atkString;
                case TypeScript.TokenID.RegularExpressionLiteral:
                    return AuthorTokenKind.atkRegexp;
                case TypeScript.TokenID.NumberLiteral:
                    return AuthorTokenKind.atkNumber;
                case TypeScript.TokenID.Whitespace:
                    return AuthorTokenKind.atkIdentifier;
                case TypeScript.TokenID.Comment:
                    return AuthorTokenKind.atkComment;
                default:
                    throw new Error("Invalid token kind:" + kind + " (" + (TypeScript.TokenID)._map[kind] + ")");
            }
        };
        return AuthorTokenKindMap;
    })();
    Formatting.AuthorTokenKindMap = AuthorTokenKindMap;    
    var Debug = (function () {
        function Debug() { }
        Debug.Assert = function Assert(cond, msg) {
            if(!cond) {
                throw new Error("assertion failure: " + msg);
            }
        };
        Debug.Fail = function Fail(msg) {
            Debug.Assert(false, msg);
        };
        return Debug;
    })();
    Formatting.Debug = Debug;    
    var HashSet_int = (function () {
        function HashSet_int() {
            this.items = [];
        }
        HashSet_int.prototype.Contains = function (item) {
            return this.items[item] !== undefined;
        };
        HashSet_int.prototype.Add = function (item) {
            this.items[item] = item;
        };
        return HashSet_int;
    })();
    Formatting.HashSet_int = HashSet_int;    
    var Dictionary_int_List_IndentationEditInfo = (function () {
        function Dictionary_int_List_IndentationEditInfo() {
            this.items = [];
        }
        Dictionary_int_List_IndentationEditInfo.prototype.GetValue = function (key) {
            var result = this.items[key];
            return result === undefined ? null : result;
        };
        Dictionary_int_List_IndentationEditInfo.prototype.Add = function (key, value) {
            this.items[key] = value;
        };
        return Dictionary_int_List_IndentationEditInfo;
    })();
    Formatting.Dictionary_int_List_IndentationEditInfo = Dictionary_int_List_IndentationEditInfo;    
    var Dictionary_int_int = (function () {
        function Dictionary_int_int() {
            this.items = [];
        }
        Dictionary_int_int.prototype.GetValue = function (key) {
            var result = this.items[key];
            return result === undefined ? null : result;
        };
        Dictionary_int_int.prototype.Add = function (key, value) {
            this.items[key] = value;
        };
        return Dictionary_int_int;
    })();
    Formatting.Dictionary_int_int = Dictionary_int_int;    
    var Math = (function () {
        function Math() { }
        Math.Max = function Max(x, y) {
            return (x > y ? x : y);
        };
        Math.Min = function Min(x, y) {
            return (x < y ? x : y);
        };
        return Math;
    })();
    Formatting.Math = Math;    
    var StringUtils = (function () {
        function StringUtils() { }
        StringUtils.IndexOf = function IndexOf(value, search, startIndex) {
            if (typeof startIndex === "undefined") { startIndex = 0; }
            return value.indexOf(search, startIndex);
        };
        StringUtils.Equals = function Equals(x, y) {
            return x == y;
        };
        StringUtils.IsNullOrEmpty = function IsNullOrEmpty(x) {
            return x === null || x === "";
        };
        StringUtils.IsWhiteSpace = function IsWhiteSpace(charCode) {
            return EditorUtilities.IsWhitespace(charCode);
        };
        StringUtils.create = function create(value, count) {
            var result = "";
            for(var i = 0; i < count; i++) {
                result += value;
            }
            return result;
        };
        StringUtils.foreach = function foreach(x, action) {
            for(var i = 0; i < x.length; i++) {
                action(x.charAt(i));
            }
        };
        return StringUtils;
    })();
    Formatting.StringUtils = StringUtils;    
    var EditorUtilities = (function () {
        function EditorUtilities() { }
        EditorUtilities.IsWhitespace = function IsWhitespace(charCode) {
            switch(charCode) {
                case 0x0009:
                case 0x000b:
                case 0x000c:
                case 0xfeff:
                case 0x0020:
                case 0x00a0:
                case 0x1680:
                case 0x2000:
                case 0x2001:
                case 0x2002:
                case 0x2003:
                case 0x2004:
                case 0x2005:
                case 0x2006:
                case 0x2007:
                case 0x2008:
                case 0x2009:
                case 0x200a:
                case 0x202f:
                case 0x200b:
                case 0x3000:
                case 0x180e:
                case 0x205f:
                    return true;
                default:
                    return false;
            }
        };
        return EditorUtilities;
    })();
    Formatting.EditorUtilities = EditorUtilities;    
    function getTokensInSpan(logger, scriptSyntaxAST, tokenKindMap, span) {
        var tokens = new List_TokenSpan();
        var tokenStream = scriptSyntaxAST.getTokenStream(span.startPosition());
        while(tokenStream.moveNext()) {
            if(logger.information()) {
                var text = "token: " + (TypeScript.TokenID)._map[tokenStream.tokenId()] + " - startPos=" + tokenStream.tokenStartPos() + ", pos=" + tokenStream.tokenEndPos();
                logger.log(text);
            }
            var endPos = tokenStream.tokenEndPos();
            if(endPos < span.startPosition()) {
                continue;
            }
            var startPos = tokenStream.tokenStartPos();
            if(startPos > span.endPosition()) {
                break;
            }
            var kind = tokenKindMap.getTokenKind(tokenStream.tokenId());
            var tokenSpan = new Formatting.TokenSpan(kind, tokenStream.tokenId(), new SnapshotSpan(span.snapshot, Span.FromBounds(startPos, endPos)));
            tokens.add(tokenSpan);
        }
        logger.log("GetTokens([" + span.startPosition() + "," + span.endPosition() + "]): returned " + tokens.count() + " tokens from source text offset " + tokenStream.sourceTextOffset());
        return tokens;
    }
    Formatting.getTokensInSpan = getTokensInSpan;
})(Formatting || (Formatting = {}));
