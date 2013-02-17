var Formatting;
(function (Formatting) {
    var MatchingBlockFinderTask = (function () {
        function MatchingBlockFinderTask(bracePoint, FileAuthoringProxy) {
            this.bracePoint = bracePoint;
            this.FileAuthoringProxy = FileAuthoringProxy;
        }
        MatchingBlockFinderTask.prototype.Run = function () {
            var parseCursor = this.FileAuthoringProxy.GetASTCursor();
            parseCursor.SeekToOffset(this.bracePoint.position, true);
            return this.FindMatchingBlockSpan(parseCursor);
        };
        MatchingBlockFinderTask.prototype.FindMatchingBlockSpan = function (parser) {
            var currentNode = parser.Current();
            while(currentNode.Kind != Formatting.AuthorParseNodeKind.apnkEmptyNode) {
                if(currentNode.ast != null) {
                    switch(currentNode.ast.nodeType) {
                        case TypeScript.NodeType.InterfaceDeclaration:
                        case TypeScript.NodeType.ClassDeclaration:
                        case TypeScript.NodeType.ModuleDeclaration:
                            return Formatting.Span.FromBounds(currentNode.ast.minChar, currentNode.ast.limChar);
                        case TypeScript.NodeType.ImportDeclaration:
                            return new Formatting.Span(currentNode.StartOffset, currentNode.EndOffset - currentNode.StartOffset);
                    }
                }
                switch(currentNode.Kind) {
                    case Formatting.AuthorParseNodeKind.apnkProg:
                        return null;
                    case Formatting.AuthorParseNodeKind.apnkVarDecl:
                    case Formatting.AuthorParseNodeKind.apnkSwitch:
                    case Formatting.AuthorParseNodeKind.apnkCase:
                    case Formatting.AuthorParseNodeKind.apnkDefaultCase:
                    case Formatting.AuthorParseNodeKind.apnkTry:
                    case Formatting.AuthorParseNodeKind.apnkCatch:
                    case Formatting.AuthorParseNodeKind.apnkFinally:
                    case Formatting.AuthorParseNodeKind.apnkIf:
                    case Formatting.AuthorParseNodeKind.apnkFor:
                    case Formatting.AuthorParseNodeKind.apnkForIn:
                    case Formatting.AuthorParseNodeKind.apnkWhile:
                    case Formatting.AuthorParseNodeKind.apnkDoWhile:
                    case Formatting.AuthorParseNodeKind.apnkWith:
                    case Formatting.AuthorParseNodeKind.apnkCall:
                    case Formatting.AuthorParseNodeKind.apnkReturn:
                    case Formatting.AuthorParseNodeKind.apnkAsg:
                    case Formatting.AuthorParseNodeKind.apnkAsgAdd:
                    case Formatting.AuthorParseNodeKind.apnkAsgSub:
                    case Formatting.AuthorParseNodeKind.apnkAsgMul:
                    case Formatting.AuthorParseNodeKind.apnkAsgDiv:
                    case Formatting.AuthorParseNodeKind.apnkAsgMod:
                    case Formatting.AuthorParseNodeKind.apnkAsgAnd:
                    case Formatting.AuthorParseNodeKind.apnkAsgXor:
                    case Formatting.AuthorParseNodeKind.apnkAsgOr:
                    case Formatting.AuthorParseNodeKind.apnkAsgLsh:
                    case Formatting.AuthorParseNodeKind.apnkAsgRsh:
                    case Formatting.AuthorParseNodeKind.apnkAsgRs2:
                        return new Formatting.Span(currentNode.StartOffset, currentNode.EndOffset - currentNode.StartOffset);
                    case Formatting.AuthorParseNodeKind.apnkBlock:
                        if((currentNode.Flags & Formatting.AuthorParseNodeFlags.apnfSyntheticNode) != Formatting.AuthorParseNodeFlags.apnfSyntheticNode) {
                            var parent = parser.Parent();
                            switch(parent.Kind) {
                                case Formatting.AuthorParseNodeKind.apnkBlock:
                                case Formatting.AuthorParseNodeKind.apnkList:
                                    return new Formatting.Span(currentNode.StartOffset, currentNode.EndOffset - currentNode.StartOffset);
                            }
                        }
                        currentNode = parser.MoveUp();
                        continue;
                    case Formatting.AuthorParseNodeKind.apnkFncDecl: {
                        var start = parser.GetNodeProperty(Formatting.AuthorParseNodeProperty.apnpFunctionKeywordMin);
                        start = (start == 0) ? currentNode.StartOffset : start;
                        var end = parser.GetNodeProperty(Formatting.AuthorParseNodeProperty.apnpRCurlyMin);
                        end = (end == 0) ? currentNode.EndOffset : end + 1;
                        return new Formatting.Span(start, end - start);
                    }
                    case Formatting.AuthorParseNodeKind.apnkObject: {
                        var start = parser.GetNodeProperty(Formatting.AuthorParseNodeProperty.apnpLCurlyMin);
                        start = (start == 0) ? currentNode.StartOffset : start;
                        var end = parser.GetNodeProperty(Formatting.AuthorParseNodeProperty.apnpRCurlyMin);
                        end = (end == 0) ? currentNode.EndOffset : end + 1;
                        return new Formatting.Span(start, end - start);
                    }
                    default:
                        currentNode = parser.MoveUp();
                        continue;
                }
            }
            return null;
        };
        return MatchingBlockFinderTask;
    })();
    Formatting.MatchingBlockFinderTask = MatchingBlockFinderTask;    
})(Formatting || (Formatting = {}));
