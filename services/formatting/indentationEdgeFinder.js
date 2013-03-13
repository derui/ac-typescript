var Formatting;
(function (Formatting) {
    var IndentationEdgeFinder = (function () {
        function IndentationEdgeFinder() { }
        IndentationEdgeFinder.FillIndentationLevels = function FillIndentationLevels(node) {
            var nodeStack = new Formatting.Stack_ParseNode();
            nodeStack.Push(node);
            while(nodeStack.Count() > 0) {
                IndentationEdgeFinder.FillIndentationLevels2(nodeStack.Pop(), nodeStack);
            }
        };
        IndentationEdgeFinder.FillBodyIndentation = function FillBodyIndentation(node, nextNodesToVisit) {
            node.IsIndentationOverrideEdge = true;
            node.ChildrenIndentationDelta = 1;
            Formatting.ParseNodeExtensions.ForAllChildren(Formatting.ParseNodeExtensions.FindChildWithEdge(node, Formatting.AuthorParseNodeEdge.apneBody), function (child) {
                if(child.AuthorNode.Details.Kind != Formatting.AuthorParseNodeKind.apnkEndCode) {
                    child.IndentationDelta = 1;
                }
                nextNodesToVisit.Push(child);
            });
        };
        IndentationEdgeFinder.FillIndentationLevels2 = function FillIndentationLevels2(node, nextNodesToVisit) {
            switch(node.AuthorNode.Details.nodeType) {
                case TypeScript.NodeType.ModuleDeclaration:
                case TypeScript.NodeType.ClassDeclaration:
                case TypeScript.NodeType.InterfaceDeclaration:
                    IndentationEdgeFinder.FillBodyIndentation(node, nextNodesToVisit);
                    Formatting.ParseNodeExtensions.ForAllChildren(node, function (child) {
                        nextNodesToVisit.Push(child);
                    });
                    return;
                case TypeScript.NodeType.ImportDeclaration:
                    node.ChildrenIndentationDelta = 1;
                    Formatting.ParseNodeExtensions.ForAllChildren(node, function (child) {
                        nextNodesToVisit.Push(child);
                    });
                    return;
            }
            switch(node.AuthorNode.Details.Kind) {
                case Formatting.AuthorParseNodeKind.apnkProg:
 {
                        node.IndentationDelta = 0;
                        node.ChildrenIndentationDelta = 0;
                        var child = Formatting.ParseNodeExtensions.FindChildWithEdge(node, Formatting.AuthorParseNodeEdge.apneBody);
                        Formatting.ParseNodeExtensions.ForAllChildren(child, function (child) {
                            child.IndentationDelta = 0;
                            child.ChildrenIndentationDelta = 0;
                            nextNodesToVisit.Push(child);
                        });
                    }
                    break;
                case Formatting.AuthorParseNodeKind.apnkBlock:
 {
                        if((node.AuthorNode.Details.Flags & Formatting.AuthorParseNodeFlags.apnfSyntheticNode) != Formatting.AuthorParseNodeFlags.apnfSyntheticNode) {
                            IndentationEdgeFinder.FillIndentationEdgesForBlock(node, 1);
                        } else {
                            Formatting.ParseNodeExtensions.ForAllChildren(node, function (child) {
                                IndentationEdgeFinder.FillIndentationLevels(child);
                            });
                            node.IndentationDelta = null;
                            node.ChildrenIndentationDelta = null;
                        }
                    }
                    break;
                case Formatting.AuthorParseNodeKind.apnkTryCatch:
                case Formatting.AuthorParseNodeKind.apnkTryFinally:
 {
                        Formatting.ParseNodeExtensions.ForAllChildren(node, function (child) {
                            IndentationEdgeFinder.FillIndentationLevels(child);
                        });
                        node.IndentationDelta = null;
                        node.ChildrenIndentationDelta = null;
                    }
                    break;
                case Formatting.AuthorParseNodeKind.apnkFncDecl:
 {
                        IndentationEdgeFinder.FillBodyIndentation(node, nextNodesToVisit);
                    }
                    break;
                case Formatting.AuthorParseNodeKind.apnkSwitch:
 {
                        node.ChildrenIndentationDelta = 1;
                        var col = Formatting.ParseNodeExtensions.GetChildren(node).Where(function (c) {
                            return c.AuthorNode.EdgeLabel == Formatting.AuthorParseNodeEdge.apneCase || c.AuthorNode.EdgeLabel == Formatting.AuthorParseNodeEdge.apneDefaultCase;
                        });
                        col.foreach(function (caseNode) {
                            caseNode.IndentationDelta = 1;
                            caseNode.ChildrenIndentationDelta = 1;
                            nextNodesToVisit.Push(caseNode);
                        });
                    }
                    break;
                case Formatting.AuthorParseNodeKind.apnkCase:
                case Formatting.AuthorParseNodeKind.apnkDefaultCase:
 {
                        var child = Formatting.ParseNodeExtensions.FindChildWithEdge(Formatting.ParseNodeExtensions.FindChildWithEdge(node, Formatting.AuthorParseNodeEdge.apneBody), Formatting.AuthorParseNodeEdge.apneBlockBody);
                        if(child != null) {
                            if(child.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkList) {
                                Formatting.ParseNodeExtensions.ForAllChildren(child, function (grandChild) {
                                    grandChild.IndentationDelta = 1;
                                    nextNodesToVisit.Push(grandChild);
                                });
                            } else if(child.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkBlock) {
                                child.IndentationDelta = 1;
                                IndentationEdgeFinder.FillIndentationEdgesForBlock(child, 1);
                            } else {
                                child.IndentationDelta = 1;
                                nextNodesToVisit.Push(child);
                            }
                        }
                    }
                    break;
                case Formatting.AuthorParseNodeKind.apnkIf:
 {
                        node.ChildrenIndentationDelta = 1;
                        var thenChild = Formatting.ParseNodeExtensions.FindChildWithEdge(node, Formatting.AuthorParseNodeEdge.apneThen);
                        IndentationEdgeFinder.FillIndentationEdgesForBlockOrNot(thenChild, 1);
                        var elseChild = Formatting.ParseNodeExtensions.FindChildWithEdge(node, Formatting.AuthorParseNodeEdge.apneElse);
                        IndentationEdgeFinder.FillIndentationEdgesForBlockOrNot(elseChild, 1);
                    }
                    break;
                case Formatting.AuthorParseNodeKind.apnkFor:
                case Formatting.AuthorParseNodeKind.apnkForIn:
                case Formatting.AuthorParseNodeKind.apnkWhile:
                case Formatting.AuthorParseNodeKind.apnkWith:
                case Formatting.AuthorParseNodeKind.apnkDoWhile:
 {
                        node.ChildrenIndentationDelta = 1;
                        var child = Formatting.ParseNodeExtensions.FindChildWithEdge(node, Formatting.AuthorParseNodeEdge.apneBody);
                        IndentationEdgeFinder.FillIndentationEdgesForBlockOrNot(child, 1);
                    }
                    break;
                case Formatting.AuthorParseNodeKind.apnkObject:
 {
                        node.IsIndentationOverrideEdge = true;
                        node.ChildrenIndentationDelta = 1;
                        var members = Formatting.ParseNodeExtensions.FindChildWithEdge(node, Formatting.AuthorParseNodeEdge.apneMembers);
                        if(members != null) {
                            if(members.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkList) {
                                Formatting.ParseNodeExtensions.ForAllChildren(members, function (grandChild) {
                                    grandChild.ChildrenIndentationDelta = 1;
                                    grandChild.IndentationDelta = 1;
                                    nextNodesToVisit.Push(grandChild);
                                });
                            } else {
                                members.ChildrenIndentationDelta = 1;
                                members.IndentationDelta = 1;
                                nextNodesToVisit.Push(members);
                            }
                        }
                    }
                    break;
                case Formatting.AuthorParseNodeKind.apnkArray:
 {
                        node.IsIndentationOverrideEdge = true;
                        node.ChildrenIndentationDelta = 1;
                        var elements = Formatting.ParseNodeExtensions.FindChildWithEdge(node, Formatting.AuthorParseNodeEdge.apneElements);
                        if(elements != null) {
                            if(elements.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkList) {
                                Formatting.ParseNodeExtensions.ForAllChildren(elements, function (grandChild) {
                                    grandChild.IsIndentationOverrideEdge = true;
                                    nextNodesToVisit.Push(grandChild);
                                });
                            } else {
                                elements.IsIndentationOverrideEdge = true;
                                nextNodesToVisit.Push(elements);
                            }
                        }
                    }
                    break;
                case Formatting.AuthorParseNodeKind.apnkTry:
                case Formatting.AuthorParseNodeKind.apnkCatch:
                case Formatting.AuthorParseNodeKind.apnkFinally:
 {
                        node.ChildrenIndentationDelta = 1;
                        var body = Formatting.ParseNodeExtensions.FindChildWithEdge(node, Formatting.AuthorParseNodeEdge.apneBody);
                        if(body == null || (body != null && body.AuthorNode.Details.Kind != Formatting.AuthorParseNodeKind.apnkTryCatch && body.AuthorNode.Details.Kind != Formatting.AuthorParseNodeKind.apnkTryFinally)) {
                            var parent = node.Parent;
                            while(parent != null) {
                                if((parent.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkTryCatch || parent.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkTryFinally) && parent.IndentationDelta != null) {
                                    node.IndentationDelta = parent.IndentationDelta;
                                    break;
                                }
                                parent = parent.Parent;
                            }
                        }
                        if(body != null && body.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkBlock) {
                            IndentationEdgeFinder.FillIndentationEdgesForBlock(body, 1);
                        } else {
                            Formatting.ParseNodeExtensions.ForAllChildren(node, function (child) {
                                nextNodesToVisit.Push(child);
                            });
                        }
                    }
                    break;
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
                case Formatting.AuthorParseNodeKind.apnkVarDecl:
                case Formatting.AuthorParseNodeKind.apnkVarDeclList:
                case Formatting.AuthorParseNodeKind.apnkCall:
                case Formatting.AuthorParseNodeKind.apnkNew:
                case Formatting.AuthorParseNodeKind.apnkDelete:
                case Formatting.AuthorParseNodeKind.apnkReturn:
                case Formatting.AuthorParseNodeKind.apnkDot:
                case Formatting.AuthorParseNodeKind.apnkIndex:
 {
                        if(node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkVarDecl && node.Parent != null && node.Parent.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkVarDeclList) {
                        } else {
                            node.ChildrenIndentationDelta = 1;
                        }
                        Formatting.ParseNodeExtensions.ForAllChildren(node, function (child) {
                            nextNodesToVisit.Push(child);
                        });
                    }
                    break;
                default:
 {
                        Formatting.ParseNodeExtensions.ForAllChildren(node, function (child) {
                            nextNodesToVisit.Push(child);
                        });
                    }
                    break;
            }
        };
        IndentationEdgeFinder.FillIndentationEdgesForBlockOrNot = function FillIndentationEdgesForBlockOrNot(node, childrenLevel) {
            if(node == null) {
                return;
            }
            node.ChildrenIndentationDelta = childrenLevel;
            if(node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkBlock) {
                IndentationEdgeFinder.FillIndentationEdgesForBlock(node, childrenLevel);
            } else {
                node.IndentationDelta = childrenLevel;
                IndentationEdgeFinder.FillIndentationLevels(node);
            }
        };
        IndentationEdgeFinder.FillIndentationEdgesForBlock = function FillIndentationEdgesForBlock(node, childrenLevel) {
            if(node == null) {
                return;
            }
            Formatting.Debug.Assert(node.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkBlock, "Expecting a node of kind block.");
            if((node.AuthorNode.Details.Flags & Formatting.AuthorParseNodeFlags.apnfSyntheticNode) != Formatting.AuthorParseNodeFlags.apnfSyntheticNode) {
                node.ChildrenIndentationDelta = childrenLevel;
            }
            var child = Formatting.FirstOrDefault(Formatting.ParseNodeExtensions.GetChildren(node), function () {
                return true;
            });
            if(child != null) {
                if(child.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkList) {
                    Formatting.ParseNodeExtensions.ForAllChildren(child, function (grandChild) {
                        grandChild.IndentationDelta = node.ChildrenIndentationDelta;
                        IndentationEdgeFinder.FillIndentationLevels(grandChild);
                    });
                } else {
                    child.IndentationDelta = node.ChildrenIndentationDelta;
                    IndentationEdgeFinder.FillIndentationLevels(child);
                }
            }
        };
        return IndentationEdgeFinder;
    })();
    Formatting.IndentationEdgeFinder = IndentationEdgeFinder;    
})(Formatting || (Formatting = {}));
