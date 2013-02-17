var Formatting;
(function (Formatting) {
    var ParseTree = (function () {
        function ParseTree(fileAuthoringProxy, span, tokens, onlySelfAndAncestors) {
            this.StartNodeSelf = null;
            this.StartNodePreviousSibling = null;
            if(tokens != null) {
                var firstToken = Formatting.FirstOrDefault(tokens, function (t) {
                    return t.Span.startPosition() >= span.start() && t.Token != Formatting.AuthorTokenKind.atkComment && t.Token != Formatting.AuthorTokenKind.atkSColon && t.Token != Formatting.AuthorTokenKind.atkComma;
                });
                if(firstToken != null) {
                    var firstTokenStart = firstToken.Span.Start;
                    var lastToken = Formatting.LastOrDefault(tokens, function (t) {
                        return t.Span.endPosition() <= span.end() && t.Token != Formatting.AuthorTokenKind.atkComment && t.Token != Formatting.AuthorTokenKind.atkSColon && t.Token != Formatting.AuthorTokenKind.atkComma;
                    });
                    if(lastToken != null) {
                        var lastTokenEnd = lastToken.Span.End;
                        if(firstTokenStart < lastTokenEnd) {
                            span = new Formatting.Span(firstTokenStart, lastTokenEnd - firstTokenStart);
                        }
                    }
                }
            }
            this.Initialize(fileAuthoringProxy, span, onlySelfAndAncestors);
        }
        ParseTree.FindCommonParentNode = function FindCommonParentNode(leftSpan, rightSpan, context) {
            if(context.CoverSpan(leftSpan) && context.CoverSpan(rightSpan)) {
                Formatting.Debug.Assert(leftSpan.start() <= rightSpan.start(), "left token should be before the right token");
                if(context.children() != null) {
                    var child = Formatting.ParseNodeExtensions.TryFindNodeForSpan(context.children(), leftSpan);
                    if(child != null && child.CoverSpan(rightSpan)) {
                        return ParseTree.FindCommonParentNode(leftSpan, rightSpan, child);
                    }
                }
                return context;
            } else {
                if(context.Parent == null) {
                    return context;
                } else {
                    return ParseTree.FindCommonParentNode(leftSpan, rightSpan, context.Parent);
                }
            }
        };
        ParseTree.prototype.Initialize = function (fileAuthoringProxy, span, onlySelfAndAncestors) {
            var astCursor = fileAuthoringProxy.GetASTCursor();
 {
                if(span.length() == 0) {
                    astCursor.SeekToOffset(span.start(), false);
                } else {
                    astCursor.MoveToEnclosingNode(span.start(), span.end());
                }
                var selfAndDescendantsNodes = new Formatting.List_ParseNode();
                var parseNodeSet = astCursor.GetSubTree(onlySelfAndAncestors ? 0 : -1);
 {
                    if(parseNodeSet.Count() > 0) {
                        var authorNodes = parseNodeSet.GetItems(0, parseNodeSet.Count());
                        if(authorNodes[0].Details.Kind != Formatting.AuthorParseNodeKind.apnkEndCode) {
                            var nodeEdge = astCursor.GetEdgeLabel();
                            if(nodeEdge != Formatting.AuthorParseNodeEdge.apneNone) {
                                var newAuthorNode = new Formatting.AuthorParseNode();
                                newAuthorNode.Level = 0;
                                newAuthorNode.Label = authorNodes[0].Label;
                                newAuthorNode.Name = authorNodes[0].Name;
                                newAuthorNode.Details = authorNodes[0].Details;
                                newAuthorNode.EdgeLabel = nodeEdge;
                                authorNodes[0] = newAuthorNode;
                            }
                        }
                        authorNodes.forEach(function (authorParseNode) {
                            if(authorParseNode.Details.Kind != Formatting.AuthorParseNodeKind.apnkEndCode) {
                                var node = new Formatting.ParseNode();
                                node.AuthorNode = authorParseNode;
                                selfAndDescendantsNodes.add(node);
                            }
                        });
                        ParseTree.AdjustNodeSpanIfNeeded(astCursor, selfAndDescendantsNodes.get(0));
                    }
                }
                this.StartNodeSelf = ParseTree.FindStartSelfNode(selfAndDescendantsNodes, span);
                var nodeLevel = 0;
                var ancestorNodes = new Formatting.List_ParseNode();
                var ancestorNodeDetails = astCursor.MoveUp();
                if(!onlySelfAndAncestors && this.StartNodeSelf != null) {
                    if(this.StartNodeSelf.AuthorNode.Level > 0) {
                        for(var i = selfAndDescendantsNodes.count() - 1; i >= 0; --i) {
                            var sibling = selfAndDescendantsNodes.get(i).AuthorNode;
                            if(sibling.Level == this.StartNodeSelf.AuthorNode.Level && ParseTree.IsSiblingEdge(sibling.EdgeLabel) && sibling.Details.EndOffset < this.StartNodeSelf.AuthorNode.Details.StartOffset) {
                                this.StartNodePreviousSibling = sibling;
                                break;
                            }
                        }
                    } else if(this.StartNodeSelf.AuthorNode.Level == 0) {
                        parseNodeSet = astCursor.GetSubTree(2);
 {
                            if(parseNodeSet.Count() > 0) {
                                var nodes = parseNodeSet.GetItems(0, parseNodeSet.Count());
                                var previousSibling = ParseTree.GetPreviousSibling(this.StartNodeSelf.AuthorNode, nodes);
                                if(previousSibling !== null) {
                                    this.StartNodePreviousSibling = previousSibling;
                                }
                            }
                        }
                    }
                }
                while(ancestorNodeDetails.Kind != Formatting.AuthorParseNodeKind.apnkEmptyNode) {
                    var nodeEdge = astCursor.GetEdgeLabel();
                    var node = new Formatting.ParseNode();
                    node.AuthorNode = new Formatting.AuthorParseNode();
                    node.AuthorNode.Details = ancestorNodeDetails;
                    node.AuthorNode.Level = --nodeLevel;
                    node.AuthorNode.EdgeLabel = nodeEdge;
                    ParseTree.AdjustNodeSpanIfNeeded(astCursor, node);
                    ancestorNodes.add(node);
                    ancestorNodeDetails = astCursor.MoveUp();
                }
                for(var i = 0; i < ancestorNodes.count(); i++) {
                    selfAndDescendantsNodes.insert(0, ancestorNodes.get(i));
                }
                this.Root = ParseTree.BuildTree(selfAndDescendantsNodes);
            }
        };
        ParseTree.GetPreviousSibling = function GetPreviousSibling(startNodeSelf, nodes) {
            var previousSibling = null;
            var siblingLevel = -1;
            var i = nodes.length - 1;
            for(; i > 0; i--) {
                if(nodes[i].Details.Equals(startNodeSelf.Details)) {
                    siblingLevel = nodes[i].Level;
                    break;
                }
            }
            for(; i > 0; i--) {
                var node = nodes[i];
                if(node.Level == siblingLevel && ParseTree.IsSiblingEdge(node.EdgeLabel) && node.Details.EndOffset < startNodeSelf.Details.StartOffset) {
                    previousSibling = node;
                    break;
                }
            }
            return previousSibling;
        };
        ParseTree.FindStartSelfNode = function FindStartSelfNode(selfAndDescendantsNodes, span) {
            var candidateNodes = selfAndDescendantsNodes.Where(function (node) {
                return node.AuthorNode.Details.StartOffset >= span.start() && node.AuthorNode.Details.StartOffset < span.end();
            });
            if(candidateNodes.count() == 0) {
                return Formatting.FirstOrDefault(selfAndDescendantsNodes, function () {
                    return true;
                });
            }
            return candidateNodes.get(0);
        };
        ParseTree.IsSiblingEdge = function IsSiblingEdge(edge) {
            return edge == Formatting.AuthorParseNodeEdge.apneArgument || edge == Formatting.AuthorParseNodeEdge.apneListItem || edge == Formatting.AuthorParseNodeEdge.apneMember;
        };
        ParseTree.BuildTree = function BuildTree(parseNodes) {
            var nodesEnumerator = parseNodes.GetEnumerator();
            if(!nodesEnumerator.MoveNext()) {
                return null;
            }
            var root = nodesEnumerator.Current();
            var lastNode = root;
            lastNode.Parent = null;
            var lastLevel = lastNode.AuthorNode.Level;
            while(nodesEnumerator.MoveNext()) {
                var currentNode = nodesEnumerator.Current();
                if(currentNode.AuthorNode.Level == lastLevel) {
                    currentNode.Parent = lastNode.Parent;
                    lastNode.Parent.addChildNode(currentNode);
                    lastNode = currentNode;
                } else if(currentNode.AuthorNode.Level > lastLevel) {
                    currentNode.Parent = lastNode;
                    lastNode.addChildNode(currentNode);
                    lastNode = currentNode;
                    lastLevel = currentNode.AuthorNode.Level;
                } else {
                    while(lastLevel > currentNode.AuthorNode.Level) {
                        lastNode = lastNode.Parent;
                        lastLevel--;
                    }
                    currentNode.Parent = lastNode.Parent;
                    lastNode.Parent.addChildNode(currentNode);
                    lastNode = currentNode;
                }
            }
            return root;
        };
        ParseTree.DumpTree = function DumpTree(logger, parseNode) {
            if(logger.information()) {
                var text = "";
                for(var i = -2; i <= parseNode.AuthorNode.Level; i++) {
                    text += " ";
                }
                text += parseNode.toString();
                logger.log(text);
                Formatting.ParseNodeExtensions.GetChildren(parseNode).foreach(function (child) {
                    ParseTree.DumpTree(logger, child);
                });
            }
        };
        ParseTree.AdjustNodeSpanIfNeeded = function AdjustNodeSpanIfNeeded(astCursor, node) {
            var propertyToGetStart = null;
            var propertyToGetEnd = null;
            var authorParseNode = node.AuthorNode;
            if(authorParseNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkObject) {
                propertyToGetStart = Formatting.AuthorParseNodeProperty.apnpLCurlyMin;
                propertyToGetEnd = Formatting.AuthorParseNodeProperty.apnpRCurlyMin;
            } else if(authorParseNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkArray) {
                propertyToGetStart = Formatting.AuthorParseNodeProperty.apnpLBrackMin;
                propertyToGetEnd = Formatting.AuthorParseNodeProperty.apnpRBrackMin;
            } else if(authorParseNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkFncDecl) {
                propertyToGetStart = Formatting.AuthorParseNodeProperty.apnpFunctionKeywordMin;
                propertyToGetEnd = Formatting.AuthorParseNodeProperty.apnpRCurlyMin;
            }
            if(propertyToGetStart != null && propertyToGetEnd != null) {
                var newStartOffset = astCursor.GetNodeProperty(propertyToGetStart);
                if(newStartOffset == 0) {
                    newStartOffset = node.AuthorNode.Details.StartOffset;
                }
                var newEndOffset = astCursor.GetNodeProperty(propertyToGetEnd);
                if(newEndOffset == 0) {
                    newEndOffset = node.AuthorNode.Details.EndOffset;
                } else {
                    newEndOffset += 1;
                }
                Formatting.ParseNodeExtensions.SetNodeSpan(node, newStartOffset, newEndOffset);
            }
        };
        return ParseTree;
    })();
    Formatting.ParseTree = ParseTree;    
})(Formatting || (Formatting = {}));
