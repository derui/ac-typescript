var Formatting;
(function (Formatting) {
    var ParseNodeExtensions = (function () {
        function ParseNodeExtensions() { }
        ParseNodeExtensions.GetChildren = function GetChildren(node) {
            if(node == null || node.children() == null) {
                return new Formatting.List_ParseNode();
            }
            return node.children();
        };
        ParseNodeExtensions.FindChildrenWithEdge = function FindChildrenWithEdge(node, edge) {
            var result = new Formatting.List_ParseNode();
            ParseNodeExtensions.GetChildren(node).foreach(function (item) {
                if(item.AuthorNode.EdgeLabel == edge) {
                    result.add(item);
                }
            });
            return result;
        };
        ParseNodeExtensions.FindChildWithEdge = function FindChildWithEdge(node, edge) {
            return Formatting.FirstOrDefault(ParseNodeExtensions.GetChildren(node).Where(function (c) {
                return c.AuthorNode.EdgeLabel == edge;
            }), function () {
                return true;
            });
        };
        ParseNodeExtensions.ForAllChildren = function ForAllChildren(node, action) {
            ParseNodeExtensions.GetChildren(node).foreach(action);
        };
        ParseNodeExtensions.comparer = function comparer(position, item) {
            return position - item.AuthorNode.Details.StartOffset;
        };
        ParseNodeExtensions.findNodeInsertionPivot = function findNodeInsertionPivot(nodes, startOffset) {
            if(nodes.count() == 0) {
                return 0;
            }
            return Formatting.BinarySearch(nodes, startOffset, ParseNodeExtensions.comparer);
        };
        ParseNodeExtensions.TryFindNodeIndexForStartOffset = function TryFindNodeIndexForStartOffset(nodes, startOffset) {
            var targetNodeIndex = -1;
            if(nodes.count() > 0) {
                var pivot = Formatting.BinarySearch(nodes, startOffset, ParseNodeExtensions.comparer);
                if(pivot < 0) {
                    pivot = ~pivot - 1;
                    targetNodeIndex = pivot;
                } else {
                    targetNodeIndex = pivot;
                }
            }
            return targetNodeIndex;
        };
        ParseNodeExtensions.TryFindNodeForSpan = function TryFindNodeForSpan(nodes, span) {
            var nodeIndex = ParseNodeExtensions.TryFindNodeIndexForStartOffset(nodes, span.start());
            if(nodeIndex >= 0 && nodeIndex < nodes.count()) {
                var node = nodes.get(nodeIndex);
                if(node.CoverSpan(span)) {
                    return node;
                }
            }
            return null;
        };
        ParseNodeExtensions.SetNodeSpan = function SetNodeSpan(node, newStartOffset, newEndOffset) {
            var authorParseNode = node.AuthorNode;
            if(newStartOffset != authorParseNode.Details.StartOffset || newEndOffset != authorParseNode.Details.EndOffset) {
                var newAuthorNode = new Formatting.AuthorParseNode();
                newAuthorNode.Details = new Formatting.AuthorParseNodeDetails();
                newAuthorNode.Details.StartOffset = newStartOffset;
                newAuthorNode.Details.EndOffset = newEndOffset;
                newAuthorNode.Details.Flags = authorParseNode.Details.Flags;
                newAuthorNode.Details.Kind = authorParseNode.Details.Kind;
                newAuthorNode.Details.nodeType = authorParseNode.Details.nodeType;
                newAuthorNode.Details.ast = authorParseNode.Details.ast;
                newAuthorNode.EdgeLabel = authorParseNode.EdgeLabel;
                newAuthorNode.Label = authorParseNode.Label;
                newAuthorNode.Level = authorParseNode.Level;
                newAuthorNode.Name = authorParseNode.Name;
                node.AuthorNode = newAuthorNode;
            }
        };
        return ParseNodeExtensions;
    })();
    Formatting.ParseNodeExtensions = ParseNodeExtensions;    
})(Formatting || (Formatting = {}));
