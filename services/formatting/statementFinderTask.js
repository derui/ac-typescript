var Formatting;
(function (Formatting) {
    var StatementFinderTask = (function () {
        function StatementFinderTask(logger, semicolonPoint, fileAuthoringProxy) {
            this.logger = logger;
            this.semicolonPoint = semicolonPoint;
            this.fileAuthoringProxy = fileAuthoringProxy;
            this.BlockSpan = null;
        }
        StatementFinderTask.prototype.Run = function () {
            var parseCursor = this.fileAuthoringProxy.GetASTCursor();
 {
                parseCursor.SeekToOffset(this.semicolonPoint.position, true);
                var nodes = null;
                var foundSubTree = false;
                while(!foundSubTree) {
                    var children = parseCursor.GetSubTree(1);
 {
                        if(children.Count() === 0) {
                            return;
                        }
                        nodes = children.GetItems(0, children.Count());
                        if(nodes.length === 2 && nodes[1].Details.Kind === Formatting.AuthorParseNodeKind.apnkList) {
                            parseCursor.MoveToChild(nodes[1].EdgeLabel, 0);
                        } else {
                            foundSubTree = true;
                        }
                    }
                }
                for(var i = nodes.length - 1; i >= 0; i--) {
                    if(nodes[i].Details.EndOffset <= this.semicolonPoint.position) {
                        var startPos = nodes[i].Details.StartOffset;
                        this.BlockSpan = new Formatting.Span(startPos, this.semicolonPoint.position - startPos + 1);
                        break;
                    }
                }
            }
        };
        return StatementFinderTask;
    })();
    Formatting.StatementFinderTask = StatementFinderTask;    
})(Formatting || (Formatting = {}));
