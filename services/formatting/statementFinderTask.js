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
                var startPos = -1;
                var node = parseCursor.SeekToOffset(this.semicolonPoint.position, true);
                while(node && node.Kind !== Formatting.AuthorParseNodeKind.apnkEmpty && node.nodeType !== TypeScript.NodeType.List) {
                    if((node.EndOffset - 1) === this.semicolonPoint.position) {
                        startPos = node.StartOffset;
                    }
                    node = parseCursor.MoveUp();
                }
                if(startPos !== -1) {
                    this.BlockSpan = new Formatting.Span(startPos, this.semicolonPoint.position - startPos + 1);
                }
            }
        };
        return StatementFinderTask;
    })();
    Formatting.StatementFinderTask = StatementFinderTask;    
})(Formatting || (Formatting = {}));
