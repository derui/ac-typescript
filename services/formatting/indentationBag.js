var Formatting;
(function (Formatting) {
    var IndentationBag = (function () {
        function IndentationBag(snapshot) {
            this.snapshot = snapshot;
            this.indentationEdits = new Formatting.Dictionary_int_List_IndentationEditInfo();
        }
        IndentationBag.prototype.AddIndent = function (edit) {
            var line = this.snapshot.GetLineNumberFromPosition(edit.Position());
            var lineIndents = this.indentationEdits.GetValue(line);
            if(lineIndents === null) {
                lineIndents = new Formatting.List_IndentationEditInfo();
                this.indentationEdits.Add(line, lineIndents);
            }
            lineIndents.add(edit);
        };
        IndentationBag.prototype.FindIndent = function (position) {
            var line = this.snapshot.GetLineNumberFromPosition(position);
            var lineIndents = this.indentationEdits.GetValue(line);
            if(lineIndents !== null) {
                for(var i = lineIndents.count() - 1; i >= 0; i--) {
                    if(position >= lineIndents.get(i).Position()) {
                        return lineIndents.get(i);
                    }
                }
            }
            return null;
        };
        return IndentationBag;
    })();
    Formatting.IndentationBag = IndentationBag;    
})(Formatting || (Formatting = {}));
