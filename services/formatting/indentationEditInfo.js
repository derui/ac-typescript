var Formatting;
(function (Formatting) {
    var IndentationEditInfo = (function () {
        function IndentationEditInfo(textEditInfo) {
            this.textEditInfo = textEditInfo;
            this.OrigIndentPosition = this.textEditInfo.Position;
        }
        IndentationEditInfo.prototype.Position = function () {
 {
                return this.textEditInfo.Position;
            }
        };
        IndentationEditInfo.prototype.Indentation = function () {
 {
                return this.textEditInfo.ReplaceWith;
            }
        };
        IndentationEditInfo.prototype.OrigIndentLength = function () {
 {
                return this.textEditInfo.Length;
            }
        };
        IndentationEditInfo.create1 = function create1(textEditInfo) {
            return new IndentationEditInfo(textEditInfo);
        };
        IndentationEditInfo.create2 = function create2(position, indentString, origPosition, origIndentLength) {
            var textEditInfo = new Formatting.TextEditInfo(position, origIndentLength, indentString);
            var result = new IndentationEditInfo(textEditInfo);
            result.OrigIndentPosition = origPosition;
            return result;
        };
        return IndentationEditInfo;
    })();
    Formatting.IndentationEditInfo = IndentationEditInfo;    
})(Formatting || (Formatting = {}));
