var Formatting;
(function (Formatting) {
    var TextEditInfo = (function () {
        function TextEditInfo(position, length, replaceWith) {
            this.position = position;
            this.length = length;
            this.replaceWith = replaceWith;
            this.Position = this.position;
            this.Length = length;
            this.ReplaceWith = this.replaceWith;
        }
        return TextEditInfo;
    })();
    Formatting.TextEditInfo = TextEditInfo;    
})(Formatting || (Formatting = {}));
