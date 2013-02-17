var Formatting;
(function (Formatting) {
    var IndentationInfo = (function () {
        function IndentationInfo(Prefix, Level) {
            if (typeof Prefix === "undefined") { Prefix = null; }
            if (typeof Level === "undefined") { Level = 0; }
            this.Prefix = Prefix;
            this.Level = Level;
        }
        return IndentationInfo;
    })();
    Formatting.IndentationInfo = IndentationInfo;    
})(Formatting || (Formatting = {}));
