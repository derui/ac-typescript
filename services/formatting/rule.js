var Formatting;
(function (Formatting) {
    var Rule = (function () {
        function Rule(Descriptor, Operation, Flag) {
            if (typeof Flag === "undefined") { Flag = Formatting.RuleFlags.None; }
            this.Descriptor = Descriptor;
            this.Operation = Operation;
            this.Flag = Flag;
        }
        Rule.prototype.toString = function () {
            return "[desc=" + this.Descriptor + "," + "operation=" + this.Operation + "," + "flag=" + this.Flag + "]";
        };
        return Rule;
    })();
    Formatting.Rule = Rule;    
})(Formatting || (Formatting = {}));
