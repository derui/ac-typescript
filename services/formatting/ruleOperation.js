var Formatting;
(function (Formatting) {
    var RuleOperation = (function () {
        function RuleOperation() {
            this.Context = null;
            this.Action = null;
        }
        RuleOperation.prototype.toString = function () {
            return "[context=" + this.Context + "," + "action=" + this.Action + "]";
        };
        RuleOperation.create1 = function create1(action) {
            return RuleOperation.create2(Formatting.RuleOperationContext.Any, action);
        };
        RuleOperation.create2 = function create2(context, action) {
            var result = new RuleOperation();
            result.Context = context;
            result.Action = action;
            return result;
        };
        return RuleOperation;
    })();
    Formatting.RuleOperation = RuleOperation;    
})(Formatting || (Formatting = {}));
