var Formatting;
(function (Formatting) {
    var RuleDescriptor = (function () {
        function RuleDescriptor(LeftTokenRange, RightTokenRange) {
            this.LeftTokenRange = LeftTokenRange;
            this.RightTokenRange = RightTokenRange;
        }
        RuleDescriptor.prototype.toString = function () {
            return "[leftRange=" + this.LeftTokenRange + "," + "rightRange=" + this.RightTokenRange + "]";
        };
        RuleDescriptor.create1 = function create1(left, right) {
            return RuleDescriptor.create4(Formatting.Shared.TokenRange.FromToken(left), Formatting.Shared.TokenRange.FromToken(right));
        };
        RuleDescriptor.create2 = function create2(left, right) {
            return RuleDescriptor.create4(left, Formatting.Shared.TokenRange.FromToken(right));
        };
        RuleDescriptor.create3 = function create3(left, right) {
            return RuleDescriptor.create4(Formatting.Shared.TokenRange.FromToken(left), right);
        };
        RuleDescriptor.create4 = function create4(left, right) {
            return new RuleDescriptor(left, right);
        };
        return RuleDescriptor;
    })();
    Formatting.RuleDescriptor = RuleDescriptor;    
})(Formatting || (Formatting = {}));
