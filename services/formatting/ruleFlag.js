var Formatting;
(function (Formatting) {
    (function (RuleFlags) {
        RuleFlags._map = [];
        RuleFlags._map[0] = "None";
        RuleFlags.None = 0;
        RuleFlags._map[1] = "CanDeleteNewLines";
        RuleFlags.CanDeleteNewLines = 1;
    })(Formatting.RuleFlags || (Formatting.RuleFlags = {}));
    var RuleFlags = Formatting.RuleFlags;
})(Formatting || (Formatting = {}));
