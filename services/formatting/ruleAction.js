var Formatting;
(function (Formatting) {
    (function (RuleAction) {
        RuleAction._map = [];
        RuleAction._map[0] = "Ignore";
        RuleAction.Ignore = 0;
        RuleAction._map[1] = "Space";
        RuleAction.Space = 1;
        RuleAction._map[2] = "NewLine";
        RuleAction.NewLine = 2;
        RuleAction._map[3] = "Delete";
        RuleAction.Delete = 3;
    })(Formatting.RuleAction || (Formatting.RuleAction = {}));
    var RuleAction = Formatting.RuleAction;
})(Formatting || (Formatting = {}));
