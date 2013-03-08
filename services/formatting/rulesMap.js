var Formatting;
(function (Formatting) {
    var RulesMap = (function () {
        function RulesMap() {
            this.map = [];
            this.mapRowLength = 0;
        }
        RulesMap.create = function create(rules) {
            var result = new RulesMap();
            result.Initialize(rules);
            return result;
        };
        RulesMap.prototype.Initialize = function (rules) {
            this.mapRowLength = Formatting.AuthorTokenKind.Length;
            this.map = new Array(this.mapRowLength * this.mapRowLength);
            var rulesBucketConstructionStateList = new Array(this.map.length);
            this.FillRules(rules, rulesBucketConstructionStateList);
            return this.map;
        };
        RulesMap.prototype.FillRules = function (rules, rulesBucketConstructionStateList) {
            var _this = this;
            rules.foreach(function (rule) {
                _this.FillRule(rule, rulesBucketConstructionStateList);
            });
        };
        RulesMap.prototype.GetRuleBucketIndex = function (row, column) {
            var rulesBucketIndex = (row * this.mapRowLength) + column;
            return rulesBucketIndex;
        };
        RulesMap.prototype.FillRule = function (rule, rulesBucketConstructionStateList) {
            var _this = this;
            var specificRule = rule.Descriptor.LeftTokenRange != Formatting.Shared.TokenRange.Any && rule.Descriptor.RightTokenRange != Formatting.Shared.TokenRange.Any;
            rule.Descriptor.LeftTokenRange.GetTokens().foreach(function (left) {
                rule.Descriptor.RightTokenRange.GetTokens().foreach(function (right) {
                    var rulesBucketIndex = _this.GetRuleBucketIndex(left, right);
                    var rulesBucket = _this.map[rulesBucketIndex];
                    if(rulesBucket == undefined) {
                        rulesBucket = _this.map[rulesBucketIndex] = new RulesBucket();
                    }
                    rulesBucket.AddRule(rule, specificRule, rulesBucketConstructionStateList, rulesBucketIndex);
                });
            });
        };
        RulesMap.prototype.GetRule = function (context) {
            var bucketIndex = this.GetRuleBucketIndex(context.tokenSpan.Token, context.nextTokenSpan.Token);
            var bucket = this.map[bucketIndex];
            if(bucket != null) {
                for(var i = 0, len = bucket.Rules().count(); i < len; i++) {
                    var rule = bucket.Rules().get(i);
                    if(rule.Operation.Context.InContext(context)) {
                        return rule;
                    }
                }
            }
            return null;
        };
        return RulesMap;
    })();
    Formatting.RulesMap = RulesMap;    
    var MaskBitSize = 5;
    var Mask = 0x1f;
    (function (Position) {
        Position._map = [];
        Position.IgnoreRulesSpecific = 0;
        Position.IgnoreRulesAny = MaskBitSize * 1;
        Position.ContextRulesSpecific = MaskBitSize * 2;
        Position.ContextRulesAny = MaskBitSize * 3;
        Position.NoContextRulesSpecific = MaskBitSize * 4;
        Position.NoContextRulesAny = MaskBitSize * 5;
    })(Formatting.Position || (Formatting.Position = {}));
    var Position = Formatting.Position;
    var RulesBucketConstructionState = (function () {
        function RulesBucketConstructionState() {
            this.rulesInsertionIndexBitmap = 0;
        }
        RulesBucketConstructionState.prototype.GetInsertionIndex = function (maskPosition) {
            var index = 0;
            var pos = 0;
            var indexBitmap = this.rulesInsertionIndexBitmap;
            while(pos <= maskPosition) {
                index += (indexBitmap & Mask);
                indexBitmap >>= MaskBitSize;
                pos += MaskBitSize;
            }
            return index;
        };
        RulesBucketConstructionState.prototype.IncreaseInsertionIndex = function (maskPosition) {
            var value = (this.rulesInsertionIndexBitmap >> maskPosition) & Mask;
            value++;
            Formatting.Debug.Assert((value & Mask) == value, "Adding more rules into the sub-bucket than allowed. Maximum allowed is 32 rules.");
            var temp = this.rulesInsertionIndexBitmap & ~(Mask << maskPosition);
            temp |= value << maskPosition;
            this.rulesInsertionIndexBitmap = temp;
        };
        return RulesBucketConstructionState;
    })();
    Formatting.RulesBucketConstructionState = RulesBucketConstructionState;    
    var RulesBucket = (function () {
        function RulesBucket() {
            this.rules = new Formatting.List_Rule();
        }
        RulesBucket.prototype.Rules = function () {
            return this.rules;
        };
        RulesBucket.prototype.AddRule = function (rule, specificTokens, constructionState, rulesBucketIndex) {
            var position;
            if(rule.Operation.Action == Formatting.RuleAction.Ignore) {
                position = specificTokens ? Position.IgnoreRulesSpecific : Position.IgnoreRulesAny;
            } else if(!rule.Operation.Context.IsAny()) {
                position = specificTokens ? Position.ContextRulesSpecific : Position.ContextRulesAny;
            } else {
                position = specificTokens ? Position.NoContextRulesSpecific : Position.NoContextRulesAny;
            }
            var state = constructionState[rulesBucketIndex];
            if(state === undefined) {
                state = constructionState[rulesBucketIndex] = new RulesBucketConstructionState();
            }
            var index = state.GetInsertionIndex(position);
            this.rules.insert(index, rule);
            state.IncreaseInsertionIndex(position);
        };
        return RulesBucket;
    })();
    Formatting.RulesBucket = RulesBucket;    
})(Formatting || (Formatting = {}));
