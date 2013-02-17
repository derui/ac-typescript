var Formatting;
(function (Formatting) {
    var RulesProvider = (function () {
        function RulesProvider(logger) {
            this.logger = logger;
            this.globalRules = new Formatting.Rules();
        }
        RulesProvider.prototype.getRuleName = function (rule) {
            return this.globalRules.getRuleName(rule);
        };
        RulesProvider.prototype.getRuleByName = function (name) {
            return this.globalRules[name];
        };
        RulesProvider.prototype.setActiveRules = function (staticList) {
            this.activeRules = staticList;
            this.rulesMap = Formatting.RulesMap.create(this.activeRules);
        };
        RulesProvider.prototype.getActiveRules = function () {
            return this.activeRules;
        };
        RulesProvider.prototype.getRulesMap = function () {
            return this.rulesMap;
        };
        RulesProvider.prototype.ensureUptodate = function (options) {
            var _this = this;
            if(this.options == null || !Services.compareDataObjects(this.options, options)) {
                var activeRules = TypeScript.timeFunction(this.logger, "RulesProvider: createActiveRules()", function () {
                    return _this.createActiveRules(options);
                });
                var rulesMap = TypeScript.timeFunction(this.logger, "RulesProvider: RulesMap.create()", function () {
                    return Formatting.RulesMap.create(activeRules);
                });
                this.activeRules = activeRules;
                this.rulesMap = rulesMap;
                this.options = options;
            }
        };
        RulesProvider.prototype.createActiveRules = function (options) {
            var rules = new Formatting.List_Rule();
            rules.AddRange(this.globalRules.HighPriorityCommonRules);
            if(options.InsertSpaceAfterCommaDelimiter) {
                rules.Add(this.globalRules.SpaceAfterComma);
            } else {
                rules.Add(this.globalRules.NoSpaceAfterComma);
            }
            if(options.InsertSpaceAfterFunctionKeywordForAnonymousFunctions) {
                rules.Add(this.globalRules.SpaceAfterAnonymousFunctionKeyword);
            } else {
                rules.Add(this.globalRules.NoSpaceAfterAnonymousFunctionKeyword);
            }
            if(options.InsertSpaceAfterKeywordsInControlFlowStatements) {
                rules.Add(this.globalRules.SpaceAfterKeywordInControl);
            } else {
                rules.Add(this.globalRules.NoSpaceAfterKeywordInControl);
            }
            if(options.InsertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis) {
                rules.Add(this.globalRules.SpaceAfterOpenParen);
                rules.Add(this.globalRules.SpaceBeforeCloseParen);
                rules.Add(this.globalRules.NoSpaceBetweenParens);
            } else {
                rules.Add(this.globalRules.NoSpaceAfterOpenParen);
                rules.Add(this.globalRules.NoSpaceBeforeCloseParen);
                rules.Add(this.globalRules.NoSpaceBetweenParens);
            }
            if(options.InsertSpaceAfterSemicolonInForStatements) {
                rules.Add(this.globalRules.SpaceAfterSemicolonInFor);
            } else {
                rules.Add(this.globalRules.NoSpaceAfterSemicolonInFor);
            }
            if(options.InsertSpaceBeforeAndAfterBinaryOperators) {
                rules.Add(this.globalRules.SpaceBeforeBinaryOperator);
                rules.Add(this.globalRules.SpaceAfterBinaryOperator);
            } else {
                rules.Add(this.globalRules.NoSpaceBeforeBinaryOperator);
                rules.Add(this.globalRules.NoSpaceAfterBinaryOperator);
            }
            if(options.PlaceOpenBraceOnNewLineForControlBlocks) {
                rules.Add(this.globalRules.NewLineBeforeOpenCurlyInControl);
            } else {
                rules.Add(this.globalRules.SpaceBeforeOpenCurlyInControl);
            }
            if(options.PlaceOpenBraceOnNewLineForFunctions) {
                rules.Add(this.globalRules.NewLineBeforeOpenCurlyInFunction);
                rules.Add(this.globalRules.NewLineBeforeOpenCurlyInTypeScriptDeclWithBlock);
            } else {
                rules.Add(this.globalRules.SpaceBeforeOpenCurlyInFunction);
                rules.Add(this.globalRules.SpaceBeforeOpenCurlyInTypeScriptDeclWithBlock);
            }
            rules.AddRange(this.globalRules.LowPriorityCommonRules);
            return rules;
        };
        return RulesProvider;
    })();
    Formatting.RulesProvider = RulesProvider;    
})(Formatting || (Formatting = {}));
