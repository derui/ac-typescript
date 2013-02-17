var Formatting;
(function (Formatting) {
    var Rules = (function () {
        function Rules() {
            this.IgnoreBeforeComment = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkComment), Formatting.RuleOperation.create1(Formatting.RuleAction.Ignore));
            this.IgnoreAfterLineComment = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkComment, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsFirstTokenLineCommentContext), Formatting.RuleAction.Ignore));
            this.NoSpaceBeforeSemicolon = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkSColon), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsNotForContext), Formatting.RuleAction.Delete));
            this.NoSpaceBeforeColon = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkColon), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsNotBinaryOpContext), Formatting.RuleAction.Delete));
            this.NoSpaceBeforeQMark = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkQMark), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsNotBinaryOpContext), Formatting.RuleAction.Delete));
            this.SpaceAfterColon = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkColon, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsNotBinaryOpContext), Formatting.RuleAction.Space));
            this.SpaceAfterQMark = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkQMark, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsNotBinaryOpContext), Formatting.RuleAction.Space));
            this.SpaceAfterSemicolon = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkSColon, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Space));
            this.NewLineAfterCloseCurly = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkRCurly, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsMultilineChildParentContext), Formatting.RuleAction.NewLine));
            this.SpaceAfterCloseCurly = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkRCurly, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineSiblingNodeContext), Formatting.RuleAction.Space));
            this.SpaceBetweenCloseCurlyAndElse = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkRCurly, Formatting.AuthorTokenKind.atkElse), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Space));
            this.SpaceBetweenCloseCurlyAndWhile = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkRCurly, Formatting.AuthorTokenKind.atkWhile), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Space));
            this.NoSpaceBeforeDot = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkDot), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceAfterDot = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkDot, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceBeforeOpenBracket = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkLBrack), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceAfterOpenBracket = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkLBrack, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceBeforeCloseBracket = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkRBrack), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceAfterCloseBracket = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkRBrack, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.SpaceAfterOpenCurly = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkLCurly, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSingleLineBlockContext), Formatting.RuleAction.Space));
            this.SpaceBeforeCloseCurly = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkRCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSingleLineBlockContext), Formatting.RuleAction.Space));
            this.NoSpaceBetweenEmptyCurlyBrackets = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkLCurly, Formatting.AuthorTokenKind.atkRCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsObjectContext), Formatting.RuleAction.Delete));
            this.NewLineAfterOpenCurlyInBlockContext = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkLCurly, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsMultilineBlockContext), Formatting.RuleAction.NewLine));
            this.NewLineBeforeCloseCurlyInFunctionOrControl = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkRCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsMultilineBlockContext), Formatting.RuleAction.NewLine));
            this.NoSpaceAfterUnaryPrefixOperator = new Formatting.Rule(Formatting.RuleDescriptor.create4(Formatting.Shared.TokenRange.UnaryPrefixOperators, Formatting.Shared.TokenRange.UnaryPrefixExpressions), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsNotBinaryOpContext), Formatting.RuleAction.Delete));
            this.NoSpaceAfterUnaryPreincrementOperator = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkInc, Formatting.Shared.TokenRange.UnaryPreincrementExpressions), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceAfterUnaryPredecrementOperator = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkDec, Formatting.Shared.TokenRange.UnaryPredecrementExpressions), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceBeforeUnaryPostincrementOperator = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.UnaryPostincrementExpressions, Formatting.AuthorTokenKind.atkInc), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceBeforeUnaryPostdecrementOperator = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.UnaryPostdecrementExpressions, Formatting.AuthorTokenKind.atkDec), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.SpaceAfterPostincrementWhenFollowedByAdd = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkInc, Formatting.AuthorTokenKind.atkAdd), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Space));
            this.SpaceAfterAddWhenFollowedByUnaryPlus = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkAdd, Formatting.AuthorTokenKind.atkAdd), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Space));
            this.SpaceAfterAddWhenFollowedByPreincrement = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkAdd, Formatting.AuthorTokenKind.atkInc), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Space));
            this.SpaceAfterPostdecrementWhenFollowedBySubtract = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkDec, Formatting.AuthorTokenKind.atkSub), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Space));
            this.SpaceAfterSubtractWhenFollowedByUnaryMinus = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkSub, Formatting.AuthorTokenKind.atkSub), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Space));
            this.SpaceAfterSubtractWhenFollowedByPredecrement = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkSub, Formatting.AuthorTokenKind.atkDec), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Space));
            this.NoSpaceBeforeComma = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkComma), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.SpaceAfterCertainKeywords = new Formatting.Rule(Formatting.RuleDescriptor.create4(Formatting.Shared.TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkVar, 
                Formatting.AuthorTokenKind.atkThrow, 
                Formatting.AuthorTokenKind.atkNew, 
                Formatting.AuthorTokenKind.atkDelete, 
                Formatting.AuthorTokenKind.atkReturn, 
                Formatting.AuthorTokenKind.atkVoid, 
                Formatting.AuthorTokenKind.atkTypeof
            ]), Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Space));
            this.NoSpaceBeforeOpenParenInFuncCall = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkLParen), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsFunctionCallOrNewContext), Formatting.RuleAction.Delete));
            this.SpaceAfterFunctionInFuncDecl = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkFunction, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsFunctionDeclContext), Formatting.RuleAction.Space));
            this.NoSpaceBeforeOpenParenInFuncDecl = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkLParen), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsFunctionOrGetSetDeclContext), Formatting.RuleAction.Delete));
            this.SpaceBetweenStatements = new Formatting.Rule(Formatting.RuleDescriptor.create4(Formatting.Shared.TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkRParen, 
                Formatting.AuthorTokenKind.atkDo, 
                Formatting.AuthorTokenKind.atkElse, 
                Formatting.AuthorTokenKind.atkCase
            ]), Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsNotForContext), Formatting.RuleAction.Space));
            this.SpaceAfterTryFinally = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkTry, 
                Formatting.AuthorTokenKind.atkFinally
            ]), Formatting.AuthorTokenKind.atkLCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Space));
            this.SpaceAfterGetSetInMember = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkIdentifier, Formatting.AuthorTokenKind.atkIdentifier), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsGetSetMemberContext), Formatting.RuleAction.Space));
            this.SpaceBeforeBinaryKeywordOperator = new Formatting.Rule(Formatting.RuleDescriptor.create4(Formatting.Shared.TokenRange.Any, Formatting.Shared.TokenRange.BinaryKeywordOperators), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Space));
            this.SpaceAfterBinaryKeywordOperator = new Formatting.Rule(Formatting.RuleDescriptor.create4(Formatting.Shared.TokenRange.BinaryKeywordOperators, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Space));
            this.NoSpaceAfterConstructor = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkConstructor, Formatting.AuthorTokenKind.atkLParen), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceAfterModuleImport = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkModule, Formatting.AuthorTokenKind.atkLParen), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.SpaceAfterCertainTypeScriptKeywords = new Formatting.Rule(Formatting.RuleDescriptor.create4(Formatting.Shared.TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkClass, 
                Formatting.AuthorTokenKind.atkDeclare, 
                Formatting.AuthorTokenKind.atkEnum, 
                Formatting.AuthorTokenKind.atkExport, 
                Formatting.AuthorTokenKind.atkExtends, 
                Formatting.AuthorTokenKind.atkGet, 
                Formatting.AuthorTokenKind.atkImplements, 
                Formatting.AuthorTokenKind.atkImport, 
                Formatting.AuthorTokenKind.atkInterface, 
                Formatting.AuthorTokenKind.atkModule, 
                Formatting.AuthorTokenKind.atkPrivate, 
                Formatting.AuthorTokenKind.atkPublic, 
                Formatting.AuthorTokenKind.atkSet, 
                Formatting.AuthorTokenKind.atkStatic
            ]), Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Space));
            this.SpaceBeforeCertainTypeScriptKeywords = new Formatting.Rule(Formatting.RuleDescriptor.create4(Formatting.Shared.TokenRange.Any, Formatting.Shared.TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkExtends, 
                Formatting.AuthorTokenKind.atkImplements
            ])), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Space));
            this.SpaceAfterModuleName = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkString, Formatting.AuthorTokenKind.atkLCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsModuleDeclContext), Formatting.RuleAction.Space));
            this.SpaceAfterArrow = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkArrow, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Space));
            this.NoSpaceAfterEllipsis = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkEllipsis, Formatting.AuthorTokenKind.atkIdentifier), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceAfterOptionalParameters = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkQMark, Formatting.Shared.TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkRParen, 
                Formatting.AuthorTokenKind.atkComma
            ])), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsNotBinaryOpContext), Formatting.RuleAction.Delete));
            this.NoSpaceBetweenEmptyInterfaceCurlyBrackets = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkLCurly, Formatting.AuthorTokenKind.atkRCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsInterfaceContext), Formatting.RuleAction.Delete));
            this.HighPriorityCommonRules = [
                this.IgnoreBeforeComment, 
                this.IgnoreAfterLineComment, 
                this.NoSpaceBeforeSemicolon, 
                this.NoSpaceBeforeColon, 
                this.SpaceAfterColon, 
                this.NoSpaceBeforeQMark, 
                this.SpaceAfterQMark, 
                this.NewLineAfterCloseCurly, 
                this.NoSpaceBeforeDot, 
                this.NoSpaceAfterDot, 
                this.NoSpaceAfterUnaryPrefixOperator, 
                this.NoSpaceAfterUnaryPreincrementOperator, 
                this.NoSpaceAfterUnaryPredecrementOperator, 
                this.NoSpaceBeforeUnaryPostincrementOperator, 
                this.NoSpaceBeforeUnaryPostdecrementOperator, 
                this.SpaceAfterPostincrementWhenFollowedByAdd, 
                this.SpaceAfterAddWhenFollowedByUnaryPlus, 
                this.SpaceAfterAddWhenFollowedByPreincrement, 
                this.SpaceAfterPostdecrementWhenFollowedBySubtract, 
                this.SpaceAfterSubtractWhenFollowedByUnaryMinus, 
                this.SpaceAfterSubtractWhenFollowedByPredecrement, 
                this.SpaceAfterOpenCurly, 
                this.SpaceBeforeCloseCurly, 
                this.SpaceAfterCloseCurly, 
                this.SpaceBetweenCloseCurlyAndElse, 
                this.SpaceBetweenCloseCurlyAndWhile, 
                this.NoSpaceBetweenEmptyCurlyBrackets, 
                this.NewLineBeforeCloseCurlyInFunctionOrControl, 
                this.SpaceAfterFunctionInFuncDecl, 
                this.NewLineAfterOpenCurlyInBlockContext, 
                this.SpaceAfterGetSetInMember, 
                this.SpaceAfterCertainKeywords, 
                this.NoSpaceBeforeOpenParenInFuncCall, 
                this.SpaceBeforeBinaryKeywordOperator, 
                this.SpaceAfterBinaryKeywordOperator, 
                this.NoSpaceAfterConstructor, 
                this.NoSpaceAfterModuleImport, 
                this.SpaceAfterCertainTypeScriptKeywords, 
                this.SpaceBeforeCertainTypeScriptKeywords, 
                this.SpaceAfterModuleName, 
                this.SpaceAfterArrow, 
                this.NoSpaceAfterEllipsis, 
                this.NoSpaceAfterOptionalParameters, 
                this.NoSpaceBetweenEmptyInterfaceCurlyBrackets, 
                
            ];
            this.LowPriorityCommonRules = [
                this.NoSpaceBeforeComma, 
                this.NoSpaceBeforeOpenBracket, 
                this.NoSpaceAfterOpenBracket, 
                this.NoSpaceBeforeCloseBracket, 
                this.NoSpaceAfterCloseBracket, 
                this.SpaceAfterSemicolon, 
                this.NoSpaceBeforeOpenParenInFuncDecl, 
                this.SpaceBetweenStatements, 
                this.SpaceAfterTryFinally
            ];
            this.SpaceAfterComma = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkComma, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Space));
            this.NoSpaceAfterComma = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkComma, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.SpaceBeforeBinaryOperator = new Formatting.Rule(Formatting.RuleDescriptor.create4(Formatting.Shared.TokenRange.Any, Formatting.Shared.TokenRange.BinaryOperators), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Space));
            this.SpaceAfterBinaryOperator = new Formatting.Rule(Formatting.RuleDescriptor.create4(Formatting.Shared.TokenRange.BinaryOperators, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Space));
            this.NoSpaceBeforeBinaryOperator = new Formatting.Rule(Formatting.RuleDescriptor.create4(Formatting.Shared.TokenRange.Any, Formatting.Shared.TokenRange.BinaryOperators), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Delete));
            this.NoSpaceAfterBinaryOperator = new Formatting.Rule(Formatting.RuleDescriptor.create4(Formatting.Shared.TokenRange.BinaryOperators, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsBinaryOpContext), Formatting.RuleAction.Delete));
            this.SpaceAfterKeywordInControl = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Keywords, Formatting.AuthorTokenKind.atkLParen), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsControlDeclContext), Formatting.RuleAction.Space));
            this.NoSpaceAfterKeywordInControl = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Keywords, Formatting.AuthorTokenKind.atkLParen), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsControlDeclContext), Formatting.RuleAction.Delete));
            this.FunctionOpenCurlyLeftTokenRange = Formatting.Shared.TokenRange.Any;
            this.FunctionOpenCurlyLeftTokenRange_Js = Formatting.Shared.TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkRParen, 
                Formatting.AuthorTokenKind.atkComment
            ]);
            this.SpaceBeforeOpenCurlyInFunction = new Formatting.Rule(Formatting.RuleDescriptor.create2(this.FunctionOpenCurlyLeftTokenRange, Formatting.AuthorTokenKind.atkLCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsFunctionDeclContext, Rules.IsNotFormatOnEnter, Rules.IsSameLineTokenOrMultilineBlockContext), Formatting.RuleAction.Space), Formatting.RuleFlags.CanDeleteNewLines);
            this.NewLineBeforeOpenCurlyInFunction = new Formatting.Rule(Formatting.RuleDescriptor.create2(this.FunctionOpenCurlyLeftTokenRange, Formatting.AuthorTokenKind.atkLCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsFunctionDeclContext, Rules.IsMultilineBlockContext), Formatting.RuleAction.NewLine), Formatting.RuleFlags.CanDeleteNewLines);
            this.TypeScriptOpenCurlyLeftTokenRange = Formatting.Shared.TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkIdentifier, 
                Formatting.AuthorTokenKind.atkComment
            ]);
            this.SpaceBeforeOpenCurlyInTypeScriptDeclWithBlock = new Formatting.Rule(Formatting.RuleDescriptor.create2(this.TypeScriptOpenCurlyLeftTokenRange, Formatting.AuthorTokenKind.atkLCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsTypeScriptDeclWithBlockContext, Rules.IsNotFormatOnEnter, Rules.IsSameLineTokenOrMultilineBlockContext), Formatting.RuleAction.Space), Formatting.RuleFlags.CanDeleteNewLines);
            this.NewLineBeforeOpenCurlyInTypeScriptDeclWithBlock = new Formatting.Rule(Formatting.RuleDescriptor.create2(this.TypeScriptOpenCurlyLeftTokenRange, Formatting.AuthorTokenKind.atkLCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsTypeScriptDeclWithBlockContext, Rules.IsMultilineBlockContext), Formatting.RuleAction.NewLine), Formatting.RuleFlags.CanDeleteNewLines);
            this.ControlOpenCurlyLeftTokenRange = Formatting.Shared.TokenRange.FromTokens([
                Formatting.AuthorTokenKind.atkRParen, 
                Formatting.AuthorTokenKind.atkComment, 
                Formatting.AuthorTokenKind.atkDo, 
                Formatting.AuthorTokenKind.atkTry, 
                Formatting.AuthorTokenKind.atkFinally, 
                Formatting.AuthorTokenKind.atkElse
            ]);
            this.SpaceBeforeOpenCurlyInControl = new Formatting.Rule(Formatting.RuleDescriptor.create2(this.ControlOpenCurlyLeftTokenRange, Formatting.AuthorTokenKind.atkLCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsControlDeclContext, Rules.IsNotFormatOnEnter, Rules.IsSameLineTokenOrMultilineBlockContext), Formatting.RuleAction.Space), Formatting.RuleFlags.CanDeleteNewLines);
            this.NewLineBeforeOpenCurlyInControl = new Formatting.Rule(Formatting.RuleDescriptor.create2(this.ControlOpenCurlyLeftTokenRange, Formatting.AuthorTokenKind.atkLCurly), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsControlDeclContext, Rules.IsMultilineBlockContext), Formatting.RuleAction.NewLine), Formatting.RuleFlags.CanDeleteNewLines);
            this.SpaceAfterSemicolonInFor = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkSColon, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsForContext), Formatting.RuleAction.Space));
            this.NoSpaceAfterSemicolonInFor = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkSColon, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext, Rules.IsForContext), Formatting.RuleAction.Delete));
            this.SpaceAfterOpenParen = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkLParen, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Space));
            this.SpaceBeforeCloseParen = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkRParen), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Space));
            this.NoSpaceBetweenParens = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkLParen, Formatting.AuthorTokenKind.atkRParen), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceAfterOpenParen = new Formatting.Rule(Formatting.RuleDescriptor.create3(Formatting.AuthorTokenKind.atkLParen, Formatting.Shared.TokenRange.Any), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.NoSpaceBeforeCloseParen = new Formatting.Rule(Formatting.RuleDescriptor.create2(Formatting.Shared.TokenRange.Any, Formatting.AuthorTokenKind.atkRParen), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsSameLineTokenContext), Formatting.RuleAction.Delete));
            this.SpaceAfterAnonymousFunctionKeyword = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkFunction, Formatting.AuthorTokenKind.atkLParen), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsFunctionDeclContext), Formatting.RuleAction.Space));
            this.NoSpaceAfterAnonymousFunctionKeyword = new Formatting.Rule(Formatting.RuleDescriptor.create1(Formatting.AuthorTokenKind.atkFunction, Formatting.AuthorTokenKind.atkLParen), Formatting.RuleOperation.create2(new Formatting.RuleOperationContext(Rules.IsFunctionDeclContext), Formatting.RuleAction.Delete));
        }
        Rules.prototype.getRuleName = function (rule) {
            var o = this;
            for(var name in o) {
                if(o[name] === rule) {
                    return name;
                }
            }
            throw new Error("Unknown rule");
        };
        Rules.IsForContext = function IsForContext(context) {
            return context.contextNode.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkFor;
        };
        Rules.IsNotForContext = function IsNotForContext(context) {
            return context.contextNode.AuthorNode.Details.Kind != Formatting.AuthorParseNodeKind.apnkFor;
        };
        Rules.IsBinaryOpContext = function IsBinaryOpContext(context) {
            if(context.contextNode.AuthorNode.Details.ast != null) {
                switch(context.contextNode.AuthorNode.Details.ast.nodeType) {
                    case TypeScript.NodeType.ImportDeclaration:
                        return true;
                }
            }
            switch(context.contextNode.AuthorNode.Details.Kind) {
                case Formatting.AuthorParseNodeKind.apnkAdd:
                case Formatting.AuthorParseNodeKind.apnkSub:
                case Formatting.AuthorParseNodeKind.apnkMul:
                case Formatting.AuthorParseNodeKind.apnkDiv:
                case Formatting.AuthorParseNodeKind.apnkMod:
                case Formatting.AuthorParseNodeKind.apnkOr:
                case Formatting.AuthorParseNodeKind.apnkXor:
                case Formatting.AuthorParseNodeKind.apnkAnd:
                case Formatting.AuthorParseNodeKind.apnkEq:
                case Formatting.AuthorParseNodeKind.apnkNe:
                case Formatting.AuthorParseNodeKind.apnkLt:
                case Formatting.AuthorParseNodeKind.apnkLe:
                case Formatting.AuthorParseNodeKind.apnkGe:
                case Formatting.AuthorParseNodeKind.apnkGt:
                case Formatting.AuthorParseNodeKind.apnkAsg:
                case Formatting.AuthorParseNodeKind.apnkInstOf:
                case Formatting.AuthorParseNodeKind.apnkIn:
                case Formatting.AuthorParseNodeKind.apnkForIn:
                case Formatting.AuthorParseNodeKind.apnkEqv:
                case Formatting.AuthorParseNodeKind.apnkNEqv:
                case Formatting.AuthorParseNodeKind.apnkLogOr:
                case Formatting.AuthorParseNodeKind.apnkLogAnd:
                case Formatting.AuthorParseNodeKind.apnkLsh:
                case Formatting.AuthorParseNodeKind.apnkRsh:
                case Formatting.AuthorParseNodeKind.apnkRs2:
                case Formatting.AuthorParseNodeKind.apnkQmark:
                case Formatting.AuthorParseNodeKind.apnkAsgAdd:
                case Formatting.AuthorParseNodeKind.apnkAsgSub:
                case Formatting.AuthorParseNodeKind.apnkAsgMul:
                case Formatting.AuthorParseNodeKind.apnkAsgDiv:
                case Formatting.AuthorParseNodeKind.apnkAsgMod:
                case Formatting.AuthorParseNodeKind.apnkAsgAnd:
                case Formatting.AuthorParseNodeKind.apnkAsgXor:
                case Formatting.AuthorParseNodeKind.apnkAsgOr:
                case Formatting.AuthorParseNodeKind.apnkAsgLsh:
                case Formatting.AuthorParseNodeKind.apnkAsgRsh:
                case Formatting.AuthorParseNodeKind.apnkAsgRs2:
                    return true;
                case Formatting.AuthorParseNodeKind.apnkVarDecl:
                    var varOrArgDecl = context.contextNode.AuthorNode.Details.ast;
                    var tokenSpan = null;
                    if(context.tokenSpan.tokenID === TypeScript.TokenID.Question) {
                        tokenSpan = context.tokenSpan.Span.span;
                    } else if(context.nextTokenSpan.tokenID === TypeScript.TokenID.Question) {
                        tokenSpan = context.nextTokenSpan.Span.span;
                    }
                    if(context.tokenSpan.tokenID === TypeScript.TokenID.Colon) {
                        tokenSpan = context.tokenSpan.Span.span;
                    } else if(context.nextTokenSpan.tokenID === TypeScript.TokenID.Colon) {
                        tokenSpan = context.nextTokenSpan.Span.span;
                    }
                    if(tokenSpan != null) {
                        if(varOrArgDecl != null && (varOrArgDecl.nodeType === TypeScript.NodeType.VarDecl || varOrArgDecl.nodeType === TypeScript.NodeType.ArgDecl)) {
                            if(TypeScript.isValidAstNode(varOrArgDecl)) {
                                if(!TypeScript.isValidAstNode(varOrArgDecl.init)) {
                                    return false;
                                }
                                var initSpan = Formatting.Span.FromBounds(varOrArgDecl.init.minChar, varOrArgDecl.init.limChar);
                                return initSpan.Contains(tokenSpan);
                            }
                        }
                    }
                    return true;
                case Formatting.AuthorParseNodeKind.apnkFncDecl:
                    var fncDecl = context.contextNode.AuthorNode.Details.ast;
                    if(context.tokenSpan.tokenID === TypeScript.TokenID.EqualsGreaterThan || context.nextTokenSpan.tokenID === TypeScript.TokenID.EqualsGreaterThan) {
                        if(fncDecl != null && TypeScript.hasFlag(fncDecl.fncFlags, TypeScript.FncFlags.IsFunctionExpression)) {
                            return true;
                        }
                    }
                    break;
                default:
                    return false;
            }
        };
        Rules.IsNotBinaryOpContext = function IsNotBinaryOpContext(context) {
            return !Rules.IsBinaryOpContext(context);
        };
        Rules.IsBlockContext = function IsBlockContext(node) {
            if(Rules.IsTypeScriptDeclWithBlockContextNode(node)) {
                return true;
            }
            switch(node.AuthorNode.Details.Kind) {
                case Formatting.AuthorParseNodeKind.apnkBlock:
                case Formatting.AuthorParseNodeKind.apnkList:
                case Formatting.AuthorParseNodeKind.apnkObject:
                case Formatting.AuthorParseNodeKind.apnkFncDecl:
                case Formatting.AuthorParseNodeKind.apnkFor:
                case Formatting.AuthorParseNodeKind.apnkIf:
                case Formatting.AuthorParseNodeKind.apnkWhile:
                case Formatting.AuthorParseNodeKind.apnkDoWhile:
                case Formatting.AuthorParseNodeKind.apnkForIn:
                case Formatting.AuthorParseNodeKind.apnkWith:
                case Formatting.AuthorParseNodeKind.apnkSwitch:
                case Formatting.AuthorParseNodeKind.apnkTryCatch:
                case Formatting.AuthorParseNodeKind.apnkCatch:
                case Formatting.AuthorParseNodeKind.apnkTry:
                case Formatting.AuthorParseNodeKind.apnkFinally:
                case Formatting.AuthorParseNodeKind.apnkTryFinally:
                    return true;
                default:
                    return false;
            }
        };
        Rules.IsTypeScriptDeclWithBlockContextNode = function IsTypeScriptDeclWithBlockContextNode(node) {
            switch(node.AuthorNode.Details.nodeType) {
                case TypeScript.NodeType.ModuleDeclaration:
                case TypeScript.NodeType.InterfaceDeclaration:
                case TypeScript.NodeType.ClassDeclaration:
                    return true;
                default:
                    return false;
            }
        };
        Rules.IsSingleLineBlockContext = function IsSingleLineBlockContext(context) {
            if(!Rules.IsBlockContext(context.contextNode)) {
                return false;
            }
            return context.ContextNodeAllOnSameLine();
        };
        Rules.IsMultilineBlockContext = function IsMultilineBlockContext(context) {
            if(!Rules.IsBlockContext(context.contextNode)) {
                return false;
            }
            return !context.ContextNodeAllOnSameLine();
        };
        Rules.IsFunctionDeclContext = function IsFunctionDeclContext(context) {
            return context.contextNode.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkFncDecl;
        };
        Rules.IsTypeScriptDeclWithBlockContext = function IsTypeScriptDeclWithBlockContext(context) {
            return Rules.IsTypeScriptDeclWithBlockContextNode(context.contextNode);
        };
        Rules.IsControlDeclContext = function IsControlDeclContext(context) {
            switch(context.contextNode.AuthorNode.Details.Kind) {
                case Formatting.AuthorParseNodeKind.apnkFor:
                case Formatting.AuthorParseNodeKind.apnkIf:
                case Formatting.AuthorParseNodeKind.apnkWhile:
                case Formatting.AuthorParseNodeKind.apnkDoWhile:
                case Formatting.AuthorParseNodeKind.apnkForIn:
                case Formatting.AuthorParseNodeKind.apnkWith:
                case Formatting.AuthorParseNodeKind.apnkSwitch:
                case Formatting.AuthorParseNodeKind.apnkTryCatch:
                case Formatting.AuthorParseNodeKind.apnkCatch:
                case Formatting.AuthorParseNodeKind.apnkTry:
                case Formatting.AuthorParseNodeKind.apnkFinally:
                case Formatting.AuthorParseNodeKind.apnkTryFinally:
                    return true;
                default:
                    return false;
            }
        };
        Rules.IsObjectContext = function IsObjectContext(context) {
            return context.contextNode.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkObject;
        };
        Rules.IsFunctionCallContext = function IsFunctionCallContext(context) {
            return context.contextNode.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkCall;
        };
        Rules.IsNewContext = function IsNewContext(context) {
            return context.contextNode.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkNew;
        };
        Rules.IsFunctionCallOrNewContext = function IsFunctionCallOrNewContext(context) {
            return Rules.IsFunctionCallContext(context) || Rules.IsNewContext(context);
        };
        Rules.IsSameLineTokenContext = function IsSameLineTokenContext(context) {
            return context.TokensAreOnSameLine();
        };
        Rules.IsSameLineSiblingNodeContext = function IsSameLineSiblingNodeContext(context) {
            return context.TokensAreSiblingNodesOnSameLine();
        };
        Rules.IsMultilineChildParentContext = function IsMultilineChildParentContext(context) {
            var parent = context.contextNode.Parent;
            if(parent == null) {
                return false;
            }
            return parent.AuthorNode.Details.EndOffset == context.nextTokenSpan.Span.startPosition() && Rules.IsMultilineBlockContext(context);
        };
        Rules.IsNotFormatOnEnter = function IsNotFormatOnEnter(context) {
            return context.formattingRequestKind != Formatting.FormattingRequestKind.FormatOnEnter;
        };
        Rules.IsSameLineTokenOrMultilineBlockContext = function IsSameLineTokenOrMultilineBlockContext(context) {
            return context.TokensAreOnSameLine() || Rules.IsMultilineBlockContext(context);
        };
        Rules.IsFunctionOrGetSetDeclContext = function IsFunctionOrGetSetDeclContext(context) {
            return Rules.IsFunctionDeclContext(context) || Rules.IsGetSetMemberContext(context);
        };
        Rules.IsGetSetMemberContext = function IsGetSetMemberContext(context) {
            return context.contextNode.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkGetMember || context.contextNode.AuthorNode.Details.Kind == Formatting.AuthorParseNodeKind.apnkSetMember;
        };
        Rules.IsFirstTokenLineCommentContext = function IsFirstTokenLineCommentContext(context) {
            var token = context.tokenSpan;
            var twoCharSpan = token.Span.Intersection(new Formatting.Span(token.Span.startPosition(), 2));
            return twoCharSpan != null && twoCharSpan.GetText() == "//";
        };
        Rules.IsModuleDeclContext = function IsModuleDeclContext(context) {
            return context.contextNode.AuthorNode.Details.nodeType == TypeScript.NodeType.ModuleDeclaration;
        };
        Rules.IsInterfaceContext = function IsInterfaceContext(context) {
            return context.contextNode.AuthorNode.Details.nodeType == TypeScript.NodeType.List && context.contextNode.Parent != null && context.contextNode.Parent.AuthorNode.Details.nodeType == TypeScript.NodeType.InterfaceDeclaration;
        };
        return Rules;
    })();
    Formatting.Rules = Rules;    
})(Formatting || (Formatting = {}));
