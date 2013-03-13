var __extends = this.__extends || function (d, b) {
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var TypeScript;
(function (TypeScript) {
    (function (TokenID) {
        TokenID._map = [];
        TokenID._map[0] = "Any";
        TokenID.Any = 0;
        TokenID._map[1] = "Bool";
        TokenID.Bool = 1;
        TokenID._map[2] = "Break";
        TokenID.Break = 2;
        TokenID._map[3] = "Case";
        TokenID.Case = 3;
        TokenID._map[4] = "Catch";
        TokenID.Catch = 4;
        TokenID._map[5] = "Class";
        TokenID.Class = 5;
        TokenID._map[6] = "Const";
        TokenID.Const = 6;
        TokenID._map[7] = "Continue";
        TokenID.Continue = 7;
        TokenID._map[8] = "Debugger";
        TokenID.Debugger = 8;
        TokenID._map[9] = "Default";
        TokenID.Default = 9;
        TokenID._map[10] = "Delete";
        TokenID.Delete = 10;
        TokenID._map[11] = "Do";
        TokenID.Do = 11;
        TokenID._map[12] = "Else";
        TokenID.Else = 12;
        TokenID._map[13] = "Enum";
        TokenID.Enum = 13;
        TokenID._map[14] = "Export";
        TokenID.Export = 14;
        TokenID._map[15] = "Extends";
        TokenID.Extends = 15;
        TokenID._map[16] = "Declare";
        TokenID.Declare = 16;
        TokenID._map[17] = "False";
        TokenID.False = 17;
        TokenID._map[18] = "Finally";
        TokenID.Finally = 18;
        TokenID._map[19] = "For";
        TokenID.For = 19;
        TokenID._map[20] = "Function";
        TokenID.Function = 20;
        TokenID._map[21] = "Constructor";
        TokenID.Constructor = 21;
        TokenID._map[22] = "Get";
        TokenID.Get = 22;
        TokenID._map[23] = "If";
        TokenID.If = 23;
        TokenID._map[24] = "Implements";
        TokenID.Implements = 24;
        TokenID._map[25] = "Import";
        TokenID.Import = 25;
        TokenID._map[26] = "In";
        TokenID.In = 26;
        TokenID._map[27] = "InstanceOf";
        TokenID.InstanceOf = 27;
        TokenID._map[28] = "Interface";
        TokenID.Interface = 28;
        TokenID._map[29] = "Let";
        TokenID.Let = 29;
        TokenID._map[30] = "Module";
        TokenID.Module = 30;
        TokenID._map[31] = "New";
        TokenID.New = 31;
        TokenID._map[32] = "Number";
        TokenID.Number = 32;
        TokenID._map[33] = "Null";
        TokenID.Null = 33;
        TokenID._map[34] = "Package";
        TokenID.Package = 34;
        TokenID._map[35] = "Private";
        TokenID.Private = 35;
        TokenID._map[36] = "Protected";
        TokenID.Protected = 36;
        TokenID._map[37] = "Public";
        TokenID.Public = 37;
        TokenID._map[38] = "Return";
        TokenID.Return = 38;
        TokenID._map[39] = "Set";
        TokenID.Set = 39;
        TokenID._map[40] = "Static";
        TokenID.Static = 40;
        TokenID._map[41] = "String";
        TokenID.String = 41;
        TokenID._map[42] = "Super";
        TokenID.Super = 42;
        TokenID._map[43] = "Switch";
        TokenID.Switch = 43;
        TokenID._map[44] = "This";
        TokenID.This = 44;
        TokenID._map[45] = "Throw";
        TokenID.Throw = 45;
        TokenID._map[46] = "True";
        TokenID.True = 46;
        TokenID._map[47] = "Try";
        TokenID.Try = 47;
        TokenID._map[48] = "TypeOf";
        TokenID.TypeOf = 48;
        TokenID._map[49] = "Var";
        TokenID.Var = 49;
        TokenID._map[50] = "Void";
        TokenID.Void = 50;
        TokenID._map[51] = "With";
        TokenID.With = 51;
        TokenID._map[52] = "While";
        TokenID.While = 52;
        TokenID._map[53] = "Yield";
        TokenID.Yield = 53;
        TokenID._map[54] = "Semicolon";
        TokenID.Semicolon = 54;
        TokenID._map[55] = "OpenParen";
        TokenID.OpenParen = 55;
        TokenID._map[56] = "CloseParen";
        TokenID.CloseParen = 56;
        TokenID._map[57] = "OpenBracket";
        TokenID.OpenBracket = 57;
        TokenID._map[58] = "CloseBracket";
        TokenID.CloseBracket = 58;
        TokenID._map[59] = "OpenBrace";
        TokenID.OpenBrace = 59;
        TokenID._map[60] = "CloseBrace";
        TokenID.CloseBrace = 60;
        TokenID._map[61] = "Comma";
        TokenID.Comma = 61;
        TokenID._map[62] = "Equals";
        TokenID.Equals = 62;
        TokenID._map[63] = "PlusEquals";
        TokenID.PlusEquals = 63;
        TokenID._map[64] = "MinusEquals";
        TokenID.MinusEquals = 64;
        TokenID._map[65] = "AsteriskEquals";
        TokenID.AsteriskEquals = 65;
        TokenID._map[66] = "SlashEquals";
        TokenID.SlashEquals = 66;
        TokenID._map[67] = "PercentEquals";
        TokenID.PercentEquals = 67;
        TokenID._map[68] = "AmpersandEquals";
        TokenID.AmpersandEquals = 68;
        TokenID._map[69] = "CaretEquals";
        TokenID.CaretEquals = 69;
        TokenID._map[70] = "BarEquals";
        TokenID.BarEquals = 70;
        TokenID._map[71] = "LessThanLessThanEquals";
        TokenID.LessThanLessThanEquals = 71;
        TokenID._map[72] = "GreaterThanGreaterThanEquals";
        TokenID.GreaterThanGreaterThanEquals = 72;
        TokenID._map[73] = "GreaterThanGreaterThanGreaterThanEquals";
        TokenID.GreaterThanGreaterThanGreaterThanEquals = 73;
        TokenID._map[74] = "Question";
        TokenID.Question = 74;
        TokenID._map[75] = "Colon";
        TokenID.Colon = 75;
        TokenID._map[76] = "BarBar";
        TokenID.BarBar = 76;
        TokenID._map[77] = "AmpersandAmpersand";
        TokenID.AmpersandAmpersand = 77;
        TokenID._map[78] = "Bar";
        TokenID.Bar = 78;
        TokenID._map[79] = "Caret";
        TokenID.Caret = 79;
        TokenID._map[80] = "And";
        TokenID.And = 80;
        TokenID._map[81] = "EqualsEquals";
        TokenID.EqualsEquals = 81;
        TokenID._map[82] = "ExclamationEquals";
        TokenID.ExclamationEquals = 82;
        TokenID._map[83] = "EqualsEqualsEquals";
        TokenID.EqualsEqualsEquals = 83;
        TokenID._map[84] = "ExclamationEqualsEquals";
        TokenID.ExclamationEqualsEquals = 84;
        TokenID._map[85] = "LessThan";
        TokenID.LessThan = 85;
        TokenID._map[86] = "LessThanEquals";
        TokenID.LessThanEquals = 86;
        TokenID._map[87] = "GreaterThan";
        TokenID.GreaterThan = 87;
        TokenID._map[88] = "GreaterThanEquals";
        TokenID.GreaterThanEquals = 88;
        TokenID._map[89] = "LessThanLessThan";
        TokenID.LessThanLessThan = 89;
        TokenID._map[90] = "GreaterThanGreaterThan";
        TokenID.GreaterThanGreaterThan = 90;
        TokenID._map[91] = "GreaterThanGreaterThanGreaterThan";
        TokenID.GreaterThanGreaterThanGreaterThan = 91;
        TokenID._map[92] = "Plus";
        TokenID.Plus = 92;
        TokenID._map[93] = "Minus";
        TokenID.Minus = 93;
        TokenID._map[94] = "Asterisk";
        TokenID.Asterisk = 94;
        TokenID._map[95] = "Slash";
        TokenID.Slash = 95;
        TokenID._map[96] = "Percent";
        TokenID.Percent = 96;
        TokenID._map[97] = "Tilde";
        TokenID.Tilde = 97;
        TokenID._map[98] = "Exclamation";
        TokenID.Exclamation = 98;
        TokenID._map[99] = "PlusPlus";
        TokenID.PlusPlus = 99;
        TokenID._map[100] = "MinusMinus";
        TokenID.MinusMinus = 100;
        TokenID._map[101] = "Dot";
        TokenID.Dot = 101;
        TokenID._map[102] = "DotDotDot";
        TokenID.DotDotDot = 102;
        TokenID._map[103] = "Error";
        TokenID.Error = 103;
        TokenID._map[104] = "EndOfFile";
        TokenID.EndOfFile = 104;
        TokenID._map[105] = "EqualsGreaterThan";
        TokenID.EqualsGreaterThan = 105;
        TokenID._map[106] = "Identifier";
        TokenID.Identifier = 106;
        TokenID._map[107] = "StringLiteral";
        TokenID.StringLiteral = 107;
        TokenID._map[108] = "RegularExpressionLiteral";
        TokenID.RegularExpressionLiteral = 108;
        TokenID._map[109] = "NumberLiteral";
        TokenID.NumberLiteral = 109;
        TokenID._map[110] = "Whitespace";
        TokenID.Whitespace = 110;
        TokenID._map[111] = "Comment";
        TokenID.Comment = 111;
        TokenID._map[112] = "Lim";
        TokenID.Lim = 112;
        TokenID.LimFixed = TokenID.EqualsGreaterThan;
        TokenID.LimKeyword = TokenID.Yield;
    })(TypeScript.TokenID || (TypeScript.TokenID = {}));
    var TokenID = TypeScript.TokenID;
    TypeScript.tokenTable = new Array();
    TypeScript.nodeTypeTable = new Array();
    TypeScript.nodeTypeToTokTable = new Array();
    TypeScript.noRegexTable = new Array();
    TypeScript.noRegexTable[TokenID.Identifier] = true;
    TypeScript.noRegexTable[TokenID.StringLiteral] = true;
    TypeScript.noRegexTable[TokenID.NumberLiteral] = true;
    TypeScript.noRegexTable[TokenID.RegularExpressionLiteral] = true;
    TypeScript.noRegexTable[TokenID.This] = true;
    TypeScript.noRegexTable[TokenID.PlusPlus] = true;
    TypeScript.noRegexTable[TokenID.MinusMinus] = true;
    TypeScript.noRegexTable[TokenID.CloseParen] = true;
    TypeScript.noRegexTable[TokenID.CloseBracket] = true;
    TypeScript.noRegexTable[TokenID.CloseBrace] = true;
    TypeScript.noRegexTable[TokenID.True] = true;
    TypeScript.noRegexTable[TokenID.False] = true;
    (function (OperatorPrecedence) {
        OperatorPrecedence._map = [];
        OperatorPrecedence._map[0] = "None";
        OperatorPrecedence.None = 0;
        OperatorPrecedence._map[1] = "Comma";
        OperatorPrecedence.Comma = 1;
        OperatorPrecedence._map[2] = "Assignment";
        OperatorPrecedence.Assignment = 2;
        OperatorPrecedence._map[3] = "Conditional";
        OperatorPrecedence.Conditional = 3;
        OperatorPrecedence._map[4] = "LogicalOr";
        OperatorPrecedence.LogicalOr = 4;
        OperatorPrecedence._map[5] = "LogicalAnd";
        OperatorPrecedence.LogicalAnd = 5;
        OperatorPrecedence._map[6] = "BitwiseOr";
        OperatorPrecedence.BitwiseOr = 6;
        OperatorPrecedence._map[7] = "BitwiseExclusiveOr";
        OperatorPrecedence.BitwiseExclusiveOr = 7;
        OperatorPrecedence._map[8] = "BitwiseAnd";
        OperatorPrecedence.BitwiseAnd = 8;
        OperatorPrecedence._map[9] = "Equality";
        OperatorPrecedence.Equality = 9;
        OperatorPrecedence._map[10] = "Relational";
        OperatorPrecedence.Relational = 10;
        OperatorPrecedence._map[11] = "Shift";
        OperatorPrecedence.Shift = 11;
        OperatorPrecedence._map[12] = "Additive";
        OperatorPrecedence.Additive = 12;
        OperatorPrecedence._map[13] = "Multiplicative";
        OperatorPrecedence.Multiplicative = 13;
        OperatorPrecedence._map[14] = "Unary";
        OperatorPrecedence.Unary = 14;
        OperatorPrecedence._map[15] = "Lim";
        OperatorPrecedence.Lim = 15;
    })(TypeScript.OperatorPrecedence || (TypeScript.OperatorPrecedence = {}));
    var OperatorPrecedence = TypeScript.OperatorPrecedence;
    (function (Reservation) {
        Reservation._map = [];
        Reservation.None = 0;
        Reservation.Javascript = 1;
        Reservation.JavascriptFuture = 2;
        Reservation.TypeScript = 4;
        Reservation.JavascriptFutureStrict = 8;
        Reservation.TypeScriptAndJS = Reservation.Javascript | Reservation.TypeScript;
        Reservation.TypeScriptAndJSFuture = Reservation.JavascriptFuture | Reservation.TypeScript;
        Reservation.TypeScriptAndJSFutureStrict = Reservation.JavascriptFutureStrict | Reservation.TypeScript;
    })(TypeScript.Reservation || (TypeScript.Reservation = {}));
    var Reservation = TypeScript.Reservation;
    var TokenInfo = (function () {
        function TokenInfo(tokenId, reservation, binopPrecedence, binopNodeType, unopPrecedence, unopNodeType, text, ers) {
            this.tokenId = tokenId;
            this.reservation = reservation;
            this.binopPrecedence = binopPrecedence;
            this.binopNodeType = binopNodeType;
            this.unopPrecedence = unopPrecedence;
            this.unopNodeType = unopNodeType;
            this.text = text;
            this.ers = ers;
        }
        return TokenInfo;
    })();
    TypeScript.TokenInfo = TokenInfo;    
    function setTokenInfo(tokenId, reservation, binopPrecedence, binopNodeType, unopPrecedence, unopNodeType, text, ers) {
        if(tokenId !== undefined) {
            TypeScript.tokenTable[tokenId] = new TokenInfo(tokenId, reservation, binopPrecedence, binopNodeType, unopPrecedence, unopNodeType, text, ers);
            if(binopNodeType != TypeScript.NodeType.None) {
                TypeScript.nodeTypeTable[binopNodeType] = text;
                TypeScript.nodeTypeToTokTable[binopNodeType] = tokenId;
            }
            if(unopNodeType != TypeScript.NodeType.None) {
                TypeScript.nodeTypeTable[unopNodeType] = text;
            }
        }
    }
    setTokenInfo(TokenID.Any, Reservation.TypeScript, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "any", TypeScript.ErrorRecoverySet.PrimType);
    setTokenInfo(TokenID.Bool, Reservation.TypeScript, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "bool", TypeScript.ErrorRecoverySet.PrimType);
    setTokenInfo(TokenID.Break, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "break", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.Case, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "case", TypeScript.ErrorRecoverySet.SCase);
    setTokenInfo(TokenID.Catch, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "catch", TypeScript.ErrorRecoverySet.Catch);
    setTokenInfo(TokenID.Class, Reservation.TypeScriptAndJSFuture, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "class", TypeScript.ErrorRecoverySet.TypeScriptS);
    setTokenInfo(TokenID.Const, Reservation.TypeScriptAndJSFuture, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "const", TypeScript.ErrorRecoverySet.Var);
    setTokenInfo(TokenID.Continue, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "continue", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.Debugger, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.Debugger, "debugger", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.Default, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "default", TypeScript.ErrorRecoverySet.SCase);
    setTokenInfo(TokenID.Delete, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.Unary, TypeScript.NodeType.Delete, "delete", TypeScript.ErrorRecoverySet.Prefix);
    setTokenInfo(TokenID.Do, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "do", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.Else, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "else", TypeScript.ErrorRecoverySet.Else);
    setTokenInfo(TokenID.Enum, Reservation.TypeScriptAndJSFuture, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "enum", TypeScript.ErrorRecoverySet.TypeScriptS);
    setTokenInfo(TokenID.Export, Reservation.TypeScriptAndJSFuture, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "export", TypeScript.ErrorRecoverySet.TypeScriptS);
    setTokenInfo(TokenID.Extends, Reservation.TypeScriptAndJSFuture, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "extends", TypeScript.ErrorRecoverySet.None);
    setTokenInfo(TokenID.Declare, Reservation.TypeScript, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "declare", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.False, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "false", TypeScript.ErrorRecoverySet.RLit);
    setTokenInfo(TokenID.Finally, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "finally", TypeScript.ErrorRecoverySet.Catch);
    setTokenInfo(TokenID.For, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "for", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.Function, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "function", TypeScript.ErrorRecoverySet.Func);
    setTokenInfo(TokenID.Constructor, Reservation.TypeScriptAndJSFutureStrict, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "constructor", TypeScript.ErrorRecoverySet.Func);
    setTokenInfo(TokenID.Get, Reservation.TypeScript, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "get", TypeScript.ErrorRecoverySet.Func);
    setTokenInfo(TokenID.Set, Reservation.TypeScript, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "set", TypeScript.ErrorRecoverySet.Func);
    setTokenInfo(TokenID.If, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "if", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.Implements, Reservation.TypeScriptAndJSFutureStrict, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "implements", TypeScript.ErrorRecoverySet.None);
    setTokenInfo(TokenID.Import, Reservation.TypeScriptAndJSFuture, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "import", TypeScript.ErrorRecoverySet.TypeScriptS);
    setTokenInfo(TokenID.In, Reservation.TypeScriptAndJS, OperatorPrecedence.Relational, TypeScript.NodeType.In, OperatorPrecedence.None, TypeScript.NodeType.None, "in", TypeScript.ErrorRecoverySet.None);
    setTokenInfo(TokenID.InstanceOf, Reservation.TypeScriptAndJS, OperatorPrecedence.Relational, TypeScript.NodeType.InstOf, OperatorPrecedence.None, TypeScript.NodeType.None, "instanceof", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.Interface, Reservation.TypeScriptAndJSFutureStrict, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "interface", TypeScript.ErrorRecoverySet.TypeScriptS);
    setTokenInfo(TokenID.Let, Reservation.JavascriptFutureStrict, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "let", TypeScript.ErrorRecoverySet.None);
    setTokenInfo(TokenID.Module, Reservation.TypeScript, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "module", TypeScript.ErrorRecoverySet.TypeScriptS);
    setTokenInfo(TokenID.New, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "new", TypeScript.ErrorRecoverySet.PreOp);
    setTokenInfo(TokenID.Number, Reservation.TypeScript, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "number", TypeScript.ErrorRecoverySet.PrimType);
    setTokenInfo(TokenID.Null, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "null", TypeScript.ErrorRecoverySet.RLit);
    setTokenInfo(TokenID.Package, Reservation.JavascriptFutureStrict, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "package", TypeScript.ErrorRecoverySet.None);
    setTokenInfo(TokenID.Private, Reservation.TypeScriptAndJSFutureStrict, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "private", TypeScript.ErrorRecoverySet.TypeScriptS);
    setTokenInfo(TokenID.Protected, Reservation.JavascriptFutureStrict, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "protected", TypeScript.ErrorRecoverySet.None);
    setTokenInfo(TokenID.Public, Reservation.TypeScriptAndJSFutureStrict, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "public", TypeScript.ErrorRecoverySet.TypeScriptS);
    setTokenInfo(TokenID.Return, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "return", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.Static, Reservation.TypeScriptAndJSFutureStrict, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "static", TypeScript.ErrorRecoverySet.None);
    setTokenInfo(TokenID.String, Reservation.TypeScript, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "string", TypeScript.ErrorRecoverySet.PrimType);
    setTokenInfo(TokenID.Super, Reservation.TypeScriptAndJSFuture, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "super", TypeScript.ErrorRecoverySet.RLit);
    setTokenInfo(TokenID.Switch, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "switch", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.This, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "this", TypeScript.ErrorRecoverySet.RLit);
    setTokenInfo(TokenID.Throw, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "throw", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.True, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "true", TypeScript.ErrorRecoverySet.RLit);
    setTokenInfo(TokenID.Try, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "try", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.TypeOf, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.Unary, TypeScript.NodeType.Typeof, "typeof", TypeScript.ErrorRecoverySet.Prefix);
    setTokenInfo(TokenID.Var, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "var", TypeScript.ErrorRecoverySet.Var);
    setTokenInfo(TokenID.Void, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.Unary, TypeScript.NodeType.Void, "void", TypeScript.ErrorRecoverySet.Prefix);
    setTokenInfo(TokenID.With, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.With, "with", TypeScript.ErrorRecoverySet.Stmt);
    setTokenInfo(TokenID.While, Reservation.TypeScriptAndJS, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "while", TypeScript.ErrorRecoverySet.While);
    setTokenInfo(TokenID.Yield, Reservation.JavascriptFutureStrict, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "yield", TypeScript.ErrorRecoverySet.None);
    setTokenInfo(TokenID.Identifier, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "identifier", TypeScript.ErrorRecoverySet.ID);
    setTokenInfo(TokenID.NumberLiteral, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "numberLiteral", TypeScript.ErrorRecoverySet.Literal);
    setTokenInfo(TokenID.RegularExpressionLiteral, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "regex", TypeScript.ErrorRecoverySet.RegExp);
    setTokenInfo(TokenID.StringLiteral, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "qstring", TypeScript.ErrorRecoverySet.Literal);
    setTokenInfo(TokenID.Semicolon, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, ";", TypeScript.ErrorRecoverySet.SColon);
    setTokenInfo(TokenID.CloseParen, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, ")", TypeScript.ErrorRecoverySet.RParen);
    setTokenInfo(TokenID.CloseBracket, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "]", TypeScript.ErrorRecoverySet.RBrack);
    setTokenInfo(TokenID.OpenBrace, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "{", TypeScript.ErrorRecoverySet.LCurly);
    setTokenInfo(TokenID.CloseBrace, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "}", TypeScript.ErrorRecoverySet.RCurly);
    setTokenInfo(TokenID.DotDotDot, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "...", TypeScript.ErrorRecoverySet.None);
    setTokenInfo(TokenID.Comma, Reservation.None, OperatorPrecedence.Comma, TypeScript.NodeType.Comma, OperatorPrecedence.None, TypeScript.NodeType.None, ",", TypeScript.ErrorRecoverySet.Comma);
    setTokenInfo(TokenID.Equals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.Asg, OperatorPrecedence.None, TypeScript.NodeType.None, "=", TypeScript.ErrorRecoverySet.Asg);
    setTokenInfo(TokenID.PlusEquals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.AsgAdd, OperatorPrecedence.None, TypeScript.NodeType.None, "+=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.MinusEquals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.AsgSub, OperatorPrecedence.None, TypeScript.NodeType.None, "-=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.AsteriskEquals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.AsgMul, OperatorPrecedence.None, TypeScript.NodeType.None, "*=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.SlashEquals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.AsgDiv, OperatorPrecedence.None, TypeScript.NodeType.None, "/=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.PercentEquals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.AsgMod, OperatorPrecedence.None, TypeScript.NodeType.None, "%=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.AmpersandEquals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.AsgAnd, OperatorPrecedence.None, TypeScript.NodeType.None, "&=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.CaretEquals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.AsgXor, OperatorPrecedence.None, TypeScript.NodeType.None, "^=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.BarEquals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.AsgOr, OperatorPrecedence.None, TypeScript.NodeType.None, "|=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.LessThanLessThanEquals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.AsgLsh, OperatorPrecedence.None, TypeScript.NodeType.None, "<<=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.GreaterThanGreaterThanEquals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.AsgRsh, OperatorPrecedence.None, TypeScript.NodeType.None, ">>=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.GreaterThanGreaterThanGreaterThanEquals, Reservation.None, OperatorPrecedence.Assignment, TypeScript.NodeType.AsgRs2, OperatorPrecedence.None, TypeScript.NodeType.None, ">>>=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.Question, Reservation.None, OperatorPrecedence.Conditional, TypeScript.NodeType.ConditionalExpression, OperatorPrecedence.None, TypeScript.NodeType.None, "?", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.Colon, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, ":", TypeScript.ErrorRecoverySet.Colon);
    setTokenInfo(TokenID.BarBar, Reservation.None, OperatorPrecedence.LogicalOr, TypeScript.NodeType.LogOr, OperatorPrecedence.None, TypeScript.NodeType.None, "||", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.AmpersandAmpersand, Reservation.None, OperatorPrecedence.LogicalAnd, TypeScript.NodeType.LogAnd, OperatorPrecedence.None, TypeScript.NodeType.None, "&&", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.Bar, Reservation.None, OperatorPrecedence.BitwiseOr, TypeScript.NodeType.Or, OperatorPrecedence.None, TypeScript.NodeType.None, "|", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.Caret, Reservation.None, OperatorPrecedence.BitwiseExclusiveOr, TypeScript.NodeType.Xor, OperatorPrecedence.None, TypeScript.NodeType.None, "^", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.And, Reservation.None, OperatorPrecedence.BitwiseAnd, TypeScript.NodeType.And, OperatorPrecedence.None, TypeScript.NodeType.None, "&", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.EqualsEquals, Reservation.None, OperatorPrecedence.Equality, TypeScript.NodeType.Eq, OperatorPrecedence.None, TypeScript.NodeType.None, "==", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.ExclamationEquals, Reservation.None, OperatorPrecedence.Equality, TypeScript.NodeType.Ne, OperatorPrecedence.None, TypeScript.NodeType.None, "!=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.EqualsEqualsEquals, Reservation.None, OperatorPrecedence.Equality, TypeScript.NodeType.Eqv, OperatorPrecedence.None, TypeScript.NodeType.None, "===", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.ExclamationEqualsEquals, Reservation.None, OperatorPrecedence.Equality, TypeScript.NodeType.NEqv, OperatorPrecedence.None, TypeScript.NodeType.None, "!==", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.LessThan, Reservation.None, OperatorPrecedence.Relational, TypeScript.NodeType.Lt, OperatorPrecedence.None, TypeScript.NodeType.None, "<", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.LessThanEquals, Reservation.None, OperatorPrecedence.Relational, TypeScript.NodeType.Le, OperatorPrecedence.None, TypeScript.NodeType.None, "<=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.GreaterThan, Reservation.None, OperatorPrecedence.Relational, TypeScript.NodeType.Gt, OperatorPrecedence.None, TypeScript.NodeType.None, ">", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.GreaterThanEquals, Reservation.None, OperatorPrecedence.Relational, TypeScript.NodeType.Ge, OperatorPrecedence.None, TypeScript.NodeType.None, ">=", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.LessThanLessThan, Reservation.None, OperatorPrecedence.Shift, TypeScript.NodeType.Lsh, OperatorPrecedence.None, TypeScript.NodeType.None, "<<", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.GreaterThanGreaterThan, Reservation.None, OperatorPrecedence.Shift, TypeScript.NodeType.Rsh, OperatorPrecedence.None, TypeScript.NodeType.None, ">>", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.GreaterThanGreaterThanGreaterThan, Reservation.None, OperatorPrecedence.Shift, TypeScript.NodeType.Rs2, OperatorPrecedence.None, TypeScript.NodeType.None, ">>>", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.Plus, Reservation.None, OperatorPrecedence.Additive, TypeScript.NodeType.Add, OperatorPrecedence.Unary, TypeScript.NodeType.Pos, "+", TypeScript.ErrorRecoverySet.AddOp);
    setTokenInfo(TokenID.Minus, Reservation.None, OperatorPrecedence.Additive, TypeScript.NodeType.Sub, OperatorPrecedence.Unary, TypeScript.NodeType.Neg, "-", TypeScript.ErrorRecoverySet.AddOp);
    setTokenInfo(TokenID.Asterisk, Reservation.None, OperatorPrecedence.Multiplicative, TypeScript.NodeType.Mul, OperatorPrecedence.None, TypeScript.NodeType.None, "*", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.Slash, Reservation.None, OperatorPrecedence.Multiplicative, TypeScript.NodeType.Div, OperatorPrecedence.None, TypeScript.NodeType.None, "/", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.Percent, Reservation.None, OperatorPrecedence.Multiplicative, TypeScript.NodeType.Mod, OperatorPrecedence.None, TypeScript.NodeType.None, "%", TypeScript.ErrorRecoverySet.BinOp);
    setTokenInfo(TokenID.Tilde, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.Unary, TypeScript.NodeType.Not, "~", TypeScript.ErrorRecoverySet.PreOp);
    setTokenInfo(TokenID.Exclamation, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.Unary, TypeScript.NodeType.LogNot, "!", TypeScript.ErrorRecoverySet.PreOp);
    setTokenInfo(TokenID.PlusPlus, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.Unary, TypeScript.NodeType.IncPre, "++", TypeScript.ErrorRecoverySet.PreOp);
    setTokenInfo(TokenID.MinusMinus, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.Unary, TypeScript.NodeType.DecPre, "--", TypeScript.ErrorRecoverySet.PreOp);
    setTokenInfo(TokenID.OpenParen, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "(", TypeScript.ErrorRecoverySet.LParen);
    setTokenInfo(TokenID.OpenBracket, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "[", TypeScript.ErrorRecoverySet.LBrack);
    setTokenInfo(TokenID.Dot, Reservation.None, OperatorPrecedence.Unary, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, ".", TypeScript.ErrorRecoverySet.Dot);
    setTokenInfo(TokenID.EndOfFile, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "<EOF>", TypeScript.ErrorRecoverySet.EOF);
    setTokenInfo(TokenID.EqualsGreaterThan, Reservation.None, OperatorPrecedence.None, TypeScript.NodeType.None, OperatorPrecedence.None, TypeScript.NodeType.None, "=>", TypeScript.ErrorRecoverySet.None);
    function lookupToken(tokenId) {
        return TypeScript.tokenTable[tokenId];
    }
    TypeScript.lookupToken = lookupToken;
    (function (TokenClass) {
        TokenClass._map = [];
        TokenClass._map[0] = "Punctuation";
        TokenClass.Punctuation = 0;
        TokenClass._map[1] = "Keyword";
        TokenClass.Keyword = 1;
        TokenClass._map[2] = "Operator";
        TokenClass.Operator = 2;
        TokenClass._map[3] = "Comment";
        TokenClass.Comment = 3;
        TokenClass._map[4] = "Whitespace";
        TokenClass.Whitespace = 4;
        TokenClass._map[5] = "Identifier";
        TokenClass.Identifier = 5;
        TokenClass._map[6] = "NumberLiteral";
        TokenClass.NumberLiteral = 6;
        TokenClass._map[7] = "StringLiteral";
        TokenClass.StringLiteral = 7;
        TokenClass._map[8] = "RegExpLiteral";
        TokenClass.RegExpLiteral = 8;
    })(TypeScript.TokenClass || (TypeScript.TokenClass = {}));
    var TokenClass = TypeScript.TokenClass;
    var SavedToken = (function () {
        function SavedToken(tok, minChar, limChar) {
            this.tok = tok;
            this.minChar = minChar;
            this.limChar = limChar;
        }
        return SavedToken;
    })();
    TypeScript.SavedToken = SavedToken;    
    var Token = (function () {
        function Token(tokenId) {
            this.tokenId = tokenId;
        }
        Token.prototype.toString = function () {
            return "token: " + this.tokenId + " " + this.getText() + " (" + (TokenID)._map[this.tokenId] + ")";
        };
        Token.prototype.print = function (line, outfile) {
            outfile.WriteLine(this.toString() + ",on line" + line);
        };
        Token.prototype.getText = function () {
            return TypeScript.tokenTable[this.tokenId].text;
        };
        Token.prototype.classification = function () {
            if(this.tokenId <= TokenID.LimKeyword) {
                return TokenClass.Keyword;
            } else {
                var tokenInfo = lookupToken(this.tokenId);
                if(tokenInfo != undefined) {
                    if((tokenInfo.unopNodeType != TypeScript.NodeType.None) || (tokenInfo.binopNodeType != TypeScript.NodeType.None)) {
                        return TokenClass.Operator;
                    }
                }
            }
            return TokenClass.Punctuation;
        };
        return Token;
    })();
    TypeScript.Token = Token;    
    var NumberLiteralToken = (function (_super) {
        __extends(NumberLiteralToken, _super);
        function NumberLiteralToken(value, text) {
                _super.call(this, TokenID.NumberLiteral);
            this.value = value;
            this.text = text;
        }
        NumberLiteralToken.prototype.getText = function () {
            return this.text;
        };
        NumberLiteralToken.prototype.classification = function () {
            return TokenClass.NumberLiteral;
        };
        return NumberLiteralToken;
    })(Token);
    TypeScript.NumberLiteralToken = NumberLiteralToken;    
    var StringLiteralToken = (function (_super) {
        __extends(StringLiteralToken, _super);
        function StringLiteralToken(value) {
                _super.call(this, TokenID.StringLiteral);
            this.value = value;
        }
        StringLiteralToken.prototype.getText = function () {
            return this.value;
        };
        StringLiteralToken.prototype.classification = function () {
            return TokenClass.StringLiteral;
        };
        return StringLiteralToken;
    })(Token);
    TypeScript.StringLiteralToken = StringLiteralToken;    
    var IdentifierToken = (function (_super) {
        __extends(IdentifierToken, _super);
        function IdentifierToken(value, hasEscapeSequence) {
                _super.call(this, TokenID.Identifier);
            this.value = value;
            this.hasEscapeSequence = hasEscapeSequence;
        }
        IdentifierToken.prototype.getText = function () {
            return this.value;
        };
        IdentifierToken.prototype.classification = function () {
            return TokenClass.Identifier;
        };
        return IdentifierToken;
    })(Token);
    TypeScript.IdentifierToken = IdentifierToken;    
    var WhitespaceToken = (function (_super) {
        __extends(WhitespaceToken, _super);
        function WhitespaceToken(tokenId, value) {
                _super.call(this, tokenId);
            this.value = value;
        }
        WhitespaceToken.prototype.getText = function () {
            return this.value;
        };
        WhitespaceToken.prototype.classification = function () {
            return TokenClass.Whitespace;
        };
        return WhitespaceToken;
    })(Token);
    TypeScript.WhitespaceToken = WhitespaceToken;    
    var CommentToken = (function (_super) {
        __extends(CommentToken, _super);
        function CommentToken(tokenID, value, isBlock, startPos, line, endsLine) {
                _super.call(this, tokenID);
            this.value = value;
            this.isBlock = isBlock;
            this.startPos = startPos;
            this.line = line;
            this.endsLine = endsLine;
        }
        CommentToken.prototype.getText = function () {
            return this.value;
        };
        CommentToken.prototype.classification = function () {
            return TokenClass.Comment;
        };
        return CommentToken;
    })(Token);
    TypeScript.CommentToken = CommentToken;    
    var RegularExpressionLiteralToken = (function (_super) {
        __extends(RegularExpressionLiteralToken, _super);
        function RegularExpressionLiteralToken(text) {
                _super.call(this, TokenID.RegularExpressionLiteral);
            this.text = text;
        }
        RegularExpressionLiteralToken.prototype.getText = function () {
            return this.text;
        };
        RegularExpressionLiteralToken.prototype.classification = function () {
            return TokenClass.RegExpLiteral;
        };
        return RegularExpressionLiteralToken;
    })(Token);
    TypeScript.RegularExpressionLiteralToken = RegularExpressionLiteralToken;    
    TypeScript.staticTokens = new Array();
    function initializeStaticTokens() {
        for(var i = 0; i <= TokenID.LimFixed; i++) {
            TypeScript.staticTokens[i] = new Token(i);
        }
    }
    TypeScript.initializeStaticTokens = initializeStaticTokens;
})(TypeScript || (TypeScript = {}));
