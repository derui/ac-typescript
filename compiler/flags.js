var TypeScript;
(function (TypeScript) {
    function hasFlag(val, flag) {
        return (val & flag) != 0;
    }
    TypeScript.hasFlag = hasFlag;
    (function (ErrorRecoverySet) {
        ErrorRecoverySet._map = [];
        ErrorRecoverySet.None = 0;
        ErrorRecoverySet.Comma = 1;
        ErrorRecoverySet.SColon = 1 << 1;
        ErrorRecoverySet.Asg = 1 << 2;
        ErrorRecoverySet.BinOp = 1 << 3;
        ErrorRecoverySet.RBrack = 1 << 4;
        ErrorRecoverySet.RCurly = 1 << 5;
        ErrorRecoverySet.RParen = 1 << 6;
        ErrorRecoverySet.Dot = 1 << 7;
        ErrorRecoverySet.Colon = 1 << 8;
        ErrorRecoverySet.PrimType = 1 << 9;
        ErrorRecoverySet.AddOp = 1 << 10;
        ErrorRecoverySet.LCurly = 1 << 11;
        ErrorRecoverySet.PreOp = 1 << 12;
        ErrorRecoverySet.RegExp = 1 << 13;
        ErrorRecoverySet.LParen = 1 << 14;
        ErrorRecoverySet.LBrack = 1 << 15;
        ErrorRecoverySet.Scope = 1 << 16;
        ErrorRecoverySet.In = 1 << 17;
        ErrorRecoverySet.SCase = 1 << 18;
        ErrorRecoverySet.Else = 1 << 19;
        ErrorRecoverySet.Catch = 1 << 20;
        ErrorRecoverySet.Var = 1 << 21;
        ErrorRecoverySet.Stmt = 1 << 22;
        ErrorRecoverySet.While = 1 << 23;
        ErrorRecoverySet.ID = 1 << 24;
        ErrorRecoverySet.Prefix = 1 << 25;
        ErrorRecoverySet.Literal = 1 << 26;
        ErrorRecoverySet.RLit = 1 << 27;
        ErrorRecoverySet.Func = 1 << 28;
        ErrorRecoverySet.EOF = 1 << 29;
        ErrorRecoverySet.TypeScriptS = 1 << 30;
        ErrorRecoverySet.ExprStart = ErrorRecoverySet.SColon | ErrorRecoverySet.AddOp | ErrorRecoverySet.LCurly | ErrorRecoverySet.PreOp | ErrorRecoverySet.RegExp | ErrorRecoverySet.LParen | ErrorRecoverySet.LBrack | ErrorRecoverySet.ID | ErrorRecoverySet.Prefix | ErrorRecoverySet.RLit | ErrorRecoverySet.Func | ErrorRecoverySet.Literal;
        ErrorRecoverySet.StmtStart = ErrorRecoverySet.ExprStart | ErrorRecoverySet.SColon | ErrorRecoverySet.Var | ErrorRecoverySet.Stmt | ErrorRecoverySet.While | ErrorRecoverySet.TypeScriptS;
        ErrorRecoverySet.Postfix = ErrorRecoverySet.Dot | ErrorRecoverySet.LParen | ErrorRecoverySet.LBrack;
    })(TypeScript.ErrorRecoverySet || (TypeScript.ErrorRecoverySet = {}));
    var ErrorRecoverySet = TypeScript.ErrorRecoverySet;
    (function (AllowedElements) {
        AllowedElements._map = [];
        AllowedElements.None = 0;
        AllowedElements.ModuleDeclarations = 1 << 2;
        AllowedElements.ClassDeclarations = 1 << 3;
        AllowedElements.InterfaceDeclarations = 1 << 4;
        AllowedElements.AmbientDeclarations = 1 << 10;
        AllowedElements.Properties = 1 << 11;
        AllowedElements.Global = AllowedElements.ModuleDeclarations | AllowedElements.ClassDeclarations | AllowedElements.InterfaceDeclarations | AllowedElements.AmbientDeclarations;
        AllowedElements.QuickParse = AllowedElements.Global | AllowedElements.Properties;
    })(TypeScript.AllowedElements || (TypeScript.AllowedElements = {}));
    var AllowedElements = TypeScript.AllowedElements;
    (function (Modifiers) {
        Modifiers._map = [];
        Modifiers.None = 0;
        Modifiers.Private = 1;
        Modifiers.Public = 1 << 1;
        Modifiers.Readonly = 1 << 2;
        Modifiers.Ambient = 1 << 3;
        Modifiers.Exported = 1 << 4;
        Modifiers.Getter = 1 << 5;
        Modifiers.Setter = 1 << 6;
        Modifiers.Static = 1 << 7;
    })(TypeScript.Modifiers || (TypeScript.Modifiers = {}));
    var Modifiers = TypeScript.Modifiers;
    (function (ASTFlags) {
        ASTFlags._map = [];
        ASTFlags.None = 0;
        ASTFlags.ExplicitSemicolon = 1;
        ASTFlags.AutomaticSemicolon = 1 << 1;
        ASTFlags.Writeable = 1 << 2;
        ASTFlags.Error = 1 << 3;
        ASTFlags.DotLHSPartial = 1 << 4;
        ASTFlags.DotLHS = 1 << 5;
        ASTFlags.IsStatement = 1 << 6;
        ASTFlags.StrictMode = 1 << 7;
        ASTFlags.PossibleOptionalParameter = 1 << 8;
        ASTFlags.ClassBaseConstructorCall = 1 << 9;
        ASTFlags.OptionalName = 1 << 10;
        ASTFlags.SkipNextRParen = 1 << 11;
    })(TypeScript.ASTFlags || (TypeScript.ASTFlags = {}));
    var ASTFlags = TypeScript.ASTFlags;
    (function (DeclFlags) {
        DeclFlags._map = [];
        DeclFlags.None = 0;
        DeclFlags.Exported = 1;
        DeclFlags.Private = 1 << 1;
        DeclFlags.Public = 1 << 2;
        DeclFlags.Ambient = 1 << 3;
        DeclFlags.Static = 1 << 4;
        DeclFlags.LocalStatic = 1 << 5;
        DeclFlags.GetAccessor = 1 << 6;
        DeclFlags.SetAccessor = 1 << 7;
    })(TypeScript.DeclFlags || (TypeScript.DeclFlags = {}));
    var DeclFlags = TypeScript.DeclFlags;
    (function (ModuleFlags) {
        ModuleFlags._map = [];
        ModuleFlags.None = 0;
        ModuleFlags.Exported = 1;
        ModuleFlags.Private = 1 << 1;
        ModuleFlags.Public = 1 << 2;
        ModuleFlags.Ambient = 1 << 3;
        ModuleFlags.Static = 1 << 4;
        ModuleFlags.LocalStatic = 1 << 5;
        ModuleFlags.GetAccessor = 1 << 6;
        ModuleFlags.SetAccessor = 1 << 7;
        ModuleFlags.IsEnum = 1 << 8;
        ModuleFlags.ShouldEmitModuleDecl = 1 << 9;
        ModuleFlags.IsWholeFile = 1 << 10;
        ModuleFlags.IsDynamic = 1 << 11;
        ModuleFlags.MustCaptureThis = 1 << 12;
    })(TypeScript.ModuleFlags || (TypeScript.ModuleFlags = {}));
    var ModuleFlags = TypeScript.ModuleFlags;
    (function (SymbolFlags) {
        SymbolFlags._map = [];
        SymbolFlags.None = 0;
        SymbolFlags.Exported = 1;
        SymbolFlags.Private = 1 << 1;
        SymbolFlags.Public = 1 << 2;
        SymbolFlags.Ambient = 1 << 3;
        SymbolFlags.Static = 1 << 4;
        SymbolFlags.LocalStatic = 1 << 5;
        SymbolFlags.GetAccessor = 1 << 6;
        SymbolFlags.SetAccessor = 1 << 7;
        SymbolFlags.Property = 1 << 8;
        SymbolFlags.Readonly = 1 << 9;
        SymbolFlags.ModuleMember = 1 << 10;
        SymbolFlags.InterfaceMember = 1 << 11;
        SymbolFlags.ClassMember = 1 << 12;
        SymbolFlags.BuiltIn = 1 << 13;
        SymbolFlags.TypeSetDuringScopeAssignment = 1 << 14;
        SymbolFlags.Constant = 1 << 15;
        SymbolFlags.Optional = 1 << 16;
        SymbolFlags.RecursivelyReferenced = 1 << 17;
        SymbolFlags.Bound = 1 << 18;
        SymbolFlags.CompilerGenerated = 1 << 19;
    })(TypeScript.SymbolFlags || (TypeScript.SymbolFlags = {}));
    var SymbolFlags = TypeScript.SymbolFlags;
    (function (VarFlags) {
        VarFlags._map = [];
        VarFlags.None = 0;
        VarFlags.Exported = 1;
        VarFlags.Private = 1 << 1;
        VarFlags.Public = 1 << 2;
        VarFlags.Ambient = 1 << 3;
        VarFlags.Static = 1 << 4;
        VarFlags.LocalStatic = 1 << 5;
        VarFlags.GetAccessor = 1 << 6;
        VarFlags.SetAccessor = 1 << 7;
        VarFlags.AutoInit = 1 << 8;
        VarFlags.Property = 1 << 9;
        VarFlags.Readonly = 1 << 10;
        VarFlags.Class = 1 << 11;
        VarFlags.ClassProperty = 1 << 12;
        VarFlags.ClassBodyProperty = 1 << 13;
        VarFlags.ClassConstructorProperty = 1 << 14;
        VarFlags.ClassSuperMustBeFirstCallInConstructor = 1 << 15;
        VarFlags.Constant = 1 << 16;
        VarFlags.MustCaptureThis = 1 << 17;
    })(TypeScript.VarFlags || (TypeScript.VarFlags = {}));
    var VarFlags = TypeScript.VarFlags;
    (function (FncFlags) {
        FncFlags._map = [];
        FncFlags.None = 0;
        FncFlags.Exported = 1;
        FncFlags.Private = 1 << 1;
        FncFlags.Public = 1 << 2;
        FncFlags.Ambient = 1 << 3;
        FncFlags.Static = 1 << 4;
        FncFlags.LocalStatic = 1 << 5;
        FncFlags.GetAccessor = 1 << 6;
        FncFlags.SetAccessor = 1 << 7;
        FncFlags.Signature = 1 << 9;
        FncFlags.Method = 1 << 10;
        FncFlags.HasReturnExpression = 1 << 11;
        FncFlags.CallMember = 1 << 12;
        FncFlags.ConstructMember = 1 << 13;
        FncFlags.HasSelfReference = 1 << 14;
        FncFlags.IsFatArrowFunction = 1 << 15;
        FncFlags.IndexerMember = 1 << 16;
        FncFlags.IsFunctionExpression = 1 << 17;
        FncFlags.ClassMethod = 1 << 18;
        FncFlags.ClassPropertyMethodExported = 1 << 19;
        FncFlags.HasSuperReferenceInFatArrowFunction = 1 << 20;
        FncFlags.IsPropertyBound = 1 << 21;
    })(TypeScript.FncFlags || (TypeScript.FncFlags = {}));
    var FncFlags = TypeScript.FncFlags;
    (function (SignatureFlags) {
        SignatureFlags._map = [];
        SignatureFlags.None = 0;
        SignatureFlags.IsIndexer = 1;
        SignatureFlags.IsStringIndexer = 1 << 1;
        SignatureFlags.IsNumberIndexer = 1 << 2;
    })(TypeScript.SignatureFlags || (TypeScript.SignatureFlags = {}));
    var SignatureFlags = TypeScript.SignatureFlags;
                    function ToDeclFlags(fncOrVarOrSymbolOrModuleFlags) {
        return fncOrVarOrSymbolOrModuleFlags;
    }
    TypeScript.ToDeclFlags = ToDeclFlags;
    (function (TypeFlags) {
        TypeFlags._map = [];
        TypeFlags.None = 0;
        TypeFlags.HasImplementation = 1;
        TypeFlags.HasSelfReference = 1 << 1;
        TypeFlags.MergeResult = 1 << 2;
        TypeFlags.IsEnum = 1 << 3;
        TypeFlags.BuildingName = 1 << 4;
        TypeFlags.HasBaseType = 1 << 5;
        TypeFlags.HasBaseTypeOfObject = 1 << 6;
        TypeFlags.IsClass = 1 << 7;
    })(TypeScript.TypeFlags || (TypeScript.TypeFlags = {}));
    var TypeFlags = TypeScript.TypeFlags;
    (function (TypeRelationshipFlags) {
        TypeRelationshipFlags._map = [];
        TypeRelationshipFlags.SuccessfulComparison = 0;
        TypeRelationshipFlags.SourceIsNullTargetIsVoidOrUndefined = 1;
        TypeRelationshipFlags.RequiredPropertyIsMissing = 1 << 1;
        TypeRelationshipFlags.IncompatibleSignatures = 1 << 2;
        TypeRelationshipFlags.SourceSignatureHasTooManyParameters = 3;
        TypeRelationshipFlags.IncompatibleReturnTypes = 1 << 4;
        TypeRelationshipFlags.IncompatiblePropertyTypes = 1 << 5;
        TypeRelationshipFlags.IncompatibleParameterTypes = 1 << 6;
    })(TypeScript.TypeRelationshipFlags || (TypeScript.TypeRelationshipFlags = {}));
    var TypeRelationshipFlags = TypeScript.TypeRelationshipFlags;
    (function (CodeGenTarget) {
        CodeGenTarget._map = [];
        CodeGenTarget.ES3 = 0;
        CodeGenTarget.ES5 = 1;
    })(TypeScript.CodeGenTarget || (TypeScript.CodeGenTarget = {}));
    var CodeGenTarget = TypeScript.CodeGenTarget;
    (function (ModuleGenTarget) {
        ModuleGenTarget._map = [];
        ModuleGenTarget.Synchronous = 0;
        ModuleGenTarget.Asynchronous = 1;
        ModuleGenTarget.Local = 1 << 1;
    })(TypeScript.ModuleGenTarget || (TypeScript.ModuleGenTarget = {}));
    var ModuleGenTarget = TypeScript.ModuleGenTarget;
    TypeScript.codeGenTarget = CodeGenTarget.ES3;
    TypeScript.moduleGenTarget = ModuleGenTarget.Synchronous;
    TypeScript.optimizeModuleCodeGen = true;
    function flagsToString(e, flags) {
        var builder = "";
        for(var i = 1; i < (1 << 31); i = i << 1) {
            if((flags & i) != 0) {
                for(var k in e) {
                    if(e[k] == i) {
                        if(builder.length > 0) {
                            builder += "|";
                        }
                        builder += k;
                        break;
                    }
                }
            }
        }
        return builder;
    }
    TypeScript.flagsToString = flagsToString;
})(TypeScript || (TypeScript = {}));
