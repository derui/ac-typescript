var Formatting;
(function (Formatting) {
    (function (FormattingRequestKind) {
        FormattingRequestKind._map = [];
        FormattingRequestKind._map[0] = "FormatDocument";
        FormattingRequestKind.FormatDocument = 0;
        FormattingRequestKind._map[1] = "FormatSelection";
        FormattingRequestKind.FormatSelection = 1;
        FormattingRequestKind._map[2] = "FormatOnEnter";
        FormattingRequestKind.FormatOnEnter = 2;
        FormattingRequestKind._map[3] = "FormatOnSemicolon";
        FormattingRequestKind.FormatOnSemicolon = 3;
        FormattingRequestKind._map[4] = "FormatOnClosingCurlyBrace";
        FormattingRequestKind.FormatOnClosingCurlyBrace = 4;
        FormattingRequestKind._map[5] = "FormatOnPaste";
        FormattingRequestKind.FormatOnPaste = 5;
    })(Formatting.FormattingRequestKind || (Formatting.FormattingRequestKind = {}));
    var FormattingRequestKind = Formatting.FormattingRequestKind;
})(Formatting || (Formatting = {}));
