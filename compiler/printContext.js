var TypeScript;
(function (TypeScript) {
    var PrintContext = (function () {
        function PrintContext(outfile, parser) {
            this.outfile = outfile;
            this.parser = parser;
            this.builder = "";
            this.indent1 = "  ";
            this.indentStrings = [];
            this.indentAmt = 0;
        }
        PrintContext.prototype.increaseIndent = function () {
            this.indentAmt++;
        };
        PrintContext.prototype.decreaseIndent = function () {
            this.indentAmt--;
        };
        PrintContext.prototype.startLine = function () {
            if(this.builder.length > 0) {
                TypeScript.CompilerDiagnostics.Alert(this.builder);
            }
            var indentString = this.indentStrings[this.indentAmt];
            if(indentString === undefined) {
                indentString = "";
                for(var i = 0; i < this.indentAmt; i++) {
                    indentString += this.indent1;
                }
                this.indentStrings[this.indentAmt] = indentString;
            }
            this.builder += indentString;
        };
        PrintContext.prototype.write = function (s) {
            this.builder += s;
        };
        PrintContext.prototype.writeLine = function (s) {
            this.builder += s;
            this.outfile.WriteLine(this.builder);
            this.builder = "";
        };
        return PrintContext;
    })();
    TypeScript.PrintContext = PrintContext;    
    function prePrintAST(ast, parent, walker) {
        var pc = walker.state;
        ast.print(pc);
        pc.increaseIndent();
        return ast;
    }
    TypeScript.prePrintAST = prePrintAST;
    function postPrintAST(ast, parent, walker) {
        var pc = walker.state;
        pc.decreaseIndent();
        return ast;
    }
    TypeScript.postPrintAST = postPrintAST;
})(TypeScript || (TypeScript = {}));
