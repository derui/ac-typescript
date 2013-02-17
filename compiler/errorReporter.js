var TypeScript;
(function (TypeScript) {
    var ErrorReporter = (function () {
        function ErrorReporter(outfile) {
            this.outfile = outfile;
            this.parser = null;
            this.checker = null;
            this.lineCol = {
                line: 0,
                col: 0
            };
            this.emitAsComments = true;
            this.hasErrors = false;
            this.pushToErrorSink = false;
            this.errorSink = [];
        }
        ErrorReporter.prototype.getCapturedErrors = function () {
            return this.errorSink;
        };
        ErrorReporter.prototype.freeCapturedErrors = function () {
            this.errorSink = [];
        };
        ErrorReporter.prototype.captureError = function (emsg) {
            this.errorSink[this.errorSink.length] = emsg;
        };
        ErrorReporter.prototype.setErrOut = function (outerr) {
            this.outfile = outerr;
            this.emitAsComments = false;
        };
        ErrorReporter.prototype.emitPrefix = function () {
            if(this.emitAsComments) {
                this.outfile.Write("// ");
            }
            this.outfile.Write(this.checker.locationInfo.filename + "(" + this.lineCol.line + "," + this.lineCol.col + "): ");
        };
        ErrorReporter.prototype.writePrefix = function (ast) {
            if(ast) {
                this.setError(ast);
            } else {
                this.lineCol.line = 0;
                this.lineCol.col = 0;
            }
            this.emitPrefix();
        };
        ErrorReporter.prototype.writePrefixFromSym = function (symbol) {
            if(symbol && this.checker.locationInfo.lineMap) {
                TypeScript.getSourceLineColFromMap(this.lineCol, symbol.location, this.checker.locationInfo.lineMap);
            } else {
                this.lineCol.line = -1;
                this.lineCol.col = -1;
            }
            this.emitPrefix();
        };
        ErrorReporter.prototype.setError = function (ast) {
            if(ast) {
                ast.flags |= TypeScript.ASTFlags.Error;
                if(this.checker.locationInfo.lineMap) {
                    TypeScript.getSourceLineColFromMap(this.lineCol, ast.minChar, this.checker.locationInfo.lineMap);
                }
            }
        };
        ErrorReporter.prototype.reportError = function (ast, message) {
            if(this.pushToErrorSink) {
                this.captureError(message);
                return;
            }
            this.hasErrors = true;
            if(ast && this.parser.errorRecovery && this.parser.errorCallback) {
                var len = (ast.limChar - ast.minChar);
                this.parser.errorCallback(ast.minChar, len, message, this.checker.locationInfo.unitIndex);
            } else {
                this.writePrefix(ast);
                this.outfile.WriteLine(message);
            }
        };
        ErrorReporter.prototype.reportErrorFromSym = function (symbol, message) {
            if(this.pushToErrorSink) {
                this.captureError(message);
                return;
            }
            this.hasErrors = true;
            if(this.parser.errorRecovery && this.parser.errorCallback) {
                this.parser.errorCallback(symbol.location, symbol.length, message, this.checker.locationInfo.unitIndex);
            } else {
                this.writePrefixFromSym(symbol);
                this.outfile.WriteLine(message);
            }
        };
        ErrorReporter.prototype.emitterError = function (ast, message) {
            this.reportError(ast, message);
            throw Error("EmitError");
        };
        ErrorReporter.prototype.duplicateIdentifier = function (ast, name) {
            this.reportError(ast, "Duplicate identifier '" + name + "'");
        };
        ErrorReporter.prototype.showRef = function (ast, text, symbol) {
            var defLineCol = {
                line: -1,
                col: -1
            };
            this.parser.getSourceLineCol(defLineCol, symbol.location);
            this.reportError(ast, "symbol " + text + " defined at (" + defLineCol.line + "," + defLineCol.col + ")");
        };
        ErrorReporter.prototype.unresolvedSymbol = function (ast, name) {
            this.reportError(ast, "The name '" + name + "' does not exist in the current scope");
        };
        ErrorReporter.prototype.symbolDoesNotReferToAValue = function (ast, name) {
            this.reportError(ast, "The name '" + name + "' does not refer to a value");
        };
        ErrorReporter.prototype.styleError = function (ast, msg) {
            var bkThrow = this.pushToErrorSink;
            this.pushToErrorSink = false;
            this.reportError(ast, "STYLE: " + msg);
            this.pushToErrorSink = bkThrow;
        };
        ErrorReporter.prototype.simpleError = function (ast, msg) {
            this.reportError(ast, msg);
        };
        ErrorReporter.prototype.simpleErrorFromSym = function (sym, msg) {
            this.reportErrorFromSym(sym, msg);
        };
        ErrorReporter.prototype.invalidSuperReference = function (ast) {
            this.simpleError(ast, "Keyword 'super' can only be used inside a class instance method");
        };
        ErrorReporter.prototype.valueCannotBeModified = function (ast) {
            this.simpleError(ast, "The left-hand side of an assignment expression must be a variable, property or indexer");
        };
        ErrorReporter.prototype.invalidCall = function (ast, nodeType, scope) {
            var targetType = ast.target.type;
            var typeName = targetType.getScopedTypeName(scope);
            if(targetType.construct && (nodeType == TypeScript.NodeType.Call)) {
                this.reportError(ast, "Value of type '" + typeName + "' is not callable.  Did you mean to include 'new'?");
            } else {
                var catString = (nodeType == TypeScript.NodeType.Call) ? "callable" : "newable";
                this.reportError(ast, "Value of type '" + typeName + "' is not " + catString);
            }
        };
        ErrorReporter.prototype.indexLHS = function (ast, scope) {
            var targetType = ast.operand1.type.getScopedTypeName(scope);
            var indexType = ast.operand2.type.getScopedTypeName(scope);
            this.simpleError(ast, "Value of type '" + targetType + "' is not indexable by type '" + indexType + "'");
        };
        ErrorReporter.prototype.incompatibleTypes = function (ast, t1, t2, op, scope, comparisonInfo) {
            if(!t1) {
                t1 = this.checker.anyType;
            }
            if(!t2) {
                t2 = this.checker.anyType;
            }
            var reason = comparisonInfo ? comparisonInfo.message : "";
            if(op) {
                this.reportError(ast, "Operator '" + op + "' cannot be applied to types '" + t1.getScopedTypeName(scope) + "' and '" + t2.getScopedTypeName(scope) + "'" + (reason ? ": " + reason : ""));
            } else {
                this.reportError(ast, "Cannot convert '" + t1.getScopedTypeName(scope) + "' to '" + t2.getScopedTypeName(scope) + "'" + (reason ? ": " + reason : ""));
            }
        };
        ErrorReporter.prototype.expectedClassOrInterface = function (ast) {
            this.simpleError(ast, "Expected var, class, interface, or module");
        };
        ErrorReporter.prototype.unaryOperatorTypeError = function (ast, op, type) {
            this.reportError(ast, "Operator '" + op + "' cannot be applied to type '" + type.getTypeName() + "'");
        };
        return ErrorReporter;
    })();
    TypeScript.ErrorReporter = ErrorReporter;    
})(TypeScript || (TypeScript = {}));
