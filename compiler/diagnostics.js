var TypeScript;
(function (TypeScript) {
    (function (CompilerDiagnostics) {
        CompilerDiagnostics.debug = false;
        CompilerDiagnostics.diagnosticWriter = null;
        CompilerDiagnostics.analysisPass = 0;
        function Alert(output) {
            if(CompilerDiagnostics.diagnosticWriter) {
                CompilerDiagnostics.diagnosticWriter.Alert(output);
            }
        }
        CompilerDiagnostics.Alert = Alert;
        function debugPrint(s) {
            if(CompilerDiagnostics.debug) {
                Alert(s);
            }
        }
        CompilerDiagnostics.debugPrint = debugPrint;
        function assert(condition, s) {
            if(CompilerDiagnostics.debug) {
                if(!condition) {
                    Alert(s);
                }
            }
        }
        CompilerDiagnostics.assert = assert;
    })(TypeScript.CompilerDiagnostics || (TypeScript.CompilerDiagnostics = {}));
    var CompilerDiagnostics = TypeScript.CompilerDiagnostics;
    var NullLogger = (function () {
        function NullLogger() { }
        NullLogger.prototype.information = function () {
            return false;
        };
        NullLogger.prototype.debug = function () {
            return false;
        };
        NullLogger.prototype.warning = function () {
            return false;
        };
        NullLogger.prototype.error = function () {
            return false;
        };
        NullLogger.prototype.fatal = function () {
            return false;
        };
        NullLogger.prototype.log = function (s) {
        };
        return NullLogger;
    })();
    TypeScript.NullLogger = NullLogger;    
    var LoggerAdapter = (function () {
        function LoggerAdapter(logger) {
            this.logger = logger;
            this._information = this.logger.information();
            this._debug = this.logger.debug();
            this._warning = this.logger.warning();
            this._error = this.logger.error();
            this._fatal = this.logger.fatal();
        }
        LoggerAdapter.prototype.information = function () {
            return this._information;
        };
        LoggerAdapter.prototype.debug = function () {
            return this._debug;
        };
        LoggerAdapter.prototype.warning = function () {
            return this._warning;
        };
        LoggerAdapter.prototype.error = function () {
            return this._error;
        };
        LoggerAdapter.prototype.fatal = function () {
            return this._fatal;
        };
        LoggerAdapter.prototype.log = function (s) {
            this.logger.log(s);
        };
        return LoggerAdapter;
    })();
    TypeScript.LoggerAdapter = LoggerAdapter;    
    var BufferedLogger = (function () {
        function BufferedLogger() {
            this.logContents = [];
        }
        BufferedLogger.prototype.information = function () {
            return false;
        };
        BufferedLogger.prototype.debug = function () {
            return false;
        };
        BufferedLogger.prototype.warning = function () {
            return false;
        };
        BufferedLogger.prototype.error = function () {
            return false;
        };
        BufferedLogger.prototype.fatal = function () {
            return false;
        };
        BufferedLogger.prototype.log = function (s) {
            this.logContents.push(s);
        };
        return BufferedLogger;
    })();
    TypeScript.BufferedLogger = BufferedLogger;    
    function timeFunction(logger, funcDescription, func) {
        var start = +new Date();
        var result = func();
        var end = +new Date();
        logger.log(funcDescription + " completed in " + (end - start) + " msec");
        return result;
    }
    TypeScript.timeFunction = timeFunction;
    function stringToLiteral(value, length) {
        var result = "";
        var addChar = function (index) {
            var ch = value.charCodeAt(index);
            switch(ch) {
                case 9:
                    result += "\\t";
                    break;
                case 10:
                    result += "\\n";
                    break;
                case 11:
                    result += "\\v";
                    break;
                case 12:
                    result += "\\f";
                    break;
                case 13:
                    result += "\\r";
                    break;
                case 34:
                    result += "\\\"";
                    break;
                case 39:
                    result += "\\\'";
                    break;
                case 92:
                    result += "\\";
                    break;
                default:
                    result += value.charAt(index);
            }
        };
        var tooLong = (value.length > length);
        if(tooLong) {
            var mid = length >> 1;
            for(var i = 0; i < mid; i++) {
                addChar(i);
            }
            result += "(...)";
            for(var i = value.length - mid; i < value.length; i++) {
                addChar(i);
            }
        } else {
            length = value.length;
            for(var i = 0; i < length; i++) {
                addChar(i);
            }
        }
        return result;
    }
    TypeScript.stringToLiteral = stringToLiteral;
})(TypeScript || (TypeScript = {}));
