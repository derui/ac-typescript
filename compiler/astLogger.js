var TypeScript;
(function (TypeScript) {
    var AstLogger = (function () {
        function AstLogger(logger) {
            this.logger = logger;
        }
        AstLogger.prototype.logScript = function (script) {
            var _this = this;
            this.logLinemap(script.locationInfo.lineMap);
            var stack = [];
            var pre = function (cur, parent) {
                stack.push(cur);
                var indent = (stack.length - 1) * 2;
                _this.logComments(script, cur.preComments, indent);
                _this.logNode(script, cur, indent);
                _this.logComments(script, cur.postComments, indent);
                return cur;
            };
            var post = function (cur, parent) {
                stack.pop();
                return cur;
            };
            TypeScript.getAstWalkerFactory().walk(script, pre, post);
        };
        AstLogger.prototype.logNode = function (script, cur, indent) {
            var msg = this.addPadding("", indent, "| ", true);
            msg = msg.concat("+ " + cur.treeViewLabel());
            msg = this.addPadding(msg, 70, " ", false);
            msg = msg + this.addLineColumn(script, cur.minChar);
            msg = this.addPadding(msg, 80, " ", false);
            msg = msg + "=> ";
            msg = msg + this.addLineColumn(script, cur.limChar);
            msg = this.addPadding(msg, 102, " ", false);
            msg = msg.concat("[" + this.addPadding(cur.minChar.toString(), 1, " ", true) + ", " + this.addPadding(cur.limChar.toString(), 1, " ", true) + "]");
            msg = this.addPadding(msg, 115, " ", false);
            msg = msg.concat("sym=" + (cur).sym);
            msg = this.addPadding(msg, 135, " ", false);
            msg = msg.concat("type=" + (cur.type === null ? "null" : cur.type.getTypeName()));
            this.logger.log(msg);
        };
        AstLogger.prototype.logComments = function (script, comments, indent) {
            if(comments == null) {
                return;
            }
            for(var i = 0; i < comments.length; i++) {
                this.logNode(script, comments[i], indent);
            }
        };
        AstLogger.prototype.logLinemap = function (linemap) {
            var result = "[";
            for(var i = 0; i < linemap.length; i++) {
                if(i > 0) {
                    result += ",";
                }
                result += linemap[i];
            }
            result += "]";
            this.logger.log("linemap: " + result);
        };
        AstLogger.prototype.addPadding = function (s, targetLength, paddingString, leftPadding) {
            var result = (leftPadding ? "" : s);
            for(var i = s.length; i < targetLength; i++) {
                result = result + paddingString;
            }
            result = result + (leftPadding ? s : "");
            return result;
        };
        AstLogger.prototype.addLineColumn = function (script, position) {
            var lineInfo = {
                line: -1,
                col: -1
            };
            TypeScript.getSourceLineColFromMap(lineInfo, position, script.locationInfo.lineMap);
            if(lineInfo.col !== -1) {
                lineInfo.col++;
            }
            return "(" + lineInfo.line + ", " + lineInfo.col + ")";
        };
        return AstLogger;
    })();
    TypeScript.AstLogger = AstLogger;    
})(TypeScript || (TypeScript = {}));
