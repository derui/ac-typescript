var TypeScript;
(function (TypeScript) {
    function stripQuotes(str) {
        return str.replace("\"", "").replace("'", "").replace("'", "").replace("\"", "");
    }
    TypeScript.stripQuotes = stripQuotes;
    function isSingleQuoted(str) {
        return str.indexOf("'") != -1;
    }
    TypeScript.isSingleQuoted = isSingleQuoted;
    function isQuoted(str) {
        return str.indexOf("\"") != -1 || isSingleQuoted(str);
    }
    TypeScript.isQuoted = isQuoted;
    function quoteStr(str) {
        return "\"" + str + "\"";
    }
    TypeScript.quoteStr = quoteStr;
    function swapQuotes(str) {
        if(str.indexOf("\"") != -1) {
            str = str.replace("\"", "'");
            str = str.replace("\"", "'");
        } else {
            str = str.replace("'", "\"");
            str = str.replace("'", "\"");
        }
        return str;
    }
    TypeScript.swapQuotes = swapQuotes;
    function changeToSingleQuote(str) {
        if(str.indexOf("\"") != -1) {
            str = str.replace("\"", "'");
            str = str.replace("\"", "'");
        }
        return str;
    }
    TypeScript.changeToSingleQuote = changeToSingleQuote;
    function switchToForwardSlashes(path) {
        return path.replace(/\\/g, "/");
    }
    TypeScript.switchToForwardSlashes = switchToForwardSlashes;
    function trimModName(modName) {
        if(modName.length > 6 && modName.substring(modName.length - 6, modName.length) == ".d.str") {
            return modName.substring(0, modName.length - 6);
        }
        if(modName.length > 4 && modName.substring(modName.length - 4, modName.length) == ".str") {
            return modName.substring(0, modName.length - 4);
        }
        if(modName.length > 5 && modName.substring(modName.length - 5, modName.length) == ".d.ts") {
            return modName.substring(0, modName.length - 5);
        }
        if(modName.length > 3 && modName.substring(modName.length - 3, modName.length) == ".ts") {
            return modName.substring(0, modName.length - 3);
        }
        if(modName.length > 3 && modName.substring(modName.length - 3, modName.length) == ".js") {
            return modName.substring(0, modName.length - 3);
        }
        return modName;
    }
    TypeScript.trimModName = trimModName;
    function getDeclareFilePath(fname) {
        return isSTRFile(fname) ? changePathToDSTR(fname) : isTSFile(fname) ? changePathToDTS(fname) : changePathToDTS(fname);
    }
    TypeScript.getDeclareFilePath = getDeclareFilePath;
    function isFileOfExtension(fname, ext) {
        var invariantFname = fname.toLocaleUpperCase();
        var invariantExt = ext.toLocaleUpperCase();
        var extLength = invariantExt.length;
        return invariantFname.length > extLength && invariantFname.substring(invariantFname.length - extLength, invariantFname.length) == invariantExt;
    }
    function isJSFile(fname) {
        return isFileOfExtension(fname, ".js");
    }
    TypeScript.isJSFile = isJSFile;
    function isSTRFile(fname) {
        return isFileOfExtension(fname, ".str");
    }
    TypeScript.isSTRFile = isSTRFile;
    function isTSFile(fname) {
        return isFileOfExtension(fname, ".ts");
    }
    TypeScript.isTSFile = isTSFile;
    function isDSTRFile(fname) {
        return isFileOfExtension(fname, ".d.str");
    }
    TypeScript.isDSTRFile = isDSTRFile;
    function isDTSFile(fname) {
        return isFileOfExtension(fname, ".d.ts");
    }
    TypeScript.isDTSFile = isDTSFile;
    function getPrettyName(modPath, quote, treatAsFileName) {
        if (typeof quote === "undefined") { quote = true; }
        if (typeof treatAsFileName === "undefined") { treatAsFileName = false; }
        var modName = treatAsFileName ? switchToForwardSlashes(modPath) : trimModName(stripQuotes(modPath));
        var components = this.getPathComponents(modName);
        return components.length ? (quote ? quoteStr(components[components.length - 1]) : components[components.length - 1]) : modPath;
    }
    TypeScript.getPrettyName = getPrettyName;
    function getPathComponents(path) {
        return path.split("/");
    }
    TypeScript.getPathComponents = getPathComponents;
    function getRelativePathToFixedPath(fixedModFilePath, absoluteModPath) {
        absoluteModPath = switchToForwardSlashes(absoluteModPath);
        var modComponents = this.getPathComponents(absoluteModPath);
        var fixedModComponents = this.getPathComponents(fixedModFilePath);
        var joinStartIndex = 0;
        for(; joinStartIndex < modComponents.length && joinStartIndex < fixedModComponents.length; joinStartIndex++) {
            if(fixedModComponents[joinStartIndex] != modComponents[joinStartIndex]) {
                break;
            }
        }
        if(joinStartIndex != 0) {
            var relativePath = "";
            var relativePathComponents = modComponents.slice(joinStartIndex, modComponents.length);
            for(; joinStartIndex < fixedModComponents.length; joinStartIndex++) {
                if(fixedModComponents[joinStartIndex] != "") {
                    relativePath = relativePath + "../";
                }
            }
            return relativePath + relativePathComponents.join("/");
        }
        return absoluteModPath;
    }
    TypeScript.getRelativePathToFixedPath = getRelativePathToFixedPath;
    function quoteBaseName(modPath) {
        var modName = trimModName(stripQuotes(modPath));
        var path = getRootFilePath(modName);
        if(path == "") {
            return modPath;
        } else {
            var components = modName.split(path);
            var fileIndex = components.length > 1 ? 1 : 0;
            return quoteStr(components[fileIndex]);
        }
    }
    TypeScript.quoteBaseName = quoteBaseName;
    function changePathToSTR(modPath) {
        return trimModName(stripQuotes(modPath)) + ".str";
    }
    TypeScript.changePathToSTR = changePathToSTR;
    function changePathToDSTR(modPath) {
        return trimModName(stripQuotes(modPath)) + ".d.str";
    }
    TypeScript.changePathToDSTR = changePathToDSTR;
    function changePathToTS(modPath) {
        return trimModName(stripQuotes(modPath)) + ".ts";
    }
    TypeScript.changePathToTS = changePathToTS;
    function changePathToDTS(modPath) {
        return trimModName(stripQuotes(modPath)) + ".d.ts";
    }
    TypeScript.changePathToDTS = changePathToDTS;
    function isRelative(path) {
        return path.charAt(0) == ".";
    }
    TypeScript.isRelative = isRelative;
    function isRooted(path) {
        return path.charAt(0) == "\\" || path.charAt(0) == "/" || (path.indexOf(":\\") != -1) || (path.indexOf(":/") != -1);
    }
    TypeScript.isRooted = isRooted;
    function getRootFilePath(outFname) {
        if(outFname == "") {
            return outFname;
        } else {
            var isPath = outFname.indexOf("/") != -1;
            return isPath ? filePath(outFname) : "";
        }
    }
    TypeScript.getRootFilePath = getRootFilePath;
    function filePathComponents(fullPath) {
        fullPath = switchToForwardSlashes(fullPath);
        var components = getPathComponents(fullPath);
        return components.slice(0, components.length - 1);
    }
    TypeScript.filePathComponents = filePathComponents;
    function filePath(fullPath) {
        var path = filePathComponents(fullPath);
        return path.join("/") + "/";
    }
    TypeScript.filePath = filePath;
    function normalizeURL(url) {
        var hostDomainAndPortRegex = /^(https?:\/\/[\-\w\.]+(:\d+)?\/)(.*)$/i;
        var matches = hostDomainAndPortRegex.exec(url);
        if(matches) {
            var hostDomainAndPort = matches[1];
            var actualPath = matches[3];
            return hostDomainAndPort + normalizePath(actualPath);
        }
        return normalizePath(url);
    }
    TypeScript.normalizeURL = normalizeURL;
    TypeScript.pathNormalizeRegExp = /\//g;
    function normalizePath(path) {
        path = switchToForwardSlashes(path);
        var startedWithSep = path.charAt(0) === "/";
        var parts = this.getPathComponents(path);
        for(var i = 0; i < parts.length; i++) {
            if(parts[i] === "." || parts[i] === "") {
                parts.splice(i, 1);
                i--;
            }
            if(i > 0 && parts[i] === ".." && parts[i - 1] !== "..") {
                parts.splice(i - 1, 2);
                i -= 2;
            }
        }
        return (startedWithSep ? "/" : "") + parts.join("/");
    }
    TypeScript.normalizePath = normalizePath;
    function normalizeImportPath(path) {
        return normalizePath(path);
    }
    TypeScript.normalizeImportPath = normalizeImportPath;
})(TypeScript || (TypeScript = {}));
