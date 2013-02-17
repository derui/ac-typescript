var OptionItem = (function () {
    function OptionItem(longname, shortname, required, value) {
        if (typeof required === "undefined") { required = false; }
        if (typeof value === "undefined") { value = true; }
        this.longname = longname;
        this.shortname = shortname;
        this.required = required;
        this.value = value;
    }
    return OptionItem;
})();
var Options = (function () {
    function Options() { }
    Options.prototype.findOption = function (argv, options) {
        var prefix = argv.match(/^(-+)(.+)$/);
        var isLong = true;
        var isValidOption = true;
        if(prefix == null) {
            return null;
        }
        if(prefix.length > 1) {
            if(prefix[1] == '-') {
                isLong = false;
            } else if(prefix[1] == '--') {
                isLong = true;
            } else {
                isValidOption = false;
            }
        }
        if(isValidOption) {
            var result = null;
            options.forEach(function (option) {
                if(isLong) {
                    if(option.longname === prefix[2]) {
                        result = option;
                    }
                } else {
                    if(option.shortname === prefix[2]) {
                        result = option;
                    }
                }
            });
            return result;
        } else {
            return null;
        }
    };
    Options.prototype.getValue = function (option, index, argv) {
        if(index >= argv.length) {
            throw "less value for option : " + option.longname;
        }
        return argv[index + 1];
    };
    Options.prototype.parse = function (options) {
        var result = {
        };
        var rest = [];
        for(var i = 2; i < process.argv.length; ++i) {
            var option = this.findOption(process.argv[i], options);
            if(option) {
                if(option.value) {
                    var value = this.getValue(option, i, process.argv);
                    result[option.longname] = value;
                    i++;
                }
            } else {
                rest.push(process.argv[i]);
            }
        }
        result['rest'] = rest;
        return result;
    };
    return Options;
})();
var fs = require('fs');
var opts = new Options().parse([
    new OptionItem("port", "p")
]);
var port = parseInt(opts['port']) || 8124;
var services = require("./typeScriptCompletion")
var typescriptLS = new services.TypeScriptLS();
typescriptLS.addDefaultLibrary();
var http = require('http');
var url = require('url');
var querystring = require('querystring');
function addFile(query) {
    if(query['file']) {
        typescriptLS.addFile(query['file'], true);
        console.log('add file : ' + query['file']);
    }
    return '';
}
function basename(fname) {
    var index = fname.lastIndexOf('/');
    if(index == -1) {
        return fname;
    }
    return fname.slice(index);
}
function completion(query) {
    if(!query['file'] || !query['line'] || !query['column']) {
        throw "parameter for completion not enough";
    }
    var isMember = false;
    if(query['member']) {
        isMember = query['member'] === '1' ? true : false;
    }
    var base = query['file'];
    var line = parseInt(query['line']);
    var column = parseInt(query['column']);
    console.log('completion start : file [' + query['file'] + "] line : " + line + " column : " + column);
    var ls = typescriptLS.getLanguageService();
    var position = typescriptLS.lineColToPosition(base, line, column);
    return ls.languageService.getCompletionsAtPosition(base, position, isMember).entries;
}
var methodHandler = {
    'add': addFile,
    'completion': completion
};
http.createServer(function (req, res) {
    var query = querystring.parse(url.parse(req.url).query);
    res.writeHead(200, {
        'Content-Type': 'application/json'
    });
    console.log(query);
    try  {
        var response = '';
        if(query['method']) {
            if(methodHandler[query['method']]) {
                response = methodHandler[query['method']](query);
            }
        }
        res.end(JSON.stringify(response));
    } catch (e) {
        console.log(e.toString());
        res.writeHead(403, {
            'Content-Type': 'application/json'
        });
        res.end('');
    }
}).listen(port, '127.0.0.1');