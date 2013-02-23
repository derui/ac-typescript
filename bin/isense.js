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
    new OptionItem("port", "p"), 
    new OptionItem("debug", "d", false, false)
]);
var port = parseInt(opts['port']) || 8124;
var services = require("./typeScriptCompletion")
var typescriptLS = new services.TypeScriptLS();
typescriptLS.addDefaultLibrary();
var http = require('http');
var url = require('url');
var querystring = require('querystring');
var lsCache = null;
function addFile(query) {
    if(query['file']) {
        typescriptLS.addFile(query['file'], true);
        console.log('add file : ' + query['file']);
        lsCache = typescriptLS.getLanguageService();
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
    if(lsCache == null) {
        lsCache = typescriptLS.getLanguageService();
    }
    var position = typescriptLS.lineColToPosition(base, line, column);
    return lsCache.languageService.getCompletionsAtPosition(base, position, isMember).entries;
}
function updateFile(query, post) {
    console.log('update file : file [' + query['file'] + ']');
    typescriptLS.updateScript(query['file'], post, true);
    lsCache = typescriptLS.getLanguageService();
    return '';
}
function updateScript(query) {
    console.log('update range in file : ' + query['file'] + " : " + query['prev'] + ' : ' + query['next'] + ' => ' + query['text']);
    typescriptLS.editScript(query['file'], parseInt(query['prev']), parseInt(query['next']), query['text']);
    lsCache = typescriptLS.getLanguageService();
    return '';
}
var methodHandler = {
    'add': addFile,
    'completion': completion,
    'update-file': updateFile,
    'update': updateScript
};
me;
http.createServer(function (req, res) {
    var postdata = "";
    var query = querystring.parse(url.parse(req.url).query);
    res.writeHead(200, {
        'Content-Type': 'application/json'
    });
    try  {
        var response = '';
        if(req.method === 'POST') {
            req.addListener('data', function (chunk) {
                postdata += chunk;
            });
            req.addListener('end', function (chunk) {
                methodHandler[query['method']](query, postdata);
            });
        } else if(req.method === 'GET') {
            if(query['method']) {
                if(methodHandler[query['method']]) {
                    response = methodHandler[query['method']](query);
                }
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
