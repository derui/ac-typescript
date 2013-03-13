///<reference path='typeScriptCompletion.ts'/>

declare var require : (name:string) => any;
declare var process : {argv:string[];};

class OptionItem {
    constructor(public longname:string, public shortname:string,public required=false,public value=true) {}
}

class Options {

    private findOption(argv:string, options:OptionItem[]) : OptionItem {
        var prefix = argv.match(/^(-+)(.+)$/);
        var isLong = true;
        var isValidOption = true;
        if (prefix == null) {
            return null;
        }

        if (prefix.length > 1) {
            if (prefix[1] == '-') {
                isLong = false;
            } else if (prefix[1] == '--') {
                isLong = true;
            } else {
                isValidOption = false;
            }
        }

        if (isValidOption) {
            var result = null;
            options.forEach((option) => {
                if (isLong) {
                    if (option.longname === prefix[2]) {
                        result = option;
                    }
                } else {
                    if (option.shortname === prefix[2]) {
                        result = option;
                    }
                }
            });
            return result;
        } else {
            return null;
        }
    }

    private getValue(option:OptionItem, index:number, argv:any) {
        if (index >= argv.length) {
            throw "less value for option : " + option.longname;
        }

        return argv[index + 1];
    }

    parse(options:OptionItem[]) {
        var result = {};
        var rest = [];
        for (var i = 2; i < process.argv.length; ++i) {
            var option = this.findOption(process.argv[i], options);
            if (option) {
                if (option.value) {
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
    }
}

var fs = require('fs');

var opts = new Options().parse(
    [new OptionItem("port", "p"),
     new OptionItem("debug", "d", false, false)]);

var debugging = opts["debug"] || false;
var port = parseInt(opts['port']) || 8124;

import services = module('typeScriptCompletion')

var typescriptLS = new services.TypeScriptLS();

typescriptLS.addDefaultLibrary();

var http = require('http');

var lsCache = null;

function log(mes) {
    if (debugging) {
        console.log(mes);
    }
}

function addFile(query) {
    if (query['file']) {
        typescriptLS.addFile(query['file'], true);

        log('add file : ' + query['file']);
        lsCache = typescriptLS.getLanguageService();
    }

    return '';
}

function basename(fname:string) : string {
    var index = fname.lastIndexOf('/');
    if (index == -1) {
        return fname;
    }

    return fname.slice(index);
}

function completion(query) {
    if (!query['file'] || !query['line'] || !query['column']) {
        throw "parameter for completion not enough";
    }

    var isMember = false;
    if (query['member']) {
        log("member is " + query['member']);
        isMember = query['member'] === 1 ? true : false;
    }

    var base = query['file'];
    var line = parseInt(query['line']);
    var column = parseInt(query['column']);
    log('completion start : file [' + query['file'] + "] line : " + line + " column : " + column
        + 'member : ' + isMember);

    if (lsCache == null) {
        lsCache = typescriptLS.getLanguageService();
    }

    var position = typescriptLS.lineColToPosition(base, line, column);
    return lsCache.languageService.getCompletionsAtPosition(base, position, isMember).entries;
}

function updateFile(query) {
    var content = decodeURIComponent(query['content']);
    log('update file : file [' + query['file'] + '] : length -> ' + content.length);

    typescriptLS.updateScript(query['file'], content, true);

    lsCache = typescriptLS.getLanguageService();
    return '';
}

function updateScript(query) {
    log('update range in file : ' + query['file'] + " : " +
                query['prev'] + ' : ' + query['next'] + ' => ' + query['text']);

    typescriptLS.editScript(query['file'], parseInt(query['prev']), parseInt(query['next']),
                            decodeURIComponent(query['text']));
    lsCache = typescriptLS.getLanguageService();
    return '';
}

var methodHandler = {
    'add' : addFile,
    'completion' : completion,
    'update-file' : updateFile,
    'update' : updateScript

};

var sockjs = require('sockjs');
var completionServer = sockjs.createServer();

completionServer.on('connection', (ws) => {
    log("client connected");
    ws.on('data', (data) => {
        var json = JSON.parse(data);

        if (json['method'] && methodHandler[json['method']]) {
            var response = "";
            try {
                response = methodHandler[json['method']](json);
            } catch (e) {
                console.log(e);
            }
            if (json['method'] && json['method'] === 'completion') {
                 ws.write(JSON.stringify(response));
            }
        }
    });
});

var server = http.createServer();
completionServer.installHandlers(server, {});
server.listen(port, "localhost");
