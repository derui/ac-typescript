var Services;
(function (Services) {
    function copyDataObject(dst, src) {
        for(var e in dst) {
            if(typeof dst[e] == "object") {
                copyDataObject(dst[e], src[e]);
            } else if(typeof dst[e] != "function") {
                dst[e] = src[e];
            }
        }
        return dst;
    }
    Services.copyDataObject = copyDataObject;
    function compareDataObjects(dst, src) {
        for(var e in dst) {
            if(typeof dst[e] == "object") {
                if(!compareDataObjects(dst[e], src[e])) {
                    return false;
                }
            } else if(typeof dst[e] != "function") {
                if(dst[e] !== src[e]) {
                    return false;
                }
            }
        }
        return true;
    }
    Services.compareDataObjects = compareDataObjects;
    var TypeScriptServicesFactory = (function () {
        function TypeScriptServicesFactory() { }
        TypeScriptServicesFactory.prototype.createLanguageService = function (host) {
            try  {
                return new Services.LanguageService(host);
            } catch (err) {
                Services.logInternalError(host, err);
                throw err;
            }
        };
        TypeScriptServicesFactory.prototype.createLanguageServiceShim = function (host) {
            try  {
                var hostAdapter = new Services.LanguageServiceShimHostAdapter(host);
                var languageService = this.createLanguageService(hostAdapter);
                return new Services.LanguageServiceShim(host, languageService);
            } catch (err) {
                Services.logInternalError(host, err);
                throw err;
            }
        };
        TypeScriptServicesFactory.prototype.createClassifier = function (host) {
            try  {
                return new Services.Classifier(host);
            } catch (err) {
                Services.logInternalError(host, err);
                throw err;
            }
        };
        TypeScriptServicesFactory.prototype.createClassifierShim = function (host) {
            try  {
                return new Services.ClassifierShim(host);
            } catch (err) {
                Services.logInternalError(host, err);
                throw err;
            }
        };
        TypeScriptServicesFactory.prototype.createCoreServices = function (host) {
            try  {
                return new Services.CoreServices(host);
            } catch (err) {
                Services.logInternalError(host.logger, err);
                throw err;
            }
        };
        TypeScriptServicesFactory.prototype.createCoreServicesShim = function (host) {
            try  {
                return new Services.CoreServicesShim(host);
            } catch (err) {
                Services.logInternalError(host.logger, err);
                throw err;
            }
        };
        return TypeScriptServicesFactory;
    })();
    Services.TypeScriptServicesFactory = TypeScriptServicesFactory;    
})(Services || (Services = {}));
