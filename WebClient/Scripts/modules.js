// https://stackoverflow.com/questions/14521108/dynamically-load-js-inside-js
// https://yuiblog.com/blog/2007/06/12/module-pattern/

function assigned(aVariable) {
    return (typeof aVariable !== 'undefined');
}

// aURL is URL of external file, 
// aModuleName (optional) is the name of the object in the module that init is called on after load
// aLocation (optional) is the location to insert the <script> element
var loadModule = function (aURL, aModuleName, aLocation) {
    var script = document.createElement('script');
    script.src = aURL;
    if (!assigned(aLocation))
        aLocation = document.body;
    if (!assigned(aModuleName)) {
        // derive module name from url: last element of split on "/" then first element of split on "."
        var urlParts = aURL.split("/");
        aModuleNameParts = (urlParts.length > 0 ? urlParts[urlParts.length - 1] : aURL).split(".");
        aModuleName = aModuleNameParts.length > 0 ? aModuleNameParts[0] : aModuleName;
    }
    var loadFunction = function () {
        // find the global module object and call init on it
        window[aModuleName].init();
    }
    script.onload = loadFunction;
    script.onreadystatechange = loadFunction;
    aLocation.appendChild(script);
};

// example
// loadModule("Scripts/Modules/testModule.js");
