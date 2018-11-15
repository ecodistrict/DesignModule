// https://www.accelebrate.com/blog/file-uploads-web-sockets-part-3-of-3/
// http://jsbin.com/acohaz/26/edit?html,js,output
// https://stackoverflow.com/questions/32207504/correct-way-to-trigger-file-download

// https://github.com/eligrey/FileSaver.js
// https://stackoverflow.com/questions/25810051/filereader-api-on-big-files

L.Control.Files = L.Control.extend({
    options: {
        collapsed: true,
        position: 'bottomleft',
        autoZIndex: true
    },

    initialize: function (options) {
        L.setOptions(this, options);
        this._filesItems = [];
        this._filesList;
        this._downloads = {};
    },

    onAdd: function (map) {
        this._initLayout();
        this._update();
        this._map = map;
        return this._container;
    },

    onRemove: function () {
        // nothing to do
    },

    _initLayout: function () {
        var className = 'leaflet-control-files',
            container = this._container = L.DomUtil.create('div', className);

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }

        var form = this._form = L.DomUtil.create('form', className + '-list');
        // expanded or collapsed
        if (this.options.collapsed) {
            if (!L.Browser.android) {
                L.DomEvent.on(container, {
                    mouseenter: this._expand,
                    mouseleave: this._collapse
                }, this);
            }
            var link = this._layersLink = L.DomUtil.create('a', className + '-toggle', container);
            link.href = '#';
            link.title = 'Files available for download';
            if (L.Browser.touch) {
                L.DomEvent
                    .on(link, 'click', L.DomEvent.stop)
                    .on(link, 'click', this._expand, this);
            } else {
                L.DomEvent.on(link, 'focus', this._expand, this);
            }
            this._map.on('click', this._collapse, this);
        } else
            this._expand();
        // fill form
        form.appendChild(document.createTextNode("download files"));
        L.DomUtil.create('br', '', form);
        this._filesList = L.DomUtil.create('div', className + '-base', form);
        this._separator = L.DomUtil.create('div', className + '-separator', form);
        form.appendChild(document.createTextNode("upload files"));
        L.DomUtil.create('br', '', form);
        this._uploadButton = L.DomUtil.create('div', className + '-upload', form);
        var innerFileBrowse = L.DomUtil.create('input', 'button files-button', this._uploadButton);
        innerFileBrowse.type = 'file';
        innerFileBrowse.multiple = true;    
        L.DomEvent.on(innerFileBrowse, "change", function (e) { this._uploadFiles(e.target.files); }, this);
        container.appendChild(form);

        // add file drop event
        container.ondragover = function (e) {
            return false;
        };
        container.ondragend = function (e) {
            return false;
        };
        container.ondragenter = function (e) {
            e.preventDefault();
            container.style.background = "lightgray";
            return false;
        };
        container.ondragleave = function (e) {
            e.preventDefault();
            container.removeAttribute("style");
            return false;
        };
        // use following construct instead ondrop to get this pointed to correct instance
        L.DomEvent.on(container, "drop", function (e) {
            e.preventDefault();
            this._uploadFiles(e.dataTransfer.files); // todo: seems dataTransfer = null in newest firefox.. seems bug..
            container.removeAttribute("style");
        }, this);
    },

    _update: function () {
        if (!this._container) { return this; }
        // clear all
        L.DomUtil.empty(this._filesList);
        // fill
        if (this._filesItems.length > 0) {
            for (var i = this._filesItems.length - 1; i >= 0; i--) {
                this._makeFilesItem(this._filesItems[i]);
            }
        }
        else {
            var label = document.createElement('label');
            label.className = 'files-empty-line';
            var name = document.createElement('span');
            name.className = 'files-empty-name';
            name.innerHTML = "No files available";
            label.appendChild(name);
            this._filesList.appendChild(label);
        }
        return this;
    },

    _makeFilesItem: function (item) {
        var row = document.createElement('div');
        row.className = 'files-row';
        var name = document.createElement('div');
        name.className = 'files-filename';
        //name.innerHTML = item.fileName;
        var sp = document.createElement('span');
        sp.innerHTML = item.fileName;
        name.appendChild(sp);
        row.appendChild(name);
        for (var ft in item.fileTypes) {
            var filetype = document.createElement('div');
            filetype.className = 'files-filetype';
            var sp2 = document.createElement('span');
            sp2.fileName = item.fileName;
            sp2.fileType = item.fileTypes[ft];
            sp2.innerHTML = sp2.fileType;
            L.DomEvent.on(sp2, 'click', this._downloadFile, this);
            filetype.appendChild(sp2);
            row.appendChild(filetype);
        }
        // add row to table div
        this._filesList.appendChild(row);
    },

    _downloadFile: function (e) {
        // always single file of specific type
        wsSend({
            type: "downloadFile",
            payload: { fileName: e.currentTarget.fileName, fileType: e.currentTarget.fileType }
        });
    },

    _uploadFiles: function (files) {
        // can be multiple files
        // iterate over the files to upload
        for (var x = 0; x < files.length; x++) {
            var fileBlockSize = 20*1024;
            var fileSliceSize = fileBlockSize * 1000; 
            var file = files[x];
            // instantiate a new FileReader object
            // loading files from the file system is an asynchronous operation, run this function when the loading process is complete
            var fr = new FileReader();

            // read and send in chunks; load file in slices and send slice in blocks
            fr.onloadend = function (e) {
                // send the file in blocks over web sockets
                message = {};
                message.type = "uploadFile";
                message.payload = {};
                message.payload.fileName = e.target.file.name;
                message.payload.fileType = e.target.file.type;
                message.payload.fileSize = e.target.file.size;
                message.payload.fileBlockOffset = e.target.fileOffset;
                var binary = "";
                var bytes = new Uint8Array(e.target.result);
                var len = bytes.byteLength;
                for (var i = 0; i < len; i++) {
                    // check if previous block can be send
                    if (i > 0 && i % fileBlockSize === 0) {
                        // send last block
                        message.payload.fileContents = window.btoa(binary);
                        wsSend(message);
                        // reset buffer
                        e.target.fileOffset += binary.length;
                        message.payload.fileBlockOffset = e.target.fileOffset;
                        binary = "";
                        message.payload.fileContents = ""; 
                    }
                    binary += String.fromCharCode(bytes[i]);
                }
                if (binary !== "") {
                    // send last blob
                    message.payload.fileContents = window.btoa(binary);
                    wsSend(message);
                    e.target.fileOffset += binary.length;
                    binary = "";
                    message.payload.fileContents = ""; 
                }
                // check if we have to send an other slice
                if (e.target.fileOffset < e.target.file.size) {
                    // initiate send of next slice
                    fr.readAsArrayBuffer(e.target.file.slice(e.target.fileOffset, e.target.fileOffset + fileSliceSize));
                }
            };
            // trigger load of slice of file into an array buffer (async)
            fr.file = file;
            fr.fileOffset = 0;
            fr.readAsArrayBuffer(file.slice(0, fileSliceSize));
        }
    },

    _expand: function () {
        L.DomUtil.addClass(this._container, 'leaflet-control-files-expanded');
        this._form.style.height = null;
        var acceptableHeight = this._map._size.y - (this._container.offsetTop + 50);

        if (acceptableHeight < this._form.clientHeight) {
            L.DomUtil.addClass(this._form, 'leaflet-control-files-scrollbar');
            this._form.style.paddingBottom = "30px";
            this._form.style.height = acceptableHeight + 'px';
        } else {
            L.DomUtil.removeClass(this._form, 'leaflet-control-files-scrollbar');
        }
        L.DomEvent.addListener(this._container, 'touchmove', L.DomEvent.stopPropagation);
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-files-expanded');
    },

    HandleFileDownloadMessage: function (message) {
        if (typeof message.add !== "undefined") {
            // add entries to downloadable files (if not already in list)
            for (var a = 0; a < message.add.length; a++) {
                var messageFileEntry = message.add[a];
                var found = false;
                for (var ai = 0; ai < this._filesItems.length; ai++) {
                    if (this._filesItems[ai].fileName === messageFileEntry.fileName) {
                        found = true;
                        // merge file types
                        var fileItem2 = this._filesItems[ai];
                        if (messageFileEntry.fileTypes !== "undefined") {
                            // add all file types uniquely to the locally cached list
                            for (var t in messageFileEntry.fileTypes) {
                                var found2 = false;
                                for (var t2 in fileItem2.fileTypes) {
                                    if (messageFileEntry.fileTypes[t] === fileItem2.fileTypes[t2]) {
                                        found2 = true;
                                        break;
                                    }
                                }
                                if (!found2) {
                                    fileItem2.fileTypes.push(messageFileEntry.fileTypes[t]);
                                }
                            }
                        }
                        break;
                    }
                }
                if (!found) {
                    // add new file entry to cache local information
                    var fileItem = {};
                    fileItem.fileName = messageFileEntry.fileName;
                    fileItem.fileTypes = messageFileEntry.fileTypes;
                    this._filesItems.push(fileItem);
                }
            }
            this._update();
        }
        else if (typeof message.remove !== "undefined") {
            // remove entries from downloadable files
            for (var r = 0; r < message.remove.length; r++) {
                var rname = message.remove[r];
                for (var ri = 0; ri < this._filesItems.length; ri++) {
                    if (this._filesItems[ri].fileName === rname) {
                        this._filesItems.splice(ri, 1);
                        break;
                    }
                }
            }
            this._update();
        }
        else if (typeof message.clear !== "undefined") {
            // remove all entries from downloadable files
            this._filesItems.length = 0;
            this._update();
        }
        else if (typeof message.fileName !== "undefined") {
            // this is the file the user requested
            if (typeof this._downloads[message.fileName] === "undefined") {
                this._downloads[message.fileName] = {};
                this._downloads[message.fileName].fileSize = message.fileSize;
                this._downloads[message.fileName].fileContents = [];
            }
            var byteCharacters = window.atob(message.fileContents);
            var byteNumbers = new Array(byteCharacters.length);
            for (var i = 0; i < byteCharacters.length; i++) {
                byteNumbers[i] = byteCharacters.charCodeAt(i);
            }
            var byteArray = new Uint8Array(byteNumbers);
            this._downloads[message.fileName].fileContents.push(byteArray);
            // detect last block
            if (message.fileBlockOffset + byteCharacters.length == message.fileSize) {
                // convert array of array of byte numbers
                var blob = new Blob(this._downloads[message.fileName].fileContents);
                // file is complete: let the user save it
                saveAs(blob, message.fileName, true);
                // remove download entry
                delete this._downloads[message.fileName];
            }
        }
    }
});

L.control.files = function (options) {
    return new L.Control.Files(options);
};
