// https://www.accelebrate.com/blog/file-uploads-web-sockets-part-3-of-3/
// http://jsbin.com/acohaz/26/edit?html,js,output
// https://stackoverflow.com/questions/32207504/correct-way-to-trigger-file-download

// https://github.com/eligrey/FileSaver.js
// https://stackoverflow.com/questions/25810051/filereader-api-on-big-files

L.Control.Files = L.Control.extend({
    options: {
        collapsed: true,
        position: 'bottomleft',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (options) {
        L.setOptions(this, options);

        //var fi = [];
        //fi.fileName = "hallo";
        //this._filesItems = [fi];

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
            // TODO keyboard accessibility
        } else {
            this._expand();
        }

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
        L.DomEvent.on(innerFileBrowse, "change", function (e) {
            this._uploadFiles(e.target.files);
        }, this);

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

        /*
        container.ondrop = function (e) {
            // prevent browser default behavior on drop
            e.preventDefault();
            
            // iterate over the files dragged on to the browser
            for (var x = 0; x < e.dataTransfer.files.length; x++) {
                var file = e.dataTransfer.files[x];
                // instantiate a new FileReader object
                var fr = new FileReader();

                // loading files from the file system is an asynchronous
                // operation, run this function when the loading process
                // is complete
                fr.onloadend = function (e) {
                    //fr.addEventListener("loadend", function (e) {
                    // send the file over web sockets
                    // todo: ws.send(fr.result);
                    alert("file received: " + file.name + " (" + file.type + ")");

                //});
                }

                // load the file into an array buffer
                fr.readAsArrayBuffer(file);
            }
            
            // restore css style
            container.removeAttribute("style");
        }
        */

    },

    _update: function () {
        if (!this._container) { return this; }


        L.DomUtil.empty(this._filesList);

        if (this._filesItems.length > 0) {
            for (let i = this._filesItems.length - 1; i >= 0; i--) {
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
        var label = document.createElement('label');
        label.className = 'files-remove-line';

        var name = document.createElement('span');
        name.innerHTML = item.fileName;

        name.className = 'files-name';
        name.item = item;
        item.name = name;
        L.DomEvent.on(name, 'click', this._downloadFile, this);

        var holder = document.createElement('div');
        holder.appendChild(name);
        label.appendChild(holder);
        this._filesList.appendChild(label);

        return label;
    },
    /*
    addFilesItems: function (filesItems) {
        for (var i = 0, len = filesItems.length; i < len; i++) {
            this._addFilesItem(filesItems[i]);
        }
    },
    
    removeFilesItems: function (filesItems) {
        var tempList = [];
        var changed = false;
        for (var i = 0, leni = this._filesItems.length; i < leni; i++) {
            var found = false;
            for (var j = 0, lenj = filesItems.length; j < lenj; j++) {
                if (filesItems[j].id == this._filesItems[i].id) {
                    found = true;
                    break;
                }
            }
            if (found)
                changed = true;
            else
                tempList.push(this._filesItems[i]);
        }

        this._filesItems = tempList;

        if (changed)
            this._update();
    },
    
    _addFilesItem: function (obj) {
        obj.time = DataManager.GetTimeObject(DataManager.BreakdownTime(obj.time)); //Change JSON UTC timestamp to javascript utc timestamp
        obj.active = false;
        this._filesItems.push(obj);
        let len = this._filesItems.length;
        if (len > 1 && this._filesItems[len - 1].time.getTime() < this._filesItems[len - 2].time.getTime()) {
            this._filesItems.sort(function (a, b) { return a.time.getTime() - b.time.getTime() })
            this._update();
        }
        else {
            //todo just add it to the DOM
            this._update();
        }

    },
    
    _onToggleFiles: function (e) {
        var item = e.currentTarget.item;

        item.active = !item.active;

        if (item.active) {
            e.currentTarget.className = 'files-icon-button files-remove-button';
            e.currentTarget.item.name.className = 'files-name';
        }
        else {
            e.currentTarget.className = 'files-icon-button files-add-button';
            e.currentTarget.item.name.className = 'files-name-inactive';
        }
    },
    */

    _downloadFile: function (e) {
        // always single file
        var item = e.currentTarget.item;
        var message = {};
        message.type = "downloadFile";
        message.payload = {};
        message.payload.fileName = item.fileName;
        wsSend(message);
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
                    if (i > 0 && i % fileBlockSize == 0) {
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
                if (binary != "") {
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

    hasElements: function () {
        return this._filesItems.length > 0 || this._filesItems.length > 0;
    },

    _expand: function () {

        if (this.hasElements()) {
            L.DomUtil.addClass(this._container, 'leaflet-control-files-expanded');
            this._form.style.height = null;
            var acceptableHeight = this._map._size.y - (this._container.offsetTop + 50);

            if (acceptableHeight < this._form.clientHeight) {
                L.DomUtil.addClass(this._form, 'leaflet-control-files-scrollbar');
                // todo: copy-past, not yet checked
                //var apply = document.getElementsByClassName('leaflet-control-files-apply')[0];


                //apply.style.width = (this._form.getBoundingClientRect().width - 17) + 'px';
                //apply.style.left = this._form.getBoundingClientRect().left + 'px';
                //apply.style.bottom = apply.getBoundingClientRect().height + 2 + 'px';

                this._form.style.paddingBottom = "30px";
                this._form.style.height = acceptableHeight + 'px';
            } else {
                L.DomUtil.removeClass(this._form, 'leaflet-control-files-scrollbar');
            }
            L.DomEvent.addListener(this._container, 'touchmove', L.DomEvent.stopPropagation);
        }
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-files-expanded');
    },

    HandleFileDownloadMessage: function (message) {
        if (typeof message.add !== "undefined") {
            // add entries to downloadable files (if not already in list)
            for (var a = 0; a < message.add.length; a++) {
                var aname = message.add[a];
                var found = false;
                for (var ai = 0; ai < this._filesItems.length; ai++) {
                    if (this._filesItems[ai].fileName == aname) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    var fileItem = {};
                    fileItem.fileName = aname;
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
                    if (this._filesItems[ri].fileName == rname) {
                        this._filesItems.splice(ri, 1);
                        break;
                    }
                }
            }
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
