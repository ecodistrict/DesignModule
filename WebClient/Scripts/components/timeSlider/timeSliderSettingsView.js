// timeslider settings

var TimeSliderSettingsView = L.Control.extend({

    initialize: function (opts) {
        this.map = opts.map;
        this.model = opts.model;
        this.timeFormat = opts.timeFormat || d3.timeFormat('%Y-%m-%d %H:%M');

        this.initLayout();
    },

    addTo: function (map) {
        this.remove();
        map._controlContainer.appendChild(this._div);
        return this;
    },

    remove: function () {
        if (this.map._controlContainer.contains(this._div)) {
            this.map._controlContainer.removeChild(this._div);
        }
    },

    initLayout: function () {
         // main div
        this._div = L.DomUtil.create('div', 'settings');
        this._div.addEventListener('mousedown', this._startmove.bind(this));
        this._div.addEventListener('touchstart', this._startmove.bind(this));
        L.DomEvent.disableClickPropagation(this._div);
        L.DomUtil.addClass(this._div, 'leaflet-control');        
    },

    show: function () {
        this.createSettings();
        this.map.addControl(this);     
    },

    hide: function () {
        this.map.removeControl(this);
        this.clearSettings();
    },

    notifyClose: function () {
        this.fire('close');
    },

    _moveit: function (e) {
        if (e.type === 'touchmove') {
            if (this.settings) {
                this.settings.style.left = (this.settings._mdx + e.changedTouches[0].clientX) + 'px';
                this.settings.style.top = (this.settings._mdy + e.changedTouches[0].clientY) + 'px';
                this.settings.style.bottom = 'auto';
            }
        } else {
            if (this.settings) {
                this.settings.style.left = (this.settings._mdx + e.clientX) + 'px';
                this.settings.style.top = (this.settings._mdy + e.clientY) + 'px';
                this.settings.style.bottom = 'auto';
            }
        }
    },

    _startmove: function (e) {
        settings = e.target;
        while (settings && !settings.classList.contains('settings'))
            settings = settings.parentNode;
        if (settings) {
            window.addEventListener('mouseup', L.control.timeslidersettings._endmove, true);
            window.addEventListener('touchend', L.control.timeslidersettings._endmove, true);
            window.addEventListener('mousemove', L.control.timeslidersettings._moveit, true);
            window.addEventListener('touchmove', L.control.timeslidersettings._moveit, true);
    
            if (typeof e.clientX === 'undefined') {
                settings._mdx = settings.offsetLeft - e.changedTouches[0].clientX;
                settings._mdy = settings.offsetTop - e.changedTouches[0].clientY;
            } else {
                settings._mdx = settings.offsetLeft - e.clientX;
                settings._mdy = settings.offsetTop - e.clientY;
            }
        }
    },

    _endmove: function (e) {
        window.removeEventListener('mouseup', this._endmove.bind(this), true);
        window.removeEventListener('touchend', this._endmove.bind(this), true);
        window.removeEventListener('mousemove', this._moveit.bind(this), true);
        window.removeEventListener('touchmove', this._moveit.bind(this), true);
    
        this.settings = null;
    },

    _createRow: function (elements) {
        var changeDate = [
            { name: "dateInput", node: "input", classes: ["dateInput"], width: 0.75, placeholder: 'now', ondblclick: function (e) { this.value = this.placeholder; } },
            { name: "dateButton", node: "button", classes: ["dateButton", "submitDate"], text: "Go", width: 0.25, onclick: this._submitDate.bind(this) }
        ];
    
        var controls = [
            { "name": "fastbackwardButton", "classes": ['fastbackwardButton', 'fastbackward', 'symbolButton'], "node": "button", "text": "&#x23EE;", onclick: this._fastBackward.bind(this) },
            { "name": "backwardButton", "classes": ['backwardButton', 'backward', 'symbolButton'], "node": "button", "text": "&#x23f4;", onclick: this._backward.bind(this) },
            { "name": "replayButton", "classes": ['replayButton', 'replay', 'symbolButton'], "node": "button", "text": "&#10226;", onclick: this._replay.bind(this) },
            { "name": "forwardButton", "classes": ['forwardButton', 'forward', 'symbolButton'], "node": "button", "text": "&#x23f5;", onclick: this._forward.bind(this) },
            { "name": "fastforwardButton", "classes": ['fastforwardButton', 'fastForward', 'symbolButton'], "node": "button", "text": "&#x23ED;", onclick: this._fastForward.bind(this) }
        ];
    
        var self = this;
        function buildRow(content, elements, className) {
            var row = document.createElement('div');
            row.className = 'settings-row ' + className;
    
            for (var i = 0; i < elements.length; i++) {
                var elem = document.createElement(elements[i].node);
                elem.name = elements[i].name;
                if (elements[i].placeholder) {
                    if (elements[i].placeholder === 'now') {
                        elem.placeholder = self.timeFormat(new Date());
                    } else {
                        elem.placeholder = elements[i].placeholder;
                    }
                }
                if (elements[i].type) {
                    elem.type = elements[i].type;
                }
                if (elements[i].width > 0) {
                    elem.style.width = (elements[i].width * 100) + '%';
                }
                if (elements[i].classes) {
                    if (elements[i].classes.length < 2) {
                        elem.className = elements[i].classes;
                    } else {
                        for (var i2 = 0; i2 < elements[i].classes.length; i2++) {
                            elem.classList.add(elements[i].classes[i2]);
                        }
                    }
                }
                if (elements[i].onclick) {
                    elem.onclick = elements[i].onclick;
                    // prevent drag events on parent
                    elem.addEventListener('mousedown', function (e) { e.stopPropagation(); });
                    elem.addEventListener('touchstart', function (e) { e.stopPropagation(); });
                }
                if (elements[i].ondblclick)
                    elem.ondblclick = elements[i].ondblclick;
                switch (elements[i].node) {
                    case 'input':
                        row.appendChild(elem);
                        break;
                    case 'button':
                        elem.innerHTML = elements[i].text;
                        row.appendChild(elem);
                        break;
                    default:
                } // eo switch
            } // eo for loop
            content.appendChild(row);
        }
        var minute = 60000;
        var hour = 60 * minute; // ms
        var day = hour * 24;
    
        buildRow(this._content, changeDate, 'changeDate');
        buildRow(this._content, controls, 'controls');
    },

    _createEntryTitle: function (text) {
        this._title = this._div.appendChild(L.DomUtil.create('div', 'settings-title'));
        this._title.innerHTML = text;
        return this._title.innerHTML;
    },    
    
    _createGrid: function (definition) {
        if (definition.title) {
            this._createEntryTitle(definition.title);
            this._createRow();
        }
    },

    createSettings: function () {
        // content holding settings elements
        this._content = this._div.appendChild(L.DomUtil.create('div', 'settings-content'));
        this._title = this._div.appendChild(L.DomUtil.create('div', 'settings-title'));
        // close cross right upper
        this._close = this._div.appendChild(L.DomUtil.create('div', 'settings-close'));
        this._close.innerHTML = '&#x2715;';
        this._close.onclick = this.notifyClose.bind(this);
        this._createGrid({
            title: 'Settings'
        });
    },
    
    clearSettings: function (aClearPosition) {    
        if (this._div) {
            // clear previous contents
            while (this._div.firstChild) {
                this._div.removeChild(this._div.firstChild);
            }
        }
    },

    _submitDate: function () {
        this.model.value = d3.timeParse(this.timeFormat)(document.getElementsByClassName('dateInput')[0].value);
    },
    
    _fastBackward: function () {
        this.model.value = TimeSliderUtils.createDate(this.model.value, -TimeSliderUtils.DAY);
    },

    _backward: function () {
        this.model.value = TimeSliderUtils.createDate(this.model.value, -TimeSliderUtils.HOUR);
    },

    _replay: function () {
        this.model.value = new Date();
    },

    _forward: function () {
        this.model.value = TimeSliderUtils.createDate(this.model.value, TimeSliderUtils.HOUR);
    },

    _fastForward: function () {
        this.model.value = TimeSliderUtils.createDate(this.model.value, TimeSliderUtils.DAY);
    }
});
L.extend(TimeSliderSettingsView.prototype, L.Evented.prototype);
