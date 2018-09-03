/**
 * TimeSlider is a UI component for displaying and manipulating a timeline.
 * 
 * TimeSliderSettingsView is a view representing time slider settings dialog.
 * Allows more precise setup of the selected date-time. Click on current date 
 * label to open the dialog.
 */

 /* globals L, d3 */

import './timeSliderSettings.css';
import TimeSliderUtils from './timeSliderUtils';

var TimeSliderSettingsView = L.Control.extend({

    initialize: function (opts) {
        this.map = opts.map;
        this.model = opts.model;
        this.timeFormat = opts.timeFormat || d3.timeFormat('%Y-%m-%d %H:%M');

        this._bindEventHandlers();

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
        this._div.addEventListener('mousedown', this._startmove);
        this._div.addEventListener('touchstart', this._startmove);
        L.DomEvent.disableClickPropagation(this._div);
        L.DomUtil.addClass(this._div, 'leaflet-control');        
    },

    show: function () {
        this._createSettings();
        this.map.addControl(this);     
    },

    hide: function () {
        this.map.removeControl(this);
        this._clearSettings();
    },

    isVisible: function () {
        return !!this._div.firstChild;
    },

    notifyClose: function () {
        this.fire('close');
    },

    _bindEventHandlers: function () {        
        this._startmove = this._startmove.bind(this);
        this._endmove = this._endmove.bind(this);
        this._moveit = this._moveit.bind(this);
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
        this.settings = e.target;
        while (this.settings && !this.settings.classList.contains('settings'))
            this.settings = this.settings.parentNode;
        if (this.settings) {
            window.addEventListener('mouseup', this._endmove, true);
            window.addEventListener('touchend', this._endmove, true);
            window.addEventListener('mousemove',this._moveit, true);
            window.addEventListener('touchmove', this._moveit, true);
    
            if (typeof e.clientX === 'undefined') {
                this.settings._mdx = this.settings.offsetLeft - e.changedTouches[0].clientX;
                this.settings._mdy = this.settings.offsetTop - e.changedTouches[0].clientY;
            } else {
                this.settings._mdx = this.settings.offsetLeft - e.clientX;
                this.settings._mdy = this.settings.offsetTop - e.clientY;
            }
        }
    },

    _endmove: function () {
        window.removeEventListener('mouseup', this._endmove, true);
        window.removeEventListener('touchend', this._endmove, true);
        window.removeEventListener('mousemove', this._moveit, true);
        window.removeEventListener('touchmove', this._moveit, true);
    
        this.settings = null;
    },

    _createRow: function () {
        /*jshint -W101*/

        var changeDate = [
            { name: 'dateInput', node: 'input', classes: ['dateInput'], width: 0.75, placeholder: 'now', ondblclick: function () { this.value = this.placeholder; } },
            { name: 'dateButton', node: 'button', classes: ['dateButton', 'submitDate'], text: 'Go', width: 0.25, onclick: this._submitDate.bind(this) }
        ];
    
        var controls = [
            { 'name': 'fastbackwardButton', 'classes': ['fastbackwardButton', 'fastbackward', 'symbolButton'], 'node': 'button', 'text': '&#x23EE;', onclick: this._fastBackward.bind(this) },
            { 'name': 'backwardButton', 'classes': ['backwardButton', 'backward', 'symbolButton'], 'node': 'button', 'text': '&#x23f4;', onclick: this._backward.bind(this) },
            { 'name': 'replayButton', 'classes': ['replayButton', 'replay', 'symbolButton'], 'node': 'button', 'text': '&#10226;', onclick: this._replay.bind(this) },
            { 'name': 'forwardButton', 'classes': ['forwardButton', 'forward', 'symbolButton'], 'node': 'button', 'text': '&#x23f5;', onclick: this._forward.bind(this) },
            { 'name': 'fastforwardButton', 'classes': ['fastforwardButton', 'fastForward', 'symbolButton'], 'node': 'button', 'text': '&#x23ED;', onclick: this._fastForward.bind(this) }
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

    _createSettings: function () {
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
    
    _clearSettings: function () {    
        if (this._div) {
            // clear previous contents
            while (this._div.firstChild) {
                this._div.removeChild(this._div.firstChild);
            }
        }
    },

    _submitDate: function () {
        var newDate = d3.timeParse(this.timeFormat)(document.getElementsByClassName('dateInput')[0].value);
        if (newDate) {
            this.model.value = newDate;
        }        
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

export default TimeSliderSettingsView;
