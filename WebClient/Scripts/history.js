function BreakdownTime(aTime) {
    var dates = aTime.split(/[ ]+/)[0].split(/[-]+/);
    var times = aTime.split(/[ ]+/)[1].split(/[:]+/);

    return {
        year: dates[0],
        month: dates[1],
        day: dates[2],
        hours: times[0],
        minutes: times[1],
        seconds: times[2]
    };
}

function GetTimeObject(obj) {
    var date = new Date();
    date.setUTCFullYear(obj.year, obj.month - 1, obj.day);
    date.setUTCHours(obj.hours);
    date.setUTCMinutes(obj.minutes);
    date.setUTCSeconds(obj.seconds);
    return date;
}


L.Control.History = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topright',
        autoZIndex: true
    },

    initialize: function (options) {
        L.setOptions(this, options);
        this._historyItems = [];
        this._measureItems = [];
        this._historyList;
        this._measureList;
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


        var className = 'leaflet-control-history',
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
            link.title = 'History of applied measures and commit new measures';

            if (L.Browser.touch) {
                L.DomEvent
                .on(link, 'click', L.DomEvent.stop)
                .on(link, 'click', this._expand, this);
            } else {
                L.DomEvent.on(link, 'focus', this._expand, this);
            }

            this._map.on('click', this._collapse, this);
            // TODO: keyboard accessibility
        } else {
            this._expand();
        }

        this._measureList = L.DomUtil.create('div', className + '-base', form);
        this._separator = L.DomUtil.create('div', className + '-separator', form);
        this._historyList = L.DomUtil.create('div', className + '-base', form);
        this._separator = L.DomUtil.create('div', className + '-separator', form);
        this._applyButton = L.DomUtil.create('div', className + '-apply', form);
        L.DomEvent.on(this._applyButton, 'click', this._applyMeasures, this);
        var innerButton = L.DomUtil.create('input', 'button history-button', this._applyButton);
        innerButton.type = 'button';
        innerButton.value = 'Apply';

        container.appendChild(form);

    },

    _update: function () {
        if (!this._container) { return this; }


        L.DomUtil.empty(this._measureList);
        L.DomUtil.empty(this._historyList);

        for (var i = this._measureItems.length - 1; i >= 0; i--) {
            this._makeMeasureItem(this._measureItems[i]);
        }

        if (this._historyItems.length > 0) {
            for (var i = this._historyItems.length - 1; i >= 0; i--) {
                this._makeHistoryItem(this._historyItems[i]);
            }
        }
        else {
            var label = document.createElement('label');
            label.className = 'history-empty-line';

            var name = document.createElement('span');
            name.className = 'history-empty-name';
            name.innerHTML = "No History Measures available";

            label.appendChild(name);
            this._historyList.appendChild(label);
        }

        return this;
    },

    _makeMeasureItem: function (obj) {
        var label = document.createElement('label');
        label.className = 'history-remove-line';

        var input = document.createElement('div');
        input.className = 'history-icon-button history-remove-button';
        input.layerId = L.stamp(obj);
        input.item = obj;
        obj.input = input;

        L.DomEvent.on(input, 'click', this._onRemovenMeasure, this);

        var name = document.createElement('span');
        if (obj.selectedObjects.length > 0) {
            name.innerHTML = obj.measure.name + ', ' + obj.selectedObjects.length + ' objects';
        }
        else {
            name.innerHTML = obj.measure.name;
        }

        name.className = 'history-name';
        L.DomEvent.on(name, 'click', this._onSelectObjectsFromHistory, this);
        obj.name = name;
        name.item = obj;

        var holder = document.createElement('div');

        label.appendChild(holder);
        holder.appendChild(name);
        holder.appendChild(input);

        this._measureList.appendChild(label);

        return label;
    },

    _makeHistoryItem: function (item) {
        var label = document.createElement('label');
        label.className = 'history-remove-line';

        var input = document.createElement('div');
        input.className = 'history-icon-button history-add-button';
        input.layerId = L.stamp(item);
        input.item = item;
        item.input = input;

        L.DomEvent.on(input, 'click', this._onToggleHistory, this);

        var name = document.createElement('span');
        if (item.selectedObjects.length > 0) {
            name.innerHTML = item.measure.name + ', ' + item.selectedObjects.length + ' objects';
        }
        else {
            name.innerHTML = item.measure.name;
        }

        name.className = 'history-name-inactive';
        name.item = item;
        item.name = name;
        L.DomEvent.on(name, 'click', this._onSelectObjectsFromHistory, this);

        var holder = document.createElement('div');

        label.appendChild(holder);
        holder.appendChild(name);
        holder.appendChild(input);

        this._historyList.appendChild(label);

        return label;
    },

    addHistoryItems: function (historyItems) {
        for (var i = 0, len = historyItems.length; i < len; i++) {
            this._addHistoryItem(historyItems[i]);
        }
    },

    removeHistoryItems: function (historyItems) {
        var tempList = [];
        var changed = false;
        for (var i = 0, leni = this._historyItems.length; i < leni; i++) {
            var found = false;
            for (var j = 0, lenj = historyItems.length; j < lenj; j++) {
                if (historyItems[j].id == this._historyItems[i].id) {
                    found = true;
                    break;
                }
            }
            if (found)
                changed = true;
            else
                tempList.push(this._historyItems[i]);
        }

        this._historyItems = tempList;

        if (changed)
            this._update();
    },

    _addHistoryItem: function (obj) {

        obj.time = GetTimeObject(BreakdownTime(obj.time)); //Change JSON UTC timestamp to javascript utc timestamp
        obj.active = false;
        this._historyItems.push(obj);
        var len = this._historyItems.length;
        if (len > 1 && this._historyItems[len - 1].time.getTime() < this._historyItems[len - 2].time.getTime()) {
            this._historyItems.sort(function (a, b) { return a.time.getTime() - b.time.getTime() })
            this._update();
        }
        else {
            //todo just add it to the DOM
            this._update();
        }

    },

    addMeasure: function (m, so, sc) {
        this._measureItems.push({ measure: m, selectedObjects: so, selectCategories: sc, time: new Date() })
        this._update();
    },

    _applyMeasures: function () {
        // apply measures from history control by sending to server
        // todo: NEW MESSAGE FORMAT
        var message = {};
        message.applyMeasures = [];
        for (var mi in this._measureItems) {
            var am = {
                measure: this._measureItems[mi].measure,
                selectCategories: this._measureItems[mi].selectCategories,
                selectedObjects: this._measureItems[mi].selectedObjects
            };
            message.applyMeasures.push(am);
        }
        wsSend(message);
        this._collapse();
        this._refocusOnMap();
    },

    _onRemovenMeasure: function (e) {
        for (var i = 0, len = this._measureItems.length; i < len; i++) {
            if (this._measureItems[i].layerId == e.currentTarget.layedId) {
                this._measureItems.splice(i, 1);
                break;
            }
        }
        if (this.hasElements())
            this._update();
        else
            this._collapse();
    },

    _onToggleHistory: function (e) {
        var item = e.currentTarget.item;

        item.active = !item.active;

        if (item.active) {
            e.currentTarget.className = 'history-icon-button history-remove-button';
            e.currentTarget.item.name.className = 'history-name';
        }
        else {
            e.currentTarget.className = 'history-icon-button history-add-button';
            e.currentTarget.item.name.className = 'history-name-inactive';
        }
    },

    _onSelectObjectsFromHistory: function (e) {
        var item = e.currentTarget.item;

        if (item.selectedObjects.length == 0)
            return;
        // todo: NEW MESSAGE FORMAT
        var sessionRequest = {};
        sessionRequest.selectObjects = {};
        sessionRequest.selectObjects.type = 'HistoryMeasure';
        sessionRequest.selectObjects.measure = item.measure;
        sessionRequest.selectObjects.selectedObjects = item.selectedObjects;
        sessionRequest.selectObjects.selectCategories = item.selectCategories;
        sessionRequest.selectObjects.mode = e.ctrlKey ? '+' : '=';
        wsSend(sessionRequest);

        this._refocusOnMap();
    },

    hasActiveElements: function () {
        if (this._measureItems.length > 0)
            return true;
        for (var i = 0, len = this._historyItems.length; i < len; i++) {
            if (this._historyItems[i].active)
                return true;
        }
        return false;
    },

    hasElements: function () {
        return this._measureItems.length > 0 || this._historyItems.length > 0;
    },

    _expand: function () {

        if (this.hasElements()) {
            L.DomUtil.addClass(this._container, 'leaflet-control-history-expanded');
            this._form.style.height = null;
            var acceptableHeight = this._map._size.y - (this._container.offsetTop + 50);

            if (acceptableHeight < this._form.clientHeight) {
                L.DomUtil.addClass(this._form, 'leaflet-control-history-scrollbar');

                var apply = document.getElementsByClassName('leaflet-control-history-apply')[0];


                apply.style.width = (this._form.getBoundingClientRect().width - 17) + 'px';
                apply.style.left = this._form.getBoundingClientRect().left + 'px';
                apply.style.bottom = apply.getBoundingClientRect().height + 2 + 'px';

                this._form.style.paddingBottom = "30px";
                this._form.style.height = acceptableHeight + 'px';
            } else {
                L.DomUtil.removeClass(this._form, 'leaflet-control-history-scrollbar');
            }
            L.DomEvent.addListener(this._container, 'touchmove', L.DomEvent.stopPropagation);

        }
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-history-expanded');
    }
});

L.control.history = function (options) {
    return new L.Control.History(options);
};
