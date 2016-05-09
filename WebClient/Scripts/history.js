L.Control.History = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topright',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (options) {
        L.setOptions(this, options);
        this._historyItems = {};
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

            // work around for Firefox Android issue https://github.com/Leaflet/Leaflet/issues/2033
            /*
            L.DomEvent.on(form, 'click', function () {
                setTimeout(L.bind(this._onInputClick, this), 0);
            }, this);
            */

            this._map.on('click', this._collapse, this);
            // TODO keyboard accessibility
        } else {
            this._expand();
        }

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

        L.DomUtil.empty(this._historyList);
        for (hi in this._historyItems) {
            this._addItem(this._historyItems[hi]);
        }
        return this;
    },

    _addItem: function (obj) {
        var label = document.createElement('label');
        label.className = 'history-remove-line';

        var input = document.createElement('div');
        input.className = 'history-remove-button';
        input.layerId = L.stamp(obj);
        this._historyItems[input.layerId] = obj;
        
        L.DomEvent.on(input, 'click', this._onRemovenMeasure, this);

        var name = document.createElement('span');
        if (obj.selectedObjects.length > 0) {
            name.innerHTML = '&nbsp;&nbsp; ' + obj.measure.name + ', ' + obj.selectedObjects.length+' objects';
        }
        else {
            name.innerHTML = '&nbsp;&nbsp; ' + obj.measure.name;
        }
        
        name.className = 'history-name';

        var holder = document.createElement('div');

        label.appendChild(holder);
        holder.appendChild(name);
        holder.appendChild(input);

        this._historyList.appendChild(label);

        return label;
    },

    addMeasure: function (m, so) {
        this._addItem({ measure: m, selectedObjects: so });
    },

    _applyMeasures: function() {
        alert('apply measures from history');
        // todo: apply measures
        this._refocusOnMap();
    },

    _onInputClick: function () {
        /*
        var inputs = this._form.getElementsByTagName('input'),
		    input, layer, hasLayer;
        var addedLayers = [],
		    removedLayers = [];
        */
        
        alert('apply measures from history');
        // todo: apply measures
        /*
        for (var i = inputs.length - 1; i >= 0; i--) {
            input = inputs[i];
            layer = this._layers[input.layerId].layer;
            hasLayer = this._map.hasLayer(layer);

            if (input.checked && !hasLayer) {
                addedLayers.push(layer);

            } else if (!input.checked && hasLayer) {
                removedLayers.push(layer);
            }
        }

        // Bugfix issue 2318: Should remove all old layers before readding new ones
        for (i = 0; i < removedLayers.length; i++) {
            this._map.removeLayer(removedLayers[i]);
        }
        for (i = 0; i < addedLayers.length; i++) {
            this._map.addLayer(addedLayers[i]);
        }
        */
        
        this._refocusOnMap();
    },

    _onRemovenMeasure: function (e) {
        delete this._historyItems[e.currentTarget.layerId];
        this._update();
    },

    hasElements: function() {
        for(var he in this._historyItems)
            return true;
        return false;
    },

    _expand: function () {
        if (this.hasElements()) {
            L.DomUtil.addClass(this._container, 'leaflet-control-history-expanded');
            this._form.style.height = null;
            var acceptableHeight = this._map._size.y - (this._container.offsetTop + 50);
            if (acceptableHeight < this._form.clientHeight) {
                L.DomUtil.addClass(this._form, 'leaflet-control-history-scrollbar');
                this._form.style.height = acceptableHeight + 'px';
            } else {
                L.DomUtil.removeClass(this._form, 'leaflet-control-history-scrollbar');
            }
        }
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-history-expanded');
    }
});

L.control.history = function (options) {
    return new L.Control.History(options);
};
