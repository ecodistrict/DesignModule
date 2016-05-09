L.Control.Info = L.Control.extend({
    options: {
        collapsed: true,
        //position: 'bottomright',
        position: 'bottomleft',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (info, options) {
        L.setOptions(this, options);

        this._lastZIndex = 0;
        this._handlingClick = false;

        /*
        for (var i in categories) {
            this._categories[i] = { category: categories[i], id: i, name: i, enabled: false };
        }
        */
    },

    onAdd: function (map) {
        this._initLayout();
        this._update();

        this._map = map;
        map.on('zoomend', this._checkDisabledLayers, this);

        return this._container;
    },

    onRemove: function () {
        this._map.off('zoomend', this._checkDisabledLayers, this);
    },

    _initLayout: function () {
        var className = 'leaflet-control-info',
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

            var link = this._categoriesLink = L.DomUtil.create('a', className + '-toggle', container);
            link.href = '#';
            link.title = 'Info';

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

        this._detailList = L.DomUtil.create('div', className + '-base', form);

        container.appendChild(form);
    },

    _update: function () {
        if (!this._container) { return this; }

        var container = this._detailList;
        L.DomUtil.empty(container);
        var h = document.createElement('h1');
        h.textContent = 'Info';
        container.appendChild(h);


        /*
        this.kpis = document.createElement('div');
        this.kpis.className = 'infokpis';
        container.appendChild(this.kpis);
        container.appendChild(document.createElement('hr'));

        this.charts = document.createElement('div');
        this.charts.className = 'infocharts';
        container.appendChild(this.charts);
        container.appendChild(document.createElement('hr'));

        var i, obj;

        for (i in this._categories) {
            obj = this._categories[i];
            this._addItem(obj);
        }
        */
        return this;
    },

    _addItem: function (obj) {
        /*
        var label = document.createElement('label');
        label.htmlFor = obj.id;
        label.className = 'info-label';
        label.innerHTML = obj.name;

        var input = document.createElement('input');
        input.type = 'checkbox';
        input.id = obj.id;
        //input.domainId = L.stamp(obj.category);
        input.className = 'info-select';
        L.DomEvent.on(input, 'click', this._onInputClick, this);

        var inputPlus = document.createElement('input');
        inputPlus.type = 'image';
        //inputPlus.value = '+';
        inputPlus.className = 'info-plus'
        L.DomEvent.on(inputPlus, 'click', this._onInputPlusClick, this);
        */
        var holder = document.createElement('div');
        holder.className = 'info-line';
        holder.id = obj.id;
        holder.innerHTML = obj.name;
        L.DomEvent.on(holder, 'click', this._onDomainClick, this);
        /*
        holder.appendChild(input);
        holder.appendChild(label);
        holder.appendChild(inputPlus);
        */
        var container = this._detailList;
        container.appendChild(holder);

        this._checkDisabledLayers();
        return container;
    },

    _expand: function () {
        L.DomUtil.addClass(this._container, 'leaflet-control-info-expanded');
        /*
        this._form.style.height = null;
        var acceptableHeight = this._map._size.y - (this._container.offsetTop + 50);
        if (acceptableHeight < this._form.clientHeight) {
            L.DomUtil.addClass(this._form, 'leaflet-control-info-scrollbar');
            this._form.style.height = acceptableHeight + 'px';
        } else {
            L.DomUtil.removeClass(this._form, 'leaflet-control-info-scrollbar');
        }
        */
        this._checkDisabledLayers();
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-info-expanded');
    },

    _checkDisabledLayers: function () {
    }
});

L.control.info = function (categories, options) {
    return new L.Control.Info(categories, options);
};
