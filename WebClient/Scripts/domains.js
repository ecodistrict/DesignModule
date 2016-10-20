L.Control.Domains = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topright',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (domains, options) {
        L.setOptions(this, options);

        this._domains = domains;
        this._lastZIndex = 0;
        this._handlingClick = false;
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
        var className = 'leaflet-control-domains',
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
            link.title = 'Domains';

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

        this._domainList = L.DomUtil.create('div', className + '-base', form);

        container.appendChild(form);
    },

    _update: function () {
        if (!this._container) { return this; }

        var container = this._domainList;
        // clear all items..
        L.DomUtil.empty(container);
        var h = document.createElement('h4');
        h.textContent = 'Domains';
        h.title = 'Select the domains you are interested in; details for these will be selectable under the details section';
        container.appendChild(h);

        for (var domainName in this._domains) {
            var domain = this._domains[domainName];
            if ((domain.kpis && domain.kpis.length>0) || (domain.charts && domain.charts.length>0))
                this._addItem(domainName, domain.enabled);
            else {
                if (domain.layers) {
                    for (var l = 0; l < domain.layers.length; l++) {
                        if (!domain.layers[l].basic || domain.layers[l].basic == 0) {
                            this._addItem(domainName, domain.enabled);
                            break;
                        }
                    }
                }
            }
        }

        return this;
    },

    _addItem: function (domainName, domainEnabled) {
        var holder = document.createElement('div');
        holder.className = 'domains-line'; // todo: not needed? see below, 3 lines..
        holder.domainName = domainName;
        holder.innerHTML = domainName;
        this._updateDomainNode(domainEnabled, holder);
        L.DomEvent.on(holder, 'click', this._onDomainClick, this);

        var container = this._domainList;
        container.appendChild(holder);

        this._checkDisabledLayers();
        return container;
    },

    _updateDomainNode: function (domainEnabled, node) {
        if (domainEnabled) {
            node.className = 'domains-line domains-line-selected';
        }
        else {
            node.className = 'domains-line';
        }
    },

    _onDomainClick: function (e) {
        this._handlingClick = true;
        var domain = this._domains[e.target.domainName];
        domain.enabled = !domain.enabled;
        this._updateDomainNode(domain.enabled, e.target);
        detailsControl.resetDomains(this._domains);
        this._handlingClick = false;
        this._refocusOnMap();
    },

    hasElements: function () {
        return (this._domainList) && this._domainList.childNodes.length>1;
    },

    _expand: function () {
        if (this.hasElements()) {
            L.DomUtil.addClass(this._container, 'leaflet-control-domains-expanded');
            this._form.style.height = null;
            var acceptableHeight = this._map._size.y - (this._container.offsetTop + 50);
            if (acceptableHeight < this._form.clientHeight) {
                L.DomUtil.addClass(this._form, 'leaflet-control-domains-scrollbar');
                this._form.style.height = acceptableHeight + 'px';
            } else {
                L.DomUtil.removeClass(this._form, 'leaflet-control-domains-scrollbar');
            }
            this._checkDisabledLayers();
        }
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-domains-expanded');
    },

    _checkDisabledLayers: function () {
    },

    resetDomains: function (domains) {
        this._domains = domains;
        this._update();
        detailsControl.resetDomains(this._domains);
    },

    updateDomains: function (domains) {
        // copy enabled domains
        for (var domainName in this._domains) {
            var domain = this._domains[domainName];
            if (domain.enabled) {
                var domain2 = domains[domainName];
                if (domain2)
                    domain2.enabled = domain.enabled;
            }
        }
        this._domains = domains;
        this._update();
        detailsControl.resetDomains(this._domains);
    }
});

L.control.domains = function (categories, options) {
    return new L.Control.Domains(categories, options);
};
