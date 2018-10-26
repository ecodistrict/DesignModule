L.Control.Details = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topright',
        autoZIndex: true
    },

    initialize: function (options) {
        L.setOptions(this, options);

        this._kpis = {};
        this._charts = {};
        this._layers = {};
        this._lastZIndex = 0;
        this._handlingClick = false;
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
        var className = 'leaflet-control-details',
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
            link.title = 'Details of selected domains in KPIs and map layers';


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

        this._detailList = L.DomUtil.create('div', className + '-base', form);

        container.appendChild(form);
    },

    resetDomains: function (domains) {
        // clear existing
        this._kpis = {};
        this._charts = {};
        this._layers = {};
        LayerManager.Reset();
        for (var domainName in domains) {
            var domain = domains[domainName];
            {
                for (var lid in domain.layers) {
                    if (!domain.layers[lid].basic) {
                        LayerManager.AddLayer(domain.layers[lid]);
                    }
                }
                for (var kid in domain.kpis) {
                    //todo add to KPI Manager!
                }
            }
        }

        // re-build details elements
        this.updateDomains(domains);
        this._update();
        LayerManager.ReactivateVisibleLayers();
    },

    updateDomains: function (domains) {
        var activecharts = {};
        var activelayers = {};
        var activekpis = {};
        var domain;
        for (var domainName in domains) {
            if (domains[domainName].enabled) {
                domain = domains[domainName];
                for (var cid in domain.charts)
                    activecharts[domain.charts[cid].id] = domain.charts[cid].id;
                for (var lid in domain.layers)
                    activelayers[domain.layers[lid].id] = domain.layers[lid].id;
                for (var kid in domain.kpis)
                    activekpis[domain.kpis[kid].id] = domain.kpis[kid].id;
            }
        }
        LayerManager.updateDomains(activelayers);
        //todo: KPI manager.updateDomains(activekpis);
        this._update();
    },

    resetkpi: function (aKPI) {
        this._kpis[aKPI.name] = aKPI;
        this._update();
    },

    updatePreview: function (aElementID, preview) {
        if (this.layers) {
            var img = document.getElementById(aElementID);
            if (img) {
                img.src = preview;
                // update domains in domainsControl
                //var domain = domainsControl._domains[img.domain];
                //domain.layer
            }
        }
    },

    updateTilesURL: function (payload) {
        // update data
        var changedActive = false;
        if (this._layers) {
            for (var layerName in this._layers) {
                var layer = this._layers[layerName];

                // check current, reference and diff layer for updates on tiles and legend
                if (layer.id == payload.id) //note: can we encapsulate everything in this if? does a refresh always contain the id of the base layer?
                {
                    if (payload.tiles) {
                        layer.tiles = payload.tiles;
                        if (layer.tileLayer && layer.tileLayer.idShowing == payload.id) {
                            updateTilesLayerOnMap(layer.id, layer.tiles);
                        }
                    }

                    if (typeof payload.legend != "undefined") {
                        layer.legend = payload.legend;
                        if (legendControl.legendLayer == layer.id && layer.tileLayer && layer.tileLayer.idShowing == payload.id) {
                            legendControl.createLegend(layer.legend, layer.id);
                        }
                    }
                }
                if (layer.ref && payload.ref && layer.ref.id == payload.ref.id) {
                    if (typeof payload.ref.tiles != "undefined") {
                        layer.ref.tiles = payload.ref.tiles;
                        if (layer.tileLayer && layer.tileLayer.idShowing == payload.ref.id) {
                            updateTilesLayerOnMap(layer.ref.id, layer.ref.tiles);
                        }
                    }
                    if (typeof payload.ref.legend != "undefined") {
                        layer.ref.legend = payload.ref.legend;
                        if (legendControl.legendLayer == layer.id && layer.tileLayer && layer.tileLayer.idShowing == payload.ref.id) {
                            legendControl.createLegend(layer.ref.legend, layer.ref.id);
                        }
                    }
                }
                if (layer.diff && payload.diff && layer.diff.id == payload.diff.id) {
                    if (typeof payload.diff.tiles != "undefined") {
                        layer.diff.tiles = payload.diff.tiles;
                        if (layer.tileLayer && layer.tileLayer.idShowing == payload.diff.id) {
                            updateTilesLayerOnMap(layer.diff.id, layer.diff.tiles);
                        }
                    }

                    if (typeof payload.diff.legend != "undefined") {
                        layer.diff.legend = payload.diff.legend;
                        if (legendControl.legendLayer == layer.id && layer.tileLayer && layer.tileLayer.idShowing == payload.diff.id) {
                            legendControl.createLegend(layer.diff.legend, layer.diff.id);
                        }
                    }
                }

            }
        }
    },

    _update: function () {
        if (!this._container) { return this; }

        var container = this._detailList;
        L.DomUtil.empty(container);
        var h = document.createElement('h4');
        h.textContent = 'Details';
        container.appendChild(h);

        // determine elements per row
        var maxElementCount = Math.max(Object.keys(this._kpis).length, LayerManager.ActiveCount, 0);
        var elementsPerRow = maxElementCount > 0 ? Math.ceil(Math.sqrt(maxElementCount)) : 1;
        this.kpis = document.createElement('div');
        this.kpis.className = 'detailskpis';
        this.kpis.style.width = (/*this.options.*/elementsPerRow * this.options.elementWidth) + 'px';
        container.appendChild(this.kpis);

        var hr1 = container.appendChild(document.createElement('hr'));

        this.charts = document.createElement('div');
        this.charts.className = 'detailscharts';
        this.charts.style.width = (/*this.options.*/elementsPerRow * this.options.elementWidth) + 'px';
        container.appendChild(this.charts);


        var hr2 = container.appendChild(document.createElement('hr'));

        this.layers = document.createElement('div');
        this.layers.className = 'detailslayers';
        this.layers.style.width = (/*this.options.*/elementsPerRow * this.options.elementWidth) + 'px';
        container.appendChild(this.layers);

        var kpiCount = 0;
        for (var kpiid in this._kpis) {
            var kpi = new KpiView({
                element: this.kpis,
                width: this.options.kpiWidth,
                height: this.options.kpiHeight,
                data: this._kpis[kpiid]
            });
            kpiCount++;
        }

        var chartCount = 0;

        var layerCount = LayerManager.SetPreviews(this.layers);

        if (kpiCount == 0 || (chartCount == 0 && layerCount == 0))
            hr1.style.display = 'None';
        if (chartCount == 0 || layerCount == 0)
            hr2.style.display = 'None';
        return this;
    },
    hasElements: function () {
        for (var kpi in this._kpis)
            return true;
        if (LayerManager.ActiveCount > 0)
            return true;
        return false;
    },

    _expand: function () {
        L.DomEvent.addListener(this._container, 'touchmove', L.DomEvent.stopPropagation);
        if (this.hasElements()) {
            L.DomUtil.addClass(this._container, 'leaflet-control-details-expanded');
            this._form.style.height = null;
            var acceptableHeight = this._map._size.y - (this._container.offsetTop + 50);
            if (acceptableHeight < this._form.scrollHeight) {
                L.DomUtil.addClass(this._form, 'leaflet-control-details-scrollbar');

                this._form.style.height = acceptableHeight + 'px';

            }
            else {
                L.DomUtil.removeClass(this._form, 'leaflet-control-details-scrollbar');
            }
        }
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-details-expanded');
    },

    setCRD: function (aCRD) {
        this._crd = aCRD;
    }
});

L.control.details = function (categories, options) {
    return new L.Control.Details(categories, options);
};
