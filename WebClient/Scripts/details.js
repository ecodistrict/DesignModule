L.Control.Details = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topright',
        autoZIndex: true,
        hideSingleBase: false
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
            link.title = 'Details of selected domains in KPIs, charts and map layers';


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

        if (window.innerWidth < 500) {
          this._detailList.setAttribute("id", "phone");
        }


        container.appendChild(form);
    },

    resetDomains: function (domains) {
        // clear existing
        this._kpis = {};
        this._charts = {};
        this._layers = {};
        // merge enabled domains (kpis, charts, layers)
        for (var domainName in domains) {
            var domain = domains[domainName];
            if (domain.enabled) {
                for (var kpiid in domain.kpis)
                    this._kpis[domain.kpis[kpiid].name] = domain.kpis[kpiid];
                for (var chartid in domain.charts)
                    this._charts[domain.charts[chartid].name] = domain.charts[chartid];
                for (var layerid in domain.layers) {
                    var layer = domain.layers[layerid];
                    // all except basic layers (they are handled by the layers control)
                    if (!layer.basic)
                        this._layers[layer.name] = layer;
                }
            }
        }
        // re-build details elements
        this._update();
    },

    resetkpi: function (aKPI) {
        for (var kpiName in this._kpis) {
            if (kpiName == aKPI.name) {
                this._kpis[aKPI.name] = aKPI;
                // rebuild details elements
                this._update();
                return;
            }
        }
        // todo: kpi not found.. add?
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

    updateTilesURL: function (aElementID, aTilesURL) {
        // update data
        var changed = false;
        if (this._layers) {
            for (var layerName in this._layers) {
                var layer = this._layers[layerName];
                // check current, reference and diff layer for element id
                if (layer.id == aElementID) {
                    if (layer.tiles != aTilesURL) {
                        layer.tiles = aTilesURL;
                        changed = true;
                        // only reload layer if not showing the objects directly (ie is using tiles)
                        if (!layer.objects)
                            changed = true;
                    }
                }
                else if (layer.ref && layer.ref.id == aElementID) {
                    if (layer.ref.tiles != aTilesURL) {
                        layer.ref.tiles = aTilesURL;
                        // only reload layer if not showing the objects directly (ie is using tiles)
                        if (!layer.ref.objects)
                            changed = true;
                    }
                }
                else if (layer.diff && layer.diff.id == aElementID) {
                    if (layer.diff.tiles != aTilesURL) {
                        layer.diff.tiles = aTilesURL;
                        changed = true; // diff always through tiler, no layer.diff.objects checking needed
                    }
                }
            }
        }
        if (changed)
            updateTilesLayerOnMap(aElementID, aTilesURL);
    },

    _update: function () {
        if (!this._container) { return this; }

        var container = this._detailList;
        L.DomUtil.empty(container);
        var h = document.createElement('h4');
        h.textContent = 'Details';
        container.appendChild(h);

        // determine elements per row
        var maxElementCount = Math.max(Object.keys(this._kpis).length, Object.keys(this._charts).length, Object.keys(this._layers).length);
        var elementsPerRow = maxElementCount >0 ? Math.ceil(Math.sqrt(maxElementCount)) : 1;
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
            addKPI(this.kpis, this._kpis[kpiid], this.options.kpiWidth, this.options.kpiHeight);
            kpiCount++;
        }

        var chartCount = 0;
        for (var chartid in this._charts) {
            addChart(this.charts, this._charts[chartid], this.options.chartWidth, this.options.chartHeight, false, false, true);
            chartCount++;
        }

        var layerCount = 0;
        for (var layerid in this._layers) {
            addLayer(this.layers, this._layers[layerid], this.options.layerWidth, this.options.layerHeight);
            layerCount++;
        }

        if (kpiCount == 0 || (chartCount == 0 && layerCount == 0))
            hr1.style.display = 'None';
        if (chartCount == 0 || layerCount == 0)
            hr2.style.display = 'None';
        return this;
    },
    hasElements : function() {
        for (var kpi in this._kpis)
            return true;
        for (var chart in this._charts)
            return true;
        for (var layer in this._layers)
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
