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
                    mouseenter: this._expand
                }, this);
            }

            // ,
            // mouseleave: this._collapse
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

        container.appendChild(form);
    },

    resetDomains: function (domains) {
        // clear existing
        this._kpis = {};
        this._charts = {};
        this._layers = {};
        LayerManager.Reset();
        //todo: GraphManager.Reset();
        //var firstLayer = null;
        //var lastLayer = null;
        //var switchLayers = [];
        // merge enabled domains (kpis, charts, layers)
        //for (var domainName in domains) {
        //    var domain = domains[domainName];
        //    if (domain.enabled) {
        //        for (var kpiid in domain.kpis)
        //            this._kpis[domain.kpis[kpiid].name] = domain.kpis[kpiid];
        //        for (var chartid in domain.charts) {
        //            //this._charts[domain.charts[chartid].name] = domain.charts[chartid];
        //            GraphManager.MakeGraph(domain.charts[chartid]);
        //        }
        //        for (var layerid in domain.layers) {
        //            var layer = domain.layers[layerid];
        //            // all except basic layers (they are handled by the layers control)
        //            if (!layer.basic) {
        //                //this._layers[layer.name] = layer;
        //                LayerManager.AddLayer(layer);
        //            }
        //        }
        //    }
        //}
        for (var domainName in domains) {
            var domain = domains[domainName];
            {
                for (var cid in domain.charts) {
                    GraphManager.MakeGraph(domain.charts[cid]);
                }
                for (var lid in domain.layers) {
                    if (!domain.layers[lid].basic)
                    {
                        LayerManager.AddLayer(domain.layers[lid]);
                    }
                }
                for (var kid in domain.kpis)
                {
                    //todo add to KPI Manager!
                }
            }
        }
        //switchlayer testing;
        //if (firstLayer && lastLayer) {
        //    var switchLayer = firstLayer;
        //    switchLayer.name = "Switch Test";
        //    switchLayer.id = "tester1";
        //    switchLayer.active = { type: "switch", id: "_tester1", layers: 
        //        [{ zoom: 0, layer: { type: "tile", tiles: firstLayer.tiles, preview: firstLayer.preview, legend: firstLayer.legend, id: "testL1" } },
        //        { zoom: 16, layer: { type: "tile", tiles: lastLayer.tiles, objects: lastLayer.objects, preview: lastLayer.preview, id: "testL2" , legend: lastLayer.legend} }]
        //    }
        //    switchLayer.active.layers = [];
        //    switchLayer.active.layers.push({ layer: {type: "empty"}, zoom: 0})
        //    for (var i = 0; i < switchLayers.length && i < 7; i++)
        //    {
        //        switchLayer.active.layers.push({
        //            layer: {
        //                type: "tile",
        //                tiles: switchLayers[i].tiles,
        //                legend: switchLayers[i].legend,
        //                id: "testertje" + i
        //            },
        //            zoom: (11+i)
        //        });
        //    }
            //LayerManager.AddLayer(switchLayer);
        //}

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
            if (domains[domainName].enabled)
            {
                domain = domains[domainName];
                for (var cid in domain.charts)
                    activecharts[domain.charts[cid].id] = domain.charts[cid].id;
                for (var lid in domain.layers)
                    activelayers[domain.layers[lid].id] = domain.layers[lid].id;
                for (var kid in domain.kpis)
                    activekpis[domain.kpis[kid].id] = domain.kpis[kid].id;
            }
        }
        GraphManager.updateDomains(activecharts);
        LayerManager.updateDomains(activelayers);
        //todo: KPI manager.updateDomains(activekpis);
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

                    if (typeof payload.legend != "undefined")
                    {
                        layer.legend = payload.legend;
                        if (legendControl.legendLayer == layer.id && layer.tileLayer && layer.tileLayer.idShowing == payload.id)
                        {
                            legendControl.createLegend(layer.legend, layer.id);
                        }
                    }
                }
                if (layer.ref && payload.ref && layer.ref.id == payload.ref.id)
                {
                    if (typeof payload.ref.tiles != "undefined") {
                        layer.ref.tiles = payload.ref.tiles;
                        if (layer.tileLayer && layer.tileLayer.idShowing == payload.ref.id) {
                            updateTilesLayerOnMap(layer.ref.id, layer.ref.tiles);
                        }
                    }
                    if (typeof payload.ref.legend != "undefined")
                    {
                        layer.ref.legend = payload.ref.legend;
                        if (legendControl.legendLayer == layer.id && layer.tileLayer && layer.tileLayer.idShowing == payload.ref.id)
                        {
                            legendControl.createLegend(layer.ref.legend, layer.ref.id);
                        }
                    }
                }
                if (layer.diff && payload.diff && layer.diff.id == payload.diff.id)
                {
                    if (typeof payload.diff.tiles != "undefined") {
                        layer.diff.tiles = payload.diff.tiles;
                        if (layer.tileLayer && layer.tileLayer.idShowing == payload.diff.id) {
                            updateTilesLayerOnMap(layer.diff.id, layer.diff.tiles);
                        }
                    }

                    if (typeof payload.diff.legend != "undefined")
                    {
                        layer.diff.legend = payload.diff.legend;
                        if (legendControl.legendLayer == layer.id && layer.tileLayer && layer.tileLayer.idShowing == payload.diff.id)
                        {
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
        var maxElementCount = Math.max(Object.keys(this._kpis).length, LayerManager.ActiveCount, GraphManager.ActiveCount);
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

        //var chartCount = 0;
        //for (var chartid in this._charts) {
        //    //addChart(this.charts, this._charts[chartid], this.options.chartWidth, this.options.chartHeight, false, false, true);
        //    chartCount++;

        //}

        //for (var i = 0; i < GraphManager.graphs.length; i++)
        //{
        //    GraphManager.graphs[i].graph.GetPreview(this.charts);
        //    chartCount++;
        //}

        var chartCount = GraphManager.SetPreviews(this.charts);

        var layerCount = LayerManager.SetPreviews(this.layers);
        //for (var layerid in this._layers) {
        //    addLayer(this.layers, this._layers[layerid], this.options.layerWidth, this.options.layerHeight);
        //    layerCount++;
        //}

        //var layers = LayerManager.GetLayers();
        //for (var l in layers)
        //{
        //    layers[l].GetPreview(this.layers, this.options.layerWidth, this.options.layerHeight);
        //    layerCount++;
        //}

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
        if (LayerManager.ActiveCount > 0)
            return true;
        if (GraphManager.ActiveCount > 0)
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
