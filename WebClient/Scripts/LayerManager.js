var LayerManager = {
    _layers: {},
    _layersonid: {},
    _subscribedLayers: {},
    _visibleLayers: [],
    _previousVisible: [],
    _length: 0,
    ActiveCount: 0,

    AddLayer: function (layer) {
        if (typeof LayerManager._layers[layer.name] === "undefined")
            LayerManager._length++;
        var newlayer = new LayerManager.DetailsLayer(layer);
        LayerManager._layers[layer.name] = newlayer;
        LayerManager._layersonid[layer.id] = newlayer;
    },

    _clearLayers: function () {
        LayerManager.ClearVisibleLayers();
        LayerManager._layers = {};
        LayerManager._layersonid = {};
        LayerManager._length = 0;
    },

    AddVisibleLayer: function (layer) {
        LayerManager._visibleLayers.push(layer);
    },

    RemoveVisibleLayer: function (layer) {
        for (var i = LayerManager._visibleLayers.length - 1; i >= 0 ; i--) {
            if (LayerManager._visibleLayers[i].id == layer.id) {
                LayerManager._visibleLayers.splice(i, 1);
                i--;
            }
        }
    },

    ClearVisibleLayers: function () {
        for (var i = LayerManager._visibleLayers.length - 1; i >= 0 ; i--) {
            if (typeof LayerManager._visibleLayers[i] != "undefined") //todo check what goes wrong?
                LayerManager._visibleLayers[i].removeDisplayLayer();
        }

        //redundant? layers have already removed themselves! Maybe make a system where removeLayers not always removes it from the LayerManager._visibleLayers
        LayerManager.VisibleLayers = [];
    },

    GetLayers: function () {
        var layers = {}
        for (var l in LayerManager._layers) {
            layers[l] = LayerManager._layers[l];
        }
        return layers;
    },

    SetPreviews: function (container) {
        var counter = 0;
        for (var lid in LayerManager._layers)
        {
            if (LayerManager._layers[lid].enabled)
            {
                counter++;
                LayerManager._layers[lid].GetPreview(container);
            }
        }
        return counter;
    },

    GetLayerByName: function (name) {
        if (typeof LayerManager._layers[name] !== "undefined")
            return LayerManager._layers[name];
        return null;
    },

    GetLayerById: function (id) {
        if (typeof LayerManager._layersonid[id] !== "undefined")
            return LayerManager._layersonid[id];
        return null;
    },

    GetVisibleLayers: function () {
        var layers = []
        for (var i = 0; i < LayerManager._visibleLayers.length; i++) {
            layers.push(LayerManager._visibleLayers[i]);
        }
        return layers;
    },

    SetNextLegend: function () {
        if (LayerManager._visibleLayers.length > 0) {
            LayerManager._visibleLayers[0].setLegend();
        }
        else
            legendControl.clearLegend();
    },

    SetNextCRD: function () {
        if (LayerManager._visibleLayers.length > 0) {
            LayerManager._visibleLayers[LayerManager._visibleLayers.length - 1].setCRD();
        }
        else
            crd.reset();
    },

    Reset: function () {
        LayerManager._previousVisible = [];
        for (var i = 0; i < LayerManager._visibleLayers.length; i++) {
            LayerManager._previousVisible.push({ name: LayerManager._visibleLayers[i].name, crd: LayerManager._visibleLayers[i].showing.crd, opacity: LayerManager._visibleLayers[i].opacity });
        }
        LayerManager._unsubscribeLayers();
        LayerManager._clearLayers();
    },

    _unsubscribeLayers: function () {
        for (var l in LayerManager._subscribedLayers) {
            wsSend({ unsubscribe: LayerManager._subscribedLayers[l].id });
        }
        LayerManager._subscribedLayers = {};
    },

    ReactivateVisibleLayers: function () {
        for (var i = 0; i < LayerManager._previousVisible.length; i++) {
            for (var l in LayerManager._layers) {
                if (LayerManager._previousVisible[i].name == LayerManager._layers[l].name) {
                    LayerManager._layers[l].addDisplayLayer(LayerManager._previousVisible[i].opacity, LayerManager._previousVisible[i].crd);
                }
            }
        }

        LayerManager.SetNextCRD();
        LayerManager.SetNextLegend();
    },


    //todo move away from checking on id of the detailsLayer and move straight to the layer!
    UpdateData: function (payload) {
        if (!payload.id)
            return;

        if (!payload.diff && !payload.ref && typeof LayerManager._subscribedLayers[payload.id] !== "undefined") //check if compatible with system
        {
            LayerManager.UpdateSubscribedLayer(payload);
        }
        //else //old system
        //{
        //    var layer = LayerManager.GetLayerById(payload.id);
        //    if (layer != null)
        //        layer.updateData(payload);
        //}
    },

    updateDomains: function (activelayers)
    {
        LayerManager.ActiveCount = 0;
        for (lid in LayerManager._layersonid)
        {
            if (typeof activelayers[lid] !== "undefined")
            {
                LayerManager.ActiveCount++;
                LayerManager._layersonid[lid].enabled = true;
            }
            else
            {
                LayerManager._layersonid[lid].enabled = false;
            }
        }
        //todo show/hide layers with update domains?
    },

    UpdateSubscribedLayer: function (payload) {
        if (typeof LayerManager._subscribedLayers[payload.id] !== "undefined") {
            LayerManager._subscribedLayers[payload.id].updateData(payload);
        }
    },

    SubscribeLayer: function (layer) {
        LayerManager._subscribedLayers[layer.id] = layer;
        wsSend({ subscribe: layer.id });
    },

    UnsubscribeLayer: function (layer) {
        if (delete LayerManager._subscribedLayers[layer.id])
            wsSend({ unsubscribe: layer.id });
    },

    GetLayer: function (layer, parent, crd) {
        if (typeof layer.type === "undefined")
            return;

        switch (layer.type) {
            case "tile": return new LayerManager.TileLayer(layer, parent, crd);
                break;
            case "object": return new LayerManager.ObjectLayer(layer, parent, crd);
                break;
            case "geo": return new LayerManager.GeoJSONLayer(layer, parent, crd);
                break;
            case "switch": return new LayerManager.SwitchLayer(layer, parent, crd);
                break;
            case "empty": return new LayerManager.EmptyLayer(layer, parent, crd);
                break;
            default: return null;
                break;
        }
    },

    Length: function () {
        return LayerManager._length;
    }
}

//manages the onzoom event and calls all switchlayers to check if they need to change their layers!
LayerManager.SwitchManager = {
    _switchLayers: {},
    _count: 0,

    addSwitchLayer: function (layer) {
        if (typeof this._switchLayers[layer.id] !== "undefined") //check if layer already added
            return;

        this._switchLayers[layer.id] = layer;
        this._count++;

        if (this._count == 1) {
            map.addEventListener("zoomend", LayerManager.SwitchManager.ZoomEnd);
        }
    },

    removeSwitchLayer: function (layer) {

        if (delete this._switchLayers[layer.id]) {
            this._count--;
            if (this._count == 0) {
                map.removeEventListener("zoomend", LayerManager.SwitchManager.ZoomEnd);
            }
        }
    },

    ZoomEnd: function (e) {
        //for now make all layers check! todo: build efficient system where efficiently can be checked which layers need to be switched
        for (var l in LayerManager.SwitchManager._switchLayers)
            LayerManager.SwitchManager._switchLayers[l].showSwitchLayer(LayerManager.SwitchManager._switchLayers[l].maplayer);
    }
}


//is object manager needed?? maybe let an object layer handle it's objects!?
LayerManager.ObjectManager = {
    objectlayergroups: {},

    addObjectLayerGroup: function (layer) {
        if (typeof this.objectlayergroups[layer.id] !== "undefined")
            return;

        var layergroup = L.layerGroup().addTo(map);
        layergroup.id = layer.id;

        this.objectlayergroups[layer.id] = layergroup;
    },

    removeObjectLayerGroup: function (layer) {
        if (typeof this.objectlayergroups[layer.id] === "undefined")
            return;

        this.objectlayergroups[layer.id].clearLayers();
        map.removeLayer(this.objectlayergroups[layer.id]);

        delete this.objectlayergroupshash[layer.id];
    },

    getObjectLayerGroup: function (layer) {
        if (typeof this.objectlayergroups[layer.id] === "undefined")
            return null;
        return this.objectlayergroups[layer.id];
    }
}

LayerManager.Object = function (data, layergroup) {
    //this.lat = data.lat;// ? data.lat : 52.31428;
    //this.lng = data.lng;// ? data.lng : 4.67426;
    this.latlng = L.latLng(data.lat, data.lng);
    this.layergroup = layergroup;

    //http://leafletjs.com/reference.html#path-options
    this.style = {
        stroke: true,
        color: data.color,
        opacity: data.opacity ? data.opacity : 0,
        fill: true,
        fillOpacity: data.fillOpacity ? data.fillOpacity : 1,
        fillColor: data.fillColor
    }

    this.radius = data.radius || 2;

    this.shape = L.circle(this.latlng, this.radius,
        this.style
    ).addTo(layergroup);

    this.update = function (data) {

        var posChanged = false;
        if (typeof data.lat !== 'undefined') {
            //this.lat = data.lat;
            this.latlng.lat = data.lat;
            posChanged = true;
        }
        if (typeof data.lng !== 'undefined') {
            //this.lng = data.lng;
            this.latlng.lng = data.lng;
            posChanged = true;
        }
        if (posChanged) {
            //this.latlng = L.latLng(this.lat, this.lng);
            this.shape.setLatLng(this.latlng);
        }

        var styleChanged = false;

        if (typeof data.color !== 'undefined')
        {
            this.style.color = data.color;
            styleChanged = true;
        }
        if (typeof data.opacity !== 'undefined')
        {
            this.style.opacity = data.opacity;
            styleChanged = true;
        }
        if (typeof data.fillColor !== 'undefined')
        {
            this.style.fillColor = data.fillColor;
            styleChanged = true;
        }
        if (typeof data.fillOpacity !== 'undefined')
        {
            this.style.fillOpacity = data.fillOpacity;
            styleChanged = true;
        }

        if (styleChanged) {
            this.shape.setStyle(this.style);
        }

        if (typeof data.radius !== 'undefined')
        {
            this.radius = data.radius;
            this.shape.setRadius(this.radius);
        }
    }

    this.remove = function () {
        this.layergroup.removeLayer(this.shape);
    }
}

LayerManager.DetailsLayer = function (data) {
    for (var v in data)
        this[v] = data[v];

    //this.crd = "active";
    this.showing = null;

    //set a get only function for maplayer property
    Object.defineProperties(this, {
        "maplayer": {
            "get": function () {
                if (this.showing)
                    return this.showing.maplayer;
                return null;
            }
        }
    });

    this.GetPreview = function (parent) {
        if (typeof this.show !== "undefined" && this.show > 0)
        {
             this.addDisplayLayer(this.show);
        }
        var aWidth = DataManager.detailsInfo.layerWidth;
        var aHeight = DataManager.detailsInfo.layerHeight;
        var mainDiv = document.createElement('div');
        mainDiv.style.width = (aWidth - 6) + 'px';
        mainDiv.style.height = aHeight + 'px';
        mainDiv.className = 'layerDetails';
        mainDiv.title = this.description;
        parent.appendChild(mainDiv);

        var h = document.createElement('h4');
        h.textContent = this.name;
        h.className = 'layerDetailsTitle';
        mainDiv.appendChild(h);

        var img = document.createElement('img');
        img.className = 'layerDetailsImg';
        img.src = this.preview;
        img.id = this.id;
        //img.domain = data.domain;
        mainDiv.appendChild(img);

        var divLayerSelected = document.createElement('div');
        if (!this.showing) {
            divLayerSelected.className = 'layerDetailsSelected layerDetailsSelectedHidden';
        }
        else
            divLayerSelected.className = 'layerDetailsSelected';
        mainDiv.appendChild(divLayerSelected);

        // add ref layer data and click handler to mainDiv
        mainDiv.layer = this;
        this.previewDisplay = {
            mainDiv: mainDiv,
            selectedDiv: divLayerSelected,
            title: h,
            img: img
        }
        mainDiv.addEventListener("click", this.onClick);
    };

    this.removeDisplayLayer = function () {
        if (this.showing) {
            map.removeLayer(this.maplayer);
            this.showing.hideLayer();
            wsSend({ unsubscribe: this.id });
            this.showing = null;
        }
        if (this.previewDisplay) {
            this.previewDisplay.selectedDiv.className = "layerDetailsSelected layerDetailsSelectedHidden";
        }
        LayerManager.RemoveVisibleLayer(this);
    };

    this.addDisplayLayer = function (opacity, crd) {
        if (this.previewDisplay) {
            this.previewDisplay.selectedDiv.className = "layerDetailsSelected";
        }
        this.opacity = opacity;
        if (crd && crd == "reference" && this.ref) {
            this.ref.showLayer(this.maplayer);
            this.showing = this.ref;
        }
        else if (crd && crd == "difference" && this.diff) {
            this.diff.showLayer(this.maplayer);
            this.showing = this.diff;
        }
        else {
            this.active.showLayer(this.maplayer);
            this.showing = this.active;
        }
        wsSend({ subscribe: this.id });
        LayerManager.AddVisibleLayer(this);

        //hijack crd
    };

    this.setCRD = function () {
        if (!this.showing)
            return;

        var crdActive;
        switch (this.showing.crd) {
            case "active": crdActive = crd.active;
                break;
            case "reference": crdActive = crd.reference;
                break;
            case "difference": crdActive = crd.difference;
                break;
            default: crdActive = crd.current;
                break;
        }
        crd.reset(this.active != null, this.ref != null, this.diff != null, this, crdActive);
    };

    this.setLegend = function () {
        if (!this.showing)
            return;
        this.showing.showLegend();
    };

    this.onClick = function (e) {
        // if layer is visible: remove else add
        var element = e.target;
        while (element && !element.layer)
            element = element.parentNode;
        if (element) {
            var layer = element.layer;
            layer.show = 0;
            var opacity = e.ctrlKey ? 0.5 : 0.8;
            // todo: handle ctrl-click to always add or replace layer

            if (layer.showing) {
                layer.removeDisplayLayer();
                if (typeof crd.crdLayer !== "undefined" && layer.id == crd.crdLayer) {
                    LayerManager.SetNextCRD();
                }
                if (typeof legendControl.legendLayer !== "undefined" && legendControl.legendLayer == layer.id) {
                    LayerManager.SetNextLegend();
                }
            }
            else {
                if (!e.ctrlKey)
                    LayerManager.ClearVisibleLayers();
                layer.addDisplayLayer(opacity);
                layer.setCRD();
                if (LayerManager._visibleLayers.length == 1)
                    layer.setLegend();
            }

        };
    };

    this.updateData = function (payload) {
        //check for old system
        if (payload.tiles || payload.legend) {
            this.active.updateData(payload);
            if (payload.legend && this.ref)
                this.ref.updateData({ legend: payload.legend })
        }

        if (payload.active)
            this.active.updateData(payload.active);
        if (payload.ref)
            this.ref.updateData(payload.ref);
        if (payload.diff)
            this.diff.updateData(payload.diff);
    };

    this.updatePreview = function () {
        if (this.previewDisplay && this.active) {
            this.previewDisplay.img.src = this.active.getPreview;
        }
    }


    //set the active/ref/diff layers!
    if (data.type) //check for new system!
    {
        switch (data.type) {
            case "tile": this.active = new LayerManager.TileLayer(data, this, "active");
                break;
            case "object": this.active = new LayerManager.ObjectLayer(data, this, "active");
                break;
            case "geo": this.active = new LayerManager.GeoJSONLayer(data, this, "active");
                break;
            case "switch": this.active = new LayerManager.SwitchLayer(data, this, "active");
                break;
            default:
                this.active = null;
                console.log("Encountered unknown active layer type: " + data.type);
                break;
        }
        if (data.ref) {
            switch (data.ref.type) {
                case "tile": this.ref = new LayerManager.TileLayer(data.ref, this, "reference");
                    break;
                case "object": this.ref = new LayerManager.ObjectLayer(data.ref, this, "reference");
                    break;
                case "geo": this.ref = new LayerManager.GeoJSONLayer(data.ref, this, "reference");
                    break;
                case "switch": this.ref = new LayerManager.SwitchLayer(data.ref, this, "reference");
                    break;
                default:
                    this.ref = null;
                    console.log("Encountered unknown reference layer type: " + data.ref.type);
                    break;
            }
        }
        else {
            this.ref = null;
        }
        if (data.diff) {
            switch (data.diff.type) {
                case "tile": this.diff = new LayerManager.TileLayer(data.diff, this, "difference");
                    break;
                case "object": this.diff = new LayerManager.ObjectLayer(data.diff, this, "difference");
                    break;
                case "geo": this.diff = new LayerManager.GeoJSONLayer(data.diff, this, "difference");
                    break;
                case "switch": this.diff = new LayerManager.SwitchLayer(data.diff, this, "difference");
                    break;
                default:
                    this.diff = null;
                    console.log("Encountered unknown difference layer type: " + data.diff.type);
                    break;
            }
        }
        else {
            this.diff = null;
        }
    }
    else { //old system
        if (data.description == "GTU") {
            this.active = new LayerManager.ObjectLayer(data, this, "active");
            data.show = { opacity: 1 };
        }
        else {
            if (data.tiles) {
                this.active = new LayerManager.TileLayer({
                    tiles: data.tiles,
                    legend: data.legend,
                    id: data.id,
                    name: data.name
                }, this, "active");
            }
            else if (data.objects) {
                this.active = new LayerManager.GeoJSONLayer({
                    legend: data.legend,
                    objects: data.objects,
                    id: data.id,
                    name: data.name
                }, this, "active");
            }
            else
            {
                //error
                this.active = new LayerManager.EmptyLayer({}, this, "active");
            }
            

            if (data.ref) {
                this.ref = new LayerManager.TileLayer(data.ref, this, "reference");
                this.ref.legend = data.legend; //for now set ref legend to active legend -> future let ref have own legend, even if it will be duplicate?
            }
            else
                this.ref = null;
            if (data.diff)
                this.diff = new LayerManager.TileLayer(data.diff, this, "difference");
            else
                this.diff = null;
        }
    }
}

LayerManager.BaseLayer = function (layer, detailsLayer, crd) {
    this.maplayer = false
    this.id = layer.id;
    this.legend = layer.legend;
    this.crd = crd;
    this.detailsLayer = detailsLayer;

    this.showLegend = function () {
        if (!this.legend)
            legendControl.clearLegend(false, this.detailsLayer.id);
        else {
            legendControl.createLegend(this.legend, this.detailsLayer.id);
        }
    };

    this.subscribe = function () {
        LayerManager.SubscribeLayer(this);
    };

    this.unsubscribe = function () {
        LayerManager.UnsubscribeLayer(this);
    };

    this.updatePreview = function (preview) {
        this.preview = preview;
        if (this.crd == "active") {
            this.detailsLayer.updatePreview();
        }
    };

    this.getPreview = function () {
        if (this.preview)
            return this.preview;
        return "";
    }
}

LayerManager.TileLayer = function (layer, detailsLayer, crd) {
    LayerManager.BaseLayer.call(this, layer, detailsLayer, crd);
    this.tiles = layer.tiles;
    this.subscribe();

    this.showLayer = function (leafletLayer) {
        if (legendControl.legendLayer == this.detailsLayer.id)
            legendControl.createLegend(this.legend, this.detailsLayer.id);

        if (leafletLayer) {
            if (leafletLayer.options.type == "tile") {
                leafletLayer.setUrl(this.tiles);
                leafletLayer.options.idShowing = this.id;
                this.maplayer = leafletLayer;
                return leafletLayer;
            }
            else {
                map.removeLayer(leafletLayer);
            }
        }
        var tileLayer = L.tileLayer(this.tiles, {
            opacity: this.detailsLayer.opacity,
            id: this.name,
            idShowing: this.id,
            zIndex: 999,
            type: "tile"
        });
        tileLayer.addTo(map);
        this.maplayer = tileLayer;

        return tileLayer;
    };

    this.hideLayer = function () {
        this.maplayer = null;
    };

    this.updateData = function (payload) {
        if (payload.tiles) {
            this.tiles = payload.tiles;
            if (this.maplayer)
                this.maplayer.setUrl(this.tiles);
        }
        if (payload.legend) {
            this.legend = payload.legend;
            if (this.maplayer && legendControl.legendLayer == this.detailsLayer.id)
                legendControl.createLegend(this.legend, this.detailsLayer.id);
        }
        if (payload.preview) {
            this.updatePreview(payload.preview);
        }
    };
}

LayerManager.ObjectLayer = function (layer, detailsLayer, crd) {
    LayerManager.BaseLayer.call(this, layer, detailsLayer, crd);
    this.objects = {};

    this.showLayer = function (leafletlayer) {
        if (legendControl.legendLayer == this.detailsLayer.id)
            legendControl.createLegend(this.legend, this.detailsLayer.id);
        this.objects = {}; //reset objects
        if (leafletlayer) {
            if (leafletlayer.type == "object") {
                leafletlayer.clearLayers();
                this.subscribe();
                return leafletlayer;
            }
            else {
                map.removeLayer(leafletlayer);
            }
        }
        this.maplayer = L.layerGroup().addTo(map);
        this.maplayer.idShowing = this.id;
        this.maplayer.id = this.name;
        this.maplayer.type = "object";

        this.subscribe(); //subscribe, then wait for objects!

        return this.maplayer;
    };

    this.hideLayer = function () {
        this.unsubscribe();
        this.maplayer.clearLayers(); //clear all leaflet layers
        this.objects = {}; //clear objects from memory
        this.maplayer = null;
    };


    /*
    payload :
    { id: layerid,
      data:
        [{ **newobject/updateobject/removeobject are mutially exclusive, if more then one are defined only the first one will be handled!**
        newobject: {
            type: "circle"/and others
            radius: ..., (for circle)
            id: ...,
            lat: ...,
            lng: ...,
            fillColor: ...,
            color: ...,
            opacity: ...,
            fillOpacity: ...
        },
        updateobject: {
            id: ...,
            lat: ...,
            lng: ...,
            fillColor: ...,
            color: ...,
            opacity: ...,
            fillOpacity: ...,
        },
        removeobject: {
            id: ...
        }
    }]
    }
    */
    this.updateData = function (payload) {
        if (payload.legend) {
            this.legend = payload.legend;
            if (this.maplayer && legendControl.legendLayer == this.detailsLayer.id)
                legendControl.createLegend(this.legend, this.detailsLayer.id);
        }
        if (payload.preview) {
            this.updatePreview(payload.preview);
        }
        if (payload.data) {
            payload = payload.data;
            for (var i = 0; i < payload.length; i++) {
                if (payload[i].newobject) {
                    this.newObject(payload[i].newobject);
                }
                else if (payload[i].updateobject) {
                    this.updateObject(payload[i].updateobject);
                }
                else if (payload[i].removeobject) {
                    this.removeObject(payload[i].removeobject);
                }
                if (payload[i].preview) {
                    this.updatePreview(payload[i].preview);
                }
                if (payload[i].legend) {
                    this.legend = payload[i].legend;
                    if (this.maplayer && legendControl.legendLayer == this.detailsLayer.id)
                        legendControl.createLegend(this.legend, this.detailsLayer.id);
                }
            }
        }
    };

    this.newObject = function (object) {
        if (typeof this.objects[object.id] !== "undefined") {
            return;
        }
        this.objects[object.id] = new LayerManager.Object(object, this.maplayer);
    };

    this.updateObject = function (object) {
        if (typeof this.objects[object.id] === "undefined")
            return;
        this.objects[object.id].update(object);
    };

    this.removeObject = function (object) {
        if (typeof this.objects[object.id] === "undefined")
            return;
        this.objects[object.id].remove();
        delete this.objects[object.id];
    };
};

LayerManager.GeoJSONLayer = function (layer, detailsLayer, crd) {
    LayerManager.BaseLayer.call(this, layer, detailsLayer, crd);
    this.geoJSON = layer.objects;
    this.subscribe();

    this.showLayer = function (leafletlayer) {
        if (leafletlayer)
            map.removeLayer(leafletlayer);

        this.maplayer = L.geoJson(layer.objects ? layer.objects.features : [], {
            opacity: this.detailsLayer.opacity,
            style: this.getStyle.bind(this)
        }).addTo(map);
        this.maplayer.type = "geo";
        return this.maplayer;
    };

    this.getStyle = function (feature) {
        var layerOpacity = this.detailsLayer.opacity;
        var fillOpacity = feature.properties.fillOpacity ? feature.properties.fillOpacity * layerOpacity : 0;
        var strokeOpacity = feature.properties.opacity ? feature.properties.opacity * layerOpacity : fillOpacity;
        var weight = feature.properties.weight ? feature.properties.weight : 0.5;


        if (typeof feature.properties.fillOpacity !== 'undefined')
            return { color: feature.properties.color, fillOpacity: fillOpacity, opacity: strokeOpacity, weight: weight };
            // test code for live trafic..
        else if (feature.properties.color == '#000000')
            return { color: '#000000', weight: 0.5, opacity: strokeOpacity };
        else
            return { color: feature.properties.color, weight: weight, opacity: strokeOpacity };
    };

    this.hideLayer = function () {
        this.maplayer = null;
    };

    this.updateData = function (payload) {
        //var elementID = payload.id;
        //// online layer
        //for (var mlid in map._layers) {
        //    var layer = map._layers[mlid];
        //    if (layer.domainLayer && layer.domainLayer.id && layer.domainLayer.id == elementID) {
        //        if (typeof payload.newobjects !== "undefined") {
        //            // dictionary of id: feature
        //            for (var id in payload.newobjects) {
        //                layer.addData(payload.newobjects[id]);
        //            }
        //        }
        //        else if (payload.changedcolors) {
        //            // dictionary of id: color
        //            for (var lid in layer._layers) {
        //                var fid = layer._layers[lid].feature.properties.id;
        //                var newColor = payload.changedcolors[fid];
        //                if (newColor)
        //                    layer._layers[lid].feature.properties.color = newColor;
        //            }
        //            layer.setStyle(
        //                function (feature) {
        //                    // todo: test code for live trafic.. generalize..
        //                    if (feature.properties.color == '#000000')
        //                        return { color: '#000000', weight: 1 };
        //                    else
        //                        return { color: feature.properties.color }
        //                });
        //        }
        //        else if (payload.removedobjects) {
        //            // dictionary id: X
        //            for (var lid in layer._layers) {
        //                var fid = layer._layers[lid].feature.properties.id;
        //                if (payload.removedobjects[fid])
        //                    layer.removeLayer(layer._layers[lid]);
        //            }
        //        }
        //        if (payload.timestamp) {
        //            showLayerTimeStamp(payload.timestamp)

        //        }
        //    }
        //}


        if (payload.objects) {
            this.geoJSON = payload.objects;
            if (this.maplayer)
                this.showLayer(this.maplayer);
        }
        if (payload.preview) {
            this.updatePreview(payload.preview);
        }
        if (payload.legend) {
            this.legend = payload.legend;
            if (this.maplayer && legendControl.legendLayer == this.detailsLayer.id)
                legendControl.createLegend(this.legend, this.detailsLayer.id);
        }
    };
};


/*
Layer.layers: [
    { zoom: 0/1/2.../14/.../20/,
      layer: tile/object/geo/empty layer
      }
]
*/
LayerManager.SwitchLayer = function (layer, detailsLayer, crd) {
    LayerManager.BaseLayer.call(this, layer, detailsLayer, crd);
    this.showing = null;
    this.layers = [];

    this.showLayer = function (leafletlayer) {
        LayerManager.SwitchManager.addSwitchLayer(this);
        return this.showSwitchLayer(leafletlayer);
    }

    this.showSwitchLayer = function (leafletlayer) {
        var nextLayer = null;
        var zoom = map.getZoom();

        //find the right layer
        for (var i = 0; i < this.layers.length; i++) {
            if (this.layers[i].zoom <= zoom)
                nextLayer = this.layers[i].layer;
            else
                break;
        }

        if (this.showing == nextLayer)
            return leafletlayer;

        this.showing = nextLayer;

        if (this.showing) {
            this.maplayer = this.showing.showLayer(leafletlayer);
            LayerManager.SetNextLegend();
            LayerManager.SetNextCRD();
            return this.maplayer;
        }

        return null;
    }

    this.hideLayer = function () {
        if (this.showing)
            this.showing.hideLayer();
        this.showing = null;
        this.maplayer = null;
        LayerManager.SwitchManager.removeSwitchLayer(this);
    }

    this.updateData = function (data) {
        if (this.showing)
            drawlayer = this.showing.maplayer;

        for (var i = 0; i < this.layers.length; i++) {
            this.layers[i].hideLayer(); //remove display
            this.layers[i].unsubscribe(); //remove subscriptions
        }
        this.layers = [];

        for (var i = 0; i < data.layers.length; i++) {
            this.layers.push({ zoom: data.layers[i].zoom, layer: LayerManager.GetLayer(data.layers[i].layer, this.detailsLayer, this.crd) });
        }

        if (this.showing)
            this.showLayer(drawlayer);
    }

    this.showLegend = function () {
        if (this.showing)
            this.showing.showLegend();
    }

    this.getPreview = function () {
        if (this.layers.length > 0)
            return this.layers[0].getPreview();
        return "";
    }


    //add the layers!
    this.updateData(layer);
};


//empty layer, won't show anything! for example can be used to keep certain zoom levels empty
LayerManager.EmptyLayer = function (layer, detailsLayer, crd) {
    LayerManager.BaseLayer.call(this, layer, detailsLayer, crd);

    this.showLayer = function (leafletlayer) {
        if (legendControl.legendLayer == this.detailsLayer.id)
            this.showLegend();
        if (leafletlayer != null)
            map.removeLayer(leafletlayer);
        return null;
    }

    this.hideLayer = function () {
        return;
    }
};