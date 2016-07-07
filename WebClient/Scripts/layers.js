function addLayer(parent, data, aWidth, aHeight) {

    var mainDiv = document.createElement('div');
    mainDiv.style.width = (aWidth-6)+'px';
    mainDiv.style.height = aHeight+'px';
    mainDiv.className = 'layerDetails';
    mainDiv.title = data.description;
    parent.appendChild(mainDiv);

    var h = document.createElement('h4');
    h.textContent = data.name;
    h.className = 'layerDetailsTitle';
    mainDiv.appendChild(h);

    var img = document.createElement('img');
    img.className = 'layerDetailsImg';
    img.src = data.preview;
    img.id = data.id;
    //img.domain = data.domain;
    mainDiv.appendChild(img);

    var divLayerSelected = document.createElement('div');
    divLayerSelected.className = 'layerDetailsSelected layerDetailsSelectedHidden';
    mainDiv.appendChild(divLayerSelected);

    // add ref layer data and click handler to mainDiv
    mainDiv.layer = data;
    mainDiv.onclick = function (e) {
        // if layer is visible: remove else add
        var element = e.target;
        while (element && !element.layer)
            element = element.parentNode;
        if (element) {
            var layer = element.layer;
            var opacity = e.ctrlKey ? 0.5 : 0.8;
            // todo: handle ctrl-click to always add or replace layer

            if (layer.leaflet_id) {
                removeDomainLayer(layer);
                layer.leaflet_id = null;
                var s = element.getElementsByClassName('layerDetailsSelected');
                for(var i=0; i<s.length; i++)
                    s[i].className = 'layerDetailsSelected layerDetailsSelectedHidden';
                legendControl.clearLegend(true);
                // todo: if other layers are visible which legend to show? none for now..
            }
            else {
                if (!e.ctrlKey) {
                    removeAllDomainLayers();
                    var s3 = element.parentNode.getElementsByClassName('layerDetailsSelected');
                    for (var i3 = 0; i3 < s3.length; i3++)
                        s3[i3].className = 'layerDetailsSelected layerDetailsSelectedHidden';
                }
                layer.leaflet_id = addLayerToMap(layer, opacity);
                var s2 = element.getElementsByClassName('layerDetailsSelected');
                for (var i2 = 0; i2 < s2.length; i2++)
                    s2[i2].className = 'layerDetailsSelected';
                // show legend
                if (layer.legend)
                    legendControl.createLegend(layer.legend);
            }
            
        }
    };
}

function removeDomainLayer(layer) {
    var ml = map._layers[layer.leaflet_id];
    if (ml) {
        ml.remove();
        wsSend({ unsubscribe: ml.domainLayer.id });
    }
}

function removeAllDomainLayers() {
    for (var id in map._layers) {
        var ml = map._layers[id];
        if (ml.domainLayer) {
            ml.remove();
            wsSend({ unsubscribe: ml.domainLayer.id });
            if (ml.domainLayer)
                ml.domainLayer.leaflet_id = null;
            ml.domainLayer = null;
        }
    }
}

function addLayerToMap(layer, opacity) {
    if (layer.timestamp)
        showLayerTimeStamp(layer.timestamp);
    else
        hideLayerTimeStamp();
    if (layer.tiles && layer.tiles !== '' && !layer.objects) {
        var tileLayer;
        tileLayer = L.tileLayer(layer.tiles, {
            opacity: opacity,
            id: layer.name
        });
        tileLayer.domainLayer = layer;  // to identify that this is domain layer
        tileLayer.setZIndex(999);
        tileLayer.addTo(map);
        wsSend({ subscribe: layer.id });
        return tileLayer._leaflet_id;
    }
    else {
        var geoJsonLayer = L.geoJson(layer.objects ? layer.objects.features : [], {
            opacity: opacity,
            style: function (feature) {
                // test code for live trafic..
                if (feature.properties.color === '#000000')
                    return { color: '#000000', weight: 1 };
                else
                    return { color: feature.properties.color };
            }
        });
        geoJsonLayer.domainLayer = layer;  // to identify that this is domain layer
        geoJsonLayer.setZIndex(999);
        geoJsonLayer.addTo(map);
        wsSend({ subscribe: layer.id });
        return geoJsonLayer._leaflet_id;
    }
}

function updateTilesLayerOnMap(aElementID, aTilesURL) {
    for (var mlid in map._layers) {
        var layer = map._layers[mlid];
        if (layer.domainLayer && layer.domainLayer.id && layer.domainLayer.id === aElementID) {
            var domainLayer = layer.domainLayer;
            if (domainLayer.tiles !== aTilesURL || !layer.redraw) {
                // update tiles url
                domainLayer.tiles = aTilesURL;
                if (!layer.redraw) {
                    // remove previous layer
                    removeDomainLayer(domainLayer);
                    // add as new layer
                    domainLayer.leaflet_id = addLayerToMap(domainLayer, layer.options.opacity);
                }
                else {
                    layer.setUrl(aTilesURL);
                    //layer.redraw();
                }
                //layer.removeLayer(layer);
                break; // todo: to avoid layer itteration going wrong?
            }
            else {
                if (layer.redraw)
                    layer.redraw();
                else {
                    if (layer.tiles) { }
                    //layer.setUrl
                }
            }
        }
    }
}

function refreshOtherLayer(aElementID) {
    for (var mlid in map._layers) {
        var layer = map._layers[mlid];
        if (layer.domainLayer && layer.domainLayer.id && layer.domainLayer.id === aElementID) {
            if (layer.redraw)
                layer.redraw();
            else {
                if (layer.tiles) { }
            }
        }
    }
}

function showLayerTimeStamp(aTimeStamp) {
    timestamp.classList.remove('timestamp-hidden');
    while (timestamp.lastChild)
        timestamp.removeChild(timestamp.lastChild);
    timestamp.appendChild(document.createTextNode(aTimeStamp));
}

function hideLayerTimeStamp() {
    timestamp.classList.add('timestamp-hidden');
    while (timestamp.lastChild)
        timestamp.removeChild(timestamp.lastChild);
}