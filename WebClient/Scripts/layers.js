function addLayer(parent, data, aWidth, aHeight) {

    var mainDiv = document.createElement('div');
    mainDiv.style.width = (aWidth - 6) + 'px';
    mainDiv.style.height = aHeight + 'px';
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
                for (var i = 0; i < s.length; i++)
                    s[i].className = 'layerDetailsSelected layerDetailsSelectedHidden';

                // check if this layer shows legend -> remove and check if we need to show another legend
                if (typeof legendControl.legendLayer != "undefined" && (legendControl.legendLayer == layer.id)) {
                    //legendControl.clearLegend(true);

                    var legendLayer = null;

                    for (maplayer in map._layers) {
                        if (typeof map._layers[maplayer].domainLayer != "undefined") {
                            if (legendLayer == null) {
                                legendLayer = map._layers[maplayer];
                            }
                            else if (legendLayer._leaflet_id > map._layers[maplayer]._leaflet_id)
                                legendLayer = map._layers[maplayer];
                        }
                    }

                    if (legendLayer != null) {
                        if (legendLayer.idShowing == legendLayer.domainLayer.id) {
                            if (legendLayer.domainLayer.legend)
                                legendControl.createLegend(legendLayer.domainLayer.legend, legendLayer.domainLayer.id);
                        }
                        else if (typeof legendLayer.domainLayer.ref != "undefined" && legendLayer.idShowing == legendLayer.domainLayer.ref.id) {
                            if (legendLayer.domainLayer.ref.legend)
                                legendControl.createLegend(legendLayer.domainLayer.ref.legend, legendLayer.domainLayer.id);
                            else if (legendLayer.domainLayer.legend)
                                legendControl.createLegend(legendLayer.domainLayer.legend, legendLayer.domainLayer.id);
                        }
                        else if (typeof legendLayer.domainLayer.diff != "undefined" && legendLayer.idShowing == legendLayer.domainLayer.diff.id) {
                            if (legendLayer.domainLayer.diff.legend)
                                legendControl.createLegend(legendLayer.domainLayer.diff.legend, legendLayer.domainLayer.id)
                        }
                        else
                            legendControl.clearLegend(false, legendLayer.domainLayer.id);
                    }
                    else
                        legendControl.clearLegend(false, null);
                }
                // check if this layer has the crd -> remove and check if we need to show another crd
                if (typeof crd.crdLayer != "undefined" && layer.id == crd.crdLayer) {
                    var crdlayer = null;
                    for (maplayer in map._layers) {
                        if (typeof map._layers[maplayer].domainLayer != "undefined") {
                            if (crdlayer == null)
                                crdlayer = map._layers[maplayer];
                            else if (crdlayer._leaflet_id < map._layers[maplayer]._leaflet_id)
                                crdlayer = map._layers[maplayer];
                        }
                    }

                    if (crdlayer != null) {

                        if (typeof crdlayer.domainLayer.ref != "undefined" && crdlayer.idShowing == crdlayer.domainLayer.ref.id) {
                            crd.reset(crdlayer.domainLayer != undefined, crdlayer.domainLayer.ref != undefined, crdlayer.domainLayer.diff != undefined, crdlayer.domainLayer, crd.reference);
                        }
                        else if (typeof crdlayer.domainLayer.diff != "undefined" && crdlayer.idShowing == crdlayer.domainLayer.diff.id) {
                            crd.reset(crdlayer.domainLayer != undefined, crdlayer.domainLayer.ref != undefined, crdlayer.domainLayer.diff != undefined, crdlayer.domainLayer, crd.difference);
                        }
                        else {
                            crd.reset(crdlayer.domainLayer != undefined, crdlayer.domainLayer.ref != undefined, crdlayer.domainLayer.diff != undefined, crdlayer.domainLayer);
                        }
                    }
                    else
                        crd.reset(false, false, false);
                }
            }
            else {
                if (!e.ctrlKey) {
                    removeAllDomainLayers();
                    legendControl.legendLayer = null;
                    crd.options.layer = null;
                    var s3 = element.parentNode.getElementsByClassName('layerDetailsSelected');
                    for (var i3 = 0; i3 < s3.length; i3++)
                        s3[i3].className = 'layerDetailsSelected layerDetailsSelectedHidden';
                }
                layer.leaflet_id = addLayerToMap(layer, opacity);
                var s2 = element.getElementsByClassName('layerDetailsSelected');
                for (var i2 = 0; i2 < s2.length; i2++)
                    s2[i2].className = 'layerDetailsSelected';
                // cur-ref-diff
                crd.reset(layer != undefined, layer.ref != undefined, layer.diff != undefined, layer);
                //    function (e) {
                //    // handle switching between current, refference and difference layer
                //    // todo: only implement for tiles now
                //    if (e.target == crd.current) {
                //        if (layer.tileLayer) {
                //            layer.tileLayer.setUrl(layer.tiles, false);
                //            layer.tileLayer.idShowing = layer.id;
                //            if (layer.legend && legendControl.legendLayer == layer.id)
                //                legendControl.createLegend(layer.legend, layer.id);
                //            else if (legendControl.legendLayer == layer.id)
                //                legendControl.clearLegend(false, layer.id);
                //        }
                //    }
                //    else if (e.target == crd.reference) {
                //        if (layer.tileLayer) {
                //            layer.tileLayer.setUrl(layer.ref.tiles, false);
                //            layer.tileLayer.idShowing = layer.ref.id;
                //            if (typeof layer.ref != "undefined" && typeof layer.ref.legend != "undefined" && legendControl.legendLayer == layer.id)
                //            {
                //                legendControl.createLegend(layer.ref.legend, layer.id);
                //            }
                //            else if (typeof layer.legend != "undefined" && legendControl.legendLayer == layer.id) {
                //                legendControl.createLegend(layer.legend, layer.id);
                //            }
                //            else if (legendControl.legendLayer == layer.id) {
                //                legendControl.clearLegend(false, layer.id);
                //            }
                //        }
                //    }
                //    else {
                //        if (layer.tileLayer) {
                //            layer.tileLayer.setUrl(layer.diff.tiles, false);
                //            layer.tileLayer.idShowing = layer.diff.id;
                //            if (layer.diff.legend && legendControl.legendLayer == layer.id)
                //                legendControl.createLegend(layer.diff.legend, layer.id);
                //            else if (legendControl.legendLayer == layer.id)
                //                legendControl.clearLegend(false, layer.id);
                //        }
                //    }
                //});
                crd.crdLayer = layer.id;

                // show legend if first layer
                if (layer.legend && legendControl.legendLayer == null)
                    legendControl.createLegend(layer.legend, layer.id);
                else if (legendControl.legendLayer == null)
                    legendControl.legendLayer = layer.id;
            }

        }
    };
}

function removeDomainLayer(layer) {
    var ml = map._layers[layer.leaflet_id];
    if (ml) {
        ml.remove();
        // todo: NEW MESSAGE FORMAT
        // todo: ml already removed? other way around?
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
        if (ml.basicLayer) {
            ml.remove();
            layerControl.removeLayer(ml);
        }
    }
}

function addLayerToMap(layer, opacity) {
    if (layer.timestamp)
        showLayerTimeStamp(layer.timestamp);
    else
        hideLayerTimeStamp();
    if (!layer.objects)
        layer.showingTiles = true;

    if (typeof layer.tiles !== "undefined" && !layer.objects) { //layer.tiles != '' && 
        var tileLayer;
        tileLayer = L.tileLayer(layer.tiles, {
            opacity: opacity,
            id: layer.name
        });
        tileLayer.domainLayer = layer;  // to identify that this is domain layer
        tileLayer.setZIndex(500);
        tileLayer.addTo(map);
        layer.tileLayer = tileLayer;
        layer.tileLayer.idShowing = layer.id;
        layer.geoJsonLayer = undefined;
        return tileLayer._leaflet_id;
    }
    else {
        var geoJsonLayer = L.geoJson(layer.objects ? layer.objects.features : [], {
            opacity: opacity,
            style: function (feature) {
                if (typeof feature.properties.fillOpacity !== 'undefined')
                    return { color: feature.properties.color };//, fillOpacity: feature.properties.fillOpacity, opacity: feature.properties.fillOpacity };
                    // test code for live trafic..
                else if (feature.properties.color == '#000000')
                    return { color: '#000000', weight: 1 };
                else
                    return { color: feature.properties.color };
            }
        });
        geoJsonLayer.domainLayer = layer;  // to identify that this is domain layer
        geoJsonLayer.setZIndex(500);
        geoJsonLayer.addTo(map);
        layer.geoJsonLayer = geoJsonLayer;
        layer.tileLayer = undefined;
        return geoJsonLayer._leaflet_id;
    }
    // todo: NEW MESSAGE FORMAT
    wsSend({ subscribe: layer.id });
}


function updateTilesLayerOnMap(aElementID, aTilesURL) {
    for (var mlid in map._layers) {
        var layer = map._layers[mlid];
        // todo: check ref or diff IDs also..

        if (layer.domainLayer && layer.idShowing && layer.idShowing == aElementID) {
            layer.setUrl(aTilesURL);
            // var domainLayer = layer.domainLayer;
            //if (domainLayer.tiles != aTilesURL || !layer.redraw) {
            //    // update tiles url
            //    domainLayer.tiles = aTilesURL;
            //    if (!layer.redraw) {
            //        // remove previous layer
            //        removeDomainLayer(domainLayer);
            //        // add as new layer
            //        domainLayer.leaflet_id = addLayerToMap(domainLayer, layer.options.opacity);
            //    }
            //    else {
            //        layer.setUrl(aTilesURL);
            //        //layer.redraw();
            //    }
            //    //layer.removeLayer(layer);
            //    break; // todo: to avoid layer itteration going wrong?
            //}
            //else {
            //    if (layer.redraw)
            //        layer.redraw();
            //    else {
            //        if (layer.tiles) {
            //            //layer.setUrl
            //        }
            //    }
            //}
        }
    }
}

function refreshOtherLayer(aElementID) {
    for (var mlid in map._layers) {
        var layer = map._layers[mlid];
        if (layer.domainLayer && layer.domainLayer.id && layer.domainLayer.id == aElementID) {
            if (layer.redraw)
                layer.redraw();
            else {
                if (layer.tiles) {
                    //layer.setUrl
                }
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

// add basic layer to layers control
// layer: name, [objects|tiles], [default]
function addBasicLayer(layer) {
    var thisLayer = null;
    if (layer.objects)
        thisLayer = L.geoJson(layer.objects.features);
    else if (layer.tiles)
        thisLayer = L.tileLayer(layer.tiles, { id: layer.name });
    // else.. add other types of layers
    if (thisLayer != null) {
        thisLayer.basicLayer = true;
        if (layer.basicID)
            thisLayer.basicID = layer.basicID;
        layerControl.addOverlay(thisLayer, layer.name);
        if (layer.default && layer.default == 1) {
            thisLayer.addTo(map);
            layerControl._update();
        }
    }
}
