// enable draw control for selections
map.options.drawControl = true;

var drawnItems = new L.FeatureGroup();
map.addLayer(drawnItems);

var selectedItems = L.geoJson(undefined, { style: function (feature) { return { color: '#f06eaa' }; } });
//var selectedItemsLayer = map.addLayer(selectedItems);
selectedItems.addTo(map);

// Initialise the draw control and pass it the FeatureGroup of editable layers
var selectControl = new L.Control.Draw({
    draw: {
        marker: false,
        polyline: false
    },
    edit: {
        featureGroup: drawnItems,
        remove: false,
        edit: false
    }
});

// work-a-round for detecting in object selection via 'draw' control..
var inDraw = false;

map.on('draw:drawstart', function () {
    inDraw = true;
});

map.on('draw:drawstop', function () {
    inDraw = false;
});

map.on('draw:created', function (e,e2) {
    var sessionRequest = {};
    sessionRequest.selectObjects = {};
    sessionRequest.selectObjects.type = e.layerType;
    sessionRequest.selectObjects.geometry = e.layer.toGeoJSON();
    if (e.layerType == "circle")
        sessionRequest.selectObjects.radius = e.layer._mRadius;
    sessionRequest.selectObjects.mode = window.event.ctrlKey ? '+' : '=';
    sessionRequest.selectObjects.selectCategories = measuresControl.options.selectCategories;
    wsSend(sessionRequest);
    inDraw = false;
});

// add handler for simple click on map

map.on('click', function (e) {
    if (!inDraw) {
        var popLocation = e.latlng;
        var sessionRequest = {};
        sessionRequest.selectObjects = {};
        sessionRequest.selectObjects.type = 'Point';
        sessionRequest.selectObjects.geometry = {};
        sessionRequest.selectObjects.geometry.geometry = {};
        sessionRequest.selectObjects.geometry.geometry.coordinates = [e.latlng.lng, e.latlng.lat];
        sessionRequest.selectObjects.mode = window.event.ctrlKey ? '~' : '=';
        sessionRequest.selectObjects.selectCategories = window.event.ctrlKey ? measuresControl.options.selectCategories : measuresControl.setSelectCategories([]); // reset selected object type
        wsSend(sessionRequest);
    }
});

function addSelectControl() {
    // reconfigure buttons
    L.drawLocal.draw.toolbar.buttons.polygon = 'Select objects by using a polygon';
    L.drawLocal.draw.toolbar.buttons.rectangle = 'Select objects by using a rectangle';
    L.drawLocal.draw.toolbar.buttons.circle = 'Select objects by using a circle';
    map.addControl(selectControl);
    // add extra button for query
    var selectByQueryButton = document.createElement('A');
    selectByQueryButton.className = 'leaflet-draw-draw-query';
    selectByQueryButton.title = 'Select objects by using a query';
    selectByQueryButton.onclick = handleSelectByQuery;
    L.DomEvent.disableClickPropagation(selectByQueryButton);
    selectControl._container.children[0].children[0].appendChild(selectByQueryButton);
}

function removeSelectControl() {
    map.removeControl(selectControl);
}

function signalSelectByQuery(aQuery) {
    var sessionRequest = {};
    sessionRequest.selectObjects = {};
    sessionRequest.selectObjects.type = 'query';
    sessionRequest.selectObjects.query = aQuery;
    sessionRequest.selectObjects.selectCategories = measuresControl.options.selectCategories;
    sessionRequest.selectObjects.mode = window.event.ctrlKey ? '+' : '=';
    wsSend(sessionRequest);
}

function handleObjectSelection(aSelectedObjects) {
    if (aSelectedObjects.mode == '=') {
        selectedItems.clearLayers();
        selectedItems.addData(aSelectedObjects.objects);
    }
    else {
        for (var o = 0; o < aSelectedObjects.objects.length; o++) {
            var oid = aSelectedObjects.objects[o].properties.id;
            // check if selectedItems contains object: remove from selectedItems else add
            for (var lid in selectedItems._layers) {
                if (selectedItems._layers[lid].feature.properties.id == oid) {
                    // check if we have to invert selection -> actively remove from selected objects
                    if (aSelectedObjects.mode == '~')
                        selectedItems.removeLayer(lid);
                    oid = undefined; // already in list, skip add
                    break;
                }
            }
            // check if marked as already in list
            if (oid != undefined)
                selectedItems.addData(aSelectedObjects.objects[o]);
        }
    }
}

