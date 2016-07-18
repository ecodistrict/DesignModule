// enable draw control for selections
map.options.drawControl = true;

var drawnItems = new L.FeatureGroup();
map.addLayer(drawnItems);

// add geometry layer for selected objects (if not tiled layer)
var selectedPointMarker = {
    radius: 4,
    fillColor: "#f06eaa",
    color: "#000",
    weight: 1   ,
    opacity: 1,
    fillOpacity: 0.8
};

function initSelectedObjectsProperties(e) {
    var objectPropertiesDialog = modalDialogCreate("Object properties");
    // add close button
    objectPropertiesDialog.appendChild(document.createElement('hr'));
    var mddb = objectPropertiesDialog.appendChild(document.createElement('div'));
    mddb.className = 'modalDialogDevideButtons';
    modelDialogAddButton(mddb, 'Close', modalDialogClose);
    // build request for retrieving object properties
    var command = {};
    command.selectedObjectsProperties = {};
    command.selectedObjectsProperties.selectedCategories = selectCategories = measuresControl.options.selectCategories;
    command.selectedObjectsProperties.selectedObjects = getSelectedObjects();
    wsSend(command);
}

// handle filling properties dialog with the retrieved values
function showSelectedObjectsProperties(aSelectedObjectsProperties) {
    var dialog = document.getElementById('modalDialog');
    // todo: implement
    // find content element

}

var selectedItems = L.geoJson(undefined,
    {
        pointToLayer: function (feature, latlng) { return L.circleMarker(latlng, selectedPointMarker); },
        style: function (feature) { return { color: '#f06eaa' }; },

        contextmenu: true,
        contextmenuWidth: 140,
        contextmenuItems: [
            '-',
            { text: 'Properties', icon: 'Content/images/info.png', callback: initSelectedObjectsProperties }]
    });
selectedItems.setZIndex(1000);
selectedItems.addTo(map);
var canSelect = false;

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
    if (canSelect && !inDraw) {
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
    canSelect = true;
}

function removeSelectControl() {
    map.removeControl(selectControl);
    canSelect = false;
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
            if (oid !== undefined)
                selectedItems.addData(aSelectedObjects.objects[o]);
        }
        // check if no objects are selected -> deselect categories
        var lid2 = undefined;
        for (var l in selectedItems._layers)
            lid2 = l;
        if (lid2 == undefined)
            measuresControl.setSelectCategories([]);
    }
}

function handleObjectsDeselect() {
    selectedItems.clearLayers();
}

function getSelectedObjects() {
    var objects = [];
    for (var lid in selectedItems._layers)
        objects.push(selectedItems._layers[lid].feature.properties.id);
    return objects;
}

