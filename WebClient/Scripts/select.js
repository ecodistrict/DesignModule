// enable draw control for selections
map.options.drawControl = true;

var drawnItems = new L.FeatureGroup();
map.addLayer(drawnItems);

// add geometry layer for selected objects (if not tiled layer)
var selectedPointMarker = {
    radius: 4,
    fillColor: "#f06eaa",
    color: "#000",
    weight: 1,
    opacity: 1,
    fillOpacity: 0.8
};

var propertiesTables = {};

function initSelectedObjectsProperties(e) {

    // build request for retrieving object properties
    var command = {};
    command.selectCategories = selectCategories = measuresControl.options.selectCategories;
    command.selectedObjects = getSelectedObjects();
    
    if (command.selectedObjects.length > 0)
        createRequestDialog('Object Properties', 'Properties of the selected objects', 'selectObjectsProperties', showSelectedObjectsProperties, command);
    else
        AddErrorMessage('Unable to get properties: no objects selected', 'warning', 10000);
}

function showSelectedObjectsProperties(container, data) {
    propertiesTables = {};
    objProps = data.selectedObjectsProperties;

    objProps.properties.sort(function (a, b) {
        return a.ordering - b.ordering;
    });

    var title = container.appendChild(document.createElement('h2'));
    title.innerText = 'Selected object properties';

    container.appendChild(document.createElement('HR'));

    tableContainer = container.appendChild(document.createElement('div'));

    tableContainer.id = "attributesContainer";

    buildAttributesTable(tableContainer);
    // attribute names are used as rows
}

function showMeasureProperties(container, data) {
    propertiesTables = {};
    objProps = data;

    objProps.properties.sort(function (a, b) {
        return a.ordering - b.ordering;
    });

    var title = container.appendChild(document.createElement('h2'));
    title.innerText = 'Selected measure properties';

    container.appendChild(document.createElement('HR'));

    tableContainer = container.appendChild(document.createElement('div'));

    tableContainer.id = "attributesContainer";

    buildAttributesTable(tableContainer);
    // attribute names are used as rows
}

function buildAttributesTable(container) {
    var tableContainer = container.appendChild(document.createElement("div"));
    tableContainer.id = "tableContainer";
    for (var i = 0; i < objProps.properties.length; i++) {
        objProps.properties[i].id = objProps.properties[i].name.replace(/\s+/g, '');
        createAttributeTable(objProps.properties[i], tableContainer);
    }

    container.appendChild(document.createElement("hr"));

    var buttonContainer = container.appendChild(document.createElement("div"));
    buttonContainer.className = 'modalDialogDevideButtons';

    modelDialogAddButton(buttonContainer, "Cancel", modalDialogClose);
    modelDialogAddButton(buttonContainer, "Apply", ApplyNewProperties);
}

function ApplyNewProperties() {
    var changes = false;
    var properties = objProps.properties;
    objProps.properties = [];

    for (var i = 0; i < properties.length; i++) {

        var table = propertiesTables[properties[i].id];

        var inputNode = table.querySelectorAll("input")[0];

        if (typeof inputNode !== "undefined") {
            if (inputNode.type != "checkbox") {
                if (!(properties[i].value == null && inputNode.value == "") && properties[i].value != inputNode.value) {
                    changes = true;

                    switch (properties[i].type) {
                        case "int": properties[i].value = parseInt(inputNode.value);
                            break;
                        case "float": properties[i].value = parseFloat(inputNode.value);
                            break;
                        default: properties[i].value = inputNode.value;
                            break;
                    }

                    objProps.properties.push(properties[i]);
                }
            }
            else if (inputNode.checked != BoolParse(properties[i].value)) {
                changes = true;
                properties[i].value = inputNode.checked;
                objProps.properties.push(properties[i]);
            }
        }
        else {
            inputNode = table.querySelectorAll("select")[0];
            if (properties[i].value != inputNode.options[inputNode.selectedIndex].value) {
                changes = true;
                properties[i].value = inputNode.options[inputNode.selectedIndex].value;
                objProps.properties.push(properties[i]);
            }
        }

    }

    if (changes) {
        // send to Server (publishing server)

        var request = {};
        request.applyObjectsProperties = objProps;
        // todo: NEW MESSAGE FORMAT
        wsSend(request);
    }
    else
        objProps.properties = properties;
    //No closing on apply??
    modalDialogClose();
}

function createAttributeTable(aAttribute, aElem) {
    switch (aAttribute.type) {
        case "int": createIntTable(aAttribute, aElem);
            break;
        case "float": createFloatTable(aAttribute, aElem);
            break;
        case "list": createListTable(aAttribute, aElem);
            break;
        case "string": createStringTable(aAttribute, aElem);
            break;
        case "bool": createBoolTable(aAttribute, aElem);
            break;
        default: console.log("Encountered unknown attribute.type variable: " + aAttribute.type + " in: " + aAttribute);
    }
}

function createIntTable(aAttribute, aElem) {
    var table = createEmptyTable(aElem, aAttribute, "int");
    table.attribute = aAttribute;

    var leftCell = table.children[0].children[0];
    var rightCell = table.children[0].children[1];

    leftCell.appendChild(document.createTextNode(aAttribute.name));
    var inputField = rightCell.appendChild(document.createElement("Input"));
    inputField.type = "number";
    inputField.className = "intInput form-control";
    inputField.value = aAttribute.value;
    inputField.disabled = !BoolParse(aAttribute.editable);
}

function createFloatTable(aAttribute, aElem) {
    var table = createEmptyTable(aElem, aAttribute, "float");
    table.attribute = aAttribute;
    //aAttribute.table = table;

    var leftCell = table.children[0].children[0];
    var rightCell = table.children[0].children[1];

    leftCell.appendChild(document.createTextNode(aAttribute.name));
    leftCell.title = aAttribute.name;
    var inputField = rightCell.appendChild(document.createElement("Input"));
    inputField.type = "number";
    inputField.className = "intInput form-control";
    inputField.step = "any";
    inputField.defaultValue = aAttribute.value;
    inputField.value = aAttribute.value;
    inputField.oldValue = aAttribute.value;
    inputField.disabled = !BoolParse(aAttribute.editable);
}

function createListTable(aAttribute, aElem) {
    switch (aAttribute.forced.toUpperCase()) {
        case "Y": createForcedListTable(aAttribute, aElem);
            break;
        case "N": createFreeListTable(aAttribute, aElem);
            break;
        default: console.log("Encountered unknown attribute.forced variable: " + aAttribute.forced + " in: " + aAttribute);
    }
}

function createForcedListTable(aAttribute, aElem) {
    var table = createEmptyTable(aElem, aAttribute, "forcedList");
    table.attribute = aAttribute;

    var leftCell = table.children[0].children[0];
    var rightCell = table.children[0].children[1];

    leftCell.appendChild(document.createTextNode(aAttribute.name));
    leftCell.title = aAttribute.name;
    var inputField = rightCell.appendChild(document.createElement("Select"));
    for (var i = 0; i < aAttribute.options.length; i++) {
        var selectOption = inputField.appendChild(document.createElement("option"));
        selectOption.value = aAttribute.options[i];
        selectOption.innerText = aAttribute.options[i];
    }
    inputField.className = "forcedListInput form-control";
    inputField.disabled = !BoolParse(aAttribute.editable);
    inputField.value = aAttribute.value;
}

function createFreeListTable(aAttribute, aElem) {
    var table = createEmptyTable(aElem, aAttribute, "freeList");
    table.attribute = aAttribute;

    var leftCell = table.children[0].children[0];
    var rightCell = table.children[0].children[1];

    leftCell.appendChild(document.createTextNode(aAttribute.name));
    var inputField = rightCell.appendChild(document.createElement("Input"));
    inputField.type = "text";
    inputField.className = "textInput form-control";
    inputField.value = aAttribute.value;
    inputField.disabled = !BoolParse(aAttribute.editable);

}

function createStringTable(aAttribute, aElem) {
    var table = createEmptyTable(aElem, aAttribute, "string");
    table.attribute = aAttribute;

    var leftCell = table.children[0].children[0];
    var rightCell = table.children[0].children[1];

    leftCell.appendChild(document.createTextNode(aAttribute.name));
    leftCell.title = aAttribute.name;
    var inputField = rightCell.appendChild(document.createElement("Input"));
    inputField.type = "text";
    inputField.className = "textInput form-control";
    inputField.value = aAttribute.value;
    inputField.disabled = !BoolParse(aAttribute.editable);
}

function createBoolTable(aAttribute, aElem) {
    var table = createEmptyTable(aElem, aAttribute, "bool");
    table.attribute = aAttribute;

    var leftCell = table.children[0].children[0];
    var rightCell = table.children[0].children[1];

    leftCell.appendChild(document.createTextNode(aAttribute.name));
    var inputField = rightCell.appendChild(document.createElement("Input"));
    inputField.type = "checkbox";
    inputField.className = "boolInput form-control";
    inputField.checked = BoolParse(aAttribute.value);
    inputField.disabled = !BoolParse(aAttribute.editable);
}

function createEmptyTable(aElem, aAttribute, type) {
    var id = aAttribute.id;

    var table = aElem.appendChild(document.createElement("table"));
    table.className = "attributeTable " + type + "Table";
    table.id = id + "Table";

    var row = table.appendChild(document.createElement("tr"));
    row.className = "attributeRow " + type + "Row";

    var leftCell = row.appendChild(document.createElement("td"));
    leftCell.className = "attributeLeftCell " + type + "LeftCell";

    var rightCell = row.appendChild(document.createElement("td"));
    rightCell.className = "attributeRightCell " + type + "RightCell";

    propertiesTables[aAttribute.id] = table;

    return table;
}

function BoolParse(input) {
    if (typeof input == "string") {
        if (input.toLowerCase() == "true" || input.toLowerCase() == "y")
            return true;
        return false;
    }
    if (typeof input == "boolean")
        return input;
    return false;
}



var selectedItems = L.geoJson(undefined,
    {
      pointToLayer: function (feature, latlng) { return L.circleMarker(latlng, selectedPointMarker); },
      style: function (feature) { return { color: '#f06eaa' }; }
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

map.on('draw:created', function (e) {
    var sessionRequest = {};
    sessionRequest.selectObjects = {};
    sessionRequest.selectObjects.type = e.layerType;
    sessionRequest.selectObjects.geometry = e.layer.toGeoJSON();
    if (e.layerType == "circle")
        sessionRequest.selectObjects.radius = e.layer._mRadius;

    var controlKey;
    if (DataManager.event) {
        controlKey = DataManager.event.ctrlKey;
    } else {
        if (window.event) {
            controlKey = window.event.ctrlKey;
        }
    }

    sessionRequest.selectObjects.mode = controlKey ? '+' : '=';

    sessionRequest.selectObjects.selectCategories = measuresControl.options.selectCategories;
    sessionRequest.selectObjects.activeBasicLayers = [];
    for (layer in DataManager.activeBasicLayers)
        sessionRequest.selectObjects.activeBasicLayers.push(layer);
    // todo: NEW MESSAGE FORMAT
    wsSend(sessionRequest);
    inDraw = false;
    delete DataManager.event;
});

// add handler for simple click on map
map.on('click', function (e) {

    if (canSelect && !inDraw) {
        var popLocation = e.latlng;

        var ctrlPressed;
        if (window.event) {
            ctrlPressed = window.event.ctrlKey;
        }
        else if (e.originalEvent) {
            ctrlPressed = e.originalEvent.ctrlKey;
        }

        if (typeof e.latlng === 'undefined')
            return;

        var sessionRequest = {};
        sessionRequest.selectObjects = {};
        sessionRequest.selectObjects.type = 'Point';
        sessionRequest.selectObjects.geometry = {};
        sessionRequest.selectObjects.geometry.geometry = {};
        sessionRequest.selectObjects.geometry.geometry.coordinates = [e.latlng.lng, e.latlng.lat];
        sessionRequest.selectObjects.zoom = map.getZoom();
        sessionRequest.selectObjects.mode = ctrlPressed ? '~' : '=';
        sessionRequest.selectObjects.selectCategories = ctrlPressed ? measuresControl.options.selectCategories : measuresControl.setSelectCategories([]); // reset selected object type
        sessionRequest.selectObjects.activeBasicLayers = [];
        for (layer in DataManager.activeBasicLayers)
            sessionRequest.selectObjects.activeBasicLayers.push(layer);
        // todo: NEW MESSAGE FORMAT
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
    if (typeof DataManager.selectContextItem == 'undefined' || DataManager.selectContextItem == null)
    {
        DataManager.selectContextItem = map.contextmenu.addItem({
            text: 'Deselect objects',
            callback: handleObjectsDeselect
        });
        DataManager.propertiesContextItem = map.contextmenu.addItem({ text: 'Properties', icon: 'Content/images/info.png', callback: initSelectedObjectsProperties });
    }
}

function removeSelectControl() {
    map.removeControl(selectControl);
    canSelect = false;
    if (DataManager.selectContextItem) {
        map.contextmenu.removeItem(DataManager.selectContextItem);
        DataManager.selectContextItem = null;
    }
}

function signalSelectByQuery(aQuery) {
    var sessionRequest = {};
    sessionRequest.selectObjects = {};
    sessionRequest.selectObjects.type = 'query';
    sessionRequest.selectObjects.query = aQuery;
    sessionRequest.selectObjects.selectCategories = measuresControl.options.selectCategories;
    sessionRequest.selectObjects.mode = '=';
    // todo: NEW MESSAGE FORMAT
    wsSend(sessionRequest);
}

function handleObjectSelection(aSelectedObjects) {
    if (aSelectedObjects.mode == '=') {
        selectedItems.clearLayers();
        selectedItems.addData(aSelectedObjects.objects);
        DataManager.selectedObjectIDs = {};
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

    if (typeof aSelectedObjects.ids != "undefined") //set the selected object ids
    {
        for (var o = 0; o < aSelectedObjects.ids.length; o++)
            DataManager.selectedObjectIDs[aSelectedObjects.ids[o]] = aSelectedObjects.ids[o];
    }
}

function handleObjectsDeselect() {

    selectedItems.clearLayers();
    measuresControl.setSelectCategories([]);
    DataManager.selectedObjectIDs = {};
}

function getSelectedObjects() {
    var objects = [];
    for (var id in DataManager.selectedObjectIDs)
        objects.push(DataManager.selectedObjectIDs[id]);

    if (objects.length == 0) {
        for (var lid in selectedItems._layers)
            objects.push(selectedItems._layers[lid].feature.properties.id);
    }
    return objects;
}
