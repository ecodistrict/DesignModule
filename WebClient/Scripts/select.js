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

function objectPropertiesSubmit(oriProps) {
    var submitInfo = document.getElementById("submit-info");
    submitInfo.className = 'submit-info-failed';
    submitInfo.innerText = '';
    if (!objectPropertiesValidation()) {
        submitInfo.innerText = 'Validation Failed. Please check the changed properties';
        return;
    }
    // go through the table to construct the JSON string
    var sendInfo = {
        selectedObjectsProperties: {
            selectCategories: oriProps.selectedObjectsProperties.selectCategories,
            properties: []
        }
    };

    var propertiesTable = document.getElementById('md-prop-tbl');
    var rowLength = propertiesTable.rows.length;
    var attrNames = [];
    // get the table headers
    for (i = 0; i < propertiesTable.rows[0].cells.length; ++i)
        attrNames[i] = propertiesTable.rows[0].cells[i].innerText;

    var toUpdate = false;
    // go through each object
    for (r = 1; r < rowLength; ++r) {
        // reference the original object properties
        var oldObjectR = oriProps.selectedObjectsProperties.properties[r - 1];
        var newObjectR = {};
        var valueChanged = false;
        // iterate each property of the object
        for (i = 0; i < propertiesTable.rows[r].cells.length; ++i) {
            // always add the non-editable attribute
            var editable = oldObjectR[attrNames[i]].editable;
            if (editable == "N") {
                newObjectR[attrNames[i]] = {};
                newObjectR[attrNames[i]].value = oldObjectR[attrNames[i]].value;
                newObjectR[attrNames[i]].type = oldObjectR[attrNames[i]].type;
                continue;
            }
            var curtValue = propertiesTable.rows[r].cells[i].firstChild.innerText;
            // 
            if (propertiesTable.rows[r].cells[i].firstChild.nodeName === 'SELECT') {
                var idx = propertiesTable.rows[r].cells[i].firstChild.selectedIndex;
                curtValue = propertiesTable.rows[r].cells[i].firstChild.options[idx].value;
            }
                
            // changed, update the attribute value
            if (curtValue != oldObjectR[attrNames[i]].value) {
                newObjectR[attrNames[i]] = {};
                newObjectR[attrNames[i]].value = curtValue;
                newObjectR[attrNames[i]].type = oldObjectR[attrNames[i]].type;
                valueChanged = true;
            }          
        }
        if (valueChanged) {
            sendInfo.selectedObjectsProperties.properties.push(newObjectR);
            toUpdate = true;
        }
            
    }
    // send to Server (publishing server)
    if (toUpdate) {
        var jsonData = JSON.stringify(sendInfo);
        wsSend(JSON.stringify(sendInfo));
        submitInfo.className = 'submit-info-succeed';
        submitInfo.innerText = "submit succeed.";
    }
}

function objectPropertiesValidation() {
    var propertiesTable = document.getElementById('md-prop-tbl');
    //gets rows of table
    var rowLength = propertiesTable.rows.length;
    if (rowLength == 0)
        return true;
    var colLength = propertiesTable.rows[0].cells.length;

    //loops through each column, the tableheader cell contains the datatype in the className property
    for (i = 0; i < colLength; i++) {
        // get the datatype of the whole column
        var dataType = propertiesTable.rows[0].cells[i].className;
        // get the data in each row of this column
        for (r = 1; r < rowLength; r++) {  
            var value = propertiesTable.rows[r].cells[i].firstChild.innerText;
            if (dataType == "float") {
                if (!/^[-+]?[0-9]+\.[0-9]+$/.test(value) && (!/^[0-9]+$/.test(value))) {
                    // TODO: set the original value back??
                    return false;
                }
                    
            }
            else if (dataType == "int") {
                if (!/^[0-9]+$/.test(value)) {
                    // todo: set the original value back??
                    return false;
                }
            }
            else if (dataType == "string") { }
        }
    }
    return true;
};

// https://codepen.io/KryptoniteDove/post/load-json-file-locally-using-pure-javascript
function loadJSONLocal(callback) {
    var xobj = new XMLHttpRequest();
    xobj.overrideMimeType("application/json");
    xobj.open('GET', 'objectProps.json', true);
    xobj.onreadystatechange = function () {
        if (xobj.readyState == 4 && xobj.status == "200") {
            // Required use of an anonymous callback as .open will NOT return a value but simply returns undefined in asynchronous mode
            callback(xobj.responseText);
        }
    };
    xobj.send(null);
};

function initSelectedObjectsProperties(e) {
    var objectPropertiesDialog = modalDialogCreate("Object properties");
    objectPropertiesDialog.appendChild(document.createElement('hr'));
    //// add close button
    //var mddb = objectPropertiesDialog.appendChild(document.createElement('div'));
    //mddb.className = 'modalDialogDevideButtons';
    //modelDialogAddButton(mddb, 'Close', modalDialogClose);
    // build request for retrieving object properties
    var command = {};
    command.selectObjectsProperties = {};
    command.selectObjectsProperties.selectedCategories = selectCategories = measuresControl.options.selectCategories;
    command.selectObjectsProperties.selectedObjects = getSelectedObjects();
    wsSend(command);

    //
    // debug only, read the JSON file for the properties of selected objects
    // 
    loadJSONLocal(function (response) {
        var objectProps = JSON.parse(response);
        if (objectProps.selectedObjectsProperties.properties.length == 0)
            return;
        var mdtable = document.createElement('table');
        mdtable.id = 'md-prop-tbl';
        mdtable.className = 'modalDialog-table';
    
        // get all the attribute names and use them as table header
        var tableHeaderRow = document.createElement('tr');
        for (var attrName in objectProps.selectedObjectsProperties.properties[0]) {
            var cell = document.createElement('th');
            cell.appendChild(document.createTextNode(attrName));
            // add the attribute type into the className
            cell.className = objectProps.selectedObjectsProperties.properties[0][attrName].type;
            tableHeaderRow.appendChild(cell);
        }
        mdtable.appendChild(tableHeaderRow);

        // add the value into the table, each object takes one row
        objectProps.selectedObjectsProperties.properties.map(function (props) {
            // create a row
            var row = document.createElement('tr');
            for (var attrName in props)
                if (props.hasOwnProperty(attrName)) {
                    var cell = document.createElement('td');
                    var dataType = props[attrName].type;
                    var dataValue = props[attrName].value;
                    if (dataType == 'list') {
                        var options = props[attrName].options.map(function (optionValue) {
                            if (dataValue == optionValue)
                                return '<option selected="selected">' + optionValue + '</option>';
                            else
                                return '<option>' + optionValue + '</option>';
                        });
                        cell.innerHTML = "<select class='cell-dropdownlist'>" + options + "</select>";
                    }
                    // other data type
                    else {
                        // insert attribute values
                        if (props[attrName].editable == "Y") {
                            // for all browser http://stackoverflow.com/questions/16232554/add-rows-to-table-dynamically-and-make-content-editable
                            cell.innerHTML = "<div contenteditable class='cell-editable'>" + props[attrName].value + "</div>";
                        }
                        else
                            cell.innerHTML = "<div>" + props[attrName].value + "</div>";

                    }
                    row.appendChild(cell);
                }
            mdtable.appendChild(row);
        });
        // add the table into the modalDialog
        objectPropertiesDialog.appendChild(mdtable);

        // add buttons in a row
        var btnRow = objectPropertiesDialog.appendChild(document.createElement('div'));
        // add the submit button
        var btn = document.createElement('input');
        btn.type = 'button';
        btn.value = 'Submit';
        btn.className = 'modalDialogButton1';
        btn.addEventListener('click', function() {
            objectPropertiesSubmit(objectProps);
        });
        btnRow.appendChild(btn);
        // add Refresh button
        var btnRefresh = document.createElement('input');
        btnRefresh.type = 'button';
        btnRefresh.value = 'Refresh';
        btnRefresh.className = 'modalDialogButton1';
        btnRefresh.addEventListener('click', function() {
            // TODO:
        });
        btnRow.appendChild(btnRefresh);
        // add Close button
        var btnClose = document.createElement('input');
        btnClose.type = 'button';
        btnClose.value = 'Close';
        btnClose.className = 'modalDialogButton1';
        btnClose.addEventListener('click', function () {
            modalDialogClose();
        });
        btnRow.appendChild(btnClose);

        // add extra div into the dialog
        var submitInfo = objectPropertiesDialog.appendChild(document.createElement('div'));
        submitInfo.id = 'submit-info';
    });
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

