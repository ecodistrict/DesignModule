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


function objectPropertiesSubmit(objProps) {
    var $submitInfo = $('#submit-info');
    $submitInfo.addClass('submit-info-failed');
    $submitInfo.text('');

    var propertiesTable = $('#md-prop-tbl tbody');
    var rowLength = objProps.selectedObjectsProperties.properties.length;
    if (rowLength == 0) return;
    var toUpdate = false;
    // just get first object: since all other objects have the same attribute value
    var anObjProps = objProps.selectedObjectsProperties.properties[0];
    // go through each attribute (<tr> in <tbody>)
    $("#md-prop-tbl > tbody > tr").each(function (i) {
        // get the attribute name
        var firstCell = $(this).find('td:first');
        var attrName = $(this).find('td:first').text();
        // get the value in each <input> or selected value from <select> of the row
        var curtValue = $(this).find('input, select option:selected').val();
        var oldValue = anObjProps[attrName].value.toString();

        // compare with the old value
        // if not the same, update all objects with new value
        if (curtValue != oldValue) {
            // update the attribute value for all the objects
            objProps.selectedObjectsProperties.properties.map(function (props) {
                props[attrName].value = curtValue;
            });
            toUpdate = true;
        }
    });

    // send to Server (publishing server)
    if (toUpdate) {
        var jsonData = JSON.stringify(objProps);
        wsSend(jsonData);
        //submitInfo.className = 'submit-info-succeed';
        //submitInfo.innerText = "submit succeed.";
        $submitInfo.addClass('submit-info-succeed');
        $submitInfo.text("Submit succeed!");
    }
}

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
        showSelectedObjectsProperties(objectProps);
    });
}

// do the validation and enable the Submit button
function handleTableChangeEvent() {
    $('#submit-info').text('');

    // get the value of this <input> inside a <td>
    var $el = $(this).find('input');
    var value = $el.val();
    // get the <input> type and then validate the value
    if ($el.attr('type') == 'number') {
        // do the validation
        if (!/^[-+]?[0-9]+\.[0-9]+$/.test(value) && (!/^[0-9]+$/.test(value))) {
            // TODO: set the original value back??
            $('#submit-info').addClass('submit-info-failed');
            $('#submit-info').text('Must be a number');
            $('#submit-btn').prop('disabled', true);
            return;
        }
    }

    //// update other objects value of the same property name, in general, select 
    //// the columns of the same column index
    //if ($('#modalDialog input[type="checkbox"]').prop('checked')) {
    //    var colIndex = $(this).index() + 1;
    //    $(this).closest('tbody')
    //        .find('tr td:nth-child(' + colIndex + ') input')
    //        .val(value);
    //}
 
    // enable the button
    $('#submit-btn').prop('disabled', false);
}

// handle filling properties dialog with the retrieved values
/*  DOM layout of the modalDiaglog showing the object Properties
DOM
    - div id=modalDialog
    - div id=tableContainer 
    - table
        -thead
            -tr
                -th
                -th
        -tbody
            - tr
            - tr
            - tr
    - div
      - button button  button
    - div
*/
function showSelectedObjectsProperties(aSelectedObjectsProperties) {
    var objectPropertiesDialog = document.getElementById('modalDialog');

    var tblContainer = objectPropertiesDialog.appendChild(document.createElement('div'));
    tblContainer.className = 'tableContainer';
    var mdtable = tblContainer.appendChild(document.createElement('table'));
    mdtable.id = 'md-prop-tbl';
    //mdtable.className = 'modalDialog-table';

    // change the <h2> in the modalDialog
    $('.modalDialog h2').text(objectProps.selectedObjectsProperties.selectCategories + ' properties');

    // adjust the modalDialog width based on the # of object attributes
    //var numAttr = Object.keys(objectProps.selectedObjectsProperties.properties[0]).length;
    //var newWidth = (numAttr * 50 + 50).toString() + "px";
    //$('.modalDialog > div').css('width', newWidth);

    // attribute names are used as rows
    $('#md-prop-tbl').append('<thead class="fixedHeader"><tr><th>Attribute Name</th><th>Value</th></tr></thead>');
    var mdtblBody = mdtable.appendChild(document.createElement('tbody'));
    mdtblBody.className = "scrollContent";

    for (var attrName in objectProps.selectedObjectsProperties.properties[0]) {
        var aRow = mdtblBody.appendChild(document.createElement('tr'));
        aRow.appendChild($("<td>" + attrName + "</td>").get(0));

        var aProperty = objectProps.selectedObjectsProperties.properties[0][attrName];
        var dataType = aProperty.type;
        var dataValue = aProperty.value;
        if (dataType == "selection") {
            var options = aProperty.options.map(function (optionValue) {
                if (dataValue == optionValue)
                    return '<option selected="selected">' + optionValue + '</option>';
                else
                    return '<option>' + optionValue + '</option>';
            });
            //cell.innerHTML = "<select class='cell-dropdownlist'>" + options + "</select>";
            $attrValue = $("<td><select class='cell-dropdownlist'>" + options + "</select></td>");
            aRow.appendChild($attrValue.get(0));
        }
        else {
            // insert attribute values
            if (aProperty.editable == "Y") {
                if (dataType != 'string')
                    //cell.innerHTML = "<input type='number' class='obj-prop-edit' value='" + dataValue  + "' />";
                    aRow.appendChild($("<td><input type='number' class='obj-prop-edit' value='" + dataValue + "' /></td>").get(0));
                else
                    //cell.innerHTML = "<input type='text' class='obj-prop-edit' value='" + dataValue + "' />";
                    aRow.appendChild($("<td><input type='text' class='obj-prop-edit' value='" + dataValue + "' /></td>").get(0));
            }
            else {
                aRow.appendChild($("<td>" + dataValue + "</td>").get(0));
            }
            //else {
            //    if (dataType != 'string')
            //        //cell.innerHTML = "<input type='number' class='obj-prop-edit' value='" + dataValue  + "' />";
            //        aRow.appendChild($("<td><input type='number' class='obj-prop-edit' value='" + dataValue + "' readonly /></td>").get(0));
            //    else
            //        //cell.innerHTML = "<input type='text' class='obj-prop-edit' value='" + dataValue + "' />";
            //        aRow.appendChild($("<td><input type='text' class='obj-prop-edit' readonly value='" + dataValue + "' readonly  /></td>").get(0));
            //}
        }
    }

    // make a <div> for checkbox and buttons
    var btnRow = objectPropertiesDialog.appendChild(document.createElement('div'));      
    // create the submit button
    var $btn = $('<input id="submit-btn" type="button" value="Submit" disabled class="modalDialogButton1"/>');
    $btn.on('click', function () { objectPropertiesSubmit(objectProps); });
    //// create Refresh button
    //var $btnRefresh = $('<input type="button" value="Refresh" class="modalDialogButton1"/>');
    //$btnRefresh.on('click', function () { 
    //    // todo
    //});
    // create Close button
    var $btnClose = $('<input type="button" value="Close" class="modalDialogButton1"/>');
    $btnClose.on('click', function () { modalDialogClose(); });

    // add buttons into the div
    btnRow.appendChild($btn.get(0));
    //btnRow.appendChild($btnRefresh.get(0));
    btnRow.appendChild($btnClose.get(0));

    // add extra div into the dialog
    var submitInfo = objectPropertiesDialog.appendChild(document.createElement('div'));
    submitInfo.id = 'submit-info';

    // event handler: handle table cell change event
    $('td').change(handleTableChangeEvent)
           .on('keyup', handleTableChangeEvent);
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

map.on('draw:created', function (e, e2) {
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


$(document).ready(function () {

})