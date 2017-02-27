showComplaintDialog = function (e) {
    var div = modalDialogCreate('Complaint Information');
    div.id = "complaintDialog";

    var complaint = e.target.complaint;

    var informationContainer = L.DomUtil.create("div", "container", div);
    informationContainer.id = "informationContainer";

    var informationList = L.DomUtil.create("ul", "informationList", informationContainer);
    informationList.id = "informationList";

    var timeLI = L.DomUtil.create("li", "informationLI", informationList);
    timeLI.innerText = "Time: " + DataManager.GetDisplayTime(complaint.time);

    var locationLI = L.DomUtil.create("li", "informationLI", informationList);
    locationLI.innerText = "long: " + complaint.marker.getLatLng().lng + ", lat: " + complaint.marker.getLatLng().lat

    var typeLI = L.DomUtil.create("li", "informationLI", informationList);
    typeLI.innerText = "Type: " + complaint.type;

    var textLI = L.DomUtil.create("li", "informationLI", informationList);
    textLI.innerHTML = "Text: <BR>" + complaint.text;

    modelDialogAddButton(div, 'Remove Complaint', function () {

        modalDialogClose();

        DataManager.RemoveComplaint(complaint.complaintid);

        map.removeLayer(complaint.marker);
    });
    modelDialogAddButton(div, 'Close', modalDialogClose);
}

showSensorDialog = function (e) {
    var div = modalDialogCreate('Sensor Information');
    div.style.width = "600px";

    var sensor = e.target.sensor;

    var informationContainer = L.DomUtil.create("div", "container", div);
    informationContainer.id = "informationContainer";

    var informationList = L.DomUtil.create("ul", "informationList", informationContainer);
    informationList.id = "informationList";

    var nameLI = L.DomUtil.create("li", "informationLI", informationList);
    nameLI.innerText = "Name: " + sensor.name;

    var addressLI = L.DomUtil.create("li", "informationLI", informationList);
    addressLI.innerText = "Address: " + sensor.address;

    var positionLI = L.DomUtil.create("li", "informationLI", informationList);
    positionLI.innerText = "Lat: " + sensor.latitude + " Lng: " + sensor.longitude;

    var substanceLI = L.DomUtil.create("li", "informationLI", informationList);
    substanceLI.innerText = "Measured substance: " + sensor.measuredsubstance;

    if (sensor.data.length > 0) {
        var data = sensor.data[sensor.data.length - 1];

        informationContainer.appendChild(document.createElement("hr"));

        var dataList = L.DomUtil.create("ul", "informationList", informationContainer);
        dataList.id = "dataList";

        var captionLI = L.DomUtil.create("li", "informationLI", dataList);
        captionLI.innerText = " Last received sensor data: ";

        var timeLI = L.DomUtil.create("li", "informationLI", dataList);
        timeLI.innerText = "Time: " + data.time;

        var concentrationLI = L.DomUtil.create("li", "informationLI", dataList);
        concentrationLI.innerText = "Concentration: " + (data.concentration) + " µg/m3";

        var windSpeedLI = L.DomUtil.create("li", "informationLI", dataList);
        windSpeedLI.innerText = "Wind speed: " + data.windSpeed;

        var windDirectionLI = L.DomUtil.create("li", "informationLI", dataList);
        windDirectionLI.innerText = "Wind direction: " + data.windDirection;

        var graphTextLI = L.DomUtil.create("li", "informationLI", dataList);
        graphTextLI.innerHTML = "<BR>Dynamic graph of concentration history: ";

        var graphLI = L.DomUtil.create("li", "informationLI", dataList);

        var graphImage = L.DomUtil.create("img", "graphImage", graphLI);
        graphImage.src = "Content/images/graph-mockup.jpg";
        graphImage.width = 500;



        //var graphSVG = addSensorChart(graphLI, , 500, 300, 50);

    }

    modelDialogAddButton(div, 'Close & Focus Sensor', function () {
        map.panTo(sensor.marker.getLatLng());

        modalDialogClose();

        setTimeout(function () { DataManager.warningGiven = false; }, 10000);
    });
    modelDialogAddButton(div, 'Close', modalDialogClose);
}