

function showSensorWarning(sensor, data) {
    if (DataManager.warningGiven) {
        //Flash the sensor if we recently had a warning
        textDiv = sensor.textDiv;

        textDiv.classList.remove('text-icon-animation');

        void textDiv.offsetWidth;

        textDiv.classList.add('text-icon-animation');

        return;
    }

    DataManager.warningGiven = true;

    var div = modalDialogCreate('Warning', "unusual reading detected!");
    div.id = "sensorWarningDialog";
    div.style.width = "400px";

    var warningContainer = L.DomUtil.create("div", "warningContainer", div);
    warningContainer.id = "warningContainer";


    var informationList = L.DomUtil.create("ul", "informationList", warningContainer);
    informationList.id = "informationList";

    var warningLI = L.DomUtil.create("li", "informationLI", informationList);
    warningLI.innerText = "Found a reading of: " + (data.concentration) + "  µg/m3";

    var timeLI = L.DomUtil.create("li", "informationLI", informationList);
    timeLI.innerText = "Time: " + DataManager.GetDisplayTime(data.time)

    var nameLI = L.DomUtil.create("li", "informationLI", informationList);
    nameLI.innerText = "Name: " + sensor.name;

    var addressLI = L.DomUtil.create("li", "informationLI", informationList);
    addressLI.innerText = "Address: " + sensor.address;

    var positionLI = L.DomUtil.create("li", "informationLI", informationList);
    positionLI.innerText = "Lat: " + sensor.latitude + " Lng: " + sensor.longitude;

    var substanceLI = L.DomUtil.create("li", "informationLI", informationList);
    substanceLI.innerText = "Measured substance: " + sensor.measuredsubstance;

    var windSpeedLI = L.DomUtil.create("li", "informationLI", informationList);
    windSpeedLI.innerText = "Wind speed: " + DataManager.wind.speed;

    var windDirection = L.DomUtil.create("li", "informationLI", informationList);
    windDirection.innerText = "Wind direction: " + DataManager.wind.direction;

    modelDialogAddButton(div, 'Close & Show Sensor', function () {
        map.panTo(sensor.marker.getLatLng());

        textDiv = sensor.textDiv;

        textDiv.classList.remove('text-icon-animation');

        void textDiv.offsetWidth;

        textDiv.classList.add('text-icon-animation');

        modalDialogClose();

        setTimeout(function(){ DataManager.warningGiven = false; }, 180000);
    });
    modelDialogAddButton(div, 'Close', modalDialogClose);
}