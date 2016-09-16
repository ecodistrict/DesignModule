// todo: check licences for noun-project icons

DataManager = {
    complaints: [],
    sensors: [],
    counter: 99,
    warningGiven: false,
    wind: null,

    _addSensor: function (sensor) {

        let scale = 0.5;
        let orr = 128;
        let orrAnchor = { x: 76, y: 66 };

        //var sensorIcon = L.icon({
        //    iconUrl: "Content/images/sensor.png",
        //    shadowUrl: "Content/images/sensor.png",

        //    iconSize: [orr * scale, orr * scale],
        //    shadowSize: [0, 0],
        //    iconAnchor: [orrAnchor.x * scale, orrAnchor.y * scale],
        //    shadowAnchor: [0, 0],
        //    popupAnchor: [0, 0]
        //});

        DataManager._addSensorMarkers(sensor);

        DataManager.sensors.push(sensor);

        //setTimeout(randomSensorValueLoop(sensor), Math.round(Math.random() * 10000));
    },

    _addComplaint: function (complaint) {
        complaint = complaint || {};

        let scale = 0.375;
        let orr = 128;
        let orrAnchor = { x: 64, y: 0 };

        var complaintIcon = L.icon({
            iconUrl: "Content/images/warning2.png",
            shadowUrl: "Content/images/sensor.png",

            iconSize: [orr * scale, orr * scale],
            shadowSize: [0, 0],
            iconAnchor: [orrAnchor.x * scale, orrAnchor.y * scale],
            shadowAnchor: [0, 0],
            popupAnchor: [0, 0]
        });

        var marker = L.marker(getRandomLatLng(), { icon: complaintIcon }).addTo(map).on('click', showComplaintDialog);

        marker.complaint = complaint;
        complaint.marker = marker;

        complaint.data = {
            time: "" + Math.round(Math.random() * 24) + ":" + Math.round(Math.random() * 60),
            location: "long: " + marker.getLatLng().lng + ", lat: " + marker.getLatLng().lat,
            type: "phone call",
            text: "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis, feugiat a, tellus. Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum. Aenean imperdiet. Etiam ultricies nisi vel augue. Curabitur ullamcorper ultricies nisi. Nam eget dui. Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio et ante tincidunt tempus. Donec vitae sapien ut libero venenatis faucibus. Nullam quis ante. Etiam sit amet orci eget eros faucibus tincidunt. Duis leo. Sed fringilla mauris sit amet nibh. Donec sodales sagittis magna. Sed consequat, leo eget bibendum sodales, augue velit cursus nunc."
        }

        DataManager.complaints.push(complaint);
    },

    //_addCar: function (car) {
    //    car = car || {};

    //    let scale = 0.5;
    //    let orr = 128;
    //    let orrAnchor = { x: 64, y: 120 };

    //    var carIcon = L.icon({
    //        iconUrl: "Content/images/sensor-car2.png",
    //        shadowUrl: "Content/images/sensor.png",

    //        iconSize: [orr * scale, orr * scale],
    //        shadowSize: [0, 0],
    //        iconAnchor: [orrAnchor.x * scale, orrAnchor.y * scale],
    //        shadowAnchor: [0, 0],
    //        popupAnchor: [0, 0]
    //    });


    //    var bounds = map.getBounds();


    //    var marker = L.marker(getRandomLatLng(), { icon: carIcon }).addTo(map);

    //    car.marker = marker;
    //    marker.car = car;

    //    var divIcon = L.divIcon({ className: 'text-icon-div' });

    //    var textMarker = L.marker(marker.getLatLng(), { icon: divIcon }).addTo(map);

    //    var textContainer = textMarker._icon;

    //    //var textDiv = L.DomUtil.create("div", textContainer);

    //    var textDiv = textContainer.appendChild(document.createElement("div"));

    //    textDiv.className = "car-text-icon";
    //    textDiv.innerHTML = "" + Math.round(Math.random() * 100);

    //    car.textDiv = textDiv;
    //    textMarker.sensor = textMarker;

    //    car.textMarker = textMarker;

    //    DataManager.cars.push(car);

    //    return car;
    //},

    _getSensor: function (sensorid) {
        for (var i = 0; i < DataManager.sensors.length; i++)
        {
            if (DataManager.sensors[i].sensorid == sensorid)
                return DataManager.sensors[i];
        }

        return null;

    },

    _getComplaint: function (complaintid) {
        for (var i = 0; i < DataManager.complaints.length; i++)
        {
            if (DataManager.complaints[i].complaintid == complaintid)
                return DataManager.complaints[i];
        }

        return null;
    },



    _addSensorMarkers: function (sensor) {

        var icon;

        var isCar = sensor.sensorid == 99;

        if (isCar) {
            let scale = 0.5;
            let orr = 128;
            let orrAnchor = { x: 64, y: 120 };

            icon = L.icon({
                iconUrl: "Content/images/sensor-car3.png",
                shadowUrl: "Content/images/sensor.png",

                iconSize: [orr * scale, orr * scale],
                shadowSize: [0, 0],
                iconAnchor: [orrAnchor.x * scale, orrAnchor.y * scale],
                shadowAnchor: [0, 0],
                popupAnchor: [0, 0]
            });
        }
        else {
            let scale = 0.5;
            let orr = 128;
            let orrAnchor = { x: 52, y: 66 };

            icon =  L.icon({
                iconUrl: "Content/images/sensor2.png",
                shadowUrl: "Content/images/sensor.png",

                iconSize: [orr * scale, orr * scale],
                shadowSize: [0, 0],
                iconAnchor: [orrAnchor.x * scale, orrAnchor.y * scale],
                shadowAnchor: [0, 0],
                popupAnchor: [0, 0]
            });
        }

        marker = L.marker(L.latLng(sensor.latitude, sensor.longitude), { icon: icon }).addTo(map).on('click', showSensorDialog);

        sensor.marker = marker;
        marker.sensor = sensor;

        var divIcon = L.divIcon({ className: 'text-icon-div' });

        var textMarker = L.marker(marker.getLatLng(), { icon: divIcon }).addTo(map).on('click', showSensorDialog);

        var textContainer = textMarker._icon;

        //var textDiv = L.DomUtil.create("div", textContainer);

        var textDiv = textContainer.appendChild(document.createElement("div"));

        if (isCar) {
            textDiv.className = "car-text-icon";
        }
        else {
            textDiv.className = "text-icon";
        }
        textDiv.innerHTML = "" + Math.round(Math.random() * 100);

        sensor.textDiv = textDiv;
        textMarker.sensor = sensor;
        sensor.textMarker = textMarker;

    },

    _getDisplayValue: function (concentration) {

        var micrograms = concentration * 1000000000; // goes from kg/m3 to microgram/m3

        var mgStringHead = micrograms.toString().split(/[,.]+/)[0];

        var count = mgStringHead.length;

        if (count >= 3)
            return Math.round(micrograms);
        else if (count == 2)
            return Math.round(micrograms * 10) / 10;

        return Math.round(micrograms * 100) / 100;

    },

    NewComplaint: function (aComplaint) {
        var complaint = DataManager._getComplaint(aComplaint.complaintid);

        if (complaint == null)
        {
            complaint = {
                complaintid: aComplaint.complaintid,
                time: aComplaint.time,
                type: aComplaint.type,
                latitude: aComplaint.latitude,
                longitude: aComplaint.longitude,
                text: aComplaint.text
            }

            let scale = 0.375;
            let orr = 128;
            let orrAnchor = { x: 64, y: 0 };

            var complaintIcon = L.icon({
                iconUrl: "Content/images/warning2.png",
                shadowUrl: "Content/images/sensor.png",

                iconSize: [orr * scale, orr * scale],
                shadowSize: [0, 0],
                iconAnchor: [orrAnchor.x * scale, orrAnchor.y * scale],
                shadowAnchor: [0, 0],
                popupAnchor: [0, 0]
            });

            var marker = L.marker(getRandomLatLng(), { icon: complaintIcon }).addTo(map).on('click', showComplaintDialog);

            marker.complaint = complaint;
            complaint.marker = marker;

            DataManager.complaints.push(complaint);
        }
    },

    RemoveComplaint: function (complaintid) {
        for (var i = 0; i < DataManager.complaints.length; i++)
        {
            if (DataManager.complaints[i].complaintid == complaintid) {

                map.removeLayer(DataManager.complaints[i].marker);

                DataManager.complaints.splice(i, 1);
            }
        }
    },

    NewSensorData: function (aData) {
        if (typeof aData.sensorid === 'undefined')
            return;

        var sensor = DataManager._getSensor(aData.sensorid);

        //todo logical processing of new data.
        if (typeof aData.latitude !== 'undefined' && typeof aData.longitude !== 'undefined') {
            if (aData.latitude != sensor.latitude || aData.longitude != sensor.longitude)  //update position if the sensor moved
            {
                var pos = L.latLng(aData.latitude, aData.longitude);

                sensor.marker.setLatLng(pos);
                sensor.textMarker.setLatLng(pos);
            }
        }
        aData.windSpeed = DataManager.wind.speed;
        aData.windDirection = DataManager.wind.direction;
        if (typeof aData.concentration !== 'undefined') {
            var micrograms = DataManager._getDisplayValue(aData.concentration);

            sensor.textDiv.innerHTML = "" + micrograms;

            if (micrograms >= 10) {
                showSensorWarning(sensor, aData.concentration);
            }
        }

        sensor.data.push(aData); //Keep a record of all the data we've received;

    },

    NewWindData: function (aData) {
        DataManager.wind.NewData(aData);
    },

    NewSensor: function (aSensor) {
        //check if sensor already excists
        var sensor = DataManager._getSensor(aSensor.sensorid);

        if (sensor == null)
        {
            sensor = {
                sensorid: aSensor.id,
                name: aSensor.name,
                address: aSensor.address,
                latitude: aSensor.latitude,
                longitude: aSensor.longitude,
                measuredsubstance: aSensor.measuredsubstance,
                mobile: aSensor.mobile,
                data: []
            }

            _addSensor(sensor);
        }
    },

    RemoveSensor: function (aSensorid) {
        for (var i = 0; i < DataManager.sensors.length; i++) {
            if (DataManager.sensors[i].complaintid == aSensorid) {

                map.removeLayer(DataManager.sensors[i].marker);
                map.removeLayer(DataManager.sensors[i].textMarker);

                DataManager.sensors.splice(i, 1);
            }
        }
    },

    AddLeak: function () {

        let scale = 0.25;
        let orr = 210;
        let orrAnchor = { x: 105, y: 105 };

        var leakIcon = L.icon({
            iconUrl: "Content/images/leak2.png",
            shadowUrl: "Content/images/sensor.png",

            iconSize: [orr * scale, orr * scale],
            shadowSize: [0, 0],
            iconAnchor: [orrAnchor.x * scale, orrAnchor.y * scale],
            shadowAnchor: [0, 0],
            popupAnchor: [0, 0]
        });

        var marker = L.marker(L.latLng(52.08869341290, 5.16701026653), { icon: leakIcon }).addTo(map).on('click', function (e) { map.removeLayer(e.target); });
    },

    StringyfyObject: function ()
    {
        var object = {
            complaint: {
                type: "Phone call",
                latitude: 52.4098,
                longitude: 4.56982,
                time: "Hier mag je nog kiezen wat je stuurt!",
                text: "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa."
            }
        }

        console.log(JSON.stringify(object));
    }


}


function AddComplaint(complaint)
{
    DataManager._addComplaint(complaint);
}

function AddMultipleSensors(amount)
{
    for (var i = 0; i < amount; i++)
        AddSensor();
}

function AddSensor()
{
    var sensor = CreateRandomSensor();

    DataManager._addSensor(sensor);
}

function getRandomLatLng() {
    var bounds = map.getBounds(),
        southWest = bounds.getSouthWest(),
        northEast = bounds.getNorthEast(),
        lngSpan = northEast.lng - southWest.lng,
        latSpan = northEast.lat - southWest.lat;

    return new L.LatLng(
            southWest.lat + latSpan * Math.random(),
            southWest.lng + lngSpan * Math.random());
}

function CreateRandomSensor() {

    var latLng = getRandomLatLng();

    var id = DataManager.counter;

    DataManager.counter++;

    return {
        sensorid: "" + id,
        name: "Test sensor " + (DataManager.sensors.length + 1),
        address: "Milkyway galaxy",
        latitude: latLng.lat,
        longitude: latLng.lng,
        measuredsubstance: "benzeen",
        mobile: false,
        data: []
    }
}

function randomSensorValueLoop(sensor) {

    var textDiv = sensor.textDiv;

    var newValue = DataManager._getDisplayValue(Math.random() / 10000000);



    textDiv.innerHTML = "" + newValue;

    textDiv.classList.remove('text-icon-animation');

    if (newValue >= 80) {

        void textDiv.offsetWidth;

        textDiv.classList.add('text-icon-animation');
    }

    var delay = Math.round(Math.random() * 10000);

    setTimeout(randomSensorValueLoop, delay, sensor);
}


function NewCarPosition(car, latlng) {
    if (car == "undefined")
        return;

    latlng = latlng || getRandomLatLng();

    car.marker.setLatLng(latlng);
    car.textMarker.setLatLng(latlng);
}

//ShowAllMarkers: function () {
//    this.ShowMarkersFromList(this.complaints);
//    this.ShowMarkersFromList(this.sensors);
//},

//ShowMarkersFromList: function (list) {
//    for (var i = 0; i < list.length; i++) {
//        map.addTo(list[i].marker);

//        if (typeof list[i].textMarker !== 'undefined')
//            map.addTo(list[i].textMarker);
//    }
//},

//HideAllMarkers: function () {
//    this.HideMarkersFromList(this.complaints);
//    this.HideMarkersFromList(this.sensors);
//},

//HideMarkersFromList: function (list) {
//    for (var i = 0; i < list.length; i++)
//    {
//        map.removeLayer(list[i].marker);

//        if (typeof list[i].textMarker !== 'undefined')
//            map.removeLayer(list[i].textMarker);
//    }
//},