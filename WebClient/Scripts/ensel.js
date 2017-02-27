// todo: check licences for noun-project icons

DataManager = {
    complaints: [],
    sensors: [],
    cars: [],
    carshash: {},
    counter: 99,
    warningGiven: false,
    wind: null,
    drawLayer: null,
    session: null,
    queryDialogData: null, // used in queryDialog.js


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

        var pos = L.latLng(complaint.latitude, complaint.longitude);

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
    },

    _getSensor: function (sensorid) {
        for (var i = 0; i < DataManager.sensors.length; i++) {
            if (DataManager.sensors[i].sensorid == sensorid)
                return DataManager.sensors[i];
        }

        return null;

    },

    _getComplaint: function (complaintid) {
        for (var i = 0; i < DataManager.complaints.length; i++) {
            if (DataManager.complaints[i].complaintid == complaintid)
                return DataManager.complaints[i];
        }

        return null;
    },

    _addSensorMarkers: function (sensor) {

        var icon;

        var isCar = sensor.sensorid == 99 || sensor.sensorid == "EFC018EE-E977-4B23-9C6E-49B118E00E9E" || sensor.sensorid == 24;

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

            icon = L.icon({
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
        textDiv.innerHTML = "-";

        sensor.textDiv = textDiv;
        textMarker.sensor = sensor;
        sensor.textMarker = textMarker;

    },

    AddCars: function (aCars) {
        for (var i = 0; i < aCars.length; i++)
            DataManager.AddCar(aCars[i]);

    },

    UpdateCars: function (aCarsData) {
        for (var i = 0; i < aCarsData.length; i++)
            DataManager.UpdateCar(aCarsData[i]);
    },

    RemoveCars: function (aCarIds) {
        for (var i = 0; i < aCarIds.length; i++)
            DataManager.RemoveCar(aCarIds[i].id);
    },

    RemoveAllCars: function () {
        if (DataManager.drawLayer != null)
            DataManager.drawLayer.clearLayers();
        DataManager.carshash = {};
    },

    AddCar: function (aCar) {
        if (DataManager.drawLayer == null) {
            DataManager.drawLayer = L.layerGroup().addTo(map);
        }

        for (var i = 0; i < DataManager.cars.length; i++) {
            if (typeof DataManager.carshash[aCar.id] !== "undefined") {
                return;
            }
        }

        var circle = L.circle([aCar.lat, aCar.lng], 2, {
            color: aCar.fill,
            opacity: 1,
            fillColor: aCar.fill,
            fillOpacity: 0.8,
        }).addTo(DataManager.drawLayer);

        circle.setStyle({ color: aCar.fill, opacity: 1 });

        aCar.circle = circle;
        DataManager.carshash[aCar.id] = aCar;
    },

    UpdateCar: function (aCarData) {
        var car = DataManager.carshash[aCarData.id];
        //for (var i = 0; i < DataManager.cars.length; i++)
        //{
        //    if (aCarData.id == DataManager.cars[i].id)
        //    {
        //        car = DataManager.cars[i]
        //        break;
        //    }
        //}

        if (typeof car === "undefined")
            return;//car not found



        var changed = false;
        if (typeof aCarData.lat !== 'undefined') {
            car.lat = aCarData.lat;
            changed = true;
        }
        if (typeof aCarData.lng !== 'undefined') {
            car.lng = aCarData.lng;
            changed = true;
        }

        var lightChange = false;
        if (aCarData.tis) {
            car.tis = aCarData.tis;
            lightChange = true;
        }

        if (typeof aCarData.bl !== 'undefined') {
            car.bl = aCarData.bl;
            lightChange = true;
        }

        if (lightChange) {
            if (aCarData.bl) {
                car.circle.setStyle({ color: "#ff0000", opacity: 1 })
            }
            else if (aCarData.tis == "LEFT" || aCarData.tis == "RIGHT" || aCarData.tis == "HAZARD") {
                car.circle.setStyle({ color: "#ffcc00", opacity: 1 })
            }
            else {
                car.circle.setStyle({ color: car.fill, opacity: 1 })
            }
        }

        if (changed) {
            car.circle.setLatLng([car.lat, car.lng]);
        }
    },

    RemoveCar: function (aCarId) {
        var car = DataManager.carshash[aCarId];

        if (typeof car === "undefined")
            return;

        DataManager.drawLayer.removeLayer(car.circle);

        delete DataManager.carshash[aCarId];
    },

    _getDisplayValue: function (concentration) {

        var micrograms = concentration;// * 1000000000; // goes from kg/m3 to microgram/m3

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

        if (complaint == null) {
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
        for (var i = 0; i < DataManager.complaints.length; i++) {
            if (DataManager.complaints[i].complaintid == complaintid) {

                map.removeLayer(DataManager.complaints[i].marker);

                DataManager.complaints.splice(i, 1);
            }
        }
    },

    GetDisplayTime: function (aTime) {
        var time = DataManager.BreakdownTime(aTime);

        var systemTime = new Date();

        var utcTime = DataManager.GetTimeObject(time);

        var builder = "";

        //if (time.day != systemTime.getDate() || time.month != (systemTime.getMonth() + 1) || time.year != systemTime.getFullYear())
        //{
        //    builder += time.day + "/" + time.month;
        //}

        //if (time.year != systemTime.getFullYear()) {
        //    builder += "/" + time.year;
        //}

        if (time.year != systemTime.getFullYear()) {
            builder += utcTime.getDay() + "/" + (utcTime.getMonth() + 1) + "/" + utcTime.getYear();
        }
        else if (utcTime.getDay() != systemTime.getDate() || utcTime.getMonth() != (systemTime.getMonth())) {
            builder += time.day;
            switch (parseInt(utcTime.getMonth()) + 1) {
                case 1: builder += " Jan.";
                    break;
                case 2: builder += " Feb.";
                    break;
                case 3: builder += " Mar.";
                    break;
                case 4: builder += " Apr.";
                    break;
                case 5: builder += " May";
                    break;
                case 6: builder += " June";
                    break;
                case 7: builder += " July";
                    break;
                case 8: builder += " Aug.";
                    break;
                case 9: builder += " Sept.";
                    break;
                case 10: builder += " Oct.";
                    break;
                case 11: builder += " Nov.";
                    break;
                case 12: builder += " Dec.";
                    break;
            }
        }
        else {
            builder += "Today";
        }

        builder += " " + utcTime.getHours() + ":" + utcTime.getMinutes();

        if (utcTime.getSeconds() != 0) {
            builder += ":" + utcTime.getSeconds();
        }

        return builder;
    },

    BreakdownTime: function (aTime) {
        var dates = aTime.split(/[ ]+/)[0].split(/[-]+/);
        var times = aTime.split(/[ ]+/)[1].split(/[:]+/);

        return {
            year: dates[0],
            month: dates[1],
            day: dates[2],
            hours: times[0],
            minutes: times[1],
            seconds: times[2]
        }
    },

    GetTimeObject: function (obj) {
        var date = new Date();
        date.setUTCFullYear(obj.year, obj.month - 1, obj.day);
        date.setUTCHours(obj.hours);
        date.setUTCMinutes(obj.minutes);
        date.setUTCSeconds(obj.seconds);
        return date;
    },

    BuildServerTimeStamp: function (date) {
        var builder = "" + date.getUTCFullYear() + "-";

        if (date.getUTCMonth() < 9)
            builder += "0";
        builder += (date.getUTCMonth() + 1) + "-";

        if (date.getUTCDate() < 10)
            builder += "0";
        builder += date.getUTCDate() + " ";

        if (date.getUTCHours() < 10)
            builder += "0";
        builder += date.getUTCHours() + ":";

        if (date.getUTCMinutes() < 10)
            builder += "0";
        builder += date.getUTCMinutes() + ":";

        if (date.getUTCSeconds() < 10)
            builder += "0";
        builder += date.getUTCSeconds();

        return builder;


    },

    NewSensorData: function (aData) {
        if (typeof aData.sensorid === 'undefined')
            return;

        var sensor = DataManager._getSensor(aData.sensorid);

        if (sensor == null) {
            return;
        }

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
                showSensorWarning(sensor, aData);
            }


            sensor.data.push(aData); //Keep a record of all the data we've received;
        }


    },

    NewWindData: function (aData) {


        if (DataManager.wind == null) {
            DataManager.wind = L.control.arrow();
            map.addControl(DataManager.wind);
        }
        DataManager.wind.NewData(aData);
    },

    NewSensor: function (aSensor) {
        //check if sensor already excists
        var sensor = DataManager._getSensor(aSensor.sensorid);

        if (sensor == null) {
            sensor = {
                sensorid: aSensor.sensorid,
                name: aSensor.name,
                address: aSensor.address,
                latitude: aSensor.latitude,
                longitude: aSensor.longitude,
                measuredsubstance: aSensor.measuredsubstance,
                mobile: aSensor.mobile,
                data: []
            }

            DataManager._addSensor(sensor);
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
        let orrX = 128;
        let orrY = 158;
        let orrAnchor = { x: 60, y: 152 };

        var leakIcon = L.icon({
            iconUrl: "Content/images/leak3.png",
            shadowUrl: "Content/images/sensor.png",

            iconSize: [orrX * scale, orrY * scale],
            shadowSize: [0, 0],
            iconAnchor: [orrAnchor.x * scale, orrAnchor.y * scale],
            shadowAnchor: [0, 0],
            popupAnchor: [0, 0]
        });

        var marker = L.marker(L.latLng(52.08869341290, 5.16701026653), { icon: leakIcon }).addTo(map).on('click', function (e) { map.removeLayer(e.target); });
    },

    StringyfyObject: function () {
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

function AddComplaint(complaint) {
    DataManager._addComplaint(complaint);
}

function AddMultipleCars(amount) {
    for (var i = 0; i < amount; i++)
        DataManager.AddCar(CreateRandomCar());
}

function AddMultipleSensors(amount) {
    for (var i = 0; i < amount; i++)
        AddSensor();
}

function StartDancingCars(maxDelay) {
    DataManager.fpsCounter = 0;
    DataManager.fpsLastCounter = 0;
    DataManager.fpsMiliseconds = Date.now();
    DataManager.dancing = true;
    maxDelay = maxDelay || 1000;
    randomCarChangeLoop(maxDelay);
}

function StopDancingCars() {
    DataManager.dancing = false;
}

function ChangeRandomCar() {
    var index = Math.floor(Math.random() * DataManager.cars.length)

    var newpos = getRandomLatLng();

    DataManager.UpdateCar({ id: DataManager.cars[index].id, lat: newpos.lat, lng: newpos.lng });
}

function AddSensor() {
    var sensor = CreateRandomSensor();

    DataManager._addSensor(sensor);
}

function getRandomLatLng() {
    if (typeof DataManager.bounds === 'undefined')
        DataManager.bounds = map.getBounds();

    var bounds = DataManager.bounds;
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

function CreateRandomCar() {
    var latLng = getRandomLatLng();

    var id = DataManager.counter;

    DataManager.counter++;

    return {
        id: "" + id,
        lat: latLng.lat,
        lng: latLng.lng,
        fill: getRandomColor()
    }
}

function getRandomColor() {
    var letters = '0123456789ABCDEF';
    var color = '#';
    for (var i = 0; i < 6; i++) {
        color += letters[Math.floor(Math.random() * 16)];
    }
    return color;
}

function randomCarChangeLoop(maxDelay) {
    if (!DataManager.dancing)
        return;

    ChangeRandomCar();

    DataManager.fpsCounter++;

    if (Date.now() >= DataManager.fpsMiliseconds + 1000) {
        console.log("fps: " + DataManager.fpsCounter);
        DataManager.fpsCounter = 0;
        DataManager.fpsMiliseconds += 1000;
    }

    var delay = Math.round(Math.random() * maxDelay);

    setTimeout(randomCarChangeLoop, maxDelay, maxDelay);
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

function CountDOM() {
    return document.getElementsByTagName('*').length
}
