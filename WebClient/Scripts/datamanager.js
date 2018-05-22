DataManager = {
    wind: null,
    session: null,
    queryDialogData: null, // used in queryDialog.js
    activeBasicLayers: {}, // used in select.js
    selectedObjectIDs: {}, // used in select.js and formdialog.js

    /* moved to graph manager
    PublisherDateTimeToDate: function (aDateTime) {
        return new Date((aDateTime - 25569) * 86400 * 1000);
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
        };
    },

    GetTimeObject: function (obj) {
        var date = new Date();
        date.setUTCFullYear(obj.year, obj.month - 1, obj.day);
        date.setUTCHours(obj.hours);
        date.setUTCMinutes(obj.minutes);
        date.setUTCSeconds(obj.seconds);
        return date;
    },
    */

    NewRangeTimeSliderData: function (aData) {
        if (DataManager.timeRangeSlider == null) {
            DataManager.timeRangeSlider = new L.Control.TimeRangeSlider();
            map.addControl(DataManager.timeRangeSlider);
        }
        DataManager.timeRangeSlider.update(aData);
    },


    _addSlider: function (slider) {
        DataManager.sliders.push(slider);
    },

    _getSlider: function (sliderID) {
        for (var i = 0; i < DataManager.sliders.length; i++) {
            if (DataManager.sliders[i].sliderID == sliderID)
                return DataManager.sliders[i];
        }
        return null;
    },

    NewRangeSliderData: function (aData) {
        slider = DataManager._getSlider(aData.ID);

        if (slider == null) {
            slider = new L.Control.rangeSlider(aData);
            DataManager._addSlider(slider);
            map.addControl(slider);
        }
        slider.update(aData);
    },

    NewWindData: function (aData) {
        if (DataManager.wind == null) {
            DataManager.wind = L.control.arrow();
            map.addControl(DataManager.wind);
        }
        else if (!DataManager.wind.active)
            map.addControl(DataManager.wind);
        DataManager.wind.NewData(aData);
    }
};

