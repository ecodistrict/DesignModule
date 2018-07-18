var TimeSliderDisplayMode = {
    COLLAPSE: 1,
    EXPAND: 2    
};

var TimeSliderController = L.Class.extend({

    initialize: function (opts) {
        this.map = opts.map;
        
        this.model = new ScaleSliderModel({
            value: new Date()
        });
        this.model.on('value', this.selectedTimeChanged.bind(this));
        this.model.on('brush', this.brushChanged.bind(this));

        this.timeFormat = d3.timeFormat('%Y-%m-%d %H:%M');

        this.timeSliderToggleView = new TimeSliderToggleView();
        this.timeSliderToggleView.on('clicked', this.expandTimeSlider.bind(this));
        
        this.timeSliderView = new TimeSliderView({
            element: d3.select('div.timeslider').node(),
            model: this.model,
            timeFormat: this.timeFormat
        });
        this.timeSliderView.on('close', this.collapseTimeSlider.bind(this));
        this.timeSliderView.on('eventSelected', this.eventSelected.bind(this));
        this.timeSliderView.on('timeClicked', this.toggleTimeSliderSettings.bind(this));
        window.addEventListener('resize', this.timeSliderView.resize.bind(this.timeSliderView));

        this.timeSliderSettingsView = new TimeSliderSettingsView({
            map: this.map,
            model: this.model,
            timeFormat: this.timeFormat
        });
        this.timeSliderSettingsView.on('close', this.hideTimeSliderSettings.bind(this));
    },

    createTimeSlider: function (timeSliderDisplayMode, options) {
        options = options || {};
        
        if (options.features) {
            this.timeSliderView.configureFeatures(options.features);
        }
        
        this.map.addControl(this.timeSliderToggleView);
        this.setTimeSliderDisplayMode(timeSliderDisplayMode);
    },

    removeTimeSlider: function () {
        this.map.removeControl(this.timeSliderToggleView);
        this.collapseTimeSlider();
    },

    setTimeSliderDisplayMode: function (timeSliderDisplayMode) {
        if (timeSliderDisplayMode === TimeSliderDisplayMode.COLLAPSE) {
            this.collapseTimeSlider();
        } else if (timeSliderDisplayMode === TimeSliderDisplayMode.EXPAND) {
            this.expandTimeSlider();
        } else {
            console.warn('Invalid timeSliderDisplayMode value: ' + timeSliderDisplayMode);
        }
    },

    processServerMessage: function (messagePayload) {
        var timeParser = d3.timeParse(this.timeFormat);
        
        function convertEvent (e) {
            return {
                start: timeParser(e.start),
                end: timeParser(e.end),
                color: e.color,
                tooltip: e.tooltip,
                level: e.level || 0
            };
        }

        if (messagePayload.setEvents) {
            var events = messagePayload.setEvents.map(convertEvent);
            this.model.setEvents(events);
        }
        else if (messagePayload.addEvents) {
            var events = messagePayload.addEvents.map(convertEvent);
            this.model.addEvents(events);
        }

        if (messagePayload.setCurrentTime) {
            this.model.value = timeParser(messagePayload.setCurrentTime);
        }

        if (messagePayload.setBrush) {
            this.model.brush = {
                start: timeParser(messagePayload.setBrush.start),
                end: timeParser(messagePayload.setBrush.end)
            };
        }

        if (messagePayload.setZoomLevel) {
            this.timeSliderView.setZoomLevel(messagePayload.setZoomLevel);
        }
    },

    expandTimeSlider: function () {
        this.timeSliderToggleView.hide();
        this.timeSliderView.show();
        InfoTextControl['leaflet-control-timeslider'] = { description: 'Change the time', active: true };

        wsSend({
            type: "timeslider",
            payload: { active: true }
        });
    },

    collapseTimeSlider: function () {
        this.timeSliderToggleView.show();            
        this.timeSliderView.hide();
        this.hideTimeSliderSettings();
        InfoTextControl['leaflet-control-timeslider'] = { active: false };

        wsSend({
            type: "timeslider",
            payload: { active: false }
        });
    },

    toggleTimeSliderSettings: function () {        
        if (!this.timeSliderSettingsView.isVisible()) {
            this.showTimeSliderSettings();
        } else {
            this.hideTimeSliderSettings();
        }
    },

    showTimeSliderSettings: function () {
        this.timeSliderSettingsView.show();
    },

    hideTimeSliderSettings: function () {
        this.timeSliderSettingsView.hide();
    },

    selectedTimeChanged: function (data) {
        wsSend({
            type: "timeslider",
            payload: { selectedTime: this.timeFormat(data.value) }
        });
    },

    brushChanged: function (data) {
        var extent;
        if (data.brush) {
            extent = [
                this.timeFormat(data.brush.start),
                this.timeFormat(data.brush.end)
            ];
        } else {
            extent = {};
        }
            
        wsSend({
            type: "timeslider",
            payload: { brush: { extent: extent } }
        });
    },

    eventSelected: function (data) {
        var selectedEvent = {
            start: this.timeFormat(data.event.start),
            end: this.timeFormat(data.event.end),
            color: data.event.color,
            tooltip: data.event.tooltip,
            level: data.event.level
        };

        wsSend({ 
            type: "timeslider",
            payload: { selectedEvent: selectedEvent }
        });
    }
});
L.extend(TimeSliderController.prototype, L.Evented.prototype);