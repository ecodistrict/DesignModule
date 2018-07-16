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

        var timeFormat = d3.timeFormat('%Y-%m-%d %H:%M');

        this.timeSliderToggleView = new TimeSliderToggleView();
        this.timeSliderToggleView.on('clicked', this.expandTimeSlider.bind(this));
        
        this.timeSliderView = new TimeSliderView({
            element: d3.select('div.timeslider').node(),
            model: this.model,
            timeFormat: timeFormat
        });
        this.timeSliderView.on('close', this.collapseTimeSlider.bind(this));
        this.timeSliderView.on('eventSelected', function () {});
        this.timeSliderView.on('timeClicked', this.toggleTimeSliderSettings.bind(this));
        window.addEventListener('resize', this.timeSliderView.resize.bind(this.timeSliderView));

        this.timeSliderSettingsView = new TimeSliderSettingsView({
            map: this.map,
            model: this.model,
            timeFormat: timeFormat
        });
        this.timeSliderSettingsView.on('close', this.hideTimeSliderSettings.bind(this));
    },

    createTimeSlider: function (timeSliderDisplayMode) {
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

    processServerMessage: function (message) {
        // todo
    },

    expandTimeSlider: function () {
        this.timeSliderToggleView.hide();
        this.timeSliderView.show();
        InfoTextControl['leaflet-control-timeslider'] = { description: 'Change the time', active: true };
    },

    collapseTimeSlider: function () {
        this.timeSliderToggleView.show();            
        this.timeSliderView.hide();
        this.hideTimeSliderSettings();
        InfoTextControl['leaflet-control-timeslider'] = { active: false };
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
    }
});
L.extend(TimeSliderController.prototype, L.Evented.prototype);