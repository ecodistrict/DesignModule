var TimeSliderDisplayMode = {
    COLLAPSE: 1,
    EXPAND: 2    
};

var TimeSliderController = L.Class.extend({

    initialize: function (opts) {
        this.map = opts.map;
        
        this.model = new ScaleModel({
            value: new Date()
        });

        this.timeSliderToggleView = new TimeSliderToggleView();
        this.timeSliderToggleView.on('clicked', this.expandTimeSlider.bind(this));
        
        this.timeSliderView = new TimeSliderView({
            element: d3.select('div.timeslider').node(),
            model: this.model
        });
        this.timeSliderView.on('close', this.collapseTimeSlider.bind(this));
        window.addEventListener('resize', this.timeSliderView.resize.bind(this.timeSliderView));
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
        InfoTextControl['leaflet-control-timeslider'] = { active: false };
    }
});
L.extend(TimeSliderController.prototype, L.Evented.prototype);