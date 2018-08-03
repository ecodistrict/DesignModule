/**
 * TimeSlider is a UI component for displaying and manipulating a timeline.
 * 
 * TimeSliderView is a view representing a time slider control and allows 
 * setting current time, applying a brush, displaying time-anchored events.
 */

var TimeSliderView = L.Control.extend({

    initialize: function (opts) {
        this.element = opts.element;
        this.model = opts.model;
        this.timeFormat = opts.timeFormat || d3.timeFormat('%Y-%m-%d %H:%M');
        this.features = L.extend({
            brush: true
        }, opts.features);

        this.render();
    },   

    render: function () {
        if (this.scaleView) return; // already rendered

        this.timesliderViewport = L.DomUtil.create('div', 'timeslider-viewport');
        this.scaleView = new ScaleSliderView({
            element: this.timesliderViewport,
            model: this.model,
            features: this.features,
            modelValueDecorator: this.timeFormat,
            modelValueScaleCreator: TimeSliderUtils.createTimeScale,
        });
        this.scaleView.on('eventSelected', this.notifyEventSelected, this);
        this.scaleView.on('valueClicked', this.notifyTimeClicked, this);
        this.scaleView.on('zoomLevelChanged', this.notifyZoomLevelChanged, this);
        this.element.appendChild(this.timesliderViewport);

        this.close = L.DomUtil.create('div', 'timeslider-close noselect');
        this.close.innerHTML = '&#x2715;';
        this.close.onclick = this.notifyClose.bind(this);
        this.element.appendChild(this.close);
    },

    remove: function () {
        if (!this.scaleView) return; // already removed

        this.element.removeChild(this.timesliderViewport);
        this.element.removeChild(this.close);

        this.scaleView.off('eventSelected', this.notifyEventSelected, this);
        this.scaleView.off('valueClicked', this.notifyTimeClicked, this);
        this.scaleView.off('zoomLevelChanged', this.notifyZoomLevelChanged, this);
        this.scaleView.remove();

        this.timesliderViewport = null;
        this.scaleView = null;
        this.close = null;
    },

    configureFeatures: function (features) {
        this.features = L.extend(this.features, features);
        if (this.scaleView) {
            this.scaleView.configureFeatures(features);
        }
    },

    resize: function () {
        if (this.scaleView) {
            this.scaleView.resize();
        }
    },

    show: function () {
        L.DomUtil.removeClass(this.element, 'hidden');
        this.resize();
    },

    hide: function () {
        L.DomUtil.addClass(this.element, 'hidden');        
    },

    setZoomLevel: function (zoomLevel) {
        if (this.scaleView) {
            this.scaleView.setZoomLevel(zoomLevel);
        }
    },

    notifyClose: function () {
        this.fire('close');
    },

    notifyTimeClicked: function () {
        this.fire('timeClicked');
    },

    notifyEventSelected: function (data) {
        this.fire('eventSelected', data);
    },

    notifyZoomLevelChanged: function (data) {
        this.fire('zoomLevelChanged', data);
    }
});
L.extend(TimeSliderView.prototype, L.Evented.prototype);
