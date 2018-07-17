var TimeSliderView = L.Control.extend({

    initialize: function (opts) {
        this.element = opts.element;
        this.model = opts.model;
        this.timeFormat = opts.timeFormat || d3.timeFormat('%Y-%m-%d %H:%M');

        this.initLayout();
    },

    initLayout: function () {
        var timesliderViewport = L.DomUtil.create('div', 'timeslider-viewport');        
        this.scaleView = new ScaleSliderView({
            element: timesliderViewport,
            model: this.model,
            modelValueDecorator: this.timeFormat,
            modelValueScaleCreator: TimeSliderUtils.createTimeScale,
        });
        this.scaleView.on('eventSelected', this.notifyEventSelected.bind(this));
        this.scaleView.on('valueClicked', this.notifyTimeClicked.bind(this));
        this.element.appendChild(timesliderViewport);

        var close = L.DomUtil.create('div', 'timeslider-close noselect');
        close.innerHTML = '&#x2715;';
        close.onclick = this.notifyClose.bind(this);
        this.element.appendChild(close);
    },

    resize: function () {
        this.scaleView.resize();
    },

    show: function () {
        L.DomUtil.removeClass(this.element, 'hidden');
        this.resize();
    },

    hide: function () {
        L.DomUtil.addClass(this.element, 'hidden');        
    },

    setZoomLevel: function (zoomLevel) {
        this.scaleView.setZoomLevel(zoomLevel);
    },

    notifyClose: function () {
        this.fire('close');
    },

    notifyTimeClicked: function () {
        this.fire('timeClicked');
    },

    notifyEventSelected: function (data) {
        this.fire('eventSelected', data);
    }
});
L.extend(TimeSliderView.prototype, L.Evented.prototype);