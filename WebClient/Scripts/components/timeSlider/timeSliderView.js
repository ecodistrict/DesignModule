var TimeSliderUtils = {
    createTimeScale: function (currentTime) {
        var hour = 1000 * 60 * 60;
        var day = hour * 24;
        var tsStart = new Date(currentTime.getTime() - day);
        var tsEnd = new Date(currentTime.getTime() + day);
        return d3.scaleTime().domain([tsStart, tsEnd]);
    }
};

var TimeSliderView = L.Control.extend({

    initialize: function (opts) {
        this.element = opts.element;
        this.model = opts.model;

        this.initLayout();
    },

    initLayout: function () {
        this.scaleView = new ScaleView({
            element: this.element,
            model: this.model,
            modelValueDecorator: d3.timeFormat('%Y-%m-%d %H:%M'),
            modelValueScaleCreator: TimeSliderUtils.createTimeScale,
            padding: { left: 32, right: 32 }
        });
        this.scaleView.on('eventSelected', this.notifyEventSelected.bind(this));

        var close = L.DomUtil.create('div', 'timeslider-close');
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

    notifyClose: function () {
        this.fire('close');
    },

    notifyEventSelected: function (data) {
        this.fire('eventSelected', data);
    }
});
L.extend(TimeSliderView.prototype, L.Evented.prototype);