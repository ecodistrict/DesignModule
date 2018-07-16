var TimeSliderView = L.Control.extend({

    initialize: function (opts) {
        this.element = opts.element;
        this.model = opts.model;
        this.timeFormat = opts.timeFormat || d3.timeFormat('%Y-%m-%d %H:%M');

        this.initLayout();
    },

    initLayout: function () {
        this.scaleView = new ScaleSliderView({
            element: this.element,
            model: this.model,
            modelValueDecorator: this.timeFormat,
            modelValueScaleCreator: TimeSliderUtils.createTimeScale,
            padding: { left: 32, right: 32 }
        });
        this.scaleView.on('eventSelected', this.notifyEventSelected.bind(this));
        this.scaleView.on('valueClicked', this.notifyTimeClicked.bind(this));

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

    notifyTimeClicked: function () {
        this.fire('timeClicked');
    },

    notifyEventSelected: function (data) {
        this.fire('eventSelected', data);
    }
});
L.extend(TimeSliderView.prototype, L.Evented.prototype);