L.Control.rangeSlider = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topleft',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (options) {
        L.setOptions(this, options);
        var date = new Date();
        this.sliderOptions = {
            range: {
                min: 0.0,
                max: 100.0
            },
            start: [0],
            connect: [true, false],
            orientation: options && options.orientation ? options.orientation : 'horizontal',
            colorInversed: options && options.colorInversed ? options.colorInversed : false
        };
        this.lastValue = options && options.value ? parseFloat(options.value) : null;
        this.sliderID = options && options.ID ? options.ID : 'rangeSlider';
        this.text = ' ';
        this.updateDelta = options && options.updateDelta ? parseInt( options.updateDelta ) : 500; // max update time while sliding 1 second
        this.live = true;
    },

    onAdd: function (map) {
        this.initLayout();
        return this._container;
    },

    onRemove: function (map) {

    },

    initLayout: function () {
        var inversed = (this.sliderOptions.colorInversed) ? 'Inversed' : '';

        var className = (this.sliderOptions && this.sliderOptions.orientation == 'vertical') ? 'leaflet-control-rangeSliderVertical' + inversed : 'leaflet-control-rangeSliderHorizontal' + inversed,
            container = this._container = L.DomUtil.create('div', className);

        container.addEventListener('contextmenu', this.containerRightClick);

        this._sliderDiv = L.DomUtil.create('div', className + '-sliderDiv');
        this._slider = noUiSlider.create(this._sliderDiv, this.sliderOptions);
        this._slider.on('start', this.sliderStart.bind(this));
        this._slider.on('slide', this.sliderMove.bind(this));
        this._slider.on('set', this.sliderSet.bind(this));
        this._container.appendChild(this._sliderDiv);

        this.valueTextDiv = L.DomUtil.create('div', className + '-sliderTextDiv');
        this.valueTextDiv.innerText = this.text;
        this._container.appendChild(this.valueTextDiv);
    },

    containerRightClick: function (e) {
        e.preventDefault(); //prevent showing of contextmenu since we use right-mouse for something else
        e.stopPropagation();
        DataManager.timeRangeSlider.goLive();
    },

    /* Not sure how useful this is atm... */
    goLive: function () {
        this.live = true;
        this._slider.set(this.liveValue);
        wsSend({
            type: "rangeslider",
            ID: this.sliderID,
            payload: { live: true }
        });
    },

    sliderStart: function () {
        this.lastValue = this._slider.get();
        this.live = false;
    },

    sliderMove: function () {
        this.live = false;
        var newValue = this._slider.get();

        var newTime = new Date();
        if (this.lastUpdate == null || newTime - this.lastUpdate > this.updateDelta) {
            this.lastUpdate = newTime;
            this.sliderSet();
        }
    },

    sliderSet: function () {
        if (!this.live) //check to see if we need to send an update
        {
            wsSend({
                type: "rangeslider",
                payload: {
                    ID: this.sliderID,                
                    value: this._slider.get()
                }
            });
        }
        this.updateValueDisplay();
    },

    updateValueDisplay: function () {
        this.valueTextDiv.innerText = this.text;
    },

    update: function (payload) {
        if (payload.live) {
            this.goLive();
        }

        if (payload.range) {
            this.sliderOptions.range = {
                min: parseInt(payload.range.min),
                max: parseInt(payload.range.max)
            }
            this._slider.updateOptions({ range: this.sliderOptions.range });
        }

        if (payload.text) 
        {
            this.text = payload.text;
            this.updateValueDisplay();
        }

        if (payload.value)
        {
            this.liveValue = parseFloat(payload.value);
            if (this.live && this._slider) {
                this._slider.set(this.liveValue);
            }
        }
    }
});