L.Control.rangeSlider = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topleft',
        autoZIndex: true
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
        this.lastUpdate = new Date();
        this.sliderID = options && options.ID ? options.ID : 'rangeSlider';
        this.text = ' ';
        this.updateDelta = options && options.updateDelta ? parseInt( options.updateDelta ) : 500; // max update time while sliding 1 second
        this.live = true;
        this.acceptText = 0;;
    },

    onAdd: function (map) {
        this.initLayout();
        return this._container;
    },

    onRemove: function (map) {

    },

    initLayout: function () {
        var inversed = (this.sliderOptions.colorInversed) ? 'Inversed' : '';

        var className = (this.sliderOptions && this.sliderOptions.orientation == 'vertical') ? 'leaflet-control-rangeSliderVertical' + inversed : 'leaflet-control-rangeSliderHorizontal' + inversed;
        this._container = L.DomUtil.create('div', className);

        // add drag support to main div
        var draggable = this._draggable = new L.Draggable(this._container);
        this._draggable.enable();
        L.DomEvent.disableClickPropagation(this._container);
        L.DomEvent.disableScrollPropagation(this._container);

        this._container.id = this.sliderID;
        this._container.addEventListener('contextmenu', this.containerRightClick);
        
        this._sliderDiv = L.DomUtil.create('div', className + '-sliderDiv');
        this._slider = noUiSlider.create(this._sliderDiv, this.sliderOptions);
        this._slider.on('start', this.sliderStart.bind(this));
        this._slider.on('slide', this.sliderMove.bind(this));
        this._slider.on('set', this.sliderSet.bind(this));

        // work-a-round for firefox and chrome to avoid dragging of control when dragging slider
        this._slider.on('end', function (e) { draggable.enable(); });
        this._slider.on('start', function (e) { draggable.disable(); });

        this._container.appendChild(this._sliderDiv);

        this.valueTextDiv = L.DomUtil.create('div', className + '-sliderTextDiv');
        this.valueTextDiv.innerText = this.text;
        this._container.appendChild(this.valueTextDiv);
    },

    containerRightClick: function (e) {
        e.preventDefault(); //prevent showing of contextmenu since we use right-mouse for something else
        e.stopPropagation();
        sliderID = e.currentTarget.id;
        slider = DataManager._getSlider(sliderID);
        if (slider) {
            slider.goLive();
        }
    },

    /* Not sure how useful this is atm... */
    goLive: function () {
        this.live = true;
        wsSend({
            type: "rangeslider",
            payload: {
                ID: this.sliderID,
                live: true
            }
        });
    },

    sliderStart: function () {
        this.live = false;
    },

    sliderMove: function () {
        this.live = false;
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
        if (!this.live) {
            this.acceptText++;
        }
    },

    updateValueDisplay: function () {
        this.valueTextDiv.innerText = this.text;
        this.acceptText--;
    },

    update: function (payload) {
        if (payload.live && !this.live) {
            this.live = true; // No need to send message
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
            if (this.live || this.acceptText>0) {
                this.text = payload.text;
                this.updateValueDisplay();
            }
        }

        if (payload.value)
        {
            if (this.live && this._slider) {
                this._slider.set(parseFloat(payload.value));
            }
        }

    }
});