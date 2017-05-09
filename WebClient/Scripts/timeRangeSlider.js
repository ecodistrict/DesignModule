L.Control.TimeRangeSlider = L.Control.extend({
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
                min: date.getTime() - 50000000,
                max: date.getTime() + 50000000
            },
            start: [0],
            connect: [true, false]
        };
        this.lastUpdate = null;
        this.lastValue = null;
        this.updateDelta = 500;
        this.valueDelta = 10; // percentage
        this.liveTime = 0;
        this.live = true;
    },

    onAdd: function (map) {
        this.initLayout();
        return this._container;
    },

    onRemove: function (map) {

    },

    initLayout: function () {
        var className = 'leaflet-control-timeRangeSlider',
            container = this._container = L.DomUtil.create('div', className);

        container.addEventListener('contextmenu', this.containerRightClick);

        this._sliderDiv = L.DomUtil.create('div', className + '-sliderDiv');
        this._slider = noUiSlider.create(this._sliderDiv, this.sliderOptions);
        this._slider.on('start', this.sliderStart.bind(this));
        this._slider.on('slide', this.sliderMove.bind(this));
        this._slider.on('set', this.sliderSet.bind(this));
        this._container.appendChild(this._sliderDiv);

        this.timeTextDiv = L.DomUtil.create('div', className + '-sliderTextDiv');
        this.timeTextDiv.innerText = 'No Time Selected';
        this._container.appendChild(this.timeTextDiv);
    },

    containerRightClick: function (e) {
        e.preventDefault(); //prevent showing of contextmenu since we use right-mouse for something else
        e.stopPropagation();
        DataManager.rangeTimeSlider.goLive();
    },

    goLive: function () {
        this.live = true;
        this._slider.set(this.liveTime);
        wsSend({
            type: "timerangeslider",
            payload: { live: true }
        });
    },

    sliderStart: function () {
        this.lastValue = this._slider.get();
        this.lastUpdate = null;
        this.live = false;
    },

    sliderMove: function () {
        this.live = false;
        var newValue = this._slider.get();
        var newTime = new Date();
        //Math.abs(newValue - this.lastValue) / (this._slider.options.range.max - this._slider.options.range.min) > this.valueDelta || 
        if (this.lastUpdate == null || newTime - this.lastUpdate > this.updateDelta) {
            this.lastValue = newValue;
            this.lastUpdate = newTime;
            this.sliderSet();
        }

        this.updateTimeDisplay();
    },

    sliderSet: function () {
        if (!this.live) //check to see if we need to send an update
        {
            wsSend({
                type: "timerangeslider",
                payload: {
                    time: this.doubleToDate(this._slider.get())
                }
            });
        }
        this.updateTimeDisplay();
    },

    updateTimeDisplay: function () {
        this.timeTextDiv.innerText = (new Date(Math.round(this._slider.get()))).toLocaleString();
    },

    doubleToDate: function (aDouble) {
        var date = new Date(Math.round(aDouble));
        var dateString = date.toISOString();
        var dateSplit = dateString.split('T');
        dateString = dateSplit[0] + ' ' + dateSplit[1];
        return dateString.substring(0, dateString.length - 5); //trim miliseconds and Z
        
    },

    dateToDouble: function (aDate) {
        var dateString = aDate.replace(' ', 'T');
        dateString += 'Z';
        return (new Date(dateString)).getTime();
    },

    update: function (payload) {
        if (payload.range) {
            this.sliderOptions.range = {
                min: this.dateToDouble(payload.range.min),
                max: this.dateToDouble(payload.range.max)
            }
            this._slider.updateOptions({ range: this.sliderOptions.range });
        }

        if (payload.time)
        {
            this.liveTime = this.dateToDouble(payload.time);
            if (this.live && this._slider) {
                this._slider.set(this.liveTime);
                this.updateTimeDisplay();
            }
        }

        if (payload.live)
            this.goLive();
    }
});