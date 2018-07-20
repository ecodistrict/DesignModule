
L.Control.windTemperatureView = L.Control.extend({
    options: {
        collapsed: false,
        position: 'topleft',
        autoZIndex: true,
    },

    /// leaflet constructor
    initialize: function (options) {
        L.setOptions(this, options);
    },

    onAdd: function (map) {
        this.active = true;
        this._initLayout();
        this._map = map;
        return this._container;
    },

    onRemove: function () {
        this.active = false;
    },

    _initLayout: function () {
        this._container = L.DomUtil.create('div', 'd3-control-windTemperatureView');

        //Code for making the div draggable
        // add drag support to main div
        this._draggable = new L.Draggable(this._container);
        this._draggable.ref = this;

        this._draggable.enable();

        this.windView = L.DomUtil.create('div', "leaflet-control-windDirection");

        var windControl = L.control.arrow(this.windView);
        map.addControl(windControl);
        this._container.appendChild(windControl.getContainer());

        this.temperatureView = L.DomUtil.create('div', "d3-control-temperatureControl");

        var temperatureControl = L.control.temp(this.temperatureView);
        map.addControl(temperatureControl);
        this._container.appendChild(temperatureControl.getContainer());


        //disabling propagation
        L.DomEvent.disableClickPropagation(this._container);
        L.DomEvent.disableScrollPropagation(this._container);
    },
});

// add temperature constructor for temperature control
L.control.windTemperature = function () {
    return new L.Control.windTemperatureView();
};