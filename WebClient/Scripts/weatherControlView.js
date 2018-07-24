﻿
L.Control.weatherView = L.Control.extend({
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
        this._container = L.DomUtil.create('div', 'control-weatherCotrolView');

        //Code for making the div draggable
        // add drag support to main div
        this._draggable = new L.Draggable(this._container);
        this._draggable.ref = this;

        this._draggable.enable();

        //TODO: drag for the control-weatherCotrolView div does not work with the windDirection control. Users have to click and drag around the temperatureControl div to drag the weatherControl
        this.windView = L.DomUtil.create('div', "leafletcontrol-windDirection");

        this.windControl = L.control.arrow(this.windView);
        map.addControl(this.windControl);
        this._container.appendChild(this.windControl.getContainer());

        this.temperatureView = L.DomUtil.create('div', "d3control-temperatureControl");

        this.temperatureControl = L.control.temp(this.temperatureView);
        map.addControl(this.temperatureControl);
        this._container.appendChild(this.temperatureControl.getContainer());


        //disabling propagation
        L.DomEvent.disableClickPropagation(this._container);
        L.DomEvent.disableScrollPropagation(this._container);

        //this._container.addEventListener('contextmenu', this._handleContextMenu);

        this._container.addEventListener('contextmenu', function (event) {
            event = event || window.event;
            this.temperatureControl.changeMercuryColorLive(event);
            this.windControl.windArrowLive();
            event.preventDefault();
            event.cancelBubble = true;
        }.bind(this));
    },

    _handleContextMenu: function (e) {
        e.preventDefault();
        e.cancelBubble = true;
    },

});

// add temperature constructor for temperature control
L.control.weather = function () {
    return new L.Control.weatherView();
};