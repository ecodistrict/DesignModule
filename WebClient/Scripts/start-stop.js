L.Control.StartStop = L.Control.extend({
    collapsed: false,
    //position: 'bottomright',
    position: 'bottomleft',
    autoZIndex: true,
    hideSingleBase: false,

    initialize: function () {

    },

    onAdd: function (map) {
        this._initLayout();

        this._map = map;
        map.on('zoomend', this._checkDisabledLayers, this);

        return this._container;
    },

    onRemove: function () {
        this._map.off('zoomend', this._checkDisabledLayers, this);
    },

    _initLayout: function () {
        var className = 'leaflet-control-startstop',
		    container = this._container = L.DomUtil.create('div', className);

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }
        
        var form = this._form = L.DomUtil.create('form', className + '-list');

        var textSpan = form.appendChild(document.createElement("span"));

       modelDialogAddButton(form, "Start", this._startSimulation);

       modelDialogAddButton(form, "Stop", this._stopSimulation);
        

        container.appendChild(form);

        L.DomUtil.addClass(container, 'leaflet-control-info-expanded');
    },

    _startSimulation: function (e) {
        //alert("Now the simulation should start");
        wsSend({ simulationControl: { start: true } });
    },

    _stopSimulation: function (e) {
        //alert("Now the simulation must stop");
        wsSend({ simulationControl: { stop: true } });
    },

    _expand: function () {
        
    },


    _collapse: function () {
        
    },

    _checkDisabledLayers: function () {
    }


});

L.control.startstop = function (categories, options) {

    return new L.Control.StartStop(categories, options);

};