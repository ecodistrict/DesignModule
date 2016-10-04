L.Control.StartStop = L.Control.extend({
    options: {
        collapsed: false,
        position: 'topright',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function () {

    },

    onAdd: function (map) {
        this._initLayout();

        this._map = map;

        return this._container;
    },

    onRemove: function () {
    },

    _initLayout: function () {
        var className = 'leaflet-control-startstop-stopped',
		    container = this._container = L.DomUtil.create('div', className);

        container.playing = false;

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);


        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }
        
        container.addEventListener("click", this._clickControl);

        //var form = this._form = L.DomUtil.create('form', className + '-list');

        //var textSpan = form.appendChild(document.createElement("span"));

       //modelDialogAddButton(form, "Start", this._startSimulation);

       //modelDialogAddButton(form, "Stop", this._stopSimulation);
        

        //container.appendChild(form);
        L.DomUtil.addClass(container, 'leaflet-control-startstop');
        L.DomUtil.addClass(container, 'leaflet-control-info-collapsed');
    },

    _clickControl: function (e) {
        var container = e.currentTarget;
        if (!container.playing)
        {
            wsSend({ simulationControl: { start: true } });
            //container.playing = true; //todo wait for server to tell I'm not playing!
        }
        else
        {
            wsSend({ simulationControl: { stop: true } });
            //container.playing = false;
        }
    },

    SimulationStarted: function() {
        this._container.playing = true;
        L.DomUtil.removeClass(this._container, 'leaflet-control-startstop-stopped');
        L.DomUtil.addClass(this._container, 'leaflet-control-startstop-playing');
    },

    SimulationStopped: function() {
        this._container.playing = false;
        L.DomUtil.removeClass(this._container, 'leaflet-control-startstop-playing');
        L.DomUtil.addClass(this._container, 'leaflet-control-startstop-stopped');
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

});

L.control.startstop = function (categories, options) {

    return new L.Control.StartStop(categories, options);

};