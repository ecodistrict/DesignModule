L.Control.StartStop = L.Control.extend({
    options: {
        collapsed: false,
        position: 'topright',
        autoZIndex: true,
        hideSingleBase: false,
        disabled: false
    },

    initialize: function () {

    },

    onAdd: function (map) {

        DataManager.simSpeed = 1.0;

        this._initLayout();

        this._map = map;

        CloseSimulation = this.SendSimClose;

        return this._container;
    },

    onRemove: function () {

    },

    _initLayout: function () {
        var className = 'leaflet-control-startstop-stopped  leaflet-control-startstop',
		    container = this._container = L.DomUtil.create('div', className);

        var className = 'leaflet-control-startstop-stopped',
        startstopButton = L.DomUtil.create('a', className);

        container.appendChild(startstopButton);
        container.playing = false;
        container.style.boxShadow = 'none';

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }


        if (!this.options.disabled) {
            container.addEventListener("click", this._clickControl);
            container.addEventListener("contextmenu", this._rightClick);
        }
        else {
            container.style.opacity = 0.7;
            container.style.cursor = "not-allowed";
        }

        //var form = this._form = L.DomUtil.create('form', className + '-list');

        //var textSpan = form.appendChild(document.createElement("span"));

        //modelDialogAddButton(form, "Start", this._startSimulation);

        //modelDialogAddButton(form, "Stop", this._stopSimulation);
        this.container = container;

        //container.appendChild(form);
        L.DomUtil.addClass(startstopButton, 'leaflet-control-startstop');
        L.DomUtil.addClass(startstopButton, 'leaflet-control-info-collapsed');
    },

    disable: function () {
        if (this.options.disabled)
            return;
        this.options.disabled = true;
        if (!this.container)
            return;
        this.container.style.opacity = 0.7;
        this.container.removeEventListener("click", this._clickControl);
        this.container.removeEventListener("contextmenu", this._rightClick);
        this.container.style.cursor = "not-allowed";
    },

    enable: function () {
        if (!this.options.disabled)
            return;
        this.options.disabled = false;
        this.container.style.opacity = 1;
        this.container.addEventListener("click", this._clickControl);
        this.container.addEventListener("contextmenu", this._rightClick);
        this.container.style.cursor = "pointer";
    },

    _rightClick: function (e) {
        e.stopPropagation();
        e.preventDefault();

        var dialog = modalDialogCreate("Set Simulation Speed");
        dialog.style.width = "200px";

        var entries = [{ value: 0.1, label: "0.1" },
                        { value: 0.2, label: "0.2" },
                        { value: 0.5, label: "0.5" },
                        { value: 1.0, label: "1.0 (real)" },
                        { value: 2.0, label: "2.0" },
                        { value: 5.0, label: "5.0" },
                        { value: 10.0, label: "10.0" },
                        { value: 20.0, label: "20.0" },
                        { value: 50.0, label: "50.0" },
                        { value: 100.0, label: "100.0" },
                        { value: 200.0, label: "200.0" },
                        { value: 500.0, label: "500.0" },
                        { value: 1000.0, label: "1000.0" },
                        { value: Number.MAX_VALUE, label: "Max" }];

        DataManager.speedEntries = entries;

        var container = L.DomUtil.create("div", "speedContainer", dialog);
        var form = L.DomUtil.create("form", "simSpeedForm", container);
        var selection = L.DomUtil.create("select", "selectionList", form);
        for (var i = 0; i < entries.length; i++) {
            var option = L.DomUtil.create("option", "listOption", selection);
            option.value = entries[i].value;
            option.innerHTML = entries[i].label;
            if (option.value == DataManager.simSpeed)
                option.selected = "selected";
        }

        selection.onchange = function (e) {
            DataManager.startControl.SendSimSpeed(e.target.value);
        };

    },

    _clickControl: function (e) {
        var container = e.currentTarget;
        if (!container.playing) {
            wsSend({ simulationControl: { start: true } });
            SyncManager.startPress();
            DataManager.startControl.SimulationStarted();
        }
        else {
            wsSend({ simulationControl: { stop: true } });
            SyncManager.stopPress();
            DataManager.startControl.SimulationStopped();
        }
    },

    SyncStartCommand: function () {
        this.SimulationStarted();
        this.SendSimStart();
    },

    SyncStopCommand: function () {
        this.SimulationStopped();
        this.SendSimStop();
    },
    SyncSpeedCommand: function (speed) {
        this.SendSimSpeed(speed);
    },

    SimulationStarted: function () {
        this._container.playing = true;
        L.DomUtil.removeClass(this._container, 'leaflet-control-startstop-stopped');
        L.DomUtil.addClass(this._container, 'leaflet-control-startstop-playing');
    },

    SimulationStopped: function () {
        this._container.playing = false;
        L.DomUtil.removeClass(this._container, 'leaflet-control-startstop-playing');
        L.DomUtil.addClass(this._container, 'leaflet-control-startstop-stopped');
    },

    _startSimulation: function (e) {
        //alert("Now the simulation should start");
        this.SendSimStart();
    },

    _stopSimulation: function (e) {
        //alert("Now the simulation must stop");
        this.SendSimStop();
    },

    _closeSimulation: function () {
        this.SendSimClose();
    },

    SendSimClose: function () {
        wsSend({ closeSimulation: true });
    },

    SendSimStart: function () {
        wsSend({ simulationControl: { start: true } });
    },

    SendSimStop: function () {
        wsSend({ simulationControl: { stop: true } });
    },

    SendSimSpeed: function (speed) {
        wsSend({ "simulationControl": { "speed": speed } });
        SyncManager.speedChange(speed);
    },

    _expand: function () {

    },


    _collapse: function () {

    },

});

L.control.startstop = function (categories, options) {

    return new L.Control.StartStop(categories, options);

};
