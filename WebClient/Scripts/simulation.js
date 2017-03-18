L.Control.simulation = L.Control.extend({
    options: {
        position: 'topright',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (options) {
        L.setOptions(this, options);
        // this._historyItems = [];
        // this._measureItems = [];
        // this._historyList;
        // this._measureList;
    },

    onAdd: function (map) {
        this._initLayout();
        this._update();

        this._map = map;
        return this._container;
    },


    _initLayout: function () {

        var className = 'leaflet-control-simulation',
        container = this._container = L.DomUtil.create('div', className);

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        var link = this._layersLink = L.DomUtil.create('a', className + '-toggle', container);
        link.href = '#';
        link.title = 'Create Simulation';
        link.onclick = this._clickControl;
        L.DomEvent.disableClickPropagation(link);

    },

    _clickControl: function (e) {
        e.target.blur();
        DataManager.formDialogID = "createSimulation";
        openFormDialog("Create Simulation", DataManager.simulationSetupData);
    },

    _update: function () {

        if (!this._container) { return this; }


        return this;
    },

    _openModal: function () {

    }



});

L.control.simulation = function (options) {
    return new L.Control.simulation(options);
};

L.Control.SimulationClose = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topright',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (options) {
        L.setOptions(this, options);
    },

    onAdd: function (map) {
        this._initLayout();
        this._update();
        this._map = map;
        return this._container;
    },

    onRemove: function () {
        //todo, do we want to remove controls?
    },

    _initLayout: function () {
        var className = 'leaflet-control-simclose',
            container = this._container = L.DomUtil.create('div', className);

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }

        container.addEventListener("click", this.showSimCloseDialog);

        var link = this._categoriesLink = L.DomUtil.create('a', className + '-toggle', container);
        link.href = '#';
        link.title = 'Close the simulation';
    },

    showSimCloseDialog: function () {
        var div = modalDialogCreate('Close Simulation');
        div.id = "simulationCloseDialog";

        var text = div.appendChild(document.createElement("span"));
        text.id = "simCloseTextSpan";
        text.innerHTML = "Are you sure you want to close the simulation?";

        var whiteSpace = div.appendChild(document.createElement("p"));

        var mddb = div.appendChild(document.createElement("div"));
        mddb.className = "modalDialogDevideButtons";

        modelDialogAddButton(mddb, 'No', function () {
            modalDialogClose();
        });
        modelDialogAddButton(mddb, 'Yes', function () {
            wsSend({ closeSimulation: true });
            modalDialogClose();
        });


    },

    _update: function () {

    },

    _expand: function () {
        //L.DomEvent.addListener(this._container, 'touchmove', L.DomEvent.stopPropagation);
        //if (this.hasElements()) {
        //    L.DomUtil.addClass(this._container, 'leaflet-control-details-expanded');
        //    this._form.style.height = null;
        //    var acceptableHeight = this._map._size.y - (this._container.offsetTop + 50);
        //    if (acceptableHeight < this._form.scrollHeight) {
        //        L.DomUtil.addClass(this._form, 'leaflet-control-details-scrollbar');
        //        this._form.style.height = acceptableHeight + 'px';
        //    }
        //    else {
        //        L.DomUtil.removeClass(this._form, 'leaflet-control-details-scrollbar');
        //    }
        //}
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-details-expanded');
    }
});

L.control.SimulationClose = function (options) {
    return new L.Control.SimulationClose(options);
};
