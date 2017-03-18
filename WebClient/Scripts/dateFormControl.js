L.Control.dateForm = L.Control.extend({
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

        var className = 'leaflet-control-dateForm',
        container = this._container = L.DomUtil.create('div', className);

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        var link = this._layersLink = L.DomUtil.create('a', className + '-toggle', container);
        link.href = '#';
        link.title = 'Selecteer data en tijden om weer te geven';
        link.onclick = this._clickControl;
        L.DomEvent.disableClickPropagation(link);

    },

    _clickControl: function (e) {
        e.target.blur();
        DataManager.formDialogID = "bikeForm";
        openFormDialog("Selecteer data en tijden om weer te geven", DataManager.formData);
    },

    _update: function () {

        if (!this._container) { return this; }


        return this;
    },

    _openModal: function () {

    }



});

L.control.dateForm = function (options) {
    return new L.Control.dateForm(options);
};
