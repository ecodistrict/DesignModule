L.Control.TimeRangeSlider = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topleft',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (options) {
        L.setOptions(this, options);

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


    },

    update: function (payload) {

    }
});