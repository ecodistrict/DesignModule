L.Control.ControlsControl = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topleft',
        autoZIndex: true
    },

    initialize: function (options) {
        L.setOptions(this, options);
        this.rightMargin = '75px';
        this.bottomMargin = '75px';
        this._enabled = false;
    },

    onAdd: function (map) {
        this._initLayout();
        this._map = map;
        this._enabled = true;
        return this._container;
    },

    onRemove: function (map) {
        this._enabled = false;
    },

    _initLayout: function () {
        var className = 'leaflet-control-controls',
            container = this._container = L.DomUtil.create('div', className);

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }

        container.addEventListener("click", this.clickControlsControl.bind(this));

        var link = this._categoriesLink = L.DomUtil.create('a', className + '-toggle', container);
        link.href = '#';
        link.title = 'Show scenario controls information';
    },

    clickControlsControl: function () {
        ScenarioControlsManager.showScenario();
    }

});

L.control.ControlsControl = function (categories, options) {
    return new L.Control.ControlsControl(categories, options);
};

L.Control.OverviewControl = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topleft',
        autoZIndex: true
    },

    initialize: function (options) {
        L.setOptions(this, options);
        this.rightMargin = '75px';
        this.bottomMargin = '75px';
        this._enabled = false;
    },

    onAdd: function (map) {
        this._initLayout();
        this._map = map;
        this._enabled = true;
        return this._container;
    },

    onRemove: function (map) {
        this._enabled = false;
    },

    _initLayout: function () {
        var className = 'leaflet-control-overview',
            container = this._container = L.DomUtil.create('div', className);

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }

        container.addEventListener("click", this.clickControlsControl.bind(this));

        var link = this._categoriesLink = L.DomUtil.create('a', className + '-toggle', container);
        link.href = '#';
        link.title = 'Show scenarios controls information';
    },

    clickControlsControl: function () {
        ScenarioControlsManager.showScenarios();
    }

});

L.control.OverviewControl = function (categories, options) {
    return new L.Control.OverviewControl(categories, options);
};