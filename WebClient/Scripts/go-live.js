L.Control.GoLive = L.Control.extend({
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
        

        this._initLayout();

        this._map = map;

        return this._container;
    },

    onRemove: function () {

    },

    _initLayout: function () {
        var className = 'leaflet-control-golive',
		    container = this._container = L.DomUtil.create('div', className);

        var className = 'leaflet-control-golive',
            goLiveButton = L.DomUtil.create('a', className);

        container.appendChild(goLiveButton);
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
        L.DomUtil.addClass(goLiveButton, 'leaflet-control-golive-button');
    },
    
    _clickControl: function (e) {
        wsSend({ goLive: true });
    },
    
    _expand: function () {

    },


    _collapse: function () {

    },

});

L.control.golive = function (categories, options) {

    return new L.Control.GoLive(categories, options);

};
