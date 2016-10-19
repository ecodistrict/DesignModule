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
    link.onclick = openSimulationDialog;
    L.DomEvent.disableClickPropagation(link);

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
