var MapViewModel = L.Evented.extend({

    Initialize: function (opts) {
        var options = opts || {};

        var _view = options.view || { lat: 52.08606, lon: 5.17689, zoom: 11 };
        Object.defineProperty(this, 'view', {
            get: function () { return _view; },
            set: function (newView) {
                if (_view !== newView) {
                    _view = newView;
                    this.fire('view', { view: _view });
                }
            }
        });

        var _lead = options.lead || false;
        Object.defineProperty(this, 'lead', {
            get: function () { return _lead; },
            set: function (newLead) {
                if (_lead !== newLead) {
                    _lead = newLead;
                    this.fire('lead', { lead: _lead });
                }
            }
        });

        var _follow = options.follow || true;
        Object.defineProperty(this, 'follow', {
            get: function () { return _follow; },
            set: function (newFollow) {
                if (_follow !== newFollow) {
                    _follow = newFollow;
                    this.fire('follow', { follow: _follow });
                }
            }
        });
    }
});


// util to parse url parameters ie get the lat/lon/zoom/session
function getParameterByName(name, def) {
    name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
    var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
        results = regex.exec(location.search);
    return results == null ? def : decodeURIComponent(results[1].replace(/\+/g, " "));
}

// base layers via mapbox, mapbox url's
mapboxUrl = 'https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiY29ybmVsaXNzZW5qYSIsImEiOiJjaWo5end1azYwMDN6dWxrbnI3eDd4ZzF0In0.tkDIhjkvxgARGteWbAeqPg';
mapboxAttribution = '© <a href="https://www.mapbox.com/about/maps/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>';

// define base layers
var baseLayers = {
    "Street": L.tileLayer(mapboxUrl, { id: 'mapbox.streets', attribution: mapboxAttribution }),
    "Grayscale": L.tileLayer(mapboxUrl, { id: 'mapbox.light', attribution: mapboxAttribution }),
    "Satellite": L.tileLayer(mapboxUrl, { id: 'mapbox.streets-satellite', attribution: mapboxAttribution })
};

// create the map
var map = L.map('map', {
    center: [getParameterByName('lat', 52.08606), getParameterByName('lon', 5.17689)],
    zoom: getParameterByName('zoom', 11), maxZoom: 18, minZoom: 2,
    layers: [baseLayers["Street"]], // default enabled layer
    contextmenu: true,
    contextmenuItems:
        getParameterByName('debug', 0) ?
            [
                { text: 'Show coordinates', callback: showCoordinates },
                { text: 'Deselect objects', callback: handleObjectsDeselect }
            ]
        :
            []
});

// set default icon path
L.Icon.Default.imagePath = location.href.substring(0, location.href.lastIndexOf('/') + 1) + 'Content/libs/images/';

// map context menu helper functions
function showCoordinates(e) {
    alert(e.latlng + " (zoom:" + map._zoom + ")");
}

// create basic layer control and add to map
var layerControl = L.control.layers(baseLayers, {}).addTo(map);

// format of mapView (lat,lon,zoom)
map.mapView = function () {
    var center = map.getCenter();
    return {
        lat: center.lat,
        lon: center.lng,
        zoom: map.getZoom(),
        extent: map.getBounds()
    };
};

// send map view (center and zoom level to backend
map._sendMapViewView = function () {
    var center = map.getCenter();
    wsSend({
        type: 'mapView',
        payload: { view: map.mapView() }
    });
    map._lastMapViewSend = Date.now();
};

// map move handler to send current map view (center location and zoom level) to  publisher
map.on('move', function (e) {
    // only handle if from gui (then e contains field orginalEvent)
    if (typeof e.originalEvent !== 'undefined') {
        if (typeof map._lastMapViewSend === 'undefined' || Date.now() - map._lastMapViewSend >= 100)
            map._sendMapViewView();
    }
});

// map zoom handler to send current map view (center location and zoom level) to  publisher
map.on('zoomend', function (e) {
    map._sendMapViewView();
});

// on end of map move always send current map view (center location and zoom level) to  publisher
map.on('moveend', function (e) {
    if (typeof e.originalEvent !== 'undefined') {
        map._sendMapViewView();
    }
});

// send map view (center and zoom level to backend
map.sendMapView = function () {
    var center = map.getCenter();
    wsSend({
        type: 'mapView',
        payload: {
            lead: map.mapViewModel.lead,
            follow: map.mapViewModel.follow,
            view: map.mapView()
        }
    });
    map._lastMapViewSend = Date.now();
};


map.mapViewModel = new MapViewModel();
map.mapViewModel.Initialize();
map.mapViewModel.on('view', function (e) { map.setView([e.view.lat, e.view.lon], e.view.zoom); }, map);

