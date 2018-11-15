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
        zoom: map.getZoom()
    };
};

// send map view (center and zoom level to backend
map._sendMapView = function () {
    var center = map.getCenter();
    wsSend({
        type: 'view',
        payload: map.mapView()
    });
    map._lastMapViewSend = Date.now();
};

// map move handler to send current map view (center location and zoom level) to  publisher
map.on('move', function (e) {
    if (typeof map._lastMapViewSend === 'undefined' || Date.now() - map._lastMapViewSend >= 300)
        map._sendMapView();
});

// on end of map move always send current map view (center location and zoom level) to  publisher
map.on('moveend', function (e) {
    map._sendMapView();
});
