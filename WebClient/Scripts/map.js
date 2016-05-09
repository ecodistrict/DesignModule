// util to parse url parameters ie get the lat/lon/zoom/session
function getParameterByName(name, def) {
    name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
    var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
        results = regex.exec(location.search);
    return results === null ? def : decodeURIComponent(results[1].replace(/\+/g, " "));
}

// base layers via mapbox
mapboxUrl = 'https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiY29ybmVsaXNzZW5qYSIsImEiOiJjaWo5end1azYwMDN6dWxrbnI3eDd4ZzF0In0.tkDIhjkvxgARGteWbAeqPg';
mapboxAttribution = '© <a href="https://www.mapbox.com/about/maps/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>';

// define base layers
var baseMapLayerGrayScale = L.tileLayer(mapboxUrl, { id: 'mapbox.light', attribution: mapboxAttribution }),
    baseMapLayerStreets = L.tileLayer(mapboxUrl, { id: 'mapbox.streets', attribution: mapboxAttribution }),
    baseMapLayerStreetsSatelite = L.tileLayer(mapboxUrl, { id: 'mapbox.streets-satellite', attribution: mapboxAttribution });

var baseLayers = {
    "Street": baseMapLayerStreets,
    "Grayscale": baseMapLayerGrayScale,
    "Satellite": baseMapLayerStreetsSatelite
};

var overlayLayers = {
};


// create the map
var map = L.map('map', {
    center: [getParameterByName('lat', 52.08606), getParameterByName('lon', 5.17689)],
    zoom: getParameterByName('zoom', 11),
    layers: [baseMapLayerGrayScale]
});

var layerControl = L.control.layers(baseLayers, overlayLayers).addTo(map);
