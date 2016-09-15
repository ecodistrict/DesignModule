L.Control.Arrow = L.Control.extend({
    speed: "unknown",
    direction: "unknown",
    options: {
        collapsed: false,
        position: 'bottomright',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (options) {
        L.setOptions(this, options);
    },

    onAdd: function (map) {
        this._initLayout();
        this._map = map;
        this._update();
        return this._container;
    },

    onRemove: function () {
    },

    _initLayout: function () {

        var className = 'leaflet-control-windDirection',
            container = this._container = L.DomUtil.create('div', className);

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }

        //var text = container.appendChild(L.DomUtil.create('span', "windDirectionText"));

        //text.innerHTML = "Wind Direction: <BR>";

        var arrow = container.appendChild(L.DomUtil.create('img', "arrowImage"));
        arrow.id = "windArrow";
        arrow.src = "Content/images/arrow.png";

    },

    _update: function () {

    },

    _expand: function () {

    },

    _collapse: function () {

    },

    NewData: (function (data) {
        let changed = false;
        if (typeof data.speed !== 'undefined' && this.speed != data.speed) {
            this.speed = data.speed;
            changed = true;
        }
        if (typeof data.direction !== 'undefined' && this.direction != data.direction) {
            this.direction = data.direction;
            changed = true;
        }

        if (changed)
            this.rotate(this.direction, this.speed);
    }),

    rotate: function (degrees, speed) {

        var scale = Math.min(1, 0.25 + (speed / 20));

        var arrow = document.getElementById("windArrow");
        arrow.style.transform = "rotate(" + (degrees - 90) + "deg) scale(" + scale + ", " + scale + ")";
        arrow.style.webkitTransform = "rotate(" + (degrees - 90) + "deg) scale(" + scale + ", " + scale + ")";
    },

    loopedRotate: function () {
        var deg = Math.round(Math.random() * 360);

        var speed = Math.random() * 25;

        var delay = Math.round(Math.random() * 10000);

        wind.rotate(deg, speed);

        setTimeout(wind.loopedRotate, delay);
    }
})

L.control.arrow = function () {
    return new L.Control.Arrow();
}

