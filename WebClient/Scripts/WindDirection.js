L.Control.Arrow = L.Control.extend({
    speed: "unknown",
    direction: "unknown",
    time: "unknown",
    arrow: null,
    text: null,

    options: {
        collapsed: false,
        position: 'topleft',
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

        var text = container.appendChild(L.DomUtil.create('span', "windDirectionText"));
        this.text = text;

        text.innerHTML = "<BR>unknown<BR>";

        var windDirectionDiv = L.DomUtil.create('div', "windDirectionDiv");
        windDirectionDiv.appendChild(text);
        container.appendChild(windDirectionDiv);

        var north = windDirectionDiv.appendChild(L.DomUtil.create('div', "north"));
        north.innerHTML = 'N';
        var south = windDirectionDiv.appendChild(L.DomUtil.create('div', "south"));
        south.innerHTML = 'S';
        var east = windDirectionDiv.appendChild(L.DomUtil.create('div', "east"));
        east.innerHTML = 'E';
        var west = windDirectionDiv.appendChild(L.DomUtil.create('div', "west"));
        west.innerHTML = 'W';
        var arrow = windDirectionDiv.appendChild(L.DomUtil.create('img', "arrowImage"));

        var points = windDirectionDiv.appendChild(L.DomUtil.create('img', "selectedArrow"));
        points.src = "Content/images/wind_points.png";

        this.arrow = arrow;
        arrow.id = "windArrow";
        arrow.src = "Content/images/arrow_wind.png";

    },

    _update: function () {

    },

    NewData: (function (data) {
        let changed = false;
        if (typeof data.speed !== 'undefined' && this.speed != data.speed) {
            this.speed = Math.max(data.speed, 0);
            changed = true;
        }
        if (typeof data.direction !== 'undefined' && this.direction != data.direction) {
            this.direction = data.direction;
            changed = true;
        }

        if (typeof data.time !== 'undefined')
            time = data.time;

        if (changed)
            this.rotate(this.direction, this.speed, data.time);
        wind = this;

    }),

    rotate: function (degrees, speed, time) {

        if (degrees < 91 && degrees > -91 || degrees > 360 && degrees < 91) {
            document.getElementsByClassName('windDirectionText')[0].style.top = '45px';
        } else {
            document.getElementsByClassName('windDirectionText')[0].style.top = '125px';
        }

        var arrow = document.getElementById("windArrow");
        arrow.style.transform = "rotate(" + (degrees + 90) + "deg)";
        arrow.style.webkitTransform = "rotate(" + (degrees + 90) + "deg)";


        var displayTime = DataManager.GetDisplayTime(time);
        this.text.innerHTML = displayTime + "<BR>" + degrees + '&deg;' + "<BR>" + Math.round(speed).toFixed(0) + 'm/s';


    },

    loopedRotate: function () {
        var deg = Math.random() * (270 - 1) + 1;

        var speed = Math.random() * 25;
        deg = Math.round(deg);
        var delay = Math.round(Math.random() * 10000);
        var date = today.getFullYear() + '-' + today.getMonth() + '-' + today.getDay() + ' 12:12:12';

        wind.rotate(deg, speed, date);
        setTimeout(wind.loopedRotate, delay);
    }
})

L.control.arrow = function () {
    return new L.Control.Arrow();
}
