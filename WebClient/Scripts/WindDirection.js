L.Control.Arrow = L.Control.extend({
    arrow: null,
    text: null,
    windHeight: 200, //only changes div size, not the wind arrow
    windWidth: 200, //only changes div size, not the wind arrow
    moveHeight: 15,
    moveWidth: 15,
    maxSpeed: 40, //in m/s
    rotating: false,
    dragging: false,
    live: true,
    active: false,


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
        this.active = true;
        this._initLayout();
        this._map = map;
        this._update();
        return this._container;
    },

    onRemove: function () {
        this.active = false;
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
        windDirectionDiv.style.width = this.windWidth + 'px';
        windDirectionDiv.style.height = this.windHeight + 'px';
        windDirectionDiv.addEventListener('click', this._windDirectionDivClick);
        if (is_touch_device()) {
            windDirectionDiv.addEventListener('touchstart', this._windDirectionDivMouseDown);
        } else {
            windDirectionDiv.addEventListener('mousedown', this._windDirectionDivMouseDown);
        }
        windDirectionDiv.addEventListener('contextmenu', this._windRightClick);
        this._windDiv = windDirectionDiv;

        //var moveDiv = L.DomUtil.create('div', 'windMoveDiv');
        //windDirectionDiv.appendChild(moveDiv);
        //moveDiv.style.width = this.moveWidth + 'px';
        //moveDiv.style.height = this.moveHeight + 'px';

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
        L.DomUtil.addClass(arrow, 'windArrowLive');
        arrow.src = "Content/images/arrow_wind.png";
    },

    _windRightClick: function (e) {
        e.preventDefault(); //prevent showing of contextmenu since we use right-mouse for something else
        e.stopPropagation();
    },

    GoLive: function() {
        DataManager.wind.live = true;
        if (DataManager.wind.time && DataManager.wind.direction && DataManager.wind.speed)
            DataManager.wind.rotate(DataManager.wind.direction, DataManager.wind.speed, DataManager.wind.time);
        L.DomUtil.addClass(DataManager.wind.arrow, 'windArrowLive');
        wsSend({
            type: 'windData',
            payload: {
                live: true
            }
        });
    },

    _windDirectionDivMouseDown: function (e) {
        if (DataManager.wind.dragging || DataManager.wind.rotating)
            return;
        e.preventDefault();
        e.stopPropagation();

        if ((e.which && e.which == 3) || (e.button && (e.button == 2))) //check if right mouse button!
        {
            DataManager.wind.GoLive();
            return;
        }

        L.DomUtil.removeClass(DataManager.wind.arrow, 'windArrowLive');

        var offset = DataManager.wind.GetOffset(e);


        if ((DataManager.wind._windDiv.offsetWidth - offset.X) <= DataManager.wind.moveWidth && (DataManager.wind._windDiv.offsetHeight - offset.Y) <= DataManager.wind.moveHeight) {
            DataManager.wind.dragging = true;

            if (is_touch_device()) {
                window.addEventListener('touchmove', DataManager.wind._dragMove);
                window.addEventListener('touchend', DataManager.wind._dragEnd);
            } else {
                window.addEventListener("mousemove", DataManager.wind._dragMove);
                window.addEventListener("mouseup", DataManager.wind._dragEnd);
            }
        }
        else
        {
            DataManager.wind.rotating = true;
            DataManager.wind.live = false;

            var data = DataManager.wind.GetDirectionSpeed(e);
            var today = new Date();
            var date = today.getFullYear() + '-' + today.getMonth() + 1 + '-' + today.getDate() + ' 12:12:12';

            DataManager.wind.rotate(data.direction, data.speed, date);

            if (is_touch_device()) {
                window.addEventListener('touchmove', DataManager.wind._directionMove);
                window.addEventListener('touchend', DataManager.wind._directionEnd);
            } else {
                window.addEventListener("mousemove", DataManager.wind._directionMove);
                window.addEventListener("mouseup", DataManager.wind._directionEnd);
            }
        }
    },

    _dragMove: function(e)
    {

    },

    _dragEnd: function(e)
    {
        if (is_touch_device()) {
            window.removeEventListener('touchmove', DataManager.wind._dragMove);
            window.removeEventListener('touchend', DataManager.wind._dragEnd);
        } else {
            window.removeEventListener("mousemove", DataManager.wind._dragMove);
            window.removeEventListener("mouseup", DataManager.wind._dragEnd);
        }

        DataManager.wind.dragging = false;
    },

    _directionMove: function (e) {
        if (!(DataManager.wind.dragging || DataManager.wind.rotating))
            return

        e = e ? e : window.event;
        e.preventDefault();
        e.stopPropagation();
        var windDiv = DataManager.wind._windDiv;

        var data = DataManager.wind.GetDirectionSpeed(e);
        var today = new Date();
        var date = today.getFullYear() + '-' + today.getMonth() + 1 + '-' + today.getDate() + ' 12:12:12';
        DataManager.wind.rotate(data.direction, data.speed, date);
    },

    _directionEnd: function (e) {
        DataManager.wind.rotating = false;

        e = e ? e : window.event;
        e.preventDefault();
        e.stopPropagation();
        if (is_touch_device()) {
            window.removeEventListener('touchmove', DataManager.wind._directionMove);
            window.removeEventListener('touchend', DataManager.wind._directionEnd);
        } else {
            window.removeEventListener("mousemove", DataManager.wind._directionMove);
            window.removeEventListener("mouseup", DataManager.wind._directionEnd);
        }
        var data = DataManager.wind.GetDirectionSpeed(e);
        var today = new Date();
        var date = today.getFullYear() + '-' + today.getMonth() + 1 + '-' + today.getDate() + ' 12:12:12';
        DataManager.wind.rotate(data.direction, data.speed, date);

        wsSend({
                type: 'windData',
                payload: {
                    direction: data.direction,
                    speed: data.speed
                }
            });



        //todo: send wsSend to server!
    },

    GetOffset: function(e)
    {
        e = e ? e : window.event;
        var currentDiv = DataManager.wind._windDiv;
        var totOffset = {
            top: 0,
            left: 0
        };
        while (currentDiv != null) {
            totOffset.top += currentDiv.offsetTop;
            totOffset.left += currentDiv.offsetLeft;
            currentDiv = currentDiv.offsetParent;
        }

        return {
            X: e.clientX - totOffset.left,
            Y: e.clientY - totOffset.top
        }
    },

    GetDirectionSpeed: function (e)
    {
        var offset = this.GetOffset(e);

        var center = {
            X: (DataManager.wind._windDiv.offsetWidth / 2),
            Y: (DataManager.wind._windDiv.offsetHeight / 2)
        }

        var centerOffset = {
            X: offset.X - center.X,
            Y: offset.Y - center.Y
        }

        var length = Math.sqrt(Math.pow(centerOffset.X, 2) + Math.pow(centerOffset.Y, 2));
        
        var direction = (Math.atan((-1 * centerOffset.X) / centerOffset.Y) * (360 / (2 * Math.PI)) + 360) % 360;
        if (centerOffset.Y >= 0)
            direction = (direction + 180) % 360;

        return {
            direction: direction,
            speed: (Math.pow(Math.min(length, center.X), 2.5) / Math.pow(center.X, 2.5)) * this.maxSpeed
        }
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
            this.time = data.time;

        if (!DataManager.wind.live && data.live)
            DataManager.wind.GoLive();

        if (DataManager.wind.live && changed)
            this.rotate(this.direction, this.speed, data.time);
        wind = this;
    }),

    rotate: function (degrees, speed) {
        degrees = Math.round(degrees * 100) / 100;
        speed = Math.round(speed * 100) / 100;
        if (degrees >= 270 || degrees <= 90) {
            document.getElementsByClassName('windDirectionText')[0].style.top = '45px';
        } else {
            document.getElementsByClassName('windDirectionText')[0].style.top = '125px';
        }

        var arrow = document.getElementById("windArrow");
        arrow.style.transform = "rotate(" + (degrees + 90) + "deg)";
        arrow.style.webkitTransform = "rotate(" + (degrees + 90) + "deg)";

        this.text.innerHTML = degrees + '&deg;' + "<BR>" + speed + ' m/s';
    },

    loopedRotate: function () {
        var deg = Math.random() * (270 - 1) + 1;

        var speed = Math.random() * 25;
        deg = Math.round(deg);
        var delay = Math.round(Math.random() * 10000);
        var today = new Date();
        var date = today.getFullYear() + '-' + today.getMonth() + 1 + '-' + today.getDate() + ' 12:12:12';

        DataManager.wind.NewData({ direction: deg, speed: speed, time: date });
        setTimeout(DataManager.wind.loopedRotate, delay);
    }
})

L.control.arrow = function () {
    return new L.Control.Arrow();
}
