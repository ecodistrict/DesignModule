//todo: change square functionality into rectangle (and circle to ellipse)
L.Control.Arrow = L.Control.extend({
    arrow: null,
    text: null,
    windHeight: 200, //only changes div size, not the wind arrow
    windWidth: 200, //only changes div size, not the wind arrow
    moveHeight: 15,
    moveWidth: 15,
    maxSpeed: 40, //in m/s
    directionBorder: 15, //pixel width of the border where direction will be changed instead of speed
    rotating: false,
    dragging: false,
    active: false,
    speedLive: true,
    directionLive: true,
    liveData: {speed: 0, direction: 0},
    currentData: {speed: 0, direction: 0},


    options: {
        collapsed: false,
        position: 'topleft',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (options) {
        L.setOptions(this, options);

        //bind our event functions to this
        this._windDirectionDivMouseDown = this._windDirectionDivMouseDown.bind(this);
        this._directionMove = this._directionMove.bind(this);
        this._directionEnd = this._directionEnd.bind(this);
        this._speedMove = this._speedMove.bind(this);
        this._speedEnd = this._speedEnd.bind(this);
        this._dragMove = this._dragMove.bind(this);
        this._dragEnd = this._dragEnd.bind(this);
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
        this._show_live_status();
        L.DomUtil.addClass(arrow, 'windArrowLive');
        
    },

    _windRightClick: function (e) {
        e.preventDefault(); //prevent showing of contextmenu since we use right-mouse for something else
        e.stopPropagation();
    },

    _show_live_status: function () {
        if (this.speedLive && this.directionLive)
            this.arrow.src = "Content/images/arrow_wind.png";
        else
            this.arrow.src = "Content/images/arrow_wind_manual.png";
    },

    GoLive: function() {
        this.speedLive = true;
        this.directionLive = true;
        this._show_live_status();
        this.currentData.direction = typeof this.liveData.direction != "undefined" ? this.liveData.direction : 0;
        this.currentData.speed = typeof this.liveData.speed != "undefined" ? this.liveData.speed : 0;;
        this.rotate(this.currentData.direction, this.currentData.speed);
        L.DomUtil.addClass(this.arrow, 'windArrowLive');
        wsSend({
            type: 'windData',
            payload: {
                live: true
            }
        });
    },

    _windDirectionDivMouseDown: function (e) {
        if (this.dragging || this.rotating)
            return;
        e.preventDefault();
        e.stopPropagation();

        if ((e.which && e.which == 3) || (e.button && (e.button == 2))) //check if right mouse button!
        {
            this.GoLive();
            return;
        }

        e.clientX = typeof e.clientX != "undefined" ? e.clientX : e.touches[0].clientX;
        e.clientY = typeof e.clientY != "undefined" ? e.clientY : e.touches[0].clientY;

        var offset = this.GetOffset(e);


        if ((this._windDiv.offsetWidth - offset.X) <= this.moveWidth && (this._windDiv.offsetHeight - offset.Y) <= this.moveHeight) {
            this.dragging = true;

            if (is_touch_device()) {
                window.addEventListener('touchmove', this._dragMove);
                window.addEventListener('touchend', this._dragEnd);
            } else {
                window.addEventListener("mousemove", this._dragMove);
                window.addEventListener("mouseup", this._dragEnd);
            }
        }
        else
        {
            this.rotating = true;

            var length = this.GetLength(offset);

            if (length > (this.windWidth / 2) - this.directionBorder) //check if we're adjustion direction
            {
                L.DomUtil.removeClass(this.arrow, 'windArrowLive');
                this.directionLive = false;
                this._show_live_status();
                this.currentData.direction = this.GetDirection(offset);
                if (is_touch_device()) {
                    window.addEventListener('touchmove', this._directionMove);
                    window.addEventListener('touchend', this._directionEnd);
                } else {
                    window.addEventListener("mousemove", this._directionMove);
                    window.addEventListener("mouseup", this._directionEnd);
                }
            }
            else // adjusting speed
            {
                this.speedLive = false;
                this._show_live_status();
                this.currentData.speed = this.GetSpeed(offset);
                if (is_touch_device()) {
                    window.addEventListener('touchmove', this._speedMove);
                    window.addEventListener('touchend', this._speedEnd);
                } else {
                    window.addEventListener("mousemove", this._speedMove);
                    window.addEventListener("mouseup", this._speedEnd);
                }
            }

            this.rotate(this.currentData.direction, this.currentData.speed);
        }
    },

    _dragMove: function(e)
    {

    },

    _dragEnd: function(e)
    {
        if (is_touch_device()) {
            window.removeEventListener('touchmove', this._dragMove);
            window.removeEventListener('touchend', this._dragEnd);
        } else {
            window.removeEventListener("mousemove", this._dragMove);
            window.removeEventListener("mouseup", this._dragEnd);
        }

        this.dragging = false;
    },

    _directionMove: function (e) {
        if (!(this.dragging || this.rotating))
            return

        e = e ? e : window.event;
        e.preventDefault();
        e.stopPropagation();

        e.clientX = typeof e.clientX != "undefined" ? e.clientX : e.touches[0].clientX;
        e.clientY = typeof e.clientY != "undefined" ? e.clientY : e.touches[0].clientY;

        var windDiv = this._windDiv;
        //var today = new Date();
        //var date = today.getFullYear() + '-' + today.getMonth() + 1 + '-' + today.getDate() + ' 12:12:12';
        //var data = DataManager.wind.GetDirectionSpeed(e);
        this.currentData.direction = this.GetDirection(this.GetOffset(e));
        this.rotate(this.currentData.direction, this.currentData.speed);
    },

    _directionEnd: function (e) {
        this.rotating = false;

        e = e ? e : window.event;
        e.preventDefault();
        e.stopPropagation();

        e.clientX = typeof e.clientX != "undefined" ? e.clientX : e.changedTouches[0].clientX;
        e.clientY = typeof e.clientY != "undefined" ? e.clientY : e.changedTouches[0].clientY;

        if (is_touch_device()) {
            window.removeEventListener('touchmove', this._directionMove);
            window.removeEventListener('touchend', this._directionEnd);
        } else {
            window.removeEventListener("mousemove", this._directionMove);
            window.removeEventListener("mouseup", this._directionEnd);
        }

        this.currentData.direction = this.GetDirection(this.GetOffset(e));
        this.rotate(this.currentData.direction, this.currentData.speed);

        wsSend({
                type: 'windData',
                payload: {
                    direction: this.currentData.direction
                }
            });
    },

    _speedMove: function (e) {
        if (!(this.dragging || this.rotating))
            return

        e = e ? e : window.event;
        e.preventDefault();
        e.stopPropagation();

        e.clientX = typeof e.clientX != "undefined" ? e.clientX : e.touches[0].clientX;
        e.clientY = typeof e.clientY != "undefined" ? e.clientY : e.touches[0].clientY;

        //var windDiv = this._windDiv;
        //var today = new Date();
        //var date = today.getFullYear() + '-' + today.getMonth() + 1 + '-' + today.getDate() + ' 12:12:12';
        //var data = this.GetDirectionSpeed(e);
        this.currentData.speed = this.GetSpeed(this.GetOffset(e));
        this.rotate(this.currentData.direction, this.currentData.speed);
    },

    _speedEnd: function (e) {
        this.rotating = false;

        e = e ? e : window.event;
        e.preventDefault();
        e.stopPropagation();

        e.clientX = typeof e.clientX != "undefined" ? e.clientX : e.changedTouches[0].clientX;
        e.clientY = typeof e.clientY != "undefined" ? e.clientY : e.changedTouches[0].clientY;

        if (is_touch_device()) {
            window.removeEventListener('touchmove', this._speedMove);
            window.removeEventListener('touchend', this._speedEnd);
        } else {
            window.removeEventListener("mousemove", this._speedMove);
            window.removeEventListener("mouseup", this._speedEnd);
        }

        this.currentData.speed = this.GetSpeed(this.GetOffset(e));
        this.rotate(this.currentData.direction, this.currentData.speed);

        wsSend({
            type: 'windData',
            payload: {
                speed: this.currentData.speed
            }
        });

    },

    GetOffset: function(e)
    {
        e = e ? e : window.event;
        var currentDiv = this._windDiv;
        var totOffset = {
            top: 0,
            left: 0
        };
        while (currentDiv != null) {
            totOffset.top += currentDiv.offsetTop;
            totOffset.left += currentDiv.offsetLeft;
            currentDiv = currentDiv.offsetParent;
        }

        var center = {
            X: (this._windDiv.offsetWidth / 2),
            Y: (this._windDiv.offsetHeight / 2)
        }

        var position = {
            X: e.clientX - totOffset.left,
            Y: e.clientY - totOffset.top
        }

        var centerOffset = {
            X: position.X - center.X,
            Y: position.Y - center.Y
        }

        return {
            X: position.X,
            Y: position.Y,
            centerOffset: centerOffset,
            center: center
        }
    },

    GetLength: function(offset)
    {
        return Math.sqrt(Math.pow(offset.centerOffset.X, 2) + Math.pow(offset.centerOffset.Y, 2));
    },

    GetSpeed: function(offset)
    {
        var length = this.GetLength(offset);
        return (Math.pow(Math.min(length, offset.center.X), 2.5) / Math.pow(offset.center.X, 2.5)) * this.maxSpeed;
    },

    GetDirection: function(offset)
    {
        var direction = (Math.atan((-1 * offset.centerOffset.X) / offset.centerOffset.Y) * (360 / (2 * Math.PI)) + 360) % 360;
        if (offset.centerOffset.Y < 0)
            direction = (direction + 180) % 360;
        return direction;
    },

    _update: function () {

    },

    NewData: (function (data) {
        let changed = false;
        let direction = this.currentData.direction;
        let speed = this.currentData.speed;
        if (typeof data.speed !== 'undefined' && this.liveData.speed != data.speed) {
            this.liveData.speed = Math.max(data.speed, 0);
            if (this.speedLive)
            {
                speed = this.liveData.speed;
                this.currentData.speed = speed;
                changed = true;
            }
        }
        if (typeof data.direction !== 'undefined' && this.direction != data.direction) {
            this.liveData.direction = data.direction;
            if (this.directionLive) {
                direction = this.liveData.direction;
                this.currentData.direction = direction;
                changed = true;
            }
        }
        
        if (!(this.speedLive && this.directionLive) && data.live)
            this.GoLive();

        if (changed)
            this.rotate(direction, speed);
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

    loopedRotate: function () { //only for testing purposes
        var deg = Math.random() * (270 - 1) + 1;

        var speed = Math.random() * 25;
        deg = Math.round(deg);
        var delay = Math.round(Math.random() * 10000);

        this.NewData({ direction: deg, speed: speed});
        setTimeout(this.loopedRotate.bind(this), delay);
    }
})

L.control.arrow = function () {
    return new L.Control.Arrow();
}
