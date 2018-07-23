// environment control to show and set wind direction and speed
// based on leaflet controls (no D3 used)
// todo: add termperature

function getDistance(draggable) {
    // calculate location and angle relative to center of control based on mouse position during drag (leaflet class)
    var rect = draggable._dragStartTarget.getBoundingClientRect(),
        centerX = (rect.left + rect.right) / 2,
        centerY = (rect.top + rect.bottom) / 2,
        offsetX = event ? event.clientX - centerX : draggable._lastEvent.clientX - centerX,
        offsetY = event ? event.clientY - centerY : draggable._lastEvent.clientY - centerY;

    draggable.dx = offsetX * 2 / rect.width;
    draggable.dy = offsetY * 2 / rect.height;
    // distance to center
    draggable.d = Math.sqrt(draggable.dx * draggable.dx + draggable.dy * draggable.dy);
    // angle in degrees to center [0..360]
    draggable.r = (Math.atan2(draggable.dy, draggable.dx) * (180 / Math.PI) + 270) % 360;
    // speed, [0..1] relative to distance to center (1=0.7 of ellipse filling control) logarithmic scale (Niek)
    draggable.s = Math.pow(Math.min(draggable.d / 0.7, 1.0), 2.5) * draggable.ref.options.maxSpeed;
    // return the relative distance to center of control
    return draggable.d;
}

// define control type
L.Control.Arrow = L.Control.extend({
    arrow: null,
    text: null,
    active: false,
    currentData: { speed: 0, direction: 0 },

    arrowImageRotationCompensation: 180,
    arrowImagePath: "Content/images/arrow_wind.png",
    arrowImageManualPath: "Content/images/arrow_wind_manual.png",

    options: {
        collapsed: false,
        position: 'topleft',
        autoZIndex: true,
        height: 150,
        width: 150,
        maxSpeed: 40 //in m/s
    },

    /// leaflet constructor
    initialize: function (options) {
        L.setOptions(this, options);
        this.parentContainer = options.element;
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
        // main div
        this._container = this.parentContainer || L.DomUtil.create('div', 'leaflet-control-windDirection');
        var parentContainer = this.parentContainer;

        // add drag support to main div
        this._draggable = new L.Draggable(this._container);
        this._draggable.ref = this;
        this._draggable._originalupdatePosition = this._draggable._updatePosition;
        this._draggable._updatePosition = function (e) {
            // action is set in _dragdown
            // check if we are ragging main div or are setting wind direction or speed
            if (this.action) {
                if (this.action > 0) {
                    var d = getDistance(this);
                    switch (this.action) {
                        // setting speed
                        case 1:
                            this.ref.currentData.speed = this.s;
                            this.ref.rotate(this.ref.currentData.direction, this.ref.currentData.speed);
                            break;
                        // setting wind direction
                        case 2:
                            this.ref.currentData.direction = this.r;
                            this.ref.rotate(this.ref.currentData.direction, this.ref.currentData.speed);
                            break;
                    }
                    // work-a-round: ensure dragend is fired!
                    this._moved = true;
                    this._moving = true;
                }
                else {
                    // ..

                }
            }
            else {
                if (!parentContainer)
                    // we are dragging the main div (control as a whole)
                    this._originalupdatePosition(e);
            }
        };
        this._draggable.enable();

        this._draggable.addEventListener('down', this._dragdown);
        this._draggable._onUp = (function (e) {
            if (e._simulated || !this._enabled) { return; }
            this.finishDrag();
            this.ref._dragup();
        }).bind(this._draggable); // work-a-round, no up event is fired

        L.DomEvent.disableClickPropagation(this._container);
        L.DomEvent.disableScrollPropagation(this._container);
        // use context menu event to return to "live" state
        this._container.addEventListener('contextmenu', this._handleContextMenu.bind(this));

        this.text = this._container.appendChild(L.DomUtil.create('span', "windDirectionText"));
        this.text.innerHTML = "<BR>unknown<BR>";
        this._windDirectionDiv = L.DomUtil.create('div', "windDirectionDiv");
        this._windDirectionDiv.appendChild(this.text);
        this._container.appendChild(this._windDirectionDiv);
        this._windDirectionDiv.style.width = this.options.width + 'px';
        this._windDirectionDiv.style.height = this.options.height + 'px';
        this._windDirectionDiv.appendChild(L.DomUtil.create('div', "north"))
            .innerHTML = 'N';
        this._windDirectionDiv.appendChild(L.DomUtil.create('div', "south"))
            .innerHTML = 'S';
        this._windDirectionDiv.appendChild(L.DomUtil.create('div', "east"))
            .innerHTML = 'E';
        this._windDirectionDiv.appendChild(L.DomUtil.create('div', "west"))
            .innerHTML = 'W';
        this.arrow = this._windDirectionDiv.appendChild(L.DomUtil.create('img', "arrowImage"));
        this.arrow.src = "Content/images/arrow_wind.png";
        this.arrow.id = "windArrow";
        L.DomUtil.addClass(this.arrow, 'windArrowLive');
        // show initial direction and speed
        this.rotate(this.currentData.direction, this.currentData.speed);
    },

    _handleContextMenu: function (e) {
        e.preventDefault();
        e.cancelBubble = true;

        // return to live state
        this.arrow.src = "Content/images/arrow_wind.png";
        wsSend({
            type: 'windData',
            payload: { live: true }
        });
    },

    _dragdown: function (e) {
        // check if we are going to drag the main div or set the wind direction or set the wind speed
        // set action accordingly to process action in 
        if (event && event.button != 2) {
            var d = getDistance(this);
            if (this.d < 1.0) {
                var angleDiff = (this.r - this.ref.currentData.direction + 180 + 360) % 360 - 180;
                if (this.d < 0.7 && -10 <= angleDiff && angleDiff <= 10) {
                    // setting wind speed
                    this.action = 1;
                    // move to selected point
                    this.ref.currentData.speed = this.s;
                    this.ref.rotate(this.ref.currentData.direction, this.ref.currentData.speed);
                }
                else {
                    // rotating
                    this.action = 2;
                    // move to selected point
                    this.ref.currentData.direction = this.r;
                    this.ref.rotate(this.ref.currentData.direction, this.ref.currentData.speed);
                }
                // not-live
                this.ref.arrow.src = "Content/images/arrow_wind_manual.png";
                wsSend({
                    type: 'windData',
                    payload: { live: false }
                });
            }
            else {
                // nothing, just dragging
                this.action = 0;
            }
        }
        else
            this.action = -1;
    },

    _dragup: function (e) {
        // end the drag action according to the previously set action
        switch (this._draggable.action) {
            case 1:
                // final speed
                this.currentData.speed = this._draggable.s;
                this.rotate(this.currentData.direction, this.currentData.speed);
                // send new speed
                wsSend({
                    type: 'windData',
                    payload: { speed: this.currentData.speed }
                });
                break;
            case 2:
                // final rotate
                this.currentData.direction = this._draggable.r;
                this.rotate(this.currentData.direction, this.currentData.speed);
                // send new direction
                wsSend({
                    type: 'windData',
                    payload: { direction: this.currentData.direction }
                });
                break;
        }
    },

    _update: function () {
        // todo: needed?
    },

    NewData: (function (data) {
        // set new state via websocket
        // show new live state
        this.arrow.src = data.live ? "Content/images/arrow_wind.png" : "Content/images/arrow_wind_manual.png";
        // speed and/or direction
        var changed = false;
        if (typeof data.speed !== 'undefined' && data.speed != this.currentData.speed) {
            this.currentData.speed = data.speed;
            changed = true;
        }
        if (typeof data.direction !== 'undefined' && data.direction != this.currentData.direction) {
            this.currentData.direction = data.direction;
            changed = true;
        }
        // show new state of speed and direction if any changed
        if (changed)
            this.rotate(this.currentData.direction, this.currentData.speed);
    }),

    rotate: function (degrees, speed) {
        // show wind direction and speed
        degrees = Math.round(degrees * 100) / 100;
        speed = Math.round(speed * 100) / 100;
        // according to position of wind vane position text with speed and direction over or below wind vane 
        if (degrees >= 270 || degrees <= 90) {
            this.text.style.top = '20%';
            this.text.style.removeProperty('bottom');
        } else {
            this.text.style.bottom = '20%';
            this.text.style.removeProperty('top');
        }
        // update visual
        this.arrow.style.transform = "rotate(" + (degrees + 90) + "deg)";
        this.arrow.style.transform.webkitTransform = "rotate(" + (degrees + 90) + "deg)";
        this.text.innerHTML = degrees + '&deg;' + "<BR>" + speed + ' m/s';
    }
});

// add arrow constructor for arrow control
L.control.arrow = function (parentElement) {
    return new L.Control.Arrow({ element: parentElement });
};
