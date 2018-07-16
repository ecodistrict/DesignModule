var ScaleSliderModel = L.Class.extend({

    initialize: function (opts) {
        opts = opts || {};

        var value = opts.value;
        Object.defineProperty(this, "value", {
            get: function () { return value; },
            set: function (newValue) {
                value = newValue;
                this.fire('value', { value: value });
            }
        });

        var brush = opts.brush;
        Object.defineProperty(this, "brush", {
            get: function () { return brush; },
            set: function (newBrush) {
                brush = newBrush;
                this.fire('brush', { brush: brush });
            }
        });

        var events = opts.events || [];
        Object.defineProperty(this, "events", {
            get: function () { return events.slice(); }
        });

        this.setEvents = function (newEvents) {
            events = newEvents;
            this.fire('events', { events: events });
        };

        this.addEvents = function (newEvents) {
            events = events.concat(newEvents);
            this.fire('events', { events: events });
        };
    }

});
L.extend(ScaleSliderModel.prototype, L.Evented.prototype);