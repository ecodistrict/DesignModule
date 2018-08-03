/**
 * Graph model. This model is a graph domain model and represents
 * all information available about a graph in the system. The main idea 
 * is to share this model within all parties that require up to date 
 * information about a graph.
 * It can be used as a model for view but it is not the main 
 * purpose of the model.
 */

var GraphModel = L.Evented.extend({

    initialize: function (opts) {
        opts = opts || {};

        var id = opts.id;
        Object.defineProperty(this, "id", {
            get: function () { return id; },
        });

        var type = opts.type;
        Object.defineProperty(this, "type", {
            get: function () { return type; },
        });

        var title = opts.title;
        Object.defineProperty(this, "title", {
            get: function () { return title; },
            set: function (newTitle) {
                title = newTitle;
                this.fire('title', { title: title });
            }
        });

        var series = opts.series;
        Object.defineProperty(this, "series", {
            get: function () { return series; },
            set: function (newSeries) {
                series = newSeries;
                this.fire('series', { series: series });
            }
        });
    }

});
