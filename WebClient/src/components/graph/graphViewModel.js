/**
 * Graph view model. This model is used only to present 
 * data to view for visualization and not meant to be a domain model.
 * This model will be used by GraphView.
 */

var GraphViewModel = L.Evented.extend({

    initialize: function (opts) {
        opts = opts || {};

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
