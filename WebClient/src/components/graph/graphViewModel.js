/**
 * Graph view model. This model is used only to present 
 * data to view for visualization and not meant to be a domain model.
 * This model will be used by GraphView.
 */

var GraphViewModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var title = options.title;
        Object.defineProperty(this, "title", {
            get: function () { return title; },
            set: function (newTitle) {
                title = newTitle;
                this.fire('title', { title: title });
            }
        });

        var axes = options.axes || [];
        Object.defineProperty(this, "axes", {
            get: function () { return axes; },
            set: function (newAxes) {
                axes = newAxes;
                this.fire('axes', { axes: axes });
            }
        });

        var categories = options.categories || [];
        Object.defineProperty(this, "categories", {
            get: function () { return categories; },
            set: function (newCategories) {
                categories = newCategories;
                this.fire('categories', { categories: categories });
            }
        });

        var series = options.series;
        Object.defineProperty(this, "series", {
            get: function () { return series; },
            set: function (newSeries) {
                series = newSeries;
                this.fire('series', { series: series });
            }
        });
    }

}); 
