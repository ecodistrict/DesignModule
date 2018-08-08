/**
 * Graph model. This model is a graph domain model and represents
 * all information available about a graph in the system. The main idea 
 * is to share this model within all parties that require up to date 
 * information about a graph.
 * It can be used as a model for view but it is not the main 
 * purpose of the model.
 */

/* globals L */ 

/* exported GraphModel */
var GraphModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var id = options.id;
        Object.defineProperty(this, 'id', {
            get: function () { return id; },
        });

        var type = options.type;
        Object.defineProperty(this, 'type', {
            get: function () { return type; },
        });

        var title = options.title || '';
        Object.defineProperty(this, 'title', {
            get: function () { return title; },
            set: function (newTitle) {
                title = newTitle;
                this.fire('title', { title: title });
            }
        });

        var axes = options.axes || [];
        Object.defineProperty(this, 'axes', {
            get: function () { return axes; },
            set: function (newAxes) {
                axes = newAxes;
                this.fire('axes', { axes: axes });
            }
        });

        var categories = options.categories || [];
        Object.defineProperty(this, 'categories', {
            get: function () { return categories; },
            set: function (newCategories) {
                categories = newCategories;
                this.fire('categories', { categories: categories });
            }
        });

        var series = options.series || [];
        Object.defineProperty(this, 'series', {
            get: function () { return series; },
            set: function (newSeries) {
                series = newSeries;
                this.fire('series', { series: series });
            }
        });
    }

});
