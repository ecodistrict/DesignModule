/**
 * Graphs model. This is a storage of graph models.
 */

/* globals L */ 

/* exported GraphsModel */
var GraphsModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var graphModels = options.graphModels || [];
        Object.defineProperty(this, 'graphModels', {
            get: function () { return graphModels.slice(); }
        });

        this.setGraphModels = function (newGraphModels) {
            graphModels = newGraphModels;
            this.fire('change', { graphModels: graphModels });
        };

        this.addGraphModels = function (newGraphModels) {
            graphModels = graphModels.concat(newGraphModels);
            this.fire('change', { graphModels: graphModels });
        };
    },

    contains: function (graphModel) {
        return (this.graphModels.indexOf(graphModel) >= 0);
    },

    getById: function (graphModelId) {
        return this.graphModels.find(function (graphModel) {
            return graphModel.id === graphModelId;
        });
    },

    add: function (graphModel) {
        var models = Array.isArray(graphModel) ? graphModel : [graphModel];
        this.addGraphModels(models);
    }

});
