/**
 * Graphs model. This is a storage of graph models.
 */

var GraphsModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var grpahModels = options.grpahModels || [];
        Object.defineProperty(this, "grpahModels", {
            get: function () { return grpahModels.slice(); }
        });

        this.setGraphModels = function (newGrpahModels) {
            grpahModels = newGrpahModels;
            this.fire('change', { grpahModels: grpahModels });
        };

        this.addGraphModels = function (newGrpahModels) {
            grpahModels = grpahModels.concat(newGrpahModels);
            this.fire('change', { grpahModels: grpahModels });
        };
    },

    contains: function (graphModel) {
        return (this.grpahModels.indexOf(graphModel) >= 0);
    },

    getById: function (graphModelId) {
        return this.grpahModels.find(function (graphModel) {
            return graphModel.id === graphModelId;
        });
    },

    add: function (graphModel) {
        var models = Array.isArray(graphModel)
            ? graphModel
            : [graphModel];
        this.addGraphModels(models);
    }

});
