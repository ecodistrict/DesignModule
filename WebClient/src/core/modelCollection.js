/**
 *  ModelCollection. This is a storage of models. This storage emmits events whenever it is changed.
 */

/* globals L */ 

var ModelCollection = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var models = options.models || [];
        Object.defineProperty(this, 'models', {
            get: function () { return models.slice(); }
        });

        this.set = function (newModels) {
            models = newModels;
            this.fire('change', { models: models });
        };

        this.add = function (newModels) {
            var modelsToAdd = Array.isArray(newModels) ? newModels : [newModels];
            modelsToAdd.filter(function (model) { return !this.contains(model); }.bind(this));
            models = models.concat(modelsToAdd);
            this.fire('change', { models: models });
        };

        this.remove = function (oldModels) {
            var modelsToRemove = Array.isArray(oldModels) ? oldModels : [oldModels];
            models = models.filter(function (model) { return modelsToRemove.indexOf(model) < 0; });
            this.fire('change', { models: models });
        };
    },

    contains: function (model) {
        return (this.models.indexOf(model) >= 0);
    },

    getById: function (modelId) {
        return this.models.find(function (model) {
            return model.id === modelId;
        });
    },

    empty: function () {
        return this.models.length === 0;
    }

});

export default ModelCollection;
