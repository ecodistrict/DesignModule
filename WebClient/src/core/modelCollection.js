/**
 *  ModelCollection. This is a storage of models. This storage emits events whenever it is changed.
 */

/* globals L */

function toArray(item) {
    if (!item) return [];
    return Array.isArray(item) ? item.slice() : [item];
}

function difference(lModels, rModels) {
    lModels = toArray(lModels);
    rModels = toArray(rModels);

    return lModels.filter(function (model) {
        return rModels.indexOf(model) < 0;
    });
}

function intersect(lModels, rModels) {
    lModels = toArray(lModels);
    rModels = toArray(rModels);

    return lModels.filter(function (model) {
        return rModels.indexOf(model) >= 0;
    });
}

var ModelCollection = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var models = options.models || [];
        Object.defineProperty(this, 'models', {
            get: function () { return models.slice(); }
        });

        this.set = function (newModels) {
            var removedModels = difference(models, newModels);
            var addedModels = difference(newModels, models);
            models = toArray(newModels);
            
            this.fire('remove', { models: removedModels });
            this.fire('add', { models: addedModels });            
            this.fire('change', { models: models });
        };

        this.add = function (newModels) {
            if (!newModels) return;

            var modelsToAdd = toArray(newModels);
            modelsToAdd = modelsToAdd.filter(function (model) { return !this.contains(model); }.bind(this));
            models = models.concat(modelsToAdd);
            this.fire('add', { models: modelsToAdd });
            this.fire('change', { models: models });
        };

        this.remove = function (oldModels) {
            if (!oldModels) return;

            var removedModels = intersect(models, oldModels);
            var modelsToRemove = toArray(oldModels);
            models = models.filter(function (model) { return modelsToRemove.indexOf(model) < 0; });
            this.fire('remove', { models: removedModels });
            this.fire('change', { models: models });
        };

        Object.defineProperty(this, 'length', {
            get: function () { return models.length; }
        });
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
