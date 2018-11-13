/**
 * ModuleControlWindowViewModel. This model is used only to present data 
 * to ModuleControlWindowView for visualization and not meant to be a domain model.
 */

/* globals L */

import ModelCollection from '../../../core/modelCollection';

var ModuleControlWindowViewModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var readyModules = options.readyModules || new ModelCollection();
        Object.defineProperty(this, 'readyModules', {
            get: function () { return readyModules; }
        });

        var busyModules = options.busyModules || new ModelCollection();
        Object.defineProperty(this, 'busyModules', {
            get: function () { return busyModules; }
        });
    }

});

export default ModuleControlWindowViewModel;
