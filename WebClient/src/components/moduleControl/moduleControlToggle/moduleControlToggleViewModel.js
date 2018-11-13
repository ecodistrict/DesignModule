/**
 * ModuleControlToggleViewModel. This model is used only to present data 
 * to ModuleControlToggleView for visualization and not meant to be a domain model.
 */

/* globals L */ 

var ModuleControlToggleViewModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var loading = options.loading || false;
        Object.defineProperty(this, 'loading', {
            get: function () { return loading; },
            set: function (newLoading) {
                loading = newLoading;
                this.fire('loading', { loading: loading });
            }
        });
    }

}); 

export default ModuleControlToggleViewModel;
