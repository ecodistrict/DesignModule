/**
 * Dock button model.
 */

/* globals L */ 

var DockButtonViewModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var docked = options.docked || false;
        Object.defineProperty(this, 'docked', {
            get: function () { return docked; },
            set: function (newDocked) {
                docked = newDocked;
                this.fire('docked', { docked: docked });
            }
        });
    }

}); 

export default DockButtonViewModel;
