/**
 * Category graph view model. This model is used only to present 
 * data to category graph view for visualization and not meant to be a domain model.
 * This model will be used by CategoryGraphView.
 */

/* globals L */ 

/* exported CategoryGraphViewModel */
var CategoryGraphViewModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var axes = options.axes || [];
        Object.defineProperty(this, 'axes', {
            get: function () { return axes; },
            set: function (newAxes) {
                axes = newAxes;
                this.fire('axes', { axes: axes });
            }
        });

        var lines = options.lines || [];
        Object.defineProperty(this, 'lines', {
            get: function () { return lines; },
            set: function (newLines) {
                lines = newLines;
                this.fire('lines', { lines: lines });
            }
        });

        var bars = options.bars || { categories: [] };
        Object.defineProperty(this, 'bars', {
            get: function () { return bars; },
            set: function (newBars) {
                bars = newBars;
                this.fire('bars', { bars: bars });
            }
        });
    },

    set: function (opts) {
        this.axes = opts.axes || [];
        this.categories = opts.categories || [];
        this.lines = opts.lines || [];
        this.bars = opts.bars || { categories: [] };
        
        this.fire('modelReset');
    }

}); 
