/**
 * DetailsItemViewModel. This model is used only to present 
 * data to details item view for visualization and not meant to be a domain model.
 * This model will be used by DetailsItemView.
 */

/* globals L */ 

var DetailsItemViewModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var id = options.id || 0;
        Object.defineProperty(this, 'id', {
            get: function () { return id; }
        });

        var selected = options.selected || false;
        Object.defineProperty(this, 'selected', {
            get: function () { return selected; },
            set: function (newSelected) {
                selected = newSelected;
                this.fire('selected', { selected: selected });
            }
        });

        var originalModel = options.originalModel || {};
        Object.defineProperty(this, 'originalModel', {
            get: function () { return originalModel; }
        });

        var title = originalModel.title || '';
        Object.defineProperty(this, 'title', {
            get: function () { return title; }
        });
        
        originalModel.on('title', function () {
            title = originalModel.title || '';
            this.fire('title', { title: title });
        }.bind(this));
    }

});

export default DetailsItemViewModel;
