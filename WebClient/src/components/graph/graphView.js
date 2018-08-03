/**
 * GraphView is awindow to display a graph. This is a base class for all
 * the graphs. It provides the legend functionality.
 */

var GraphView = WindowView.extend({
    
    initialize: function (opts) {
        if (!opts.graphViewModel) throw new Error('graphViewModel is not provided');
        if (!opts.graphLegendModel) throw new Error('graphLegendModel is not provided');

        this.graphViewModel = opts.graphViewModel;
        this.graphLegendModel = opts.graphLegendModel;

        WindowView.prototype.initialize.call(
            this, 
            L.extend({ class: 'graph-view' }, opts)
        );

        this.setTitle(this.graphViewModel.title);
    },

    onRender: function (viewport) {
        var legendContainer = L.DomUtil.create('div', 'graph-legend hidden', this.element());
        this.legendView = new GraphLegendView({
            element: legendContainer,
            model: this.graphLegendModel
        });
        this.legendView.on('entryClicked', this._notifyEntryClicked, this);
    },

    onRemove: function () {
        this.legendView.off('entryClicked', this._notifyEntryClicked, this);
        this.legendView.remove();
    },

    _notifyEntryClicked: function (eventData) {
        this.fire('entryClicked', eventData);
    }

}); 
