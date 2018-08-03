/**
 * GraphView is awindow to display a graph. This is a base class for all
 * the graphs. It provides the legend functionality.
 */

var GraphView = WindowView.extend({
    
    initialize: function (opts) {
        if (!opts.graphModel) throw new Error('graphModel is not provided');
        if (!opts.graphLegendModel) throw new Error('graphLegendModel is not provided');

        this.graphModel = opts.graphModel;
        this.graphLegendModel = opts.graphLegendModel;

        WindowView.prototype.initialize.call(
            this, 
            L.extend({ class: 'graph-view' }, opts)
        );

        this.setTitle(this.graphModel.title);
    },

    onRender: function () {
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
