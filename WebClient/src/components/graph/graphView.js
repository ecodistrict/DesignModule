/**
 * GraphView is awindow to display a graph. This is a base class for all
 * the graphs. It provides the legend functionality.
 */

/* globals L, WindowView, GraphLegendView */ 

/* exported GraphView */
var GraphView = WindowView.extend({
    
    initialize: function (opts) {
        if (!opts.graphViewModel) throw new Error('graphViewModel is not provided');
        if (!opts.graphLegendModel) throw new Error('graphLegendModel is not provided');

        this.graphViewModel = opts.graphViewModel;
        this.graphLegendModel = opts.graphLegendModel;

        this.onInitializeGraph(opts);

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

        this.onRenderGraph(viewport);
    },

    onRemove: function () {
        this.onRemoveGraph();

        this.legendView.off('entryClicked', this._notifyEntryClicked, this);
        this.legendView.remove();
    },

    onAdd: function (windowManager) {
        WindowView.prototype.onAdd.call(this, windowManager);
        this.onAddGraph(windowManager);
    },

    onInitializeGraph: function (/* jshint unused:false */ opts) {
        // override in child graph classes
    },

    onRenderGraph: function (/* jshint unused:false */ viewport) {
        // override in child graph classes
    },

    onRemoveGraph: function () {
        // override in child graph classes
    },

    onAddGraph: function (/* jshint unused:false */ windowManager) {
        // override in child graph classes
    },

    _notifyEntryClicked: function (eventData) {
        this.fire('entryClicked', eventData);
    }

}); 
