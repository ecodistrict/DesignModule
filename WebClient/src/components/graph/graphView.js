/**
 * GraphView is awindow to display a graph. This is a base class for all
 * the graphs. It provides the legend functionality.
 */

/* globals L, WindowView, GraphLegendView */ 

/* exported GraphView */
var GraphView = WindowView.extend({
    
    initialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided to the View');
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
        this._legendContainer = L.DomUtil.create(
            'div', 
            'graph-legend right hidden', 
            this.element()
        );
        this.legendView = new GraphLegendView({
            element: this._legendContainer,
            model: this.graphLegendModel
        });
        this.legendView.on('focus', this._notifyFocus, this);
        this.legendView.on('entryClicked', this._notifyEntryClicked, this);

        this.on('moved', this._adjustLegendLocation, this);
        this.on('resized', this._adjustLegendLocation, this);

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
    },

    _moveLegendToRight: function () {
        L.DomUtil.removeClass(this._legendContainer, 'left');
        L.DomUtil.addClass(this._legendContainer, 'right');
    },

    _moveLegendToLeft: function () {
        L.DomUtil.removeClass(this._legendContainer, 'right');
        L.DomUtil.addClass(this._legendContainer, 'left');
    },

    _adjustLegendLocation: function () {
        var graphViewRect = this.element().getBoundingClientRect();
        var legendRect = this._legendContainer.getBoundingClientRect();
        
        var rightsideSpace = window.innerWidth - graphViewRect.right;
        var leftsideSpace = graphViewRect.left;
        var legendOnRight = graphViewRect.right < legendRect.left;
        var legendMargin = legendOnRight ? 
            legendRect.left - graphViewRect.right : graphViewRect.left - legendRect.right;

        if (legendOnRight) {
            if ((legendRect.right > window.innerWidth) &&
                (leftsideSpace > rightsideSpace)) {
                
                this._moveLegendToLeft();
            }
        } else {
            if ((graphViewRect.right + legendRect.width + legendMargin  <= window.innerWidth) ||
                (rightsideSpace > leftsideSpace)) {

                this._moveLegendToRight();
            }
        }
    }

}); 
