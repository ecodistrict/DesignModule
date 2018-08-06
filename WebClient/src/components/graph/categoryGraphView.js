/**
 * CategoryGraphView is awindow to display a category graph.
 * Category graph is a graph where x axis is a categories axis.
 */

var CategoryGraphView = GraphView.extend({
    
    initialize: function (opts) {        
        GraphView.prototype.initialize.call(this, opts);
    },

    onRenderGraph: function (viewport) {
        var svg = this._getSvg(viewport);
        
    },

    _getSvg: function (viewport) {
        var grpahSvg = d3.select(viewport).selectAll('svg.category-graph')
            .data([this.graphViewModel]);
        grpahSvg.enter().append('svg')
            .attr('class', 'category-graph');
        return d3.select(viewport).select('svg.category-graph');
    }

}); 
