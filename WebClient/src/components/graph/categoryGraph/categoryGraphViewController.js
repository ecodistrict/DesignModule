/**
 * Category graph view controller.
 */

/* globals L, GraphViewController, CategoryGraphView  */ 

/* exported CategoryGraphViewController */
var CategoryGraphViewController = GraphViewController.extend({

    createGraphView: function () {
        return new CategoryGraphView(L.extend({
            graphViewModel: this.graphViewModel,
            graphLegendModel: this.graphLegendViewModel,            
        }, this._graphViewOptions));
    }
    
});
