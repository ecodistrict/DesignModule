/**
 * Category graph view controller.
 */

var CategoryGraphViewController = GraphViewController.extend({

    createGraphView: function () {
        return new CategoryGraphView(L.extend({
            graphViewModel: this.graphViewModel,
            graphLegendModel: this.graphLegendViewModel,            
        }, this._viewOptions));
    }
    
});
