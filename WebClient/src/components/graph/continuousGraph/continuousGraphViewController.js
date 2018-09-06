/**
 * Continuous graph view controller.
 */

/* globals L, GraphViewController, ContinuousGraphView  */

/* exported CategoryGraphViewController */
var ContinuousGraphViewController = GraphViewController.extend({

    createGraphView: function () {
        return new ContinuousGraphView(L.extend({
            graphViewModel: this.graphViewModel,
            graphLegendModel: this.graphLegendViewModel,
        }, this._graphViewOptions));
    }

});