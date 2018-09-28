/**
 * Continuous graph view controller.
 */

/* globals L, GraphViewController, ContinuousGraphView  */

/* exported CategoryGraphViewController */
var ContinuousGraphViewController = GraphViewController.extend({

    createGraphView: function () {
        this.graphViewModel.on('series categories axes', this._onDataChanged, this);
        return new ContinuousGraphView(L.extend({
            graphViewModel: this.graphViewModel,
            graphLegendModel: this.graphLegendViewModel,
        }, this._graphViewOptions));
    },

    _onDataChanged: function () {
        this.graphViewModel.fire('modelReset');
    }

});