/**
 * Category graph view controller.
 */

/* globals L */

import GraphViewController from '../graphViewController';
import CategoryGraphView from './categoryGraphView';
import { convertGraphModelToCategoryGraphViewModel as toCategoryGraphViewModel } from './categoryGraphViewModelConverters';

var CategoryGraphViewController = GraphViewController.extend({

    onInitialize: function () {
        this.categoryGraphViewModel = toCategoryGraphViewModel(this.graphViewModel);

        this.graphViewModel.on('series categories axes', this._onDataChanged, this);
    },

    onRemove: function () {
        this.graphViewModel.off('series categories axes', this._onDataChanged, this);
    },

    createGraphView: function () {
        var graphView = new CategoryGraphView(L.extend({
            graphViewModel: this.graphViewModel,
            categoryGraphViewModel: this.categoryGraphViewModel,
            graphLegendModel: this.graphLegendViewModel,
        }, this._graphViewOptions));
        graphView.on('graphCategoryClicked', this._onGraphCategoryClicked, this);

        return graphView;
    },

    _onDataChanged: function () {
        var updatedModel = toCategoryGraphViewModel(this.graphViewModel);
        this.categoryGraphViewModel.set({
            axes: updatedModel.axes,
            lines: updatedModel.lines,
            bars: updatedModel.bars
        });
    },

    _onGraphCategoryClicked: function (data) {
        this._notifyGraphCategoryClicked(data.categoryId);        
    },

    _notifyGraphCategoryClicked: function (categoryId) {
        this.fire('graphCategoryClicked', {
            graphModel: this.graphModel,
            categoryId: categoryId
        });
    }
    
});

export default CategoryGraphViewController;
