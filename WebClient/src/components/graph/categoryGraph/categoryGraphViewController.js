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
        return new CategoryGraphView(L.extend({
            graphViewModel: this.graphViewModel,
            categoryGraphViewModel: this.categoryGraphViewModel,
            graphLegendModel: this.graphLegendViewModel,
        }, this._graphViewOptions));
    },

    _onDataChanged: function () {
        var updatedModel = toCategoryGraphViewModel(this.graphViewModel);
        this.categoryGraphViewModel.set({
            axes: updatedModel.axes,
            lines: updatedModel.lines,
            bars: updatedModel.bars
        });
    }
    
});

export default CategoryGraphViewController;
