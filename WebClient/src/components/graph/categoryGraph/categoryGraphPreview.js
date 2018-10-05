/**
 * CategoryGraphPreview display a preview of a given category graph.
 */

/* globals L, d3 */

import './categoryGraphPreview.css';
import View from '../../../core/view';
import { convertGraphModelToCategoryGraphViewModel as toCategoryGraphViewModel } from './categoryGraphViewModelConverters';
import categoryGraphLines from './categoryGraphLines';
import categoryGraphBars from './categoryGraphBars';

var CategoryGraphPreview = View.extend({

    onInitialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided to the constructor');
        if (!opts.graphModel) throw new Error('graphModel is not provided');

        this._graphModel = opts.graphModel;
        this._categoryGraphViewModel = toCategoryGraphViewModel(this._graphModel);
        this._graphModel.on('series categories axes', this._onGraphModelChanged, this);
    },

    onRender: function () {
        this._rootElement = L.DomUtil.create('div', 'category-graph-preview', this._parent);        

        this._buildSvgViewport();
        this._buildGraphContentContainer();

        this._redraw();
        this._categoryGraphViewModel.on('modelReset', this._redraw, this);

        return this._rootElement;
    },

    onRemove: function () {
        this._graphModel.off('series categories axes', this._onGraphModelChanged, this);
    },

    show: function () {
        View.prototype.show.call(this);
        this._redraw();
    },

    _onGraphModelChanged: function () {
        var updatedModel = toCategoryGraphViewModel(this._graphModel);
        this._categoryGraphViewModel.set({
            axes: updatedModel.axes,
            lines: updatedModel.lines,
            bars: updatedModel.bars
        });
    },

    _buildSvgViewport: function () {
        this._svg = d3.select(this._rootElement).append('svg')
            .attr('class', 'category-graph');
    },

    _buildSceneGeometry: function () {
        var width = this._svg.node().getBoundingClientRect().width;
        var height = this._svg.node().getBoundingClientRect().height;

        this._sceneGeometry = {
            left: 0,
            top: 0,
            width: width,
            height: height
        };
    },

    _buildScales: function () {
        var width = this._sceneGeometry.width;
        var height = this._sceneGeometry.height;
        var axes = this._categoryGraphViewModel.axes;

        this._xScale = d3.scaleBand().rangeRound([0, width]).padding([0.2]);
        this._xScale.domain(axes.xBottom.domain);

        this._yLeftScale = null;
        if (axes.yLeft) {
            this._yLeftScale = d3.scaleLinear()
                .range([height, 0])
                .domain(axes.yLeft.domain);
        }

        this._yRightScale = null;
        if (axes.yRight) {
            this._yRightScale = d3.scaleLinear()
                .range([height, 0])
                .domain(axes.yRight.domain);
        }
    },

    _buildGraphContentContainer: function () {
        this._svg.append('g')
            .attr('class', 'graph-content-container');
    },

    _redraw: function () {
        this._buildSceneGeometry();
        this._buildScales();

        this._updateGraphContentContainer();
        this._updateBars();
        this._updateLines();
    },

    _updateGraphContentContainer: function () {
        var x = this._sceneGeometry.left;
        var y = this._sceneGeometry.top;

        this._svg.select('.graph-content-container')
            .attr('transform', 'translate(' + x + ', ' + y + ')');
    },

    _updateBars: function () {
        var bars = this._categoryGraphViewModel.bars;

        var drawCategoryGraphBars = categoryGraphBars(bars)
            .xBottomScale(this._xScale)
            .yLeftScale(this._yLeftScale)
            .yRightScale(this._yRightScale);

        var g = this._svg.select('g.graph-content-container');
        g.call(drawCategoryGraphBars);
    },

    _updateLines: function() {
        var lines = this._categoryGraphViewModel.lines;

        var drawCategoryGraphLines = categoryGraphLines(lines)
            .xBottomScale(this._xScale)
            .yLeftScale(this._yLeftScale)
            .yRightScale(this._yRightScale);

        var g = this._svg.select('g.graph-content-container');
        g.call(drawCategoryGraphLines);
    },

});

export default CategoryGraphPreview;
