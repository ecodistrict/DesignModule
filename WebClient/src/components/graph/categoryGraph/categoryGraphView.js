/**
 * CategoryGraphView is awindow to display a category graph.
 * Category graph is a graph where x axis is a categories axis.
 */

/* globals d3 */

import './categoryGraph.css';

import GraphView from '../graphView';
import DomUtil from '../../../utils/DomUtil';
import categoryGraphLines from './categoryGraphLines';
import categoryGraphLinesPoints from './categoryGraphLinesPoints';
import categoryGraphBars from './categoryGraphBars';

var CategoryGraphView = GraphView.extend({
    
    onInitializeGraph: function (opts) {
        if (!opts.categoryGraphViewModel) throw new Error('categoryGraphViewModel is not provided');
        this.categoryGraphViewModel = opts.categoryGraphViewModel;

        this._padding = {
            left: 60,
            right: 60,
            top: 10,
            bottom: 10
        };
        this._animationDuration = 300;
    },

    onRenderGraph: function () {
        if (this._svg) return;

        this.categoryGraphViewModel.on('modelReset', this._onDataChanged, this);
        
        this._buildSvgViewport();
        this._buildTooltip();
        this._buildXAxisTickGeometry();
        this._buildSceneGeometry();
        this._buildScales();
        this._buildXAxis();
        this._buildYLeftAxis();
        this._buildYRightAxis();
        this._buildGraphContentContainer();

        this._redraw();
    },

    onRemoveGraph: function () {
        if (!this._svg) return;

        this.categoryGraphViewModel.off('modelReset', this._onDataChanged, this);

        this._tip.destroy();
        this._lineTip.destroy();

        this._svg.remove();
        this._svg = null;
    },

    onAddGraph: function () {
        var performAnimation = true;
        this._redraw(performAnimation);
    },

    onResize: function () {        
        this._redraw();
    },

    _onDataChanged: function () {
        var performAnimation = true;
        this._redraw(performAnimation);
    },

    _buildSvgViewport: function () {        
        this._svg = d3.select(this.viewportElement()).append('svg')
            .attr('class', 'category-graph');
    },

    _buildXAxisTickGeometry: function () {
        function fixValue(value) {
            if (value < 0) return 0;
            return value;
        }

        var svgWidth = this._svg.node().getBoundingClientRect().width;
        var sceneWidth = fixValue(svgWidth - this._padding.left - this._padding.right);
        var tickBottomMargin = 5;
        var categories = this.categoryGraphViewModel.axes.xBottom.domain;
        
        var categoryWidth = sceneWidth * 0.8 / (categories.length || 1);
        var horizontalTextSize = this._calculateTickTextSize('2018-08-10');
        var horizontalWidth = horizontalTextSize.width;

        var rotated = horizontalWidth > categoryWidth;
        var textWidth = categoryWidth;
        var areaHeight = horizontalTextSize.height + tickBottomMargin;
        
        if (rotated) {
            var maxCategoryTitleWidth = categories.reduce(
                function (maxWidth, category) {
                    return Math.max(maxWidth, this._calculateTickTextSize(category.title).width);
                }.bind(this), 0
            );
            var baseTickTextWidth = this._calculateTickTextSize('2018-08-10 16:02').width;
            
            textWidth = Math.min(maxCategoryTitleWidth, baseTickTextWidth) + 2;
            areaHeight = textWidth / Math.sqrt(2) + tickBottomMargin;
        }
        
        this._xAxisTickGeometry = {
            rotated: rotated,
            textWidth: textWidth,
            areaHeight: areaHeight
        };
    },

    _buildSceneGeometry: function () {
        var svgWidth = this._svg.node().getBoundingClientRect().width;
        var svgHeight = this._svg.node().getBoundingClientRect().height;

        function fixValue(value) {
            if (value < 0) return 0;
            return value;
        }

        var sceneWidth = fixValue(svgWidth - this._padding.left - this._padding.right);
        var sceneHeight = fixValue(
            svgHeight - 
            this._padding.top - 
            this._padding.bottom - 
            this._xAxisTickGeometry.areaHeight -
            30
        );

        this._sceneGeometry = {
            left: fixValue(this._padding.left),
            top: fixValue(this._padding.top),
            width: sceneWidth,
            height: sceneHeight
        };
    },

    _buildScales: function () {
        var width = this._sceneGeometry.width;
        var height = this._sceneGeometry.height;
        var axes = this.categoryGraphViewModel.axes;

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

    _buildXAxis: function () {
        this._xAxis = d3.axisBottom(this._xScale);
        
        this._svg.append('g')
            .attr('class', 'axis axis--x')
            .call(this._xAxis);

        this._svg.select('g.axis--x').append('text')
            .attr('class', 'title');

        this._updateXAxis();
    },

    _buildYLeftAxis: function () {
        this._svg.append('g')
            .attr('class', 'axis axis--y axis--y-left');

        this._svg.select('g.axis--y-left').append('text')
            .attr('class', 'title');

        this._updateYLeftAxis();
    },

    _buildYRightAxis: function () {        
        this._svg.append('g')
            .attr('class', 'axis axis--y axis--y-right');

        this._svg.select('g.axis--y-right').append('text')
            .attr('class', 'title');

        this._updateYRightAxis();
    },

    _buildGraphContentContainer: function () {
        var x = this._sceneGeometry.left;
        var y = this._sceneGeometry.top;

        this._svg.append('g')
            .attr('class', 'graph-content-container')
            .attr('transform', 'translate(' + x + ', ' + y + ')');
    },

    _buildTooltip: function () {

        var valueFormat = function (axisId, value) {
            var format = this._axisValueFormat(this.categoryGraphViewModel.axes[axisId]);
            return format(value);
        }.bind(this);
        

        function tooltipHtml(d) {
            if (!d) return '';
            if (d.tooltip) return d.tooltip;

            var tooltip = '';
            if (d.seriesTitle) {
                tooltip += '<div class="title">' + d.seriesTitle + '</div>';
            }
            if (d.y !== undefined) {
                tooltip += '<div class="value">' + valueFormat(d.axisId, d.y) + '</div>';
            }            
            return tooltip;
        }

        // general tooltip
        this._tip = d3.tip()
            .attr('class', 'd3-tip category-graph-tip')
            .style('z-index', 999)
            .offset(function () {
                return [-5, 0];
            })
            .html(tooltipHtml);

        // line tooltip
        this._lineTip = d3.tip()
            .attr('class', 'd3-tip category-graph-tip')
            .style('z-index', 999)
            .offset(function () {
                var r = this.getBoundingClientRect();
                var m = (r.left + r.right) / 2;
                return [d3.event.clientY - r.top - 4, d3.event.clientX - m];
            })
            .html(tooltipHtml);

        this._svg.call(this._tip);
        this._svg.call(this._lineTip);
    },

    _redraw: function (performAnimation) {
        this._buildXAxisTickGeometry();
        this._buildSceneGeometry();
        this._buildScales();
        this._updateXAxis(performAnimation);
        this._updateYLeftAxis(performAnimation);
        this._updateYRightAxis(performAnimation);
        this._updateBars(performAnimation);
        this._updateLines(performAnimation);
        this._updateLinePoints(performAnimation);
    },

    _updateXAxis: function () {
        var x = this._sceneGeometry.left;
        var y = this._sceneGeometry.top + this._sceneGeometry.height;
        var maxTextWidth = this._xAxisTickGeometry.textWidth;
        var axes = this.categoryGraphViewModel.axes;

        this._svg.select('g.axis--x')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(this._xAxis.scale(this._xScale));

        this._svg.select('g.axis--x').selectAll('.tick text')
            .classed('rotate', this._xAxisTickGeometry.rotated)
            .attr('transform', this._xAxisTickGeometry.rotated ? 
                'rotate(45)' : '')
            .on('mouseover.tooltip touchstart.tooltip', this._tip.show)
            .on('mouseout.tooltip touchend.tooltip', this._tip.hide)
            .on('click touchend', function (d) {
                this._notifyGraphCategoryClicked(d.id);
            }.bind(this))
            .text(function (d) {
                return DomUtil.truncateTextByWidth(this, d.title, maxTextWidth);
            });

        var labelX = this._sceneGeometry.width / 2;
        var labelY = this._xAxisTickGeometry.areaHeight + 24;
        this._svg.select('g.axis--x .title')
            .attr('transform', 'translate(' + labelX + ', ' + labelY + ')' )
            .text(axes.xBottom.title);
    },

    _updateYLeftAxis: function () {
        var axes = this.categoryGraphViewModel.axes;

        if (!axes.yLeft) {
            return this._svg.select('g.axis--y-left').selectAll('*').remove();
        }

        var x = this._sceneGeometry.left;
        var y = this._sceneGeometry.top;

        var axis = d3.axisLeft(this._yLeftScale)
            .tickFormat(this._axisValueFormat(axes.yLeft))
            .ticks(axes.yLeft.ticksCount)
            .tickValues(axes.yLeft.tickValues)
            .scale(this._yLeftScale);

        this._svg.select('g.axis--y-left')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(axis);

        this._svg.select('g.axis--y-left .title')
            .attr('x', - this._sceneGeometry.height / 2)
            .attr('y', '-3em')
            .text(axes.yLeft.title);
    },

    _updateYRightAxis: function () {
        var axes = this.categoryGraphViewModel.axes;

        if (!axes.yRight) {
            return this._svg.select('g.axis--y-right').selectAll('*').remove();
        }

        var x = this._sceneGeometry.left + this._sceneGeometry.width;
        var y = this._sceneGeometry.top;

        var axis = d3.axisRight(this._yRightScale)
            .tickFormat(this._axisValueFormat(axes.yRight))
            .ticks(axes.yRight.ticksCount)
            .tickValues(axes.yRight.tickValues)
            .scale(this._yRightScale);

        this._svg.select('g.axis--y-right')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(axis);

        this._svg.select('g.axis--y-right .title')
            .attr('x', - this._sceneGeometry.height / 2)
            .attr('y', '4em')
            .text(axes.yRight.title);
    },

    _updateBars: function (performAnimation) {
        var animationDuration = performAnimation ? this._animationDuration : 0;

        var bars = this.categoryGraphViewModel.bars;

        var drawCategoryGraphBars = categoryGraphBars(bars)
            .xBottomScale(this._xScale)
            .yLeftScale(this._yLeftScale)
            .yRightScale(this._yRightScale)
            .tip(this._tip)
            .duration(animationDuration);

        var g = this._svg.select('g.graph-content-container');
        g.call(drawCategoryGraphBars);
    },

    _updateLines: function(performAnimation) {
        var animationDuration = performAnimation ? this._animationDuration : 0;

        var lines = this.categoryGraphViewModel.lines;

        var drawCategoryGraphLines = categoryGraphLines(lines)
            .xBottomScale(this._xScale)
            .yLeftScale(this._yLeftScale)
            .yRightScale(this._yRightScale)
            .tip(this._lineTip)
            .duration(animationDuration);

        var g = this._svg.select('g.graph-content-container');
        g.call(drawCategoryGraphLines);
    },

    _updateLinePoints: function (performAnimation) {
        var animationDuration = performAnimation ? this._animationDuration : 0;

        var lines = this.categoryGraphViewModel.lines;        

        var drawCategoryGraphLinesPoints = categoryGraphLinesPoints(lines)
            .xBottomScale(this._xScale)
            .yLeftScale(this._yLeftScale)
            .yRightScale(this._yRightScale)
            .tip(this._tip)
            .duration(animationDuration);

        var g = this._svg.select('g.graph-content-container');
        g.call(drawCategoryGraphLinesPoints);
    },

    _axisValueFormat: function (axis) {
        var format = d3.format(axis.formatSpecifier || '');
        var units = axis.units || '';

        return function (tickData) {
            return format(tickData) + units;
        };
    },

    _calculateTickTextSize: function (text) {
        var xAxisTmp = this._svg.append('g')
            .attr('class', 'axis axis--x  hidden');
        var xAxisTickTextTmp = xAxisTmp.append('g')
            .attr('class', 'tick')
            .append('text');
        var textSize = DomUtil.calculateTextSize(
            xAxisTickTextTmp.node(), 
            text
        );

        xAxisTmp.remove();

        return textSize;
    },

    _notifyGraphCategoryClicked: function (categoryId) {
        this.fire('graphCategoryClicked', {
            view: this,
            categoryId: categoryId
        });
    }
    
});

export default CategoryGraphView;
