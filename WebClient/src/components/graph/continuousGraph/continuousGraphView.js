/**
 * ContinuousGraphView is a window to display a continuous graph with points(x,y) in both the axis.
 */

/* globals d3, L, GraphView, D3Utils  */

/* exported ContinuousGraphView */
var ContinuousGraphView = GraphView.extend({

    initialize: function (opts) {
        this.onInitializeGraph();
        GraphView.prototype.initialize.call(this, opts);
    },

    onInitializeGraph: function () {
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

        this._buildSvgViewport();
        this._buildSceneGeometry();
        this._buildScales();
        this._buildXBottomAxis();
        this._buildXTopAxis();
        this._buildYLeftAxis();
        this._buildYRightAxis();
        this._buildGraphContentContainer();

        this.bisectDate = d3.bisector(function (d) {
            return d; //d.x;
        }).left;
        this._redraw();
    },

    onRemoveGraph: function () {

    },

    onAddGraph: function () {
        this._redraw();
    },

    onResize: function () {
        this._redraw();
    },

    _buildSvgViewport: function () {
        this._svg = d3.select(this.viewportElement()).append('svg')
            .attr('class', 'continuous-graph');
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
            60
        );

        this._sceneGeometry = {
            left: fixValue(this._padding.left),
            top: fixValue(this._padding.top) + 25,
            width: sceneWidth,
            height: sceneHeight
        };
    },

    _buildScales: function () {
        var width = this._sceneGeometry.width;
        var height = this._sceneGeometry.height;
        var axes = this.graphViewModel.axes;

        this._xBottomScale = null;
        if (axes.xBottom) {
            this._xBottomScale = d3.scaleLinear()
                .range([0, width])
                .domain(axes.xBottom.scale);
        }

        this._xTopScale = null;
        if (axes.xTop) {
            this._xTopScale = d3.scaleLinear()
                .range([0, width])
                .domain(axes.xTop.scale);
        }

        this._yLeftScale = null;
        if (axes.yLeft) {
            this._yLeftScale = d3.scaleLinear()
                .range([height, 0])
                .domain(axes.yLeft.scale);
        }

        this._yRightScale = null;
        if (axes.yRight) {
            this._yRightScale = d3.scaleLinear()
                .range([height, 0])
                .domain(axes.yRight.scale);
        }
    },

    _buildXBottomAxis: function () {
        this._svg.append('g')
            .attr('class', 'axis axis--x axis--x-bottom');

        this._svg.select('g.axis--x-bottom').append('text')
            .attr('class', 'title');

        this._updateXBottomAxis();
    },

    _buildXTopAxis: function () {
        this._svg.append('g')
            .attr('class', 'axis axis--x axis--x-top');

        this._svg.select('g.axis--x-top').append('text')
            .attr('class', 'title');

        this._updateXTopAxis();
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

    _redraw: function (performAnimation) {
        this._buildSceneGeometry();
        this._buildScales();
        this._buildToolTipOverlay();
        this._updateXBottomAxis(performAnimation);
        this._updateXTopAxis(performAnimation);
        this._updateYLeftAxis(performAnimation);
        this._updateYRightAxis(performAnimation);
        this._updateLines(performAnimation);
        this._updatePoints(performAnimation);
        this._buildToolTipElementList();
    },

    _buildToolTipElementList: function () {
        this._toolTipElementListBottomX = new Map();
        this._toolTipElementListTopX = new Map();
        var lineData = this._buildTypeCollection('line');

        var xBottomLineCount = 0;
        var xTopLineCount = 0;

        var ele;
        for (ele in lineData) {

            if (lineData[ele].data[0].xAxisId === 'xBottom')
                xBottomLineCount++;

            else if (lineData[ele].data[0].xAxisId === 'xTop')
                xTopLineCount++;


            var pointEle;
            for (pointEle in lineData[ele].data) {
                var elemKey = lineData[ele].data[pointEle].x;
                var elemVal = {
                    'x': lineData[ele].data[pointEle].x,
                    'y': lineData[ele].data[pointEle].y,
                    'xAxisId': lineData[ele].data[pointEle].xAxisId,
                    'yAxisId': lineData[ele].data[pointEle].yAxisId,
                    'color': lineData[ele].data[pointEle].color
                };
                if (lineData[ele].data[pointEle].xAxisId === 'xBottom') {
                    var toolTipElemBottomX = this._toolTipElementListBottomX.get(elemKey);
                    if (toolTipElemBottomX === undefined) {
                        this._toolTipElementListBottomX.set(elemKey, [elemVal]);
                    } else {
                        toolTipElemBottomX.push(elemVal);
                    }
                } else if (lineData[ele].data[pointEle].xAxisId === 'xTop') {
                    var toolTipElemTopX = this._toolTipElementListTopX.get(elemKey);
                    if (toolTipElemTopX === undefined) {
                        this._toolTipElementListTopX.set(elemKey, [elemVal]);
                    } else {
                        toolTipElemTopX.push(elemVal);
                    }
                }
            }
        }
    },

    _buildToolTipOverlay: function () {
        var x = this._sceneGeometry.left;
        var y = this._sceneGeometry.top;

        if (d3.select('.toolTip-overlay-container').empty()) {
            this._svg.append('rect')
                .attr('class', 'toolTip-overlay-container')
                .attr('width', this._sceneGeometry.width)
                .attr('height', this._sceneGeometry.height)
                .attr('transform', 'translate(' + x + ', ' + y + ')')
                .on('mouseover', function () {
                    d3.selectAll('.toolTip').style('display', null);
                })
                .on('mouseout', function () {
                    d3.selectAll('.toolTip').style('display', 'none');
                })
                .on('mousemove', this._mousemove.bind(this));
        } else {
            d3.select('.toolTip-overlay-container')
                .attr('width', this._sceneGeometry.width)
                .attr('height', this._sceneGeometry.height);
        }
    },

    _mousemove: function () {
        d3.selectAll('.toolTip').remove();

        var mousePos = d3.mouse(d3.event.currentTarget)[0];

        //xBottom
        this._buildToolTip(mousePos, this._toolTipElementListBottomX, this._xBottomScale);

        //xTop
        if (this._xTopScale != null)
            this._buildToolTip(mousePos, this._toolTipElementListTopX, this._xTopScale);
    },

    _buildToolTip: function (mousePos, toolTipElementList, xScale) {
        var valueFormat = function (axisId, value) {
            var format = this._axisValueFormat(this.graphViewModel.axes[axisId]);
            return format(value);
        }.bind(this);

        var mouseXPosBottomX = xScale.invert(mousePos);
        var mapKeysBottomX = Array.from(toolTipElementList.keys());
        var sortedKeysBottomX = mapKeysBottomX.sort(function (a, b) {
            return a - b;
        });

        var focusKeyPositionBottomX = this.bisectDate(sortedKeysBottomX, mouseXPosBottomX, 1);
        var focusKeyBottomX = sortedKeysBottomX[focusKeyPositionBottomX];

        var prevKeyBottomX = sortedKeysBottomX[focusKeyPositionBottomX - 1];
        var nextKeyBottomX = focusKeyBottomX;
        var selectedKey = mouseXPosBottomX - prevKeyBottomX > nextKeyBottomX - mouseXPosBottomX ?
            nextKeyBottomX : prevKeyBottomX;

        var selectedElementBottomX = toolTipElementList.get(selectedKey);
        for (var ele in selectedElementBottomX) {
            var yScale;

            if (selectedElementBottomX[ele].yAxisId === 'yLeft')
                yScale = this._yLeftScale;
            else if (selectedElementBottomX[ele].yAxisId === 'yRight')
                yScale = this._yRightScale;

            this._svg.append('g')
                .attr('class', 'toolTip toolTip-element' + ele)
                .append('circle')
                .attr('r', 7);

            d3.select('.toolTip-element' + ele)
                .append('rect')
                .attr('x', 10)
                .attr('y', -20)
                .attr('width', 60)
                .attr('height', 35);

            d3.select('.toolTip-element' + ele)
                .append('text')
                .attr('x', 0)
                .attr('y', 0);

            var toolTipEle = d3.select('.toolTip-element' + ele);

            toolTipEle.attr('transform', 'translate(' + (this._sceneGeometry.left +
                        xScale(selectedElementBottomX[ele].x)) + ',' +
                    (this._sceneGeometry.top + yScale(selectedElementBottomX[ele].y)) + ')')
                .style('stroke', selectedElementBottomX[ele].color);

            toolTipEle.select('text')
                .append('svg:tspan')
                .attr('x', 15)
                .attr('y', -5)
                .text(function () {
                    return 'x: ' + valueFormat(selectedElementBottomX[ele].xAxisId, selectedElementBottomX[ele].x);
                })
                .append('svg:tspan')
                .attr('x', 15)
                .attr('dy', 15)
                .text(function () {
                    return 'y: ' + valueFormat(selectedElementBottomX[ele].yAxisId, selectedElementBottomX[ele].y);
                });
        }
    },

    _updateXBottomAxis: function () {
        var axes = this.graphViewModel.axes;

        if (!axes.xBottom) {
            return this._svg.select('g.axis--x-bottom').selectAll('*').remove();
        }

        var x = this._sceneGeometry.left;
        var y = this._sceneGeometry.top + this._sceneGeometry.height;

        var ticksCount;
        var tickValues;
        if (axes.xBottom.ticks) {
            if (axes.xBottom.ticks.constructor === Array)
                tickValues = axes.xBottom.ticks;
            else
                ticksCount = axes.xBottom.ticks;
        }

        var axis = d3.axisBottom(this._xBottomScale)
            .tickFormat(this._axisValueFormat(axes.xBottom))
            .ticks(ticksCount)
            .tickValues(tickValues)
            .scale(this._xBottomScale);

        this._svg.select('g.axis--x-bottom')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(axis);

        this._svg.select('g.axis--x-bottom .title')
            .attr('x', this._sceneGeometry.width / 2)
            .attr('y', '2.4em')
            .text(axes.xBottom.title);

    },

    _updateXTopAxis: function () {
        var axes = this.graphViewModel.axes;

        if (!axes.xTop) {
            return this._svg.select('g.axis--x-top').selectAll('*').remove();
        }

        var x = this._sceneGeometry.left;
        var y = this._sceneGeometry.top;

        var ticksCount;
        var tickValues;
        if (axes.xTop.ticks) {
            if (axes.xTop.ticks.constructor === Array)
                tickValues = axes.xTop.ticks;
            else
                ticksCount = axes.xTop.ticks;
        }

        var axis = d3.axisTop(this._xTopScale)
            .tickFormat(this._axisValueFormat(axes.xTop))
            .ticks(ticksCount)
            .tickValues(tickValues)
            .scale(this._xTopScale);

        this._svg.select('g.axis--x-top')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(axis);

        this._svg.select('g.axis--x-top .title')
            .attr('x', this._sceneGeometry.width / 2)
            .attr('y', '-1.7em')
            .text(axes.xTop.title);

    },

    _updateYLeftAxis: function () {
        var axes = this.graphViewModel.axes;

        if (!axes.yLeft) {
            return this._svg.select('g.axis--y-left').selectAll('*').remove();
        }

        var x = this._sceneGeometry.left;
        var y = this._sceneGeometry.top;

        var ticksCount;
        var tickValues;
        if (axes.yLeft.ticks) {
            if (axes.yLeft.ticks.constructor === Array)
                tickValues = axes.yLeft.ticks;
            else
                ticksCount = axes.yLeft.ticks;
        }

        var axis = d3.axisLeft(this._yLeftScale)
            .tickFormat(this._axisValueFormat(axes.yLeft))
            .ticks(ticksCount)
            .tickValues(tickValues)
            .scale(this._yLeftScale);

        this._svg.select('g.axis--y-left')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(axis);

        this._svg.select('g.axis--y-left .title')
            .attr('x', -this._sceneGeometry.height / 2)
            .attr('y', '-3em')
            .text(axes.yLeft.title);
    },

    _updateYRightAxis: function () {
        var axes = this.graphViewModel.axes;

        if (!axes.yRight) {
            return this._svg.select('g.axis--y-right').selectAll('*').remove();
        }

        var x = this._sceneGeometry.left + this._sceneGeometry.width;
        var y = this._sceneGeometry.top;

        var ticksCount;
        var tickValues;
        if (axes.yRight.ticks) {
            if (axes.yRight.ticks.constructor === Array)
                tickValues = axes.yRight.ticks;
            else
                ticksCount = axes.yRight.ticks;
        }

        var axis = d3.axisRight(this._yRightScale)
            .tickFormat(this._axisValueFormat(axes.yRight))
            .ticks(ticksCount)
            .tickValues(tickValues)
            .scale(this._yRightScale);

        this._svg.select('g.axis--y-right')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(axis);

        this._svg.select('g.axis--y-right .title')
            .attr('x', -this._sceneGeometry.height / 2)
            .attr('y', '4em')
            .text(axes.yRight.title);
    },

    _axisValueFormat: function (axis) {
        var format = d3.format(axis.formatSpecifier || '');
        var units = axis.units || '';

        return function (tickData) {
            return format(tickData) + units;
        };
    },

    _updateLines: function (performAnimation) {
        var animationDuration = performAnimation ? this._animationDuration : 0;

        var lines = this._buildTypeCollection('line');

        var drawContinuousGraphLines = continuousGraphLines(lines)
            .xTopScale(this._xTopScale)
            .xBottomScale(this._xBottomScale)
            .yLeftScale(this._yLeftScale)
            .yRightScale(this._yRightScale)
            //.tip(this._lineTip)
            .duration(animationDuration);

        var g = this._svg.select('g.graph-content-container');
        g.call(drawContinuousGraphLines);
    },



    _updatePoints: function (performAnimation) {
        var animationDuration = performAnimation ? this._animationDuration : 0;

        var points = this._buildTypeCollection('point');

        var drawContinuousGraphPoints = continuousGraphPoints(points)
            .xTopScale(this._xTopScale)
            .xBottomScale(this._xBottomScale)
            .yLeftScale(this._yLeftScale)
            .yRightScale(this._yRightScale)
            //.tip(this._lineTip)
            .duration(animationDuration);

        var g = this._svg.select('g.graph-content-container');
        g.call(drawContinuousGraphPoints);
    },

    _buildTypeCollection: function (elementType) {
        function createLinePoint(series, point) {
            return {
                seriesId: series.id,
                seriesTitle: series.title,
                color: series.color,
                xAxisId: point.xAxisId,
                yAxisId: point.yAxisId,
                x: point.x,
                y: point.y
            };
        }

        function createLineData(series) {
            return series.data.map(function (point) {
                return createLinePoint(series, point);
            });
        }

        var lines = this.graphViewModel.series
            .filter(function (series) {
                return series.type === elementType;
            })
            .map(function (series) {
                return {
                    seriesId: series.id,
                    seriesTitle: series.title,
                    color: series.color,
                    data: createLineData(series)
                };
            });

        return lines;
    }

});