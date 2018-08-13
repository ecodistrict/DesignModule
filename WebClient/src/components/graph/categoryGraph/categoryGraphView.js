/**
 * CategoryGraphView is awindow to display a category graph.
 * Category graph is a graph where x axis is a categories axis.
 */

/* globals d3, L, GraphView, DomUtil  */ 

/* exported CategoryGraphView */
var CategoryGraphView = GraphView.extend({
    
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

        this.graphViewModel.on('series categories axes', this._onDataChanged, this);
        
        this._buildInternalData();
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

        this.graphViewModel.off('series categories axes', this._onDataChanged, this);

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
        this._buildInternalData();
        this._redraw(performAnimation);
    },

    _buildInternalData: function () {
        this._buildInternalAxesData();
        this._buildInternalBarsData();
        this._buildInternalLinesData();
    },

    _buildInternalAxesData: function () {
        var yDomainsDefault = this._calculateYDomains();
        this._axesData = {
            xBottom: {
                title: this.graphViewModel.axes.xBottom.title,
                domain: this.graphViewModel.categories.map(function (category) {
                    return L.extend({
                        tooltip: category.title,
                        valueOf: function () { return this.id; }
                    }, category);
                })
            }
        };

        function createAxisData(axis) {
            var ticksCount = typeof axis.ticks === 'number' ? axis.ticks : 5;
            var tickValues = Array.isArray(axis.ticks) ? axis.ticks : null;

            return {
                title: axis.title,
                domain: axis.scale || yDomainsDefault.yLeft,
                units: axis.units || '',
                formatSpecifier: axis.formatSpecifier || '',
                ticksCount: ticksCount,
                tickValues: tickValues
            };
        }

        if (this.graphViewModel.axes.yLeft) {
            this._axesData.yLeft = createAxisData(this.graphViewModel.axes.yLeft);
        }

        if (this.graphViewModel.axes.yRight) {
            this._axesData.yRight = createAxisData(this.graphViewModel.axes.yRight);
        }
    },

    _buildInternalBarsData: function () {
        var constructBarTooltipHtml = function(series, point) {
            return '<div>' + series.title + '</div><div>' +
                this._axisValueFormat(this._axesData[point.axisId])(point.y) + '</div>';
        }.bind(this);

        var createBarData = function (series, point) {
            return { 
                seriesId: series.id,
                color: series.color,
                axisId: point.axisId,
                tooltip: constructBarTooltipHtml(series, point),
                y: function () {
                    var scale = this._scaleByAxisId(point.axisId);
                    return  scale(point.y);
                }.bind(this)
            };
        }.bind(this);

        var categoriesMap = this.graphViewModel.categories.reduce(
            function (result, category) {
                result[category.id] = { data: [] };
                return result;
            },
            {}
        );

        this.graphViewModel.series.forEach(function (series) {
            if (series.type !== 'bar') return;

            series.data.forEach(function (point) {
                var bar = createBarData(series, point);
                categoriesMap[point.categoryId].data.push(bar);
            });
        });

        this._barsData = {
            categories: this.graphViewModel.categories.map(function (category) {
                return {
                    categoryId: category.id,
                    data: categoriesMap[category.id].data
                };
            })
        };
    },

    _buildInternalLinesData: function () {
        function constructLineTooltipHtml(series) {
            return '<div>' + series.title + '</div>';
        }

        var constructLinePointTooltipHtml = function(series, point) {
            return '<div>' + series.title + '</div><div>' +
                this._axisValueFormat(this._axesData[point.axisId])(point.y) + '</div>';
        }.bind(this);

        var createLinePoint = function (series, seriesPoint) {
            return {
                categoryId: seriesPoint.categoryId,
                color: series.color,
                tooltip: constructLinePointTooltipHtml(series, seriesPoint),
                y: function () {
                    var scale = this._scaleByAxisId(seriesPoint.axisId);
                    return  scale(seriesPoint.y);
                }.bind(this)
            };
        }.bind(this);

        function createLineData (series) {
            return series.data.map(function (point) {
                return createLinePoint(series, point);
            });
        }

        var lines = this.graphViewModel.series
            .filter(function (series) { 
                return series.type === 'line'; 
            })
            .map(function (series) {
                return {
                    seriesId: series.id,
                    color: series.color,
                    tooltip: constructLineTooltipHtml(series),
                    data: createLineData(series)
                };
            });
        
        this._linesData = { lines: lines };
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
        
        var categoryWidth = sceneWidth * 0.8 / (this.graphViewModel.categories.length || 1);
        var horizontalTextSize = this._calculateTickTextSize('2018-08-10');
        var horizontalWidth = horizontalTextSize.width;

        var rotated = horizontalWidth > categoryWidth;
        var textWidth = horizontalWidth;
        var areaHeight = horizontalTextSize.height + tickBottomMargin;
        
        if (rotated) {
            var maxCategoryTitleWidth = this.graphViewModel.categories.reduce(
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

        this._xScale = d3.scaleBand().rangeRound([0, width]).padding([0.2]);
        this._xScale.domain(this._axesData.xBottom.domain);

        this._yLeftScale = null;
        if (this._axesData.yLeft) {
            this._yLeftScale = d3.scaleLinear()
                .range([height, 0])
                .domain(this._axesData.yLeft.domain);
        }

        this._yRightScale = null;
        if (this._axesData.yRight) {
            this._yRightScale = d3.scaleLinear()
                .range([height, 0])
                .domain(this._axesData.yRight.domain);
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
        function tooltipText(d) {
            return (d && d.tooltip) ? d.tooltip : '';
        }

        // general tooltip
        this._tip = d3.tip()
            .attr('class', 'd3-tip category-graph-tip')
            .style('z-index', 999)
            .offset(function () {
                return [-5, 0];
            })
            .html(tooltipText);

        // line tooltip
        this._lineTip = d3.tip()
            .attr('class', 'd3-tip category-graph-tip')
            .style('z-index', 999)
            .offset(function () {
                var r = this.getBoundingClientRect();
                var m = (r.left + r.right) / 2;
                return [d3.event.clientY - r.top - 4, d3.event.clientX - m];
            })
            .html(tooltipText);

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

        this._svg.select('g.axis--x')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(this._xAxis.scale(this._xScale));

        this._svg.select('g.axis--x').selectAll('.tick text')
            .classed('rotate', this._xAxisTickGeometry.rotated)
            .on('mouseover', this._tip.show)
            .on('touchstart', this._tip.show)
            .on('mouseout', this._tip.hide)
            .on('touchend', this._tip.hide)
            .text(function (d) {
                return DomUtil.truncateTextByWidth(this, d.title, maxTextWidth);
            });

        var labelX = this._sceneGeometry.width / 2;
        var labelY = this._xAxisTickGeometry.areaHeight + 20;
        this._svg.select('g.axis--x .title')
            .attr('transform', 'translate(' + labelX + ', ' + labelY + ')' )
            .text(this._axesData.xBottom.title);
    },

    _updateYLeftAxis: function () {
        if (!this._axesData.yLeft) {
            return this._svg.select('g.axis--y-left').selectAll('*').remove();
        }

        var x = this._sceneGeometry.left;
        var y = this._sceneGeometry.top;

        var axis = d3.axisLeft(this._yLeftScale)
            .tickFormat(this._axisValueFormat(this._axesData.yLeft))
            .ticks(this._axesData.yLeft.ticksCount)
            .tickValues(this._axesData.yLeft.tickValues)
            .scale(this._yLeftScale);

        this._svg.select('g.axis--y-left')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(axis);

        this._svg.select('g.axis--y-left .title')
            .attr('x', - this._sceneGeometry.height / 2)
            .attr('y', '-3em')
            .text(this._axesData.yLeft.title);
    },

    _updateYRightAxis: function () {
        if (!this._axesData.yRight) {
            return this._svg.select('g.axis--y-right').selectAll('*').remove();
        }

        var x = this._sceneGeometry.left + this._sceneGeometry.width;
        var y = this._sceneGeometry.top;

        var axis = d3.axisRight(this._yRightScale)
            .tickFormat(this._axisValueFormat(this._axesData.yRight))
            .ticks(this._axesData.yRight.ticksCount)
            .tickValues(this._axesData.yRight.tickValues)
            .scale(this._yRightScale);

        this._svg.select('g.axis--y-right')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(axis);

        this._svg.select('g.axis--y-right .title')
            .attr('x', - this._sceneGeometry.height / 2)
            .attr('y', '4em')
            .text(this._axesData.yRight.title);
    },

    _updateBars: function (performAnimation) {
        var animationDuration = performAnimation ? this._animationDuration : 0;

        var height = this._sceneGeometry.height;
        var x = this._xScale;

        var setEventHandlers = function(selection) {
            return selection
                .on('mouseover', this._tip.show)
                .on('touchstart.tip', this._tip.show)
                .on('touchstart.anm', this._markTouchstart(true))
                .on('mouseout', this._tip.hide)
                .on('touchend.tip', this._tip.hide)
                .on('touchend.anm', this._markTouchstart(false));
        }.bind(this);

        var g = this._svg.select('g.graph-content-container');

        var categories = g.selectAll('.bar-category')
            .data(this._barsData.categories);

        categories.exit().remove();

        var newCategory = categories.enter().append('g')
            .attr('class', 'bar-category');

        categories.merge(newCategory)
            .attr('transform', function (category) { 
                return 'translate(' + x(category.categoryId) + ', 0)'; 
            }.bind(this));

        newCategory.selectAll('.bar')
            .data(function (category) { return category.data; })
            .enter().append('rect')
                .attr('class', 'bar')
                .style('fill', function (d) { return d.color; })
                .style('stroke', function (d) { return d.color; })
                .call(setEventHandlers);
                

        var updatedBars = categories.selectAll('.bar')
            .data(function (category) { return category.data; });

        updatedBars.enter().append('rect')
            .attr('class', 'bar')
            .style('fill', function (d) { return d.color; })
            .style('stroke', function (d) { return d.color; })
            .call(setEventHandlers);

        updatedBars.exit().remove();

        categories.selectAll('.bar').merge(updatedBars)
            .transition()
            .duration(animationDuration)
            .style('fill', function (d) { return d.color; })
            .style('stroke', function (d) { return d.color; })
            .attr('x', function (d, i, data) { return i * (x.bandwidth() / data.length + 2);  })
            .attr('y', function (d) { return d.y(); })
            .attr('width',  function (d, i, data) {  return x.bandwidth() / data.length; })
            .attr('height', function (d) { return height - d.y(); });
    },

    _updateLines: function(performAnimation) {
        var animationDuration = performAnimation ? this._animationDuration : 0;

        var x = this._xScale;

        var baseLine = d3.line()
            .x(0)
            .y(0);

        var valueLine = d3.line()
            .x(function(d) { return x(d.categoryId) + x.bandwidth() / 2; })
            .y(function(d) { return d.y(); });

        var setEventHandlers = function(selection) {
            return selection
                .on('mouseover', this._lineTip.show)
                .on('mouseout', this._lineTip.hide);
        }.bind(this);

        var g = this._svg.select('g.graph-content-container');

        var lines = g.selectAll('.line')
            .data(this._linesData.lines, function (d) { return d.seriesId; });

        lines.exit().remove();

        lines.enter().append('path')
            .attr('class', 'line')
            .attr('d', baseLine)
            .style('stroke', function (d) { return d.color; })
            .attr('d', function (d) { return baseLine(d.data); })
            .call(setEventHandlers)
            .transition()
                .duration(animationDuration)
                .attr('d', function (d) { return valueLine(d.data); })
                .style('stroke', function (d) { return d.color; });

        lines.transition()
            .duration(animationDuration)
            .attr('d', function (d) { return valueLine(d.data); })
            .style('stroke', function (d) { return d.color; });
    },

    _updateLinePoints: function (performAnimation) {
        var animationDuration = performAnimation ? this._animationDuration : 0;

        var setEventHandlers = function(selection) {
            return selection
                .on('mouseover', this._tip.show)
                .on('touchstart.tip', this._tip.show)
                .on('touchstart.anm', this._markTouchstart(true))
                .on('mouseout', this._tip.hide)
                .on('touchend.tip', this._tip.hide)
                .on('touchend.anm', this._markTouchstart(false));
        }.bind(this);

        var x = this._xScale;
        var g = this._svg.select('g.graph-content-container');

        var newLines = g.selectAll('.line-points-container')
            .data(this._linesData.lines, function (d) { return d.seriesId; });
        newLines.enter().append('g')
            .attr('class', 'line-points-container');
        newLines.exit().remove();

        var lines = g.selectAll('.line-points-container')
            .data(this._linesData.lines, function (d) { return d.seriesId; });        

        var points = lines.selectAll('circle.point')
            .data(function(d) { return d.data; });

        points.enter().append('circle')
            .attr('class', function(d,i) { return 'point point-' + i; })
            .attr('cx', 0)
            .attr('cy', 0)
            .style('fill', function (d) { return d.color; })
            .style('stroke', function (d) { return d.color; })
            .call(setEventHandlers)
            .transition()
                .duration(animationDuration)
                .attr('cx', function(d) { return x(d.categoryId) + x.bandwidth() / 2; })
                .attr('cy', function(d) { return d.y(); });

        points.exit().remove();
        
        points.transition()
            .duration(animationDuration)
            .attr('cx', function(d) { return x(d.categoryId) + x.bandwidth() / 2; })
            .attr('cy', function(d) { return d.y(); })
            .style('fill', function (d) { return d.color; })
            .style('stroke', function (d) { return d.color; });
    },

    _calculateYDomains: function () {
        var domains = {
            yLeft: [0, null],
            yRight: [0, null]
        };

        this.graphViewModel.series.forEach(function (series) {
            series.data.forEach(function (d) {
                var domain = domains[d.axisId];
                if (!domain[1]) domain[1] = d.y;

                domain[0] = Math.min(domain[0], d.y);
                domain[1] = Math.max(domain[1], d.y);
            });
        });

        return domains;    
    },

    _scaleByAxisId: function (axisId) {
        if (axisId === 'yLeft') return this._applyYLeftScale.bind(this);
        if (axisId === 'yRight') return this._applyYRightScale.bind(this);
        
        return;
    },

    _applyYLeftScale: function (value) {
        return this._yLeftScale(value);
    },

    _applyYRightScale: function (value) {
        return this._yRightScale(value);
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
            .attr('class', '.tick')
            .append('text');
        var textSize = DomUtil.calculateTextSize(
            xAxisTickTextTmp.node(), 
            text
        );

        xAxisTmp.remove();

        return textSize;
    },

    _markTouchstart: function (enableMark) {
        return function () {
            return d3.select(this).classed('touchstart', enableMark);
        };
    }
    
}); 
