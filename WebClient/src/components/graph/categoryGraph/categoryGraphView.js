/**
 * CategoryGraphView is awindow to display a category graph.
 * Category graph is a graph where x axis is a categories axis.
 */

/* globals d3, L, GraphView  */ 

/* exported CategoryGraphView */
var CategoryGraphView = GraphView.extend({
    
    initialize: function (opts) {
        this._padding = {
            left: 55,
            right: 55,
            top: 10,
            bottom: 60
        };
        this._animationDuration = 300;

        GraphView.prototype.initialize.call(this, opts);
    },

    onRenderGraph: function () {
        if (this._svg) return;

        this.graphViewModel.on('series categories axes', this._onDataChanged, this);
        
        this._buildInternalData();
        this._buildSvgViewport();
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

        this._svg.remove();
        this._svg = null;
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
                        valueOf: function () { return this.id; }
                    }, category);
                })
            }
        };

        if (this.graphViewModel.axes.yLeft) {
            this._axesData.yLeft = {
                title: this.graphViewModel.axes.yLeft.title,
                domain: this.graphViewModel.axes.yLeft.scale || yDomainsDefault.yLeft
            };                
        }

        if (this.graphViewModel.axes.yRight) {
            this._axesData.yRight = {
                title: this.graphViewModel.axes.yRight.title,
                domain: this.graphViewModel.axes.yRight.scale || yDomainsDefault.yRight
            };                
        }

        this._axisMap = {};
        for (var axisName in this.graphViewModel.axes) {
            if(!Object.prototype.hasOwnProperty.call(this.graphViewModel.axes, axisName)) continue;

            var id = this.graphViewModel.axes[axisName].id;
            this._axisMap[id] = axisName;
        }
    },

    _buildInternalBarsData: function () {
        var createBarData = function (series, point) {
            return { 
                seriesId: series.id,
                color: series.color,
                axisId: point.axisId,
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
        var createLinePoint = function (seriesPoint) {
            return {
                categoryId: seriesPoint.categoryId,
                y: function () {
                    var scale = this._scaleByAxisId(seriesPoint.axisId);
                    return  scale(seriesPoint.y);
                }.bind(this)
            };
        }.bind(this);

        function createLineData (data) {
            return data.map(createLinePoint);
        }

        var lines = this.graphViewModel.series
            .filter(function (series) { 
                return series.type === 'line'; 
            })
            .map(function (series) {
                return {
                    seriesId: series.id,
                    color: series.color,
                    data: createLineData(series.data)
                };
            });
        
        this._linesData = { lines: lines };
    },

    _buildSvgViewport: function () {        
        this._svg = d3.select(this.viewportElement()).append('svg')
            .attr('class', 'category-graph');
    },

    _buildSceneGeometry: function () {
        var svgWidth = this._svg.node().getBoundingClientRect().width;
        var svgHeight = this._svg.node().getBoundingClientRect().height;

        function fixValue(value) {
            if (value < 0) return 0;
            return value;
        }
        
        this._sceneGeometry = {
            left: fixValue(this._padding.left),
            top: fixValue(this._padding.top),
            width: fixValue(svgWidth - this._padding.left - this._padding.right),
            height: fixValue(svgHeight - this._padding.top - this._padding.bottom),
        };
    },

    _buildScales: function () {
        var width = this._sceneGeometry.width;
        var height = this._sceneGeometry.height;

        this._xScale = d3.scaleBand().rangeRound([0, width]).padding([0.2]);
        this._xScale.domain(this._axesData.xBottom.domain);

        if (this._axesData.yLeft) {
            this._yLeftScale = d3.scaleLinear()
                .range([height, 0])
                .domain(this._axesData.yLeft.domain);
        }

        if (this._axesData.yRight) {
            this._yRightScale = d3.scaleLinear()
                .range([height, 0])
                .domain(this._axesData.yRight.domain);
        }
    },

    _buildXAxis: function () {
        this._xAxis = d3.axisBottom(this._xScale)
            .tickFormat(function (category) { return category.title; });
        
        this._svg.append('g')
            .attr('class', 'axis axis--x')
            .call(this._xAxis);

        this._svg.select('g.axis--x').append('text')
            .attr('class', 'title');

        this._updateXAxis();
    },

    _buildYLeftAxis: function () {
        if (!this._yLeftScale) return;

        this._yLeftAxis = d3.axisLeft(this._yLeftScale);
        
        this._svg.append('g')
            .attr('class', 'axis axis--y axis--y-left')
            .call(this._yLeftAxis);

        this._svg.select('g.axis--y-left').append('text')
            .attr('class', 'title');

        this._updateYLeftAxis();
    },

    _buildYRightAxis: function () {
        if (!this._yRightScale) return;

        this._yRightAxis = d3.axisRight(this._yRightScale);
        
        this._svg.append('g')
            .attr('class', 'axis axis--y axis--y-right')
            .call(this._yRightAxis);

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
        this._updateXAxis(performAnimation);
        this._updateYLeftAxis(performAnimation);
        this._updateYRightAxis(performAnimation);
        this._updateBars(performAnimation);
        this._updateLines(performAnimation);
    },

    _updateXAxis: function () {
        var x = this._sceneGeometry.left;
        var y = this._sceneGeometry.top + this._sceneGeometry.height;

        this._svg.select('g.axis--x')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(this._xAxis.scale(this._xScale));

        this._svg.select('g.axis--x .title')
            .attr('transform', 'translate(' + (this._sceneGeometry.width / 2) + ', ' + 40 + ')' )
            .text(this._axesData.xBottom.title);
    },

    _updateYLeftAxis: function () {
        if (!this._yLeftAxis) return;

        var x = this._sceneGeometry.left;
        var y = this._sceneGeometry.top;

        this._svg.select('g.axis--y-left')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(this._yLeftAxis.scale(this._yLeftScale));

        this._svg.select('g.axis--y-left .title')
            .attr('x', - this._sceneGeometry.height / 2)
            .attr('y', -35)
            .text(this._axesData.yLeft.title);
    },

    _updateYRightAxis: function () {
        if (!this._yRightAxis) return;

        var x = this._sceneGeometry.left + this._sceneGeometry.width;
        var y = this._sceneGeometry.top;

        this._svg.select('g.axis--y-right')
            .attr('transform', 'translate(' + x + ', ' + y + ')')
            .call(this._yRightAxis.scale(this._yRightScale));

        this._svg.select('g.axis--y-right .title')
            .attr('x', - this._sceneGeometry.height / 2)
            .attr('y', 45)
            .text(this._axesData.yRight.title);
    },

    _updateBars: function (performAnimation) {
        var animationDuration = performAnimation ? this._animationDuration : 0;

        var height = this._sceneGeometry.height;
        var x = this._xScale;

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
                .style('stroke', function (d) { return d.color; });

        var updatedBars = categories.selectAll('.bar')
            .data(function (category) { return category.data; });

        updatedBars.enter().append('rect')
            .attr('class', 'bar')
            .style('fill', function (d) { return d.color; })
            .style('stroke', function (d) { return d.color; });

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

        var g = this._svg.select('g.graph-content-container');

        var lines = g.selectAll('.line')
            .data(this._linesData.lines, function (d) { return d.seriesId; });

        lines.exit().remove();

        lines.enter().append('path')
            .attr('class', 'line')
            .attr('d', baseLine)
            .style('stroke', function (d) { return d.color; })
            .attr('d', function (d) { return baseLine(d.data); })
            .transition()
                .duration(animationDuration)
                .attr('d', function (d) { return valueLine(d.data); })
                .style('stroke', function (d) { return d.color; });

        lines.transition()
            .duration(animationDuration)
            .attr('d', function (d) { return valueLine(d.data); })
            .style('stroke', function (d) { return d.color; });
    },

    _calculateYDomains: function () {
        var axisMap = {};
        if (this.graphViewModel.axes.yLeft) {
            axisMap[this.graphViewModel.axes.yLeft.id] = 'yLeft';
        }

        if (this.graphViewModel.axes.yRight) {
            axisMap[this.graphViewModel.axes.yRight.id] = 'yRight';
        }

        var domains = {
            yLeft: [0, null],
            yRight: [0, null]
        };

        this.graphViewModel.series.forEach(function (series) {
            series.data.forEach(function (d) {
                var domain = domains[axisMap[d.axisId]];
                if (!domain[1]) domain[1] = d.y;

                domain[0] = Math.min(domain[0], d.y);
                domain[1] = Math.max(domain[1], d.y);
            });
        });

        return domains;    
    },

    _scaleByAxisId: function (axisId) {
        var axisName = this._axisMap[axisId];
        if (axisName === 'yLeft') return this._applyYLeftScale.bind(this);
        if (axisName === 'yRight') return this._applyYRightScale.bind(this);
        
        return;
    },

    _applyYLeftScale: function (value) {
        return this._yLeftScale(value);
    },

    _applyYRightScale: function (value) {
        return this._yRightScale(value);
    },

}); 
