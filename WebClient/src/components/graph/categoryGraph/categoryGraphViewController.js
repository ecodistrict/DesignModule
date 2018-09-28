/**
 * Category graph view controller.
 */

/* globals L */

import GraphViewController from '../graphViewController';
import CategoryGraphView from './categoryGraphView';
import CategoryGraphViewModel from './categoryGraphViewModel';

var CategoryGraphViewController = GraphViewController.extend({

    onInitialize: function () {
        this.categoryGraphViewModel = new CategoryGraphViewModel({
            axes: this._buildAxes(),
            lines: this._buildLines(),
            bars: this._buildBars()
        });

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
        this.categoryGraphViewModel.set({
            axes: this._buildAxes(),
            lines: this._buildLines(),
            bars: this._buildBars()
        });
    },

    _buildAxes: function () {
        var categories = this.graphViewModel.categories;
        var axes = this.graphViewModel.axes;

        var xDomain = categories.map(function (category) {
            return L.extend({
                tooltip: category.title,
                valueOf: function () { return this.id; }
            }, category);
        });

        var newAxes = {
            xBottom: {
                title: axes.xBottom.title,
                domain: xDomain
            }
        };

        var yDomainsDefault = this._calculateYDomains();

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

        if (axes.yLeft) {
            newAxes.yLeft = createAxisData(axes.yLeft);
        }

        if (axes.yRight) {
            newAxes.yRight = createAxisData(axes.yRight);
        }

        return newAxes;
    },

    _buildLines: function () {        

        function createLinePoint(series, point) {
            return { 
                categoryId: point.categoryId,
                seriesId: series.id,
                seriesTitle: series.title,
                color: series.color,
                axisId: point.axisId,
                y: point.y
            };
        }

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
                    seriesTitle: series.title,
                    color: series.color,
                    data: createLineData(series)
                };
            });
        
        return lines;
    },

    _buildBars: function () {
        function createBarData(series, point) {
            return { 
                categoryId: point.categoryId,
                seriesId: series.id,
                seriesTitle: series.title,
                color: series.color,
                axisId: point.axisId,
                y: point.y
            };
        }

        var categoriesMap = this.graphViewModel.categories.reduce(
            function (result, category) {
                result[category.id] = { bars: [] };
                return result;
            },
            {}
        );

        this.graphViewModel.series.forEach(function (series) {
            if (series.type !== 'bar') return;

            series.data.forEach(function (point) {
                var bar = createBarData(series, point);
                categoriesMap[point.categoryId].bars.push(bar);
            });
        });

        var categories = this.graphViewModel.categories.map(function (category) {
            return {
                categoryId: category.id,
                bars: categoriesMap[category.id].bars
            };
        });

        var bars = {
            categories: categories            
        };

        return bars;
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
    
});

export default CategoryGraphViewController;
