/**
 * Set of function for convertion from a given model to a categoryGraphViewModel.
 */

/* globals L */

import CategoryGraphViewModel from './categoryGraphViewModel';

function calculateYDomains(series) {
    var domains = {
        yLeft: [0, null],
        yRight: [0, null]
    };

    series.forEach(function (series) {
        series.data.forEach(function (d) {
            var domain = domains[d.axisId];
            if (!domain[1]) domain[1] = d.y;

            domain[0] = Math.min(domain[0], d.y);
            domain[1] = Math.max(domain[1], d.y);
        });
    });

    return domains;    
}

function buildAxes(categories, axes, series) {
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

    var yDomainsDefault = calculateYDomains(series);

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
}

function buildLines(series) {        

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

    var lines = series
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
}

function buildBars(categories, series) {
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

    var categoriesMap = categories.reduce(
        function (result, category) {
            result[category.id] = { bars: [] };
            return result;
        },
        {}
    );

    series.forEach(function (series) {
        if (series.type !== 'bar') return;

        series.data.forEach(function (point) {
            var bar = createBarData(series, point);
            var categoryInfo = categoriesMap[point.categoryId];
            
            if (categoryInfo) {
                categoryInfo.bars.push(bar);
            }            
        });
    });

    var barCategories = categories.map(function (category) {
        return {
            categoryId: category.id,
            bars: categoriesMap[category.id].bars
        };
    });

    var bars = {
        categories: barCategories            
    };

    return bars;
}


export function convertGraphModelToCategoryGraphViewModel(graphModel) {
    return new CategoryGraphViewModel({
        axes: buildAxes(graphModel.categories, graphModel.axes, graphModel.series),
        lines: buildLines(graphModel.series),
        bars: buildBars(graphModel.categories, graphModel.series)
    });
}
