/**
 * Graph service. Provides graph models to the application.
 * Responsible for retrieving graphs from server side and 
 * maintaining them in sync with the server.
 */

/* globals L, d3, wsSend */

import './graphModel';
import ModelCollection from '../../core/modelCollection';
import GraphModel from './graphModel';
import chroma from 'chroma-js';

function buildCategories(chartColumns) {
    var categories = chartColumns[0].slice(1).map(function(category) {
		return {
			id: '' + category,
			title: '' + category
		};
    });
    
    return categories;
}

function buildSeries(type, chartColumns, nameFunction, colorFunction) {
    var categories = buildCategories(chartColumns);

    var series = chartColumns.slice(1).map(function(row) {
        if (!Array.isArray(row)) return null;

		var data = row.slice(1).map(function(item, i) {
			return {
				categoryId: categories[i].id,
				y: item,
				axisId: 'yLeft'			
			};
		});
		var rowName = row[0];
		
		return {
			type: type,
			id: nameFunction(rowName),
			title: nameFunction(rowName),
			color: colorFunction(rowName),
			data: data
		};
    });
    series = series.filter(function (item) { return !!item; });

    return series;
}

function convertChartToGraphModel(chart) {
    if (!chart) throw new Error('chart is not provided');
    if (!chart.data) throw new Error('chart.data is not provided');
    if (!Array.isArray(chart.data.columns)) throw new Error('chart.data.columns is not an array');    

	var color = d3.scaleOrdinal(d3.schemeCategory10);

	var axes = {
		yLeft: {
			formatSpecifier: ',.1f'
		},
		xBottom: {
			title: chart.data.x || ''
		}
	};
	
	var columns = chart.data.columns;
	
	var categories = buildCategories(columns);	
    var series = buildSeries(chart.type, columns, function (name) { return name; }, color);
    
    if (chart.ref) {
        var refChart = chart.ref;
        if (!refChart.data) throw new Error('chart.ref.data is not provided');
        if (!Array.isArray(refChart.data.columns)) throw new Error('chart.ref.data.columns is not an array');

        var refColumns = chart.data.columns;
        var refSeries = buildSeries(
            chart.type,
            refColumns, 
            function (name) { return 'Reference ' + name; }, 
            function (x) { return chroma(color(x)).brighten(1.4).desaturate(1.1); }
        );

        if (series.length !== refSeries.length) throw new Error('ref series doesn\'t match base series' );

        var combinedSeries = [];
        for (var i = 0; i < series.length; ++i) {
            combinedSeries.push(series[i]);
            combinedSeries.push(refSeries[i]);
        }
        
        series = combinedSeries;
    }
	
	return new GraphModel({
		type: 'category',
		id: chart.id,
		title: chart.title,
		axes: axes,
		categories: categories,
        series: series,
        clickable: chart.clickable
	});
}

function updateGraph(graphModel, graphData) {
    if (!graphModel) return;
    if (!graphData) return;

    graphModel.title = graphData.title;
    graphModel.axes = graphData.axes;
    graphModel.categories = graphData.categories;
    graphModel.series = graphData.series;
    graphModel.clickable = graphData.clickable;
}

var GraphService = L.Evented.extend({

    initialize: function () {
        this._graphsModel = new ModelCollection();
        
        Object.defineProperty(this, 'graphsModel', {
            get: function () { return this._graphsModel; }
        });
    },

    remove: function () {
        this.off();
        
        this._graphsModel.off();
        this._graphsModel.set([]);
        this._graphsModel = null;
    },

    setGraphs: function (graphs) {
        if (!graphs) return;

        var originalGraphs = Array.isArray(graphs) ? graphs : [graphs];

        try {
            var graphModels = originalGraphs.map(convertChartToGraphModel);
            this.graphsModel.set(graphModels);
        } catch (error) {
            console.error('Failed to set graphs because graphs data is incorrect.', error);
        }
    },

    updateGraphs: function (graphs) {
        if (!graphs) return;
        
        var originalGraphs = Array.isArray(graphs) ? graphs : [graphs];
        var graphsModel = this.graphsModel;

        try {
            var graphModels = originalGraphs
                .map(convertChartToGraphModel)
                .map(function (newGraphModel) {
                    var graphModel = graphsModel.getById(newGraphModel.id);
                    if (graphModel) {
                        updateGraph(graphModel, newGraphModel);
                        return graphModel;
                    }
                    return newGraphModel;
                });
            graphsModel.set(graphModels);
        } catch (error) {
            console.error('Failed to update graphs because graphs data is incorrect.', error);
        }
    },

    showGraphs: function (graphs) {
        if (!graphs) return;
        
        var originalGraphs = Array.isArray(graphs) ? graphs : [graphs];
        var graphsModel = this.graphsModel;

        try {
            var graphModels = originalGraphs.map(convertChartToGraphModel);
            graphModels.forEach(function (newGraphModel) {
                var graphModel = graphsModel.getById(newGraphModel.id);
                if (!graphModel) return;

                updateGraph(graphModel, newGraphModel);
            });

            this.fire('showGraphs', {
                graphModels: graphModels
            });
        } catch (error) {
            console.error('Failed to show graphs because graphs data is incorrect.', error);
        }
    },

    reportGraphCategorySelected: function (graphModel, categoryId) {
        if (!graphModel) return;
        if (graphModel.clickable !== 'labels') return;

        wsSend({
            'type': 'graphLabelClick',
            'payload': {
                'graphID': graphModel.id,
                'labelTitle': categoryId
            }
        });
    }

});

export default GraphService;
