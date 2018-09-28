# Debugging hints

## Trigger functionality from developer console

In Chrome or Explorer or any other browser one can execute javascript code from Developer Console and trigger application functionality.

An important detail is that classes are not directly accessible from the global scope because our code is wrapped in modules. To access a class or any other object or function exported by some module you can use `require(modulePath)` function. This function is available in the global scope and it allows loading the specified module using its path relative to project folder, e.g `require('./src/components/graph/graphModel.js')`. **Note** that `require()` function is available in global scope only in debug build and not in production.

### Example

Show bar graphs on the screen. Put the following code into the console.

```javascript
var charts = [...]; // charts/graphs array

// GraphModel class is not accessible from global 
// scope therefore it should be imported from webpack modules.
var GraphModel = require('./src/components/graph/graphModel.js');
// Now GraphModel can be used to instantiate GraphModel objects.

function convertChartToGraphModel(chart) {
	var color = d3.scaleOrdinal(d3.schemeCategory10);

	var axes = {
		yLeft: {
			formatSpecifier: ',.1f'
		},
		xBottom: {
			title: chart.data.x
		}
	};
	
	var columns = chart.data.columns;
	
	var categories = columns[0].slice(1).map(
	function(category) {
		return {
			id: category,
			title: category
		};
	});
	
	var series = columns.slice(1).map(
	function(row) {
		var data = row.slice(1).map(
		function(item, i) {
			return {
				categoryId: categories[i].id,
				y: item,
				axisId: 'yLeft'			
			};
		});
		var rowName = row[0];
		
		return {
			type: chart.type,
			id: rowName,
			title: rowName,
			color: color(rowName),
			data: data
		};
	});	
	
	return new GraphModel({
		type: 'category',
		id: chart.id,
		title: chart.title,
		axes: axes,
		categories: categories,
		series: series,
	});
}

var graphModels = charts.map(convertChartToGraphModel);

// graphViewManager is exposed to the glbal scope.
graphModels.forEach(function(graphModel) { return graphViewManager.showGraph(graphModel); });
```
