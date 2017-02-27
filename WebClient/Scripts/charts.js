function addChart(parent, definition, aWidth, aHeight, aWithAxes, aWithLegend, aWithRectangle) {
    if (definition.chartType == 'verticalBars')
        addChartVerticalBars(parent, definition, aWidth, aHeight, aWithAxes, aWithLegend, aWithRectangle);
    else if (definition.chartType == 'lines')
        addChartLines(parent, definition, aWidth, aHeight, aWithAxes, aWithLegend, aWithRectangle);
}

function addChartVerticalBars(parent, definition, aWidth, aHeight, aWithAxes, aWithLegend, aWithRectangle) {

    var margin = { top: 3, right: 3, bottom: 3, left: 20 },
        width = aWidth - margin.left - margin.right,
        height = aHeight - margin.top - margin.bottom;

    var x0 = d3.scale.ordinal()
        .rangeRoundBands([0, width], .1);

    var x1 = d3.scale.ordinal();

    var y = d3.scale.linear()
        .range([height, 0]);

    var color = d3.scale.ordinal()
        //.range(['#98abc5', '#8a89a6', '#7b6888', '#6b486b', '#a05d56', '#d0743c', '#ff8c00']);
        //.range(['#5588bb', '#66bbbb', '#aa6644', '#99bb55', '#ee9944', '#444466', '#bb5555']); // http://www.advsofteng.com/doc/cdjavadoc/colorbar.htm
        //.range(['#4F81BC', '#C0504E', '#9BBB58', '#23BFAA', '#8064A1', '#4AACC5', '#F79647', '#33558B']); // http://canvasjs.com/html5-javascript-column-chart/
        //.range(['#1B2B57', '#47558C', '#98ABDE', '#DCE1EF']); // http://www.improving-visualisation.org/case-studies/id=7
        .range(['#4682B4', '#B0C4DE', '#46826D', '#68C1A2', '#FF8C00', '#7C73CE']); // bulletcharts+bit of testing

    var xAxis = d3.svg.axis()
        .scale(x0)
        .orient('bottom');

    var yAxis = d3.svg.axis()
        .scale(y)
        .orient('left')
        .tickFormat(d3.format('.2s'));

    // create main svg
    var svg = d3.select(parent).append('svg')
        .attr('width', width + margin.left + margin.right)
        .attr('height', height + margin.top + margin.bottom)
      .append('g')
        .attr('class', 'chartDetailsMainGroup')
        .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');


    if (aWithRectangle) {
        // add rounded rectangle
        svg.append('rect')
            .attr('width', width + margin.left + margin.right - 3)
            .attr('height', height + margin.top + margin.bottom)
            .attr('x', 3.5 - margin.left)
            .attr('y', 1.5 - margin.top)
            .attr('rx', 3)
            .attr('ry', 3)
            .attr('class', 'chartDetailsRect');
    }

    x0.domain(definition.groupValues.map(function (d) { return d.key; }));
    x1.domain(definition.groupNames).rangeRoundBands([0, x0.rangeBand()]);
    y.domain([0, d3.max(definition.groupValues, function (d) { return d3.max(d.groupValues, function (d) { return d.value; }); })]);
    if (aWithAxes) {
        svg.append('g')
            .attr('class', 'x axis')
            .attr('transform', 'translate(0,' + height + ')')
            .call(xAxis);

        svg.append('g')
            .attr('class', 'y axis')
            .call(yAxis)
          .append('text')
            .attr('transform', 'rotate(-90)')
            .attr('y', 6)
            .attr('dy', '.71em')
            .style('text-anchor', 'end')
            .text(definition.name);
    }

    svg.append('text')
        .attr('transform', 'rotate(-90)')
        .attr('x', -44)
        .attr('y', -12)
        .attr('dy', '.71em')
        .style('text-anchor', 'middle')
        .text(definition.name);

    var key = svg.selectAll('.key')
        .data(definition.groupValues/*data*/)
      .enter().append('g')
        .attr('class', 'key')
        .attr('transform', function (d) { return 'translate(' + x0(d.key) + ',0)'; });

    key.selectAll('rect')
        .data(function (d) { return d.groupValues; })
        .enter().append('rect')
        .attr('width', x1.rangeBand())
        .attr('x', function (d) { return x1(d.name); })
        .attr('y', function (d) { return y(d.value); })
        .attr('height', function (d) { return height - y(d.value); })
        .style('fill', function (d) { return color(d.name); });

    if (aWithLegend) {
        var legend = svg.selectAll('.legend')
            .data(groupNames.slice().reverse())
          .enter().append('g')
            .attr('class', 'legend')
            .attr('transform', function (d, i) { return 'translate(0,' + i * 20 + ')'; });

        legend.append('rect')
            .attr('x', width - 18)
            .attr('width', 18)
            .attr('height', 18)
            .style('fill', color);

        legend.append('text')
            .attr('x', width - 24)
            .attr('y', 9)
            .attr('dy', '.35em')
            .style('text-anchor', 'end')
            .text(function (d) { return d; });
    }
}

/*
 
.axis path,
.axis line {
  fill: none;
  stroke: #000;
  shape-rendering: crispEdges;
}

.x.axis path {
  display: none;
}

.line {
  fill: none;
  stroke: steelblue;
  stroke-width: 1.5px;
}
  
 */

function addChartLines(parent, definition, aWidth, aHeight, aWithAxes, aWithLegend, aWithRectangle) {
    // http://bl.ocks.org/mbostock/3884955

    //var margin = { top: 20, right: 80, bottom: 30, left: 50 },
    //width = 960 - margin.left - margin.right,
    //height = 500 - margin.top - margin.bottom;

    var margin = { top: 3, right: 3, bottom: 3, left: 20 },
    width = aWidth - margin.left - margin.right,
    height = aHeight - margin.top - margin.bottom;



    var parseDate = d3.time.format("%Y%m%d").parse;

    var x = d3.time.scale()
        .range([0, width]);

    var y = d3.scale.linear()
        .range([height, 0]);

    var color = d3.scale.ordinal() // d3.scale.category10();
        .range(['#4682B4', '#B0C4DE', '#46826D', '#68C1A2', '#FF8C00', '#7C73CE']); // bulletcharts+bit of testing

    var xAxis = d3.svg.axis()
        .scale(x)
        .orient("bottom");

    var yAxis = d3.svg.axis()
        .scale(y)
        .orient("left");

    var line = d3.svg.line()
        .interpolate("basis")
        .x(function (d) { return x(d.date); })
        .y(function (d) { return y(d.temperature); });

    var svg = d3.select(parent).append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
      .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    d3.tsv("data.tsv", function (error, data) {
        if (error) throw error;

        color.domain(d3.keys(data[0]).filter(function (key) { return key !== "date"; }));

        data.forEach(function (d) {
            d.date = parseDate(d.date);
        });

        var keys = color.domain().map(function (name) {
            return {
                name: name,
                values: data.map(function (d) {
                    return { date: d.date, temperature: +d[name] };
                })
            };
        });

        x.domain(d3.extent(data, function (d) { return d.date; }));

        y.domain([
          d3.min(keys, function (c) { return d3.min(c.values, function (v) { return v.temperature; }); }),
          d3.max(keys, function (c) { return d3.max(c.values, function (v) { return v.temperature; }); })
        ]);

        if (aWithAxes) {
            svg.append("g")
                .attr("class", "x axis")
                .attr("transform", "translate(0," + height + ")")
                .call(xAxis);

            svg.append("g")
                .attr("class", "y axis")
                .call(yAxis)
              .append("text")
                .attr("transform", "rotate(-90)")
                .attr("y", 6)
                .attr("dy", ".71em")
                .style("text-anchor", "end")
                .text("Temperature (ºF)");
        }

        svg.append('text')
            .attr('transform', 'rotate(-90)')
            .attr('x', -44)
            .attr('y', -12)
            .attr('dy', '.71em')
            .style('text-anchor', 'middle')
            .text(definition.name);

        var key = svg.selectAll(".key")
            .data(keys)
          .enter().append("g")
            .attr("class", "key");

        key.append("path")
            .attr("class", "lineChartLine")
            .attr("d", function (d) { return line(d.values); })
            .style("stroke", function (d) { return color(d.name); });
        if (aWithLegend) {
            key.append("text")
                .datum(function (d) { return { name: d.name, value: d.values[d.values.length - 1] }; })
                .attr("transform", function (d) { return "translate(" + x(d.value.date) + "," + y(d.value.temperature) + ")"; })
                .attr("x", 3)
                .attr("dy", ".35em")
                .text(function (d) { return d.name; });
        }
    });











}

//function addSensorChart(sParent, aData, aWidth, aHeight, aMinimalMax)
//{
//    aMinimalMax = aMinimalMax || 40;

//    var svgContainer = aParent.append("svg")
//                                .attr("width", aWidth)
//                                .attr("height", aHeight);

//    var max = Math.max(d3.max(aData, function (d) { return d.concentration; }), aMinimalMax);

//    var yScale = d3.scale.linear().domain([0, max]).range([5, height - 5]);

//    var xScale = d3.scale.linear().domain([0, aData.length - 1]).range([5, width - 5]);

//    var lineFunction = d3.svg.line()
//                            .x(function(d, i) { return xScale(i);})
//                            .y(function (d) { return yScale(d.concentration); })

//    var lineGraph = svgContainer.append("path")
//                            .attr("d", lineFunction(aData))
//                            .attr("stroke", "blue")
//                            .attr("stroke-width", 2)
//                            .attr("fill", "none");

//    return svgContainer;
//}