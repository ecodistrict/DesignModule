function addKPI(parent, data, aWidth, aHeight) {

    var margin = { top: 3, right: 3, bottom: 3, left: 30 },
        width = aWidth - margin.left - margin.right,
        height = aHeight - margin.top - margin.bottom;

    var makeKPI = d3.bullet()
        .tickFormat(null)
        .width(width)
        .height(height);
    /*
    var svg = d3.select(parent).selectAll('svg')
        .data([data])
        .enter().append('svg')
        .attr('class', 'bullet')
        .attr('width', width + margin.left + margin.right)
        .attr('height', height + margin.top + margin.bottom)
        .append('g')
        .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')')
        .call(makeKPI);
    */

    var svg = d3.select(parent).append('svg').data([data])
        .attr('class', 'bullet')
        .attr('width', width + margin.left + margin.right)
        .attr('height', height + margin.top + margin.bottom)
        .append('g')
        .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')')
        .call(makeKPI);


    var title = svg.append('g')
        .style('text-anchor', 'end')
        .attr('transform', 'translate(-6,' + height / 2 + ')');

    title.append('text')
        .attr('class', 'title')
        .text(function (d) { return d.title; });

    title.append('text')
        .attr('class', 'subtitle')
        .attr('dy', '1em')
        .text(function (d) { return d.subtitle; });
}

/*
var kpiData = [
        { 'title': 'kpi1', 'subtitle': '-', 'ranges': [150, 225, 300], 'measures': [220, 270], 'markers': [250] },
        { 'title': 'kpi2', 'subtitle': '-', 'ranges': [150, 225, 300], 'measures': [220, 270], 'markers': [250] },
        { 'title': 'kpi3', 'subtitle': '%', 'ranges': [20, 25, 30], 'measures': [21, 23], 'markers': [26] }
];


var svg = d3.select(detailsControl.kpis).selectAll('svg')
    .data(kpiData)
    .enter().append('svg')
    .attr('class', 'bullet')
    .attr('width', width + margin.left + margin.right)
    .attr('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')')
    .call(makeKPI);


d3.selectAll('button').on('click', function () {
    //svg.datum(randomize).call(makeKPI.duration(1000)); // TODO automatic transition
});
*/