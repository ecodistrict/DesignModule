function addKPI(parent, data, aWidth, aHeight) {

    var margin = { top: 3, right: 3, bottom: 3, left: 30 },
        width = aWidth - margin.left - margin.right,
        height = aHeight - margin.top - margin.bottom;

    var makeKPI = d3.bullet()
        .tickFormat(null)
        .width(width)
        .height(height);

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
