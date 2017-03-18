var barObject = {
  "data": [
    {'x': 1,'y': 4},
    {'x': 2,'y': 20},
    {'x': 3,'y': 5},
    {'x': 4,'y': 40},
    {'x': 5,'y': 2},
    {'x': 6,'y': 60},
    {'x': 7,'y': 4},
    {'x': 8,'y': 20},
    {'x': 9,'y': 5},
    {'x': 10,'y': 40},
    {'x': 11,'y': 2},
    {'x': 12,'y': 60}
  ],  
  xTitle:"uur per dag",
  yTitle:"Aantal mensen per dag",
  color : d3.scale.category20(),
  margins : {top: 40,right: 20,bottom: 30,left: 40},

  id : "test123",
  type : "bar",
  divWidth : 400,
  divHeight : 400,
  width : 400,
  height : 400,
}


function BarChart(graphObject) {
  this.graphObject = graphObject;
  this.visible = false;
  graphObject.preview = {};
  this.graphID = graphObject.id;
  this.previewDiv = null;

  this.Initialize = function (container) {
    var width = this.graphObject.width,
    height = this.graphObject.height,
    marginLeft = this.graphObject.margins.left,
    marginTop = this.graphObject.margins.top,
    data = barObject.data,
    color = barObject.color;

    // d3.select(container).select('svg').remove();

    var svg = d3.select(container).append("svg")
    .attr("width", width)
    .attr("height", height);

    svg.className = "graph-svg";

    this.graphObject.container = container;
    this.graphObject.svg = svg;
    container.style.visibility = "hidden";
    container.graph = this;
    this.Update();

  }
  this._UpdatePreview = function () {
    console.log(this);
    if (this.previewDiv == null || this.graphObject.data == null || this.graphObject.data.length == 0)
    return;

    var graph = this.graphObject;
    var width = DataManager.detailsInfo.chartWidth;
    var height = DataManager.detailsInfo.chartHeight;

    svg = d3.select(this.previewDiv).select('svg');
    svg.selectAll('g').remove();


  }
  this.GetPreview = function(container)    {
    if (this.previewDiv != null)
    return container.appendChild(this.previewDiv);

    var previewContainer = container.appendChild(document.createElement("div"));
    previewContainer.className = "detailContainer graphDetails";
    previewContainer.style.width = DataManager.detailsInfo.chartWidth + "px";
    previewContainer.style.height = DataManager.detailsInfo.elementHeight + "px";

    this.previewDiv = previewContainer;
    previewContainer.graph = this;

    previewContainer.addEventListener("click", this._clickEvent);

    var title = previewContainer.appendChild(document.createElement("h4"));
    title.className = "detailTitle graphDetailTitle";
    // title.textContent = this.activeNode.title;
    title.style.width = DataManager.detailsInfo.elementWidth + "px";

    var svgContainer = previewContainer.appendChild(document.createElement("div"));
    svgContainer.className = "preview-svg-container";
    svgContainer.style.width = DataManager.detailsInfo.chartWidth + "px";
    svgContainer.style.height = DataManager.detailsInfo.chartHeight + "px";

    var svg = d3.select(svgContainer).append("svg")
    .attr("width", DataManager.detailsInfo.chartWidth)
    .attr("height", DataManager.detailsInfo.chartHeight);

    svg.className = "graph-svg-preview";
    this.graphObject.preview.container = previewContainer;
    this.graphObject.preview.svg = svg;

    var graph = this.graphObject;
    this._UpdatePreview();
  }
  this.Update = function () {

    var graph = this.graphObject;
    d3.select(graph.container).select('svg').selectAll('g').remove();

    //sets data and displaydata
    var width = graph.container.clientWidth;
    var height = graph.container.clientHeight;
    var marginLeft = GraphManager.defaultValues.graphPadding.left + GraphManager.defaultValues.axisMargin.y;
    var marginTop = GraphManager.defaultValues.graphPadding.top;
    var marginRight = GraphManager.defaultValues.graphPadding.right;
    var marginBottom = GraphManager.defaultValues.graphPadding.bottom + GraphManager.defaultValues.axisMargin.x;

    this._fillBarChart(graph);
    graph.svg.attr("width", width);
    graph.svg.attr("height", height);
    this._UpdatePreview();
  }

  this._clickEvent = function (e) {
    var graph = e.currentTarget.graph;
    if (graph.visible) {
      graph._closeGraph();
    } else {
      graph._openGraph();
    }
  }

  this._closeGraph = function () {
    this.visible = false;
    GraphManager.RemoveGraph(this.graphID)
    L.DomUtil.removeClass(this.previewDiv, "chartPreviewActive");
  }

  this._openGraph = function () {
    this.visible = true;
    this._resetSize();
    GraphManager.AddGraph(this.graphObject.container);
    L.DomUtil.addClass(this.previewDiv, "chartPreviewActive");
  }

  this._resetSize = function () {
    var changed = false
    if (parseInt(this.graphObject.container.style.width) != this.graphObject.width) {
      this.graphObject.container.style.width = this.graphObject.width + "px";
      changed = true;
    }
    if (parseInt(this.graphObject.container.style.height) != this.graphObject.height) {
      this.graphObject.container.style.height = this.graphObject.height + "px";
      changed = true;
    }
    if (changed)
    this.Update();
  }

  this._fillBarChart  = function (graph) {
    console.log(graph);
    // d3.select(graph.container).select('svg').remove();

    var xRange = d3.scale.ordinal().rangeRoundBands([graph.margins.left, graph.container.clientWidth - graph.margins.right], 0.1).domain(graph.data.map(function (d) {
      return d.x;
    })),
    yRange = d3.scale.linear().range([graph.container.clientHeight - graph.margins.top, graph.margins.bottom]).domain([0,
      d3.max(graph.data, function (d) {
        return d.y;
      })
    ]),
    xAxis = d3.svg.axis()
    .scale(xRange)
    .tickSize(5)
    .tickSubdivide(true),

    yAxis = d3.svg.axis()
    .scale(yRange)
    .tickSize(5)
    .orient("left")
    .tickSubdivide(true);
    graph.svg.append('g')
    .attr('class', 'x axis')
    .attr('transform', 'translate(0,' + (graph.container.clientHeight - graph.margins.bottom) + ')')
    .call(xAxis);
    graph.svg.append('g')
    .attr('class', 'y axis')
    .attr('transform', 'translate(' + (graph.margins.left) + ',0)')
    .call(yAxis).append("text")
    .attr("transform", "rotate(-90) translate(-20,0)")
    .attr("y", 6)
    .attr("dy", ".71em")
    .style("text-anchor", "end")
    .style("fill","#c5c5c5")
    .text(graph.yTitle);
    graph.svg.selectAll('rect')
    .data(graph.data)
    .enter()
    .append('g')
    .attr('width', xRange.rangeBand())
    .attr('class','blocks')
    .append('rect')
    .attr('value', function (d) {
      return d.y;
    }).attr('x', function (d) {
      return xRange(d.x);
    })
    .attr('y', function (d) {
      return yRange(d.y);
    })
    .attr('width', xRange.rangeBand())
    .attr('height', function (d) {
      return ((graph.container.clientHeight - graph.margins.bottom) - yRange(d.y));
    })
    .attr('fill', function (d,i) {
      return graph.color(i);
    }).on("mouseover", function () {

      if (d3.select(this.parentElement).select('text').empty()) {
        var textObject = d3.select(this.parentElement).append('text').text(event.target.attributes.value.value)
        .attr('width', xRange.rangeBand())
        .attr('height',
        function (d) {
          hoogte = ((graph.container.clientHeight - graph.margins.bottom) - yRange(d.y));
          if (hoogte < 40)
          return 40;
          return hoogte;
        })
        .style("text-anchor", "middle")
        .attr('fill', 'white')
        .style("pointer-events","none");

        textObject.attr("transform", function(d, i) {
          var y = (graph.container.clientHeight - (graph.container.clientHeight - yRange(d.y)) + 20);
          var x = xRange(d.x) + (xRange.rangeBand() / 2) ;

          if (graph.container.clientHeight - d3.select(this).attr('height') < graph.container.clientHeight - (graph.container.clientHeight - yRange(d.y))) {
            y = (graph.container.clientHeight - (graph.container.clientHeight - yRange(d.y)) - 5) ;
            d3.select(this).attr('fill','black');
          }

          return "translate(" + x + "," + y + ")";
        })
      }


    }).on("mouseout", function() {
      d3.select(this.parentElement).select('text').remove();
    });

  }
}
