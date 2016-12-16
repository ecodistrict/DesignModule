var graphObjectSpider = {
  data: {
    "title":"Air humidity",
    "labels":["Situatie 1","Situatie 2","Situatie 3"],
    "axes":["Mobility performance","Quality of life","Economic Success","Global Environment"],
    "matrix":[
      [
        {
          "value":8.166666,
          "data":{
            "title":"Situatie 1 - Mobility performance",
            "labels":["Situatie 1","Situatie 2"],
            "axes":["Congestion and delay","Intermodel integration","Accessibility","Commuting travel time","Robustness","Reliability"],
            "matrix":[
              [
                { "value": 5 },
                { "value": 10,
                  "data":{
                  "title":"Situatie 1 - Intermodel integration",
                  "labels":["Situatie 1"],
                  "axes":["Air pollution emissions","Noise hindrance","Heat islands","Green and blue areas","Mobility space uses","Opportunity for active mobility"],
                  "matrix":[
                    [
                      { "value": 3 },
                      { "value": 5 },
                      { "value": 7 },
                      { "value": 4 },
                      { "value": 4 },
                      { "value": 4 }
                    ]
                  ]
                } // eo situatie 1 - Quality of life
                },
                { "value": 4 },
                { "value": 3 },
                { "value": 6 },
                { "value": 6 }
              ],[
                { "value": 2 },
                { "value": 4 },
                { "value": 7 },
                { "value": 1 },
                { "value": 4 },
                { "value": 4 }
              ]
            ]
          } // eo situatie 1 - Mobility performance
        },
        {
          "value": 7.1666,
          "data":{
            "title":"Situatie 1 - Quality of life",
            "labels":["Situatie 1"],
            "axes":["Air pollution emissions","Noise hindrance","Heat islands","Green and blue areas","Mobility space uses","Opportunity for active mobility"],
            "matrix":[
              [
                { "value": 5,
                  "link" : "test123213"
                },
                { "value": 9 },
                { "value": 8 },
                { "value": 7 },
                { "value": 7 },
                { "value": 7 }
              ]
            ]
          } // eo situatie 1 - Quality of life
        },
        {
          "value": 4.333,
          "data":{
            "title":"Situatie 1 - Economic Success",
            "labels":["Situatie 1"],
            "axes":["Commuting travel time","Mobility space usage","Urban functional diversity"],
            "matrix":[
              [
                { "value": 4 },
                { "value": 8 },
                { "value": 1 }
              ]
            ]
          } // eo situatie 1 - Economic Success
        },
        {
          "value": 6.25,
          "data":{
            "title":"Situatie 1 - Global Environment",
            "labels":["Situatie 1"],
            "axes":["Emission of GHG","Heat islands","Opportunity for active mobility","Green and blue areas"],
            "matrix":[
              [
                { "value": 2 },
                { "value": 7 },
                { "value": 8 },
                { "value": 8 }
              ]
            ]
          } // eo situatie 1 - Global Environment
        }
      ], // eo situatie 1
      [
        {
          "value":5.6666,
          "data":{
            "title":"Situatie 2 - Mobility performance",
            "labels":["Situatie 2"],
            "axes":["Congestion and delay","Intermodel integration","Accessibility","Commuting travel time","Robustness","Reliability"],
            "matrix":[
              [
                { "value": 10 },
                { "value": 10 },
                { "value": 9 },
                { "value": 7 },
                { "value": 6 },
                { "value": 6 }
              ]
            ]
          } // eo situatie 2 - Mobility performance
        },
        {
          "value": 3.5,
          "data":{
            "title":"Situatie 2 - Quality of life",
            "labels":["Situatie 2"],
            "axes":["Air pollution emissions","Noise hindrance","Heat islands","Green and blue areas","Mobility space uses","Opportunity for active mobility"],
            "matrix":[
              [
                { "value": 5 },
                { "value": 1 },
                { "value": 8 },
                { "value": 2 },
                { "value": 2 },
                { "value": 3 }
              ]
            ]
          } // eo situatie 2 - Quality of life
        },
        {
          "value": 8.666,
          "data":{
            "title":"Situatie 2 - Economic Success",
            "labels":["Situatie 2"],
            "axes":["Commuting travel time","Mobility space usage","Urban functional diversity"],
            "matrix":[
              [
                { "value": 9 },
                { "value": 8 },
                { "value": 9 }
              ]
            ]
          } // eo situatie 2 - Economic Success
        },
        {
          "value": 8.5,
          "data":{
            "title":"Situatie 2 - Global Environment",
            "labels":["Situatie 2"],
            "axes":["Emission of GHG","Heat islands","Opportunity for active mobility","Green and blue areas"],
            "matrix":[
              [
                { "value": 7 },
                { "value": 9 },
                { "value": 8 },
                { "value": 9 }
              ]
            ]
          } // eo situatie 2 - Global Environment
        }
      ], // eo situatie 2
      [
        {
          "value":3,
          "data":{
            "title":"Situatie 3 - Mobility performance",
            "labels":["Situatie 3"],
            "axes":["Congestion and delay","Intermodel integration","Accessibility","Commuting travel time","Robustness","Reliability"],
            "matrix":[
              [
                { "value": 5 },
                { "value": 1 },
                { "value": 4 },
                { "value": 3 },
                { "value": 3 },
                { "value": 2 }
              ]
            ]
          } // eo situatie 3 - Mobility performance
        },
        {
          "value": 8.666,
          "data":{
            "title":"Situatie 3 - Quality of life",
            "labels":["Situatie 3"],
            "axes":["Air pollution emissions","Noise hindrance","Heat islands","Green and blue areas","Mobility space uses","Opportunity for active mobility"],
            "matrix":[
              [
                { "value": 6 },
                { "value": 9 },
                { "value": 8 },
                { "value": 9 },
                { "value": 9 },
                { "value": 9 }
              ]
            ]
          } // eo situatie 3 - Quality of life
        },
        {
          "value": 2,
          "data":{
            "title":"Situatie 3 - Economic Success",
            "labels":["Situatie 3"],
            "axes":["Commuting travel time","Mobility space usage","Urban functional diversity"],
            "matrix":[
              [
                { "value": 1 },
                { "value": 2 },
                { "value": 3 }
              ]
            ]
          } // eo situatie 3 - Economic Success
        },
        {
          "value": 3.25,
          "data":{
            "title":"Situatie 3 - Global Environment",
            "labels":["Situatie 3"],
            "axes":["Emission of GHG","Heat islands","Opportunity for active mobility","Green and blue areas"],
            "matrix":[
              [
                { "value": 2 },
                { "value": 7 },
                { "value": 2 },
                { "value": 2 }
              ]
            ]
          } // eo situatie 3 - Global Environment
        }
      ] // eo situatie 3
    ]
  }, // eo dataTest
  id : "test123",
  type : "spider",
  divWidth : 240,
  divHeight : 240,
  width : 400,
  height : 400,
  cfg :   {
    w: 201,
    h: 201,
    maxValue: 10, //What is the value that the biggest circle will represent
    levels: 5, //How many levels or inner circles should there be drawn
    labelFactor: 1.32, 	//How much farther than the radius of the outer circle should the labels be placed
    margin: {top: 100, right: 100, bottom: 100, left: 100},
    roundStrokes: true, //If true the area and stroke will follow a round path (cardinal-closed)
    color: d3.scale.category10(), //Color function
    wrapWidth: 60, 		//The number of pixels after which a label needs to be given a new line
    opacityArea: 0.35, 	//The opacity of the area of the blob
    dotRadius: 4, 			//The size of the colored circles of each blog
    opacityCircles: 0.1, 	//The opacity of the circles of each blob
    strokeWidth: 2, 		//The width of the stroke around each blob
    fontSize:  '11px', 		//The width of the stroke around each blob
  }
};


var graphObjectSpider2 = JSON.parse(JSON.stringify(graphObjectSpider));
graphObjectSpider2.cfg = {};
graphObjectSpider2.cfg = graphObjectSpider.cfg;
graphObjectSpider2.id = "test123213";

function updateGraph(graphId, data) {

  var graph = GraphManager._getGraph(graphId).graph;
  if (graph !== null) { //only update graphs that exist

    d3.select(graph.container).select('svg').selectAll('g').remove();

    //sets data and displaydata
    var width = graph.graphObject.container.clientWidth;
    var height = graph.graphObject.container.clientHeight;
    var marginLeft = GraphManager.defaultValues.graphPadding.left + GraphManager.defaultValues.axisMargin.y;
    var marginTop = GraphManager.defaultValues.graphPadding.top;
    var marginRight = GraphManager.defaultValues.graphPadding.right;
    var marginBottom = GraphManager.defaultValues.graphPadding.bottom + GraphManager.defaultValues.axisMargin.x;;

    graph.graphObject.dataset = data;
    graph.graphObject.data = setLevelData(0, false, data);
    graph._fillSpider(graph.graphObject);

    graph.graphObject.svg.attr("width", width);
    graph.graphObject.svg.attr("height", height);

    graph._UpdatePreview();

  }
}

function spiderNode(data, graphObject, parent) {
  this.graphObject = graphObject;
  this.parent = parent;
  this.labels = data.labels;
  this.axes = data.axes;
  this.matrix = data.matrix;
  this.title = data.title;
}

function SpiderChart(graphObject) {
  this.graphObject = graphObject;
  this.visible = false;
  graphObject.preview = {};
  this.graphID = graphObject.id;
  this.previewDiv = null;
  this.activeNode = new spiderNode(graphObject.data, graphObject, null);

  this.Initialize = function (container) {
    var width = this.graphObject.width;
    var height = this.graphObject.height;
    var marginLeft = GraphManager.defaultValues.graphPadding.left + GraphManager.defaultValues.axisMargin.y;
    var marginTop = GraphManager.defaultValues.graphPadding.top;

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
    if (this.previewDiv == null || this.graphObject.data == null || this.graphObject.data.length == 0)
    return;
    if (this.activeNode.matrix.length == 0 || this.activeNode.matrix[0].length == 0)
    return;

    var graph = this.graphObject;
    var width = DataManager.detailsInfo.chartWidth;
    var height = DataManager.detailsInfo.chartHeight;

    svg = d3.select(this.previewDiv).select('svg');
    svg.selectAll('g').remove();

    // //If the supplied maxValue is smaller than the actual one, replace by the max in the data
    var maxValue = graph.cfg.maxValue;
    var allAxis = this.activeNode.axes,	//Names of each axis
    total = allAxis.length,					//The number of different axes

    radius = Math.min(width/2 -10, height/2 -10),
    Format = d3.format('.1f');
    angleSlice = Math.PI * 2 / total;		//The width in radians of each "slice"

    //Scale for the radius
    var rScale = d3.scale.linear()
    .range([0, radius])
    .domain([0, maxValue]);

    //Append a g element
    var g = svg.append("g").attr("transform", "translate(" + (width/2) + "," + (height/2) + ")");

    /////////////////////////////////////////////////////////
    /////////////// Draw the Circular grid //////////////////
    /////////////////////////////////////////////////////////

    //Wrapper for the grid & axes
    var axisGrid = g.append("g").attr("class", "axisWrapper");
    //Draw the background circles

    axisGrid.selectAll(".levels")
    .data(d3.range(1,(this.graphObject.cfg.levels+1)).reverse())
    .enter()
    .append("circle")
    .attr("class", "gridCircle")
    .attr("r", function(d, i){
      return radius/graph.cfg.levels*d;
    }).style("opacity", "0");

    /////////////////////////////////////////////////////////
    //////////////////// Draw the axes //////////////////////
    /////////////////////////////////////////////////////////

    //Create the straight lines radiating outward from the center
    var axis = axisGrid.selectAll(".axis")
    .data(allAxis)
    .enter()
    .append("g")
    .attr("class", "axis");
    //Append the lines
    axis.append("line")
    .attr("x1", 0)
    .attr("y1", 0)
    .attr("x2", function(d, i){ return rScale(maxValue*1.1) * Math.cos(angleSlice*i - Math.PI/2); })
    .attr("y2", function(d, i){ return rScale(maxValue*1.1) * Math.sin(angleSlice*i - Math.PI/2); })
    .attr("class", "line")
    .style("stroke", "#CDCDCD")
    .style("stroke-width", "1px")

    //The radial line function
    var radarLine = d3.svg.line.radial()
    .interpolate("linear-closed")
    .radius(function(d) { return rScale(d.value); })
    .angle(function(d,i) {	return i*angleSlice; });

    if(graph.cfg.roundStrokes)
      radarLine.interpolate("cardinal-closed");

    //Create a wrapper for the blobs
    var blobWrapper = g.selectAll(".radarWrapper")
    .data(this.activeNode.matrix)
    .enter().append("g")
    .attr("class", "radarWrapper");

    //Append the backgrounds
    blobWrapper
    .append("path")
    .attr("class", "radarArea")
    .attr("d", function(d,i) {
      return radarLine(d);
    })
    .style("fill-opacity", graph.cfg.opacityArea);
    blobWrapper.style("fill", function(d,i) { return graph.cfg.color(i); })
    .style("stroke", function(d,i) { return graph.cfg.color(i); })
    .style("stroke-width", "1.5px")

    //Create the outlines
    blobWrapper.append("path")
    .attr("class", "radarStroke")
    .attr("d", function(d,i) {
      return radarLine(d);
    })
    .style("stroke-width",  "1.5px")
    .style("fill", "none")
    .style("filter" , "url(#glow_small)");


    //Filter for the outside glow
    var filter = g.append('defs').append('filter').attr('id','glow_small'),
    feGaussianBlur = filter.append('feGaussianBlur').attr('stdDeviation','0.8').attr('result','coloredBlur'),
    feMerge = filter.append('feMerge'),
    feMergeNode_1 = feMerge.append('feMergeNode').attr('in','coloredBlur'),
    feMergeNode_2 = feMerge.append('feMergeNode').attr('in','SourceGraphic');



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
    title.textContent = this.activeNode.title;
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

    this._fillSpider(graph);
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

  this._fillSpider = function (graph) {

    d3.select(graph.container).select('svg').selectAll('g').remove();

    //If the supplied maxValue is smaller than the actual one, replace by the max in the data
    var maxValue = graph.cfg.maxValue;
    var allAxis = this.activeNode.axes,	//Names of each axis
    total = allAxis.length,					//The number of different axes
    // radius = Math.min(cfg.w/2, cfg.h/2), 	//Radius of the outermost circle

    radius = Math.min((graph.container.clientWidth - (graph.cfg.margin.left + graph.cfg.margin.right))/2, (graph.container.clientHeight - (graph.cfg.margin.top + graph.cfg.margin.bottom)) /2),
    Format = d3.format('.1f');
    angleSlice = Math.PI * 2 / total;		//The width in radians of each "slice"

    //Scale for the radius
    var rScale = d3.scale.linear()
    .range([0, radius])
    .domain([0, maxValue]);

    //Initiate the radar chart SVG
    var svg = graph.svg
    .attr("width",  graph.cfg.w + graph.cfg.margin.left + graph.cfg.margin.right)
    .attr("height", graph.cfg.h + graph.cfg.margin.top + graph.cfg.margin.bottom)
    .attr("class", "radar"+graph.id);

    if (d3.select('.legend').empty())
      d3.select('svg').remove('.legend');

    var LegendOptions = [];
    var legend;

    LegendOptions = [];
    for (var i = 0; i < this.activeNode.labels.length; i++) {
      LegendOptions.push(this.activeNode.labels[i]);
    }
    //Initiate Legend
    var legend = svg.append("g")
    .attr("class", "legend")
    .attr("height", 100)
    .attr("width", graph.cfg.w)
    .attr('transform', 'translate(20,40)');

    //Create colour squares
    legend.selectAll('rect')
    .data(LegendOptions)
    .enter()
    .append("rect")
    .attr("x", 0)
    .attr("y", function(d, i){ return i * 20;})
    .attr("width", 10)
    .attr("height", 10)
    .style("fill", function(d, i){ return  graph.cfg.color(i);});

    //Create text next to squares
    legend.selectAll('text')
    .data(LegendOptions)
    .enter()
    .append("text")
    .attr("x", 17)
    .attr("y", function(d, i){ return i * 20 + 9;})
    .attr("font-size", "11px")
    .attr("fill", "#737373")
    .text(function(d) { return d; });


    if (this.activeNode.parent) {
      var levelContainer = svg.append("g")
      .attr('class', "levelContainer")
      .attr('height', 100)
      .attr('width', graph.cfg.w)
      .attr('transform', 'translate(20,20)');

      BackOptions = ['Terug'];
      levelContainer.selectAll('rect')
      .data(BackOptions)
      .enter()
      .append("text")
      .attr("x", 13)
      .attr("y", function(d, i){ return i * 20 + 9;})
      .attr("font-size", "14px")
      .attr("fill", "#737373")
      .text(function(d) { return d; }).attr('text-anchor','middle')
      .style('cursor','pointer')
      .on('click',(function() {

        this.activeNode = new spiderNode(this.activeNode.parent, this.graphObject, this.activeNode.parent.parent);
        this.Update();

      }).bind(this));

    }

    //Append a g element
    var g = svg.append("g").attr("transform", "translate(" + ((graph.container.clientWidth - (graph.cfg.margin.left + graph.cfg.margin.right) )/2 + graph.cfg.margin.left) + "," + graph.container.clientHeight/2  + ")");

    if (typeof this.activeNode.title !== "undefined") {
      if (typeof graph.title !== "undefined")
        graph.title.remove('text');

      if (typeof this.activeNode.title !== "undefined") {
        graph.title = svg.append("text")
        .attr("x", graph.container.clientWidth/2)
        .attr("y", GraphManager.defaultValues.graphPadding.top)
        .attr("dy", 20 - GraphManager.defaultValues.graphPadding.top)
        .attr("text-anchor", "middle")
        .attr("pointer-events", "none")
        .attr("class", "graph-title-text")
        .style("font-size", "16px")
        .text(this.activeNode.title);
      }
    }

    //Filter for the outside glow
    var filter = g.append('defs').append('filter').attr('id','glow'),
    feGaussianBlur = filter.append('feGaussianBlur').attr('stdDeviation','2.5').attr('result','coloredBlur'),
    feMerge = filter.append('feMerge'),
    feMergeNode_1 = feMerge.append('feMergeNode').attr('in','coloredBlur'),
    feMergeNode_2 = feMerge.append('feMergeNode').attr('in','SourceGraphic');

    /////////////////////////////////////////////////////////
    /////////////// Draw the Circular grid //////////////////
    /////////////////////////////////////////////////////////

    //Wrapper for the grid & axes
    var axisGrid = g.append("g").attr("class", "axisWrapper");

    //Draw the background circles
    axisGrid.selectAll(".levels")
    .data(d3.range(1,(graph.cfg.levels+1)).reverse())
    .enter()
    .append("circle")
    .attr("class", "gridCircle")
    .attr("r", function(d, i){return radius/graph.cfg.levels*d;})
    .style("fill", "#CDCDCD")
    .style("stroke", "#CDCDCD")
    .style("fill-opacity", graph.cfg.opacityCircles)
    .style("filter" , "url(#glow)");

    //Text indicating at what % each level is
    axisGrid.selectAll(".axisLabel")
    .data(d3.range(1,(graph.cfg.levels+1)).reverse())
    .enter().append("text")
    .attr("class", "axisLabel")
    .attr("x", 4)
    .attr("y", function(d){return -d*radius/graph.cfg.levels;})
    .attr("dy", "0.4em")
    .style("font-size", "12px")
    .attr("fill", "#737373")
    .text(function(d,i) { return Format(maxValue * d/graph.cfg.levels); });

    /////////////////////////////////////////////////////////
    //////////////////// Draw the axes //////////////////////
    /////////////////////////////////////////////////////////

    //Create the straight lines radiating outward from the center
    var axis = axisGrid.selectAll(".axis")
    .data(allAxis)
    .enter()
    .append("g")
    .attr("class", "axis");
    //Append the lines
    axis.append("line")
    .attr("x1", 0)
    .attr("y1", 0)
    .attr("x2", function(d, i){ return rScale(maxValue*1.1) * Math.cos(angleSlice*i - Math.PI/2); })
    .attr("y2", function(d, i){ return rScale(maxValue*1.1) * Math.sin(angleSlice*i - Math.PI/2); })
    .attr("class", "line")
    .style("stroke", "white")
    .style("stroke-width", graph.cfg.strokeWidth + "px")

    //Append the labels at each axis
    axis.append("text")
    .attr("class", "legend")
    .style("font-size", "11px")
    .attr("text-anchor", "middle")
    .attr("dy", "0.35em")
    .attr("x", function(d, i){ return rScale(maxValue * graph.cfg.labelFactor) * Math.cos(angleSlice*i - Math.PI/2); })
    .attr("y", function(d, i){ return rScale(maxValue * graph.cfg.labelFactor) * Math.sin(angleSlice*i - Math.PI/2); })
    .text(function(d){return d})
    .call(wrap, graph.cfg.wrapWidth).on('click',(function(event) {
    }).bind(this));

    /////////////////////////////////////////////////////////
    ///////////// Draw the radar chart blobs ////////////////
    /////////////////////////////////////////////////////////

    //The radial line function
    var radarLine = d3.svg.line.radial()
    .interpolate("linear-closed")
    .radius(function(d) {return rScale(d.value);})
    .angle(function(d,i) {	return i*angleSlice; });

    if(graph.cfg.roundStrokes) {
      radarLine.interpolate("cardinal-closed");
    }

    //Create a wrapper for the blobs
    var blobWrapper = g.selectAll(".radarWrapper")
    .data(this.activeNode.matrix)
    .enter().append("g")
    .attr("class", "radarWrapper");

    var mouseOverFunction = function() {
      //Dim all blobs
      this.svg.selectAll(".radarArea")
      .transition().duration(200)
      .style("fill-opacity", 0.1);

      //Bring back the hovered over blob
      d3.select(this.path).select('path').transition().duration(200)
      .style("fill-opacity", 0.7);
    }

    for (var i=0; i < blobWrapper[0].length; i++) {
      blobWrapper[0][i].addEventListener('mouseover', mouseOverFunction.bind({svg:svg,path:blobWrapper[0][i]}))
      blobWrapper[0][i].addEventListener('mouseout', (function(){
        //Bring back all blobs
        this.selectAll(".radarArea")
        .transition().duration(200)
        .style("fill-opacity", graph.cfg.opacityArea);
      }).bind(svg));
    }

    //Append the backgrounds
    blobWrapper
    .append("path")
    .attr("class", "radarArea")
    .attr("d", function(d,i) {return radarLine(d);})
    .style("fill-opacity", graph.cfg.opacityArea);

    blobWrapper
    .style("fill", function(d,i) { return graph.cfg.color(i); })
    .style("stroke", function(d,i) { return graph.cfg.color(i); });

    //Create the outlines
    blobWrapper.append("path")
    .attr("class", "radarStroke")
    .attr("d", function(d,i) {return radarLine(d); })
    .style("stroke-width", graph.cfg.strokeWidth + "px")
    .style("fill", "none")
    .style("filter" , "url(#glow)");

    //Append the circles
    blobWrapper.selectAll(".radarCircle")
    .data(function(d,i) { return d; })
    .enter().append("circle")
    .attr("class", "radarCircle")
    .attr("r", graph.cfg.dotRadius)
    .attr("cx", function(d,i){ return rScale(d.value) * Math.cos(angleSlice*i - Math.PI/2); })
    .attr("cy", function(d,i){ return rScale(d.value) * Math.sin(angleSlice*i - Math.PI/2); })
    .style("fill-opacity", 0.8).style("fill", function(d,i,j) { return graph.cfg.color(j); })

    /////////////////////////////////////////////////////////
    //////// Append invisible circles for tooltip ///////////
    /////////////////////////////////////////////////////////

    //Wrapper for the invisible circles on top
    var blobCircleWrapper = g.selectAll(".radarCircleWrapper")
    .data(this.activeNode.matrix)
    .enter().append("g")
    .attr("class", "radarCircleWrapper");

    //Append a set of invisible circles on top for the mouseover pop-up
    blobCircleWrapper.selectAll(".radarInvisibleCircle")
    .data(function(d,i) { return d; })
    .enter().append("circle")
    .attr("class", "radarInvisibleCircle")
    .attr("r", graph.cfg.dotRadius*1.5)
    .attr("cx", function(d,i){ return rScale(d.value) * Math.cos(angleSlice*i - Math.PI/2); })
    .attr("cy", function(d,i){ return rScale(d.value) * Math.sin(angleSlice*i - Math.PI/2); })
    .style("fill", "none")
    .style("pointer-events", "all")
    .on("mouseenter", (function(d,i,j) {

      newX =  parseFloat(d3.select(d3.event.target).attr('cx')) - 10;
      newY =  parseFloat(d3.select(d3.event.target).attr('cy')) - 10;
      tooltipContainer
      .attr('cx', newX + 10)
      .attr('cy', newY + 10)
      .attr('r', 10)
      .style('opacity', 1)
      .style("fill", graph.cfg.color(j));

      var clicked = this.activeNode.matrix[j][i];
      tooltip
      .attr('x', newX + 10)
      .attr('y', newY + 14)
      .style("fill", "#fff")
      .style("font-size", "10px")
      .text(Format(d.value))
      .style('cursor','pointer')
      .style("text-anchor", "middle")
      .on("click", (function() {
        if (typeof this.clicked.data !== 'undefined') {
          this.graphObject.activeNode = new spiderNode(this.clicked.data, this.graphObject, this.graphObject.activeNode);
          this.graphObject.Update();
        }
        if (typeof this.clicked.link !== 'undefined') {
          if (GraphManager._getGraph(this.clicked.link)) {
            this.graphObject.activeNode = new spiderNode(GraphManager._getGraph(this.clicked.link).graph.activeNode, this.graphObject, this.graphObject.activeNode);
            this.graphObject.Update();
          }
        }

      }).bind({graphObject : this, "clicked": clicked}));
    }).bind(this));

    //Set up the small tooltip for when you hover over a circle
    var tooltipGroup = g.append('g').attr("class","tooltipGroup").style("opacity", 1);
    var tooltipContainer = tooltipGroup.append("circle").attr("class", "tooltipcontainer");
    var tooltip = tooltipGroup.append("text").attr("class", "tooltip");

    tooltipGroup.on("mouseenter", (function() {
      g.select('.tooltipGroup').style("opacity", 1);
    }).bind(g)).on("mouseleave", (function() {
      g.select('.tooltipGroup').style("opacity", 0);
    }).bind(g));
  }
}

function wrap(text, width) {
  text.each(function() {
    var text = d3.select(this),
    words = text.text().split(/\s+/).reverse(),
    word,
    line = [],
    lineNumber = 0,
    lineHeight = 1.4, // ems
    y = text.attr("y"),
    x = text.attr("x"),
    dy = parseFloat(text.attr("dy")),
    tspan = text.text(null).append("tspan").attr("x", x).attr("y", y).attr("dy", dy + "em");

    while (word = words.pop()) {
      line.push(word);
      tspan.text(line.join(" "));
      if (tspan.node().getComputedTextLength() > width) {
        line.pop();
        tspan.text(line.join(" "));
        line = [word];
        tspan = text.append("tspan").attr("x", x).attr("y", y).attr("dy", ++lineNumber * lineHeight + dy + "em").text(word);
      }
    }
  });
}//wrap
