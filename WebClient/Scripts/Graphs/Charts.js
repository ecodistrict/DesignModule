function generateCharts() {

  types = ['line','bar','spline','area','step','area-step','area-spline','scatter','pie','donut','gauge'];
  // dataset = [
  //   [],
  //   [['name', 13, 8, 12],['name1', 3, 16, 24, 2]],
  //   [['name', 123, 5, 2],['name1', 2, 132, 55, 72]],
  //   [['name', 8, 12, -25],['name1', 45, 1, -65, 112]],
  //   [['name', 312, 0, 12],['name1', 12, 0, 0, 24]],
  //   [['name', 1, 2, 3],['name1', 121, 55, 265, 2]],
  //   [['name', 312, 15, 51],['name1', 6, 6, 6, 6]],
  //   [['name', 221, 255, 225],['name1', 18, 91, 13, 1]]
  // ];
  var columns = [];
  columns['columns'] = [];
  columns['columns'].push(['name', 22, 55, 25]);
  columns['columns'].push(['name1', 12, 22, 65, 12, null, 52, 13]);


  columns['gauge'] = [];
  columns['gauge'].push(['name', 12, 44]);
  columns['gauge'].push(['name1', 12, 22]);

  for (var i = 0; i < types.length; i++) {

    if (types[i] === 'gauge') {
      var column = columns['gauge'];
    } else {
      var column = columns['columns'];
    }

    chartObject = {
      "data":   {
        columns: column
      },
      "title":"Air humidity",
      margins : {top: 40,right: 20,bottom: 30,left: 40},
      id : "test" + i,
      type : types[i],
      divWidth : 300,
      divHeight : 200,
      width : 300,
      height : 200,
    }


    GraphManager.MakeGraph(chartObject);

  }

}



var chartObject = {
  "data":   {columns: [
    ['name', 22, 55, 25],
    ['name1', 12, 22, 65, 12]
  ]},
  "title":"Air humidity",
  margins : {top: 40,right: 20,bottom: 30,left: 40},
  id : "test123",
  type : "line",
  divWidth : 400,
  divHeight : 400,
  width : 400,
  height : 400,
}


function Chart(graphObject) {

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
    data = barObject.data;

    var svg = d3.select(container).append("div")
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

    var graph = this.graphObject;
    var width = DataManager.detailsInfo.chartWidth;
    var height = DataManager.detailsInfo.chartHeight;

    var svg = d3.select(this.previewDiv).select('svg');

    var columns = graph.data.columns;
    console.log(graph.preview.chart);
    graph.preview.chart.load({
      columns: columns,
      type: this.graphObject.type,
      selection: {
        enabled: false
      }
    });

  }
  this.GetPreview = function(container)    {

    var graph = this.graphObject;

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
    title.textContent = graph.title;
    title.style.width = DataManager.detailsInfo.elementWidth + "px";

    var svgContainer = previewContainer.appendChild(document.createElement("div"));
    svgContainer.className = "preview-svg-container";
    svgContainer.style.width = DataManager.detailsInfo.chartWidth + "px";
    svgContainer.style.height = DataManager.detailsInfo.chartHeight + "px";

    var svg = d3.select(svgContainer).append("svg")
    .attr("width", DataManager.detailsInfo.chartWidth)
    .attr("height", DataManager.detailsInfo.chartHeight);

    svg.className = "graph-svg-preview";
    graph.preview.container = previewContainer;
    graph.preview.svg = svg;
    graph.preview.chart = c3.generate({
      bindto: svg,
      data: {
        columns: []
      },
      size: {
        height: DataManager.detailsInfo.chartHeight,
        width: DataManager.detailsInfo.chartWidth
      },
      axis: {
        x: {show: false },
        y: {show: false }
      },
      point: {
        show: false
      },
      legend: {
        show: false
      }
    });


    graph.preview.chart.internal.config.axis_x_tick_outer = false;
    graph.preview.chart.internal.config.interaction_enabled = false;
    graph.preview.chart.internal.config.tooltip_show = false;

    switch(graph.type) {
      case 'pie':
      graph.preview.chart.internal.config.pie_label_show = false;
      break;
      case 'donut':
      graph.preview.chart.internal.config.donut_label_show = false;
      break;
      case 'gauge':
      graph.preview.chart.internal.config.gauge_label_format = function(value, ratio){
        return ''; //returning here the value and not the ratio
      }
      break;
      default:
    }


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

    graph.chart = c3.generate({
      bindto: graph.svg,
      data: {
        columns: [],
        labels: true,

      },
      size: {
        height: height - marginTop,
        width: width - marginRight
      },
      tooltip: {
        grouped: false
      },
      legend: {
        show: false
      },
      gauge: {
        label: {
          format: function(value, ratio) {
            return '';
          }
        }
      }

      });
      graph.chart.internal.config.legend_show = this.visible;
      this.graphObject.container.getElementsByClassName('graph-title')[0].innerText = this.graphObject.title;
      graph.chart.element.style.marginTop = '26px';
      this._fillCharts(graph);
      graph.svg.attr("width", width - (marginLeft + marginRight));
      graph.svg.attr("height", height - (marginTop + marginBottom));
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
    this.ShowGraph = function() {
      this._openGraph();
    }
    this.HideGraph = function() {
      this._closeGraph();
    }
    this._closeGraph = function () {
      this.visible = false;
      GraphManager.RemoveGraph(this.graphID)
      L.DomUtil.removeClass(this.previewDiv, "chartPreviewActive");
      this.graphObject.chart.internal.config.legend_show = false;
      this.graphObject.chart.legend.show = false;
      this.Update();
    }

    this._openGraph = function () {
      this.visible = true;
      this._resetSize();
      GraphManager.AddGraph(this.graphObject.container);
      L.DomUtil.addClass(this.previewDiv, "chartPreviewActive");
      this.graphObject.chart.internal.config.legend_show = true;
      this.graphObject.chart.legend.show = true;
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
      if (changed) {
        this.graphObject.chart.resize({
          height: parseInt(this.graphObject.height),
          width: parseInt(this.graphObject.width)
        });
      }

      this.Update();
    }

    this._fillCharts  = function (graph) {
      // d3.select(graph.container).select('svg').remove();
      var columns = graph.data.columns;
      graph.chart.load({
        columns: columns,
        type: graph.type
      });

    }
  }
