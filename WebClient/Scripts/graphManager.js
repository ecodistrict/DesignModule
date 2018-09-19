var graphPosition = {
    bottomLeft: "bottomleft",
    bottomRight: "bottomright",
    topLeft: "topleft",
    topRight: "topRight"
}

var graphType = {
    horizontalBar: "hbar",
    verticalBar: "vbar",
    line: "line",
    spider: "spider",
    scatterplot: "splot"
}

var clickOptions = {
    none: "none",
    xAxis: "xAxis",
    yAxis: "yAxis",
    both: "both",
    labels: "labels"
}

var GraphManager = {
    alignedGraphs: [],
    movedGraphs: [],
    hiddenGraphs: [],
    graphs: [],
    position: null,
    container: null,
    zIndexManager: {
        baseIndex: 501,
        graphDivs: [],
        updateIndexes: function () {
            for (var i = o; i < GraphManager.zIndexManager.graphDivs.length; i++) {
                GraphManager.zIndexManager.graphDivs[i].style.zIndex = "" + (GraphManager.zIndexManager.baseIndex + i + 1);
            }
        },
        focus: function (graphID) {
            var graphDiv = null;
            for (var i = 0; i < GraphManager.zIndexManager.graphDivs.length; i++) {
                if (graphDiv != null) {
                    GraphManager.zIndexManager.graphDivs[i].style.zIndex = "" + (GraphManager.zIndexManager.baseIndex + i);
                    GraphManager.zIndexManager.graphDivs[i - 1] = GraphManager.zIndexManager.graphDivs[i];
                }
                else if (GraphManager.zIndexManager.graphDivs[i].graphID == graphID) {
                    graphDiv = GraphManager.zIndexManager.graphDivs[i];
                }
            }
            if (graphDiv != null) {
                GraphManager.zIndexManager.graphDivs[GraphManager.zIndexManager.graphDivs.length - 1] = graphDiv;
                graphDiv.style.zIndex = "" + (GraphManager.zIndexManager.baseIndex + GraphManager.zIndexManager.graphDivs.length);
            }
        },
        newGraph: function (graphDiv) {
            GraphManager.zIndexManager.graphDivs.push(graphDiv);
            graphDiv.style.zIndex = "" + (GraphManager.zIndexManager.baseIndex + GraphManager.zIndexManager.graphDivs.length);
        },
        removeGraph: function (graphID) {

        }
    },
    dragObject: {
        dragDiv: null,
        moved: null,
        hAlign: null,
        vAlign: null,
        hSign: null,
        vSign: null,
        startMouseX: null,
        startMouseY: null,
        startDivX: null,
        startDivY: null,
        prevIndex: null
    },
    resizeObject: {
        resizeDiv: null,
        startMouseX: null,
        startMouseY: null,
        startDivW: null,
        startDivH: null,
    },
    idcounter: 0, //Only for fake data purposes
    ActiveCount: 0,
    defaultValues: {
        width: 300, //Width of a graph div todo: positioning when not using default values
        height: 200, //Height of a graph div
        description: "No Description", //Description will be showed if no description is provided
        name: "No Name", //Displayed if no name if provided
        position: graphPosition.topLeft, //prevered positioning of graphs
        type: graphType.line, //Default graph type
        interpolation: "linear", //https://coderwall.com/p/thtwbw/d3-js-interpolation-options
        flashBorder: false, //todo: implementation. makes border of a graph flash when received new data
        maxPoints: 100, //9007199254740991, //Graph only plots the last x points of an array
        x: { label: "", set: 0 }, //Attribute name for the attribute holding the value for the x-axis
        y: [{ color: "LightBlue", label: "", set: 0 }], //Attribute name for the attribute holding the value of the y-axis along with color and label
        xScale: "linear", //sets the scale to use for the x axis "linear"/"ordinal"/"power"/"log" todo: date
        yScale: "linear", //sets the scale to use for the y axis "linear"/"ordinal"/"power"/"log" todo: date
        xAxis: true, //sets showing of x axis
        yAxis: true, //sets showing of y axis
        xAxisOrient: "bottom", //sets orientation of xAxis: "bottom"/"top"
        yAxisOrient: "left", //sets orientation of yAxis: "left"/"right"
        holdminmax: true, //Keeps a progressive track on min max values of the graph even if those min/max values won't be displayed anymore
        additive: false, //For entries with multiple y values, stacks them together if true
        leftMargin: 48, //Left margin of the GraphManager
        rightMargin: 63, //Right margin of the GraphManager
        topMargin: 48, //Top margin of the GraphManager
        bottomMargin: 50, //Bottom margin of the GraphManager
        graphSpacing: { //extra spacing between the graphs when auto positioning
            x: 0,
            y: 10
        },
        axisMargin: { //Margins for the axis
            x: 20,
            y: 40
        },
        margins: { top: 40, right: 20, bottom: 30, left: 40 }, //used for new charts
        graphPadding: {
            left: 10,
            right: 10,
            top: 30,
            bottom: 10
        },
        axisTextPadding: 5,
        draggable: true, //Makes graphs draggable or not
        snapping: true, //Makes graphs snap to aligned graphs and inserts them on drop
        snapRange: 10, //Falloff distance to consider a snap
        resizable: true, //Whether or not graphs can be resized
        resizeSnapping: true, //Makes graphs snap to the default size (and then they can also snap tot he aligned graphs)
        resizeSnapRange: 10, //Falloff distance to consider a resize snap
        maxWidth: null, //Maximum resize width for a graph
        maxHeight: null, //Maximum resize height for a graph
        minWidth: 300, //Minimum resize width for a graph
        minHeight: 200, //Minumum resize height for a graph
        clickable: clickOptions.none,
        xOffset: 25, //x offset when aligning graphs on top of each other
        yOffset: 25, //y offset when aligning graphs on top of each other
        margins: { top: 40, right: 20, bottom: 30, left: 40 },
        showLabels: false,
        showLegends: false
    },

    Initialize: function () {
        GraphManager.position = GraphManager.defaultValues.position;

        this.container = document.body.appendChild(document.createElement("div"));
        this.container.id = "graphsContainer";
        //this.container.style.position = "fixed";
        this.container.style.left = this.defaultValues.leftMargin + "px";
        this.container.style.top = this.defaultValues.topMargin + "px";
        //this.container.style.backgroundColor = "rgba(255, 0, 255, 0.5)";
        this.container.style.zIndex = "" + GraphManager.zIndexManager.baseIndex;
        this._resize();
        window.addEventListener("resize", this._resize);
    },

    _resize: function () {
        var w = window.innerWidth;
        var h = window.innerHeight;

        GraphManager.container.style.width = w - (GraphManager.defaultValues.leftMargin + GraphManager.defaultValues.rightMargin) + "px";
        GraphManager.container.style.height = h - (GraphManager.defaultValues.topMargin + GraphManager.defaultValues.bottomMargin) + "px";

        GraphManager.RepositionGraphs();
    },

    //todo check if SetPosition is obsolete
    SetPosition: function (aPos) {
        GraphManager.position = aPos;
        GraphManager.RepositionGraphs();
    },

    SetPreviews: function (container) {
        var counter = 0;
        for (var i = 0; i < GraphManager.graphs.length; i++) {
            if (GraphManager.graphs[i].enabled && !GraphManager.graphs[i].standalone) {
                counter++;
                GraphManager.graphs[i].graph.GetPreview(container);
            }
        }
        return counter;
    },

    updateDomains: function (activegraphs) {
        GraphManager.ActiveCount = 0;
        for (var i = 0; i < GraphManager.graphs.length; i++) {
            if (typeof activegraphs[GraphManager.graphs[i].graphID] !== "undefined") {
                GraphManager.ActiveCount++;
                GraphManager.graphs[i].enabled = true;
            }
            else {
                GraphManager.graphs[i].enabled = false;
                if (GraphManager.graphs[i].graph.visible) {
                    GraphManager.graphs[i].graph.HideGraph();
                }
            }
            //todo show/hide graphs we're already showing??
        }
    },

    MakeGraph: function (graphObject) {

        var g = GraphManager._getGraph(graphObject.id);
        if (g != null || graphObject.type == "spider") {
            if (g != null && g.graph!=null && g.graph.ReInit)
                g.graph.ReInit(graphObject);
            return;
        }

        graphDiv = document.createElement("div");
        if (graphObject.type == "line" || typeof graphObject.type == "undefined")
            GraphManager.container.appendChild(graphDiv);
        graphDiv.className = "graphDiv";
        if (graphObject.divWidth) {
            graphDiv.style.width = graphObject.divWidth + "px";
        } else {
            graphDiv.style.width = GraphManager.defaultValues.width + "px";
        }
        if (graphObject.divHeight) {
            graphDiv.style.height = graphObject.divHeight + "px";
        } else {
            graphDiv.style.height = GraphManager.defaultValues.height + "px";
        }
        GraphManager.zIndexManager.newGraph(graphDiv);

        graphDiv.addEventListener("touchstart", GraphManager._startDrag);
        graphDiv.addEventListener("mousedown", GraphManager._startDrag);

        graphDiv.graphID = graphObject.id;

        var div = graphDiv.appendChild(document.createElement('div'));
        div.className = 'modalDialog-close';
        div.innerHTML = '&#x2715;';
        div.graphID = graphDiv.graphID;

        div.addEventListener("touchstart", function (e) {
            var graphDiv = GraphManager._getGraph(e.currentTarget.graphID);
            graphDiv.graph._closeGraph();
        });
        div.addEventListener("click", function (e) {
            var graphDiv = GraphManager._getGraph(e.currentTarget.graphID);
            graphDiv.graph._closeGraph();
        });

        var div = graphDiv.appendChild(document.createElement("div"));
        div.className = "graph-title";
        div.graphID = graphDiv.graphID;

        var resizeDiv = graphDiv.appendChild(document.createElement("div"));
        resizeDiv.className = "graph-resize";
        resizeDiv.graphID = graphDiv.graphID;

        resizeDiv.addEventListener("touchstart", GraphManager._startGraphResize);
        resizeDiv.addEventListener("mousedown", GraphManager._startGraphResize);

        GraphManager.SetGraphParameters(graphObject);

        GraphManager.BuildGraph(graphObject, graphDiv);

        GraphManager.graphs.push(graphDiv);


        detailsControl._update();

        GraphManager.hiddenGraphs.push(graphDiv);

        if (!(graphObject.type == "line" || typeof graphObject.type == "undefined"))
            GraphManager.container.appendChild(graphDiv);
    },

    SetGraphParameters: function (graphObject) {
        graphObject.type = (typeof graphObject.type === 'undefined') ? GraphManager.defaultValues.type : graphObject.type;
        //graphObject.name = (typeof graphObject.name === 'undefined') ? GraphManager.defaultValues.name : graphObject.name;
        graphObject.width = (typeof graphObject.width === 'undefined') ? GraphManager.defaultValues.width : graphObject.width;
        graphObject.height = (typeof graphObject.height === 'undefined') ? GraphManager.defaultValues.height : graphObject.height;
        graphObject.x = (typeof graphObject.x === 'undefined') ? GraphManager.defaultValues.x : graphObject.x;
        graphObject.y = (typeof graphObject.y === 'undefined') ? JSON.parse(JSON.stringify(GraphManager.defaultValues.y)) : graphObject.y;
        graphObject.xScale = (typeof graphObject.xScale === 'undefined') ? GraphManager.defaultValues.xScale : graphObject.xScale;
        graphObject.yScale = (typeof graphObject.yScale === 'undefined') ? GraphManager.defaultValues.yScale : graphObject.yScale;
        graphObject.maxPoints = (typeof graphObject.maxPoints === 'undefined') ? GraphManager.defaultValues.maxPoints : graphObject.maxPoints;
        graphObject.interpolation = (typeof graphObject.interpolation === 'undefined') ? GraphManager.defaultValues.interpolation : graphObject.interpolation;
        graphObject.additive = (typeof graphObject.additive === 'undefined') ? GraphManager.defaultValues.additive : graphObject.additive;
        graphObject.xAxis = (typeof graphObject.xAxis === 'undefined') ? GraphManager.defaultValues.xAxis : graphObject.xAxis;
        graphObject.yAxis = (typeof graphObject.yAxis === 'undefined') ? GraphManager.defaultValues.yAxis : graphObject.yAxis;
        graphObject.xAxisOrient = (typeof graphObject.xAxisOrient === 'undefined') ? GraphManager.defaultValues.xAxisOrient : graphObject.xAxisOrient;
        graphObject.yAxisOrient = (typeof graphObject.yAxisOrient === 'undefined') ? GraphManager.defaultValues.yAxisOrient : graphObject.yAxisOrient;
        graphObject.holdminmax = (typeof graphObject.holdminmax === 'undefined') ? GraphManager.defaultValues.holdminmax : graphObject.holdminmax;
        graphObject.margins = (typeof graphObject.margins === 'undefined') ? GraphManager.defaultValues.margins : graphObject.margins;
        graphObject.showLabels = (typeof graphObject.showLabels === 'undefined') ? GraphManager.defaultValues.showLabels : graphObject.showLabels;
        graphObject.showLegends = (typeof graphObject.showLegends === 'undefined') ? GraphManager.defaultValues.showLegends : graphObject.showLegends;
        graphObject.clickable = (typeof graphObject.clickable === 'undefined') ? GraphManager.defaultValues.clickable : graphObject.clickable;
        return graphObject;
    },

    BuildGraph: function (graphObject, container) {
        var graph;
        if (!graphObject.type) {
            graphObject.type = graphType.line;
        }

        switch (graphObject.type) {
            case 'spider':
                GraphManager.defaultValues.minWidth = 400;
                GraphManager.defaultValues.minHeight = 400;
                graph = new SpiderChart(graphObject);
                break;

            // todo
            // # line
            // preview aanpassen
            // # Bar
            // preview aanpassen
            // grouped check
            
            // NEW
            case 'line':
                graph = new LineBottomLeft(graphObject);
                break;
            case 'bar':
                graph = new VerticalBarChart(graphObject);
                break;
            case 'newbar':
                graph = new VerticalBarChart(graphObject);
                break;
            case 'table':
                graph = new TableGraph(graphObject);
                break;

            default: console.log("Graph type not yet supported");
                break;
        }

        graph.Initialize(container);
        return graph;
    },

    BuildLineGraph: function (graphObject, container) {
        // todo: implement different axis orientations of line graphs!

        var graph = new LineBottomLeft(graphObject);
        graph.Initialize(container);
        if (typeof graphObject.data !== "undefined")
            graph.Update(graphObject.data);
        return graph;
    },

    UpdateGraphs: function (dataArray) {
        for (var i = 0; i < dataArray.length; i++) {
            var graph = GraphManager._getGraph(dataArray[i].id);
            if (graph == null) //only update graphs that exist
                continue;
            graph.graph.Update(dataArray[i].data);
        }
    },

    //generates a chart and shows it
    MakeAndShowChart: function (payload) {
        var graphObject = payload;
        graphObject.standalone = true; //flags that the chart is not a details chart
        this.MakeGraph(graphObject);
        graphObject.destroyOnClose = true; //TODO: use this as flag and destroy any chart that gets closed with that flag!
        this.ShowGraphs([graphObject.id]);
    },

    //command to make one of the excisting graphs visible
    ShowGraphs: function (dataArray) { 
        for (var i = 0; i < dataArray.length; i++) {
            var graph = GraphManager._getGraph(dataArray[i]);
            if (graph != null && !graph.graph.visible)
                graph.graph.ShowGraph();
        }
    },

    ResetGraphs: function (dataArray) {
        var graph;
        for (var i = 0; i < dataArray.length; i++)
        {
            graph = GraphManager._getGraph(dataArray[i]);
            if (graph && graph.graph.Reset)
                graph.graph.Reset();
            else
                console.log("Graph does not support Reset, id: " + dataArray[i]);
        }
    },

    RepositionGraphs: function () {
        for (var i = 0; i < GraphManager.alignedGraphs.length; i++)
            GraphManager.PositionGraph(GraphManager.alignedGraphs[i], i);
    },

    PositionGraph: function (graphDiv, i) {
        var h = GraphManager.container.clientHeight;
        var w = GraphManager.container.clientWidth;


        var defaultHeight = GraphManager.defaultValues.height;
        var defaultWidth = GraphManager.defaultValues.width;
        if (parseInt(graphDiv.style.height) > GraphManager.defaultValues.height) {
            defaultHeight = parseInt(graphDiv.style.height);
        }
        if (parseInt(graphDiv.style.width) > GraphManager.defaultValues.width) {
            defaultWidth = parseInt(graphDiv.style.width);
        }

        var amount = i;

        if (defaultHeight > h || defaultWidth > w) {
            //todo a single graph does not fit the container!
            console.log('doesn\'t fit');
            // return;
        }

        var col = 0;

        while ((amount * defaultHeight) > h) {
            col++;
            amount -= Math.floor(h / defaultHeight);
        }

        var row = amount;
        if ((amount + 1) * defaultHeight > h) //check if our new graph fits into the current column
        {
            col++;
            row = 0;
        }

        var halign, valign

        var xStart, yStart, xSign, ySign;
        switch (GraphManager.position) {
            case graphPosition.bottomLeft: halign = "left"; valign = "bottom";
                xStart = 0;
                yStart = h - GraphManager.defaultValues.height;
                xSign = 1;
                ySign = -1;
                break;
            case graphPosition.bottomRight: halign = "right"; valign = "bottom";
                xStart = w - GraphManager.defaultValues.width;
                yStart = h - GraphManager.defaultValues.height;
                xSign = -1;
                ySign = -1;
                break;
            case graphPosition.topLeft: halign = "left"; valign = "top";
                xStart = 0;
                yStart = 0;
                xSign = 1;
                ySign = 1;
                break;
            case graphPosition.topRight: halign = "right"; valign = "top";
                xStart = w - GraphManager.defaultValues.width;
                yStart = 0;
                xSign = -1;
                ySign = 1;
                break;
        }

        if (col % 2 == 0) //same side aligning
        {
            col /= 2;
            if (GraphManager.position == graphPosition.bottomLeft || GraphManager.position == graphPosition.topLeft) {
                graphDiv.style.left = col * (GraphManager.defaultValues.xOffset + GraphManager.defaultValues.graphSpacing.x) + "px";//GraphManager.defaultValues.width + "px";
            }
            else {
                graphDiv.style.left = w - (GraphManager.defaultValues.width + (col * (GraphManager.defaultValues.xOffset + GraphManager.defaultValues.graphSpacing.x))) + "px";
            }
        }
        else //other side aligning
        {
            col = (col - 1) / 2;
            if (GraphManager.position == graphPosition.bottomLeft || GraphManager.position == graphPosition.topLeft) {
                graphDiv.style.left = w - (GraphManager.defaultValues.width + (col * (GraphManager.defaultValues.xOffset + GraphManager.defaultValues.graphSpacing.x))) + "px";
            }
            else {
                graphDiv.style.left = col * (GraphManager.defaultValues.xOffset + GraphManager.defaultValues.graphSpacing.x) + "px";
            }
        }

        if (GraphManager.position == graphPosition.topLeft || GraphManager.position == graphPosition.topRight) {
            graphDiv.style.top = (row * (GraphManager.defaultValues.height + GraphManager.defaultValues.graphSpacing.y) + (GraphManager.defaultValues.yOffset * col)) + "px";
        }
        else {
            graphDiv.style.top = h - (((row + 1) * GraphManager.defaultValues.height + GraphManager.defaultValues.graphSpacing.y) + (col * GraphManager.defaultValues.yOffset)) + "px";
        }
    },

    RemoveGraph: function (graphID) {

        var graphDiv = null;
        var removed = false;
        for (var i = 0; i < GraphManager.alignedGraphs.length; i++) {
            if (GraphManager.alignedGraphs[i].graphID == graphID) {
                graphDiv = GraphManager.alignedGraphs[i];
                GraphManager.alignedGraphs.splice(i, 1);
            }
        }
        for (var i = 0; i < GraphManager.movedGraphs.length; i++) {
            if (GraphManager.movedGraphs[i].graphID == graphID) {
                graphDiv = GraphManager.movedGraphs[i];
                GraphManager.movedGraphs.splice(i, 1);
            }
        }

        if (graphDiv != null) {
            if (graphDiv.graph.graphObject.destroyOnClose) {
                graphDiv.parentNode.removeChild(graphDiv);
            }
            else {
                GraphManager.hiddenGraphs.push(graphDiv);
                graphDiv.style.visibility = "hidden";
                graphDiv.style.left = "-10000px"; //todo: look for more elegant fix!
                graphDiv.style.top = "-10000px";
            }
            GraphManager.RepositionGraphs();
        }
    },

    AddGraph: function (graphDiv) {
        //check to see if it's a hidden graph?
        for (var i = 0; i < GraphManager.hiddenGraphs.length; i++) {
            if (GraphManager.hiddenGraphs[i].graphID == graphDiv.graphID) {
                GraphManager.hiddenGraphs.splice(i, 1);
            }
        }

        //check graph is already added inside alighedGraphs
        for (var j = 0; j < GraphManager.alignedGraphs.length; j++) {
            if (GraphManager.alignedGraphs[j].graphID === graphDiv.graphID) {
                return;
            }
        }

        graphDiv.style.visibility = "visible";
        GraphManager.container.appendChild(graphDiv);
        GraphManager.alignedGraphs.push(graphDiv);
        GraphManager.PositionGraph(graphDiv, GraphManager.alignedGraphs.length - 1);
        GraphManager.zIndexManager.focus(graphDiv.graphID);
    },
    /*
    data: [
    { x: value,
      y: [value, null, value] -> graph with 3 lines  
    },
    ...
    ]
    */
    GetVar: function(aValue, aScaleType, aQnt, aUnit)
    {
        if (aScaleType == "time")
        {
            var result = new Date((aValue - 25569) * 86400 * 1000);
            result.value = result.getTime();
            result.GetDisplayValue = (function () { return this; }).bind(result);
            return result;
        }
        else
        {
            var result = new UnitConverter.ConvNum(aQnt, aValue);
            result.SetUnit(aUnit);
            return result;
        }
    },

    AddGraphData: function (graph, data) {
        //check if there are changes
        if (data == null)
            return;



        var maxX = null

        for (var x = 0; x < graph.data.length ; x++) {
            if (graph.data[x].length > 0 && (maxX == null || graph.data[x][graph.data[x].length - 1].x.value > maxX))
                maxX = graph.data[x][graph.data[x].length - 1].x.value;
        }

        for (var j = 0; j < data.length; j++) {
            //can I assume the arrays always have the same length!?
            for (var i = 0; i < graph.data.length; i++) {
                var xNum = GraphManager.GetVar(data[j].x, graph.xScale, graph.x.qnt, graph.x.unit);
                if (maxX != null && xNum.value < maxX) //check if new xValue is bigger then all the others!
                    continue;
                maxX = xNum.value;
                if (data[j].y[i] != null) {
                    if (typeof data[j].y[i] === "boolean" && !data[j].y[i]) {
                        graph.data[i].push(null);
                    }
                    else {
                        var yNum = new UnitConverter.ConvNum(graph.y[0].qnt, data[j].y[i]);
                        yNum.SetUnit(graph.y[0].unit);
                        graph.data[i].push({ x: xNum, y: yNum });
                    }
                }
            }
        }

        // todo: check if we can add them instead of rebuilding display data
        graph.displayData = [];

        for (var j = 0; j < graph.data.length; j++) {
            graph.displayData.push([]);
            for (var i = Math.max(0, graph.data[j].length - graph.maxPoints) ; i < graph.data[j].length; i++)
                graph.displayData[j].push(graph.data[j][i]);
        }
    },

    _showGraphInfo: function (e) {
        console.log("Todo: show graph info");
    },

    _getGraph: function (graphID) {
        for (var i = 0; i < GraphManager.alignedGraphs.length; i++)
            if (GraphManager.alignedGraphs[i].graphID == graphID)
                return GraphManager.alignedGraphs[i];

        for (var i = 0; i < GraphManager.movedGraphs.length; i++)
            if (GraphManager.movedGraphs[i].graphID == graphID)
                return GraphManager.movedGraphs[i];

        for (var i = 0; i < GraphManager.hiddenGraphs.length; i++)
            if (GraphManager.hiddenGraphs[i].graphID == graphID)
                return GraphManager.hiddenGraphs[i];

        return null
    },

    _startGraphResize: function (e) {
        if (!GraphManager.defaultValues.resizable)
            return;

        if (!GraphManager.dragObject || GraphManager.dragObject.resizing)
            return;

        e.preventDefault();
        e.stopPropagation();

        var graphDiv = GraphManager._getGraph(e.currentTarget.graphID);
        GraphManager.resizeObject.resizeDiv = graphDiv;
        GraphManager.resizeObject.startDivW = parseInt(graphDiv.style.width);
        GraphManager.resizeObject.startDivH = parseInt(graphDiv.style.height);

        if (typeof (e.clientX) === 'undefined') {
            GraphManager.resizeObject.startMouseX = e.changedTouches[0].clientX;
            GraphManager.resizeObject.startMouseY = e.changedTouches[0].clientY;
        } else {
            GraphManager.resizeObject.startMouseX = e.clientX;
            GraphManager.resizeObject.startMouseY = e.clientY;
        }

        graphDiv.style.opacity = "0.5";

        window.addEventListener('touchmove', GraphManager._resizeGraphMove);
        window.addEventListener('touchend', GraphManager._endGraphResize);
        window.addEventListener("mousemove", GraphManager._resizeGraphMove);
        window.addEventListener("mouseup", GraphManager._endGraphResize);

        GraphManager.dragObject.resizing = true;
    },

    _resizeGraphMove: function (e) {

        e.preventDefault();
        e.stopPropagation();
        if (typeof (e.clientX) === 'undefined') {
            var deltaX = e.changedTouches[0].clientX - GraphManager.resizeObject.startMouseX;
            var deltaY = e.changedTouches[0].clientY - GraphManager.resizeObject.startMouseY;
        } else {
            var deltaX = e.clientX - GraphManager.resizeObject.startMouseX;
            var deltaY = e.clientY - GraphManager.resizeObject.startMouseY;
        }


        var width = GraphManager.resizeObject.startDivW + deltaX;
        var height = GraphManager.resizeObject.startDivH + deltaY;

        if (GraphManager.defaultValues.maxWidth != null)
            width = Math.min(width, GraphManager.defaultValues.maxWidth);

        if (GraphManager.defaultValues.maxHeight != null)
            height = Math.min(height, GraphManager.defaultValues.maxHeight);

        width = Math.round(Math.max(GraphManager.defaultValues.minWidth, width));
        height = Math.round(Math.max(GraphManager.defaultValues.minHeight, height));

        if (Math.abs(width - GraphManager.defaultValues.width) < GraphManager.defaultValues.resizeSnapRange && Math.abs(height - GraphManager.defaultValues.height) < GraphManager.defaultValues.resizeSnapRange) {
            width = GraphManager.defaultValues.width;
            height = GraphManager.defaultValues.height;
        }

        GraphManager.resizeObject.resizeDiv.style.width = width + "px";
        GraphManager.resizeObject.resizeDiv.style.height = height + "px";

    },

    _endGraphResize: function (e) {
        e.preventDefault();
        e.stopPropagation();

        window.removeEventListener("touchmove", GraphManager._resizeGraphMove);
        window.removeEventListener("touchend", GraphManager._endGraphResize);
        window.removeEventListener("mousemove", GraphManager._resizeGraphMove);
        window.removeEventListener("mouseup", GraphManager._endGraphResize);

        GraphManager.resizeObject.resizeDiv.style.opacity = "1";
        GraphManager.resizeObject.resizeDiv.graph.Update();
        GraphManager.dragObject.resizing = false;
    },

    _startDrag: function (e) {

        if (!GraphManager.dragObject || GraphManager.dragObject.dragging)
            return;

        GraphManager.zIndexManager.focus(e.currentTarget.graphID);

        if (!GraphManager.defaultValues.draggable)
            return;

        var graphDiv = null;
        for (var i = 0; i < GraphManager.alignedGraphs.length; i++) {

            if (GraphManager.alignedGraphs[i].graphID == e.currentTarget.graphID) {
                graphDiv = GraphManager.alignedGraphs[i];
                GraphManager.alignedGraphs.splice(i, 1);
                GraphManager.dragObject.prevIndex = i;
                break;
            }
        }
        if (graphDiv == null) {
            for (var i = 0; i < GraphManager.movedGraphs.length; i++) {
                if (GraphManager.movedGraphs[i].graphID == e.currentTarget.graphID) {
                    graphDiv = GraphManager.movedGraphs[i];
                    GraphManager.movedGraphs.splice(i, 1);
                    GraphManager.dragObject.prevIndex = null;
                    break;
                }
            }
        }

        // console.log(GraphManager.dragObject.dragDiv);

        e.preventDefault();
        e.stopPropagation();

        GraphManager.dragObject.dragDiv = graphDiv;
        GraphManager.dragObject.moved = false;

        if (graphDiv.style.left != "") {
            GraphManager.dragObject.hAlign = "left";
            GraphManager.dragObject.hSign = 1;
        } else {
            GraphManager.dragObject.hAlign = "right";
            GraphManager.dragObject.hSign = -1;
        }

        if (graphDiv.style.top != "") {
            GraphManager.dragObject.vAlign = "top";
            GraphManager.dragObject.vSign = 1;
        } else {
            GraphManager.dragObject.vAlign = "bottom";
            GraphManager.dragObject.vSign = -1;
        }


        if (typeof (e.clientX) === 'undefined') {
            GraphManager.dragObject.startMouseX = e.changedTouches[0].clientX;
            GraphManager.dragObject.startMouseY = e.changedTouches[0].clientY;
        } else {
            GraphManager.dragObject.startMouseX = e.clientX;
            GraphManager.dragObject.startMouseY = e.clientY;
        }

        GraphManager.dragObject.startDivX = parseInt(graphDiv.style[GraphManager.dragObject.hAlign]);
        GraphManager.dragObject.startDivY = parseInt(graphDiv.style[GraphManager.dragObject.vAlign]);

        window.addEventListener('touchmove', GraphManager._dragMove);
        window.addEventListener('touchend', GraphManager._endDrag);
        window.addEventListener("mousemove", GraphManager._dragMove);
        window.addEventListener("mouseup", GraphManager._endDrag);

        GraphManager.dragObject.dragging = true;
    },

    _dragMove: function (e) {


        e.preventDefault();
        e.stopPropagation();

        if (typeof (e.clientX) === 'undefined') {
            var deltaX = e.touches[0].clientX - GraphManager.dragObject.startMouseX;
            var deltaY = e.touches[0].clientY - GraphManager.dragObject.startMouseY;
        } else {
            var deltaX = e.clientX - GraphManager.dragObject.startMouseX;
            var deltaY = e.clientY - GraphManager.dragObject.startMouseY;
        }

        var dragDiv = GraphManager.dragObject.dragDiv;
        var dragObject = GraphManager.dragObject;

        var newX = (GraphManager.dragObject.startDivX + (deltaX * dragObject.hSign));
        var newY = (GraphManager.dragObject.startDivY + (deltaY * dragObject.vSign));

        if (parseInt(dragDiv.style.width) != GraphManager.defaultValues.width || parseInt(dragDiv.style.height) != GraphManager.defaultValues.height) {
            //graph doesn't have default width/height
        }


        //check snapping on own start position
        if (GraphManager.defaultValues.snapping && GraphManager.dragObject.prevIndex != null) {
            if (Math.abs(newX - dragObject.startDivX) < GraphManager.defaultValues.snapRange && Math.abs(newY - dragObject.startDivY) < GraphManager.defaultValues.snapRange) {
                newX = GraphManager.dragObject.startDivX;
                newY = GraphManager.dragObject.startDivY;
            }
        }

        if (GraphManager.defaultValues.snapping) // Check snapping on other
        {
            for (var i = 0; i < GraphManager.alignedGraphs.length; i++) {
                if (Math.abs(newX - parseInt(GraphManager.alignedGraphs[i].style[dragObject.hAlign])) < GraphManager.defaultValues.snapRange && Math.abs(newY - parseInt(GraphManager.alignedGraphs[i].style[dragObject.vAlign])) < GraphManager.defaultValues.snapRange) {
                    newX = parseInt(GraphManager.alignedGraphs[i].style[dragObject.hAlign]);
                    newY = parseInt(GraphManager.alignedGraphs[i].style[dragObject.vAlign]);
                    break;
                }
            }
        }

        dragDiv.style[dragObject.hAlign] = newX + "px";
        dragDiv.style[dragObject.vAlign] = newY + "px";

    },

    _endDrag: function (e) {
        GraphManager.dragObject.dragging = false;
        e.preventDefault();
        e.stopPropagation();

        window.removeEventListener('touchmove', GraphManager._dragMove);
        window.removeEventListener('touchend', GraphManager._endDrag);
        window.removeEventListener("mousemove", GraphManager._dragMove);
        window.removeEventListener("mouseup", GraphManager._endDrag);

        if (GraphManager.defaultValues.snapping) {
            var snapped = false;

            var dragObject = GraphManager.dragObject;

            var dragDiv = dragObject.dragDiv;

            var divX = parseInt(dragDiv.style[dragObject.hAlign]);
            var divY = parseInt(dragDiv.style[dragObject.vAlign]);

            if (GraphManager.dragObject.prevIndex != null) { //check own snap
                if (divX == dragObject.startDivX && divY == dragObject.startDivY) {
                    GraphManager.alignedGraphs.splice(dragObject.prevIndex, 0, dragDiv);
                    snapped = true;
                }
            }

            if (!snapped) {
                for (var i = 0; i < GraphManager.alignedGraphs.length; i++) {
                    if (divX == parseInt(GraphManager.alignedGraphs[i].style[dragObject.hAlign]) && divY == parseInt(GraphManager.alignedGraphs[i].style[dragObject.vAlign])) {
                        var index = i;
                        if (dragObject.prevIndex != null && index >= dragObject.prevIndex)
                            index++;
                        GraphManager.alignedGraphs.splice(index, 0, dragDiv);
                        snapped = true;
                        break;
                    }
                }
            }
            if (!snapped) {
                GraphManager.movedGraphs.push(dragDiv);
            }
        }
        else {
            GraphManager.movedGraphs.push(GraphManager.dragObject.dragDiv);
        }

        GraphManager.RepositionGraphs();
    }
}
