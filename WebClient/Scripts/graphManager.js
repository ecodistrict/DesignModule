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
    scatterplot: "splot"
}

var GraphManager = {
    alignedGraphs: [],
    movedGraphs: [],
    position: null,
    container: null,
    zIndexManager: {
        baseIndex: 600,
        graphDivs: [],
        updateIndexes: function () {
            for (var i = o; i < GraphManager.zIndexManager.graphDivs.length; i++)
            {
                GraphManager.zIndexManager.graphDivs[i].style.zIndex = "" + (GraphManager.zIndexManager.baseIndex + i + 1);
            }
        },
        focus: function (graphID) {
            var graphDiv = null;
            for (var i = 0; i < GraphManager.zIndexManager.graphDivs.length; i++)
            {
                if (graphDiv != null)
                {
                    GraphManager.zIndexManager.graphDivs[i].style.zIndex = "" + (GraphManager.zIndexManager.baseIndex + i);
                    GraphManager.zIndexManager.graphDivs[i - 1] = GraphManager.zIndexManager.graphDivs[i];
                }
                else if (GraphManager.zIndexManager.graphDivs[i].graphID == graphID)
                {
                    graphDiv = GraphManager.zIndexManager.graphDivs[i];
                }
            }
            if (graphDiv != null)
            {
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
    defaultValues: {
        width: 300, //Width of a graph div todo: positioning when not using default values
        height: 200, //Height of a graph div
        description: "No Description", //Description will be showed if no description is provided
        name: "No Name", //Displayed if no name if provided
        position: graphPosition.bottomLeft, //prevered positioning of graphs
        type: graphType.line, //Default graph type
        interpolation: "linear", //https://coderwall.com/p/thtwbw/d3-js-interpolation-options
        flashBorder: false, //todo: implementation. makes border of a graph flash when received new data
        maxPoints: 10, //9007199254740991, //Graph only plots the last x points of an array
        x: {label: "" }, //Attribute name for the attribute holding the value for the x-axis
        y: [{color: "LightBlue", label: "" }], //Attribute name for the attribute holding the value of the y-axis along with color and label
        xScale: "linear", //sets the scale to use for the x axis "linear"/"ordinal"/"power"/"log" todo: time
        yScale: "linear", //sets the scale to use for the y axis "linear"/"ordinal"/"power"/"log" todo: time
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
        axisMargin: { //Margins for the axis
            x: 20,
            y: 40
        },
        graphPadding: {
            left: 10,
            right: 10,
            top: 10,
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
        xOffset: 25, //x offset when aligning graphs on top of each other
        yOffset: 25 //y offset when aligning graphs on top of each other
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

    _resize: function() {
        var w = window.innerWidth;
        var h = window.innerHeight;

        GraphManager.container.style.width = w - (GraphManager.defaultValues.leftMargin + GraphManager.defaultValues.rightMargin) + "px";
        GraphManager.container.style.height = h - (GraphManager.defaultValues.topMargin + GraphManager.defaultValues.bottomMargin) + "px";

        GraphManager.RepositionGraphs();
    },

    SetPosition: function(aPos) {
        GraphManager.position = aPos;
        GraphManager.RepositionGraphs();
    },

    MakeGraph: function (graphObject) {
        //todo remove code block when testing is not needed anymore
        if (typeof graphObject == 'undefined')
        {
            graphObject = { id: GraphManager.idcounter };
            GraphManager.idcounter++;
        }


        var g = GraphManager._getGraph(graphObject.id);
        if (g != null)
            return;

        var graphDiv = GraphManager.container.appendChild(document.createElement("div"));
        graphDiv.className = "graphDiv";
        graphDiv.style.width = GraphManager.defaultValues.width + "px";
        graphDiv.style.height = GraphManager.defaultValues.height + "px";
        GraphManager.zIndexManager.newGraph(graphDiv);
        //graphDiv.style.position = "absolute";
        //graphDiv.style.backgroundColor = "rgba(" + Math.round(Math.random() * 255) + ", " + Math.round(Math.random() * 255) + ", " + Math.round(Math.random() * 255) + ", 1)";
        graphDiv.addEventListener("mousedown", GraphManager._startDrag);
        graphDiv.graphID = graphObject.id;

        var div = graphDiv.appendChild(document.createElement('div'));
        div.className = 'modalDialog-close';
        div.innerHTML = '&#x2715;';
        div.graphID = graphDiv.graphID;
        div.addEventListener("click", function (e) { GraphManager.RemoveGraph(e.currentTarget.graphID); });

        var infoDiv = graphDiv.appendChild(document.createElement("div"));
        infoDiv.className = "graph-info";
        infoDiv.graphID = graphDiv.graphID;
        infoDiv.addEventListener("click", GraphManager._showGraphInfo);

        var resizeDiv = graphDiv.appendChild(document.createElement("div"));
        resizeDiv.className = "graph-resize";
        resizeDiv.graphID = graphDiv.graphID;
        resizeDiv.addEventListener("mousedown", GraphManager._startGraphResize);

        graphObject = GraphManager.SetGraphParameters(graphObject);

        //only for label testing!
        graphObject.x.label = "x-axis";
        graphObject.y[0].label = "y-axis";

        GraphManager.BuildGraph(graphObject, graphDiv);

        GraphManager.alignedGraphs.push(graphDiv);
        GraphManager.PositionGraph(graphDiv, GraphManager.alignedGraphs.length - 1);

        graphDiv.style.visibility = "hidden";
    },

    SetGraphParameters: function (graphObject) {
        graphObject.type = (typeof graphObject.type === 'undefined') ? GraphManager.defaultValues.type : graphObject.type;
        //graphObject.name = (typeof graphObject.name === 'undefined') ? GraphManager.defaultValues.name : graphObject.name;
        graphObject.width = (typeof graphObject.width === 'undefined') ? GraphManager.defaultValues.width : graphObject.width;
        graphObject.height = (typeof graphObject.height === 'undefined') ? GraphManager.defaultValues.height : graphObject.height;
        graphObject.x = (typeof graphObject.x === 'undefined') ? GraphManager.defaultValues.x : graphObject.x;
        graphObject.y = (typeof graphObject.y === 'undefined') ? GraphManager.defaultValues.y : graphObject.y;
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

        return graphObject;
    },

    BuildGraph: function (graphObject, container)
    {

        var width = graphObject.width;
        var height = graphObject.height;
        var marginLeft = GraphManager.defaultValues.graphPadding.left;
        var marginTop = GraphManager.defaultValues.graphPadding.top;

        if (graphObject.xAxis)
        {
            height -= GraphManager.defaultValues.axisMargin.x;
            if (graphObject.xAxisOrient == "top")
                marginTop += GraphManager.defaultValues.axisMargin.x;
        }
        if (graphObject.yAxis)
        {
            width -= GraphManager.defaultValues.axisMargin.y;
            if (graphObject.yAxisOrient == "left")
                marginLeft += GraphManager.defaultValues.axisMargin.y;
        }


        var svg = d3.select(container).append("svg")
                                            .attr("width", width)
                                            .attr("height", height);

        svg.className = "graph-svg";

        var lineG = []

        for (var i = 0; i < graphObject.y.length; i++)
            lineG.push(svg.append("g"));

        //remove: only for SSM testing!
        switch (GraphManager.idcounter)
        {
            case 0: graphObject.name = "Total GTU Distance";
                graphObject.holdminmax = false;
                GraphManager.idcounter++;
                break;
            case 1: graphObject.name = "Total GTU Travel Time";
                GraphManager.idcounter++;
                break;
            case 2: graphObject.name = "Average GTU Speed";
                GraphManager.idcounter++;
                break;
            case 3: graphObject.name = "Average GTU Travel Time";
                GraphManager.idcounter++;
                break;
            case 4: graphObject.name = "Total GTU Time Delay";
                GraphManager.idcounter++;
                break;
            case 5: graphObject.name = "Average Trip Length";
                GraphManager.idcounter++;
                break;
            case 6: graphObject.name = "Total Number Stops";
                GraphManager.idcounter++
                break;
            default: graphObject.name = "Test Graph";
        }

        var text = null;

        if (typeof graphObject.name !== "undefined")
        {
            text = svg.append("text")
                .attr("x", (width / 2))
                .attr("y", marginTop)
                .attr("dy", 10)
                .attr("text-anchor", "middle")
                .attr("pointer-events", "none")
                .attr("class", "graph-title-text")
                .style("font-size", "16px")
                .text(graphObject.name);
        }

        if (graphObject.xAxis) {
            var axisX = svg.append("g")
                                .attr("class", "axis xAxis");
                                //.attr("transform", "translate(" + 0 + ", " + (graphObject.xAxisOrient == "top" ? marginTop : height - (GraphManager.defaultValues.axisMargin.x) + ")");

            graphObject.axisX = axisX;

            if (graphObject.x.label != "")
            {
                var axisXLabel = svg.append("text")
                                    .attr("class", "axisLabel xAxisLabel")
                                    .text(graphObject.x.label);
                graphObject.axisXLabel = axisXLabel;
            }
        }
        if (graphObject.yAxis) {
            var axisY = svg.append("g")
                                .attr("class", "axis yAxis");
            graphObject.axisY = axisY;

            if (graphObject.y[0].label != "") { //todo check to see if there are different labels -> label lines instead of axis!
                var axisYLabel = svg.append("text")
                                    .attr("class", "axisLabel yAxisLabel")
                                    .text(graphObject.y[0].label);
                graphObject.axisYLabel = axisYLabel;
            }
        }


        graphObject.container = container;
        graphObject.svg = svg;
        graphObject.data = [],
        graphObject.lineG = lineG;
        graphObject.text = text;

        container.graph = graphObject;

        //GraphManager.UpdateGraph(graphObject, [{ x: 1, y: [1] }, { x: 2, y: [2] }, { x: 3, y: [3] }]);

        return graphObject;
    },

    UpdateGraphs: function (dataArray)
    {
        for (var i = 0; i < dataArray.length; i++)
        {
            var graph = GraphManager._getGraph(dataArray[i].id);
            if (graph == null) //only update graphs that exist
                continue;
            GraphManager.UpdateGraph(graph.graph, dataArray[i]);
        }
    },

    UpdateGraph: function (graph, data)
    {
        if (data != null)
        {
            graph.data.push(data);
            graph.container.style.visibility = "visible";
        }

        var width = graph.container.clientWidth;
        var height = graph.container.clientHeight;
        var marginLeft = GraphManager.defaultValues.graphPadding.left;
        var marginTop = GraphManager.defaultValues.graphPadding.top;
        var marginRight = GraphManager.defaultValues.graphPadding.right;
        var marginBottom = GraphManager.defaultValues.graphPadding.bottom;

        if (typeof graph.axisX !== "undefined")
        {
            if (graph.xAxisOrient == "top")
                marginTop += GraphManager.defaultValues.axisMargin.x;
            else
                marginBottom += GraphManager.defaultValues.axisMargin.x;
            graph.axisX.attr("transform", "translate(" + 0 + ", " + (graph.xAxisOrient == "top" ? marginTop : height - marginBottom) + ")");
        }

        if (typeof graph.axisY !== "undefined") {
            if (graph.yAxisOrient == "left")
                marginLeft += GraphManager.defaultValues.axisMargin.y;
            else
                marginRight += GraphManager.defaultValues.axisMargin.y;
            graph.axisY.attr("transform", "translate(" + (graph.yAxisOrient == "left" ? marginLeft : width - marginRight) + ", " + 0 + ")");
        }

        if (typeof graph.axisXLabel !== "undefined" && typeof graph.axisX !== "undefined")
        {
            var yTransform
            if (graph.xAxisOrient == "top")
            {
                yTransform = marginTop + 15 + GraphManager.defaultValues.axisTextPadding;
            }
            else
            {
                yTransform = height - (marginBottom + GraphManager.defaultValues.axisTextPadding);
            }

            if (typeof graph.axisY !== "undefined" && graph.yAxisOrient == "right")
            {
                graph.axisXLabel.attr("text-anchor", "start");
                graph.axisXLabel.attr("x", marginLeft);
                graph.axisXLabel.attr("y", yTransform);
                //graph.axisXLabel.attr("transform", "translate(" + marginLeft + ", " + yTransform + ")");
            }
            else
            {
                graph.axisXLabel.attr("text-anchor", "end");
                graph.axisXLabel.attr("x", (width - marginRight));
                graph.axisXLabel.attr("y", yTransform);
                //graph.axisXLabel.attr("transform", "translate(" + (width - marginRight) + ", " + yTransform + ")");
            }
        }
        if (typeof graph.axisYLabel !== "undefined" && typeof graph.axisY !== "undefined")
        {
            var xTransform
            var rotation;
            if (graph.yAxisOrient == "left") {
                xTransform = marginLeft + GraphManager.defaultValues.axisTextPadding;
                rotation = 90;
            }
            else {
                xTransform = width - (marginRight + GraphManager.defaultValues.axisTextPadding);
                rotation = 270;
            }

            if (typeof graph.axisX !== "undefined" && graph.xAxisOrient == "top") {
                if (rotation < 180) {
                    graph.axisYLabel.attr("text-anchor", "end")
                }
                else
                {
                    graph.axisYLabel.attr("text-anchor", "start");
                }
                graph.axisYLabel.attr("x", xTransform);
                graph.axisYLabel.attr("y", height - marginBottom);
                graph.axisYLabel.attr("transform", "rotate(" + rotation + "," + xTransform + "," + height - marginBottom + ")");
                
                //graph.axisYLabel.attr("transform", "translate(" + xTransform + ", " + marginTop + ")");
            }
            else {
                if (rotation < 180) {
                    graph.axisYLabel.attr("text-anchor", "start")
                }
                else {
                    graph.axisYLabel.attr("text-anchor", "end");
                }
                graph.axisYLabel.attr("x", xTransform);
                graph.axisYLabel.attr("y", marginTop);
                graph.axisYLabel.attr("transform", "rotate(" + rotation + "," + xTransform + "," + marginTop + ")");
                
                //graph.axisXLabel.attr("transform", "translate(" + xTransform + ", " + height - marginBottom + ")");
            }
        }

        graph.svg.attr("width", width);
        graph.svg.attr("height", height);

        if (graph.text != null)
            graph.text.attr("x", (width / 2))


        var displayData = [];
        for (var i = Math.max(0, graph.data.length - graph.maxPoints) ; i < graph.data.length; i++)
        {
            yData = [graph.data[i].y[0]];
            for (var j = 1; j < graph.data[i].y.length; j++)
            {
                if (graph.additive)
                    yData.push(graph.data[i].y[j] + yData[j - 1]);
                else
                    yData.push(graph.data[i].y[j]);
            }
            displayData.push({ x: graph.data[i].x, y: yData });
        }

        var minX = (typeof graph.minX === "undefined") ? d3.min(displayData, function (d) { return d.x }) : graph.minX;
        var maxX = (typeof graph.maxX === "undefined") ? d3.max(displayData, function (d) { return d.x }) : graph.maxX;
        var minY = (typeof graph.minY === "undefined") ? d3.min(displayData, function (d) { return d3.min(d.y) }) : graph.minY;
        var maxY = (typeof graph.maxY === "undefined") ? d3.max(displayData, function (d) { return d3.max(d.y) }) : graph.maxY;

        if (graph.holdminmax)
        {
            if (typeof graph.holdvalues === "undefined") //todo fix possible missed values when new data.length > maxPoints
                graph.holdvalues = {
                    //minX: minX,
                    //maxX: maxX,
                    minY: minY,
                    maxY: maxY
                }
            else
            {
            //    minX = (minX > graph.holdvalues.minX) ? graph.holdvalues.minX : minX;
            //    maxX = (maxX < graph.holdvalues.maxX) ? graph.holdvalues.maxX : maxX;
                minY = (minY > graph.holdvalues.minY) ? graph.holdvalues.minY : minY;
                maxY = (maxY < graph.holdvalues.maxY) ? graph.holdvalues.maxY : maxY;
            }
        }

        if (minX == maxX) {
            minX--;
            maxX++;
        }

        if (minY == maxY) {
            minY--;
            maxY++;
        }


        var xScale;
        var yScale;

        var width = graph.container.clientWidth;
        var height = graph.container.clientHeight;

        switch (graph.xScale) {
            case "linear": xScale = d3.scale.linear().domain([minX, maxX]).range([marginLeft, width - marginRight]);
                break;
            case "ordinal": xScale = d3.scale.ordinal().domain([minX, maxX]).range([marginLeft, width - marginRight]);
                break;
            case "power": xScale = d3.scale.power().domain([minX, maxX]).range([marginLeft, width - marginRight]);
                break;
            case "log": xScale = d3.scale.log().domain([minX, maxX]).range([marginLeft, width - marginRight]);
                break;
        }

        switch (graph.yScale)
        {
            case "linear": yScale = d3.scale.linear().domain([minY, maxY]).range([height - marginBottom, marginTop]);
                break;
            case "ordinal": yScale = d3.scale.ordinal().domain([minY, maxY]).range([height - marginBottom, marginTop]);
                break;
            case "power": yScale = d3.scale.power().domain([minY, maxY]).range([height - marginBottom, marginTop]);
                break;
            case "log": yScale = d3.scale.log().domain([minY, maxY]).range([height - marginBottom, marginTop]);
                break;
        }

        switch (graph.type)
        {
            case graphType.line: GraphManager.UpdateLineGraph(graph, displayData, xScale, yScale);
                break;
            case graphType.horizontalBar:
                break;
            case graphType.verticalBar:
                break;
            case graphType.scatterplot:
                break;
        }

    },

    UpdateLineGraph: function (graph, data, xScale, yScale)
    {
        var lineFunction = d3.svg.line()
                                    .x(function (d) { return xScale(d.x); })
                                    .y(function (d) { return yScale(d.y); })
                                    .interpolate(graph.interpolation);
        

        if (typeof graph.axisX !== "undefined") {
            var xAxis = d3.svg.axis().scale(xScale).orient(graph.xAxisOrient).ticks(5);
            graph.axisX.call(xAxis);
        }
        if (typeof graph.axisY !== "undefined") {
            var yAxis = d3.svg.axis().scale(yScale).orient(graph.yAxisOrient).ticks(5);
            graph.axisY.call(yAxis);
        }

        graph.lineG[0].selectAll("path").remove();

        
        var paths = graph.lineG[0].selectAll("path").data(data);

        paths.attr("d", lineFunction(data))
                        .attr("stroke", graph.y[0].color)
                        .attr("class", "graphLine")
                        .attr("fill", "none");

        paths.enter().append("path")
                        .attr("d", lineFunction(data))
                        .attr("stroke", graph.y[0].color)
                        .attr("class", "graphLine")
                        .attr("fill", "none");

        paths.exit().remove();

       
                                                        


    },

    RepositionGraphs: function()
    {
        for (var i = 0; i < GraphManager.alignedGraphs.length; i++)
            GraphManager.PositionGraph(GraphManager.alignedGraphs[i], i);
    },

    PositionGraph: function (graphDiv, i) {
        var h = GraphManager.container.clientHeight;
        var w = GraphManager.container.clientWidth;

        var amount = i;

        if (GraphManager.defaultValues.height > h || GraphManager.defaultValues.width > w) {
            //todo a single graph does not fit the container!
            return;
        }

        var col = 0;

        while ((amount * GraphManager.defaultValues.height) > h) {
            col++;
            amount -= Math.floor(h / GraphManager.defaultValues.height);
        }

        var row = amount;
        if ((amount + 1) * GraphManager.defaultValues.height > h) //check if our new graph fits into the current column
        {
            col++;
            row = 0;
        }

        var halign, valign

        var xStart, yStart, xSign, ySign;
        switch (GraphManager.position)
        {
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
                graphDiv.style.left = col * GraphManager.defaultValues.xOffset + "px";//GraphManager.defaultValues.width + "px";
            }
            else
            {
                graphDiv.style.left = w - (GraphManager.defaultValues.width + (col * GraphManager.defaultValues.xOffset)) + "px";
            }
        }
        else //other side aligning
        {
            col = (col - 1) / 2;
            if (GraphManager.position == graphPosition.bottomLeft || GraphManager.position == graphPosition.topLeft) {
                graphDiv.style.left = w - (GraphManager.defaultValues.width + (col * GraphManager.defaultValues.xOffset)) + "px";
            }
            else
            {
                graphDiv.style.left = col * GraphManager.defaultValues.xOffset + "px";
            }
        }

        if (GraphManager.position == graphPosition.topLeft || GraphManager.position == graphPosition.topRight) {
            graphDiv.style.top = (row * GraphManager.defaultValues.height) + (GraphManager.defaultValues.yOffset * col) + "px";
        }
        else
        {
            graphDiv.style.top = h - (((row + 1) * GraphManager.defaultValues.height) + (col * GraphManager.defaultValues.yOffset)) + "px";
        }
    },

    RemoveGraph: function (graphID) {
        var removed = false;
        for (var i = 0; i < GraphManager.alignedGraphs.length; i++)
        {
            if (GraphManager.alignedGraphs[i].graphID == graphID) {
                GraphManager.container.removeChild(GraphManager.alignedGraphs[i]);
                GraphManager.alignedGraphs.splice(i, 1);
                removed = true;
            }
        }
        for (var i = 0; i < GraphManager.movedGraphs.length; i++)
        {
            if (GraphManager.movedGraphs[i].graphID == graphID) {
                GraphManager.container.removeChild(GraphManager.movedGraphs[i]);
                GraphManager.movedGraphs.splice(i, 1);
            }
        }

        if (removed)
            GraphManager.RepositionGraphs();

        GraphManager.zIndexManager.removeGraph(graphID);
    },

    _showGraphInfo: function (e) {
        alert("Todo: show graph info");
    },

    _getGraph: function (graphID) {
        for (var i = 0; i < GraphManager.alignedGraphs.length; i++)
            if (GraphManager.alignedGraphs[i].graphID == graphID)
                return GraphManager.alignedGraphs[i];

        for (var i = 0; i < GraphManager.movedGraphs.length; i++)
            if (GraphManager.movedGraphs[i].graphID == graphID)
                return GraphManager.movedGraphs[i];

        return null
    },

    _startGraphResize: function (e) {
        if (!GraphManager.defaultValues.resizable)
            return;

        e.preventDefault();
        e.stopPropagation();

        var graphDiv = GraphManager._getGraph(e.currentTarget.graphID);
        GraphManager.resizeObject.resizeDiv = graphDiv;
        GraphManager.resizeObject.startDivW = parseInt(graphDiv.style.width);
        GraphManager.resizeObject.startDivH = parseInt(graphDiv.style.height);
        GraphManager.resizeObject.startMouseX = e.clientX;
        GraphManager.resizeObject.startMouseY = e.clientY;

        graphDiv.style.opacity = "0.5";

        window.addEventListener("mousemove", GraphManager._resizeGraphMove);
        window.addEventListener("mouseup", GraphManager._endGraphResize);
    },

    _resizeGraphMove: function (e) {
        e.preventDefault();
        e.stopPropagation();

        var deltaX = e.clientX - GraphManager.resizeObject.startMouseX;
        var deltaY = e.clientY - GraphManager.resizeObject.startMouseY;

        var width = GraphManager.resizeObject.startDivW + deltaX;
        var height = GraphManager.resizeObject.startDivH + deltaY;

        if (GraphManager.defaultValues.maxWidth != null)
            width = Math.min(width, GraphManager.defaultValues.maxWidth);
        
        if (GraphManager.defaultValues.maxHeight != null)
            height = Math.min(height, GraphManager.defaultValues.maxHeight);

        width = Math.round(Math.max(GraphManager.defaultValues.minWidth, width));
        height = Math.round(Math.max(GraphManager.defaultValues.minHeight, height));

        if (Math.abs(width - GraphManager.defaultValues.width) < GraphManager.defaultValues.resizeSnapRange && Math.abs(height - GraphManager.defaultValues.height) < GraphManager.defaultValues.resizeSnapRange)
        {
            width = GraphManager.defaultValues.width;
            height = GraphManager.defaultValues.height;
        }

        GraphManager.resizeObject.resizeDiv.style.width = width + "px";
        GraphManager.resizeObject.resizeDiv.style.height = height + "px";

    },

    _endGraphResize: function (e) {
        e.preventDefault();
        e.stopPropagation();
        window.removeEventListener("mousemove", GraphManager._resizeGraphMove);
        window.removeEventListener("mouseup", GraphManager._endGraphResize);

        GraphManager.resizeObject.resizeDiv.style.opacity = "1";
        GraphManager.UpdateGraph(GraphManager.resizeObject.resizeDiv.graph);
    },

    _startDrag: function (e) {


        GraphManager.zIndexManager.focus(e.currentTarget.graphID);

        if (!GraphManager.defaultValues.draggable)
            return;

        var graphDiv = null;
        for (var i = 0; i < GraphManager.alignedGraphs.length; i++)
        {
            if (GraphManager.alignedGraphs[i].graphID == e.currentTarget.graphID)
            {
                graphDiv = GraphManager.alignedGraphs[i];
                GraphManager.alignedGraphs.splice(i, 1);
                GraphManager.dragObject.prevIndex = i;
                break;
            }
        }
        if (graphDiv == null)
        {
            for (var i = 0; i < GraphManager.movedGraphs.length; i++)
            {
                if (GraphManager.movedGraphs[i].graphID == e.currentTarget.graphID) {
                    graphDiv = GraphManager.movedGraphs[i];
                    GraphManager.movedGraphs.splice(i, 1);
                    GraphManager.dragObject.prevIndex = null;
                    break;
                }
            }
        }

        e.preventDefault();
        e.stopPropagation();

        GraphManager.dragObject.dragDiv = graphDiv;
        GraphManager.dragObject.moved = false;

        if (graphDiv.style.left != "")
        {
            GraphManager.dragObject.hAlign = "left";
            GraphManager.dragObject.hSign = 1;
        }
        else
        {
            GraphManager.dragObject.hAlign = "right";
            GraphManager.dragObject.hSign = -1;
        }

        if (graphDiv.style.top != "")
        {
            GraphManager.dragObject.vAlign = "top";
            GraphManager.dragObject.vSign = 1;
        }
        else
        {
            GraphManager.dragObject.vAlign = "bottom";
            GraphManager.dragObject.vSign = -1;
        }


        GraphManager.dragObject.startMouseX = e.clientX;
        GraphManager.dragObject.startMouseY = e.clientY;
        GraphManager.dragObject.startDivX = parseInt(graphDiv.style[GraphManager.dragObject.hAlign]);
        GraphManager.dragObject.startDivY = parseInt(graphDiv.style[GraphManager.dragObject.vAlign]);

        window.addEventListener("mousemove", GraphManager._dragMove);
        window.addEventListener("mouseup", GraphManager._endDrag);
    },

    _dragMove: function (e) {
        e.preventDefault();
        e.stopPropagation();
        var deltaX = e.clientX - GraphManager.dragObject.startMouseX;
        var deltaY = e.clientY - GraphManager.dragObject.startMouseY;

        var dragDiv = GraphManager.dragObject.dragDiv;
        var dragObject = GraphManager.dragObject;

        var newX = (GraphManager.dragObject.startDivX + (deltaX * dragObject.hSign));
        var newY = (GraphManager.dragObject.startDivY + (deltaY * dragObject.vSign));

        if (parseInt(dragDiv.style.width) != GraphManager.defaultValues.width || parseInt(dragDiv.style.height) != GraphManager.defaultValues.height)
        {
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
            for (var i = 0; i < GraphManager.alignedGraphs.length; i++)
            {
                if (Math.abs(newX - parseInt(GraphManager.alignedGraphs[i].style[dragObject.hAlign])) < GraphManager.defaultValues.snapRange && Math.abs(newY - parseInt(GraphManager.alignedGraphs[i].style[dragObject.vAlign])) < GraphManager.defaultValues.snapRange)
                {
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
        e.preventDefault();
        e.stopPropagation();
        window.removeEventListener("mousemove", GraphManager._dragMove);
        window.removeEventListener("mouseup", GraphManager._endDrag);

        if (GraphManager.defaultValues.snapping)
        {
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

            if (!snapped)
            {
                for (var i = 0; i < GraphManager.alignedGraphs.length; i++)
                {
                    if (divX == parseInt(GraphManager.alignedGraphs[i].style[dragObject.hAlign]) && divY == parseInt(GraphManager.alignedGraphs[i].style[dragObject.vAlign]))
                    {
                        var index = i;
                        if (dragObject.prevIndex != null && index >= dragObject.prevIndex)
                            index++;
                        GraphManager.alignedGraphs.splice(index, 0, dragDiv);
                        snapped = true;
                        break;
                    }
                }
            }
            if (!snapped)
            {
                GraphManager.movedGraphs.push(dragDiv);
            }
            
        }
        else
        {
            GraphManager.movedGraphs.push(GraphManager.dragObject.dragDiv);
        }

        
        GraphManager.RepositionGraphs();
    }

}