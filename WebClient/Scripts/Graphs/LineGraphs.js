function LineBottomLeft(graphObject) {
    this.graphObject = graphObject;
    this.visible = false;
    graphObject.preview = {};
    this.graphID = graphObject.id;
    this.previewDiv = null;
    this.Initialize = function (container) {
        var width = this.graphObject.width - GraphManager.defaultValues.axisMargin.y;
        var height = this.graphObject.height - GraphManager.defaultValues.axisMargin.x;
        var marginLeft = GraphManager.defaultValues.graphPadding.left + GraphManager.defaultValues.axisMargin.y;
        var marginTop = GraphManager.defaultValues.graphPadding.top;

        var svg = d3.select(container).append("svg")
        .attr("width", width)
        .attr("height", height);

        svg.className = "graph-svg";

        var lineG = svg.append("g");

        var text = null;

        if (typeof this.graphObject.name !== "undefined") {
            text = svg.append("text")
            .attr("x", (width / 2))
            .attr("y", marginTop)
            .attr("dy", 20 - GraphManager.defaultValues.graphPadding.top)
            .attr("text-anchor", "middle")
            .attr("pointer-events", "none")
            .attr("class", "graph-title-text")
            .style("font-size", "16px")
            .text(this.graphObject.name);
        }

        //generate x axis;
        var axisX = svg.append("g")
            .attr("class", "axis xAxis");
        this.graphObject.axisX = axisX;
        if (this.graphObject.x.label != "") {
            var axisXLabel = svg.append("text")
            .attr("class", "axisLabel xAxisLabel")
            .text(this.graphObject.x.label);
            this.graphObject.axisXLabel = axisXLabel;
        }

        //generate y axis
        var axisY = svg.append("g")
            .attr("class", "axis yAxis");
        this.graphObject.axisY = axisY;
        if (this.graphObject.y[0].label != "") { //todo check to see if there are different labels -> label lines instead of axis!
            var axisYLabel = svg.append("text")
            .attr("class", "axisLabel yAxisLabel")
            .text(this.graphObject.y[0].label);
            this.graphObject.axisYLabel = axisYLabel;
        }


        this.graphObject.container = container;
        this.graphObject.svg = svg;
        this.graphObject.data = [];
        this.graphObject.displayData = [];
        for (var i = 0; i < graphObject.y.length; i++) {
            this.graphObject.data[i] = [];
            this.graphObject.displayData[i] = [];
        }
        this.graphObject.lineG = lineG;
        this.graphObject.text = text;

        container.style.visibility = "hidden";
        container.graph = this;
    }

    this._UpdatePreview = function () {

        if (this.previewDiv == null || this.graphObject.data == null || this.graphObject.data.length == 0)
            return;
        else {
            let found = false;
            for (var i = 0; i < this.graphObject.data.length; i++) {
                if (this.graphObject.data[i].length > 0)
                    found = true;
            }
            if (!found)
                return;
        }

        var graph = this.graphObject;

        var width = DataManager.detailsInfo.chartWidth;
        var height = DataManager.detailsInfo.chartHeight;

        var minX = graph.bounds.minX;
        var maxX = graph.bounds.maxX;
        var minY = graph.bounds.minY;
        var maxY = graph.bounds.maxY;

        var xScale;
        var yScale;

        //var width = graph.container.clientWidth;
        //var height = graph.container.clientHeight;

        switch (graph.xScale) {
            case "linear": xScale = d3.scale.linear().domain([minX, maxX]).range([DataManager.detailsInfo.graphMargin, width - DataManager.detailsInfo.graphMargin]);
                break;
            case "ordinal": xScale = d3.scale.ordinal().domain([minX, maxX]).range([DataManager.detailsInfo.graphMargin, width - DataManager.detailsInfo.graphMargin]);
                break;
            case "power": xScale = d3.scale.power().domain([minX, maxX]).range([DataManager.detailsInfo.graphMargin, width - DataManager.detailsInfo.graphMargin]);
                break;
            case "log": xScale = d3.scale.log().domain([minX, maxX]).range([DataManager.detailsInfo.graphMargin, width - DataManager.detailsInfo.graphMargin]);
                break;
        }

        switch (graph.yScale) {
            case "linear": yScale = d3.scale.linear().domain([minY, maxY]).range([height - DataManager.detailsInfo.graphMargin, DataManager.detailsInfo.graphMargin]);
                break;
            case "ordinal": yScale = d3.scale.ordinal().domain([minY, maxY]).range([height - DataManager.detailsInfo.graphMargin, DataManager.detailsInfo.graphMargin]);
                break;
            case "power": yScale = d3.scale.power().domain([minY, maxY]).range([height - DataManager.detailsInfo.graphMargin, DataManager.detailsInfo.graphMargin]);
                break;
            case "log": yScale = d3.scale.log().domain([minY, maxY]).range([height - DataManager.detailsInfo.graphMargin, DataManager.detailsInfo.graphMargin]);
                break;
        }

        graph.preview.lineG.selectAll("path").remove();

        var lineFunction = d3.svg.line()
            .x(function (d) { return xScale(d.x); })
            .y(function (d) { return yScale(d.y); })
            .interpolate(graph.interpolation);

        for (var i = 0; i < graph.y.length; i++) {
            

            graph.preview.lineG.append("path")
            .attr("d", lineFunction(graph.displayData[i]))
            .attr("class", "graphLine")
            .attr("stroke", graph.y[i].color)
            .attr('stroke-width', 2)
            .attr("fill", "none");
        }
    }

    this.GetPreview = function(container)
    {

        if (this.previewDiv != null) 
            return container.appendChild(this.previewDiv);


        var previewContainer = container.appendChild(document.createElement("div"));
        previewContainer.className = "detailContainer graphDetails";
        previewContainer.style.width = DataManager.detailsInfo.chartWidth + "px";
        previewContainer.style.height = DataManager.detailsInfo.elementHeight + "px";
        //if (typeof this.graphObject.description !== "undefined")
        //    previewContainer.title = this.graphObject.description;

        this.previewDiv = previewContainer;
        previewContainer.graph = this;

        previewContainer.addEventListener("click", this._clickEvent);

        var title = previewContainer.appendChild(document.createElement("h4"));
        title.className = "detailTitle graphDetailTitle";
        title.textContent = this.graphObject.name;
        title.style.width = DataManager.detailsInfo.elementWidth + "px";

        var svgContainer = previewContainer.appendChild(document.createElement("div"));
        svgContainer.className = "preview-svg-container";
        svgContainer.style.width = DataManager.detailsInfo.chartWidth + "px";
        svgContainer.style.height = DataManager.detailsInfo.chartHeight + "px";

        var svg = d3.select(svgContainer).append("svg")
            .attr("width", DataManager.detailsInfo.chartWidth)
            .attr("height", DataManager.detailsInfo.chartHeight);

        svg.className = "graph-svg-preview";

        var lineG = svg.append("g");
        lineG.className = "graph-g-preview";

        this.graphObject.preview.container = previewContainer;
        this.graphObject.preview.svg = svg;
        this.graphObject.preview.lineG = lineG;

        this._UpdatePreview();

    }
    this.Update = function (data)
    {
        var graph = this.graphObject;


        //sets data and displaydata
        GraphManager.AddGraphData(graph, data);

        var width = graph.container.clientWidth;
        var height = graph.container.clientHeight;
        var marginLeft = GraphManager.defaultValues.graphPadding.left + GraphManager.defaultValues.axisMargin.y;
        var marginTop = GraphManager.defaultValues.graphPadding.top;
        var marginRight = GraphManager.defaultValues.graphPadding.right;
        var marginBottom = GraphManager.defaultValues.graphPadding.bottom + GraphManager.defaultValues.axisMargin.x;;


        graph.axisX.attr("transform", "translate(" + 0 + ", " + (height - marginBottom) + ")");

        graph.axisY.attr("transform", "translate(" + marginLeft + ", " + 0 + ")");

        //sets the labels //todo possible to remove this from Update and add it to initialization and resizing;
        if (typeof graph.axisXLabel !== "undefined") {
            var yTransform = height - (marginBottom + GraphManager.defaultValues.axisTextPadding);

            graph.axisXLabel.attr("text-anchor", "end");
            graph.axisXLabel.attr("x", (width - marginRight));
            graph.axisXLabel.attr("y", yTransform);
        }
        if (typeof graph.axisYLabel !== "undefined") {
            var xTransform = marginLeft + GraphManager.defaultValues.axisTextPadding;
            var rotation = 90;

            graph.axisYLabel.attr("text-anchor", "start")
            graph.axisYLabel.attr("x", xTransform);
            graph.axisYLabel.attr("y", marginTop);
            graph.axisYLabel.attr("transform", "rotate(" + rotation + "," + xTransform + "," + marginTop + ")");
        }

        graph.svg.attr("width", width);
        graph.svg.attr("height", height);

        if (graph.text != null)
            graph.text.attr("x", (width / 2));

        //gets the data to display
        var displayData = graph.displayData;
        //for (var i = Math.max(0, graph.data.length - graph.maxPoints) ; i < graph.data.length; i++) {
        //    yData = [graph.data[i].y[0]];
        //    for (var j = 1; j < graph.data[i].y.length; j++) {
        //        if (graph.additive)
        //            yData.push(graph.data[i].y[j] + yData[j - 1]);
        //        else
        //            yData.push(graph.data[i].y[j]);
        //    }
        //    displayData.push({ x: graph.data[i].x, y: yData });
        //}

        //graph.displayData = displayData;

        //sets min/max values
        var minX = (typeof graph.minX === "undefined") ? d3.min(displayData, function (d) { return d3.min(d, function (p) { return p.x; }) }) : graph.minX;
        var maxX = (typeof graph.maxX === "undefined") ? d3.max(displayData, function (d) { return d3.max(d, function (p) { return p.x; }) }) : graph.maxX;
        var minY = (typeof graph.minY === "undefined") ? d3.min(displayData, function (d) { return d3.min(d, function (p) { return p.y; }) }) : graph.minY;
        var maxY = (typeof graph.maxY === "undefined") ? d3.max(displayData, function (d) { return d3.max(d, function (p) { return p.y; }) }) : graph.maxY;
        if (graph.holdminmax) {
            if (typeof graph.holdvalues === "undefined") //todo fix possible missed values when new data.length > maxPoints
                graph.holdvalues = {
                    minY: minY,
                    maxY: maxY
                }
            else {
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


        graph.bounds = {
            minX: minX,
            maxX: maxX,
            minY: minY,
            maxY: maxY
        }

        var xScale;
        var yScale;

        //var width = graph.container.clientWidth;
        //var height = graph.container.clientHeight;

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

        switch (graph.yScale) {
            case "linear": yScale = d3.scale.linear().domain([minY, maxY]).range([height - marginBottom, marginTop]);
                break;
            case "ordinal": yScale = d3.scale.ordinal().domain([minY, maxY]).range([height - marginBottom, marginTop]);
                break;
            case "power": yScale = d3.scale.power().domain([minY, maxY]).range([height - marginBottom, marginTop]);
                break;
            case "log": yScale = d3.scale.log().domain([minY, maxY]).range([height - marginBottom, marginTop]);
                break;
        }

        var xAxis = d3.svg.axis().scale(xScale).orient(graph.xAxisOrient).ticks(5);
        graph.axisX.call(xAxis);


        var yAxis = d3.svg.axis().scale(yScale).orient(graph.yAxisOrient).ticks(5);
        graph.axisY.call(yAxis);

        graph.lineG.selectAll("path").remove();

        var lineFunction = d3.svg.line()
            .x(function (d) {
                return xScale(d.x);
            })
            .y(function (d) {
                return yScale(d.y);
            })
            .interpolate(graph.interpolation);

        for (var i = 0; i < graph.y.length; i++) {
            
            graph.lineG.append("path")
            .attr("d", lineFunction(displayData[i]))
            .attr("class", "graphLine")
            .attr("stroke", graph.y[i].color)
            .attr("stroke-width", 3)
            .attr("fill", "none");
        }

        this._UpdatePreview();
    }

    this._clickEvent = function (e) {
        var graph = e.currentTarget.graph;

        if (graph.visible)
        {
            graph._closeGraph();
        }
        else
        {
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
}