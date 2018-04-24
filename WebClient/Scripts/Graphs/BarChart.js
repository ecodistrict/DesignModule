//function GenerateRandomData() {
//    var data = [];

//    var categoryCount = Math.floor((Math.random() * 12) + 2);

//    var barCount = Math.floor((Math.random() * 3) + 2);

//    var barCounts = [];

//    for (var i = 0; i < barCount; i++)
//        barCounts.push(Math.floor((Math.random() * 4) + 1));

//    for (var i = 0; i < categoryCount; i++) {
//        var category = {
//            title: "Area" + (i + 1),
//            data: []
//        }

//        var seriesCount = 0;

//        for (var j = 0; j < barCount; j++) {

//            var barData = {
//                leftAxis: true,
//                data: []
//            }
//            var total = 0;
//            for (var h = 0; h < barCounts[j]; h++) {
//                var value = Math.floor((Math.random() * 30));
//                barData.data.push({ start: total, value: value, series: (seriesCount + " Series") });
//                seriesCount++;
//                total += value;
//            }

//            category.data.push(barData);
//        }

//        data.push(category);
//    }

//    return data;
//}

//function GenerateBarCharts(amount)
//{
//    var domainName = "BarTest";
//    var domains = {};
//    var domain = { enabled: true, charts: [], layers: [], kpis: [] };
//    domains[domainName] = domain;
//    for (var i = 0; i < amount; i++)
//    {
//        var graphObject = {
//            id: "BarChart" + i,
//            title: "Een grafiekje",
//            barMargin: 0.15,
//            categoryMargin: 1,
//            xAxisMargin: 30,
//            yAxisMargin: 30,
//            type: "newbar",
//            data: GenerateRandomData()
//        };
//        domain.charts.push(graphObject);
//    }

//    wsLookup["domains"](domains);
//}

//function GenerateNewBarChart(index) {
//    var graphObject = {
//        id: "BarChart" + index ? index : parseInt(Math.random() * 10000),
//        title: "Een grafiekje",
//        barMargin: 0.15,
//        categoryMargin: 1,
//        width: 400,
//        height: 300,
//        xAxisMargin: 30,
//        yAxisMargin: 30,
//        type: "newbar",
//        data: GenerateRandomData()
//    };
//    GraphManager.MakeGraph(graphObject);
//    //var chart = new BarChart(graphObject);
//    //chart.Initialize(container);
//    //return chart;
//}

VerticalBarChart = function (aGraphObject) {
    //graphObject.valueColors = {
    //    type: "discrete",
    //    entries: [{ min: 0, max: 20, color: "#FF0000" }, { min: 20, max: 50, color: "#FE642E" }, { min: 50, max: 70, color: "#F7FE2E" }, { min: 70, max: 100, color: "#00FF00" }],
    //    //entries: [{ value: 0, color: "#FF0000" }, { value: 8, color: "#FE642E" }, { value: 17, color: "#F7FE2E" }, { value: 25, color: "#00FF00" }],
    //    minColor: "#FF0000",
    //    maxColor: "#00FF00",
    //    defaultColor: "#666666"
    //}
    //parameters
    this.graphObject = aGraphObject;
    this.visible = false;
    this.graphObject.preview = {};
    this.graphID = this.graphObject.id;
    this.previewDiv = null;
    this.colors = {};
    //this.refColors = {};
    this.setColors = this.graphObject.colors ? this.graphObject.colors : {};
    this.seriesNames = [],
    this.dc20 = d3.scale.category20();
    this.labelDiv = null;
    this.labelVisible = true;
    this.converted = false;
    this.titleMargin = 30;
    this.valueColors = this.graphObject.valueColors;

    //public functions
    this.Initialize = function (container) {
        

        container.graph = this;

        this.container = container;

        container.style.width = this.graphObject.width + "px";
        container.style.height = this.graphObject.height + "px";

        var svg = this.svg = d3.select(container).append("svg")
            .attr("width", this.graphObject.width)
            .attr("height", this.graphObject.height);
        svg.className = "graph-svg";

        this.graphGroup = svg.append("g")
			.attr("class", "graph-g")
            .attr("transform", "translate(" + GraphManager.defaultValues.graphPadding.left + ", " + GraphManager.defaultValues.graphPadding.top + ")");

        this.dataGroup = this.graphGroup.append("g")
            .attr("class", "graph-data-g")
            .attr("transform", "translate(" + this.graphObject.yAxisMargin + ", 0)");

        this.xAxisGroup = this.graphGroup.append("g")
			.attr("class", "axis xAxis");

        this.yLeftAxisGroup = this.graphGroup.append("g")
			.attr("class", "axis yAxis");

        if (typeof this.graphObject.title !== "undefined") {
            this.titleStroke = svg.append("text")
                .attr("x", (this.graphObject.width / 2))
				.attr("y", 20)
				.attr("dy", 3)
				.attr("text-anchor", "middle")
				.attr("pointer-events", "none")
				.attr("class", "graph-title-text")
				.style("font-size", "16px")
				.style("stroke", "rgba(255, 255, 255, 0.6)")
				.style("stroke-width", "3px")
                .text(this.graphObject.title)
                .call(this._wrapTitleLetters, this.graphObject.width - this.titleMargin);
                

            this.titleText = svg.append("text")
                .attr("x", (this.graphObject.width / 2))
				.attr("y", 20)
				.attr("dy", 3)
				.attr("text-anchor", "middle")
				.attr("pointer-events", "none")
				.attr("class", "graph-title-text")
				.style("font-size", "16px")
                .text(this.graphObject.title)
                .call(this._wrapTitleLetters, this.graphObject.width - this.titleMargin);
        }

        var labelDiv = this.labelDiv = container.appendChild(document.createElement("div"));
        labelDiv.className = "bar-label-div";
        labelDiv.style.left = (this.graphObject.width + 5) + "px";
        labelDiv.style.top = "0px";

        this._updateLabels();

        if (this.converted) {
            this.graphObject.data = this._convertOldData(this.graphObject.data);
            if (this.graphObject.ref)
                this.graphObject.ref.data = this._convertOldData(this.graphObject.ref.data);
        }

        // check if ref data should be converted
        
        this.Update();

        container.style.visibility = "hidden";
    };

    this.Reset = function () {
        this.graphObject.data = [];
        this.Update();
    };

    this.GetPreview = function (container) {
        if (this.previewDiv != null)
            return container.appendChild(this.previewDiv);

        var previewDiv = this.previewDiv = document.createElement("div");
        previewDiv.className = "detailContainer graphDetails";
        previewDiv.style.width = DataManager.detailsInfo.chartWidth + "px";
        previewDiv.graph = this;
        previewDiv.addEventListener("click", this._clickEvent);

        var title = previewDiv.appendChild(document.createElement("h4"));
        title.className = "detailTitle graphDetailTitle";
        title.textContent = this.graphObject.title;
        title.style.width = DataManager.detailsInfo.elementWidth + "px";

        var svgContainer = previewDiv.appendChild(document.createElement("div"));
        svgContainer.className = "preview-svg-container";
        svgContainer.style.width = DataManager.detailsInfo.chartWidth + "px";
        svgContainer.style.height = DataManager.detailsInfo.chartHeight + "px";

        var svg = d3.select(svgContainer).append("svg")
			.attr("width", DataManager.detailsInfo.chartWidth)
			.attr("height", DataManager.detailsInfo.chartHeight);
        svg.className = "graph-svg-preview";

        var graphGroup = svg.append("g")
        graphGroup.className = "graph-g-preview";

        this.preview = {
            svg: svg,
            graphGroup: graphGroup,
            container: previewDiv
        };

        this._updatePreview();

        container.appendChild(previewDiv);

        return previewDiv;
    };

    this.ReInit = function (aGraphObject) {
        // re-do init stuff
        this.graphObject = aGraphObject;
        if (this.graphObject.axis)
            this._convertFromOldBar();
        // check if ref data should be converted
        if (this.converted) {
            this.graphObject.data = this._convertOldData(this.graphObject.data);
            if (this.graphObject.ref)
                this.graphObject.ref.data = this._convertOldData(this.graphObject.ref.data);
        }
        this.Update();
    };

    this.Update = function (data) {
        if (typeof data != "undefined") {
            // new data, check if needs to be converted
            if (this.converted) 
                data = this._convertOldData(data);
            // store new (converted) data
            this.graphObject.data = data;
        }

        // use existing data but create deep copy
        data = JSON.parse(JSON.stringify(this.graphObject.data)); 

        if (this.graphObject.ref && this.graphObject.ref.data) {
            rdata = this.graphObject.ref.data;
            // merge this.refData into data
            for (var d1 = 0; d1 < data.length; d1++) {
                for (var r1 = 0; r1 < rdata.length; r1++) {
                    if (rdata[r1].title === data[d1].title) {
                        for (var r2 = rdata[r1].data.length - 1; r2 >= 0; r2--) {
                            var rdata_copy = JSON.parse(JSON.stringify(rdata[r1].data[r2]));
                            data[d1].data.splice(r2 + 1, 0, rdata_copy); // insert at position after current data ie zip sets together
                            for (var _d3 = 0; _d3 < data[d1].data[r2 + 1].data.length; _d3++)
                                data[d1].data[r2 + 1].data[_d3].series += "-ref";
                        }
                        break;
                    }
                }
            }
        }

        var categoryWidth = this.categoryWidth = data.length > 0 ? this._getCategoryWidth(data[0].data) : 0; //every category needs to be the same width
        var barWidth = this.barWidth = 1 + this.graphObject.barMargin;
        var categoryMargin = this.categoryMargin = this.graphObject.categoryMargin;
        var barMargin = this.barMargin = this.graphObject.barMargin;

        var axisCount = this.axisCount = this._getXAxisAmount(data); //rewrite to use series?

        var divWidth = parseFloat(this.container.style.width);
        var divHeight = parseFloat(this.container.style.height);

        var graphWidth = divWidth - (GraphManager.defaultValues.graphPadding.left + GraphManager.defaultValues.graphPadding.right);
        var graphHeight = divHeight - (GraphManager.defaultValues.graphPadding.top + GraphManager.defaultValues.graphPadding.bottom);

        var dataWidth = graphWidth - (axisCount * this.graphObject.yAxisMargin);
        var dataHeight = graphHeight - this.graphObject.xAxisMargin;

        var maxY = this.maxY = this._getMaxY(data);
        var minY = this.minY = this._getMinY(data);

        var yScale = d3.scale.linear()
			.range([dataHeight, 0])
			.domain([minY, maxY]);

        //var yNormalScale = d3.scale.linear()
		//	.range([0, dataHeight])
		//	.domain([minY, maxY]);

        var barsWidth = this.barsWidth = this._getWidthInBars(data);

        var xScale = d3.scale.linear()
			.range([0, dataWidth])
			.domain([0, barsWidth]);

        var barPixelWidth = xScale(1);

        this.svg
            .attr("width", divWidth)
            .attr("height", divHeight);

        if (typeof this.graphObject.title !== "undefined") {
            this.titleStroke
				.attr("x", (divWidth / 2))
                .text(this.graphObject.title)
                .call(this._wrapTitleLetters, divWidth - this.titleMargin);

            this.titleText
				.attr("x", (divWidth / 2))
                .text(this.graphObject.title)
                .call(this._wrapTitleLetters, divWidth - this.titleMargin);
        }

        this.graphGroup
			.attr("width", graphWidth)
			.attr("height", graphHeight);

        this.dataGroup
            .attr("width", dataWidth)
            .attr("height", dataHeight);


        //Generate left y-axis
        var yLeftAxis = d3.svg.axis().scale(yScale).orient("left").ticks(5);

        this.yLeftAxisGroup
            .attr("width", this.graphObject.yAxisMargin)
			.attr("height", dataHeight)
            .attr("transform", "translate(" + this.graphObject.yAxisMargin + ", 0)");

        this.yLeftAxisGroup.call(yLeftAxis);

        //generate and position x-axis
        this.xAxisGroup
			.attr("width", dataWidth)
            .attr("height", this.graphObject.xAxisMargin)
            .attr("transform", "translate(" + this.graphObject.yAxisMargin + "," + dataHeight + ")");

        var label = this.xAxisGroup.selectAll("text")
			.data(data)
			.attr("x", function (d, i) { return xScale((i + 0.5) * categoryWidth); })
            .attr("y", 0)
			.attr("dy", "1.1em")
			.attr("text-anchor", "middle")
			.attr("pointer-events", "none")
			.attr("class", "graph-title-text")
			.style("font-size", "12px")
			.text(function (d) { return d.title })
            .call(this._wrapLetters, xScale(categoryWidth) - 5);

        label.enter().append("text")
			.attr("x", function (d, i) { return xScale((i + 0.5) * categoryWidth); })
            .attr("y", 0)
			.attr("dy", "1.1em")
			.attr("text-anchor", "middle")
			.attr("pointer-events", "none")
			.attr("class", "graph-title-text")
			.style("font-size", "12px")
			.text(function (d) { return d.title })
            .call(this._wrapLetters, xScale(categoryWidth) - 5);

        label.exit().remove();

        //generate bars	
        var column = this.dataGroup.selectAll(".barColumn")
		  .data(data)
		  .attr("transform", function (d, i) { return "translate(" + xScale(((i * categoryWidth) + (0.5 * categoryMargin))) + ")"; });

        column.enter().append("g")
		  .attr("class", "barColumn")
		  .attr("transform", function (d, i) { return "translate(" + xScale(((i * categoryWidth) + (0.5 * categoryMargin))) + ")"; });

        column.exit().remove();


        var bar = column.selectAll("g")
			.data(function (d) { return d.data; });

        bar.exit()
			.remove();

        bar.attr("transform", function (d, i) { return "translate(" + xScale((i * barWidth)) + ")"; });

        bar.enter().append("g")
			.attr("transform", function (d, i) { return "translate(" + xScale((i * barWidth)) + ")"; })

        var getColor = this._getColor.bind(this);

        var rect = bar.selectAll("rect")
			.data(function (d) { return d.data; });

        rect.exit()
			.remove();

        //rect.attr("y", function (d) { return yScale(d.start + d.value); })
		//	.attr("height", function (d) { return yNormalScale(d.value); })
		//	.attr("width", barPixelWidth)
		//	.attr("fill", function (d) { return seriesToColor(d.series); });

        //rect.enter().append("rect")
		//	.attr("y", function (d) { return yScale(d.start + d.value); })
		//	.attr("height", function (d) { return yNormalScale(d.value); })
		//	.attr("width", barPixelWidth)
		//	.attr("fill", function (d) { return seriesToColor(d.series); });

        rect.attr("y", function (d) { return Math.min(yScale(d.start + d.value), yScale(d.start)); })
			.attr("height", function (d) { return Math.abs(yScale(d.start + d.value) - yScale(d.start)); })
			.attr("width", barPixelWidth)
			.attr("fill", function (d) { return getColor(d.series, d.start + d.value); });

        rect.enter().append("rect")
			.attr("y", function (d) { return Math.min(yScale(d.start + d.value), yScale(d.start)); })
			.attr("height", function (d) { return Math.abs(yScale(d.start + d.value) - yScale(d.start)); })
			.attr("width", barPixelWidth)
			.attr("fill", function (d) { return getColor(d.series, d.start + d.value); });

        this.labelDiv.style.left = (divWidth + 5) + "px";
        if ((this.seriesNames.length > 1 || typeof this.valueColors != "undefined") && this.labelVisible)
            this.labelDiv.style.visibility = "inherit";
        else
            this.labelDiv.style.visibility = "hidden";
    };

    this._closeGraph = function () {
        this.HideGraph();
    }

    this._openGraph = function () {
        this.ShowGraph();
    }

    this.ShowGraph = function () {
        this.visible = true;
        this._resetSize();
        GraphManager.AddGraph(this.container);
        if (this.previewDiv)
            L.DomUtil.addClass(this.previewDiv, "chartPreviewActive");
    };

    this.HideGraph = function () {
        this.visible = false;
        GraphManager.RemoveGraph(this.graphID)
        if (this.previewDiv)
            L.DomUtil.removeClass(this.previewDiv, "chartPreviewActive");
    };

    //private functions

    this._updatePreview = function () {
        if (this.previewDiv == null || typeof this.categoryWidth == "undefined")
            return;

        var data = this.graphObject.data;
        var width = DataManager.detailsInfo.chartWidth;
        var height = DataManager.detailsInfo.chartHeight;
        var margin = DataManager.detailsInfo.graphMargin;
        var categoryWidth = this.categoryWidth;
        var categoryMargin = this.categoryMargin;
        var barWidth = this.barWidth;
        var graphWidth = width - (2 * margin);
        var graphHeight = height - (2 * margin);

        var yScale = d3.scale.linear()
			.range([graphHeight, 0])
			.domain([this.minY, this.maxY]);

        //var yNormalScale = d3.scale.linear()
		//	.range([0, graphHeight])
		//	.domain([this.minY, this.maxY]);

        var xScale = d3.scale.linear()
			.range([0, graphWidth])
			.domain([0, this.barsWidth]);

        var barPixelWidth = xScale(1);

        this.preview.graphGroup
			.attr("width", graphWidth)
			.attr("height", graphHeight)
            .attr("transform", "translate(" + margin + "," + margin + ")");

        var column = this.preview.graphGroup.selectAll(".barColumn")
		  .data(data)
		  .attr("transform", function (d, i) { return "translate(" + xScale(((i * categoryWidth) + (0.5 * categoryMargin))) + ")"; });

        column.enter().append("g")
		  .attr("class", "barColumn")
		  .attr("transform", function (d, i) { return "translate(" + xScale(((i * categoryWidth) + (0.5 * categoryMargin))) + ")"; });

        column.exit().remove();


        var bar = column.selectAll("g")
			.data(function (d) { return d.data; });

        bar.exit()
			.remove();

        bar.attr("transform", function (d, i) { return "translate(" + xScale((i * barWidth)) + ")"; });

        bar.enter().append("g")
			.attr("transform", function (d, i) { return "translate(" + xScale((i * barWidth)) + ")"; })

        var getColor = this._getColor.bind(this);

        var rect = bar.selectAll("rect")
			.data(function (d) { return d.data; });

        rect.exit()
			.remove();

        rect.attr("y", function (d) { return Math.min(yScale(d.start + d.value), yScale(d.start)); })
			.attr("height", function (d) { return Math.abs(yScale(d.start + d.value) - yScale(d.start)); })
			.attr("width", barPixelWidth)
			.attr("fill", function (d) { return getColor(d.series, d.start + d.value); });

        rect.enter().append("rect")
			.attr("y", function (d) { return Math.min(yScale(d.start + d.value), yScale(d.start)); })
			.attr("height", function (d) { return Math.abs(yScale(d.start + d.value) - yScale(d.start)); })
			.attr("width", barPixelWidth)
			.attr("fill", function (d) { return getColor(d.series, d.start + d.value); });
    }

    this._getWidthInBars = function (categories) {
        if (categories.length > 0)
            return (this._getCategoryWidth(categories[0].data) * categories.length);
        return this.graphObject.categoryMargin;
    }

    this._getCategoryWidth = function (bars) {
        return this.graphObject.categoryMargin + (bars.length * (1 + this.graphObject.barMargin)) - (bars.length > 0 ? this.graphObject.barMargin : 0);
    }

    this._getMaxY = function (data) {
        return d3.max(data, function (d) { return d3.max(d.data, function (d) { return d3.max(d.data, function (d) { return Math.max(d.start + d.value, d.start); }); }); });
    }

    this._getMinY = function (data) {
        return d3.min(data, function (d) { return d3.min(d.data, function (d) { return d3.min(d.data, function (d) { return Math.min(d.start + d.value, d.start); }); }); });
    }
    /*
    this._seriesCount = function (data) {
        if (data.length == 0)
            return 0;
        return d3.max(data, function (d) { return d.data.length == 0 ? 0 : d3.sum(d.data, function (d) { return d.data.length; }); });
    }
    */
    this._getXAxisAmount = function (data) {
        for (var i = 0; i < data.length; i++)
            for (var j = 0; j < data[i].data.length; j++)
                if (!data[i].data[j].leftAxis)
                    return 2;
        return 1;
    }

    this._seriesToColor = function (series) {
        if (typeof this.colors[series] == "undefined") {
            if (typeof this.setColors[series] != "undefined")
            {
                this.colors[series] = this.setColors[series];
                this.colors[series+"-ref"] = chroma(this.setColors[series]).brighten(2).desaturate(0.5).hex(); // calculate lighter version of color
            }
            else
            {
                this.colors[series] = this.dc20(series);
                this.colors[series+"-ref"] = this.dc20(series+"-ref");
            }
            this.seriesNames.push(series);
            this._updateLabels();
        }
        return this.colors[series];
    }

    this._getColor = function (series, value) {
        if (this.valueColors) 
            return this._valueToColor(value);
        return this._seriesToColor(series);
    }

    this._valueToColor = function (value) {
        if (this.valueColors.type == "discrete")
        {
            for (var i = 0; i < this.valueColors.entries.length; i++)
                if (value >= this.valueColors.entries[i].min && value <= this.valueColors.entries[i].max)
                    return this.valueColors.entries[i].color;
            return this.valueColors.defaultColor;
        }
        else //ramp
        {
            if (typeof this.colorInterpolations == "undefined")
            {
                this.colorInterpolations = [];
                for (var i = 0; i < this.valueColors.entries.length - 1; i++)
                {
                    var value1 = this.valueColors.entries[i].value;
                    var value2 = this.valueColors.entries[i + 1].value;
                    var color1 = this.valueColors.entries[i].color;
                    var color2 = this.valueColors.entries[i + 1].color;

                    this.colorInterpolations.push({ min: value1, max: value2, interpolation: d3.scale.linear().domain([value1, value2]).interpolate(d3.interpolateRgb).range([d3.rgb(color1), d3.rgb(color2)]) });
                }
            }
            if (this.colorInterpolations.length == 0)
                return this.valueColors.defaultColor;

            if (value < this.colorInterpolations[0].min)
                return this.valueColors.minColor ? this.valueColors.minColor : this.valueColors.defaultColor;

            for (var i = 0; i < this.colorInterpolations.length; i++)
                if (value >= this.colorInterpolations[i].min && value <= this.colorInterpolations[i].max)
                    return this.colorInterpolations[i].interpolation(value);
            
            return this.valueColors.maxColor ? this.valueColors.maxColor : this.valueColors.defaultColor;
        }
    }

    this._clickEvent = (function (e) {

        if (this.visible) {
            this._closeGraph();
        }
        else {
            this._openGraph();
        }
    }).bind(this);

    this._resetSize = function () {
        var changed = false
        if (parseInt(this.container.style.width) != this.graphObject.width) {
            this.container.style.width = this.graphObject.width + "px";
            changed = true;
        }
        if (parseInt(this.container.style.height) != this.graphObject.height) {
            this.container.style.height = this.graphObject.height + "px";
            changed = true;
        }

        if (changed)
            this.Update();
    }

    this._showLabels = function () {
        this.labelDiv.visibility = "visible";
    }

    this._hideLabels = function () {
        this.labelDiv.visiblilty = "hidden";
    }

    this._updateLabels = function () {
        this.labelDiv.innerHTML = "";
        if (this.valueColors)
        {
            if (this.valueColors.type == "discrete") {
                var table = this.labelDiv.appendChild(document.createElement("table"));
                table.className = "bar-label-table";
                for (var i = 0; i < this.valueColors.entries.length; i++) {
                    var row = table.appendChild(document.createElement("tr"));
                    row.className = "bar-label-row";

                    var colorField = row.appendChild(document.createElement("td"));
                    colorField.className = "bar-label-color-td";
                    colorField.style.backgroundColor = this.valueColors.entries[i].color;

                    var textField = row.appendChild(document.createElement("td"));
                    textField.className = "bar-label-text-td";
                    textField.innerHTML = this.valueColors.entries[i].min + " - " + this.valueColors.entries[i].max;
                }
            }
            else //ramp
            {
                var table = this.labelDiv.appendChild(document.createElement("table"));
                table.className = "bar-label-table";
                for (var i = 0; i < this.valueColors.entries.length; i++) {
                    var row = table.appendChild(document.createElement("tr"));
                    row.className = "bar-label-row";

                    var colorField = row.appendChild(document.createElement("td"));
                    colorField.className = "bar-label-color-td";
                    colorField.style.backgroundColor = this.valueColors.entries[i].color;

                    var textField = row.appendChild(document.createElement("td"));
                    textField.className = "bar-label-text-td";
                    textField.innerHTML = this.valueColors.entries[i].value;
                }
            }
        }
        else if (this.seriesNames.length > 0)
        {
            var table = this.labelDiv.appendChild(document.createElement("table"));
            table.className = "bar-label-table";
            for (var i = 0; i < this.seriesNames.length; i++) {
                var name = this.seriesNames[i];
                //if (name.substr(name.length - 4)!=="-ref") {
                var row = table.appendChild(document.createElement("tr"));
                row.className = "bar-label-row";

                var colorField = row.appendChild(document.createElement("td"));
                colorField.className = "bar-label-color-td";
                colorField.style.backgroundColor = this._seriesToColor(this.seriesNames[i]);

                var textField = row.appendChild(document.createElement("td"));
                textField.className = "bar-label-text-td";
                textField.innerHTML = this.seriesNames[i];
                //}
            }
        }
        else
        {
            //todo: display no series?
        }
    }

    //copied and adjusted from: https://bl.ocks.org/mbostock/7555321
    //not used for now. todo: check graph loading speed when enabled
    this._wrap = function (text, width) {
        text.each(function () {
            var text = d3.select(this),
                words = text.text().split(/\s+/).reverse(),
                word,
                line = [],
                lineNumber = 0,
                lineHeight = 1.1, // ems
                y = text.attr("y"),
                x = text.attr("x"),
                dy = parseFloat(text.attr("dy"));
            tspan = text.text(null).append("tspan").attr("x", x).attr("y", y).attr("dy", dy + "em");
            while (word = words.pop()) {
                line.push(word);
                tspan.text(line.join(" "));
                if (tspan.node().getComputedTextLength() > width && line.length > 1) {
                    line.pop();
                    tspan.text(line.join(" "));
                    line = [word];
                    tspan = text.append("tspan").attr("x", x).attr("y", y).attr("dy", ++lineNumber * lineHeight + dy + "em").text(word);
                }
            }
        });
    }

    this._wrapLetters = function (text, width) {
        text.each(function () {
            var text = d3.select(this),
                letters = text.text(),
                letter,
                done = false,
                line = "",
                lineNumber = 0,
                lineHeight = 1.1, // ems
                font = window.getComputedStyle(this, null).getPropertyValue('font-family'),
                fontSize = window.getComputedStyle(this, null).getPropertyValue('font-size')
                canvas = document.createElement("canvas"),
                context = canvas.getContext("2d"),
                y = text.attr("y"),
                x = text.attr("x"),
                dy = parseFloat(text.attr("dy"));
            context.font = fontSize + " " + font;
            tspan = text.text(null).append("tspan").attr("x", x).attr("y", y).attr("dy", dy + "em");
            while (lineNumber < 2 && letters.length > 0) {
                letter = letters[0];
                if (context.measureText(line + letter).width > width /*&& line.length > 1*/) {
                    tspan.text(line);
                    if (lineNumber < 1) {
                        line = "";
                        tspan = text.append("tspan").attr("x", x).attr("y", y).attr("dy", ++lineNumber * lineHeight + dy + "em").text(line);
                    }
                    else
                        return;
                }
                else
                {
                    line = line + letter;
                    letters = letters.substr(1);
                    tspan.text(line);
                }
            }
        });
    }

    this._wrapTitleLetters = function (text, width) {
        text.each(function () {
            var text = d3.select(this),
                letters = text.text(),
                letter,
                done = false,
                line = "",
                lineNumber = 0,
                lineHeight = 1.1, // ems
                font = window.getComputedStyle(this, null).getPropertyValue('font-family'),
                fontSize = window.getComputedStyle(this, null).getPropertyValue('font-size')
            canvas = document.createElement("canvas"),
            context = canvas.getContext("2d"),
            y = text.attr("y"),
            x = text.attr("x"),
            dy = parseFloat(text.attr("dy"));
            context.font = fontSize + " " + font;
            tspan = text.text(null).append("tspan").attr("x", x).attr("y", y);
            while (lineNumber < 2 && letters.length > 0) {
                letter = letters[0];
                if (context.measureText(line + letter).width > width /*&& line.length > 1*/) {
                    tspan.text(line);
                    if (lineNumber < 1) {
                        line = "";
                        tspan = text.append("tspan").attr("x", x).attr("y", y).attr("dy", ++lineNumber * lineHeight + "em").text(line);
                    }
                    else
                        return;
                }
                else {
                    line = line + letter;
                    letters = letters.substr(1);
                    tspan.text(line);
                }
            }
        });
    }

    this._convertFromOldBar = function ()
    {
        //barMargin: 0.15,
        //            categoryMargin: 1,
        //            xAxisMargin: 30,
        //            yAxisMargin: 30,
        this.graphObject.barMargin = 0.15;
        this.graphObject.categoryMargin = 1;
        this.graphObject.xAxisMargin = 30;
        this.graphObject.yAxisMargin = 30;

        this.converted = true;
    }

    this._convertOldData = function (data)
    {
        var newData = [];

        if (data.columns.length == 0)
            return newData;

        var categoryCount = data.columns[0].length - 1;

        for (var i = 0; i < categoryCount; i++)
        {
            newData.push({ title: data.columns[0][i+1], data: [] });
        }


        var stackbar = {};

        var getStackGroup = function (series) {
            if (typeof data.groups == "undefined")
                return -1;
            for (var i = 0; i < data.groups.length; i++)
                for (var j = 0; j < data.groups[i].length; j++)
                    if (data.groups[i][j] == series)
                        return i;
            return -1;
        };

        var AddStackedValue = function (groupNo, catNo, value, series)
        {
            var prev = stackbar[groupNo][catNo].data[stackbar[groupNo][catNo].data.length - 1];
            stackbar[groupNo][catNo].data.push({ start: prev.start + prev.value, value: value, series: series });
        }



        for (var i = 1; i < data.columns.length; i++)
        {
            var series = data.columns[i][0];
            var stackgroup = getStackGroup(series);

            if (stackgroup == "no")
            {
                for (var j = 1; j < data.columns[i].length; j++) {
                    newData[j - 1].data.push({ leftAxis: true, data: [{ start: 0, value: data.columns[i][j], series: series }] });
                }
            }
            else if (typeof stackbar[stackgroup] == "undefined")
            {
                stackbar[stackgroup] = [];
                for (var j = 1; j < data.columns[i].length; j++) {
                    var bar = { leftAxis: true, data: [{ start: 0, value: data.columns[i][j], series: series }] };
                    newData[j - 1].data.push(bar);
                    stackbar[stackgroup].push(bar);
                }
            }
            else
            {
                for (var j = 1; j < data.columns[i].length; j++) {
                    AddStackedValue(stackgroup, j - 1, data.columns[i][j], series);
                }
            }
        }

        return newData;

    }

    if (this.graphObject.axis)
        this._convertFromOldBar();
};