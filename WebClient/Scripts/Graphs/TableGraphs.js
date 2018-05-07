
// http://bl.ocks.org/AMDS/4a61497182b8fcb05906

// dynamic tables in D3
//http://bl.ocks.org/LeeMendelowitz/11383724


// todo: init <-> update (dynamic)
// todo: from publisher code

function TableGraph(graphObject) {
    this.graphObject = graphObject;
    this.visible = false;
    graphObject.preview = {};
    this.graphID = graphObject.id;
    this.previewDiv = null;
    this.Initialize = function (container) {
        var width = this.graphObject.width;// - GraphManager.defaultValues.axisMargin.y;
        var height = this.graphObject.height;// - GraphManager.defaultValues.axisMargin.x;
        var marginLeft = GraphManager.defaultValues.graphPadding.left;// + GraphManager.defaultValues.axisMargin.y;
        var marginTop = GraphManager.defaultValues.graphPadding.top;

        this.graphObject.container = container;

        //var data = graphObject.data;
        this.graphObject.data = graphObject.data;
        this.graphObject.displayData = [];

        container.style.visibility = "hidden";
        container.graph = this;
        this.Update();
    };

    this.Reset = function () {
        this.graphObject.data = [];
        this.graphObject.displayData = [];
        this.Update();
    };

    this._UpdatePreview = function () {
        // todo: implement update for preview on reset of graph!
        if (this.previewDiv == null || this.graphObject.data == null || this.graphObject.data.length == 0)
            return;
        else {
            var found = false;
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

    };

    this.GetPreview = function (container) {

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
        title.textContent = this.graphObject.name;
        title.style.width = DataManager.detailsInfo.elementWidth + "px";

        var svgContainer = previewContainer.appendChild(document.createElement("div"));
        svgContainer.className = "preview-svg-container";
        svgContainer.style.width = DataManager.detailsInfo.chartWidth + "px";
        svgContainer.style.height = DataManager.detailsInfo.chartHeight + "px";

        this.graphObject.preview.container = previewContainer;
        this._UpdatePreview();

    };
    this.Update = function (data) {
        var graph = this.graphObject;


        //sets data and displaydata
        // todo: GraphManager.AddGraphData(graph, data);

        var width = graph.container.clientWidth;
        var height = graph.container.clientHeight;
        var marginLeft = GraphManager.defaultValues.graphPadding.left; // + GraphManager.defaultValues.axisMargin.y;
        var marginTop = GraphManager.defaultValues.graphPadding.top;
        var marginRight = GraphManager.defaultValues.graphPadding.right;
        var marginBottom = GraphManager.defaultValues.graphPadding.bottom; // + GraphManager.defaultValues.axisMargin.x;

        //gets the data to display
        var displayData = graph.displayData;

        //sets min/max values
        var minX = (typeof graph.minX === "undefined") ? d3.min(displayData, function (d) {
            return d3.min(d, function (p) {
                if (p)
                    return p.x.GetDisplayValue();
                return null;
            });
        }) : graph.minX;
        var maxX = (typeof graph.maxX === "undefined") ? d3.max(displayData, function (d) {
            return d3.max(d, function (p) {
                if (p)
                    return p.x.GetDisplayValue();
                return null;
            });
        }) : graph.maxX;
        var minY = (typeof graph.minY === "undefined") ? d3.min(displayData, function (d) {
            return d3.min(d, function (p) {
                if (p)
                    return p.y.GetDisplayValue();
                return null;
            });
        }) : graph.minY;
        var maxY = (typeof graph.maxY === "undefined") ? d3.max(displayData, function (d) {
            return d3.max(d, function (p) {
                if (p)
                    return p.y.GetDisplayValue();
                return null;
            });
        }) : graph.maxY;


        if (graph.holdminmax) {
            if (typeof graph.holdvalues === "undefined") { //todo fix possible missed values when new data.length > maxPoints
                graph.holdvalues = {
                    minY: minY,
                    maxY: maxY
                };
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
        };

        d3.select(this.graphObject.container).selectAll("table").remove();
        
        var sortAscending = true;
        var table = d3.select(this.graphObject.container).append('table')
            .classed('tgtable', true);
        var dataTitles = this.graphObject.data.header;
        var dataTitlesKeys = d3.keys(dataTitles);
        var dataData = this.graphObject.data.data;
        var showTitles = this.graphObject.data.showHeader;

        if (typeof showTitles === 'undefined' || showTitles) {
            var headers = table.append('thead').append('tr')
                .classed('tgtr', true)
                .selectAll('th')
                .data(dataTitlesKeys).enter()
                .append('th')
                .classed('tgth', true)
                .text(function (d) {
                    return dataTitles[d];
                })
                .on('click', function (d) {
                    headers.attr('class', 'tgth header');

                    if (sortAscending) {
                        rows.sort(function (a, b) { return b[d] < a[d] ? -1 : (b[d] > a[d] ? 1 : 0); });
                        sortAscending = false;
                        this.className = 'tgth aes';
                    } else {
                        rows.sort(function (a, b) { return b[d] > a[d] ? -1 : (b[d] < a[d] ? 1 : 0); });
                        sortAscending = true;
                        this.className = 'tgth des';
                    }
                });
        }

        var rows = table.append('tbody').selectAll('tr')
            .data(dataData).enter()
            .append('tr')
            .classed('tgtr', true);
        rows.selectAll('td')
            .data(function (d) {
                return dataTitlesKeys.map(function (k) {
                    return { 'value': d[k], 'name': k };
                });
            }).enter()
            .append('td')
            .classed('tgtd', true)
            .attr('data-th', function (d) {
                return d.name;
            })
            .text(function (d) {
                return d.value;
            });

        this._UpdatePreview();
    };

    this._clickEvent = function (e) {
        var graph = e.currentTarget.graph;

        if (graph.visible) {
            graph._closeGraph();
        }
        else {
            graph._openGraph();
        }
    };

    this.ShowGraph = function () {
        this._openGraph();
    };

    this.HideGraph = function () {
        this._closeGraph();
    };

    this._closeGraph = function () {
        this.visible = false;
        GraphManager.RemoveGraph(this.graphID);
        if (this.previewDiv != null)
            L.DomUtil.removeClass(this.previewDiv, "chartPreviewActive");
    };

    this._openGraph = function () {
        this.visible = true;
        this._resetSize();
        GraphManager.AddGraph(this.graphObject.container);
        if (this.previewDiv != null)
            L.DomUtil.addClass(this.previewDiv, "chartPreviewActive");
    };

    this._resetSize = function () {
        var changed = false;
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
    };
}
