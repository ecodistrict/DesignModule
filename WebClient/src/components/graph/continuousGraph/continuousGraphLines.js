/**
 * d3 function to draw continuous graph lines.
 */

 /* globals d3 */ 

 /* exported continuousGraphLines */
 var continuousGraphLines = function(lines) {
    var xTopDefault = d3.scaleBand().range([0, null]).domain([0, null]);
    var xBottomDefault = d3.scaleBand().range([0, null]).domain([0, null]);
    var yLeftDefault = d3.scaleLinear().range([null, 0]).domain([0, null]);
    var yRightDefault = d3.scaleLinear().range([null, 0]).domain([0, null]);
    // var tipControllerDefault = {
    //     show: function () {},
    //     hide: function () {}
    // };
    var animationDurationDefault = 0;

    var xTop = xTopDefault;
    var xBottom = xBottomDefault;
    var yLeft = yLeftDefault;
    var yRight = yRightDefault;
    //var tipController = tipControllerDefault;
    var animationDuration = animationDurationDefault;

    function xTopScale(scale) {
        if (scale === undefined) return xTop;
        xTop = scale || xTopDefault;
        return this;
    }

    function xBottomScale(scale) {
        if (scale === undefined) return xBottom;
        xBottom = scale || xBottomDefault;
        return this;
    }
    
    function yLeftScale(scale) {
        if (scale === undefined) return yLeft;
        yLeft = scale || yLeftDefault;
        return this;
    }

    function yRightScale(scale) {
        if (scale === undefined) return yRight;
        yRight = scale || yRightDefault;
        return this;
    }

    // function tip(tip) {
    //     if (tip === undefined) return tipController;
    //     tipController = tip || tipControllerDefault;
    //     return this;
    // }

    function duration(duration) {
        if (duration === undefined) return animationDuration;
        animationDuration = duration;
        return this;
    }

    function scaleByAxisId(axisId) {
        if (axisId === 'yLeft') return yLeft;
        if (axisId === 'yRight') return yRight;        
        if (axisId === 'xBottom') return xBottom;
        if (axisId === 'xTop') return xTop; //TODO: define xTop
        return function() { return 0; };
    }

    function drawLines(selection) {
        var x = function (axisId, value) {
            var scale = scaleByAxisId(axisId);
            return scale(value);
        };
        var y = function (axisId, value) {
            var scale = scaleByAxisId(axisId);
            return scale(value);
        };

        var baseLine = d3.line()
            .x(0)
            .y(0);

        var valueLine = d3.line()
            .defined(function(d) { return (d.y !== null && d.x !== null); })
            .x(function(d) { return x(d.xAxisId, d.x); })
            .y(function(d) { return y(d.yAxisId, d.y); });

        // function setEventHandlers(selection) {
        //     return selection
        //         .on('mouseover', tipController.show)
        //         .on('mouseout', tipController.hide);
        // }

        var dashedLinePaths = selection.selectAll('.dashedLine')
        .data(lines, function (d) { return d.seriesId; });

        dashedLinePaths.exit().remove();

        dashedLinePaths.enter().append('path')
            .attr('class', 'dashedLine')
            //.style('stroke', function (d) { return d.color; })
            .attr('d', function (d) { return baseLine(d.data); })
            //.call(setEventHandlers)
            .transition()
                .duration(animationDuration)
                .attr('d', function (d) { return valueLine(d.data.filter(valueLine.defined())); });
                //.style('stroke', function (d) { return d.color; });

        dashedLinePaths.transition()
            .duration(animationDuration)
            .attr('d', function (d) { return valueLine(d.data.filter(valueLine.defined())); });
            //.style('stroke', function (d) { return d.color; });





        var linePaths = selection.selectAll('.line')
            .data(lines, function (d) { return d.seriesId; });

        linePaths.exit().remove();

        linePaths.enter().append('path')
            .attr('class', 'line')
            .style('stroke', function (d) { return d.color; })
            .attr('d', function (d) { return baseLine(d.data); })
            //.call(setEventHandlers)
            .transition()
                .duration(animationDuration)
                .attr('d', function (d) { return valueLine(d.data); })
                .style('stroke', function (d) { return d.color; });

        linePaths.transition()
            .duration(animationDuration)
            .attr('d', function (d) { return valueLine(d.data); })
            .style('stroke', function (d) { return d.color; });
    }

    drawLines.xTopScale = xTopScale;
    drawLines.xBottomScale = xBottomScale;
    drawLines.yLeftScale = yLeftScale;
    drawLines.yRightScale = yRightScale;
    //drawLines.tip = tip;
    drawLines.duration = duration;    

    return drawLines;
};