/**
 * d3 function to draw continuous graph points.
 */

 /* globals d3 */ 

 /* exported continuousGraphPoints */
 var continuousGraphPoints = function(points) {
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

    function drawPoints(selection) {
        var x = function (axisId, value) {
            var scale = scaleByAxisId(axisId);
            return scale(value);
        };
        var y = function (axisId, value) {
            var scale = scaleByAxisId(axisId);
            return scale(value);
        };

        var pointCollectionContainer = selection.selectAll('.points-collection')
            .data(points, function (d) { return d.seriesId; });

        pointCollectionContainer.enter().append('g')
            .attr('class', 'points-collection');
            
        pointCollectionContainer.exit().remove();

        var pointCollection = selection.selectAll('.points-collection')
            .data(points, function (d) { return d.seriesId; });

        var pointPlots = pointCollection.selectAll('.point')
            .data(function (d) { return d.data; });

        pointPlots.exit().remove();

        pointPlots.enter().append('circle')
            .attr('class', 'point')
            .attr('r', 5)
            .attr('cx', 0)
            .attr('cy', 0)
            .style('stroke', function (d) { return d.color; })
            .style('fill', function (d) { return d.color; })
            //.call(setEventHandlers)
            .transition()
                .duration(animationDuration)
                .attr('r', 5)
                .attr('cx', function (d) { return x(d.xAxisId, d.x); })
                .attr('cy', function (d) { return y(d.yAxisId, d.y); })
                .style('stroke', function (d) { return d.color; })
                .style('fill', function (d) { return d.color; });

        pointPlots.transition()
            .duration(animationDuration)
            .attr('r', 5)
            .attr('cx', function (d) { return x(d.xAxisId, d.x); })
            .attr('cy', function (d) { return y(d.yAxisId, d.y); })
            .style('stroke', function (d) { return d.color; })
            .style('fill', function (d) { return d.color; });
    }

    drawPoints.xTopScale = xTopScale;
    drawPoints.xBottomScale = xBottomScale;
    drawPoints.yLeftScale = yLeftScale;
    drawPoints.yRightScale = yRightScale;
    //drawPoints.tip = tip;
    drawPoints.duration = duration;    

    return drawPoints;
};