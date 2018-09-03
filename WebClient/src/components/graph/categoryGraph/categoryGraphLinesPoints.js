/**
 * d3 function to draw category graph lines points.
 */

 /* globals d3 */

export default function categoryGraphLinesPoints(lines) {
    var xBottomDefault = d3.scaleBand().rangeRound([0, null]);
    var yLeftDefault = d3.scaleLinear().range([null, 0]).domain([0, null]);
    var yRightDefault = d3.scaleLinear().range([null, 0]).domain([0, null]);
    var tipControllerDefault = {
        show: function () {},
        hide: function () {}
    };
    var animationDurationDefault = 0;

    var xBottom = xBottomDefault;
    var yLeft = yLeftDefault;
    var yRight = yRightDefault;
    var tipController = tipControllerDefault;
    var animationDuration = animationDurationDefault;

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

    function tip(tip) {
        if (tip === undefined) return tipController;
        tipController = tip || tipControllerDefault;
        return this;
    }

    function duration(duration) {
        if (duration === undefined) return animationDuration;
        animationDuration = duration;
        return this;
    }

    function scaleByAxisId(axisId) {
        if (axisId === 'yLeft') return yLeft;
        if (axisId === 'yRight') return yRight;
        return function() { return 0; };
    }

    function markTouchstart(enableMark) {
        return function () {
            return d3.select(this).classed('touchstart', enableMark);
        };
    }

    function drawPoints(selection) {
        var x = xBottom;
        var y = function (axisId, value) {
            var scale = scaleByAxisId(axisId);
            return scale(value);
        };

        var setEventHandlers = function(selection) {
            return selection
                .on('mouseover', tipController.show)
                .on('touchstart.tip', tipController.show)
                .on('touchstart.anm', markTouchstart(true))
                .on('mouseout', tipController.hide)
                .on('touchend.tip', tipController.hide)
                .on('touchend.anm', markTouchstart(false));
        }.bind(this);

        var newLinePaths = selection.selectAll('.line-points-container')
            .data(lines, function (d) { return d.seriesId; });
        newLinePaths.enter().append('g')
            .attr('class', 'line-points-container');
        newLinePaths.exit().remove();

        var lineContainers = selection.selectAll('.line-points-container')
            .data(lines, function (d) { return d.seriesId; });        

        var points = lineContainers.selectAll('circle.point')
            .data(function(d) { return d.data; });

        points.enter().append('circle')
            .attr('class', function(d,i) { return 'point point-' + i; })
            .attr('cx', 0)
            .attr('cy', 0)
            .style('fill', function (d) { return d.color; })
            .style('stroke', function (d) { return d.color; })
            .call(setEventHandlers)
            .transition()
                .duration(animationDuration)
                .attr('cx', function(d) { return x(d.categoryId) + x.bandwidth() / 2; })
                .attr('cy', function(d) { return y(d.axisId, d.y); });

        points.exit().remove();
        
        points.transition()
            .duration(animationDuration)
            .attr('cx', function(d) { return x(d.categoryId) + x.bandwidth() / 2; })
            .attr('cy', function(d) { return y(d.axisId, d.y); })
            .style('fill', function (d) { return d.color; })
            .style('stroke', function (d) { return d.color; });
    }

    drawPoints.xBottomScale = xBottomScale;
    drawPoints.yLeftScale = yLeftScale;
    drawPoints.yRightScale = yRightScale;
    drawPoints.tip = tip;
    drawPoints.duration = duration;    

    return drawPoints;
}
