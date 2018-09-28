/**
 * d3 function to draw category graph lines.
 */

 /* globals d3 */ 


export default function categoryGraphLines(lines) {
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

    function drawLines(selection) {
        var x = xBottom;
        var y = function (axisId, value) {
            var scale = scaleByAxisId(axisId);
            return scale(value);
        };

        var baseLine = d3.line()
            .x(0)
            .y(0);

        var valueLine = d3.line()
            .x(function(d) { return x(d.categoryId) + x.bandwidth() / 2; })
            .y(function(d) { return y(d.axisId, d.y); });

        function setEventHandlers(selection) {
            return selection
                .on('mouseover', tipController.show)
                .on('mouseout', tipController.hide);
        }

        var linePaths = selection.selectAll('.line')
            .data(lines, function (d) { return d.seriesId; });

        linePaths.exit().remove();

        linePaths.enter().append('path')
            .attr('class', 'line')
            .style('stroke', function (d) { return d.color; })
            .attr('d', function (d) { return baseLine(d.data); })
            .call(setEventHandlers)
            .transition()
                .duration(animationDuration)
                .attr('d', function (d) { return valueLine(d.data); })
                .style('stroke', function (d) { return d.color; });

        linePaths.transition()
            .duration(animationDuration)
            .attr('d', function (d) { return valueLine(d.data); })
            .style('stroke', function (d) { return d.color; });
    }

    drawLines.xBottomScale = xBottomScale;
    drawLines.yLeftScale = yLeftScale;
    drawLines.yRightScale = yRightScale;
    drawLines.tip = tip;
    drawLines.duration = duration;    

    return drawLines;
}
