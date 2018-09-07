/**
 * d3 function to draw category graph bars.
 */

 /* globals d3 */

 export default function categoryGraphBars(bars) {
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

    function drawBars(selection) {
        var x = xBottom;
        var y = function (axisId, value) {
            var scale = scaleByAxisId(axisId);
            return scale(value);
        };

        function setEventHandlers(selection) {
            return selection
                .on('mouseover', tipController.show)
                .on('touchstart.tip', tipController.show)
                .on('touchstart.anm', markTouchstart(true))
                .on('mouseout', tipController.hide)
                .on('touchend.tip', tipController.hide)
                .on('touchend.anm', markTouchstart(false));
        }

        function setBarAttributes(selection) {
            return selection
                .style('fill', function (d) { return d.color; })
                .style('stroke', function (d) { return d.color; })
                .attr('x', function (d, i, data) { return i * (x.bandwidth() / data.length + 2);  })
                .attr('y', function (d) { return Math.min(y(d.axisId, d.y), y(d.axisId, 0)); })
                .attr('width',  function (d, i, data) {  return x.bandwidth() / data.length; })
                .attr('height', function (d) { return Math.abs(y(d.axisId, 0) - y(d.axisId, d.y)); });
        }

        var categories = selection.selectAll('.bar-category')
            .data(bars.categories);

        categories.exit().remove();

        var newCategory = categories.enter().append('g')
            .attr('class', 'bar-category');

        var newAndUpdateCategories = newCategory.merge(categories)
            .attr('transform', function (category) { 
                return 'translate(' + x(category.categoryId) + ', 0)'; 
            });

        var updatedBars = newAndUpdateCategories.selectAll('.bar')
            .data(function (category) { return category.bars; });
        
        updatedBars.exit().remove();
        
        updatedBars.enter().append('rect')
            .attr('class', 'bar')
            .style('fill', function (d) { return d.color; })
            .style('stroke', function (d) { return d.color; })
            .call(setEventHandlers)
            .transition()
                .duration(animationDuration)
                .call(setBarAttributes);

        updatedBars.transition()
            .duration(animationDuration)
            .call(setBarAttributes);            
    }

    drawBars.xBottomScale = xBottomScale;
    drawBars.yLeftScale = yLeftScale;
    drawBars.yRightScale = yRightScale;
    drawBars.tip = tip;
    drawBars.duration = duration;    

    return drawBars;
}
