/**
 * Kpi is a Chart design based on the recommendations of Stephen Few. Implementation
 * based on the work of Clint Ivy, Jamie Love, and Jason Davies.    
 */

var KpiViewOrientation = {};
Object.defineProperty(KpiViewOrientation, 'left', { value: 'left' });
Object.defineProperty(KpiViewOrientation, 'right', { value: 'right' });
Object.defineProperty(KpiViewOrientation, 'bottom', { value: 'bottom' });
Object.defineProperty(KpiViewOrientation, 'top', { value: 'top' });

var KpiView = L.Control.extend({
    
    initialize: function (opts) {
        L.setOptions(this, opts.leafletOptions);

        this.orient = opts.orient || KpiViewOrientation.left;
        this.duration = opts.duration || 0;

        this.data = L.extend({
            markers: [],
            ranges: [],
            measures: [],
            title: '',
            subtitle: ''
        }, opts.data);
        
        this.width = opts.width || 400;
        this.height = opts.height || 50;
        this.tickFormat = opts.tickFormat || null;

        this.parentElement = opts.element || null;

        this.padding = L.extend({ 
            top: 5, 
            right: 10, 
            bottom: 20, 
            left: 100
        }, opts.padding);

        this.render();
    },

    render: function () {
        if (this.kpiViewport) {
            return this.kpiViewport;
        }

        var bulletGeometry = {
            left: this.padding.left,
            top: this.padding.top,
            width: this.width - this.padding.left - this.padding.right,
            height: this.height - this.padding.top - this.padding.bottom
        };

        var buildBullet = d3.bullet()
            .tickFormat(this.tickFormat)
            .orient(this.orient)
            .duration(this.duration)
            .width(bulletGeometry.width)
            .height(bulletGeometry.height);

        this.kpiViewport = L.DomUtil.create('div', 'kpi-viewport');
        d3.select(this.kpiViewport)
            .attr('width', this.width)
            .attr('height', this.height);

        var svg = d3.select(this.kpiViewport).append('svg').data([this.data])
            .attr('class', 'kpi')
            .attr('width', this.width)
            .attr('height', this.height)
            .append('g')
            .attr('transform', 'translate(' + bulletGeometry.left + ',' + bulletGeometry.top + ')')
            .call(buildBullet);

        var title = svg.append('g')
            .style('text-anchor', 'end')
            .attr('transform', 'translate(-6,' + bulletGeometry.height / 2 + ')');

        title.append('text')
            .attr('class', 'title')
            .text(function (d) { return d.title; });

        title.append('text')
            .attr('class', 'subtitle')
            .attr('dy', '1em')
            .text(function (d) { return d.subtitle; });

        if (this.parentElement) {
            this.parentElement.appendChild(this.kpiViewport);
        }

        return this.kpiViewport;
    },

    remove: function () {
        if (!this.kpiViewport) return;

        var parent = this.kpiViewport.parentNode;
        if (parent) {
            parent.removeChild(this.kpiViewport);
        }

        this.kpiViewport = null;
    },

    onAdd: function (map) {
        return this.render();
    },
});
