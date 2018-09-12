/**
 * Tooltip view.
 */

/* globals L, d3 */ 

import './tooltip.css';

function buildHtmlFunction(html) {    
    if (typeof html === 'function') return html;
        
    return function () {
        return html;
    };
}

var TooltipView = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        this._parent = document.body;
        this._html = buildHtmlFunction(options.html || '');

        this.show = this._createShow();
        this.hide = this._createHide();

        this.render();
    },

    render: function () {
        if (this._rootElement) return;

        this._rootElement = L.DomUtil.create('div', 'tooltip-view', this._parent);
    },

    remove: function () {
        if (!this._rootElement) return;

        L.DomUtil.remove(this._rootElement);
        this._rootElement = null;
    },

    _createShow: function () {
        var tooltip = this;
        
        return function () {
            var element = this;

            tooltip._rootElement.innerHTML = tooltip._html();            

            var elementRect = element.getBoundingClientRect();
            var tooltipRect = tooltip._rootElement.getBoundingClientRect();

            var left = elementRect.left + (elementRect.width - tooltipRect.width) / 2;
            var top = elementRect.top - tooltipRect.height - 5;

            d3.select(tooltip._rootElement)
                .style('left', left + 'px')
                .style('top', top + 'px');

            L.DomUtil.addClass(tooltip._rootElement, 'visible');

            return tooltip;
        };
    },

    _createHide: function () {
        var tooltip = this;
        
        return function () {
            L.DomUtil.removeClass(tooltip._rootElement, 'visible');
            return tooltip;
        };        
    }

});

export default TooltipView;
