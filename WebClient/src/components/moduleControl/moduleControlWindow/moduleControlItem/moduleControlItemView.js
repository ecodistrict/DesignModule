/**
 * ModuleControlItemView. View for displaying a module info based on ModuleControlItemModel.
 */

/* globals L, d3 */ 

import './moduleControlItem.css';
import moduleControlItemHtml from './moduleControlItem.html';
import View from '../../../../core/view';
import TooltipView from '../../../../core/widgets/tooltip/tooltipView';

var ModuleControlItemView = View.extend({

    onInitialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided to the View');
        if (!opts.moduleControlItemModel) throw new Error('moduleControlItemModel is not provided');

        this._moduleControlItemModel = opts.moduleControlItemModel;
    },

    onRender: function () {
        this._rootElement = L.DomUtil.create('div', 'module-control-item', this._parent);
        this._rootElement.innerHTML = moduleControlItemHtml;
        this._rootElement.addEventListener('contextmenu', function (e) { e.preventDefault(); });

        this._setupTooltip();

        this._updateName();
        this._updateStatus();
        this._updateLoading();

        this._moduleControlItemModel.on('name', this._updateName, this);
        this._moduleControlItemModel.on('status', this._updateStatus, this);
        this._moduleControlItemModel.on('loading', this._updateLoading, this);

        return this._rootElement;
    },

    onRemove: function () {
        this._moduleControlItemModel.off('name', this._updateName, this);
        this._moduleControlItemModel.off('status', this._updateStatus, this);
        this._moduleControlItemModel.off('loading', this._updateLoading, this);

        this._moduleTooltip.remove();
    },

    _updateName: function () {
        d3.select(this._rootElement).select('.module-name')
            .text(this._moduleControlItemModel.name);
    },

    _updateStatus: function () {
        d3.select(this._rootElement).select('.module-status-text')
            .text(this._moduleControlItemModel.status);        
    },

    _updateLoading: function () {
        d3.select(this._rootElement).select('.module-status-icon')
            .classed('fa-circle-notch', this._moduleControlItemModel.loading)
            .classed('fa-spin', this._moduleControlItemModel.loading)
            .classed('fa-check', !this._moduleControlItemModel.loading)            
            .classed('module-status-icon-ready', !this._moduleControlItemModel.loading);
        
        // workaround: IE11 and Edge do not redraw an element after class is removed
        // therefore icon will remain spinning even after fa-spin class is removed.
        // This can be fixed with changing the element thus forceing element redraweal.
        if (!this._moduleControlItemModel.loading) {
            d3.select(this._rootElement).select('.module-status-icon')
                .style('transform', 'none');
        }   
    },

    _setupTooltip: function () {
        this._moduleTooltip = new TooltipView({
            html: function () { 
                return '<div>' + this._moduleControlItemModel.name + '</div>' + 
                       '<div> Status: ' + this._moduleControlItemModel.status + '</div>';
            }.bind(this),
            delay: 1300
        });
        d3.select(this._rootElement)
            .on('mouseover', this._moduleTooltip.show)
            .on('touchstart', this._moduleTooltip.show)
            .on('mouseout', this._moduleTooltip.hide)
            .on('touchend', this._moduleTooltip.hide);
    }

});

export default ModuleControlItemView;
