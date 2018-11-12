/**
 * ModuleControlToggleView. Represents a toggle button that shows or hides the ModuleControlWindowView.
 */

/* globals L */ 

import './moduleControlToggle.css';
import moduleControlToggleHtml from './moduleControlToggle.html';
import View from '../../../core/view';

/* globals L, d3 */

var ControlView = L.Control.extend(View.prototype);

var ModuleControlToggleView = ControlView.extend({    
    options: {
        collapsed: true,
        position: 'bottomright',
        autoZIndex: true
    },

    onInitialize: function (opts) {
        if (!opts) throw new Error('opts is not provided');
        if (!opts.toggleViewModel) throw new Error('opts.toggleViewModel is not provided');

        this._toggleViewModel = opts.toggleViewModel;
        this._parent = opts.parent;
    },

    onRender: function () {
        this._rootElement = L.DomUtil.create('div', 'module-control-toggle-view', this._parent);
        this._rootElement.innerHTML = moduleControlToggleHtml;
        this._rootElement.setAttribute('aria-haspopup', 'true');

        L.DomEvent.disableClickPropagation(this._rootElement);
        L.DomEvent.disableScrollPropagation(this._rootElement);

        this._processLoadingState();
        this._toggleViewModel.on('loading', this._processLoadingState, this);

        this._rootElement.addEventListener('click', this._onClick.bind(this));
        this._rootElement.addEventListener('touchend', this._onClick.bind(this));
        this._rootElement.addEventListener('mousedown', this._onPress.bind(this));
        this._rootElement.addEventListener('touchstart', this._onPress.bind(this));
        return this._rootElement;
    },

    onRemove: function () {
        this._toggleViewModel.off('loading', this._processLoadingState, this);
    },

    onAdd: function () {
        this.render();
        return this.element();
    },

    _processLoadingState: function () {
        d3.select(this._rootElement).select('.module-control-progress-icon')
            .classed('hidden', !this._toggleViewModel.loading);
    },

    _onClick: function (e) {
        e.preventDefault();
        e.stopPropagation();

        this._notifyClicked();
    },

    _onPress: function (e) {
        e.stopPropagation();
        this._notifyFocus();
    },

    _notifyClicked: function () {
        this.fire('clicked', { view: this });
    }

});

export default ModuleControlToggleView;
