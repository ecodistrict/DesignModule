/**
 * TimeSlider is a UI component for displaying and manipulating a timeline.
 * 
 * TimeSliderToggleView is a view representing a toggle button that shows 
 * the TimeSlider control when clicked.
 */

/* globals L */

import './timeSliderToggle.css';

var TimeSliderToggleView = L.Control.extend({

    options: {
        collapsed: true,
        position: 'bottomright',
        autoZIndex: true
    },

    initialize: function (aTimesliderDiv, options) {
        L.setOptions(this, options);
    },

    onAdd: function () {
        this.initLayout();
        return this.container;
    },

    initLayout: function () {
        this.container = L.DomUtil.create('div', 'timeslider-toggle-button hidden'); // initially hidden ie expanded
        L.DomEvent.disableClickPropagation(this.container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(this.container);
        }        
        L.DomEvent
            .on(this.container, 'click', L.DomEvent.stop)
            .on(this.container, 'click', this.notifyClicked, this);
        
        var link = L.DomUtil.create('a');
        link.href = '#';
        link.title = 'Timeslider';
        L.DomEvent
            .on(link, 'click', L.DomEvent.stop)
            .on(link, 'click', this.notifyClicked, this);
        this.container.appendChild(link);
    },

    show: function () {
        L.DomUtil.removeClass(this.container, 'hidden');        
    },

    hide: function () {
        L.DomUtil.addClass(this.container, 'hidden');        
    },

    notifyClicked: function () {
        this.fire('clicked');
    }
});
L.extend(TimeSliderToggleView.prototype, L.Evented.prototype);

export default TimeSliderToggleView;
