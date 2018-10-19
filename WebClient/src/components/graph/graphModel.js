/**
 * Graph model. This model is a graph domain model and represents
 * all information available about a graph in the system. The main idea 
 * is to share this model within all parties that require up to date 
 * information about a graph.
 * It can be used as a model for view but it is not the main 
 * purpose of the model.
 */

/* globals L */ 

var GraphModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var id = options.id;
        Object.defineProperty(this, 'id', {
            get: function () { return id; },
        });
        
        var type = options.type;
        Object.defineProperty(this, 'type', {
            get: function () { return type; },
        });

        var title;
        var axes;
        var categories;
        var series;
        var clickable;

        function setTitle(newTitle) {
            if (title === newTitle) return false;
            title = newTitle;
            return true;
        }

        function setAxes(newAxes) {
            if (axes === newAxes) return false;
            axes = newAxes;
            return true;
        }

        function setCategories(newCategories) {
            if (categories === newCategories) return false;
            categories = newCategories;
            return true;
        }

        function setSeries(newSeries) {
            if (series === newSeries) return false;
            series = newSeries;
            return true;
        }

        function setClickable(newClickable) {
            if (clickable === newClickable) return false;
            clickable = newClickable;
            return true;
        }

        setTitle(options.title || '');
        setAxes(options.axes || []);
        setCategories(options.categories || []);
        setSeries(options.series || []);
        setClickable(options.clickable);

        Object.defineProperty(this, 'title', {
            get: function () { return title; },
            set: function (newTitle) {
                if (setTitle(newTitle)) {
                    this._notifyTitle();
                    this._notifyChange();
                }
            }
        });
        
        Object.defineProperty(this, 'axes', {
            get: function () { return axes; },
            set: function (newAxes) {
                if (setAxes(newAxes)) {
                    this._notifyAxes();
                    this._notifyChange();
                }
            }
        });

        Object.defineProperty(this, 'categories', {
            get: function () { return categories; },
            set: function (newCategories) {
                if (setCategories(newCategories)) {
                    this._notifyCategories();
                    this._notifyChange();
                }
            }
        });

        Object.defineProperty(this, 'series', {
            get: function () { return series; },
            set: function (newSeries) {
                if (setSeries(newSeries)) {
                    this._notifySeries();
                    this._notifyChange();
                }
            }
        });

        Object.defineProperty(this, 'clickable', {
            get: function () { return clickable; },
            set: function (newClickable) {
                if (setClickable(newClickable)) {
                    this._notifyClickable();
                    this._notifyChange();
                }
            }
        });

        this._setTitle = setTitle;
        this._setAxes = setAxes;
        this._setCategories = setCategories;
        this._setSeries = setSeries;
        this._setClickable = setClickable;
    },

    set: function (graphOpts) {
        var changed = false;

        if (typeof graphOpts.title !== 'undefined') {
            if (this._setTitle(graphOpts.title)) {
                this._notifyTitle();
                changed = true;
            }
        }

        if (typeof graphOpts.axes !== 'undefined') {
            if (this._setAxes(graphOpts.axes)) {
                this._notifyAxes();
                changed = true;
            }
        }

        if (typeof graphOpts.categories !== 'undefined') {
            if (this._setCategories(graphOpts.categories)) {
                this._notifyCategories();
                changed = true;
            }
        }

        if (typeof graphOpts.series !== 'undefined') {
            if (this._setSeries(graphOpts.series)) {
                this._notifySeries();
                changed = true;
            }
        }

        if (typeof graphOpts.clickable !== 'undefined') {
            if (this._setClickable(graphOpts.clickable)) {
                this._notifyClickable();
                changed = true;
            }
        }

        if (changed) {
            this._notifyChange();
        }
    },

    _notifyTitle: function () {
        this.fire('title', {
            graphModel: this,
            title: this.title
        });
    },

    _notifyAxes: function () {
        this.fire('axes', {
            graphModel: this,
            axes: this.axes
        });
    },

    _notifyCategories: function () {
        this.fire('categories', {
            graphModel: this,
            categories: this.categories
        });
    },

    _notifySeries: function () {
        this.fire('series', {
            graphModel: this,
            series: this.series
        });
    },

    _notifyClickable: function () {
        this.fire('title', {
            graphModel: this,
            clickable: this.clickable
        });
    },

    _notifyChange: function () {
        this.fire('change', {
            graphModel: this
        });
    }

});

export default GraphModel;
