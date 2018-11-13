/**
 * ModuleControlItemModel. This model is a view model to display module info on the screen.
 * This model can be used in different views.
 */

/* globals L */

var ModuleControlItemModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var id = options.id || 0;
        Object.defineProperty(this, 'id', {
            get: function () { return id; }
        });

        var name;
        var status;
        var loading;

        function setName(newName) {
            if (name === newName) return false;
            
            name = newName;
            return true;
        }

        function setStatus(newStatus) {
            if (status === newStatus) return false;

            status = newStatus;
            return true;
        }

        function setLoading(newLoading) {
            if (loading === newLoading) return false;

            loading = !!newLoading;
            return true;
        }

        setName(options.name || '');
        setStatus(options.status || '');
        setLoading(options.loading || false);


        Object.defineProperty(this, 'name', {
            get: function () { return name; },
            set: function (newName) {
                if (setName(newName)) {
                    this._notifyName();
                }
            }
        });

        Object.defineProperty(this, 'status', {
            get: function () { return status; },
            set: function (newStatus) {
                if (setStatus(newStatus)) {
                    this._notifyStatus();
                }
            }
        });

        Object.defineProperty(this, 'loading', {
            get: function () { return loading; },
            set: function (newLoading) {
                if (setLoading(newLoading)) {
                    this._notifyLoading();
                }
            }
        });

        this._setName = setName;
        this._setStatus = setStatus;
        this._setLoading = setLoading;
    },

    set: function (moduleOpts) {
        if (typeof moduleOpts.name !== 'undefined') {
            if (this._setName(moduleOpts.name)) {
                this._notifyName();
            }
        }

        if (typeof moduleOpts.status !== 'undefined') {
            if (this._setStatus(moduleOpts.status)) {
                this._notifyStatus();
            }
        }

        if (typeof moduleOpts.loading !== 'undefined') {            
            if (this._setLoading(moduleOpts.loading)) {
                this._notifyLoading();
            }
        }
    },

    _notifyName: function () {
        this.fire('name', { 
            model: this,
            name: this.name
        });
    },

    _notifyStatus: function () {
        this.fire('status', { 
            model: this,
            status: this.status
        });
    },

    _notifyLoading: function () {
        this.fire('loading', { 
            model: this,
            loading: this.loading
        });
    },

});

export default ModuleControlItemModel;
