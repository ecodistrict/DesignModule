/**
 * ModuleModel. This model is a modules domain model and represents
 * all information available about a graph in the system. The main idea 
 * is to share this model within all parties that require up to date 
 * information about a module.
 */

/* globals L */

export var ModuleStatus = {
    IDLE:        'idle',
    LOCKED:      'locked',
    BUSY:        'busy',
    READY:       'ready',
    CALCULATING: 'calc',
    REMOVED:     'removed'
};

export function validateStatus(status) {
    for (var s in ModuleStatus) {        
        if (!ModuleStatus.hasOwnProperty(s)) continue;
        if (status === ModuleStatus[s]) return true;
    }
    
    return false;
}

function validateProgress(progress) {
    return typeof progress === 'number';
}

var ModuleModel = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        var id = options.id || 0;
        Object.defineProperty(this, 'id', {
            get: function () { return id; }
        });

        var name;
        var status;
        var progress;

        function setName(newName) {
            if (name === newName) return false;
            
            name = newName;
            return true;
        }

        function setStatus(newStatus) {
            if (!validateStatus(newStatus)) {
                throw new Error('Invalid module status ' + newStatus);
            }
            if (status === newStatus) return false;

            status = newStatus;
            return true;
        }

        function setProgress(newProgress) {
            if (!validateProgress(newProgress)) {
                throw new Error('Invalid progress value ' + newProgress);
            }
            if (progress === newProgress) return false;
            
            progress = newProgress;
            return true;
        }

        setName(options.name || '');
        setStatus(options.status || ModuleStatus.IDLE);
        setProgress(options.progress || 0);

        Object.defineProperty(this, 'name', {
            get: function () { return name; },
            set: function (newName) {
                if (setName(newName)) {
                    this._notifyName();
                    this._notifyChange();
                }
            }
        });

        Object.defineProperty(this, 'status', {
            get: function () { return status; },
            set: function (newStatus) {
                if (setStatus(newStatus)) {
                    this._notifyStatus();
                    this._notifyChange();
                }
            }
        });
        
        Object.defineProperty(this, 'progress', {
            get: function () { return progress; },
            set: function (newProgress) {
                if (setProgress(newProgress)) {
                    this._notifyProgress();
                    this._notifyChange();
                }
            }
        });

        this._setName = setName;
        this._setStatus = setStatus;
        this._setProgress = setProgress;
    },

    set: function (moduleOpts) {
        var changed = false;

        if (typeof moduleOpts.name !== 'undefined') {
            if (this._setName(moduleOpts.name)) {
                this._notifyName();
                changed = true;
            }
        }

        if (typeof moduleOpts.status !== 'undefined') {
            if (this._setStatus(moduleOpts.status)) {
                this._notifyStatus();
                changed = true;
            }
        }

        if (typeof moduleOpts.progress !== 'undefined') {            
            if (this._setProgress(moduleOpts.progress)) {
                this._notifyProgress();
                changed = true;
            }
        }

        if (changed) {
            this._notifyChange();
        }
    },

    _notifyName: function () {
        this.fire('name', { 
            module: this,
            name: this.name
        });
    },

    _notifyProgress: function () {
        this.fire('progress', { 
            module: this,
            progress: this.progress
        });
    },

    _notifyStatus: function () {
        this.fire('status', { 
            module: this,
            status: this.status
        });
    },

    _notifyChange: function () {
        this.fire('change', { module: this });
    }

});

export default ModuleModel;
