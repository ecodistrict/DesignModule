/**
 * Graph legend view model. This model is used only to present 
 * data to view for visualization and not meant to be a domain model.
 * This model will be used by GraphLegendView.
 */

var GraphLegendViewModel = L.Evented.extend({

    initialize: function (opts) {
        opts = opts || {};        
        var optionEntries = opts.entries || [];

        var entries = optionEntries.map(function (entry){
            return L.extend({ enabled: true }, entry);
        });
        Object.defineProperty(this, "entries", {
            get: function () { return entries; },
            set: function (newEntries) {
                entries = newEntries;
                this.fire('entries', { entries: entries });
            }
        });
    },

    enableEntry: function (entry) {
        if (this.entries.indexOf(entry) < 0) return;

        entry.enabled = true;
        this.fire('entries', { entries: this.entries });
    },

    disableEntry: function (entry) {
        if (this.entries.indexOf(entry) < 0) return;

        entry.enabled = false;
        this.fire('entries', { entries: this.entries });
    },

    toggleEntry: function (entry) {
        if (this.entries.indexOf(entry) < 0) return;

        entry.enabled = !entry.enabled;
        this.fire('entries', { entries: this.entries });
    },

    merge: function (entries) {
        var localEntries = this.entries;
        var newEntries = entries.map(function (entry) {
            var localEntry = localEntries.find(function (e) { return e.id === entry.id; }) || { enabled: true };
            return L.extend(localEntry, entry);
        });
        this.entries = newEntries;
    },

    enabledEntriesCount: function () {
        return this.entries.reduce(function (count, entry) {
            return count + (entry.enabled ? 1 : 0);
        }, 0);
    }

});
