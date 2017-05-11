var ContextManager = {
    items: {},

    contextMessage: function (payload)
    {
        for (var i = 0; i < payload.length; i++)
        {
            if (payload[i].new)
                this.addContext(payload[i].new);
            else if (payload[i].change)
                this.changeContext(payload[i].change);
            else if (payload[i].delete)
                this.deleteContext(payload[i].delete);
        }
    },

    resetContextMenu: function (payload)
    {
        for (text in this.items)
            map.contextmenu.removeItem(this.items[text]);
        this.items = {};
    },

    addContext: function (contextItem)
    {
        switch (contextItem.owner)
        {
            case "map": this.addMapContext(contextItem)
                break;
            case "marker": //not used? for now implemented in marker messages
                break;
        }
    },

    changeContext: function (contextItem)
    {
        //todo: implement
    },

    deleteContext: function (contextItem)
    {
        if (contextItem.text && typeof this.items[contextItem.text] != 'undefined')
        {
            map.contextmenu.removeItem(this.items[contextItem.text]);
            delete this.items[contextItem.text];
        }
    },

    addMapContext: function (contextItem)
    {
        if (contextItem.text && typeof this.items[contextItem.text] == 'undefined')
            this.items[contextItem.text] = map.contextmenu.addItem({
                text: contextItem.text,
                callback: (function (e) {
                    wsSend({
                            type: 'context',
                            payload: {
                                owner: 'map',
                                id: this.id,
                                position: {
                                    lat: e.latlng.lat,
                                    lon: e.latlng.lng
                                },
                                action: 'select',
                            }
                        });
                }).bind({id: contextItem.id})
            });
    }
}