var ContextManager = {
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

    addContext: function (contextItem)
    {
        switch (contextItem.owner)
        {
            case "map": this.addMapContext(contextItem)
                break;
            case "marker":
                break;
        }
    },

    changeContext: function (contextItem)
    {
        //todo: implement
    },

    deleteContext: function (contextItem)
    {
        //todo: implement
    },

    addMapContext: function (contextItem)
    {
        map.contextmenu.addItem({
            text: contextItem.text,
            callback: (function (e) {
                wsSend({
                    message: {
                        type: 'context',
                        payload: {
                            owner: 'map',
                            id: this.id,
                            position: e.latlng,
                            action: 'select',
                        }
                    }
                });
            }).bind({id: contextItem.id})
        });
    }
}