ControlPropertiesDialog = function (data) {
    //define object properties
    this.dialog = {};

    //define object function
    this.BuildDialog = function (data) {
        var dialog = this.dialog = modalDialogCreate('Control properties', 'control properties');

        var contentContainer = dialog.appendChild(document.createElement('div'));
        contentContainer.className = 'ctrlPropContainer';
        
        this.BuildNode(contentContainer, data);

        dialog.appendChild(document.createElement('hr'));
        var mddb = dialog.appendChild(document.createElement('div'));
        mddb.className = 'modalDialogDevideButtons';
        modelDialogAddButton(mddb, 'Cancel', modalDialogClose);

        modelDialogAddButton(mddb, 'Apply', (function (e) {
            var changeObject = {
                change: [],
                delete: []
            }
            this.readNodeChanges(data, changeObject);
            var message = {
                type: "changeControlProperties",
                payload: changeObject
            };
            wsSend(message);
            modalDialogClose();
        }).bind(this));
    };

    this.BuildNode = function (container, node) {
        var list = container.appendChild(document.createElement('ul'));
        list.className = 'ctrlUL';

        this.buildFieldValueLI(list, 'ID', node.id, false);
        this.buildFieldValueLI(list, 'Name', node.name, false);
        this.buildFieldValueLI(list, 'Description', node.description, false);

        var propLI = (this.buildFieldValueLI(list, 'Properties', node.properties.length, false)).li;
        if (node.properties.length > 0) {
            var propList = propLI.appendChild(document.createElement('ul'));
            propList.className = 'ctrlUL ctrlPropUL';

            for (var i = 0; i < node.properties.length; i++) {
                node.properties[i].input = (this.buildFieldValueLI(propList, node.properties[i].field, node.properties[i].value, true)).input;
            }
        }

        this.buildFieldValueLI(list, 'Children', node.children.length, false);
        //var childrenList = childrenLI.appendChild(document.createElement('ul'));
        //childrenList.className = 'ctrlUL ctrlChildrenUL';

        for (var i = 0; i < node.children.length; i++)
        {
            var childLI = list.appendChild(document.createElement('li'));
            childLI.className = 'ctrlLI ctrlChildLI';
            this.BuildNode(childLI, node.children[i]);
        }

    };

    this.buildFieldValueLI = function(container, field, value, editable)
    {
        editable = editable ? true : false;
        var li = container.appendChild(document.createElement('li'));
        li.className = 'ctrlLI';

        var fieldSpan = li.appendChild(document.createElement('span'));
        fieldSpan.className = 'ctrlFieldSpan';
        fieldSpan.innerHTML = field;
        
        var valueSpan = li.appendChild(document.createElement('span'));
        valueSpan.className = 'ctrlValueSpan';
        //valueSpan.innerHTML = value;

        var valueInput = valueSpan.appendChild(document.createElement('input'));
        valueInput.type = "text"
        valueInput.startValue = value;
        valueInput.value = value;
        valueInput.disabled = !editable;

        return {"li":  li, "input": valueInput };
    };

    this.readNodeChanges = function (node, changeObject) {
        for (var i = 0; i < node.properties.length; i++)
        {
            var prop = node.properties[i];
            if (prop.value != prop.input.value)
            {
                if (prop.input.value != '')
                    changeObject.change.push({ id: node.id, field: prop.field, value: prop.input.value });
                else
                    changeObject.delete.push({ id: node.id, field: prop.field });
            }
        }

        for (var i = 0; i < node.children.length; i++)
        {
            this.readNodeChanges(node.children[i], changeObject);
        }
    };

    this.BuildDialog(data);

    //initialize object
};