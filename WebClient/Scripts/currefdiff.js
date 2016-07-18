L.Controls.crd = {
    initialize: function (aParent) {
        this.div = aParent;
        this.form = control.div.appendChild(document.createElement('form'));
        this.current = this._addRadioButton(control, 'layerVersion', 'current', true, aOnClick);
        this.reference = this._addRadioButton(control, 'layerVersion', 'reference', false, aOnClick);
        this.difference = this._addRadioButton(control, 'layerVersion', 'difference', false, aOnClick);
    },

    _addRadioButton: function (aGroup, aTitle, aChecked, aOnClick) {
        var button = this.form.appendChild(document.createElement('div'));
        button.className = 'crdRadioButton';
        var l = button.appendChild(document.createElement('label'));
        var i = l.appendChild(document.createElement('input'));
        i.className = 'crdRadioButtonInput';
        i.type = 'radio';
        i.value = aTitle;
        i.name = aGroup;
        i.checked = aChecked;
        i.crd = this;
        if (aOnClick)
            i.onclick = aOnClick;
        var s = l.appendChild(document.createElement('span'));
        s.className = 'crdRadioButtonSpan';
        s.textContent = aTitle;
        return i
    },

    setVisibility: function (aElement, aVisible) {
        if (aVisible)
            aElement.style.removeProperty('display'); // restore original display style
        else
            aElement.style.display = 'none';
    },

    setEnabled: function (aCurrent, aReference, aDifference) {
        if (aCurrent + aReference + aDifference > 1) {
            setVisibility(div, true);
            setVisibility(this.parentElement.parentElement, aCurrent);
            setVisibility(this.reference.parentElement.parentElement, aReference);
            setVisibility(this.difference.parentElement.parentElement, aDifference);
        }
        else
            setVisibility(this.div, false);
    },

    getEnabled: function () {
        return {
            current: this.current.parentElement.parentElement.style.display != 'none',
            reference: this.reference.parentElement.parentElement.style.display != 'none',
            difference: this.difference.parentElement.parentElement.style.display != 'none'
        }
    },

    setSelected: function (aCurrent, aReference, aDifference) {
        this.current.checked = aCurrent;
        this.reference.checked = aReference;
        this.difference.checked = aDifference;
    },

    getSelected: function () {
        return {
            current: this.current.checked,
            reference: this.reference.checked,
            difference: this.difference.checked
        }
    }
}