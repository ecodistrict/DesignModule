var TemperatureModel = L.Class.extend({
    
    initialize: function initializeTemperature(opts) {
        opts = opts || {};

        var valueK = typeof opts.valueK !== 'undefined' ? opts.valueK : 273.15;
        var unit = opts.unit || TemperatureUnit.C;

        Object.defineProperty(this, "value", {
            get: function () {
                var temperatureConverter = new UnitConverter.ConvNum('Temperature', valueK);
                temperatureConverter.SetUnit(unit);
                return (parseFloat(temperatureConverter.GetDisplayValue().toFixed(2)));
            },
            set: function (newValue) {
                var temperatureConverter;
                temperatureConverter = new UnitConverter.ConvNum('Temperature', newValue, unit);
                temperatureConverter.SetUnit('K');
                valueK = parseFloat(temperatureConverter.GetDisplayValue().toFixed(2));

                this.fire('value', { value: newValue });
            }
        });

        Object.defineProperty(this, "temperatureUnit", {
            get: function () {
                return this.unit;
            },
            set: function (newUnit) {
                unit = newUnit;
                this.fire('unit', { unit: unit });
                //Computing the new temperature as per the new unit - calling the get() for the value
                this.fire('value', { value: this.value });
            }
        });


    }

});
L.extend(TemperatureModel.prototype, L.Evented.prototype);