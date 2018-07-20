var TemperatureUnit = {
    C: 'C',
    K: 'K',
    F: 'F'
};

var TemperatureModel = L.Class.extend({
    
    initialize: function initializeTemperature(opts) {
        opts = opts || {};

        var valueK = typeof opts.valueK !== 'undefined' ? opts.valueK : 273.15;
        var unit = opts.unit || TemperatureUnit.C;

        Object.defineProperty(this, "value", {
            get: function () {
                var temperatureConverter = new UnitConverter.ConvNum('Temperature', valueK);
                if (unit === TemperatureUnit.K)
                    return valueK;
                else if (unit === TemperatureUnit.C) {
                    temperatureConverter.SetUnit('°C')
                    return (parseFloat(temperatureConverter.GetDisplayValue().toFixed(2)));
                }
                else if (unit === TemperatureUnit.F) {
                    temperatureConverter.SetUnit('°F')
                    return (parseFloat(temperatureConverter.GetDisplayValue().toFixed(2)));
                }
            },
            set: function (newValue) {
                var temperatureConverter;
                if (unit === TemperatureUnit.K)
                    valueK = newValue;
                else if (unit === TemperatureUnit.C) {
                    temperatureConverter = new UnitConverter.ConvNum('Temperature', newValue, '°C');
                    temperatureConverter.SetUnit('K');
                    valueK = parseFloat(temperatureConverter.GetDisplayValue().toFixed(2));
                }
                else if (unit === TemperatureUnit.F) {
                    temperatureConverter = new UnitConverter.ConvNum('Temperature', newValue, '°F');
                    temperatureConverter.SetUnit('K');
                    valueK = parseFloat(temperatureConverter.GetDisplayValue().toFixed(2));
                }

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

                //Computing the new temperature as per the new unit
                var temperatureConverter = new UnitConverter.ConvNum('Temperature', valueK);
                if (unit === TemperatureUnit.K)
                    newVal = valueK;
                else if (unit === TemperatureUnit.C) {
                    temperatureConverter.SetUnit('°C')
                    newVal = (parseFloat(temperatureConverter.GetDisplayValue().toFixed(2)));
                }
                else if (unit === TemperatureUnit.F) {
                    temperatureConverter.SetUnit('°F')
                    newVal = (parseFloat(temperatureConverter.GetDisplayValue().toFixed(2)));
                }

                this.fire('value', { value: newVal });
            }
        });


    }

});
L.extend(TemperatureModel.prototype, L.Evented.prototype);