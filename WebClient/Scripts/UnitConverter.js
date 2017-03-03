var UnitConverter = {
    ENTRIES: [
        {
            qnt: 'Length', si: "m", units: [
               { unit: 'micron', factor: 1E6, offset: 0 },
               { unit: 'mm', factor: 1000, offset: 0 },
               { unit: 'cm', factor: 100, offset: 0 },
               { unit: 'dm', factor: 10, offset: 0 },
               { unit: 'km', factor: 1 / 1000, offset: 0 },
               { unit: 'nautical mile', factor: 1 / 1852, offset: 0 },
               { unit: 'ft', factor: 3.28083333334646, offset: 0 },
               { unit: 'inch', factor: 100 / 2.54, offset: 0 },
               { unit: 'yd', factor: 1.09361329833771, offset: 0 },
               { unit: 'statute mile', factor: 0.000621371192237334, offset: 0 }
            ]
        },
        {
            qnt: 'Mass', si: "kg", units: [
               { unit: 'ton (metric)', factor: 1 / 1000, offset: 0 },
               { unit: 'ton (UK)', factor: 0.000984203533290685, offset: 0 },
               { unit: 'quarter (28 lb)', factor: 0.078736437648615, offset: 0 },
               { unit: 'g', factor: 1000, offset: 0 },
               { unit: 'kiloton (metric)', factor: 1 / 1000000, offset: 0 },
               { unit: 'lb', factor: 2.20462262184878, offset: 0 },
               { unit: 'stone', factor: 0.15747304441777, offset: 0 },
               { unit: 'ton (US)', factor: 0.00110231131092439, offset: 0 },
               { unit: 'oz', factor: 35.2739907229404, offset: 0 }
            ]
        },
        {
            qnt: 'Time', si: "s", units: [
               { unit: 'min', factor: 1 / 60, offset: 0 },
               { unit: 'h', factor: 1 / 3600, offset: 0 },
               { unit: 'ms', factor: 1000, offset: 0 },
               { unit: 'week', factor: 1 / 604800, offset: 0 },
               { unit: 'year', factor: 3.16880878140289E-8, offset: 0 }, //todo use more accurate of 1/(365.2425 * 24 * 3600) for gregorian calendar, current is in julian and rounded up!
               { unit: 'day', factor: 1 / 86400, offset: 0 }
            ]
        },
        {
            qnt: 'Temperature', si: "K", units: [
               { unit: '°C', factor: 1, offset: -273.15 },
               { unit: '°F', factor: 1.8, offset: -459.67 },
               { unit: '°R', factor: 1.8, offset: 0 }
            ]
        },
        {
            qnt: 'Force', si: "N", units: [
               { unit: 'kN', factor: 1 / 1000, offset: 0 },
               { unit: 'kgf', factor: 0.101971621297793, offset: 0 },
               { unit: 'dyne', factor: 100000, offset: 0 },
               { unit: 'lbf', factor: 0.224809024733489, offset: 0 },
               { unit: 'short ton force', factor: 0.00110231131092439 * 0.101971621297793, offset: 0 }
            ]
        },
        {
            qnt: 'Pressure', si: "Pa", units: [
               { unit: 'bar', factor: 1 / 100000, offset: 0 },
               { unit: 'MPa', factor: 1 / 1000000, offset: 0 },
               { unit: 'kPa', factor: 1 / 1000, offset: 0 },
               { unit: 'Ba', factor: 10, offset: 0 }, // barye
               { unit: 'atm', factor: 1 / 101325, offset: 0 },
               { unit: 'mm Hg', factor: 0.760 / 101.325, offset: 0 },
               { unit: 'hPa', factor: 1 / 100, offset: 0 },
               { unit: 'mbar', factor: 1 / 100, offset: 0 },
               { unit: 'psi', factor: 0.000145037743897283, offset: 0 },
               { unit: 'ft H2O', factor: 0.000334562292153176, offset: 0 }
            ]
        },
        {
            qnt: 'Energy', si: "J", units: [
               { unit: 'kJ', factor: 1 / 1000, offset: 0 },
               { unit: 'MJ', factor: 1 / 1000000, offset: 0 },
               { unit: 'erg', factor: 10000000, offset: 0 },
               { unit: 'cal', factor: 0.238845896627496, offset: 0 },
               { unit: 'Btu', factor: 0.000947813394498891, offset: 0 },
               { unit: 'kW*h', factor: 1 / 3600000, offset: 0 }
            ]
        },
        {
            qnt: 'Energy flux', si: "W", units: [
               { unit: 'kW', factor: 1 / 1000, offset: 0 },
               { unit: 'MW', factor: 1 / 1000000, offset: 0 },
               { unit: 'erg/s', factor: 10000000, offset: 0 },
               { unit: 'cal/s', factor: 0.238845896627496, offset: 0 },
               { unit: 'hp (metric)', factor: 0.00135962115516133, offset: 0 },
               { unit: 'hp', factor: 0.00134102209244049, offset: 0 },
               { unit: 'Btu/s', factor: 0.000947816987913438, offset: 0 }
            ]
        },
        {
            qnt: 'Energy emission', si: "W/m2", units: [
               { unit: 'kW/m2', factor: 1 / 1000, offset: 0 },
               { unit: 'cal/s*m2', factor: 0.238845896627496, offset: 0 },
               { unit: 'hp/ft2', factor: 0.000124585511788873, offset: 0 },
               { unit: 'Btu/s*ft2', factor: 8.80550661161464E-5, offset: 0 }
            ]
        },
        {
            qnt: 'Area', si: "m2", units: [
               { unit: 'km2', factor: 1 / 1000000, offset: 0 },
               { unit: 'ha', factor: 1 / 10000, offset: 0 },
               { unit: 'ft2', factor: 10.7638687066351, offset: 0 },
               { unit: 'acre', factor: 0.000247105163015276, offset: 0 },
               { unit: 'yd2', factor: 1.19599004630108, offset: 0 },
               { unit: 'inch2', factor: Math.pow(100 / 2.54, 2), offset: 0 },
               { unit: 'square statute mile', factor: 3.8610215854e-07, offset: 0 } //todo use current rounded or use Math.pow(0.000621371192237334, 2) ?
            ]
        },
        { qnt: 'Molar area', si: 'm2/kmol', units: [] },
        {
            qnt: 'Volume', si: "m3", units: [
               { unit: 'gal (UK)', factor: 220, offset: 0 },
               { unit: 'ft3', factor: 35.3145, offset: 0 }, //cu ft?
               { unit: 'liter', factor: 1000, offset: 0 },
               { unit: 'pint (UK)', factor: 1759.75476057657, offset: 0 },
               { unit: 'pint (US)', factor: 2113.37853145553, offset: 0 },
               { unit: 'gal (US)', factor: 264.172051241558, offset: 0 },
               { unit: 'bbl', factor: 6.28981076758366, offset: 0 },
               { unit: 'fl oz (US)', factor: 33814.0222016107, offset: 0 },
               { unit: 'fl oz (UK)', factor: 35195.033276904, offset: 0 },
               { unit: 'km3', factor: 1 / 1000000000, offset: 0 },
               { unit: 'cu yd', factor: Math.pow(1.09361329833771, 3), offset: 0 },
               { unit: 'cu in', factor: Math.pow(100 / 2.54, 3), offset: 0 },
               { unit: 'cu mi', factor: Math.pow(0.000621370033933018, 3), offset: 0 }
            ]
        },
        { qnt: 'Molar volume', si: 'm3/kmol', units: [] },
        {
            qnt: 'Velocity', si: "m/s", units: [
               { unit: 'cm/s', factor: 100, offset: 0 },
               { unit: 'km/h', factor: 3.6, offset: 0 },
               { unit: 'ft/s', factor: 3.28083989501312, offset: 0 },
               { unit: 'mph', factor: 2.2369362920544, offset: 0 },
               { unit: 'mph (nautical)', factor: 1.94384617178935, offset: 0 }
            ]
        },
        {
            qnt: 'Acceleration', si: "m/s2", units: [
               { unit: 'ft/s2', factor: 3.28083989501312, offset: 0 },
               { unit: 'km/s2', factor: 0.001, offset: 0 },
               { unit: 'mph/s', factor: 2.2369362920544, offset: 0 },
               { unit: 'mps2', factor: 0.00062137119224, offset: 0 }
            ]
        },
        {
            qnt: 'Mass rate', si: "kg/s", units: [
               { unit: 'g/s', factor: 1000, offset: 0 },
               { unit: 'kg/h', factor: 3600, offset: 0 },
               { unit: 'g/min', factor: 1000 / 60, offset: 0 },
               { unit: 'ton (metric)/h', factor: 3.6, offset: 0 },
               { unit: 'lb/s', factor: 2.20462262184878, offset: 0 },
               { unit: 'lb/h', factor: 7936.69690548188, offset: 0 }
            ]
        },
        {
            qnt: 'Volume rate', si: "m3/s", units: [
               { unit: 'l/h', factor: 3600000, offset: 0 },
               { unit: 'l/min', factor: 60000, offset: 0 },
               { unit: 'l/s', factor: 1000, offset: 0 },
               { unit: 'm3/h', factor: 3600, offset: 0 },
               { unit: 'm3/min', factor: 60, offset: 0 },
               { unit: 'gal (US)/s', factor: 264.172051241558, offset: 0 },
               { unit: 'gal (US)/h', factor: 951019.38446961, offset: 0 }
            ]
        },
        {
            qnt: 'Density', si: "kg/m3", units: [
               { unit: 'g/m3', factor: 1000, offset: 0 },
               { unit: 'mg/m3', factor: 1000000, offset: 0 },
               { unit: 'g/l', factor: 1, offset: 0 },
               { unit: 'g/cm3', factor: 1 / 1000, offset: 0 },
               { unit: 'lb/ft3', factor: 0.0624279620335609, offset: 0 },
               { unit: 'lb/m3', factor: 2.20462262184878, offset: 0 },
               { unit: 'mg/l', factor: 1000, offset: 0 },
               { unit: 'kg/l', factor: 1 / 1000, offset: 0 }
            ]
        },
        {
            qnt: 'Concentration', si: "kg/m3", units: [
               { unit: 'mg/m3', factor: 1000000, offset: 0 },
               { unit: 'g/cm3', factor: 1 / 1000, offset: 0 },
               { unit: 'g/m3', factor: 1000, offset: 0 },
               { unit: 'lb/ft3', factor: 0.0624279620335609, offset: 0 },
               { unit: 'lb/m3', factor: 2.20462262184878, offset: 0 }
            ]
        },
        {
            qnt: 'Elasticity', si: "N/m", units: [
               { unit: 'dyne/cm', factor: 1000, offset: 0 }
            ]
        },
        {
            qnt: 'Viscosity', si: "Pa*s", units: [
               { unit: 'P', factor: 10, offset: 0 },
               { unit: 'cP', factor: 1000, offset: 0 },
               { unit: 'µP', factor: 10000000, offset: 0 }
            ]
        },
        {
            qnt: 'Enthalpy', si: "J/kg", units: [
               { unit: 'kJ/kg', factor: 1 / 1000, offset: 0 },
               { unit: 'cal/kg', factor: 0.238845896627496, offset: 0 },
               { unit: 'cal/g', factor: 0.000238845896627496, offset: 0 },
               { unit: 'kcal/kg', factor: 0.000238845896627496, offset: 0 },
               { unit: 'Btu/lb', factor: 1 / 2326, offset: 0 }
            ]
        },
        {
            qnt: 'Entropy', si: "J/kg*K", units: [
               { unit: 'kJ/kg*K', factor: 1 / 1000, offset: 0 },
               { unit: 'cal/kg*°C', factor: 0.238845896627496, offset: 0 },
               { unit: 'cal/g*°C', factor: 0.000238845896627496, offset: 0 },
               { unit: 'kcal/kg*°C', factor: 0.000238845896627496, offset: 0 }
            ]
        },
        {
            qnt: 'Heat capacity', si: "J/kg*K", units: [
               { unit: 'kJ/kg*K', factor: 1 / 1000, offset: 0 },
               { unit: 'cal/kg*°C', factor: 0.238845896627496, offset: 0 },
               { unit: 'cal/g*°C', factor: 0.000238845896627496, offset: 0 },
               { unit: 'kcal/kg*°C', factor: 0.000238845896627496, offset: 0 },
               { unit: 'Btu/lb*°F', factor: 0.000238845896627496, offset: 0 }
            ]
        },
        { qnt: 'Heat transfer coeff', si: 'W/m/K', units: [] },
        { qnt: 'Toxic dose', si: 's*(kg/m3)^n', units: [] },
        {
            qnt: 'Heat radiation dose', si: "s*(W/m2)^4/3", units: [
               { unit: 's*(kW/m2)^4/3', factor: Math.pow(1 / 1000, 4 / 3), offset: 0 },
               { unit: 'TDU', factor: Math.pow(1 / 1000, 4 / 3), offset: 0 },
               { unit: 'min*(kW/m2)^4/3', factor: (1 / 60) * Math.pow(1 / 1000, 4 / 3), offset: 0 },
               { unit: '(cal/m2)^4/3', factor: Math.pow(0.238845896627496, 4 / 3), offset: 0 },
               { unit: 's*(hp/ft2)^4/3', factor: Math.pow(0.000124585511788873, 4 / 3), offset: 0 },
               { unit: '(Btu/ft2)^4/3', factor: Math.pow(8.80550661161464E-5, 4 / 3), offset: 0 }
            ]
        },
        {
            qnt: 'Angle', si: "rad", units: [
               { unit: 'deg', factor: 180 / Math.PI, offset: 0 },
               { unit: 'grad', factor: 200 / Math.PI, offset: 0 }
            ]
        },
        {
            qnt: 'Frequency', si: "Hz", units: [
               { unit: '/minute', factor: 60, offset: 0 },
               { unit: '/hour', factor: 3600, offset: 0 },
               { unit: '/day', factor: 86400, offset: 0 },
               { unit: '/year', factor: 365 * 86400, offset: 0 }
            ]
        },
        {
            qnt: 'Distribution', si: "/m2", units: [
               { unit: '/km2', factor: 1000000, offset: 0 },
               { unit: '/ha', factor: 10000, offset: 0 },
               { unit: '/sq. yard', factor: 0.83613, offset: 0 },
               { unit: '/acre', factor: 4046.86, offset: 0 },
               { unit: '/square statute mile', factor: 2589990, offset: 0 }
            ]
        },
        {
            qnt: 'Dimensionless', si: "-", units: [
               { unit: '%', factor: 100, offset: 0 }
            ]
        },
        {
            qnt: 'Time fraction', si: "-", units: [
               { unit: '%', factor: 100, offset: 0 },
               { unit: 'hours/day', factor: 24, offset: 0 },
               { unit: 'hours/week', factor: 168, offset: 0 },
               { unit: 'days/week', factor: 7, offset: 0 },
               { unit: 'days/year', factor: 365, offset: 0 }
            ]
        },
        'No units',
        { qnt: 'Molecular weight', si: "kg/kmol", units: [] },
        {
            qnt: 'Risk frequency', si: "/year", units: [
               { unit: '/km.year', factor: 1, offset: 0 },
               { unit: '/m.year', factor: 1 / 1000, offset: 0 },
               { unit: '/mile.year', factor: 1 / 0.621371192237334, offset: 0 },
               { unit: '/nautical mile.year', factor: 1.852, offset: 0 }
            ]
        },
        { qnt: 'Inverse length', si: "1/m", units: [] },
        {
            qnt: 'Rotation freq', si: "rad/s", units: [
               { unit: 'rpm', factor: 30 / Math.PI, offset: 0 }
            ]
        },
        { qnt: 'Pressure impulse', si: "Pa*s", units: [] },
        { qnt: 'Mass flux', si: "kg/m2*s", units: [] },
        {
            qnt: 'Monetary', si: "-", units: [
               { unit: '', factor: 1, offset: 0 }
            ]
        },
        {
            qnt: 'Thermal conductivity', si: "W/m*K", units: [
               { unit: 'cal/s*m*°C', factor: 0.238845896627496, offset: 0 }
            ]
        },
        {
            qnt: 'Dipole moment', si: "c-m", units: [
               { unit: 'D', factor: 1E-21 / 299.792458E6, offset: 0 }
            ]
        },
        { qnt: 'Solubility', si: "(J/m3)^(1/2)", units: [] }
        //,Probit A -> todo implement dynamic substance support
    ],
    Converters: {},

    Converter: function (aEntry) {
        this.qnt = aEntry.qnt;
        this.si = aEntry.si;
        this.conversionUnits = {};
        this.units = [];

        this.AddConversionUnit = function (aUnit, aFactor, aOffset) {
            this.conversionUnits[aUnit] = { unit: aUnit, factor: aFactor, offset: aOffset };
            this.units.push(aUnit);
        }

        this.AddConversionUnit(this.si, 1, 0);

        if (typeof aEntry.units != "undefined") {
            for (var i = 0; i < aEntry.units.length; i++) {
                this.AddConversionUnit(aEntry.units[i].unit, aEntry.units[i].factor, aEntry.units[i].offset);
            }
        }

        this.RemoveConversionUnit = function (aUnit) {
            if (typeof this.conversionUnits[aUnit] !== "undefined") {
                delete this.conversionUnits[aUnit];
                for (var i = 0; i < units.length; i++)
                    if (this.units[i].unit == aUnit) {
                        this.units.splice(i, 1);
                    }
            }
        }

        this.CanConvertMultiple = function (aUnits) {
            for (var i = 0; i < aUnits.length; i++)
                if (!this.CanConvert(aUnits[i]))
                    return false;
            return true;
        }

        this.CanConvert = function (aUnit) {
            return typeof this.conversionUnits[aUnit] !== "undefined" && this.conversionUnits[aUnit].factor != null && this.conversionUnits[aUnit].offset != null && !isNaN(this.conversionUnits[aUnit].factor) && !isNaN(this.conversionUnits[aUnit].offset);
        }

        this.Clear = function (aUnit) {
            if (typeof this.conversionUnits[aUnit] !== "undefined") {
                this.conversionUnits[aUnit].factor = null;
                this.conversionUnits[aUnit].offset = null;
            }
        }

        this.IsEmpty = function (aUnit) {
            if (typeof this.conversionUnits[aUnit] !== "undefined")
                return (this.conversionUnits[aUnit].factor == null || isNaN(this.conversionUnits[aUnit].factor)) && (this.conversionUnits[aUnit].offset == null || isNaN(this.conversionUnits[aUnit].offset))
        }

        this.ConvertSItoUnit = function (aUnit, aValue) {
            if (this.CanConvert(aUnit)) {
                return (aValue * this.conversionUnits[aUnit].factor) + this.conversionUnits[aUnit].offset;
            }
            else
                throw "Can't convert unit!";
        }

        this.ConvertUnitToSI = function (aUnit, aValue) {
            if (this.CanConvert(aUnit)) {
                return (aValue - this.conversionUnits[aUnit].offset) / this.conversionUnits[aUnit].factor;
            }
            else
                throw "Can't convert unit!";
        }

        this.ConvertUnitToUnit = function (aSourceUnit, aTargetUnit, aValue) {
            if (this.CanConvertMultiple([aSourceUnit, aTargetUnit])) {
                return (this.ConvertSItoUnit(aTargetUnit, this.ConvertUnitToSI(aSourceUnit, aValue)));
            }
            else
                throw "Can't convert unit!";
        }


    },

    DateConverter: function () {
        this.qnt = "Date";
        this.si = "utc";
        this.conversionUnits = {};
        this.units = [];

        this.AddConversionUnit = function (aUnit, aFactor, aOffset) { //todo implement for Date types
            this.conversionUnits[aUnit] = { unit: aUnit, factor: aFactor, offset: aOffset };
            this.units.push(aUnit);
        }

        this.AddConversionUnit(this.si, 1, 0);

        this.RemoveConversionUnit = function (aUnit) {
            if (typeof this.conversionUnits[aUnit] !== "undefined") {
                delete this.conversionUnits[aUnit];
                for (var i = 0; i < units.length; i++)
                    if (this.units[i].unit == aUnit) {
                        this.units.splice(i, 1);
                    }
            }
        }

        this.CanConvertMultiple = function (aUnits) {
            for (var i = 0; i < aUnits.length; i++)
                if (!this.CanConvert(aUnits[i]))
                    return false;
            return true;
        }

        this.CanConvert = function (aUnit) {
            return typeof this.conversionUnits[aUnit] !== "undefined";
        }

        this.Clear = function (aUnit) {
            delete typeof this.conversionUnits[aUnit]
        }

        this.IsEmpty = function (aUnit) {
            return typeof this.conversionUnits[aUnit] === "undefined"
        }

        this.ConvertSItoUnit = function (aUnit, aValue) {
            var date = new Date(aValue); //creates in local timezone:-(
            date.setUTCMinutes(date.getUTCMinutes() - date.getTimezoneOffset()); //sets the localtime as it was
            return date;
            if (this.CanConvert(aUnit)) {
                switch (aUnit.toLowerCase()) {
                    case "utc": return aValue;
                        break;
                    case "gmt":
                        break;
                    case "cst":
                        break;
                    case "est":
                        break;
                }
            }
            else
                throw "Can't convert unit!";
        }

        this.ConvertUnitToSI = function (aUnit, aValue) {
            if (aUnit == "utc")
                return aValue;
            var date = new Date(aValue); //creates in local timezone:-(
            date.setUTCMinutes(date.getUTCMinutes() + date.getTimezoneOffset()); //sets the localtime as it was
            return date;
            if (this.CanConvert(aUnit)) {
                //todo implement unit dependant Date conversion
            }
            else
                throw "Can't convert unit!";
        }

        this.ConvertUnitToUnit = function (aSourceUnit, aTargetUnit, aValue) {
            if (this.CanConvertMultiple([aSourceUnit, aTargetUnit])) {
                //todo implement unit dependant Date conversion
            }
            else
                throw "Can't convert unit!";
        }
    }
}

UnitConverter.ConvNum = function (aQnt, aValue, aUnit) {
    if (typeof UnitConverter.Converters[aQnt] === "undefined")
        throw "Can't build ConvNum of unknown quantity";
    else if (aUnit != null && !UnitConverter.Converters[aQnt].CanConvert(aUnit))
        throw "Can't build ConvNum with unknown unit";
    else {
        this.qnt = aQnt;
        this.converter = UnitConverter.Converters[this.qnt];
        this.unit = typeof aUnit === "undefined" ? this.converter.si : aUnit;
        this.value = this.converter.ConvertUnitToSI(this.unit, aValue);
    }
}

UnitConverter.ConvNum.prototype = {
    GetDisplayValue: function () {
        return this.converter.ConvertSItoUnit(this.unit, this.value);
    },

    SetUnit: function (aUnit) {
        if (!this.converter.CanConvert(aUnit))
            throw "Can't set unit to unknown unit!";
        else
            this.unit = aUnit;
    },

    Clone: function () {
        return new UnitConverter.ConvNum(this.qnt, this.converterConvertSIToUnit(this.unit, this.value), this.unit);
    },

    Add: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't add ConvNum of different quantities";
        else
            return new UnitConverter.ConvNum(this.qnt, this.converter.ConvertSItoUnit(this.unit, this.value + aConvNum.value), this.unit);
    },

    //destructive add
    _Add: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't add ConvNum of different quantities";
        else
            this.value += aConvNum.value;
    },

    Subtract: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't subtract ConvNum of different quantities";
        else
            return new UnitConverter.ConvNum(this.qnt, this.converter.ConvertSItoUnit(this.unit, this.value - aConvNum.value), this.unit);
    },

    //destructive subtract
    _Subtract: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't subtract ConvNum of different quantities";
        else
            this.value -= aConvNum.value;
    },

    MultiplyBy: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't multiply ConvNum of different quantities";
        else
            return new UnitConverter.ConvNum(this.qnt, this.converter.ConvertSItoUnit(this.unit, this.value * aConvNum.value), this.unit);
    },

    //destructive multiplyby
    _MultiplyBy: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't multiply ConvNum of different quantities";
        else this.value *= aConvNum.value;
    },

    DivideBy: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't divide ConvNum of different quantities";
        else
            return new UnitConverter.ConvNum(this.qnt, this.converter.ConvertSItoUnit(this.unit, this.value / aConvNum.value), this.unit);
    },


    //destructive divideby
    _DivideBy: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't divide ConvNum of different quantities";
        else
            this.value /= aConvNum.value;
    },

    Equals: function (aConvNum) {
        return this.qnt == aConvNum.qnt && this.value == aConvNum.value;
    },


    //returns whether this is strict lesser then another object
    LesserThan: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't check lesser for ConvNum of different quantities";
        else
            return this.value < aConvNum.value;
    },

    //returns whether this is strict bigger then another object
    BiggerThan: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't check bigger for ConvNum of different quantities";
        else
            return this.value > aConvNum.value;
    },

    //returns whether this is lesser or equal then another object
    LesserThanEqual: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't check lesserEqual for ConvNum of different quantities";
        else
            return this.value <= aConvNum.value;
    },

    //returns whether this is Bigger or equal then another object
    BiggerThanEqual: function (aConvNum) {
        if (this.qnt != aConvNum.qnt)
            throw "Can't check lesser for ConvNum of different quantities";
        else
            return this.value >= aConvNum.value;
    }
}

for (var i = 0; i < UnitConverter.ENTRIES.length; i++) {
    UnitConverter.Converters[UnitConverter.ENTRIES[i].qnt] = new UnitConverter.Converter(UnitConverter.ENTRIES[i]);
    UnitConverter.Converters["Date"] = new UnitConverter.DateConverter();
}