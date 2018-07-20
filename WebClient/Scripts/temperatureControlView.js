
L.Control.Temp = L.Control.extend({
    options: {
        collapsed: false,
        position: 'topleft',
        autoZIndex: true,
        height: 150,
        width: 150,
    },

    /// leaflet constructor
    initialize: function (options) {
        L.setOptions(this, options);
        this.parentContainer = options.element;
    },

    onAdd: function (map) {
        this.active = true;
        this._initLayout();
        this._map = map;
        return this._container;
    },

    onRemove: function () {
        this.active = false;
    },

    _initLayout: function () {
        this._container = this.parentContainer;//L.DomUtil.create('div', 'd3-control-temperatureControl');

        //Code for making the div draggable
        // add drag support to main div
        //this._draggable = new L.Draggable(this._container);
        //this._draggable.ref = this;

        //this._draggable.enable();

        this._temperatureDiv = L.DomUtil.create('div', "temperatureDiv");
        this._container.appendChild(this._temperatureDiv);

        //disabling propagation
        L.DomEvent.disableClickPropagation(this._container);
        L.DomEvent.disableScrollPropagation(this._container);

        // use context menu event to return to "live" state
        this._container.addEventListener('contextmenu', this._handleContextMenu);

        //--------------------Variable declarations------------------------
        //svg image vars
        var width = 90;
        var height = 160;
        this.borderWidth = 1;

        //general x-coordinate var
        this.xPosForBaseComponents = (width / 2.5);

        //capillary tube vars
        this.tubeWidth = 12;
        this.tubeRectY = (height / 8);
        this.tubeHeight = ((3.1 * height) / 4) - this.tubeRectY;
        this.tubeRectX = this.xPosForBaseComponents - (this.tubeWidth / 2);

        this.tubeBulbPixelOverlap = 3; //the spherical bulb overlaps the capillary tube by defined pixels

        //mercury vals
        this.mercuryColor_red = "#E50000";
        this.mercuryColor_blue = "#30669F";
        this.centerColor = "#CCD1D1";

        //min and max temperature in K
        var minTemperature = new UnitConverter.ConvNum('Temperature', 253.15);
        var maxTemperature = new UnitConverter.ConvNum('Temperature', 323.15);

        //kelvin min and max values
        this.minKelvin = minTemperature.GetDisplayValue();
        this.maxKelvin = maxTemperature.GetDisplayValue();

        //celsius min and max values
        minTemperature.SetUnit('°C');
        maxTemperature.SetUnit('°C');
        this.minCelsius = parseFloat(minTemperature.GetDisplayValue().toFixed(2));
        this.maxCelsius = parseFloat(maxTemperature.GetDisplayValue().toFixed(2));

        //fahrenheit min and max values
        minTemperature.SetUnit('°F');
        maxTemperature.SetUnit('°F');
        this.minFahrenheit = parseFloat(minTemperature.GetDisplayValue().toFixed(2));
        this.maxFahrenheit = parseFloat(maxTemperature.GetDisplayValue().toFixed(2));

        //Universal Scale
        this.universalScale = d3.scale.linear()
            .domain([this.minKelvin, this.maxKelvin])
            .range([this.tubeHeight - this.tubeBulbPixelOverlap, 0]);

        /*
        //D3 V4 code for the scale
        this.universalScale = d3.scaleLinear()
            .domain([this.minKelvin, this.maxKelvin])
            .range([this.tubeHeight - this.tubeBulbPixelOverlap, 0]);
        */

        //selected unit variable - default set to C
        this.selectedUnit = TemperatureUnit.C;

        //setting up the temperature model and passing in the default values
        this.temperatureModel = new TemperatureModel({
            valueK: 273.15,
            unit: this.selectedUnit,
        });

        //connecting the model event
        this.temperatureModel.on('value', this.modelValueChanged.bind(this));
        this.temperatureModel.on('temperatureUnit', this.modelValueChanged.bind(this));

        //mouse click Y-position for the mercury level
        this.mouseClickY;

        //this flag becomes true when left click is detected (left click - edit temperature; right click - fetch live temperature)
        this.editTemperatureFlag = false;

        //-----------------Creating and adding components------------------------

        this.temperatureDiv = d3.select(this._temperatureDiv).style("width", width + "px").style("height", height + "px");

        //div for unit and its dropDown
        this.unitsControlDiv = this.temperatureDiv.append("div")
            .style("position", "absolute")
            .style("top", (this.tubeRectY) + "px")
            .style("left", (this.xPosForBaseComponents + (this.tubeWidth * 1.5)) + "px");

        //Label displaying the temprature unit
        this.temperatureUnitLabel = this.unitsControlDiv.append('label')
            .attr("class", "mercuryControl")
            .html("&#176;" + this.selectedUnit)
            .style("position", "absolute").style("width", "34px");

        this.temperatureUnitLabel.on('click', function () {
            this.unitsDropdown.style("display", "block");
        }.bind(this));

        this.unitsDropDownDiv();
        this.temperatureTextbox();

        //svg image declaration
        this.svg = this.temperatureDiv
            .append("svg")
            .attr("width", width)
            .attr("height", height);

        //definition of the mercury circle color gradient component
        this.defs = this.svg.append("defs");
        this.mercuryBulbGradientInitialization("mercuryGradient_red", this.mercuryColor_red);
        this.mercuryBulbGradientInitialization("mercuryGradient_blue", this.mercuryColor_blue);

        this.thermometerDesign();
        this.mercuryDesign();
        this.axisInitialization();

        //setting the default startup value for the temperature control
        this.temperatureModel.value = this.temperatureModel.value;
    },

    _handleContextMenu: function (e) {
        e.preventDefault();
        e.cancelBubble = true;
    },

    modelValueChanged: function modelValueChanged(data) {
        //update mercury level
        this.mercuryLevelManagement();
        //update temperature in the value box - is this necessary? the ubove functions does both the updations
        this.temperatureValueManagement();
    },

    displayAxis: function displayAxis() {
        // D3 axis object for the temperature scale
        var axis;
        if (this.selectedUnit === TemperatureUnit.C)
            this.universalScale.domain([this.minCelsius, this.maxCelsius]);
        else if (this.selectedUnit === TemperatureUnit.K)
            this.universalScale.domain([this.minKelvin, this.maxKelvin]);
        else if (this.selectedUnit === TemperatureUnit.F)
            this.universalScale.domain([this.minFahrenheit, this.maxFahrenheit]);

        axis = d3.svg.axis()
            .scale(this.universalScale);

        axis.innerTickSize(5)
            .outerTickSize(0)
            .ticks(8)
            .orient("left");

        /*
         //D3 V4 code for the axis
         axis = d3.axisLeft()
            .scale(this.universalScale);

        axis.tickSizeInner(5)
            .tickSizeOuter(0)
            .ticks(8);
         */

        return axis;
    },

    mercuryLevelManagement: function () {
        var mercuryY = this.universalScale(this.temperatureModel.value) + this.tubeRectY;
        var mercuryHeight = this.tubeRectY + this.tubeHeight - mercuryY + this.tubeBulbPixelOverlap;
        this.mercury.attr("y", mercuryY)
            .attr("height", mercuryHeight);
    },

    temperatureValueManagement: function () {
        var temperaturePixel = this.temperatureModel.value;
        this.temperatureValueInput.property('value', temperaturePixel);
    },

    getTemperatureValue: function () {
        var mouseClickTemperatureValue = parseFloat(this.universalScale.invert(this.mouseClickY - this.tubeRectY).toFixed(2));
        return mouseClickTemperatureValue;
    },

    changeMercuryColor: function (event) {
        switch (event.which) {
            case 1:
                this.mercury.attr("fill", this.mercuryColor_blue);
                this.mercuryBulb.style("fill", "url(#mercuryGradient_blue)")
                    .style("stroke", this.mercuryColor_blue);
                this.editTemperatureFlag = true;
                break;
            case 3:
                this.mercury.attr("fill", this.mercuryColor_red);
                this.mercuryBulb.style("fill", "url(#mercuryGradient_red)")
                    .style("stroke", this.mercuryColor_red);
                this.editTemperatureFlag = false;
                break;
        }
        this.temperatureValueInput.property("disabled", !this.editTemperatureFlag);
    },

    setUnitDropDown: function (selUnit) {
        this.selectedUnit = selUnit;
        this.temperatureUnitLabel.html("&#176;" + this.selectedUnit);
        this.unitsDropdown.style("display", "none");
        var axis = this.displayAxis();

        this.svg.selectAll("g.temperatureAxis")
            .call(axis);
        this.temperatureModel.temperatureUnit = this.selectedUnit;
    },

    unitsDropDownDiv: function () {
        this.unitsDropdown = this.unitsControlDiv.append("div")
            .attr("id", "tempDropDown")
            .attr("class", "dropdown-content ");

        var unitC = this.unitsDropdown.append("a")
            .attr("href", "#")
            .html("&#176;" + TemperatureUnit.C)
            .on('click', function () {
                this.setUnitDropDown(TemperatureUnit.C)
            }.bind(this));

        var unitF = this.unitsDropdown.append("a")
            .attr("href", "#")
            .html("&#176;" + TemperatureUnit.F)
            .on('click', function () {
                this.setUnitDropDown(TemperatureUnit.F)
            }.bind(this));

        var unitK = this.unitsDropdown.append("a")
            .attr("href", "#")
            .html("&#176;" + TemperatureUnit.K)
            .on('click', function () {
                this.setUnitDropDown(TemperatureUnit.K)
            }.bind(this));
    },

    temperatureTextbox: function () {
        this.temperatureValueInput = this.temperatureDiv.append('input')
            .attr('type', 'text')
            .attr("class", "mercuryControl")
            .attr('id', 'temperatureValue')
            .attr('value', '0.0')
            .style("position", "absolute").style("top", (this.tubeRectY + (2 * this.tubeHeight) / 3) + "px").style("left", (this.xPosForBaseComponents + this.tubeWidth) + "px").style("width", "34px");

        //setting default state
        this.temperatureValueInput.property("disabled", !this.editTemperatureFlag);

        //on-input event for updating the mercury level
        this.temperatureValueInput.on('input', function () {
            var inputTemperatureVal = document.getElementById("temperatureValue").value;

            //regular expression for checking the user input
            var inputRegex = /^-?\d+\.?\d{1,2}$/;
            var isValid = (inputTemperatureVal.match(inputRegex) !== null);

            if (isValid) {
                var minDomain = parseFloat(this.universalScale.domain()[0].toFixed(2));
                var maxDomain = parseFloat(this.universalScale.domain()[1].toFixed(2));

                //if (inputTemperatureVal < minDomain)
                //    inputTemperatureVal = minDomain;
                //else if (inputTemperatureVal > maxDomain)
                //    inputTemperatureVal = maxDomain;
                if (inputTemperatureVal >= minDomain && inputTemperatureVal <= maxDomain)
                    this.temperatureModel.value = inputTemperatureVal;
            }
        }.bind(this));
    },

    mercuryBulbGradientInitialization: function (gradientID, mercuryColor) {
        var mercuryBulbBGradient = this.defs.append("radialGradient")
            .attr("id", gradientID);

        mercuryBulbBGradient.append("stop")
            .attr("offset", "1%")
            .attr("stop-color", this.centerColor);

        mercuryBulbBGradient.append("stop")
            .attr("offset", "95%")
            .attr("stop-color", mercuryColor);
    },

    thermometerDesign: function () {
        //spherical bulb vars
        var bulbR = 14;
        var bulbCx = this.xPosForBaseComponents;
        var bulbCy = this.tubeRectY + this.tubeHeight + bulbR - this.tubeBulbPixelOverlap;

        //expansion chamber semi-sphere vars
        var exp_bulbR = this.tubeWidth / 2;
        var exp_bulbCx = this.xPosForBaseComponents - 0.5;
        var exp_bulbCy = this.tubeRectY;

        //thermometer expansion chamber semi-circle
        this.svg.append("circle")
            .attr("class", "thermometerStructure innerDesign")
            .attr("cx", exp_bulbCx)
            .attr("cy", exp_bulbCy)
            .attr("r", exp_bulbR)
            .style("stroke-width", this.borderWidth + "px");

        //thermometer capillary tube rectangle
        this.svg.append("rect")
            .attr("class", "thermometerStructure innerDesign")
            .attr("x", this.tubeRectX)
            .attr("y", this.tubeRectY)
            .attr("width", this.tubeWidth)
            .attr("height", this.tubeHeight)
            .style("shape-rendering", "crispEdges")
            .style("stroke-width", this.borderWidth + "px");

        //white circle inside the thermometer expansion chamber semi-sphere to cover up the capillary tube rectangle edge line
        this.svg.append("circle")
            .attr("class", "innerDesign")
            .attr("cx", exp_bulbCx)
            .attr("cy", exp_bulbCy)
            .attr("r", exp_bulbR - 1);

        //thermometer spherical bulb
        this.svg.append("circle")
            .attr("class", "thermometerStructure innerDesign")
            .attr("cx", bulbCx)
            .attr("cy", bulbCy)
            .attr("r", bulbR)
            .style("stroke-width", this.borderWidth + "px");

        //white rect inside the thermometer capillary tube rectangle to cover up the spherical bulb line
        this.svg.append("rect")
            .attr("class", "innerDesign mercuryControl")
            .attr("x", this.tubeRectX)
            .attr("y", this.tubeRectY)
            .attr("width", this.tubeWidth - 1)
            .attr("height", this.tubeHeight)
            //on-click event used to regulate the mercury level
            .on('click', function (event) {
                event = event || window.event;
                this.changeMercuryColor(event);

                if (this.editTemperatureFlag) {
                    this.mouseClickY = d3.event.offsetY;
                    this.temperatureModel.value = this.getTemperatureValue();
                }
            }.bind(this));

        //filling in the mercury color inside the spherical bulb
        this.mercuryBulb = this.svg.append("circle")
            .attr("cx", bulbCx)
            .attr("cy", bulbCy)
            .attr("r", bulbR - 5)
            .style("fill", "url(#mercuryGradient_red)")
            .style("stroke", this.mercuryColor_red)
            .style("stroke-width", this.borderWidth + "px");
    },

    mercuryDragHandler: function () {
        /*
        //D3 V4 code for the first line of the drag handler
        var dragHandler = d3.drag().subject(this.subjectPos)
        */

        var dragHandler = d3.behavior.drag()
            .on("drag", function () {
                //this._draggable.disable();
                if (this.editTemperatureFlag) {
                    this.mouseClickY = d3.event.offsetY || d3.event.y;
                    var temperatureValueTemp = this.getTemperatureValue();

                    var minDomain = this.universalScale.domain()[0].toFixed(2);
                    var maxDomain = this.universalScale.domain()[1].toFixed(2);
                    //max temperature condition - this.mouseClickY will be updated accordingly
                    if (temperatureValueTemp >= parseFloat(maxDomain)) {
                        this.mouseClickY = this.universalScale(parseFloat(maxDomain)) + this.tubeRectY;
                    }
                    //min temperature condition - this.mouseClickY will be updated accordingly
                    else if (temperatureValueTemp <= parseFloat(minDomain)) {
                        this.mouseClickY = this.universalScale(parseFloat(minDomain)) + this.tubeRectY;
                    }
                    //updating the model with the updated temperature obtained after computing the new this.mouseClickY
                    this.temperatureModel.value = this.getTemperatureValue();
                }
                //this._draggable.enable();
            }.bind(this));
        return dragHandler;
    },

    /*
    //D3 V4 function for the drag handler
    subjectPos: function (d) { return { x: 0, y: d3.event.y } },
    */

    mercuryDesign: function () {
        var dragHandler = this.mercuryDragHandler();

        //mercury rect vars
        var mercuryRectX = this.tubeRectX + 3;
        var mercuryRectY = this.tubeRectY;
        var mercuryRectWidth = this.tubeWidth - 7;
        var mercuryRectHeight = this.tubeHeight + this.tubeBulbPixelOverlap;

        //drag bar vars
        //var dragBarWidth = this.tubeWidth + 15;
        //var dragBarHeight = 5;

        //red rect inside the thermometer capillary tube rectangle to act as the mercury
        this.mercury = this.svg.append("rect")
            .attr("class", "mercuryControl")
            .attr("id", "dragMercuryRect")
            .attr("x", mercuryRectX)
            .attr("y", mercuryRectY)
            .attr("width", mercuryRectWidth)
            .attr("height", mercuryRectHeight)
            .attr("fill", this.mercuryColor_red)
            .call(dragHandler);

        //mercury drag bar
        //var mercuryDragBar = this.svg.append("rect")
        //    .attr("id", "dragRect")
        //    .attr("x", this.mercuryRectX - (dragBarWidth / 3))
        //    .attr("y", this.mercuryRectY - (dragBarHeight / 2))
        //    .attr("width", dragBarWidth)
        //    .attr("height", dragBarHeight)
        //    .attr("fill", "green")
        //    .call(dragHandler);

        //on-click event used to regulate the mercury level
        this.mercury.on('click', function () {
            if (this.editTemperatureFlag) {
                this.mouseClickY = d3.event.offsetY;
                this.temperatureModel.value = this.getTemperatureValue();
            }
        }.bind(this));

        this.mercury.on('mousedown', function (event) {
            event = event || window.event;
            this.changeMercuryColor(event);
        }.bind(this));
    },

    axisInitialization: function () {
        //Defining the axis and along with it setting up the universal scale with the selectedUnit
        var axis = this.displayAxis();

        // Add the axis to the image
        var svgAxis = this.svg.append("g")
            .attr("class", "temperatureAxis")
            .style("font-size", "10px")
            .attr("id", "tempScale")
            .attr("transform", "translate(" + (this.tubeRectX - 1) + "," + this.tubeRectY + ")")
            .call(axis);

        //remove the scale line
        svgAxis.select("path")
            .style("stroke", "none")
            .style("fill", "none");
    },
});

// add temperature constructor for temperature control
L.control.temp = function (parentElement) {
    return new L.Control.Temp({ element: parentElement });
};