var ScenarioControlsManager = {
    scenarios: [
        { quarter: "Q1", year: "2019", name: "Q1<br/>2019", controls: { "1": { checked: true }, "7": { checked: true }, "8": { checked: true }, "11": { checked: true } }, id: 1 },
        { quarter: "Q2", year: "2019", name: "Q2<br/>2019", controls: { "4": { checked: true }, "5": { checked: true }, "6": { checked: true }, "7": { checked: true }, "8": { checked: true }, "9": { checked: true }, "10": { checked: true } }, id: 2 },
        { quarter: "Q3", year: "2019", name: "Q3<br/>2019", controls: { "1": { checked: true }, "2": { checked: true }, "5": { checked: true }, "17": { checked: true }, "16": { checked: true } }, id: 3 },
        { quarter: "Q4", year: "2019", name: "Q4<br/>2019", controls: { "2": { checked: true }, "5": { checked: true } }, id: 4 },
        { quarter: "Q1", year: "2020", name: "Q1<br/>2020", controls: { "1": { checked: true }, "10": { checked: true }, "14": { checked: true } }, id: 5 },
        { quarter: "Q2", year: "2020", name: "Q2<br/>2020", controls: { "4": { checked: true }, "6": { checked: true }, "8": { checked: true }, "11": { checked: true }, "12": { checked: true }, "16": { checked: true } }, id: 6 },
        { quarter: "Q3", year: "2020", name: "Q3<br/>2020", controls: { "2": { checked: true }, "3": { checked: true }, "4": { checked: true }, "12": { checked: true }, "13": { checked: true } }, id: 7 },
        { quarter: "Q4", year: "2020", name: "Q4<br/>2020", controls: { "13": { checked: true } }, id: 8 },
        { quarter: "Q1", year: "2021", name: "Q1<br/>2021", controls: {  }, id: 9 },
        { quarter: "Q2", year: "2021", name: "Q2<br/>2021", controls: {  }, id: 10 },
        { quarter: "Q3", year: "2021", name: "Q3<br/>2021", controls: {  }, id: 11 },
        { quarter: "Q4", year: "2021", name: "Q4<br/>2021", controls: {  }, id: 12 }
    ],
    controls: [
        { name: "Werkzaamheden HRL A10 Zuid (-3%)", id: 1 },
        { name: "Werkzaamheden HRL A10 Zuid (-5%)", id: 2 },
        { name: "Werkzaamheden HRL A10 Zuid (-10%)", id: 3 },
        { name: "Werkzaamheden HRR A10 Zuid (-3%)", id: 4 },
        { name: "Werkzaamheden HRR A10 Zuid (-5%)", id: 5 },
        { name: "Werkzaamheden A9 BaHo (-5%)", id: 6 },
        { name: "Werkzaamheden A9 BaHo (-10%)", id: 7 },
        { name: "Werkzaamheden PH-Tunnel (-100%)", id: 8 },
        { name: "Werkzaamheden Metro (-AVL%)", id: 9 },
        { name: "Werkzaamheden S108 (+S108)", id: 10 },
        { name: "Werkzaamheden S108 (+S109)", id: 11 },
        { name: "Werkzaamheden treinstation zuidas (-30%)", id: 12 },
        { name: "Uitbereiding HRR A10 Zuid (-20%)", id: 13 },
        { name: "Uitbereiding HRL A10 Zuid (+20%)", id: 14 },
        { name: "Uitbereiding A9 BaHo (+20%)", id: 15 },
        { name: "Uitbereiding Metro (+AVL)", id: 16 },
        { name: "Uitbereiding treinstation zuidas (+10%)", id: 17 }
    ],

    activeScenarioId: 3,

    showScenarios: function (options) {
        //this.controls = options.controls;

        //this.scenarios = options.scenarios;

        var div = modalDialogCreate('Scenario Management', 'Planning');
        div.style.width = '800px';
        div.style.margin = '5% auto';

        this.buildScenariosTable(div);
        
        var mddb = div.appendChild(document.createElement('div'));
        mddb.className = 'modalDialogDevideButtons';
        var _this = this;
        modelDialogAddButton(mddb, 'Close', modalDialogClose);
        //modelDialogAddButton(mddb, 'Apply', function () {
        //    //todo: implement Apply
        //    modalDialogClose();
        //});
    },

    buildScenariosTable: function (div) {
        var table = div.appendChild(document.createElement('table'));
        table.id = "scenarioControlsTable";

        var head = table.appendChild(document.createElement('thead'));
        var tr = head.appendChild(document.createElement('tr'));

        var th = tr.appendChild(document.createElement('th'));

        for (var i = 0; i < this.scenarios.length; i++)
        {
            th = tr.appendChild(document.createElement('th'));
            th.id = "scenarioTableHeader" + i;
            th.className = "clickableHeader";
            th.scenario = this.scenarios[i];
            //if (this.scenarios[i].id == this.activeScenarioId)
            //    th.className = "thActive";
            var thText = th.appendChild(document.createElement('span'));
            thText.innerHTML = this.scenarios[i].name;
            th.addEventListener("click", (function (e) {
                this.activeScenarioId = e.currentTarget.scenario.id;
                modalDialogClose();
            }).bind(this));
        }

        var body = table.appendChild(document.createElement('tbody'));
        for (var i = 0; i < this.controls.length; i++)
        {
            var control = this.controls[i];
            tr = body.appendChild(document.createElement('tr'));
            td = tr.appendChild(document.createElement('td'));
            td.className = "scenarioTableControlText"
            var tdText = td.appendChild(document.createElement('span'));
            tdText.innerHTML = this.controls[i].name;
            for (var j = 0; j < this.scenarios.length; j++)
            {
                var scenario = this.scenarios[j];
                td = tr.appendChild(document.createElement('td'));
                td.headers = "scenarioTableHeader" + j;
                var input = td.appendChild(document.createElement('input'));
                input.setAttribute("type", "checkbox");
                input.className = "scenarioTableInput";
                input.checked = (typeof this.scenarios[j].controls[this.controls[i].id] !== "undefined");
                input.control = control
                input.scenario = scenario;
                //if (this.activeScenarioId == scenario.id)
                //{
                //    input.addEventListener("click", function (e) {
                //        var input = e.currentTarget;
                //        if (input.checked) {
                //            input.scenario.controls[input.control.id] = { checked: true };
                //        }
                //        else {
                //            delete input.scenario.controls[input.control.id];
                //        }
                //    });
                //    td.className = "tdActive";
                //}
                //else
                    input.disabled = true;
            }
        }
    },

    showScenario: function (id) {
        var div = modalDialogCreate('Scenario Management', 'Planning');
        div.style.width = '800px';
        div.style.margin = '5% auto';

        var scenario = this.scenarios[0];

        for (var i = 0; i < this.scenarios.length; i++)
        {
            if (this.scenarios[i].id == this.activeScenarioId)
            {
                scenario = this.scenarios[i]
                break;
            }
        }

        this.buildScenarioTable(div, scenario);

        var mddb = div.appendChild(document.createElement('div'));
        mddb.className = 'modalDialogDevideButtons';
        var _this = this;
        modelDialogAddButton(mddb, 'Close', modalDialogClose);
        modelDialogAddButton(mddb, 'Apply', function () {
            //todo: implement Apply
            modalDialogClose();
        });
    },

    buildScenarioTable: function (div, scenario) {
        var table = div.appendChild(document.createElement('table'));
        table.id = "scenarioControlsTable";

        var head = table.appendChild(document.createElement('thead'));
        var tr = head.appendChild(document.createElement('tr'));

        var th = tr.appendChild(document.createElement('th'));
        
        th = tr.appendChild(document.createElement('th'));
        th.scenario = scenario;
        //if (this.scenarios[i].id == this.activeScenarioId)
        //    th.className = "thActive";
        var thText = th.appendChild(document.createElement('span'));
        thText.innerHTML = scenario.name;

        var body = table.appendChild(document.createElement('tbody'));
        for (var i = 0; i < this.controls.length; i++) {
            var control = this.controls[i];
            tr = body.appendChild(document.createElement('tr'));
            td = tr.appendChild(document.createElement('td'));
            td.className = "scenarioTableControlText"
            var tdText = td.appendChild(document.createElement('span'));
            tdText.innerHTML = this.controls[i].name;
            td = tr.appendChild(document.createElement('td'));
            var input = td.appendChild(document.createElement('input'));
            input.setAttribute("type", "checkbox");
            input.className = "scenarioTableInput";
            input.checked = (typeof scenario.controls[this.controls[i].id] !== "undefined");
            input.control = control
            input.scenario = scenario;
            input.addEventListener("click", function (e) {
                var input = e.currentTarget;
                if (input.checked) {
                    input.scenario.controls[input.control.id] = { checked: true };
                }
                else {
                    delete input.scenario.controls[input.control.id];
                }
            });
        }
    }


}