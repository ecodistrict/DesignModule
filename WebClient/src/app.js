/**
 * Application entry point
 */

import './assets/style/main.css';
import './utils/polyfill';
import PerimeterLayout from './core/window/perimeterLayout';
import WindowManager from './core/window/windowManager';
import TimeSliderController from './components/timeSlider/timeSliderController';
import GraphComponent from './components/graph/graphComponent';
import DetailsViewController from './components/details/detailsViewController';
import ModuleService from './components/moduleControl/moduleService';
import ModuleControlComponent from './components/moduleControl/moduleControlComponent';

var wsp = 'https:' == document.location.protocol ? 'wss' : 'ws';
// determine parameters and config
window.wsBaseURL = wsp + '://vps17642.public.cloudvps.com';
window.session = getParameterByName('session', '');
if (session == '') {
    //session = '6B84B11553BC4AA6B2B6A0759F6EB278'; // us_demo
    //session = '1234$undefined$57b4281041e330e716b22a74'; // default test session
    //session = '5721de16eae0e0c543f5234f$undefined$5714e0ccb83ce5066723bf41'; // ecodistrict warsaw
    //session = '57b428a6cef25a0a0d6681ac$undefined$57b4281041e330e716b22a74'; // ecodistrict antwerpen
    //session = '57b428a6cef25a0a0d6681ac$57f7721a38ddeb850af42d13$57b4281041e330e716b22a74'; // ecodistrict antwerpen alt 1
    //session = '4B95BE74F9A44DA0908A30B27C3E8C99'; // schiedam
    //session = '80BD7A788D6B40A4B3608AA95C26E68F'; // us_utrecht2017
    //session = '7435449FACA0425A88E072CAAC549E2B'; // us_ams_2017 Design
    //session = '7435449FACA0425A88E072CAAC549E2B-Design'; // us_ams_2017 Design
    //session = '7435449FACA0425A88E072CAAC549E2B-Debug-Design'; // us_ams_2017 Design debug version
    //session = '7435449FACA0425A88E072CAAC549E2B-Monitor'; // us_ams_2017 Monitor
    //session = '7435449FACA0425A88E072CAAC549E2B-Evaluate'; // us_ams_2017 Evaluate
    //session = '6DB7603E76694E2AA06F3F7F199EF973'; //us_ams_2017 Monitor
    //session = "sesmi${DE087221-5BB1-40AC-90A9-371CA39A399F}$testexpert"; // test expert user
    //session = "sesmi${d3d55d19-6568-4836-aa00-c2e88fd31fe9}$testuser"; // test guid from db
    //session = "sesmi${6051026f-3b94-4291-88fd-f4c26d5804dd}$testuser"; //test sensa-guid
    //session = "sesmi${9F180B74-FE6A-4D66-9736-F6C9C59E87DD}$fietsuser"; //sensor box
    //session = '9AADAB9F472D4E1EAA1DE7CE202780AB'; // response production
    //session = '5904DD8ACDD54E15A3A8AA8EA55C0C64'; // response test
    //session = '17C9E2AE2A6341AD92EA00E9D3BDC2BB'; //  response production DynamicPlumes
    //session = '15B0B9AD002343459B36471524BC0FDD'; // karlsruhe
    //session = 'OmniTransConnectorA642109';
    //session = 'EarlyWarning'; //  early warning (Jan Duyzer)
    //session = '7F9B10D93ABB493F876E3D035D55269F'; // Santos (E-Bus)
    //session = '3F5435CDD5A7490DAD3A173B4548A06E'; // New Dehli
    //session = "sesmi${103ae874-daaf-40f4-8b89-a9f8fddd9644}$fietsuser"; // ensel fiets utrecht-nijmegen
    //session = "sesmi${e4fd1734-ce20-439f-bd60-b0a29fb744b6}$fietsuser"; // ensel fiets eindhoven, representatief
    //session = 'B9E738A9BCD94D3AABB051924A0E8F12'; //Hans test database design
    //session = 'C298FCB67045468CBCDA6CC2C415C041'; //Hans test database monitor
    //session = '67AC10799B044C01B552DB44283990D7'; //Hans test database evaluate
    //session = 'TestRKDesign'; //Ralph test database design
    //session = 'TestRKMonitor'; //Ralph test database monitor
    //session = 'TestRKEvaluate'; //Ralph test database evaluate
    //session = 'HenkTest';
    //session = 'D937BA6603B64147BDA2C8CFED9D973F'; //app-usmodel01 AMS
}
window.clientType = getParameterByName('clientType', '');
var sessionDescription = getParameterByName('sessionDescription', 'NOT connected to an active session');
window.scenario = '';
var userid = '';
// split session in session id/scenario id/user id
var sessionParts = session.split('$');
if (sessionParts.length > 1) {
    session = sessionParts[0];
    if (sessionParts.length > 2) {
        scenario = sessionParts[1];
        userid = sessionParts[2];
    }
    else
        userid = sessionParts[1];
}

DataManager.sessionInfo = {
    session: session,
    scenario: scenario,
    userid: userid,
    description: sessionDescription,
    referenceScenario: null
};

var layoutOptions;
if (window.innerWidth < 500) { // Mobile
    document.body.setAttribute("id", "phone");

    layoutOptions = {
        padding: {
            left: 0,
            right: 0,
            top: 0,
            botton: 0
        },
        verticalCellSpacing: 5,
        topRowCellCount: 0,
        leftColumnCellCount: 2,
        rightColumnCellCount: 0,
        maxCellWidth: window.innerWidth - 50,
        maxCellHeight: window.innerHeight / 3,
        minCellWidth: 150,
        minCellHeight: 80
    };
} else {
    document.body.removeAttribute("id");

    layoutOptions = {
        padding: {
            left: 0,
            right: 0,
            top: 0,
            botton: 0
        },
        verticalCellSpacing: 8,
        horizontalCellSpacing: 15,
        topRowCellCount: 2,
        leftColumnCellCount: 4,
        rightColumnCellCount: 4,
        maxCellWidth: null,
        maxCellHeight: null,
        minCellWidth: 150,
        minCellHeight: 80
    };
}

var layout = new PerimeterLayout(layoutOptions);
window.windowManager = new WindowManager({
    element: document.querySelector('#windows-area'),
    layout: layout
});

// top-right


// domains
window.domainsControl = L.control.domains({});
map.addControl(domainsControl);

DataManager.detailsInfo = {
    elementWidth: 140,
    kpiWidth: 134, kpiHeight: 40,
    chartWidth: 134, chartHeight: 100,
    layerWidth: 134, layerHeight: 140,
    padding: 2,
    border: 1,
    graphMargin: 5
}

if (window.innerWidth < 500) {
    DataManager.detailsInfo.chartWidth = 20;
    DataManager.detailsInfo.chartHeight = 20;
}

// graphs
window.graphComponent = new GraphComponent({
    windowManager: windowManager
});
window.graphService = graphComponent.service();

// details
window.detailsViewController = new DetailsViewController({
    graphService: graphComponent.service(),
    graphViewManager: graphComponent.graphViewManager(),
    graphPreviewFactory: graphComponent.graphPreviewFactory()
});
map.addControl(window.detailsViewController.view());

// details
window.detailsControl = L.control.details(DataManager.detailsInfo);
map.addControl(detailsControl);


//history
window.historyControl = L.control.history(baseLayers); // todo (NK): why pass baseLayers as options?
map.addControl(historyControl);
map.removeControl(historyControl);
DataManager.HistoryControl = historyControl;

//presenter/viewer
window.presenterViewerControl = L.control.PresenterViewer();
map.addControl(presenterViewerControl);
map.removeControl(presenterViewerControl);
DataManager.presenterViewerControl = presenterViewerControl;


// top-left

//controls
DataManager.controlsControl = L.control.ControlsControl();
//map.addControl(controlsControl);

DataManager.overviewControl = L.control.OverviewControl();
//map.addControl(overviewControl);

//measures
window.measuresControl = L.control.measures([], historyControl);
map.addControl(measuresControl);
map.removeControl(measuresControl);
measuresControl.setSelectCategories([]);

DataManager.wind = L.control.arrow();//L.control.wind({});
//map.addControl(DataManager.wind);

//timeRangeSlider
//DataManager.timeRangeSlider = new L.Control.TimeRangeSlider();
//map.addControl(DataManager.timeRangeSlider);

// test vertical slider
//DataManager.timeRangeSliderV = new L.Control.TimeRangeSlider({ orientation: 'vertical' });
//map.addControl(DataManager.timeRangeSliderV);

// bottom-right (reverse order)

window.timeSliderController = new TimeSliderController({
    map: map
});

/*var timesliderControl = L.control.timeslider(timeslider);
map.addControl(timesliderControl);*/

// module control
window.moduleControlComponent = new ModuleControlComponent({
    map: map,
    windowManager: windowManager,
    toastFunction: AddErrorMessage
});
window.moduleService = moduleControlComponent.service();

// current-reference-difference control
window.crd = L.control.crd({ parent: currefdiff });
map.addControl(crd);

detailsControl.setCRD(crd);

// bottom-left (reverse order)

// info
window.infoControl = L.control.info();
map.addControl(infoControl);

// legend
window.legendControl = L.control.legend;
map.addControl(legendControl);

// files
window.filesControl = L.control.files();
DataManager.filesControl = filesControl;

// other

// project description
window.projectDescription = L.control.projectDescription;
projectDescription.options.description = sessionDescription;
map.addControl(projectDescription);

map.addEventListener('overlayadd',
    function (e) {
        if (e.layer.basicLayer && e.layer.basicID)
            DataManager.activeBasicLayers[e.layer.basicID] = e.layer.basicID;
    });

map.addEventListener('overlayremove',
    function (e) {
        if (e.layer.basicLayer && e.layer.basicID)
            delete DataManager.activeBasicLayers[e.layer.basicID];
    });

// link up connection control
window.connectionStatus = document.getElementById('connectionStatus');
connectionStatus.classList.add('connection-status-disconnected');
connectionStatus.onclick = function (e) {
    if (e.target.classList.contains('connection-status-disconnected'))
        wsConnect();
    else
        wsClose();
};

// default: try to connect
wsConnect();

/**
 * Expose webpack require() method to the global scope.
 * This module is for debugging only and should not be included into production build.
 */
if (!PRODUCTION) {
    window.require = function (module) {
        /* jshint -W106 */
        return __webpack_require__(module).default;
    };
}
