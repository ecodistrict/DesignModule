var graphObjectSpider = {};
var testData = {};

function generateTestData() {
  datasetTest = [];
  var cats = ["Mobility performance","Quality of life","Economic Success","Global Environment"]
  var subcats = ["Congestion and delay","Intermodel integration","Accessibility","Commuting travel time","Robustness","Reliability","Air pollution emissions","Noise hindrance","Heat islands","Green and blue areas","Mobility space uses","Opportunity for active mobility","Green and blue areas"]
  var situationCount = Math.floor((Math.random() * 3) + 1);

  var situationWrapper = [];
  var situation = [];
  for (var i=1; i < situationCount + 1; i++) {
    if (typeof situation[i] === "undefined") {
      situation[i] = [];
    }
    for (var i2 = 1; i2 < Math.floor(Math.random() * (20 - 10) + 10); i2++) {
      situation[i].push({
        "name" : "situation "+ i,
        "cat" : cats[Math.floor((Math.random() * cats.length))],
        "subcat" : subcats[Math.floor((Math.random() * subcats.length))],
        "value" : Math.floor((Math.random() * 10))
      });
    }
    situationWrapper.push(situation[i]);
  }
  testData.cfg = {};
  var margin = {top: 100, right: 100, bottom: 100, left: 100},
  width = 201, // 270
  height = 201;
  testData.cfg = {
    w: height,
    h: width,
    maxValue: 10, //What is the value that the biggest circle will represent
    levels: 5, //How many levels or inner circles should there be drawn
    labelFactor: 1.22, 	//How much farther than the radius of the outer circle should the labels be placed
    margin: margin,
    roundStrokes: true, //If true the area and stroke will follow a round path (cardinal-closed)
    color: d3.scale.category10(), //Color function
    wrapWidth: 60, 		//The number of pixels after which a label needs to be given a new line
    opacityArea: 0.35, 	//The opacity of the area of the blob
    dotRadius: 4, 			//The size of the colored circles of each blog
    opacityCircles: 0.1, 	//The opacity of the circles of each blob
    strokeWidth: 2, 		//The width of the stroke around each blob
    fontSize:  '11px', 		//The width of the stroke around each blob
  }

  testData.type = "spider";
  testData.data = setLevelData(0, false, situationWrapper);
  testData.level = 0;
  testData.legend = true;
  testData.labels = true;
  testData.divWidth = 240;
  testData.divHeight = 240;
  testData.width = 400;
  testData.height = 400;
  testData.dataset = situationWrapper;
  testData.clickedSituation = 0;
  testData.maxValue = 10;
  testData.name = "Air humidity";

  var graphObjectSpider2 = JSON.parse(JSON.stringify(testData));
  graphObjectSpider2.cfg = {};
  graphObjectSpider2.cfg = testData.cfg;
  graphObjectSpider2.id =  "test" + Math.floor((Math.random() * 100));


  GraphManager.MakeGraph(graphObjectSpider2);

}



dataSet = [[
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Congestion and delay", "value" : 5},
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Intermodel integration", "value" : 10},
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Accessibility", "value" : 4},
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Commuting travel time", "value" : 3},
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Robustness", "value" : 6},
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Reliability", "value" : 6},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Air pollution emissions", "value" : 5},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Noise hindrance", "value" : 9},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Heat islands", "value" : 8},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Green and blue areas", "value" : 7},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Mobility space uses", "value" : 7},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Opportunity for active mobility", "value" : 7},
  {"name":"situatie 1", "cat" : "Economic Success", "subcat" : "Commuting travel time", "value" : 4},
  {"name":"situatie 1", "cat" : "Economic Success", "subcat" : "Mobility space usage", "value" : 8},
  {"name":"situatie 1", "cat" : "Economic Success", "subcat" : "Urban functional diversity", "value" : 1},
  {"name":"situatie 1", "cat" : "Global Environment", "subcat" : "Emission of GHG", "value" : 2},
  {"name":"situatie 1", "cat" : "Global Environment", "subcat" : "Heat islands", "value" : 7},
  {"name":"situatie 1", "cat" : "Global Environment", "subcat" : "Opportunity for active mobility", "value" : 8},
  {"name":"situatie 1", "cat" : "Global Environment", "subcat" : "Green and blue areas", "value" : 8}
],[
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Congestion and delay", "value" : 4},
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Intermodel integration", "value" : 3},
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Accessibility", "value" : 3},
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Commuting travel time", "value" : 2},
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Robustness", "value" : 1},
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Reliability", "value" : 6},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Air pollution", "value" : 10},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Noise hindrance", "value" : 7},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Heat islands", "value" : 9},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Green and blue areas", "value" : 6},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Mobility space uses", "value" : 6},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Opportunity for active mobility", "value" : 8},
  {"name":"situatie 2", "cat" : "Economic Success", "subcat" : "Commuting travel time", "value" : 5},
  {"name":"situatie 2", "cat" : "Economic Success", "subcat" : "Mobility space usage", "value" : 6},
  {"name":"situatie 2", "cat" : "Economic Success", "subcat" : "Urban functional diversity", "value" : 7},
  {"name":"situatie 2", "cat" : "Global Environment", "subcat" : "Emission of GHG", "value" : 6},
  {"name":"situatie 2", "cat" : "Global Environment", "subcat" : "Heat islands", "value" : 8},
  {"name":"situatie 2", "cat" : "Global Environment", "subcat" : "Opportunity for active mobility", "value" : 9},
  {"name":"situatie 2", "cat" : "Global Environment", "subcat" : "Green and blue areas", "value" : 8}
],[
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Congestion and delay", "value" : 10},
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Intermodel integration", "value" : 9},
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Accessibility", "value" :8},
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Commuting travel time", "value" : 7},
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Robustness", "value" : 10},
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Reliability", "value" : 9},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Air pollution", "value" : 6},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Noise hindrance", "value" : 7},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Heat islands", "value" : 9},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Green and blue areas", "value" : 8},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Mobility space uses", "value" :8},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Opportunity for active mobility", "value" : 6},
  {"name":"situatie 3", "cat" : "Economic Success", "subcat" : "Commuting travel time", "value" : 5},
  {"name":"situatie 3", "cat" : "Economic Success", "subcat" : "Mobility space usage", "value" : 4},
  {"name":"situatie 3", "cat" : "Economic Success", "subcat" : "Urban functional diversity", "value" : 6},
  {"name":"situatie 3", "cat" : "Global Environment", "subcat" : "Emission of GHG", "value" : 3},
  {"name":"situatie 3", "cat" : "Global Environment", "subcat" : "Heat islands", "value" : 2},
  {"name":"situatie 3", "cat" : "Global Environment", "subcat" : "Opportunity for active mobility", "value" : 1},
  {"name":"situatie 3", "cat" : "Global Environment", "subcat" : "Green and blue areas", "value" : 2}
]];


dataSet1 = [[
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Congestion and delay", "value" : 5},
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Intermodel integration", "value" : 10},
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Accessibility", "value" : 4},
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Commuting travel time", "value" : 3},
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Robustness", "value" : 6},
  {"name":"situatie 1", "cat" : "Mobility performance", "subcat" : "Reliability", "value" : 6},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Air pollution emissions", "value" : 5},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Noise hindrance", "value" : 9},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Heat islands", "value" : 8},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Green and blue areas", "value" : 7},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Mobility space uses", "value" : 7},
  {"name":"situatie 1", "cat" : "Quality of life", "subcat" : "Opportunity for active mobility", "value" : 7},
  {"name":"situatie 1", "cat" : "Economic Success", "subcat" : "Commuting travel time", "value" : 4},
  {"name":"situatie 1", "cat" : "Economic Success", "subcat" : "Mobility space usage", "value" : 8},
  {"name":"situatie 1", "cat" : "Economic Success", "subcat" : "Urban functional diversity", "value" : 1},
  {"name":"situatie 1", "cat" : "Global Environment", "subcat" : "Emission of GHG", "value" : 2},
  {"name":"situatie 1", "cat" : "Global Environment", "subcat" : "Heat islands", "value" : 7},
  {"name":"situatie 1", "cat" : "Global Environment", "subcat" : "Opportunity for active mobility", "value" : 8},
  {"name":"situatie 1", "cat" : "Global Environment", "subcat" : "Green and blue areas", "value" : 8}
],[
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Congestion and delay", "value" : 4},
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Intermodel integration", "value" : 3},
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Accessibility", "value" : 3},
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Commuting travel time", "value" : 2},
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Robustness", "value" : 1},
  {"name":"situatie 2", "cat" : "Mobility performance", "subcat" : "Reliability", "value" : 6},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Air pollution", "value" : 10},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Noise hindrance", "value" : 7},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Heat islands", "value" : 9},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Green and blue areas", "value" : 6},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Mobility space uses", "value" : 6},
  {"name":"situatie 2", "cat" : "Quality of life", "subcat" : "Opportunity for active mobility", "value" : 8},
  {"name":"situatie 2", "cat" : "Economic Success", "subcat" : "Commuting travel time", "value" : 5},
  {"name":"situatie 2", "cat" : "Economic Success", "subcat" : "Mobility space usage", "value" : 6},
  {"name":"situatie 2", "cat" : "Economic Success", "subcat" : "Urban functional diversity", "value" : 7},
  {"name":"situatie 2", "cat" : "Global Environment", "subcat" : "Emission of GHG", "value" : 6},
  {"name":"situatie 2", "cat" : "Global Environment", "subcat" : "Heat islands", "value" : 8},
  {"name":"situatie 2", "cat" : "Global Environment", "subcat" : "Opportunity for active mobility", "value" : 9},
  {"name":"situatie 2", "cat" : "Global Environment", "subcat" : "Green and blue areas", "value" : 8}
],[
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Congestion and delay", "value" : 10},
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Intermodel integration", "value" : 9},
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Accessibility", "value" :8},
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Commuting travel time", "value" : 7},
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Robustness", "value" : 10},
  {"name":"situatie 3", "cat" : "Mobility performance", "subcat" : "Reliability", "value" : 9},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Air pollution", "value" : 6},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Noise hindrance", "value" : 7},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Heat islands", "value" : 9},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Green and blue areas", "value" : 8},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Mobility space uses", "value" :8},
  {"name":"situatie 3", "cat" : "Quality of life", "subcat" : "Opportunity for active mobility", "value" : 6},
  {"name":"situatie 3", "cat" : "Economic Success", "subcat" : "Commuting travel time", "value" : 5},
  {"name":"situatie 3", "cat" : "Economic Success", "subcat" : "Mobility space usage", "value" : 4},
  {"name":"situatie 3", "cat" : "Economic Success", "subcat" : "Urban functional diversity", "value" : 6},
  {"name":"situatie 3", "cat" : "Global Environment", "subcat" : "Emission of GHG", "value" : 3},
  {"name":"situatie 3", "cat" : "Global Environment", "subcat" : "Heat islands", "value" : 2},
  {"name":"situatie 3", "cat" : "Global Environment", "subcat" : "Opportunity for active mobility", "value" : 1},
  {"name":"situatie 3", "cat" : "Global Environment", "subcat" : "Green and blue areas", "value" : 2}
],[
  {"name":"situatie 4", "cat" : "Mobility performance", "subcat" : "Congestion and delay", "value" : 5},
  {"name":"situatie 4", "cat" : "Mobility performance", "subcat" : "Intermodel integration", "value" : 2},
  {"name":"situatie 4", "cat" : "Mobility performance", "subcat" : "Accessibility", "value" :5},
  {"name":"situatie 4", "cat" : "Mobility performance", "subcat" : "Commuting travel time", "value" : 2},
  {"name":"situatie 4", "cat" : "Mobility performance", "subcat" : "Robustness", "value" : 5},
  {"name":"situatie 4", "cat" : "Mobility performance", "subcat" : "Reliability", "value" : 2},
  {"name":"situatie 4", "cat" : "Quality of life", "subcat" : "Air pollution", "value" : 8},
  {"name":"situatie 4", "cat" : "Quality of life", "subcat" : "Noise hindrance", "value" : 2},
  {"name":"situatie 4", "cat" : "Quality of life", "subcat" : "Heat islands", "value" : 1},
  {"name":"situatie 4", "cat" : "Quality of life", "subcat" : "Green and blue areas", "value" : 4},
  {"name":"situatie 4", "cat" : "Quality of life", "subcat" : "Mobility space uses", "value" :2},
  {"name":"situatie 4", "cat" : "Quality of life", "subcat" : "Opportunity for active mobility", "value" : 1},
  {"name":"situatie 4", "cat" : "Economic Success", "subcat" : "Commuting travel time", "value" : 6},
  {"name":"situatie 4", "cat" : "Economic Success", "subcat" : "Mobility space usage", "value" : 8},
  {"name":"situatie 4", "cat" : "Economic Success", "subcat" : "Urban functional diversity", "value" : 3},
  {"name":"situatie 4", "cat" : "Global Environment", "subcat" : "Emission of GHG", "value" : 5},
  {"name":"situatie 4", "cat" : "Global Environment", "subcat" : "Heat islands", "value" : 8},
  {"name":"situatie 4", "cat" : "Global Environment", "subcat" : "Opportunity for active mobility", "value" : 9},
  {"name":"situatie 4", "cat" : "Global Environment", "subcat" : "Green and blue areas", "value" : 10}
],[
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Congestion and delay", "value" : 5},
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Intermodel integration", "value" : 5},
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Accessibility", "value" :5},
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Commuting travel time", "value" : 5},
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Robustness", "value" : 5},
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Reliability", "value" : 5},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Air pollution", "value" : 5},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Noise hindrance", "value" : 5},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Heat islands", "value" : 5},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Green and blue areas", "value" : 5},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Mobility space uses", "value" :5},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Opportunity for active mobility", "value" : 5},
  {"name":"situatie 5", "cat" : "Economic Success", "subcat" : "Commuting travel time", "value" : 5},
  {"name":"situatie 5", "cat" : "Economic Success", "subcat" : "Mobility space usage", "value" : 5},
  {"name":"situatie 5", "cat" : "Economic Success", "subcat" : "Urban functional diversity", "value" : 5},
  {"name":"situatie 5", "cat" : "Global Environment", "subcat" : "Emission of GHG", "value" : 5},
  {"name":"situatie 5", "cat" : "Global Environment", "subcat" : "Heat islands", "value" : 5},
  {"name":"situatie 5", "cat" : "Global Environment", "subcat" : "Opportunity for active mobility", "value" : 5},
  {"name":"situatie 5", "cat" : "Global Environment", "subcat" : "Green and blue areas", "value" : 12}
],[
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Congestion and delay", "value" : 1},
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Intermodel integration", "value" : 3},
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Accessibility", "value" :1},
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Commuting travel time", "value" : 3},
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Robustness", "value" : 1},
  {"name":"situatie 5", "cat" : "Mobility performance", "subcat" : "Reliability", "value" : 1},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Air pollution", "value" : 10},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Noise hindrance", "value" : 10},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Heat islands", "value" : 12},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Green and blue areas", "value" : 8},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Mobility space uses", "value" :9},
  {"name":"situatie 5", "cat" : "Quality of life", "subcat" : "Opportunity for active mobility", "value" : 11},
  {"name":"situatie 5", "cat" : "Economic Success", "subcat" : "Commuting travel time", "value" : 10},
  {"name":"situatie 5", "cat" : "Economic Success", "subcat" : "Mobility space usage", "value" : 10},
  {"name":"situatie 5", "cat" : "Economic Success", "subcat" : "Urban functional diversity", "value" : 10},
  {"name":"situatie 5", "cat" : "Global Environment", "subcat" : "Emission of GHG", "value" : 3},
  {"name":"situatie 5", "cat" : "Global Environment", "subcat" : "Heat islands", "value" : 4},
  {"name":"situatie 5", "cat" : "Global Environment", "subcat" : "Opportunity for active mobility", "value" : 6},
  {"name":"situatie 5", "cat" : "Global Environment", "subcat" : "Green and blue areas", "value" : 3}
]];

//
// dataSet = [[
//   {"name":"situatie 1", "cat" : "0 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "1 uur", "subcat" : "Congestion and delay", "value" : 5},
//   {"name":"situatie 1", "cat" : "2 uur", "subcat" : "Intermodel integration", "value" : 10},
//   {"name":"situatie 1", "cat" : "3 uur", "subcat" : "Accessibility", "value" : 4},
//   {"name":"situatie 1", "cat" : "4 uur", "subcat" : "Commuting travel time", "value" : 3},
//   {"name":"situatie 1", "cat" : "5 uur", "subcat" : "Robustness", "value" : 6},
//   {"name":"situatie 1", "cat" : "6 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "7 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "8 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "9 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "10 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "11 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "12 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "13 uur", "subcat" : "Congestion and delay", "value" : 5},
//   {"name":"situatie 1", "cat" : "14 uur", "subcat" : "Intermodel integration", "value" : 10},
//   {"name":"situatie 1", "cat" : "15 uur", "subcat" : "Accessibility", "value" : 4},
//   {"name":"situatie 1", "cat" : "16 uur", "subcat" : "Commuting travel time", "value" : 3},
//   {"name":"situatie 1", "cat" : "17 uur", "subcat" : "Robustness", "value" : 6},
//   {"name":"situatie 1", "cat" : "18 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "19 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "20 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "21 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "22 uur", "subcat" : "Reliability", "value" : 6},
//   {"name":"situatie 1", "cat" : "23 uur", "subcat" : "Reliability", "value" : 6}
//
// ]];



graphObjectSpider.id = "test123";
graphObjectSpider.type = "spider";
graphObjectSpider.data = setLevelData(0, false, dataSet);
graphObjectSpider.level = 0;
graphObjectSpider.legend = true;
graphObjectSpider.labels = true;
graphObjectSpider.divWidth = 240;
graphObjectSpider.divHeight = 240;
graphObjectSpider.width = 400;
graphObjectSpider.height = 400;
graphObjectSpider.dataset = dataSet;
graphObjectSpider.clickedSituation = 0;
graphObjectSpider.maxValue = 10;
graphObjectSpider.name = "Air humidity";

graphObjectSpider.cfg = {};
var margin = {top: 100, right: 100, bottom: 100, left: 100},
width = 201, // 270
height = 201;
graphObjectSpider.cfg = {
  w: height,
  h: width,
  maxValue: 10, //What is the value that the biggest circle will represent
  levels: 5, //How many levels or inner circles should there be drawn
  labelFactor: 1.22, 	//How much farther than the radius of the outer circle should the labels be placed
  margin: margin,
  roundStrokes: true, //If true the area and stroke will follow a round path (cardinal-closed)
  color: d3.scale.category10(), //Color function
  wrapWidth: 60, 		//The number of pixels after which a label needs to be given a new line
  opacityArea: 0.35, 	//The opacity of the area of the blob
  dotRadius: 4, 			//The size of the colored circles of each blog
  opacityCircles: 0.1, 	//The opacity of the circles of each blob
  strokeWidth: 2, 		//The width of the stroke around each blob
  fontSize:  '11px', 		//The width of the stroke around each blob
}

var graphObjectSpider2 = JSON.parse(JSON.stringify(graphObjectSpider));
graphObjectSpider2.cfg = {};
graphObjectSpider2.cfg = graphObjectSpider.cfg;
graphObjectSpider2.id = "test123213";
// graphObjectSpider2.data = setLevelData(0, false, dataSet2);

function updateGraph(graphId, data) {
  // updateGraph.data = setLevelData(0, false, data);\


      var graph = GraphManager._getGraph(graphId);
      if (graph !== null) { //only update graphs that exist

      d3.select(graph.graph.container).select('svg').selectAll('g').remove();

      //sets data and displaydata
      // GraphManager.AddGraphData(graph, data);
      var width = graph.graph.graphObject.container.clientWidth;
      var height = graph.graph.graphObject.container.clientHeight;
      var marginLeft = GraphManager.defaultValues.graphPadding.left + GraphManager.defaultValues.axisMargin.y;
      var marginTop = GraphManager.defaultValues.graphPadding.top;
      var marginRight = GraphManager.defaultValues.graphPadding.right;
      var marginBottom = GraphManager.defaultValues.graphPadding.bottom + GraphManager.defaultValues.axisMargin.x;;

      graph.graph.graphObject.dataset = data;
      graph.graph.graphObject.data = setLevelData(0, false, data);
      graph.graph._fillSpider(graph.graph.graphObject);

      graph.graph.graphObject.svg.attr("width", width);
      graph.graph.graphObject.svg.attr("height", height);

      graph.graph._UpdatePreview();


      }

      // graph.graph.Update(dataArray[i]);
      //GraphManager.UpdateGraph(graph.graph, dataArray[i]);


}

function setLevelData(situation, catName, dataSet) {

  if (situation === false) {
    newArrayListContainer = [];
    for (i=0; i < dataSet.length; i++) {
      var newArrayList = [];
      var count = 0;
      for (var i1 = 0; i1 < dataSet[i].length; i1++) {
        if (catName === dataSet[i][i1].cat) {
          if (dataSet[i][i1].subcat) {
            newArrayList.push({"axis":dataSet[i][i1].subcat,"value":dataSet[i][i1].value});
            count++;
          }
        }
      }
      newArrayListContainer.push(newArrayList);
    }
    if (count > 1) {
      return newArrayListContainer;
    } else {
      return false;
    }



  }


  if (!catName) {
    var avg = [];
    for (var i = 0; i < dataSet.length; i++) {
      if (!avg[i]) {
        avg[i] = [];
      }
      dataN = dataSet[i];
      var values = [];
      for (key in dataN) {
        dataRow = dataN[key];
        if (values[dataRow.cat]) {
          newVal = values[dataRow.cat].value + dataRow.value;
          count = values[dataRow.cat].count + 1;
          values[dataRow.cat] = {"value":newVal,"count":count};
        } else {
          values[dataRow.cat] = {"value":dataRow.value,"count":1};
        }
      }
      avg[i] = values;
    }

    for (var i = 0; i < avg.length; i++) {
      for (var key in avg[i]) {
        avg[i][key].value = avg[i][key].value / avg[i][key].count;
      }
    }

    var parsedData = [];
    for (var i = 0; i < avg.length; i++) {
      parsedData.push([]);
      for (var key in avg[i]) {
        parsedData[i].push({"axis":key, "value": avg[i][key].value})
      }
    }

    return parsedData;
  } else {


    var newArrayList = [];
    var count = 0;
    for (var i1 = 0; i1 < dataSet[situation].length; i1++) {
      if (catName === dataSet[situation][i1].cat) {
        if (dataSet[situation][i1].subcat) {
          newArrayList.push({"axis":dataSet[situation][i1].subcat,"value":dataSet[situation][i1].value});
          count++;
        }
      }
    }
    if (count > 1) {
      newArrayListContainer = [];
      newArrayListContainer.push(newArrayList);
      return newArrayListContainer;
    } else {
      return false;
    }
  }
}

function SpiderChart(graphObject) {
  this.graphObject = graphObject;
  this.visible = false;
  graphObject.preview = {};
  this.graphID = graphObject.id;
  this.previewDiv = null;


  this.Initialize = function (container) {
    var width = this.graphObject.width;
    var height = this.graphObject.height;
    var marginLeft = GraphManager.defaultValues.graphPadding.left + GraphManager.defaultValues.axisMargin.y;
    var marginTop = GraphManager.defaultValues.graphPadding.top;

    var svg = d3.select(container).append("svg")
    .attr("width", width)
    .attr("height", height);

    svg.className = "graph-svg";

    this.graphObject.container = container;
    this.graphObject.svg = svg;
    container.style.visibility = "hidden";
    container.graph = this;
    this.Update();

  }
  this._UpdatePreview = function () {
    if (this.previewDiv == null || this.graphObject.data == null || this.graphObject.data.length == 0)
    return;
    else {
      let found = false;
      for (var i = 0; i < this.graphObject.data.length; i++) {
        if (this.graphObject.data[i].length > 0)
        found = true;
      }
      if (!found)
      return;
    }

    var width = DataManager.detailsInfo.chartWidth;
    var height = DataManager.detailsInfo.chartHeight;


    svg = d3.select(this.previewDiv).select('svg');

    svg.selectAll('g').remove();

    // //If the supplied maxValue is smaller than the actual one, replace by the max in the data
    var maxValue = Math.max(cfg.maxValue, d3.max(graphObject.data, function(i){return d3.max(i.map(function(o){return o.value;}))}));

    var allAxis = (graphObject.data[0].map(function(i, j){return i.axis})),	//Names of each axis
    total = allAxis.length,					//The number of different axes
    // radius = Math.min(cfg.w/2, cfg.h/2), 	//Radius of the outermost circle

    radius = Math.min(width/2 -10, height/2 -10),
    Format = d3.format('.1f');
    angleSlice = Math.PI * 2 / total;		//The width in radians of each "slice"

    //Scale for the radius
    var rScale = d3.scale.linear()
    .range([0, radius])
    .domain([0, maxValue]);


    //Append a g element
    var g = svg.append("g").attr("transform", "translate(" + (width/2) + "," + (height/2) + ")");
    //Filter for the outside glow
    var filter = g.append('defs').append('filter').attr('id','glow'),
    feGaussianBlur = filter.append('feGaussianBlur').attr('stdDeviation','2.5').attr('result','coloredBlur'),
    feMerge = filter.append('feMerge'),
    feMergeNode_1 = feMerge.append('feMergeNode').attr('in','coloredBlur'),
    feMergeNode_2 = feMerge.append('feMergeNode').attr('in','SourceGraphic');

    /////////////////////////////////////////////////////////
    /////////////// Draw the Circular grid //////////////////
    /////////////////////////////////////////////////////////

    //Wrapper for the grid & axes
    var axisGrid = g.append("g").attr("class", "axisWrapper");

    //Draw the background circles
    axisGrid.selectAll(".levels")
    .data(d3.range(1,(cfg.levels+1)).reverse())
    .enter()
    .append("circle")
    .attr("class", "gridCircle")
    .attr("r", function(d, i){return radius/cfg.levels*d;})
    .style("fill", "#CDCDCD")
    .style("stroke", "#CDCDCD")
    .style("fill-opacity", cfg.opacityCircles)
    .style("filter" , "url(#glow)");

    /////////////////////////////////////////////////////////
    //////////////////// Draw the axes //////////////////////
    /////////////////////////////////////////////////////////

    //Create the straight lines radiating outward from the center
    var axis = axisGrid.selectAll(".axis")
    .data(allAxis)
    .enter()
    .append("g")
    .attr("class", "axis");
    //Append the lines
    axis.append("line")
    .attr("x1", 0)
    .attr("y1", 0)
    .attr("x2", function(d, i){ return rScale(maxValue*1.1) * Math.cos(angleSlice*i - Math.PI/2); })
    .attr("y2", function(d, i){ return rScale(maxValue*1.1) * Math.sin(angleSlice*i - Math.PI/2); })
    .attr("class", "line")
    .style("stroke", "white")
    .style("stroke-width", "2px");


    //The radial line function
    var radarLine = d3.svg.line.radial()
    .interpolate("linear-closed")
    .radius(function(d) { return rScale(d.value); })
    .angle(function(d,i) {	return i*angleSlice; });

    if(cfg.roundStrokes) {
      radarLine.interpolate("cardinal-closed");
    }

    //Create a wrapper for the blobs
    var blobWrapper = g.selectAll(".radarWrapper")
    .data(graphObject.data)
    .enter().append("g")
    .attr("class", "radarWrapper");

    //Append the backgrounds
    blobWrapper
    .append("path")
    .attr("class", "radarArea")
    .attr("d", function(d,i) { return radarLine(d); })
    .style("fill-opacity", cfg.opacityArea);
    blobWrapper.style("fill", function(d,i) { return cfg.color(i); })
    .style("stroke", function(d,i) { return cfg.color(i); })


    //Create the outlines
    blobWrapper.append("path")
    .attr("class", "radarStroke")
    .attr("d", function(d,i) { return radarLine(d); })
    .style("stroke-width", cfg.strokeWidth + "px")
    .style("fill", "none")
    .style("filter" , "url(#glow)");

    //Append the circles

    blobWrapper.selectAll(".radarCircle")
    .data(function(d,i) { return d; })
    .enter().append("circle")
    .attr("class", "radarCircle")
    .attr("r", cfg.dotRadius * 0.5)
    .attr("cx", function(d,i){ return rScale(d.value) * Math.cos(angleSlice*i - Math.PI/2); })
    .attr("cy", function(d,i){ return rScale(d.value) * Math.sin(angleSlice*i - Math.PI/2); })
    .style("fill-opacity", 0.8).style("fill", function(d,i,j) { return cfg.color(j); })



  }
  this.GetPreview = function(container)    {

    if (this.previewDiv != null)
    return container.appendChild(this.previewDiv);

    var previewContainer = container.appendChild(document.createElement("div"));
    previewContainer.className = "detailContainer graphDetails";
    previewContainer.style.width = DataManager.detailsInfo.chartWidth + "px";
    previewContainer.style.height = DataManager.detailsInfo.elementHeight + "px";
    //if (typeof this.graphObject.description !== "undefined")
    //    previewContainer.title = this.graphObject.description;

    this.previewDiv = previewContainer;
    previewContainer.graph = this;

    previewContainer.addEventListener("click", this._clickEvent);

    var title = previewContainer.appendChild(document.createElement("h4"));
    title.className = "detailTitle graphDetailTitle";
    title.textContent = this.graphObject.name;
    title.style.width = DataManager.detailsInfo.elementWidth + "px";

    var svgContainer = previewContainer.appendChild(document.createElement("div"));
    svgContainer.className = "preview-svg-container";
    svgContainer.style.width = DataManager.detailsInfo.chartWidth + "px";
    svgContainer.style.height = DataManager.detailsInfo.chartHeight + "px";

    var svg = d3.select(svgContainer).append("svg")
    .attr("width", DataManager.detailsInfo.chartWidth)
    .attr("height", DataManager.detailsInfo.chartHeight);

    svg.className = "graph-svg-preview";
    this.graphObject.preview.container = previewContainer;
    this.graphObject.preview.svg = svg;


    cfg = (graphObject) ? graphObject.cfg : this.graphObject.cfg;
    marginLeft = (graphObject) ? graphObject.marginLeft : this.graphObject.marginLeft;
    marginTop = (graphObject) ? graphObject.marginTop : this.graphObject.marginTop;
    data = (graphObject) ? graphObject.data : this.graphObject.data;

    var graph = this.graphObject;



    if (data && !graphObject) {
      graphObject = {};
      graphObject.data = data;
    }




    this._UpdatePreview();

  }
  this.Update = function () {
    var graph = this.graphObject;

    d3.select(graph.container).select('svg').selectAll('g').remove();
    // d3.select(graph.container).select('svg').remove();

    //sets data and displaydata
    // GraphManager.AddGraphData(graph, data);
    var width = graph.container.clientWidth;
    var height = graph.container.clientHeight;
    var marginLeft = GraphManager.defaultValues.graphPadding.left + GraphManager.defaultValues.axisMargin.y;
    var marginTop = GraphManager.defaultValues.graphPadding.top;
    var marginRight = GraphManager.defaultValues.graphPadding.right;
    var marginBottom = GraphManager.defaultValues.graphPadding.bottom + GraphManager.defaultValues.axisMargin.x;;

    this._fillSpider(graph);

    graph.svg.attr("width", width);
    graph.svg.attr("height", height);

    this._UpdatePreview();
  }

  this._clickEvent = function (e) {
    var graph = e.currentTarget.graph;

    if (graph.visible)
    {
      graph._closeGraph();
    }
    else
    {
      graph._openGraph();
    }
  }

  this._closeGraph = function () {
    this.visible = false;
    GraphManager.RemoveGraph(this.graphID)
    L.DomUtil.removeClass(this.previewDiv, "chartPreviewActive");
  }

  this._openGraph = function () {
    this.visible = true;
    this._resetSize();
    GraphManager.AddGraph(this.graphObject.container);
    L.DomUtil.addClass(this.previewDiv, "chartPreviewActive");
  }

  this._resetSize = function () {

    var changed = false
    if (parseInt(this.graphObject.container.style.width) != this.graphObject.width) {
      this.graphObject.container.style.width = this.graphObject.width + "px";
      changed = true;
    }
    if (parseInt(this.graphObject.container.style.height) != this.graphObject.height) {
      this.graphObject.container.style.height = this.graphObject.height + "px";
      changed = true;
    }

    if (changed)
    this.Update();
  }

  this._fillSpider = function (graph) {



    d3.select(graph.container).select('svg').selectAll('g').remove();

    if ('undefined' === typeof cfg) {
      cfg = {};
    }

    //Put all of the options into a variable called cfg
    if('undefined' !== typeof graphObject.cfg){
      for(var i in graphObject.cfg){
        if('undefined' !== typeof graphObject.cfg[i]){ cfg[i] = graphObject.cfg[i]; }
      }//for i
    }//if

    //If the supplied maxValue is smaller than the actual one, replace by the max in the data
    var maxValue = Math.max(cfg.maxValue, d3.max(graphObject.data, function(i){return d3.max(i.map(function(o){return o.value;}))}));

    var allAxis = (graphObject.data[0].map(function(i, j){return i.axis})),	//Names of each axis
    total = allAxis.length,					//The number of different axes
    // radius = Math.min(cfg.w/2, cfg.h/2), 	//Radius of the outermost circle

    radius = Math.min((graphObject.container.clientWidth - (cfg.margin.left + cfg.margin.right))/2, (graphObject.container.clientHeight - (cfg.margin.top + cfg.margin.bottom)) /2),
    Format = d3.format('.1f');
    angleSlice = Math.PI * 2 / total;		//The width in radians of each "slice"

    //Scale for the radius
    var rScale = d3.scale.linear()
    .range([0, radius])
    .domain([0, maxValue]);


    //Initiate the radar chart SVG
    var svg = graph.svg
    .attr("width",  cfg.w + cfg.margin.left + cfg.margin.right)
    .attr("height", cfg.h + cfg.margin.top + cfg.margin.bottom)
    .attr("class", "radar"+graphObject.id);
    // hier wordt niet de breedte gezet



      var LegendOptions = [];
      for (var i = 0; i < graphObject.dataset.length; i++) {
        LegendOptions.push(graphObject.dataset[i][0].name);
      }
      //Initiate Legend
      var legend = svg.append("g")
      .attr("class", "legend")
      .attr("height", 100)
      .attr("width", cfg.w)
      .attr('transform', 'translate(20,40)')
      ;
      //Create colour squares
      legend.selectAll('rect')
      .data(LegendOptions)
      .enter()
      .append("rect")
      .attr("x", 0)
      .attr("y", function(d, i){ return i * 20;})
      .attr("width", 10)
      .attr("height", 10)
      .style("fill", function(d, i){ return  cfg.color(i);})
      ;
      //Create text next to squares
      legend.selectAll('text')
      .data(LegendOptions)
      .enter()
      .append("text")
      .attr("x", 17)
      .attr("y", function(d, i){ return i * 20 + 9;})
      .attr("font-size", "11px")
      .attr("fill", "#737373")
      .text(function(d) { return d; });


    if (graphObject.level > 0) {

      var levelContainer = svg.append("g")
      .attr('class', "levelContainer")
      .attr('height', 100)
      .attr('width', cfg.w)
      .attr('transform', 'translate(20,20)');

      BackOptions = ['Terug'];
      levelContainer.selectAll('rect')
      .data(BackOptions)
      .enter()
      .append("text")
      .attr("x", 13)
      .attr("y", function(d, i){ return i * 20 + 9;})
      .attr("font-size", "14px")
      .attr("fill", "#737373")
      .text(function(d) { return d; }).attr('text-anchor','middle')
      .style('cursor','pointer')
      .on('click',(function() {
        this.graphObject.level = 0;
        this.graphObject.data = setLevelData(0, false, this.graphObject.dataset);
        this.graphObject.subTitle = false;
        this._fillSpider(this.graphObject);
        this.Update();

      }).bind(this));

    }

    //Append a g element
    var g = svg.append("g").attr("transform", "translate(" + ((graphObject.container.clientWidth - (cfg.margin.left + cfg.margin.right) )/2 + cfg.margin.left) + "," + graphObject.container.clientHeight/2  + ")");

    if (typeof graphObject.name !== "undefined") {
      if (typeof this.graphObject.title !== "undefined") {
        this.graphObject.title.remove('text');
      }
      this.graphObject.title = svg.append("text")
      .attr("x", graphObject.container.clientWidth/2)
      .attr("y", GraphManager.defaultValues.graphPadding.top)
      .attr("dy", 20 - GraphManager.defaultValues.graphPadding.top)
      .attr("text-anchor", "middle")
      .attr("pointer-events", "none")
      .attr("class", "graph-title-text")
      .style("font-size", "16px")
      .text(graphObject.name);
    }

    if (graphObject.subTitle) {
      if (typeof this.graphObject.subTitleText !== "undefined") {
        this.graphObject.subTitleText.remove('text');
      }
      this.graphObject.subTitleText = svg.append("text")
      .attr("x", graphObject.container.clientWidth/2)
      .attr("y", GraphManager.defaultValues.graphPadding.top + 14)
      .attr("dy", 20 - GraphManager.defaultValues.graphPadding.top)
      .attr("text-anchor", "middle")
      .attr("pointer-events", "none")
      .attr("class", "graph-title-text")
      .style("font-size", "11px")
      .text(graphObject.subTitle);
    } else {
      if (typeof this.graphObject.subTitleText !== "undefined") {
        this.graphObject.subTitleText.remove('text');
      }
    }


    //Filter for the outside glow
    var filter = g.append('defs').append('filter').attr('id','glow'),
    feGaussianBlur = filter.append('feGaussianBlur').attr('stdDeviation','2.5').attr('result','coloredBlur'),
    feMerge = filter.append('feMerge'),
    feMergeNode_1 = feMerge.append('feMergeNode').attr('in','coloredBlur'),
    feMergeNode_2 = feMerge.append('feMergeNode').attr('in','SourceGraphic');

    /////////////////////////////////////////////////////////
    /////////////// Draw the Circular grid //////////////////
    /////////////////////////////////////////////////////////

    //Wrapper for the grid & axes
    var axisGrid = g.append("g").attr("class", "axisWrapper");

    //Draw the background circles
    axisGrid.selectAll(".levels")
    .data(d3.range(1,(cfg.levels+1)).reverse())
    .enter()
    .append("circle")
    .attr("class", "gridCircle")
    .attr("r", function(d, i){return radius/cfg.levels*d;})
    .style("fill", "#CDCDCD")
    .style("stroke", "#CDCDCD")
    .style("fill-opacity", cfg.opacityCircles)
    .style("filter" , "url(#glow)");

    //Text indicating at what % each level is
    axisGrid.selectAll(".axisLabel")
    .data(d3.range(1,(cfg.levels+1)).reverse())
    .enter().append("text")
    .attr("class", "axisLabel")
    .attr("x", 4)
    .attr("y", function(d){return -d*radius/cfg.levels;})
    .attr("dy", "0.4em")
    .style("font-size", "12px")
    .attr("fill", "#737373")
    .text(function(d,i) { return Format(maxValue * d/cfg.levels); });

    /////////////////////////////////////////////////////////
    //////////////////// Draw the axes //////////////////////
    /////////////////////////////////////////////////////////

    //Create the straight lines radiating outward from the center
    var axis = axisGrid.selectAll(".axis")
    .data(allAxis)
    .enter()
    .append("g")
    .attr("class", "axis");
    //Append the lines
    axis.append("line")
    .attr("x1", 0)
    .attr("y1", 0)
    .attr("x2", function(d, i){ return rScale(maxValue*1.1) * Math.cos(angleSlice*i - Math.PI/2); })
    .attr("y2", function(d, i){ return rScale(maxValue*1.1) * Math.sin(angleSlice*i - Math.PI/2); })
    .attr("class", "line")
    .style("stroke", "white")
    .style("stroke-width", "2px");

    //Append the labels at each axis
    axis.append("text")
    .attr("class", "legend")
    .style("font-size", "11px")
    .attr("text-anchor", "middle")
    .attr("dy", "0.35em")
    .attr("x", function(d, i){ return rScale(maxValue * cfg.labelFactor) * Math.cos(angleSlice*i - Math.PI/2); })
    .attr("y", function(d, i){ return rScale(maxValue * cfg.labelFactor) * Math.sin(angleSlice*i - Math.PI/2); })
    .text(function(d){return d})
    .call(wrap, cfg.wrapWidth).on('click',(function(event) {

      this.graphObject.data = setLevelData(false, event, this.graphObject.dataset);
      if (this.graphObject.data) {
        this.graphObject.clickedSituation = false;
        this.graphObject.level = 1;
        this.graphObject.subTitle = event;
        this._fillSpider(this.graphObject);
        this.Update();
      }

    }).bind(this));



    /////////////////////////////////////////////////////////
    ///////////// Draw the radar chart blobs ////////////////
    /////////////////////////////////////////////////////////

    //The radial line function
    var radarLine = d3.svg.line.radial()
    .interpolate("linear-closed")
    .radius(function(d) { return rScale(d.value); })
    .angle(function(d,i) {	return i*angleSlice; });

    if(cfg.roundStrokes) {
      radarLine.interpolate("cardinal-closed");
    }

    //Create a wrapper for the blobs
    var blobWrapper = g.selectAll(".radarWrapper")
    .data(graphObject.data)
    .enter().append("g")
    .attr("class", "radarWrapper");

    var mouseOverFunction = function() {
      //Dim all blobs
      this.svg.selectAll(".radarArea")
      .transition().duration(200)
      .style("fill-opacity", 0.1);

      //Bring back the hovered over blob
      d3.select(this.path).select('path').transition().duration(200)
      .style("fill-opacity", 0.7);
    }

    for (var i=0; i < blobWrapper[0].length; i++) {
      blobWrapper[0][i].addEventListener('mouseover', mouseOverFunction.bind({svg:svg,path:blobWrapper[0][i]}))
      blobWrapper[0][i].addEventListener('mouseout', (function(){
        //Bring back all blobs
        // console.log(this);
        this.selectAll(".radarArea")
        .transition().duration(200)
        .style("fill-opacity", cfg.opacityArea);
      }).bind(svg));
    }

    //Append the backgrounds
    blobWrapper
    .append("path")
    .attr("class", "radarArea")
    .attr("d", function(d,i) { return radarLine(d); })
    .style("fill-opacity", cfg.opacityArea);

    if (graphObject.level > 0) {
      if (!graphObject.clickedSituation) {
        // optie 1
        blobWrapper.style("fill", function(d,i) { return cfg.color(i); })
        .style("stroke", function(d,i) { return cfg.color(i); })
      } else {
        // optie 2
        blobWrapper.style("fill", function(d,i) { return cfg.color(graphObject.clickedSituation); })
        .style("stroke", function(d,i) { return cfg.color(graphObject.clickedSituation); })
      }
    } else {
      // optie 3
      blobWrapper.style("fill", function(d,i) { return cfg.color(i); })
      .style("stroke", function(d,i) { return cfg.color(i); })
    }

    //Create the outlines
    blobWrapper.append("path")
    .attr("class", "radarStroke")
    .attr("d", function(d,i) { return radarLine(d); })
    .style("stroke-width", cfg.strokeWidth + "px")
    .style("fill", "none")
    .style("filter" , "url(#glow)");

    //Append the circles
    if (graphObject.level > 0) {
      if (!graphObject.clickedSituation) {
        blobWrapper.selectAll(".radarCircle")
        .data(function(d,i) { return d; })
        .enter().append("circle")
        .attr("class", "radarCircle")
        .attr("r", cfg.dotRadius)
        .attr("cx", function(d,i){ return rScale(d.value) * Math.cos(angleSlice*i - Math.PI/2); })
        .attr("cy", function(d,i){ return rScale(d.value) * Math.sin(angleSlice*i - Math.PI/2); })
        .style("fill-opacity", 0.8).style("fill", function(d,i,j) { return cfg.color(j); })
      } else {
        blobWrapper.selectAll(".radarCircle")
        .data(function(d,i) { return d; })
        .enter().append("circle")
        .attr("class", "radarCircle")
        .attr("r", cfg.dotRadius)
        .attr("cx", function(d,i){ return rScale(d.value) * Math.cos(angleSlice*i - Math.PI/2); })
        .attr("cy", function(d,i){ return rScale(d.value) * Math.sin(angleSlice*i - Math.PI/2); })
        .style("fill-opacity", 0.8).style("fill", function(d,i) { return cfg.color(graphObject.clickedSituation); })
      }

    } else {
      blobWrapper.selectAll(".radarCircle")
      .data(function(d,i) { return d; })
      .enter().append("circle")
      .attr("class", "radarCircle")
      .attr("r", cfg.dotRadius)
      .attr("cx", function(d,i){ return rScale(d.value) * Math.cos(angleSlice*i - Math.PI/2); })
      .attr("cy", function(d,i){ return rScale(d.value) * Math.sin(angleSlice*i - Math.PI/2); })
      .style("fill-opacity", 0.8).style("fill", function(d,i,j) { return cfg.color(j); })
    }


    /////////////////////////////////////////////////////////
    //////// Append invisible circles for tooltip ///////////
    /////////////////////////////////////////////////////////

    //Wrapper for the invisible circles on top
    var blobCircleWrapper = g.selectAll(".radarCircleWrapper")
    .data(graphObject.data)
    .enter().append("g")
    .attr("class", "radarCircleWrapper");

    //Append a set of invisible circles on top for the mouseover pop-up
    blobCircleWrapper.selectAll(".radarInvisibleCircle")
    .data(function(d,i) { return d; })
    .enter().append("circle")
    .attr("class", "radarInvisibleCircle")
    .attr("r", cfg.dotRadius*1.5)
    .attr("cx", function(d,i){ return rScale(d.value) * Math.cos(angleSlice*i - Math.PI/2); })
    .attr("cy", function(d,i){ return rScale(d.value) * Math.sin(angleSlice*i - Math.PI/2); })
    .style("fill", "none")
    .style("pointer-events", "all")
    .on("mouseenter", (function(d,i,j) {
      newX =  parseFloat(d3.select(d3.event.target).attr('cx')) - 10;
      newY =  parseFloat(d3.select(d3.event.target).attr('cy')) - 10;

      tooltipContainer
      .attr('cx', newX + 10)
      .attr('cy', newY + 10)
      .attr('r', 10)
      .transition().duration(200)
      .style('opacity', 1);

      if (graphObject.level > 0) {
        if (!graphObject.clickedSituation) {
          tooltipContainer.style("fill", cfg.color(j));
        } else {
        tooltipContainer.style("fill", cfg.color(graphObject.clickedSituation));
        }
      } else {
        tooltipContainer.style("fill", cfg.color(j));
      }

      tooltip
      .attr('x', newX + 10)
      .attr('y', newY + 14)
      .style("fill", "#fff")
      .style("font-size", "10px")
      .text(Format(d.value))
      .style('cursor','pointer')
      .style("text-anchor", "middle")
      .on("click", (function() {

        this.graphObject.data = setLevelData(j, d.axis, this.graphObject.dataset);
        if (this.graphObject.data) {
          this.graphObject.clickedSituation = j;
          this.graphObject.level = 1;
          this.graphObject.subTitle = d.axis;
          this._fillSpider(this.graphObject);
          this.Update();
        }


        // level = 1;
        // catName = "";
        //

      }).bind(this));
    }).bind(this));



    //Set up the small tooltip for when you hover over a circle
    var tooltipContainer = g.append("circle").attr("class", "tooltipcontainer").style("opacity", 1);
    var tooltip = g.append("text").attr("class", "tooltip").style("opacity", 1);

    tooltip.on("mouseenter", (function() {
      g.select('.tooltipcontainer').style("opacity", 1);
      g.select('.tooltip').style("opacity", 1);
    }).bind(g));

    tooltip.on("mouseleave", (function() {
      g.select('.tooltipcontainer').style("opacity", 0);
      g.select('.tooltip').style("opacity", 0);
    }).bind(g));




  }


}
// function createNextChart(level,catName) {
//   nieuweLijst = [];
//   count = 0;
//   for (var i1 = 0; i1 < dataSet[level].length; i1++) {
//     if (catName === dataSet[level][i1].cat) {
//       if (dataSet[level][i1].subcat) {
//         nieuweLijst.push({"axis":dataSet[level][i1].subcat,"value":dataSet[level][i1].value});
//         count++;
//       }
//     }
//   }
//   if (count > 1) {
//     this.graphObject.clickedSituation = level;
//     nieuweLijstContainer = [];
//     nieuweLijstContainer.push(nieuweLijst);
//
//     nieuwObject = graphObjectSpider
//     nieuwObject.data = nieuweLijstContainer;
//     nieuwObject.level = 1;
//
//     return nieuwObject;
//     // SpiderChart(nieuwObject);
//   } else {
//     return false;
//     // alert('no data available');
//   }
// }
function wrap(text, width) {
  text.each(function() {
    var text = d3.select(this),
    words = text.text().split(/\s+/).reverse(),
    word,
    line = [],
    lineNumber = 0,
    lineHeight = 1.4, // ems
    y = text.attr("y"),
    x = text.attr("x"),
    dy = parseFloat(text.attr("dy")),
    tspan = text.text(null).append("tspan").attr("x", x).attr("y", y).attr("dy", dy + "em");

    while (word = words.pop()) {
      line.push(word);
      tspan.text(line.join(" "));
      if (tspan.node().getComputedTextLength() > width) {
        line.pop();
        tspan.text(line.join(" "));
        line = [word];
        tspan = text.append("tspan").attr("x", x).attr("y", y).attr("dy", ++lineNumber * lineHeight + dy + "em").text(word);
      }
    }
  });
}//wrap
