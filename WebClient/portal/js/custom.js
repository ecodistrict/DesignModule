var mobile = false;
var Lang = false;
var data = {};
var loggedin = {};
// JSON example/explanation
// fill in between the asterisks
// data {
//   casename : array(array('*casename*'),object{"*lang_code*":"*lang_text*"}); // object is an option
//   id: array('*id*');
//   backgroundImages: array(array('*src_image*'),object{"src":"*src_overlayImage*","class":"*classname*"}); // object is an option
//   description: array(array(object{"*attribute*":"*text*"},object{"*attribute2*":"*text2*"}), object{"*lang_code*":array(object{"*attribute*":"*text*"},object{"*attribute2*":"*text2*"})}); // object{"LANG_CODE"} is an option
//   casuslink: object{"link":"*url*","buttonText":array(array("*defaultButtonText*"),object{"*lang_code*":"*langButtonText*"},object{"*lang_code2*":"*langButtonText2*"})} // object{"*lang_code*":"*langButtonText*"} is an option
//   partnersObject: object{"name":array(array("*defaultText*"),object{"*lang_code*":"*langText*"},object{"*lang_code*":"*langText*"}),"partners":array(object{"name":"*partnerName","image":"*partnerImage*"},object{"name":"*partnerName2","image":"*partnerImage2*"})} // object{"*lang_code*":"*langText*"} is an option
//   simulationsObject: object{"name":array(array("*defaultText*"),object{"*lang_code*":"*langText*"},object{"*lang_code*":"*langText*"}),"simulations":array(array("*simulationName*"),array("*simulationName2*"))} // object{"*lang_code*":"*langText*"} is an option
//   locationObject: object{"name":array(array("*defaultText*"),object{"*lang_code*":"*langText*"},object{"*lang_code*":"*langText*"}),"location":array(array("*defaultLocationName*"),object{"*lang_code*":"*langLocationName*"},object{"*lang_code2*":"*langLocationName2*"})} // object{"*lang_code*":"*langText*"} is an option
// }

// data['zonder_NL'] = {"casename":[["data zonder extra language"]],"id":["ssm"],"backgroundImages": [["odysa_map.jpg"]],"description":[[{"p":"Smart Traffic is the new generation resource for the road administators to monitor and evaluate the traffic efficiently in an effective way and let trafficlights control the traffic effective and connected."},{"p":"Smart Traffic delivers an intelligent way to control the traffic lights based on an integral traffic image based on a detection-information and floating car data."},{"hr":""}]],"casuslink":{"link":"../default.html","buttonText":[["Open case"]]},"partnersObject":{"name":[["Partners"]],"partners":[{"name":"Sweco","image":"partners_logo_sweco.jpg"}]},"simulationsObject":{"name":[["Simulation system"]],"simulations":[["OTS"],["Vissim"]]},"locationObject":{}};
// data['2_talen'] = {"casename":[["2_talen_test"],{"NL":"2_talen_test_NL"},{"SP":"2_talen_test_SP"}],"id":["ssm"],"backgroundImages": [["odysa_map.jpg"],{"src":"buttons_over.png","class":"overlay"}],"description":[[{"p":"Smart Traffic is the new generation resource for the road administators to monitor and evaluate the traffic efficiently in an effective way and let trafficlights control the traffic effective and connected."},{"p":"Smart Traffic delivers an intelligent way to control the traffic lights based on an integral traffic image based on a detection-information and floating car data."},{"hr":""}],{"NL":[{"p":"Smart Traffic is de nieuwe generatie hulpmiddelen voor de wegbeheerder om verkeer te monitoren, de prestatie van het netwerk te evalueren en verkeerslichten het verkeer zeer efficiënt, effectief en connected te laten regelen."},{"p":"Smart Traffic levert een intelligente aansturing van de verkeerslichten op basis van een integraal verkeersbeeld gebaseerd op detectie-informatie en floating car data."},{"hr":""}]},{"SP":[{"p":"¿Smart Traffic is de nieuwe generatie hulpmiddelen voor de wegbeheerder om verkeer te monitoren, de prestatie van het netwerk te evalueren en verkeerslichten het verkeer zeer efficiënt, effectief en connected te laten regelen."},{"p":"¿Smart Traffic levert een intelligente aansturing van de verkeerslichten op basis van een integraal verkeersbeeld gebaseerd op detectie-informatie en floating car data."},{"hr":""}]}],"casuslink":{"link":"../default.html","buttonText":[["Open case"],{"NL":"Open casus"},{"SP":"abrir declinación"}]},"partnersObject":{"name":[["Partners"],{"NL":"Partners"},{"SP":"Camarada"}],"partners":[{"name":"Sweco","image":"partners_logo_sweco.jpg"}]},"simulationsObject":{"name":[["Simulation system"],{"NL":"Simulatie omgeving"},{"SP":"simulación alrededores"}],"simulations":[["OTS"],["Vissim"]]},"locationObject":{"name":[["Location"],{"NL":"Locatie"},{"SP":"establecimiento"}],"location":[["Eindhoven: eisenhowerlaan, Insulindelaan and Onze‐Lieve‐Vrouwestraat"],{"NL":"Eindhoven: eisenhowerlaan, Insulindelaan en Onze‐Lieve‐Vrouwestraat"},{"NL":"Eindhoven: eisenhowerlaan, Insulindelaan e Onze‐Lieve‐Vrouwestraat"}]}};
data['ssm'] = {"casename":[["Smart Traffic with floating car data"],{"NL":"Smart Traffic met floating car data"}],"id":["ssm"],"backgroundImages":[["odysa_map.jpg"],{"src":"buttons_over.png","class":"overlay"}],"description":[[{"p":"Smart Traffic is an innovative tool for road administators to monitor traffic and to evaluate overall network performance. A comprehensive insight into the actual traffic state is obtained by combining detectordata with floating car data. This is used by Smart Traffic to provide intelligent traffic light control. Real-time, connected and effective."},{"hr":""}],{"NL":[{"p":"Smart Traffic is een innovatieve tool die door wegbeheerders gebruikt kan worden om verkeer te monitoren en de prestatie van het netwerk te evalueren. Een integraal verkeersbeeld wordt verkregen door detectie-informatie slim te combineren met floating car data. Op basis hiervan verzorgt Smart Traffic een intelligente aansturing van verkeerslichten. Real-time, connected en effectief."},{"hr":""}]}],"casuslink":{"link":"../default.html","buttonText":[["Open case"],{"NL":"Open casus"}]},"partnersObject":{"name":[["Partners"],{"NL":"Partners"}],"partners":[{"name":"Sweco","image":"partners_logo_sweco.png"},{"name":"PTV Group","image":"partners_ptv-group.jpg"}]},"simulationsObject":{"name":[["Simulation system"],{"NL":"Simulatie omgeving"}],"simulations":[["Vissim"]]},"locationObject":{"name":[["Location"],{"NL":"Locatie"}],"location":[["Eindhoven: eisenhowerlaan, Insulindelaan and Onze‐Lieve‐Vrouwestraat"],{"NL":"Eindhoven: eisenhowerlaan, Insulindelaan en Onze‐Lieve‐Vrouwestraat"}]}};
data['ODYSA_INCAR'] = {"casename":[["ODYSA IN CAR"],{"NL":"ODYSA IN CAR"}],"id":["ODYSA_INCAR"],"backgroundImages":[["odysa_map.jpg"],{"src":"buttons_over.png","class":"overlay"}],"description":[[{"p":"ODYSA IN CAR is a dynamic green wave in the car that shows an advisory speed on a display with the required speed to get a green light at the next traffic light."},{"hr":""}],{"NL":[{"p":"ODYSA IN CAR is een dynamische groene golf in de auto waarbij op een display wordt een adviessnelheid wordt weergegeven die aangeeft hoe hard gereden moet worden om bij het volgende verkeerslicht groen te krijgen."},{"hr":""}]}],"casuslink":{"link":"../default.html","buttonText":[["Open case"],{"NL":"Open casus"}]},"partnersObject":{"name":[["Partners"],{"NL":"Partners"}],"partners":[{"name":"DTV Consultants","image":"partners_logo_dtv.png"},{"name":"PTV Group","image":"partners_ptv-group.jpg"}]},"simulationsObject":{"name":[["Simulation system"],{"NL":"Simulatie omgeving"}],"simulations":[["Vissim"]]},"locationObject":{"name":[["Location"],{"NL":"Locatie"}],"location":[["Eindhoven: eisenhowerlaan, Insulindelaan and Onze‐Lieve‐Vrouwestraat"],{"NL":"Eindhoven: eisenhowerlaan, Insulindelaan en Onze‐Lieve‐Vrouwestraat"}]}};
data['cacc'] = {"casename":[["CACC and shockwaves on the A58"],{"NL":"CACC en schokgolven op de A58"}],"id":["a58"],"backgroundImages":[["Cacc_map.jpg"],{"src":"buttons_over.png","class":"overlay"}],"description":[[{"p":"CACC is a system whereby predecessors can be followed on short distances. So the intensity can be higher without to commencing shockwaves. If they do commence, The CACC is better capable than humans to solve the shockwaves at the front of the shockwave. CACC leaves the shockwaves more efficently."},{"hr":""}],{"NL":[{"p":"CACC is een systeem waarmee voorgangers op korte afstanden kunnen worden gevolgd. Zodoende kan de intensiteit hoger zijn zonder dat er schokgolven ontstaan. Als deze wel ontstaan, is CACC beter in staat dan bestuurders om de schokgolven aan de voorkant op te lossen. CACC verlaat de schokgolf op een efficiëntere wijze."},{"hr":""}]}],"casuslink":{"link":"../default.html","buttonText":[["Open case"],{"NL":"Open casus"}]},"partnersObject":{"name":[["Partners"],{"NL":"Partners"}],"partners":[{"name":"TU Delft","image":"partners_logo_tudelft.png"}]},"simulationsObject":{"name":[["Simulation system"],{"NL":"Simulatie omgeving"}],"simulations":[["OTS"]]},"locationObject":{"name":[["Location"],{"NL":"Locatie"}],"location":[["a58 between Tilburg and Eindhoven"],{"NL":"a58 tussen Tilburg en Eindhoven"}]}};

Lang = getParameterByName('lang');
loggedin.name = 'Jasper';
emptyCasusContainer();
createCasus(data, Lang);
createtextTransform();
// checkLoggedIn(loggedin, Lang);
function checkLoggedIn(loggedin, Lang) {
  document.getElementById('userMenu').innerHTML = '';
  var welcomeText;
  if (Lang === 'NL') {
    welcomeText = "Welkom: ";
  } else {
    welcomeText = "Welcome: ";
  }

  if (loggedin.name) {
    username = document.createElement('span');
    username.innerHTML = '<b>' + welcomeText + '</b>' + loggedin.name;
    document.getElementById('userMenu').appendChild(username);
  }


}
var LanguageDiv = document.getElementById('lang');
languages = ['EN','NL'];
var languageLink;
for (var i = 0; i < languages.length; i++) {
  languageLink = document.createElement('a');
  languageLink.href = '?lang=' + languages[i];
  languageLink.innerText = languages[i];
  languageLink.classList.add('languages');
  languageLink.onclick = function(e) {
    e.preventDefault();
    Lang = e.target.innerText;
    emptyCasusContainer();
    createCasus(data, e.target.innerText);
    // checkLoggedIn(loggedin, e.target.innerText);
    for (var i = 0; i < e.target.parentElement.children.length; i++) {
      if (e.target.innerText === e.target.parentElement.children[i].innerText) {
        e.target.parentElement.children[i].classList.remove('selected');
        e.target.parentElement.children[i].classList.add('selected');
      } else {
        e.target.parentElement.children[i].classList.remove('selected');
      }
    }
  }

  if (Lang === languages[i]) {
    languageLink.classList.add('selected');
  } else {
    languageLink.classList.remove('selected');
  }
  if (Lang === null) {
    if (languages[i] === "EN") {
      languageLink.classList.add('selected');
    } else {
      languageLink.classList.remove('selected');
    }
  }
  LanguageDiv.appendChild(languageLink);
}
function translate(data) {
  var found = false;
  if (data.length === 1) { // if length === 1 there are no translations
    data_val = data[0];
  }
  for (var i = 1; i < data.length; i++) {
    for (key in data[i]) {
      if (key === Lang) {
        found = true;
        data_val = data[i][key];
      }
    }
  }
  if (!found) { // check if lang is found (if not found = defaultVal)
    data_val = data[0];
  }
  return data_val;
}
function emptyCasusContainer() {
  document.getElementById('container').innerHTML = ''; // Reset container
}
function createCasus(data, language) {
  if (language === 'EN') {
    language = null;
  }


  for (key in data) {
    casus = data[key];

    case_id = casus['id'][0];
    if (language) {
      case_name = translate(casus['casename']);
      case_backgroundImage = casus['backgroundImages'][0];
      case_overlayImagesArray = casus['backgroundImages'];
      case_description = translate(casus['description']);
      case_casuslink = casus['casuslink'];
      case_casuslinkButtonText = translate(casus['casuslink']['buttonText']);
      case_partnersObject = casus['partnersObject']['partners'];
      case_partnersObject_name = translate(casus['partnersObject']['name']);
      case_simulationsObject = casus['simulationsObject']['simulations'];
      case_simulationsObject_name = translate(casus['simulationsObject']['name']);
      case_locationObject_name = translate(casus['locationObject']['name']);
      case_locationObject = translate(casus['locationObject']['location']);
    } else {
      case_name = casus['casename'][0];
      case_backgroundImage = casus['backgroundImages'][0];
      case_overlayImagesArray = casus['backgroundImages'];
      case_description = casus['description'][0];
      case_casuslink = casus['casuslink'];
      case_casuslinkButtonText = casus['casuslink']['buttonText'][0];
      case_partnersObject = casus['partnersObject']['partners'];
      case_partnersObject_name = casus['partnersObject']['name'][0];
      case_simulationsObject = casus['simulationsObject']['simulations'];
      case_simulationsObject_name = casus['simulationsObject']['name'][0];

      case_locationObject_name = '';
      case_locationObject = '';
      if (!isEmptyObject(casus['locationObject'])) {
        case_locationObject_name = casus['locationObject']['name'][0];
        case_locationObject = casus['locationObject']['location'][0];
      }

    }

    casusContainer = createBasic(case_id);

    // create/build casusImage(s)
    casusImg = document.createElement('img');
    casusImg.src = 'images/' + case_backgroundImage;
    casusImg.classList.add('corner');
    imageContainer.appendChild(casusImg);
    buildImage(imageContainer, case_overlayImagesArray);

    // create/build casusLink
    casusLink = document.createElement('a');
    casusLink.href = case_casuslink['link'];
    casusLink.target = "_blank";
    casusLink.innerText = case_casuslinkButtonText;
    casusLink.classList.add('button','corner');

    // create/build case description
    description = buildText(case_description);

    // create/build case location
    if (case_locationObject) {
      locationText = document.createElement('p');
      locationText.innerHTML = '<b>'+ case_locationObject_name +': </b>' + case_locationObject;
    }

    // create simulation(s)
    simulationText = document.createElement('p');
    var simulation = '';
    for (var i = 0; i < case_simulationsObject.length; i++) {
      simulation = simulation + ',' + case_simulationsObject[i];
    }
    if (simulation) { // check if simulation is filled
      simulation = simulation.substring(1);
      simulationText.innerHTML = '<b>'+ case_simulationsObject_name +': </b>' + simulation;
    }

    // create partner(s)
    partnersDiv = document.createElement('p');
    partnerImgs = document.createElement('div');
    partnerImgs.classList.add('partnerImgs');
    var partners = '';
    var partnerimg = '';
    for (var i = 0; i < case_partnersObject.length; i++) {
      partnerimg = document.createElement('img');
      partnerimg.src = 'images/' + case_partnersObject[i].image;
      partnerimg.classList.add('partnerLogo');
      partners = partners + ',' + case_partnersObject[i].name;
      partnerImgs.appendChild(partnerimg);
    }
    if (partners) { // check if partners is filled
      partners = partners.substring(1);
      partnersDiv.innerHTML = '<b>'+ case_partnersObject_name +': </b>' + partners;
    }

    buttonContainer = document.createElement('div');
    buttonContainer.className = 'buttonContainer';
    buttonContainer.appendChild(casusLink);
    textContainer.innerHTML =  '<h1>'+ case_name +'</h1>' + description;

    textContainer.appendChild(simulationText);
    textContainer.appendChild(partnersDiv);
    if (case_locationObject) {
      textContainer.appendChild(locationText);
    }
    textContainer.appendChild(partnerImgs);

    casusContainer.appendChild(buttonContainer);
    clearboth = document.createElement('div');
    clearboth.className = 'clearBoth';
    casusContainer.appendChild(clearboth);

    document.getElementById('container').appendChild(casusContainer);
    showElems(textContainer);
    showElems(imageContainer);
    showElems(buttonContainer);
  }
}
function createtextTransform() {
  textTransform = document.getElementById('text');
  plusText = document.createElement('span');
  plusText.innerText = '+';
  plusText.onclick = function () {
    multiplier = 0.5;
    if (document.body.style.fontSize == "") {
      document.body.style.fontSize = "1.0em";
    }
    document.body.style.fontSize = parseFloat(document.body.style.fontSize) + (multiplier * 0.2) + "em";

  }

  minusText = document.createElement('span');
  minusText.innerText = '-';
  minusText.onclick = function () {
    multiplier = 0.5;
    if (document.body.style.fontSize == "") {
      document.body.style.fontSize = "1.0em";
    }
    document.body.style.fontSize = parseFloat(document.body.style.fontSize) - (multiplier * 0.2) + "em";


  }

  textTransform.appendChild(minusText);
  textTransform.appendChild(plusText);



}



function checkWidthPage() {
  if (window.innerWidth < 769) {
    document.body.setAttribute("id", "phone");
    mobile = true;
  } else {
    document.body.removeAttribute("id");
    mobile = false;
  }

  if (document.getElementById('container').offsetWidth < document.getElementById('logo').children[0].children[0].naturalWidth) {
    document.getElementById('companyLogos').children[0].classList.add('smaller');
    document.getElementById('companyLogos').children[1].classList.add('smaller');
    document.getElementById('logo').children[0].children[0].style.width = "100%";
  } else {
    document.getElementById('companyLogos').children[0].classList.remove('smaller');
    document.getElementById('companyLogos').children[1].classList.remove('smaller');
    document.getElementById('logo').children[0].children[0].style.width = "";
  }
}



function showElems(elem) {
  elem.style.opacity = 0;
  setTimeout(function () {
    // Fade it in.
    elem.style.opacity = 1;
  }, 0);
}

function buildImage(imageContainer, overlayImages) {
  outputImages = '';

  if (overlayImages.length > 1) {
    for (var i = 1; i < overlayImages.length; i++) {
      images = overlayImages[i];
      outputImage = document.createElement('img');
      outputImage.src = 'images/' + images.src;
      outputImage.classList.add('overlay', images.class);
      outputImage.draggable = "true";
      imageContainer.appendChild(outputImage);
    } // eo for
  } // eo overlayImages.length
} // eo buildImage

function createBasic(Id, buttonContainer) {
  casus = document.createElement('div');
  casus.id = "casus-" + Id;
  casus.classList.add('corner');

  imageContainer = document.createElement('div');
  imageContainer.id = "imageContainer-" + Id;
  imageContainer.classList.add('corner');

  textContainer = document.createElement('div');
  textContainer.id = "textContainer-" + Id;

  casus.appendChild(imageContainer);
  casus.appendChild(textContainer);

  return casus;
}

function buildText(description) {

  var text = '';
  for (var i = 0; i < description.length; i++) {

    if (description[i].p) {
      text += '<p>' + description[i].p + '</p>';
    } else if (description[i].h1) {
      text += '<h1>' + description[i].h1 + '</h1>';
    } else if (description[i].h2) {
      text += '<h2>' + description[i].h2 + '</h2>';
    } else if (description[i].hr === '') {
      text += '<div class="horizontalRule"></div>';
    }
  }
  return text;
}

checkWidthPage()

window.onscroll = function(e) {
  // console.log(document.body.scrollTop);
  var overlay = document.getElementsByClassName('map_overlay');
  for (i = 0; i < overlay.length; i++) {
    var map_overlay = overlay[i];

    map_overlayPosition = map_overlay.getBoundingClientRect().top;
    map_overlayHeight = map_overlay.getBoundingClientRect().height;

    if (map_overlayPosition < 0 && Math.abs(map_overlayPosition) < map_overlayHeight) {

      hundred = map_overlayHeight;
      percentage = Math.abs(map_overlayPosition);
      var value = percentage / hundred;
      opacity = 0.8 - value.toFixed(2);
      map_overlay.style.opacity = opacity;

    }
  }
}

document.addEventListener("DOMContentLoaded", function(event) {


  if (document.getElementsByClassName('legend').length > 0){

    function drag_start(event) {
      var style = window.getComputedStyle(event.target, null);
      event.dataTransfer.setData("text/plain",(parseInt(style.getPropertyValue("left"),10) - event.clientX) + ',' + (parseInt(style.getPropertyValue("top"),10) - event.clientY));

      dm.style.cursor = '-webkit-grabbing';
      dm.style.cursor = '-moz-grabbing';
    }
    function onDragging(event) {
      dm.style.cursor = '-webkit-grabbing';
      dm.style.cursor = '-moz-grabbing';
    }
    function drag_over(event) {
      event.preventDefault();
      dm.style.cursor = '-webkit-grabbing';
      dm.style.cursor = '-moz-grabbing';

      return false;
    }
    function drop(event) {
      var offset = event.dataTransfer.getData("text/plain").split(',');
      var dm = document.getElementsByClassName('legend')[0];

      dm.style.left = (event.clientX + parseInt(offset[0],10)) + 'px';
      dm.style.top = (event.clientY + parseInt(offset[1],10)) + 'px';
      dm.style.cursor = '-webkit-grab';
      dm.style.cursor = '-moz-grab';
      event.preventDefault();
      return false;
    }
    var dm = document.getElementsByClassName('legend')[0];

    dm.addEventListener('dragstart',drag_start,false);
    dm.addEventListener('drag',onDragging,false);
    document.body.addEventListener('dragover',drag_over,false);
    document.body.addEventListener('drop',drop,false);
  }


  var checkExist = setInterval(function() {
    if (document.getElementById('imageContainer-ssm').getBoundingClientRect().height > 0) {
      // buildSVG();
      checkWidthPage();
      clearInterval(checkExist);
    }
  }, 100); // check every 100ms




});
window.onresize = function(e) {
  checkWidthPage();

};

function isEmptyObject(obj) {
  for(var p in obj){
    return false;
  }
  return true;
};


function getParameterByName(name) {
  url = window.location.href;
  name = name.replace(/[\[\]]/g, "\\$&");
  var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
  results = regex.exec(url);
  if (!results) return null;
  if (!results[2]) return '';
  return decodeURIComponent(results[2].replace(/\+/g, " "));
}
