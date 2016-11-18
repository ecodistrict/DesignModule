var mobile = false;
var Lang = false;
function translate(defaultText, translationText, Lang) {
  // translationText[0] = LangCode
  // translationText[1] = LangText
  if (Lang === translationText[0]) {
    return translationText[1];
  } else {
    return defaultText;
  }
}
Lang = getParameterByName('lang');

var LanguageDiv = document.getElementById('lang');
languages = ['EN','NL'];
var languageLink;
for (var i = 0; i < languages.length; i++) {
  languageLink = document.createElement('a');
  languageLink.href = '?lang=' + languages[i];
  languageLink.innerText = languages[i];
  languageLink.classList.add('languages');

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
var imageContainer, imageContainer, description, outputImages;
var overlayImages = [];

// createCasus('casusName','casusID','backgroundImage','OverlayImagesArray_src-class','DescriptionObject(p_* + hr_*)','CasusLink','PartnersArray', 'SimulationArray', 'Locatie')

// Smart Traffic
description = [{'p':translate('Smart Traffic is the new generation resource for the road administators to monitor and evaluate the traffic efficiently in an effective way and let trafficlights control the traffic effective and connected.',
['NL','Smart Traffic is de nieuwe generatie hulpmiddelen voor de wegbeheerder om verkeer te monitoren, de prestatie van het netwerk te evalueren en verkeerslichten het verkeer zeer efficiënt, effectief en connected te laten regelen.'],Lang)},
{'p':translate('Smart Traffic delivers an intelligent way to control the traffic lights based on an integral traffic image based on a detection-information and floating car data.',
['NL','Smart Traffic levert een intelligente aansturing van de verkeerslichten op basis van een integraal verkeersbeeld gebaseerd op detectie-informatie en floating car data.'],Lang)},{'hr':''}];
overlayImages = [{'src':'buttons_over.png','class':'overlay'}];
createCasus(translate('Smart Traffic with floating car data',['NL','Smart Traffic met floating car data'],Lang), 'ssm', 'odysa_map.jpg', overlayImages, description, '../default.html?session=sweco', [{'name':'Sweco','image':'partners_logo_sweco.jpg'}], [['OTS'],['Vissim']], ['Eindhoven: eisenhowerlaan, Insulindelaan en Onze‐Lieve‐Vrouwestraat']);

// A58
description = [{'p':translate('CACC is a system whereby predecessor could be followed on short distances. So the intensity can be higher without to commence shockwaves. If they do commence, The CACC is better capable than the controllers to fix the shockwaves on the front. CACC belated the shockwaves on an more efficently way.',
['NL','CACC is een systeem waarmee voorgangers op korte afstanden kunnen worden gevolgd. Zodoende kan de intensiteit hoger zijn zonder dat er schokgolven ontstaan. Als deze wel ontstaan, is CACC beter in staat dan bestuurders om de schokgolven aan de voorkant op te lossen. CACC verlaat de schokgolf op een efficiëntere wijze.'],Lang)},
{'hr':''}];
overlayImages = [{'src':'buttons_over.png','class':'overlay'}];
createCasus(translate('CACC and schockwaves on the A58',['NL','CACC en schokgolven op de A58'],Lang), 'a58', 'Cacc_map.jpg', overlayImages, description, '../default.html?session=dtv', [{'name':'TU Delft','image':'partners_logo_tudelft.png'}], ['OTS'], ['a58 tussen Tilburg en Eindhoven']);

// Odysa
description = [{'p':translate('ODYSA INCAR is an dynamic green wave in the car where on a display get displayed how hard you drive and how hard you should drive to arrive at a green traffic light.',
['NL','ODYSA INCAR is een dynamische groene golf in de auto waarbij op een display wordt weergegeven hoe hard gereden moet worden om bij het volgende verkeerslicht groen te krijgen.'],Lang)},
{'hr':''}];

overlayImages = [{'src':'buttons_over.png','class':'overlay'}];
createCasus(translate('ODYSA INCAR',['NL','ODYSA INCAR'],Lang),'ODYSA_INCAR', 'odysa_map.jpg', overlayImages, description, '../default.html?session=tud',[{'name':'DTV','image':'partners_logo_dtv.png'}], ['Vissim'],['Eindhoven: eisenhowerlaan, Insulindelaan en Onze‐Lieve‐Vrouwestraat']);




function createCasus(title, Id, imageUrl, overlayImages, description, link, partnersArray, simulationsArray, location) {

  casus = createBasic(Id);

  casusImg = document.createElement('img');
  casusImg.src = 'images/' + imageUrl;
  casusImg.classList.add('corner');
  imageContainer.appendChild(casusImg);
  buildImage(imageContainer, overlayImages);

  casusLink = document.createElement('a');
  casusLink.href = link;
  casusLink.target = "_blank";

  casusLink.innerText = translate('Open case',['NL','Open casus'],Lang);
  casusLink.classList.add('button');
  casusLink.classList.add('corner');

  casusText = document.createElement('div');
  casusText.className = 'casusText';


  locationText = document.createElement('p');
  locationText.innerHTML = '<b>'+translate('Location: ',['NL','Locatie: '],Lang)+'</b>' + location;

  simulationText = document.createElement('p');
  var simulation = '';
  for (var i = 0; i < simulationsArray.length; i++) {
    simulation = simulation + ', ' + simulationsArray[i];
  }
  if (simulation) {
    simulation = simulation.substring(1);
    simulationText.innerHTML = '<b>'+ translate('Simulation system: ',['NL','Simulatie omgeving: '],Lang) +'</b> ' + simulation;
  }

  partnersDiv = document.createElement('p');
  partnerImgs = document.createElement('div');
  partnerImgs.classList.add('partnerImgs');

  var partners = '';
  var partnerimg = '';
  for (var i = 0; i < partnersArray.length; i++) {
    partnerimg = document.createElement('img');
    partnerimg.src = 'images/' + partnersArray[i].image;
    partnerimg.classList.add('partnerLogo');
    partners = partners + ', ' + partnersArray[i].name;
    partnerImgs.appendChild(partnerimg);
  }

  if (partners) {
    partners = partners.substring(1);
    partnersDiv.innerHTML = '<b>'+ translate('Partners:',['NL','Partners:'],Lang) +'</b> ' + partners;
  }

  buttonContainer = document.createElement('div');
  buttonContainer.className = 'buttonContainer';
  buttonContainer.appendChild(casusLink);
  textContainer.innerHTML =  '<h1>'+ title +'</h1>' + buildText(description);

  textContainer.appendChild(simulationText);
  textContainer.appendChild(partnersDiv);
  textContainer.appendChild(locationText);
  textContainer.appendChild(partnerImgs);

  casus.appendChild(buttonContainer);
  clearboth = document.createElement('div');
  clearboth.className = 'clearBoth';
  casus.appendChild(clearboth);

  document.getElementById('container').appendChild(casus);
  showElems(textContainer);
  showElems(imageContainer);
  showElems(buttonContainer);
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

  if (overlayImages.length > 0) {
    for (var i = 0; i < overlayImages.length; i++) {
      images = overlayImages[i];
      outputImage = document.createElement('img');
      outputImage.src = 'images/' + images.src;
      outputImage.classList.add('overlay');
      outputImage.classList.add(images.class);
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
  checkWidthPage()

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

});
window.onresize = function(e) {
  checkWidthPage()


  // var container = document.getElementById('container');
  // var casus,hoogte,ratio,newWidth;
  // for (var i = 0; i < container.children.length; i++) {
  //   casus = container.children[i];
  //   images = casus.children[0].children;
  //   hoogte = casus.children[1].getBoundingClientRect().height;
  //   breedte = casus.children[1].getBoundingClientRect().breedte;
  //   for (var i2 = 0; i2 < images.length; i2++) {
  //     image = images[i2];
  //     natHeight = image.naturalHeight;
  //     natWidth = image.naturalWidth;
  //     ratio = hoogte / natHeight ;
  //     newWidth = natWidth * ratio;
  //     if (newWidth > breedte) {
  //       console.log(natHeight + '+' + natWidth);
  //       image.style.height = hoogte + 'px';
  //       image.style.width = (newWidth - 20) + 'px';
  //     }
  //   }
  // }


  // casusImg.style.height = casus.getBoundingClientRect().height;
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
