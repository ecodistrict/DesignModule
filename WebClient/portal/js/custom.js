var mobile = false;

function checkWidthPage() {
  if (window.innerWidth < 769) {
    document.body.setAttribute("id", "phone");
    mobile = true;
  } else {
    document.body.removeAttribute("id");
    mobile = false;
  }

  if (document.getElementById('container').offsetWidth < document.getElementById('logo').children[0].children[0].naturalWidth) {
    document.getElementById('logo').children[0].children[0].style.width = "100%";
  } else {
    document.getElementById('logo').children[0].children[0].style.width = "";
  }
}

var imageContainer, imageContainer, description, outputImages;
var overlayImages = [];
var description = {};

description = {
  'p_1':'ODYSA INCAR is een dynamische groene golf in de auto waarbij op een display wordt weergegeven hoe hard gereden moet worden om bij het volgende verkeerslicht groen te krijgen.',
  'p_2':'Locatie: Eindhoven: eisenhowerlaan, Insulindelaan en Onze‐Lieve‐Vrouwestraat',
  'hr_2':''
}
overlayImages = [{'src':'map_layer_overlay.png','class':'map_overlay'},{'src':'legend.png','class':'legend'}];
createCasus('ODYSA INCAR','ODYSA_INCAR', 'map_layer.jpg', overlayImages, description, 'http://www.google.nl/', 'DTV');


description = {
  'p_1':'orem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.',
  'h2':'Sed ut perspiciatis',
  'p_2':'Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam.',
  'p_3':'eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugi',
  'p_4':'Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam.',
  'p_5':'eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugi',
  'hr':''
};
overlayImages = [{'src':'a58.png','class':'overlay'}];

createCasus('A58', 'a58', 'map.jpg', overlayImages, description, 'http://www.google.nl/', 'DTV');
description = {
  'p_1':'orem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.',
  'h2':'Sed ut perspiciatis',
  'p_2':'Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam.',
  'p_3':'eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugi',
  'hr':''
};
overlayImages = [{'src':'platooning.png','class':'platooning'}];
createCasus('Glosa', 'glosa', 'map.jpg', overlayImages, description, 'http://www.google.nl/', 'DTV');


description = {
  'p_1':'orem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.',
  'h2':'Sed ut perspiciatis',
  'p_2':'Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam.',
  'p_4':'Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam.',
  'p_5':'eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugi',
  'hr':''
};
overlayImages = [{'src':'eindhoven.png','class':'eindhoven'}];
createCasus('Eindhoven', 'eindhoven', 'map.jpg', overlayImages, description, 'http://www.google.nl/', 'DTV');

// title, image, omschrijving


function createCasus(title, Id, imageUrl, overlayImages, description, link, company) {

  casus = createBasic(Id);

  casusImg = document.createElement('img');
  casusImg.src = 'images/' + imageUrl;
  imageContainer.appendChild(casusImg);
  buildImage(imageContainer, overlayImages);

  casusLink = document.createElement('a');
  casusLink.href = link;
  casusLink.target = "_blank";
  casusLink.innerText = 'Open casus';
  casusLink.className = 'button';

  casusBy = document.createElement('div');
  casusBy.className = 'casusBy';
  casusText = document.createElement('div');
  casusText.className = 'casusText';

  companyH2 = document.createElement('h2');
  companyH2.innerText = ' ' + company;

  casusByText = document.createElement('span');
  casusByText.innerText = 'By: ';

  casusText.appendChild(casusByText);
  casusText.appendChild(companyH2);

  casusBy.appendChild(casusText);

  buttonContainer = document.createElement('div');
  buttonContainer.className = 'buttonContainer';
  buttonContainer.appendChild(casusBy);
  buttonContainer.appendChild(casusLink);
  textContainer.innerHTML =  '<h1>'+ title +'</h1>' + buildText(description);

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
      imageContainer.appendChild(outputImage);
    }
  }
}

function createBasic(Id, buttonContainer) {
  casus = document.createElement('div');
  casus.id = "casus-" + Id;

  imageContainer = document.createElement('div');
  imageContainer.id = "imageContainer-" + Id;

  textContainer = document.createElement('div');
  textContainer.id = "textContainer-" + Id;


  casus.appendChild(imageContainer);
  casus.appendChild(textContainer);

  return casus;
}

function buildText(description) {
  var text = '';
  for (var key in description) {
    if (key.substring(0, 1) === 'p') {
      text += '<p>' + description[key] + '</p>';
    } else if (key === 'h1') {
      text += '<h1>' + description[key] + '</h1>';
    } else if (key === 'h2') {
      text += '<h2>' + description[key] + '</h2>';
    } else if (key.substring(0, 2) === 'hr') {
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

    // Math.abs(x)

    if (map_overlayPosition < 0 && Math.abs(map_overlayPosition) < map_overlayHeight) {

        hundred = map_overlayHeight;
        percentage = Math.abs(map_overlayPosition);
        var value = percentage / hundred;
        opacity = 0.8 - value.toFixed(2);

        map_overlay.style.opacity = opacity;




    }

    //  && (legendPosition === legendHeight || legendPosition < legendHeight)



    // if ()


  }

  // console.log(document.getElementsByClassName('legend'));

}

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
