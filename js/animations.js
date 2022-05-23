/* Title Banner */
anime({ 
  targets: '#title',
  translateX: 20,
});

var titleHover = document.querySelector('#title');

function animateButton(scale, duration, elasticity) { 
  anime.remove(titleHover);
  anime({ 
    targets: titleHover,
    scale: scale,
    duration: duration,
    elasticity: elasticity
  }); 
}; 

function enterButton() { 
  animateButton(1.05, 800, 400) 
};
function leaveButton() { 
  animateButton(1.0, 600, 300) 
};

titleHover.addEventListener('mouseenter', enterButton, false);
titleHover.addEventListener('mouseleave', leaveButton, false);


/* Animation: Right */
anime({ 
  targets: ['#title', '#introDescription', '#me15to64info', '#capoppropinfo', '#mepoppropinfo', '#akpoppropinfo', '#summaryDescription'], 
  translateX: 10,
});

/* Animation: Left */
anime({ 
  targets: ['#introBorderRight', '#varDescription', '#ca15to64info', '#ak15to64info', '#cajailpropinfo', '#mejailpropinfo', '#akjailpropinfo', '#geoDescription', '#bias'],
  translateX: -50,
});

/* Animation: Right (Extreme) */
anime({ 
  targets: ['.navContainer', '#varHeader', '#art1', '#infoTitle', '#geoIntro'], 
  translateX: 50,
}); 

/* Animation: Left (Extreme) */
anime({ 
  targets: ['#introHeader', '#art2', '#visTitle'],
  translateX: -50,
});

/* All Elements (.container): Fade In */
anime({
  targets: ['.container', '.navContainer', '#introBorderRight', '.introArt'],
  opacity: [0, 1],
  duration: 900,
  easing: 'easeInOutSine'
});