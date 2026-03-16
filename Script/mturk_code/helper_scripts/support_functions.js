function range(j, k) { 
  return Array
      .apply(null, Array((k - j) + 1))
      .map(function(_, n){ return n + j; }); 
}

/* set up countdown timer 
mins should either be a round number (minutes) or
a proportion that, when multiplied by 60, 
is a whole number (seconds) */
function setBreakTimer (mins) {

  if (mins < 1) {
    minutes = 0;
    seconds = mins * 60;
  } else {
    minutes = mins;
    seconds = 00;
  }

  document.getElementById('countdownTimer').innerHTML = minutes + ":" + seconds;
  startTimer();
}

/* functions in this file are sourced in all phases of membias */

function startTimer() {
  
  var presentTime = document.getElementById('countdownTimer').innerHTML;
  var timeArray = presentTime.split(/[:]+/);
  var m = timeArray[0];
  var s = checkSecond((timeArray[1] - 1));
  if(s==59){m=m-1}

  if(m<0){
    $('#contButton').show();
    $('#restTimer').hide();
    $('#countdownTimer').hide();
    proceedToNextTask('break');
    return;
  }
  
  document.getElementById('countdownTimer').innerHTML =
    m + ":" + s;
  setTimeout(startTimer, 1000);
}

function checkSecond(sec) {
  if (sec < 10 && sec >= 0) {sec = "0" + sec}; // add zero in front of numbers < 10
  if (sec < 0) {sec = "59"};
  return sec;
}

/**
 * Shuffles array in place. ES6 version
 * @param {Array} a items An array containing the items.
 */
function shuffle(a) {
    for (let i = a.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [a[i], a[j]] = [a[j], a[i]];
    }
    return a;
}

/* get browser and screen size information */
  function getBrowserInfo () {

    createInputElement(navigator.product,'engine');
    createInputElement(navigator.platform,'platform');
    createInputElement(navigator.language,'language');
    createInputElement(screen.width,'width');
    createInputElement(screen.height,'height');
}

/* functions to restrict trials to a specified duration */
var trialTimer;

function SetTrialDuration (dur) {
    trialTimer = setTimeout(function () {
      console.log('here')
      // hide prior trial stim
    $('.stim, .dot').hide();
      currSubTask++
      taskFunctions[task](); 
    }, dur);
  }

function stopTrialDuration () {
  clearTimeout(trialTimer);
}

/* force MTURK submit button to appear if tilde key is pressed */
function showSubmitButton () {

  $(document).bind("keydown.key", function(event) {
    if (event.which == 192) {
      $('#submitButton').show();
    }
  });
}


function showContButton () {

  $(document).bind("keydown.key", function(event) {
    if (event.which == 187) {
      $('#contButton').show();
    }
  });
}

/* pressing enter key clicks continue button */
function pressEnterCont () {
  $(document).bind("keydown.key", function(event) {
    if (event.which == 13 && $('#contButton').is(':visible')) {
      $('#contButton').click();
    }
  });

   $(document).bind("keydown.key", function(event) {
    if (event.which == 13 && $('#acceptButton').is(':visible')) {
      $('#acceptButton').click();
    }
  });
}

function keyBindOMO () {
  $(document).bind("keydown.key", function(event) {
    if ($('#image1').is(':visible')) {
      if (event.which == 72) {recordResponses('1');}
      if (event.which == 74) {recordResponses('2');}
      if (event.which == 75) {recordResponses('3');}  
    }  
  });  
}

// used in v4
function keyBindConf () {
  $(document).bind("keydown.key", function(event) {
    if ($('#confContainer').is(':visible')) {
      if (event.which == 72) {recordResponses('1','conf','L');}
      if (event.which == 74) {recordResponses('2','conf','L');}
      if (event.which == 75) {recordResponses('3','conf','L');}
    } else if($('#recogContainer').is(':visible')) {
      if (event.which == 72) {recordResponses('1','recog','I');}
      if (event.which == 74) {recordResponses('2','recog','I');}
      if (event.which == 75) {recordResponses('3','recog','I');}
    }
  });  
}

// used in v2 and v3
function keyBindMemConf () {
  $(document).bind("keydown.key", function(event) {
    if ($('#confContainer').is(':visible')) {
      if (event.which == 72) {recordConfResponses('1');}
      if (event.which == 74) {recordConfResponses('2');}
      if (event.which == 75) {recordConfResponses('3');}
      if (event.which == 76) {recordConfResponses('4');}
    }
  });  
}

/* does what it says */
function hideVisibleInstructions() {

  visibleInstruct = $('.instructions:visible');
  $(visibleInstruct).hide();

  // keep showing the contact info at the top left
  $('#contact').show();
}

/* record a variable as an input element */
function createInputElement(value,name){
      newElem = document.createElement('input');
      newElem.type = 'hidden';
      newElem.value = value;
      newElem.name = name;
      newElem.id = name;
      document.getElementById('mturk_form').appendChild(newElem);        
}

/* show the current trial number out of all trials */
function updateTrialCounter(counter, numTrial) { 
  tc = document.getElementById('trialCounter');
  count = counter + 1
  tc.innerText = 'Trial ' + count + ' of ' + numTrial;
}

  /* force MTURK submit button to appear */
  function secretSubmitButton () {

    $(document).bind("keydown.key", function(event) {
      if (event.which == 192) {
        $(document).unbind("keydown.key")
        $('#submitButton').show();
      }
    });
  }



// From http://code.google.com/p/js-uri/source/browse/trunk/lib/URI.js
// Function to parse MTurk URL to get WorkerID

$.extend({
  getUrlVars: function(){
    var parser = /^(?:([^:\/?\#]+):)?(?:\/\/([^\/?\#]*))?([^?\#]*)(?:\?([^\#]*))?(?:\#(.*))?/;
    var result = window.location.href.match(parser);
    var scheme    = result[1] || null;
    var authority = result[2] || null;
    var path      = result[3] || null;
    var query     = result[4] || null;
    var fragment  = result[5] || null;

    if (query === null || query === undefined) {
      return {};
    }
    var vars = [], hash;
    var hashes = query.split('&');
    for(var i = 0; i < hashes.length; i++)
    {
      hash = hashes[i].split('=');
      vars.push(hash[0]);
      vars[hash[0]] = hash[1];
    }
    return vars;
  },
  getUrlVar: function(name){
    return $.getUrlVars()[name];
  }
});


// take care of shit mturk needs for form to be submitted correctly, other mturk stuff
function setMturkForm(TURK, SANDBOX) {

  if (TURK == 1) {

    if (SANDBOX == 1) {
      url = 'workersandbox'
    } else if (SANDBOX == 0) {
      url = 'www'
    }

    document.getElementById('mturk_form').action = 'https://' + url + '.mturk.com/mturk/externalSubmit';
    
    assignId = $.getUrlVar('assignmentId');
    createInputElement("assignmentId", "assignmentId");
    document.getElementById('assignmentId').value = assignId;
    
  }

  /* make fake submit button when not in mturk mode, for testing */
  if (TURK == 0) {
      fakeButton = document.createElement("input");
      fakeButton.type = 'submit';
      fakeButton.id = "submitButton";
   }
}

/* if on mturk, function to check viewport (window) size */
function getWindowSize() {
  
  /* dimensions of browser window, or iframe */
  if (TURK == 0) {
    var height = $(window).height();
    var width = $(window).width();
  } else if (TURK == 1) {
    /* height determined by whether there is a scrollbar on the iframe or not */
    var height =  window.innerHeight;
    var width =  window.innerWidth;   
  }

  return [height, width];

}

/* Recursive function to wait for a resize event, 
check against needed window size, 
and wait for a new resize event until window is big enough. 
Returns TRUE when window size is big enough */
function checkWindowSize(dims) {

  if (dims[0] >= $('#locationBounds').height() && dims[1] >= $('#locationBounds').width()) {
    
    $('#windowCheck2').hide();
    $('#windowCheck3').show();
    
    // turn off .onresize function
    $('#contButton').one('click', function() {window.onresize = false})
    $('#contButton').show();

  } else {
          
    $('#windowCheck2').show();
    $('#windowCheck3').hide();
    $('#contButton').hide();

  }  
  
  setTimeout(function () {
    window.onresize = function () {
      
      var dims = getWindowSize(TURK);
      return(checkWindowSize(dims))

      }
  }, 500);
  
}