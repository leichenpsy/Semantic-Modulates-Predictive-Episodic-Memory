/* get subject number from url */
var params = new URLSearchParams(window.location.search);
document.getElementById('assignmentId').value = params.get('assignmentId')


/* debug mode */
var	TURK = 1;  // change to 1 when posting on Mturk

var DEBUG = 1; // change to 0 when ready for the real deal

var SANDBOX = 1; // change to 0 when ready for the real deal

var batch = 1; // change to 0 when ready for the real deal

/* experiment materials arrays*/
// var imagePath = 'https://run.pavlovia.org/leipsy/image-size-mem/experiment materials/';
var imagePath = '../experiment materials/Experiment images';
var numSyllables = [1,2]
var numPracPerSy = [1,2,3,4,5,6,7,8]
var conceptNames = {
    1 : {
        1: 'desk',
        2: 'chair',
        3: 'sofa',
        4: 'dresser'
    },
    2 : {
        1 : 'bike',
        2 : 'car',
        3 : 'trolley',
        4 : 'scooter'        
    },
    3 : {
        1 : 'ring',
        2 : 'crown',
        3 : 'medal',
        4 : 'necklace'
    },
    4 : {
        1 : 'shirt',
        2 : 'skirt',
        3 : 'mitten',
        4 : 'sweater'
    }
}

var pracSequence = []

var encodeArrCollection = {
    a:{
        a1:{},
        a2:{},
        a3:{},
        a4:{}
    },
    b:{
        b1:{},
        b2:{},
        b3:{},
        b4:{}
    },
    c:{
        c1:{},
        c2:{},
        c3:{},
        c4:{}
    },
    d:{
        d1:{},
        d2:{},
        d3:{},
        d4:{}
    }
},memoryTestPictures;//stimulus(pictures) collection
var imageArr = [];
var audioArr = [];

// var targetCategories, cueCategories, withinCategories, crossCategories, sameCategories, diffCategories, fillerCategories
// var pairsCombo,encodeSequence,fillerCombo,predictTestSequence,memoryTestSequence;

/* counter */
var currTrial = 0;
var currTask = 0;
var currSession = 0;
var currInstruct = 0;
var round = 1;
var nInstruct,nTask,task;
var breakOption = false;
var useSpacebar = 1;
var order,picIndex;//predictTest sequence index
var noResponse;
var stimulus;

/* timing */
var iti = 500;//interval
var displayTime = 1000;
var trialStartTime;
var begin_time;
var right_now;


/* Counterbalance */
var condition,sessionBalance,testOption;//encode and test response mapping


/* Save Data */
var saveData = {
    practice:[],
    encodePictures:[],
    mathDistract:[],
    memoryTest:[],
    predictTest:[],
    demog:[]
}

/* Instruction arrays */
var feedbackType = ['Yes','No'];
var feedbackSentence = ['has one syllable','has two syllables'];

var instructCollection = {
    welcome:['#welcome','#attention'],
    encodePictures: ['#session1Instruction','#session1PracInstruction','#session1RepInstruction','#session1BeginInstruction'],
    mathDistract:['#session2Instruction'],
    predictTest:['#predictTestInstruction'],
    memoryTest:['#memoryTestInstruction']
}

/* Sessions and functions arrays */
var sessions = ['welcome','encodePictures','mathDistract','memoryTest', 'predictTest','postExperiment'];
var sessionFunctions = {
    welcome:[
        function(){condition = getRandomInt(2); preloadAll(); showInstructions()},
        function(){showInstructions(); generatePracSequence()}
    ],
    encodePictures:[
        function(){
            if(condition === 1){
                $('.Option').html(size[1]);
                $('.kOption').html(size[0])
            }else{
                $('.jOption').html(size[0]);
                $('.kOption').html(size[1])
            };
            console.log('condition is ' + condition);
            useSpacebar = 1;
            showInstructions()},
        function(){useSpacebar = 1; showInstructions()},
        function(){useSpacebar = 0; generatePracTrials()},
        function(){pracTrials()},
        function(){checkAccuracy()},
        function(){useSpacebar = 0; generateEncodeSequence();generateEncodeTrials()},
        function(){encodeTrials()},
    ],
    mathDistract:[
        function(){useSpacebar = 1; showInstructions()},
        function(){useSpacebar = 0; mathTrials()},
        function(){
            if(sessionBalance == 1){
                sessions = ['welcome','encodePictures','mathDistract','memoryTest','predictTest','postExperiment']; 
            }
            console.log('sessionBalance',sessionBalance,sessions);
            currTask++;
            runSessions(sessions[currSession]);
        }],
    memoryTest:[
        function(){useSpacebar = 1; showInstructions();
            testOption = getRandomInt(2);
            console.log('testOption', testOption);
            if(testOption == 1){$('#memoryTestInstructOption1').hide();}else{$('#memoryTestInstructOption2').hide()}
        },
        function(){hideVisibleInstructions();useSpacebar = 0; generateMemoryTestSequence()},
        function(){memoryTestTrials()}
    ],
    predictTest:[
        function(){useSpacebar = 1; showInstructions()},
        function(){predictTestSequence = generatePredictTestSequence();useSpacebar = 0; currTask++;order = 0; picIndex = 0;console.log(predictTestSequence);runSessions(sessions[currSession]);},
        function(){predictTestTrials();}
    ],
    postExperiment:[
        function(){$('#demogs').show();
        $(document).unbind('keydown');
        console.log('unbind space key')
       }
    ]
}


function getRandomInt(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

function createInputElement(value,name){
    var newElem = document.createElement("input");
    newElem.type = 'hidden';
    newElem.value = value;
    newElem.name = name;
    newElem.id = name;
    document.getElementById('responses').appendChild(newElem);        

}
function getBrowserInfo(end){
    if(end == true){
        begin_time = right_now
    }
    else{
        right_now = new Date();
        createInputElement(navigator.userAgent,'browserStart');
        createInputElement(navigator.product,'engineStart');
        createInputElement(navigator.platform,'platformStart');
        createInputElement(navigator.language,'languageStart');
        createInputElement(screen.width,'widthStart');
        createInputElement(screen.height,'heightStart'); 
        createInputElement(right_now,'startTime'); 
    }
    if(end == true){
        right_now = new Date();
        createInputElement(navigator.userAgent,'browserEnd');
        createInputElement(navigator.product,'engineEnd');
        createInputElement(navigator.platform,'platformEnd');
        createInputElement(navigator.language,'languageEnd');
        createInputElement(screen.width,'widthEnd');
        createInputElement(screen.height,'heightEnd'); 
        var expTime = right_now - begin_time;
        createInputElement(right_now,'endTime');
        createInputElement(expTime,'experimentTime')
    }      
};

function preloadAll(){
    var preLoadImg, preLoadAudio; 
    var categories = ['a','b','c','d'];

    //preload practice materials
    for (var i = 0; i < 2; i++){
        for (var j = 0; j<8; j++){
            preLoadImg = new Image();
            preLoadImg.src = imagePath + 'practice/' + numSyllables[i] + '_' + numPracPerSy[j] + '.jpg';
            preLoadAudio = new Audio();
            preLoadAudio.src = imagePath + 'practice/' + numSyllables[i] + '_' + numPracPerSy[j] + '.mp3';
            imageArr.push(preLoadImg);
            audioArr.push(preLoadAudio)
        }
    }
    //preload main trials materials
    for(var i = 0; i < 4; i++){
        for (var j = 1; j<5; j++){
            for (var k = 1; k < 33; k++){
                preLoadImg = new Image();
                preLoadImg.src = imagePath + categories[i] + j + '_' + k + '.jpg';
                imageArr.push(preLoadImg)
            }
        }
    }
    console.log('preload all the images and audios')
}


function hideVisibleInstructions() {
    visibleInstruct = $('.instructions:visible');
    $(visibleInstruct).hide();
};

function showInstructions(state = true){
    hideVisibleInstructions();
    var session = sessions[currSession];
    $(instructCollection[session][currInstruct]).show();
    if(state){
        currInstruct++;
        currTask++;
    }else{
        currInstruct = 0;
        currTask = 0;
    }
    
};

function recordResponse(button, currSession, currTask, currTrial){
    var data = {
        currSession: '',
        currTask:'',
        currTrial:'',
        stimulus: '',
        response: '',
        correctResponse: '',
        responseTime: '',
        round:'',
        type:'',
        predictType:'',
        sizeType:'',
        predictPair:'',
        pairCaType:'',
        pairSiType:'',
        picType:''
    };
    var responseDate = new Date();
    var responseTime = responseDate - trialStartTime;
    data.response = button;
    data.responseTime = responseTime;
    data.currSession = sessions[currSession];
    data.sessionNum = currSession;
    data.stimulus = stimulus;
    
    if(currSession == 1 && currTask == 3){
        data.currTask = 'practice'
        data.round = round;
        if(condition == 0){
           data.correctResponse = (pracSets[currTrial] == 1)? 'j':'k';
        }
        else{
           data.correctResponse = (pracSets[currTrial] == 1)? 'k':'j';
        }
        
    }else if(currSession == 1 && currTask == 6){
        data.currTask = 'encodePictures';
        data.type = $('#encodePicture').data('type');
        data.predictType = $('#encodePicture').data('predictType');
        data.sizeType = $('#encodePicture').data('sizeType');
        if(condition == 0){
            data.correctResponse = (stimulus[1] == '1' || stimulus[1] == '2')? 'j':'k';
        }
        else{
            data.correctResponse = (stimulus[1] == '1' || stimulus[1] == '2')? 'k':'j';
        }
    }else if(currSession == 2 && currTask == 1){
        data.currTask = 'mathDistract';
        var answer = $('#mathQuestion').data('answer');
        data.correctResponse = (answer == 1)? '1':(answer == 2)? '2':(answer == 3)? '3':'4';

    }else if(sessions[currSession]== 'predictTest'){
        data.currTask = 'predictTest';
        data.stimulus = predictTestSequence[currTrial];
        var check1 = pairsCombo.indexOf(predictTestSequence[currTrial][0]);
        var check2 = pairsCombo.indexOf(predictTestSequence[currTrial][1]);
        data.correctResponse = (check1 > check2)? '1':(check1 < check2)? '2':'no correct answer';
        data.predictPair = (check1 > check2)? predictTestSequence[currTrial][0]:(check1 < check2)? predictTestSequence[currTrial][1]:'NA';
        var pair = data.predictPair
        data.pairCaType = (data.predictPair == 'NA')? 'NA':(withinCategories.indexOf(pair.slice(0,2)) == -1)? 'cross':'within';
        data.pairSiType = (pair == 'NA')? 'NA': (sameCategories.indexOf(pair.slice(0,2)) == -1)? 'diff':'same';

    }else if(sessions[currSession] == 'memoryTest'){
        data.currTask = 'memoryTest';
        var answer = $('#memoryTestPicture').data('correctResponse');
        data.type = $('#memoryTestPicture').data('type');
        data.predictType = $('#memoryTestPicture').data('predictType');
        data.sizeType = $('#memoryTestPicture').data('sizeType');
        data.picType = answer;
        if(testOption == 0){
            data.correctResponse = (answer == 'old')? 'd':'k';
        }else{
            data.correctResponse = (answer == 'old')? 'k':'d';
        }
    }
    data.currTrial = currTrial;
    saveData[data.currTask].push(data);
}

$(document).bind("keydown.key", function(event){
    // space bar: triggers collection of browser info on the welcome/consent screen
    if (event.which == 32 && $('#consent').is(":visible")) {
        getBrowserInfo(false);
    }
    //  space bar: advances through instructions
    if (event.which == 32 && useSpacebar == 1) {
        runSessions(sessions[currSession]);
    }
    // B button: puts you at the prior instructions
    if (event.which == 66 && currInstruct > 1 && $('#session1PracInstruction').is(':visible')) {
        currInstruct = currInstruct - 2
        currTask = currTask -2
        runSessions(sessions[currSession]);
    }
    // D F J K button: record response in the memoryTest Session
    if((event.which == 68 || event.which == 70 || event.which == 74 || event.which == 75) && sessions[currSession] == 'memoryTest' && $('#memoryTestPicture').is(':visible')){
        var data = saveData['memoryTest'][currTrial-1];
        var responseDate = new Date();
        var responseTime = responseDate - trialStartTime;
        if(data.response == 'no response detected'){
            data.response = (event.which == 68)? 'd': (event.which == 70)? 'f':(event.which == 74)? 'j': 'k';
            data.responseTime = responseTime;
            runSessions(sessions[currSession]);
        }
    }

    // j button: record response in the encodePicture session
    if(event.which == 74 && currSession == 1 && (currTask == 3 || currTask == 6) && $('#displayTrial').is(':visible')){
        $('#encodePicture').css({'border-color': 'blue', 'border-width': '1px', 'border-style': 'solid'})
        var data = (currTask == 3)? saveData['practice'][currTrial-1+20*(round-1)]:saveData['encodePictures'][currTrial-1];
        var responseDate = new Date();
        var responseTime = responseDate - trialStartTime;
        if(data.response == 'no response detected'){
            data.response = 'j';
            data.responseTime = responseTime;
            if(currTask == 3 && pracSets[currTrial-1] == 1){
                document.getElementById('feedback').innerHTML = (data.correctResponse == 'j')? feedback[0]: feedback[2]
            }else if(currTask == 3 && pracSets[currTrial-1] == 2){
                document.getElementById('feedback').innerHTML = (data.correctResponse == 'j')? feedback[1]: feedback[3]
            }
        }

    }

    // k button: record response in the encodePicture session
    if(event.which == 75 && currSession == 1 && (currTask == 3 || currTask == 6) && $('#displayTrial').is(':visible')){
        $('#encodePicture').css({'border-color': 'blue', 'border-width': '1px', 'border-style': 'solid'})
        var data = (currTask == 3)? saveData['practice'][currTrial-1 + 20 * (round-1)]:saveData['encodePictures'][currTrial-1];
        var responseDate = new Date();
        var responseTime = responseDate - trialStartTime;
        if(data.response == 'no response detected'){
            data.response = 'k';
            data.responseTime = responseTime;
            if(currTask == 3 && pracSets[currTrial-1] == 1){
                document.getElementById('feedback').innerHTML = (data.correctResponse == 'k')? feedback[0]: feedback[2]
            }else if(currTask == 3 && pracSets[currTrial-1] == 2){
                document.getElementById('feedback').innerHTML = (data.correctResponse == 'k')? feedback[1]: feedback[3]
            }
        }
    }

    // 1 2 3 4 button: record response in the mathDistract session
    if((event.which == 49 || event.which == 50 || event.which == 51 || event.which == 52) && currSession == 2 && currTask ==1 && $('#mathDisplayTrial').is(':visible')){
        var data = saveData['mathDistract'][currTrial-1];
        var responseDate = new Date();
        var responseTime = responseDate - trialStartTime;
        if(data.response == 'no response detected'){
            data.response = (event.which == 49)? '1': (event.which == 50)? '2':(event.which == 51)? '3':'4';
            data.responseTime = responseTime;
            $('#mathDisplayTrial').hide();
        }   
    }

    //1 2 button: record response in the predictTest session
    if((event.which == 49 || event.which == 50) && sessions[currSession] == 'predictTest' && $('#predictTestTrialInstruction').is(':visible')){
        var data = saveData['predictTest'][currTrial-1];
        var responseDate = new Date();
        var responseTime = responseDate - trialStartTime;
        if(data.response == 'no response detected'){
            data.response = (event.which == 49)? '1': '2';
            data.responseTime = responseTime;
            $('#predictTestTrialInstruction').hide();
            order = 0;
            runSessions(sessions[currSession]);
        }  
    }
});

function runSessions(session){
    nTask = sessionFunctions[session].length;
    if(currTask < nTask){
        console.log(currTask, session)
        sessionFunctions[session][currTask]();
        
    }
    else{
        console.log('move on to the next session');
        currTask = 0;
        currInstruct = 0;
        currSession++;
        currTrial = 0;
        sessionFunctions[sessions[currSession]][currTask]();
    }
};

function shuffleArray(array) {
    for (let i = array.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [array[i], array[j]] = [array[j], array[i]];
    }
}
function generatePracSequence(){
    for (var i = 0; i < 2; i++){
        for (var j = 0; j < 8; j++){
            var pracImg = '';
            pracImg +=  numSyllables[i] + '_' + numPracPerSy[j]
            pracSequence.push(pracImg)
        }
    }
}
function generatePracTrials(){    
    shuffleArray(pracSequence);
    console.log('generate practice trials')
    currTask++;//move to the next task
    runSessions(sessions[currSession]);
}    
function generatePrac(i){
    if(pracSets[i] == 1){
        var index = pracPictures['smallIndex'];
        stimulus = 'small' + pracPictures['small'][index];
        document.getElementById('encodePicture').src = imagePath + stimulus + '.jpg'
        pracPictures['smallIndex']++;
    }
    else{
        var index = pracPictures['largeIndex'];
        stimulus = 'large' + pracPictures['large'][index];
        document.getElementById('encodePicture').src = imagePath + stimulus + '.jpg' 
        pracPictures['largeIndex']++;
    }
}
function pracTrials(){
    if(currTrial ==0){
        showPracticeTrials()
    }
    else if(currTrial < pracSets.length){
        setTimeout(function(){
            showPracticeTrials();
        },displayTime)
    }
    else{
        setTimeout(function(){
            $('#displayTrial').hide();
            console.log(saveData['practice']);
            for (var i in saveData['practice'][currTrial-1 + 20 * (round - 1)]){
                createInputElement(saveData['practice'][currTrial-1 + 20 * (round -1)][i],i)
            }
            currTask++;
            runSessions(sessions[currSession])    
        },displayTime)
    }
}
function showPracticeTrials(){
    hideVisibleInstructions();
    document.getElementById('fixation').innerHTML = '+';
    $('#displayTrial').hide();
    $('#encodePicture').css({'border-color': 'white', 'border-width': '0px', 'border-style': 'solid'})
    document.getElementById('feedback').innerHTML = '';
    if(currTrial > 0){
        for (var i in saveData['practice'][currTrial-1 + 20 * (round - 1)]){
            createInputElement(saveData['practice'][currTrial-1 + 20 * (round - 1)][i],i)
        }
        console.log(saveData['practice'][currTrial-1 + 20 * (round - 1)]);
    }
    generatePracPictures(currTrial);
    $('#fixation').show();
    setTimeout(function(){
        $('#fixation').hide();
        $('#displayTrial').show();
        trialStartTime = new Date();
        recordResponse('no response detected', currSession, currTask, currTrial);
        currTrial++;
        runSessions(sessions[currSession])
    },iti)
};

function checkAccuracy(){
    currTrial = 0;
    var acc = 0; 
    var num = pracSets.length;
    var state = true;
    for (var i = 0; i<saveData['practice'].length; i++){
        if (saveData['practice'][i].round == round){
            if(saveData['practice'][i].correctResponse == saveData['practice'][i].response){
                acc++;
            }
        }
    }
    if(acc/num > 0.9){
        currInstruct ++;
    }else{
        round ++;
        state = false;
    }
    useSpacebar = 1;
    console.log('accuracy is ' + acc/num);
    showInstructions(state);
}

function generatePairs(arr1, arr2){
    var sizeCombo1 = ['SS','SL','LS','LL'];
    var sizeCombo2 = ['SS','LS','LL','SL'];
    var sizeCombo3 = ['LL','SL','SS','LS'];
    var sizeCombo4 = ['LL','LS','SL','SS'];
    var sizeCombo5 = ['SL','LL','LS','SS'];
    var sizeCombo6 = ['SL','SS','LL','LS'];
    var sizeCombo7 = ['LS','LL','SS','SL'];
    var sizeCombo8 = ['LS','SS','SL','LL'];
    var sizeSets = [];
    sizeSets.push(sizeCombo1, sizeCombo2,sizeCombo3,sizeCombo4,sizeCombo5,sizeCombo6,sizeCombo7,sizeCombo8);
    var random = getRandomInt(8);
    var smallArr1 = arr1.slice(0,2);
    var smallArr2 = arr2.slice(0,2);
    var largeArr1 = arr1.slice(2,4);
    var largeArr2 = arr2.slice(2,4)
    shuffleArray(smallArr1);
    shuffleArray(largeArr1);
    shuffleArray(smallArr2);
    shuffleArray(largeArr2);
    var assignIndex = [1,2,2,1,1,1,2,2];
    var sizeAssign = sizeSets[random];
    var str = [];var assignments = [];
    var pairCombo = [];
    var j = 0; var k = 0; var m = 0; var n = 0;
    for (var i= 0; i<sizeAssign.length; i++){
        var item = sizeAssign[i].split('');
        str = str.concat(item);
    }
    for (var i = 0; i<str.length; i++){
        var item;
        if(str[i] == 'S'){
            if(assignIndex[i] == 1){
                item = smallArr1[j];
                j++;
            }else{item = smallArr2[k];k++}
            assignments.push(item)
        }else{
            if(assignIndex[i] == 1){
                item = largeArr1[m];
                m++;
            }else{item = largeArr2[n];n++}
            assignments.push(item)
        }
    }
    for(var i = 0; i<assignments.length; i=i+2){
        var pair;
        pair = assignments[i] + assignments[i+1];
        pairCombo.push(pair);
    }
    return pairCombo;
}

function generateSequence(pairCombo, arr3, arr4){
    var sequence = [];
    for (var i = 0; i<4; i++){
        var newArr = [];
        for(var j = 0; j<4; j++){
            var combo = pairCombo.concat(arr3,arr4);
            newArr = newArr.concat(combo);
        }
        shuffleArray(newArr);
        sequence = sequence.concat(newArr);
    }
    return sequence
}

function checkBacktoBack(sequence){
    for (var i = 0; i<sequence.length-1; i++){
        if(sequence[i].length == 2){continue;}
        else{
            if(sequence[i] == sequence[i+1]){return false;}
            var pattern =[];
            var j = i+1;
            while(j<sequence.length && sequence[j].length == 4){
                pattern.push(sequence[j]);
                j++;
            }
            
            var index = pattern.indexOf(sequence[i]);
            if(index == -1 || index > (pattern.length)/2){
                continue
            }
            var seg1 = pattern.slice(0,index)
            var seg2 = pattern.slice(index+1, index * 2+1)
            if(seg1 == seg2){
                return false
            }
        }
    }
    return true;
}

function flatSequence(arr){
    var sequence = []
    for(var i = 0; i<arr.length;i++){
        if(arr[i].length == 2){
            sequence.push(arr[i])
        }else{
            var t = arr[i].slice(0,2);
            sequence.push(t);
            var t = arr[i].slice(2,);
            sequence.push(t)
        }
    }
    return sequence;
}

function checkTransProb(sequence){
    var a=0,b=0,c=0,d=0;//a-> small to small, b -> small to large, c -> large to small, d -> large to large
    for(var i = 0; i<sequence.length -1; i++){
        if (sequence[i][1] == '1' || sequence[i][1] == '2'){
            if (sequence[i+1][1] == '1' || sequence[i+1][1] == '2'){
                a++;
            }else{b++}
        }else{
            if (sequence[i+1][1] == '1' || sequence[i+1][1] == '2'){
                c++;
            }else{d++}
        }
    }
    var checkResult = {
        small_to_small : a/128,
        small_to_large : b/128,
        large_to_small : c/128,
        large_to_large : d/128
    }
    return checkResult;
}

function generateEncodeSequence(){
    var pool = [['a1','a2','a3','a4'],['b1','b2','b3','b4'],['c1','c2','c3','c4'],['d1','d2','d3','d4']];
    shuffleArray(pool);
    var pairs = generatePairs(pool[0],pool[1]);
    pairsCombo = pairs;
    console.log(pairs);
    cueCategories = [];
    targetCategories = [];
    fillerCategories = [];
    sameCategories = [];
    diffCategories = [];
    withinCategories = [];
    crossCategories = [];
    for(var i = 0; i<pairs.length;i++){
        cueCategories.push(pairs[i].slice(0,2));
        targetCategories.push(pairs[i].slice(2,4));
        if(pairs[i][0] == pairs[i][2]){
            withinCategories.push(pairs[i].slice(0,2),pairs[i].slice(2,4))
        }else{
            crossCategories.push(pairs[i].slice(0,2),pairs[i].slice(2,4))
        }
        if((Number(pairs[i][1]) <= 2 && Number(pairs[i][3]) <= 2) || (Number(pairs[i][1]) > 2 && Number(pairs[i][3]) > 2)){
            sameCategories.push(pairs[i].slice(0,2),pairs[i].slice(2,4));
        }else{
            diffCategories.push(pairs[i].slice(0,2),pairs[i].slice(2,4));
        }

    }
    console.log(cueCategories,targetCategories);
    console.log(withinCategories,crossCategories);
    console.log(diffCategories,sameCategories);
    fillerCombo = generatePairs(pool[2],pool[3]);
    fillerCategories = flatSequence(pool[2]);
    fillerCategories = fillerCategories.concat(flatSequence(pool[3]));
    console.log(fillerCategories);
    var sequence = generateSequence(pairs, pool[2],pool[3]);
    var checkResult = checkBacktoBack(sequence);
    while(!checkResult){
        sequence = generateSequence(pairs, pool[2],pool[3]);
        checkResult = checkBacktoBack(sequence);
    }
    encodeSequence = flatSequence(sequence);
}

function range(start, end, step = 1){
    const allNumbers = [start, end, step].every(Number.isFinite);
    if(!allNumbers){throw new TypeError('range() expects only finite numbers as arguments.');}
    if (step <= 0) {
      throw new Error('step must be a number greater than 0.');
    }
    const length = Math.floor(Math.abs((end - start) / step)) + 1;
    return Array.from(Array(length), (x, index) => start + index * step);
}

function generateEncodeTrials(){
    for (var i in encodeArrCollection){
        for(var j in encodeArrCollection[i]){
            var arr = range(1,30);
            shuffleArray(arr);
            var arrEncode = arr.slice(0,16);
            var arrTest = arr.slice(16,20);
            var arrFoil = arr.slice(20,28);
            encodeArrCollection[i][j]['arrEncode'] = arrEncode;
            encodeArrCollection[i][j]['arrTest'] = arrTest;
            encodeArrCollection[i][j]['arrFoil'] = arrFoil;
            encodeArrCollection[i][j]['encodeIndex'] = 0;
            encodeArrCollection[i][j]['testIndex'] = 0;
            encodeArrCollection[i][j]['foilIndex'] = 0;
        }
    }
    currTrial = 0;
    currTask++;//move to the next task
    runSessions(sessions[currSession]);
}
function generateEncodePictures(i,sequence){
    var property = sequence[i][0];
    var arr = sequence[i];
    var index = encodeArrCollection[property][arr]['encodeIndex'];
    var num = encodeArrCollection[property][arr]['arrEncode'][index];
    stimulus = sequence[i] + '_' + num;
    encodeArrCollection[property][arr]['encodeIndex']++;
    document.getElementById('encodePicture').src =  imagePath + stimulus + '.jpg';
    if(fillerCategories.indexOf(arr) !== -1){
        $('#encodePicture').data('type','filler')
        $('#encodePicture').data('predictType','NA')
        $('#encodePicture').data('sizeType','NA')
    }else if(cueCategories.indexOf(arr) !== -1){
        $('#encodePicture').data('type','cue');
        if(crossCategories.indexOf(arr) !== -1){
            $('#encodePicture').data('predictType','cross')
        }else{$('#encodePicture').data('predictType','within')};
        if(sameCategories.indexOf(arr) !== -1){
            $('#encodePicture').data('sizeType','same')
        }else{$('#encodePicture').data('sizeType','diff')}
    }else{
        $('#encodePicture').data('type','target');
        if(crossCategories.indexOf(arr) !== -1){
            $('#encodePicture').data('predictType','cross')
        }else{$('#encodePicture').data('predictType','within')};
        if(sameCategories.indexOf(arr) !== -1){
            $('#encodePicture').data('sizeType','same')
        }else{$('#encodePicture').data('sizeType','diff')}
    }    
};
/* In the DEBUG mode, only 20 size judgment trials */
function encodeTrials(){
    if(DEBUG == 0){
            if(currTrial == 0 || (currTrial == 128 && !$('#displayTrial').is(':visible'))){
            showEncodeTrials()
        }
        else if(currTrial == 128 && $('#displayTrial').is(':visible')){
            setTimeout(function(){
                $('#displayTrial').hide();
                $('#encodePicture').css({'border-color': 'white', 'border-width': '0px', 'border-style': 'solid'})
                sequenceBreak('encodeSequence');
            },displayTime)
        }
        else if(currTrial < 256 && currTrial !== 128){
            setTimeout(function(){
                showEncodeTrials();
            },displayTime)
        }
        else{
            setTimeout(function(){
                $('#displayTrial').hide();
                $('#encodePicture').css({'border-color': 'white', 'border-width': '0px', 'border-style': 'solid'})
                for (var i in saveData['encodePictures'][currTrial-1]){
                    createInputElement(saveData['encodePictures'][currTrial-1][i],i)
                }
                console.log(saveData['encodePictures']);
                currTask++;
                runSessions(sessions[currSession])    
            },displayTime)
        }
    }else{
            if(currTrial == 0 || (currTrial == 2 && !$('#displayTrial').is(':visible'))){
                showEncodeTrials()
            }
            else if(currTrial == 2 && $('#displayTrial').is(':visible')){
                setTimeout(function(){
                    $('#displayTrial').hide();
                    $('#encodePicture').css({'border-color': 'white', 'border-width': '0px', 'border-style': 'solid'})
                    sequenceBreak('encodeSequence');
                },displayTime)
            }
            else if(currTrial < 4 && currTrial !== 2){
                setTimeout(function(){
                    showEncodeTrials();
                },displayTime)
            }
            else{
                setTimeout(function(){
                    $('#displayTrial').hide();
                    $('#encodePicture').css({'border-color': 'white', 'border-width': '0px', 'border-style': 'solid'})
                    console.log(saveData['encodePictures']);
                    for (var i in saveData['encodePictures'][currTrial-1]){
                        createInputElement(saveData['encodePictures'][currTrial-1][i],i)
                    }
                    currTask++;
                    runSessions(sessions[currSession])    
                },displayTime)
            }
        }
    
}

function showEncodeTrials(){
    hideVisibleInstructions();
    document.getElementById('fixation').innerHTML = '+';
    $('#displayTrial').hide();
    $('#encodePicture').css({'border-color': 'white', 'border-width': '0px', 'border-style': 'solid'})
    document.getElementById('feedback').innerHTML = '';
    if(currTrial > 0){
        for (var i in saveData['encodePictures'][currTrial-1]){
            createInputElement(saveData['encodePictures'][currTrial-1][i],i)
        }
        console.log(saveData['encodePictures'][currTrial-1]);
    }
    generateEncodePictures(currTrial,encodeSequence);
    $('#fixation').show();
    setTimeout(function(){
        $('#fixation').hide();
        $('#displayTrial').show();
        trialStartTime = new Date();
        recordResponse('no response detected', currSession, currTask, currTrial);
        currTrial++;
        runSessions(sessions[currSession])
    },iti)
}

function sequenceBreak(sessionName){
    $('#breakScreen').show();
    $('#10sWarning').hide();
    $('#5sWarning').hide();
    $('#1sWarning').hide();
    setTimeout(function(){
        $('#10sWarning').show();
    },20000);
    setTimeout(function(){
        $('#10sWarning').hide();
        $('#5sWarning').show();
    },25000);
    setTimeout(function(){
        $('#5sWarning').hide();
        $('#1sWarning').show();
    },29000);
    setTimeout(function(){
        $('#1sWarning').hide();
        $('#breakScreen').hide();
        if(sessionName == 'encodeSequence'){
            encodeTrials();
        }else if(sessionName == 'memoryTest'){
            breakOption = true;
            memoryTestTrials();
        }
    },30000)
}

function generateMathQuestions(){
    var answer = getRandomInt(5)
    while(answer == 0){
        answer = getRandomInt(5)
    }
    var calType = getRandomInt(2);
    var num2 = getRandomInt(61);
    while(num2 == 0){
        num2 = getRandomInt(61);
    }
    var num1;
    var symbo;
    num1 = (calType == 0)? num2 + answer : num2 * answer;
    symbo = (calType == 0)? ' - ' :' / '
    stimulus = num1 + symbo + num2;
    document.getElementById('mathQuestion').innerHTML = num1 + symbo + num2 + ' = '
    $('#mathQuestion').data('answer',answer);
}
/* In the DEBUG mode, only 4 math trials */
function mathTrials(){
        if(DEBUG == 0){
            if(currTrial ==0){
            showMathTrials();
        }
        else if(currTrial<60){
            setTimeout(function(){
                showMathTrials();
            },5000)
        }else{
            setTimeout(function(){
                $('#mathDistractScreen').hide();
                $('.fixation').hide();
                console.log(saveData['mathDistract']);
                for (var i in saveData['mathDistract'][currTrial-1]){
                    createInputElement(saveData['mathDistract'][currTrial-1][i],i)
                }
                currTask++//move to the next task
                runSessions(sessions[currSession]);
            }, 5000)
        }   
    }else{
        if(currTrial ==0){
            showMathTrials();
        }
        else if(currTrial<2){
            setTimeout(function(){
                showMathTrials();
            },5000)
        }else{
            setTimeout(function(){
                $('#mathDistractScreen').hide();
                $('.fixation').hide();
                console.log(saveData['mathDistract']);
                for (var i in saveData['mathDistract'][currTrial-1]){
                    createInputElement(saveData['mathDistract'][currTrial-1][i],i)
                }
                currTask++//move to the next task
                runSessions(sessions[currSession]);
            }, 5000)
        }   
    }
    
}
function showMathTrials(){
    hideVisibleInstructions();
    $('#mathDisplayTrial').hide();
    if(currTrial>0){
        console.log(saveData['mathDistract'][currTrial-1]);
        for (var i in saveData['mathDistract'][currTrial-1]){
            createInputElement(saveData['mathDistract'][currTrial-1][i],i)
        }
    };
    generateMathQuestions();
    $('#mathDisplayTrial').show();
    trialStartTime = new Date();
    recordResponse('no response detected', currSession, currTask, currTrial);
    currTrial++;
    runSessions(sessions[currSession]);
}

function generateScrambledPairs(intactPairs){
    var scrambleCondition = getRandomInt(2);
    var list1 = [];
    var list2 = [];
    var pairs = intactPairs.slice();
    pairs.sort((a,b)=>{
        if(a[0]==a[2]&& b[0]!==b[2]){return -1};
        if(a[0]!== a[2]&& b[0]==b[2]){return 1};
    });
    list1.push(pairs[0]);
    list2.push(pairs[1]);
    // scrambleCondition == 0, swap x-x, y-x ; scrambleCondition == 1, swap x-x, x-y
    if(scrambleCondition == 0){
        if(pairs[2][2] == pairs[0][0]){
            list1.push(pairs[2]);
            list2.push(pairs[3])
        }else{
            list1.push(pairs[3]);
            list2.push(pairs[2])
        }
    }else{
        if(pairs[2][2] == pairs[0][0]){
            list1.push(pairs[3]);
            list2.push(pairs[2])
        }else{
            list1.push(pairs[2]);
            list2.push(pairs[3])};
    }
    var swapTarget = list1[0].slice(0,2).concat(list1[1].slice(2,4));
    var swapCue = list1[1].slice(0,2).concat(list1[0].slice(2,4));
    list1.push(swapTarget,swapCue);
    
    swapTarget = list2[0].slice(0,2).concat(list2[1].slice(2,4));
    swapCue = list2[1].slice(0,2).concat(list2[0].slice(2,4));
    list2.push(swapTarget,swapCue);

    var scrambledPairs = [];
    var list = [];
    list.push(list1,list2);
    for(var i = 0; i<list.length; i++){
        for(var j = 0; j<2; j++){
            var order = getRandomInt(2);
            if(order == 0){
                var comparePair = [];
                comparePair.push(list[i][j],list[i][2]);
                scrambledPairs.push(comparePair);
                comparePair = [list[i][3],list[i][j]];
                scrambledPairs.push(comparePair);
            }else{
                var comparePair = [];
                comparePair.push(list[i][2],list[i][j]);
                scrambledPairs.push(comparePair);
                comparePair = [list[i][j],list[i][3]];
                scrambledPairs.push(comparePair);
            }
        }
    }
    return scrambledPairs;    
}

function generatePredictTestSequence(){
    console.log('pairsCombo',pairsCombo);
    console.log('fillerCombo',fillerCombo);
    var predPairs = generateScrambledPairs(pairsCombo);
    var fillerPairs = generateScrambledPairs(fillerCombo);
    var sequence = predPairs.concat(fillerPairs);
    shuffleArray(sequence);
    return sequence;
}

function generatePredictTestPictures(currTrial, order, picIndex, predictTestSequence){
    var combo = predictTestSequence[currTrial][order];
    var property = combo[picIndex*2]
    var arr = combo.slice(picIndex*2,picIndex*2+2);
    var index = encodeArrCollection[property][arr]['testIndex'];
    var num = encodeArrCollection[property][arr]['arrTest'][index];
    stimulus = arr + '_' + num;
    console.log(currTrial, order, picIndex, stimulus)
    encodeArrCollection[property][arr]['testIndex']++;
    document.getElementById('predictPicture').src =  imagePath + stimulus + '.jpg';
}

function showPredictTrials(){
    hideVisibleInstructions();
    document.getElementById('predictFixation').innerHTML = '+';
    $('#predictPicture').hide();
    if(order == 0 || order == 1){
        generatePredictTestPictures(currTrial,order,picIndex,predictTestSequence);
        if(picIndex == 0){
            $('#predictFixation').show();
            setTimeout(function(){
                $('#predictFixation').hide();
                console.log('display 1st picture')
                $('#predictPicture').show();
                picIndex++;
                console.log(currTask,currTrial,order,picIndex)
                runSessions(sessions[currSession]);
            },1000);
            
        }else{
            setTimeout(function(){
                $('#predictPicture').show();
                picIndex = 0;
                order++;
                console.log(currTask,currTrial,order,picIndex);
                runSessions(sessions[currSession]);
            },500)
        }
    }else{
        $('#predictTestTrialInstruction').show();
        console.log('predictTestResponse');
        trialStartTime = new Date();
        recordResponse('no response detected', currSession, currTask, currTrial);
        currTrial++;
        console.log(currTrial, order)
    }
}

function predictTestTrials(){
    if(DEBUG == 0){
    if(currTrial < 16 && order == 0 && picIndex == 0){
        if(currTrial >0){
            for (var i in saveData['predictTest'][currTrial-1]){
                createInputElement(saveData['predictTest'][currTrial-1][i],i)
            }
        }
        showPredictTrials();
    }else if(currTrial < 16){
        setTimeout(function(){
            showPredictTrials();
        },1000)
    }else if(currTrial == 16){
        console.log(currTrial);
        $('#predictTestScreen').hide();
        for (var i in saveData['predictTest'][currTrial-1]){
            createInputElement(saveData['predictTest'][currTrial-1][i],i)
        }
        console.log(saveData['predictTest'])
        currTask++;
        runSessions(sessions[currSession]);
    }
}else{
        if(currTrial < 2 && order == 0 && picIndex == 0){
            if(currTrial >0){
                for (var i in saveData['predictTest'][currTrial-1]){
                    createInputElement(saveData['predictTest'][currTrial-1][i],i)
                }
            }
            showPredictTrials();
        }else if(currTrial < 2){
            setTimeout(function(){
                showPredictTrials();
            },1000)
        }else if(currTrial == 2){
            console.log(currTrial);
            $('#predictTestScreen').hide();
            for (var i in saveData['predictTest'][currTrial-1]){
                createInputElement(saveData['predictTest'][currTrial-1][i],i)
            }
            console.log(saveData['predictTest'])
            currTask++;
            runSessions(sessions[currSession]);
        }
    }
}

function generateMemoryTestSequence(){
    var testPictures = [];
    for(var j in encodeArrCollection){
        for (var i in encodeArrCollection[j]){
            for (var k = 0; k < encodeArrCollection[j][i]['arrEncode'].length; k++){
                var pic = i + '_' + encodeArrCollection[j][i]['arrEncode'][k];
                testPictures.push(pic);
            }
            for (var k = 0; k < encodeArrCollection[j][i]['arrFoil'].length; k++){
                var pic = i + '_' + encodeArrCollection[j][i]['arrFoil'][k];
                testPictures.push(pic);
            }
        }
    }
    shuffleArray(testPictures);
    memoryTestPictures = testPictures.slice();
    currTask++;
    runSessions(sessions[currSession]);
}

function generateMemoryTestPicture(){
    stimulus = memoryTestPictures[currTrial];
    document.getElementById('memoryTestPicture').src =  imagePath + stimulus + '.jpg';
    var key1 = memoryTestPictures[currTrial].slice(0,1);
    var key2 = memoryTestPictures[currTrial].slice(0,2);
    var num = memoryTestPictures[currTrial].slice(3,);
    console.log(encodeArrCollection[key1][key2]['arrEncode']);
    console.log(num);
    console.log(encodeArrCollection[key1][key2]['arrEncode'].indexOf(Number(num)));
    if(encodeArrCollection[key1][key2]['arrEncode'].indexOf(Number(num)) == -1){
        $('#memoryTestPicture').data('correctResponse','foil')
    }else{
        $('#memoryTestPicture').data('correctResponse','old')
    }
    if(fillerCategories.indexOf(key2) !== -1){
        $('#memoryTestPicture').data('type','filler')
        $('#memoryTestPicture').data('predictType','NA')
        $('#memoryTestPicture').data('sizeType','NA')
    }else if(cueCategories.indexOf(key2) !== -1){
        $('#memoryTestPicture').data('type','cue');
        if(crossCategories.indexOf(key2) !== -1){
            $('#memoryTestPicture').data('predictType','cross')
        }else{$('#memoryTestPicture').data('predictType','within')};
        if(sameCategories.indexOf(key2) !== -1){
            $('#memoryTestPicture').data('sizeType','same')
        }else{$('#memoryTestPicture').data('sizeType','diff')}
    }else{
        $('#memoryTestPicture').data('type','target');
        if(crossCategories.indexOf(key2) !== -1){
            $('#memoryTestPicture').data('predictType','cross')
        }else{$('#memoryTestPicture').data('predictType','within')};
        if(sameCategories.indexOf(key2) !== -1){
            $('#memoryTestPicture').data('sizeType','same')
        }else{$('#memoryTestPicture').data('sizeType','diff')}
    }    
    console.log('end of generateMemoryPicture function')
}

function showMemoryTestTrials(){
    // $('#memoryTestScreen').css('margin-top',0);
    hideVisibleInstructions();
    $('#memoryTestPicture').hide();
    generateMemoryTestPicture();
    $('#memoryTestInstruct').hide();
    $('.memoryTestOptions').hide();
    if(currTrial>0){
        console.log(saveData['memoryTest'][currTrial-1]);
        for (var i in saveData['memoryTest'][currTrial-1]){
            createInputElement(saveData['memoryTest'][currTrial-1][i],i)
        }
    };
    document.getElementById('memoryTestFixation').innerHTML = '+';
    $('#memoryTestFixation').show();
    setTimeout(function(){
        $('#memoryTestFixation').hide();
        
        $('#memoryTestPicture').show();
        console.log('showed test picture')

        trialStartTime = new Date();
        $('#memoryTestInstruct').show();
        if(testOption == 0){
            $('#memoryTestOption1').show();
        }else{
            $('#memoryTestOption2').show();
        }
        recordResponse('no response detected', currSession, currTask, currTrial);
        currTrial++;
    },1000)
}
/* In the DEBUG mode, only 4 memory test trials */
function memoryTestTrials(){
    if(DEBUG == 0){
            if(currTrial < 192 || (currTrial > 192 && currTrial < 384)|| (currTrial == 192 && breakOption)){
            showMemoryTestTrials();
        }else if(currTrial == 192 && !breakOption){
            $('#memoryTestPicture').hide();
            $('#memoryTestInstruct').hide();
            $('.memoryTestOptions').hide();
            sequenceBreak('memoryTest');
        }
        else{
            $('#memoryTestScreen').hide();
            for (var i in saveData['memoryTest'][currTrial-1]){
                createInputElement(saveData['memoryTest'][currTrial-1][i],i)
            }
            console.log(saveData['memoryTest']);
            currTask++;
            runSessions(sessions[currSession]);
        }
    }else{
        if(currTrial < 2 || (currTrial > 2 && currTrial < 4)|| (currTrial == 2 && breakOption)){
            showMemoryTestTrials();
        }else if(currTrial == 2 && !breakOption){
            $('#memoryTestPicture').hide();
            $('#memoryTestInstruct').hide();
            $('.memoryTestOptions').hide();
            sequenceBreak('memoryTest');
        }
        else{
            $('#memoryTestScreen').hide();
            for (var i in saveData['memoryTest'][currTrial-1]){
                createInputElement(saveData['memoryTest'][currTrial-1][i],i)
            }
            console.log(saveData['memoryTest']);
            currTask++;
            runSessions(sessions[currSession]);
        }
    }
    
}
function checkHitAccepted(){
    if(SANDBOX == 1){
        document.getElementById("mturk_form").action = "https://workersandbox.mturk.com/mturk/externalSubmit";
    }else{
        document.getElementById("mturk_form").action = "https://www.mturk.com/mturk/externalSubmit";
    }
    // assignId = $.getUrlVar("assignmentId");
    // workerId = $.getUrlVar("workerId");
    // createInputElement(subj,'workerID');
    // document.getElementById("assignmentId").value = assignId;
}


// <--------below Pavlovia setting----------->
// function postTest(){
//     $(document).unbind('keydown');
//     var pavlovia_init = {
//         type: "pavlovia",
//         command: "init"
//     };

//     var fake_trial = {
//         type: 'html-keyboard-response',
//         stimulus: "<p>Please wait while your data is being saved...</p> <p><b>Don't press any buttons, refresh the page, or close any tabs or windows. Thanks!</b></p>",
//         choices: jsPsych.NO_KEYS,
//         trial_duration: 4000,
//         on_finish: function() {

//         // collect screen size, etc again now that expt is complete


//         // replace jsPsych data object with our custom one
//         var dataCollection = [];
//         for(var i in saveData){
//             for (var j = 0; j<saveData[i].length; j++){
//                 dataCollection.push(saveData[i][j])
//             };
//         }
//         dataCollection.push({pairsCombo:pairsCombo},{encodeSequence:encodeSequence},{fillerCombo:fillerCombo},{predictTestSequence:predictTestSequence},{memoryTestSequence:memoryTestSequence},{cueCategories:cueCategories},{targetCategories:targetCategories},{withinCategories:withinCategories},{crossCategories:crossCategories},{fillerCategories:fillerCategories},{sameCategories:sameCategories},{diffCategories:diffCategories});
//         jsPsych.data._customInsert(dataCollection) 
//         // add subject ID as column
//             // jsPsych.data.addProperties({subject: subj});
//             console.log('finished fake trial')

//         }

//     }
//     var pavlovia_finish = {
//         type: "pavlovia",
//         command: "finish",

//         on_finish: function() {
//             console.log('finished pavlovia finish')
//         }
//     };

//     var showCode = {
//         type: 'html-keyboard-response',
//         stimulus: '<p style="font-size:24px; ">The Code is: <b>1080</b> Please enter the code into mTurk before closing this window. Thank you!</p>',
//         choices: jsPsych.NO_KEYS,
//         on_finish: function(){
//             console.log('code is "1080"')
//         }
//     }

//     var wait4sec = {
//       type: 'html-keyboard-response',
//       stimulus: "<p>Please wait while your data is being saved...</p> <p><b>Don't press any buttons, refresh the page, or close any tabs or windows. Thanks!</b></p>",
//       choices: jsPsych.NO_KEYS,
//       trial_duration: 4000
//     };

//     var timeline = [];
//     timeline.push(pavlovia_init, fake_trial, pavlovia_finish, wait4sec, showCode)
//     jsPsych.init({
//     timeline: timeline
//     })
// }
