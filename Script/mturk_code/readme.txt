Files included in this directory:
interact_with_mturk.R 	- code that relies on the PyMturkR package to post hits, pay workers, set qualifications, and send emails
synonyms.html 			- a stimulus presentation script coded in javascript/HTML. On each trial, a target word and three options are presented. The task is to choose the synonym of the target word

In helper scripts:
mturk_functions.R 		- a few functions that are used in interact_with_mturk.R, just to keep it readable
*style.css 		 		- design file for the stim presentation code
*synonyms.js 	 		- a js variable of the stimuli used in synonyms.html
*support_functions.just - javascript functions that I use for multiple experiments

* Currently synonyms.html sources a copy of these three files that are hosted on my personal website. You can source these local versions if you want to make changes to them.


NOTE: At the moment, you can run synonyms.html directly in your browser, and it will progress through the experiment but it won't give you the data at the end - there is some code that's commented out that can be used to produce a csv of the responses, but at the moment it's not compatible with the mturk platform. If you want to use this functionality let me know and I can tell you how to get it going. But as currently written, it works on mturk if you post it through interact_with_mturk.R
