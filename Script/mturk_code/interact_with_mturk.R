Sys.setenv(RETICULATE_PYTHON = '/Users/leichen/opt/miniconda3/bin/python3')
Sys.setenv(AWS_SECRET_ACCESS_KEY = "LPOPw2rWsyKlUspTTioI1m7RDjpy6y0LOMgYh68j", AWS_ACCESS_KEY_ID = "AKIAINKEX26LDYLGGGZQ")
library(reticulate)
# I treat this as a more interactive script - I don't run it all at once but
# instead will run the set-up, and then skip to different sections depending on
# whether I need to post a new hit, check on one that's already been posted, etc.
# Note: must be run from the mturk_code directory


################ set-up ##########################################################
setwd('/Users/leichen/Research project/Predictive memory/Script/mturk_code')
proj_dir = getwd()
source(paste(proj_dir, 'helper_scripts/mturk_functions.R', sep = '/'))


library(pyMTurkR)
library(plyr)


#  choose whether you are submitting your HIT to the sandbox or posting it for real
sandbox = 0
ifelse(sandbox == 0, 
       options("pyMTurkR.sandbox" = FALSE), 
       options("pyMTurkR.sandbox" = TRUE))


################ configure stimulus presentation code #############################


# import stimulus presentation code into R
f2 <- paste(getwd(), 'index.html', sep='/')
qf <- GenerateHTMLQuestion(file = f2, frame.height = 975)
mturk_code <- qf$string


# overwrite SANDBOX variable in synonyms.html 
# to either post  to the sandbox or to the real site
# mturk_code = gsub('SANDBOX = 1|SANDBOX = 0', paste('SANDBOX = ', sandbox), qf$string)

# overwrite TURK variable in synonyms.html always be configured for use on mturk
# if TURK = 0 in synonyms.html you can run the script locally in a browser, but
# responses will not be compiled at the end
# mturk_code = gsub('TURK = 0', paste('TURK = 1'), mturk_code)

# if not on sandbox, then make sure debug is turned off
# if sandbox equals 1, then debug will not change,
# so DEBUG is set as in the synonyms.html file will still be true
# if (sandbox == 0) {
#   mturk_code = gsub('DEBUG = 1', paste('DEBUG = 0'), mturk_code)
# }



################ post on mturk #################################################


# post experiment
syn_hit <- post.novel.syn.hit(sandbox, mturk_code, n_assign = 8, cost = '5')

# extend hit in various ways (add subjects or time)
ExtendHIT(hit = syn_hit, add.assignments = 9)
ExtendHIT(hit = syn_hit, add.seconds = seconds(days = 1))

# if you messed up, take down the hit
ExpireHIT(syn_hit)



################ check on posted hits #############################################

# I like to paste the assignments of various hits here 
# as lazy way of keeping a record, instead of saving 
# syn_hit directly in a data file. Also is helpful when 
# I've posted multiple hits with different stim presentation 
# code and want to check on them both
#syn_hit = '3DTJ4WT8BDFK7QQFPIK104UBPYQEZI' #debug version on sandbox
#syn_hit = '36818Z1KV3DGWK2AO6N60KZ5PPH3A5' #complete version on sandbox

#syn_hit = '3S829FDFT21QAPC0S5OYE5CO4RXXD8' #pilot posted on mturk August 11 4:35pm, 9 slots, $5/person
#syn_hit = '30ZKOOGW2W6KMHOV129NCRK8RBGA19'#sandbox test debug space key problem August 23 
#syn_hit = '3U18MJKL1UMBT3SIQ33CFTNZUF6CNL' #sandbox test debug space key problem August 23
#syn_hit = '3EFNPKWBMSOKV74SW4TFSI313SP03Q'#pilot posted on mturk August 23 8:38pm, 9 slots, $5/person
#syn_hit = '38Z7YZ2SB32N93RAY5UHBUW8ZGVIQL' # pilot posted on mTurk August 24 2:45pm, 9 slots, $5/person

syn_hit = c('3S829FDFT21QAPC0S5OYE5CO4RXXD8', '3EFNPKWBMSOKV74SW4TFSI313SP03Q', '38Z7YZ2SB32N93RAY5UHBUW8ZGVIQL') # August 3 HITs

syn_hit = '3R6RZGK0XFC14B2WOMAU35P4792YVW' #pilot posted on mTurk September 3, 11:25 am, 9 slots, $7.5/person

#syn_hit = '3BAKUKE49HCCLYAC3DYBOLPRFR6R16' #pilot posted on mTurk September 8, 8:04 am, 9 slots, $5/person + $2.5 bonus

syn_hit = '3FW4EL5A3LO26ZBN7RJOSWROV7322M' #pilot posted on mTurk September 9, 10.05 am, 9 slots, $5/person + $2.5 bonus

syn_hit = '3HEM8MA6H9CFQPEEB0K1KRXKMPWQP2' #pilot posted on mTurk September 10, 9:32 am, 9 slots, $5/person + $2.5 bonus

syn_hit = c('3S829FDFT21QAPC0S5OYE5CO4RXXD8', '3EFNPKWBMSOKV74SW4TFSI313SP03Q', '38Z7YZ2SB32N93RAY5UHBUW8ZGVIQL','3BAKUKE49HCCLYAC3DYBOLPRFR6R16','3FW4EL5A3LO26ZBN7RJOSWROV7322M','3R6RZGK0XFC14B2WOMAU35P4792YVW','3HEM8MA6H9CFQPEEB0K1KRXKMPWQP2') ## all 7 HITs, 33 usable data

syn_hit = c('3R6RZGK0XFC14B2WOMAU35P4792YVW','3BAKUKE49HCCLYAC3DYBOLPRFR6R16','3FW4EL5A3LO26ZBN7RJOSWROV7322M','3HEM8MA6H9CFQPEEB0K1KRXKMPWQP2') ## same material 4  HITs in early September

syn_hit = c('3BAKUKE49HCCLYAC3DYBOLPRFR6R16','3FW4EL5A3LO26ZBN7RJOSWROV7322M','3HEM8MA6H9CFQPEEB0K1KRXKMPWQP2') ## motivation 3 HITs

syn_hit = '3GL25Y6843UTEJID5YN1XVZIF3QMXE' #posted Sep 20, 2021, 8:45 am. 12 slots, $5 + $2.5 bonus / participant ##terrible HIT

syn_hit = '329E6HTMSW2M8XBW0X0VUY21ULYK3S' #posted Sep 21, 2021, 2:53 pm. Sandbox + debug

syn_hit = '3Y7LTZE0YTM1WMN1FIZ64MGEB7VZU1' #posted Sep 21, 2021, 3:28 pm. Sandbox
syn_hit = '32K26U12DNOY6075SQKE9GZU1WVVDO' #posted Sep 21, 2021, 4:53 pm. Sandbox for Nina

syn_hit = '3RSBJ6YZECQ0E0C4IKTQ2O87HGYFO1' #posted Sep 21, 2021, 5:02 pm. Sandbox for Nina
syn_hit = c('3RSBJ6YZECQ0E0C4IKTQ2O87HGYFO1','3Y7LTZE0YTM1WMN1FIZ64MGEB7VZU1')

syn_hit = '3BO3NEOQM0HVMNKT99UT4Q1U8S6AIA' #posted Sep 23, 2021, 6:25 pm. Sandbox

syn_hit = '33EEIIWHK77SV9BAXCQY9F6TKCQVQ1' #posted Sep 23, 2021, 22:16 pm. Sandbox

syn_hit = '3VMHWJRYHVGN89QWJBGML1Z0TLQFXV' #posted Sep 23, 2021, 23:26 pm. Sandbox + debug

syn_hit = '3UV0D2KX1MJWA72MZ0EHOQ220QM4F6' #posted Sep 24, 2021, 12:08 am, Sanbox + debug


syn_hit = '3WUVMVA7OB3HHD939DLHLPKTTVJAZH' #posted Sep 24, 2021, 9:34 am. 12 slots, $5/participant + $2.5 bonus

syn_hit = '39I4RL8QGJHGO3TSUQ5J8FUO5WKH43' #posted Sep 25, 2021, 8:02 am, 6 slots, $5/participant + $2.5 bonus

syn_hit = '3RWB1RTQDJN9KLEIP631Y0CKTDP8PD' # posted Sep 27, 2021, 10:00 am, 12 slots, $5/participant + $2.5 bonus

syn_hit = '3VEI3XUCZRXF2QTEOF91PH7PJ64PRZ' #posted Sep 27, 2021, 3:23 pm, 6 slots, $5/participant + $2.5 bonus

syn_hit = '3Z56AA6EK40GV4YH4BEBXIMYIFKM69' #posted Sep 28, 2021, 10:06 am, 12 slots, $5/participant + $2.5 bonus

syn_hit = '3IVKZBIBJ09ZQOV3KR116IAUJSBHSU' #posted Sep 30, 2021, 3:28 pm, 6 slots, $5/participant + $2.5 bonus

syn_hit = '3QMELQS6Y5B7SKB99JASJHG8OE5R6C' #posted Oct 1, 2021, 9:24 pm, 8 slots, $5/participant + $2.5 bonus

syn_hit = c('3WUVMVA7OB3HHD939DLHLPKTTVJAZH','39I4RL8QGJHGO3TSUQ5J8FUO5WKH43', '3RWB1RTQDJN9KLEIP631Y0CKTDP8PD','3VEI3XUCZRXF2QTEOF91PH7PJ64PRZ', '3Z56AA6EK40GV4YH4BEBXIMYIFKM69','3IVKZBIBJ09ZQOV3KR116IAUJSBHSU', '3QMELQS6Y5B7SKB99JASJHG8OE5R6C') #version 2 all 7 HITs

syn_hit = c('3RWB1RTQDJN9KLEIP631Y0CKTDP8PD','3Z56AA6EK40GV4YH4BEBXIMYIFKM69', '3VEI3XUCZRXF2QTEOF91PH7PJ64PRZ','3IVKZBIBJ09ZQOV3KR116IAUJSBHSU')
# this gives you a data frame similar to what mturk gives you through its website
# with WorkerID, time submitted, and other info, in addition to any response 
# variables you've included in the experiment presentation code
s <- get.answers(hit = syn_hit)
s$Comments # quick check of anything that workers left for me in the Comment box at the end
approved <- s[s$AssignmentStatus != 'Rejected',]

# approve assignments/pay workers
submitted <- s[s$AssignmentStatus =='Submitted',]$AssignmentId
ApproveAssignment(submitted)

#send bonus money
bonusWorkers <- s[s$AssignmentStatus !='Rejected',]$WorkerId
bonusAssignments <- s[s$AssignmentStatus !='Rejected',]$AssignmentId
#GrantBonus(bonusWorkers,bonusAssignments,'2.5','Thanks for your great work on my HITs. We are making compensation for underpaid work. Hope to work with you again in the future!')
bonusWorkers <- includeList$WorkerId
bonusAssignments <- s[s$WorkerId %in% includeList$WorkerId,]$AssignmentId
GrantBonus(bonusWorkers, bonusAssignments, '2.5', 'This is the bonus for the image size judgment task. Thank you so much for your excellent work!')

# reject assignments
rejectList <- subset(s, WorkerId %in% scammerList$WorkerId)
rejectList <- subset(s, !WorkerId %in% scammerCheck$WorkerId)
rejected <- rejectList$AssignmentId

RejectAssignment(rejected, 'Sorry, your data is incomplete')
RejectAssignment(rejected,'You did not respond to any of the size judgement questions.')
RejectAssignment(rejected,'Your performance on size judgement task is significantly lower other workers and you did not respond to any of the math questions.')
RejectAssignment(rejected,'Your performance on size judgement task is significantly lower other workers and you did not respond to the majority of math questions.')

# assign qual to all who completed the hit so they can't take it again
# value can be any number you like - I use it to keep track of who
# completed different versions of the same experiment
all_quals <- set.qual.ids(sandbox)
AssignQualifications('39CRKCMYSXZ25T3YFHD921RFKPWSOG', s$WorkerId, value ='1') #has done size judgment task
#AssignQualifications('3ZA92V7SPMRJRPAWG64RQ6S83NQC9U', 'A13WTEQ06V3B6D', value = '1') #make up hit qualifications
#AssignQualifications('3ZA92V7SPMRJRPAWG64RQ6S83NQC9U', 'A29DS7SH4Y2ACU', value = '1') #make up hit qualifications
#AssignQualifications('3ZA92V7SPMRJRPAWG64RQ6S83NQC9U', 'A15WJ1MM05HTSA', value = '1') #compensation hit qualifications



############## contact workers ################################################
# example of an email inviting workers back for a different experiment

invite_back = c("WORKERID_1", "WORKERID_2", "ETC")

message = "Hello,

You are receiving this email because you completed 'Remember images and their locations' in the past 4 - 9 days. Based on your good performance in the memory task, we'd like to invite you to participate in the same experiment, this time using different images.

The instructions for this experiment are identical to what you completed earlier. You will receive $8.00 for completing the task.

This HIT will be available over the next three days. We encourage you to work on it whenever is convenient for you -- it cannot be completed by anyone else so it will be available to you until you complete it or return it.

Please search for 'Remember images and their locations: Round 2' or for the requester 'STS Lab' to find this HIT. If you have any questions or if you experience any issues, feel free to email us at schilllab@gmail.com.

Best,

Alexa
STS Lab"

ContactWorkers(
  subjects = "Invitation to complete experiment: $8.00",
  msgs = message,
  workers = invite_back,
  batch = TRUE
)

