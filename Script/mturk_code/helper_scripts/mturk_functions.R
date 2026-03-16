
# retrieve responses for a given hit's assignment ID
# also should accept vectors of multiple assignment ID but I haven't tested this in awhile
get.answers = function (hits) {
  s <- GetAssignments(hit = hits,
                      get.answers = TRUE)
  # if there are assignments available, re-structure Answers form
  if (dim(s$Answers)[1] > 0) {
    answers = reshape2::dcast(WorkerId ~ QuestionIdentifier, value.var = 'FreeText', data = s$Answers)
    x_all <- merge(s$Assignments, answers, by = 'WorkerId')
    # commented out code was used for pymturkr 0.3.7; current version (1.1.4)
    # has resolved a bunch of the issues we needed to solve
    # due to ANOTHER bug in GetAssignments, having multiple hits will create duplicated rows of the same assignment
    # this gets rid of identical rows so there is only one assignment per row
    # s_unique <- s[!duplicated(s),]
    #
    # x_all = data.frame()
    # for (i in 1:dim(s_unique)[1]) {
    #
    #   x = s_unique[i,]
    #
    #   ans = XML::xmlToList(x$Answer)
    #   dat = do.call(rbind, ans)
    #
    #   dat1 = as.array(dat[,2])
    #   names(dat1) <- dat[,1]
    #
    #   dat2 = data.frame(do.call(cbind, dat1))
    #   for (var in colnames(dat2)) {
    #     attr(dat2[,deparse(as.name(var))], "names") <- NULL
    #   }
    #
    #   dat2[, ] <- lapply(dat2[, ], as.character)
    #
    #   x_data = cbind(x[,1:10], dat2)
    #   x_all <- plyr::rbind.fill(x_data, x_all)
    # }
    return(x_all)
  }
}

# requestor-created qualification IDs (annoyingly) are different on sandbox
# so if you want to check that they are working on sandbox you have to re-make them there.
# This picks out the correct qual IDs based on if you're in the sandbox or not
set.qual.ids = function (sandbox) {
  
  # has done memory task
  has_done_mb     <- ifelse(sandbox == 1, 
                            '3Z6U0SAT6YUV5FTBCA9OL14BV6VJJP', 
                            '3NWWSPF6WT8SV0PY4QMF02NCKJKU60')
      
  # invited back for memory follow-up
  mb_invited_back <- ifelse(sandbox == 1,
                            '3KPNUZIAIW5SJSIHYL0X5OSAOXRH1E',
                            '3J9ZK359J5FDPKBIJ0EXIX6VZVQ319')
  
  # has done synonym task
  has_done_syn    <- ifelse(sandbox == 1,
                           '3KPNUZIAIW5SJSIHYL0X5OSAOXRH1E',
                           '3IGIWF3LEGL55UX5XD336IHYGKXLI4')
                         
  return(list(taken_mb  = has_done_mb, 
              mb_return = mb_invited_back,
              taken_syn = has_done_syn))
  
}

# function to make qualification IDs for a new hit, combining
# requestor-made quals with standard ones 
# (location, numer of HITs completed, approval rate, etc.)
make.initial.quals <- function(sandbox) {
  
  mb_quals = set.qual.ids(sandbox)
  
  # make qualification requirement for recognition HITs
  # preview = TRUE; HIT cannot be seen unless worker meets qualifications
  
  if (sandbox==0) {
    
    quals.list <- list(
      list(QualificationTypeId = '39CRKCMYSXZ25T3YFHD921RFKPWSOG', #have done it before # preview is True unless the qualificationId does not exist on this participant
           Comparator = "DoesNotExist",
           IntegerValues = 1,
           RequiredToPreview = TRUE
      ),
      list(QualificationTypeId = "000000000000000000L0", #percent Hits aaproved
           Comparator = ">",
           IntegerValues = 95,
           RequiredToPreview = TRUE
      ),
      list(QualificationTypeId = "00000000000000000040", #number Hits approved
           Comparator = ">",
           IntegerValues = 100,
           RequiredToPreview = TRUE
      ),
      list(QualificationTypeId = "00000000000000000071", #workerLocale
           Comparator = "EqualTo",
           LocaleValues = list(Country = "US"),
           RequiredToPreview = TRUE
      )
    )
  } else if (sandbox == 1) {
    
    quals.list <- list(
      #list(QualificationTypeId = "000000000000000000L0",
           #Comparator = ">",
           #IntegerValues = 97,
          # RequiredToPreview = TRUE
      #),
      list(QualificationTypeId = "00000000000000000071",
           Comparator = "EqualTo",
           LocaleValues = list(Country = "US"),
           RequiredToPreview = TRUE
      )
    )
  }
  
  quals <- GenerateQualificationRequirement(quals.list)
  
}

# generate qualification ID for participants for whom
# who you have manually set a qual - in my case for participants I
# invite to participate in another experiment
make.return.quals <- function(sandbox, return) {
  
  mb_quals = set.qual.ids(sandbox)
  
  quals.list <- list(
    list(QualificationTypeId = mb_quals$mb_return,
         Comparator = "=",
         IntegerValues = return,
         RequiredToPreview = TRUE
    )
  )
  
  quals <- GenerateQualificationRequirement(quals.list)
  
}

# this posts a hit that is available only for workers who 
# haven't already participated in a past version
post.novel.syn.hit <- function(sandbox, qf, n_assign, cost) {
  
  all_quals = make.initial.quals(sandbox)
 
  # this creates AND posts hits!!!!!
  hit <- CreateHIT(expiration = seconds(days = 3),
                   assignments = n_assign,
                   title = 'Size Judgment Task (~ 50 min, $5 + bonus)',
                   description = 'In this study, you will judge the size of the object depicted in a photo. The whole study will take about 50 minutes. This HIT will expire 120 minutes after you accept it. Please begin to work on the HIT immediately after you accept it. We will not compensate for expired HIT. In addition, if you began or completed another size-judgment study through this lab, you are not allowed to complete this HIT. Please direct your full attention towards the task. We check scammers throughout and will reject possible scammer assignments. You will be paid $5 for completing this study and will be rewarded a bonus $2.5 based on your performance in the image size judgment task.',
                   keywords = 'psychology, object size',
                   duration = seconds(minutes =  120),
                   reward = cost,
                   auto.approval.delay = seconds(days = 3),
                   qual.req = all_quals,
                   question = qf)
  
  return(hit$HITId)
}
