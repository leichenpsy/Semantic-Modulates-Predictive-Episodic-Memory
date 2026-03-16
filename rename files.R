
setwd('/Users/leichen/Research project/Predictive memory/experiment materials/Experiment images/desk')
old_files <- list.files(pattern = '*',full.names = TRUE)
new_files <- paste0('/Users/leichen/Research project/Predictive memory/experiment materials/Experiment images/d4_',40:44,'.jpg')
file.copy(from = old_files, to = new_files)
file.remove(old_files)

