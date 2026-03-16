temp = read.csv('concept similarity matrix.csv', sep=',')
setwd('/Users/leichen/Research project/Predictive memory')
rownames(temp) <- temp[,1]
temp[,1] <- NULL
corrplot(as.matrix(temp), method = 'circle', type = 'upper',  order = 'original',#p.mat = round(pc_stress_1w$p.value,digits=3),
# sig.level = c(.001, .01, .05), pch.cex = .9,
insig = 'label_sig', pch.col = 'red',
tl.col = 'black', tl.srt = 45,cl.lim = c(-1,1))
library(corrplot)
