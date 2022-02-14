current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
source('anovatab.R')
bw <- read.table(file = 'breadwrapper.txt',header=TRUE,sep='')
fit_bw <- lm(Seal_Strength~sealtemp+I(sealtemp^2)+polyethylene+I(polyethylene^2), data=bw)
anovatab(fit_bw)


bf_study <- read.csv(file = 'bf_study.csv')
