#gets the path to the folder where this R script is in
current_path = rstudioapi::getActiveDocumentContext()$path
#set current working directory to the folder this script is in
setwd(dirname(current_path))

ldlData <- read.csv(file = 'ldldata.csv')
colnames(ldlData)<-c('weight','ldl')

#pdf('ldlplot.pdf',width = 11.75, height = 7.25, paper='a4r')
plot(ldlData$weight,ldlData$ldl)
fitLdl=lm(ldl~weight,data=ldlData)
abline(fitLdl,col='red',lwd=2)
summary(fitLdl)
