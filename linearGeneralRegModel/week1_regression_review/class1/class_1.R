"
(1) Download the optometry_SE_AC.xls spreadsheet and open it in Excel. Save it as a csv (comma separated value file) - it should now be called optometry_SE_AC.csv. 

(2) Open optometry_SE_AC.csv in a text editor to take a look at its structure.

(3) Read the data in this file into R as a dataframe.

(4) Rename the two columns to make them easier to reference.

(5) Get a report-ready scatter plot of mean spherical error (on the vertical axis) against  anterior chamber depth - include nice plot and axis labels etc.

(6) Fit a simple linear regression model to these data with spherical error as the response. What do you conclude?

(7) Plot the simple ls line on your scatter plot and include a suitable descriptive legend for this line.

(8) Fit a quadratic curved line as well - is there any evidence that we need a curved line here?

(9) In any case, plot the curved line on the scatter plot and amend the legend accordingly.

(10) Save the plot as a .png image or .jpeg image for inclusion in a report.

"
######################################
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
optometryData <- read.csv(file = 'optometry_SE_AC.csv')
colnames(optometryData)<-c('MeanRefractiveErrors','ChamberDepthMM')

#pdf('optometryPlot.pdf',width = 11.75, height = 7.25, paper='a4r')
plot(optometryData$ChamberDepthMM, 
     optometryData$MeanRefractiveErrors, 
     main = "Mean Refractive Errors\n explained by Chamber Depth",
     xlab = "Chamber Depth (MM)",
     ylab = "Mean Refractive Errors")
fitOptometry=lm(MeanRefractiveErrors~ChamberDepthMM,data=optometryData)
abline(fitOptometry,col='red',lwd=2)
legend('topleft', legend='Least Squares Line',col='red',lwd=1)
#dev.off()

#summary(fitOptometry)
#the bigger the chamber Dept, smaller it's the error, 
#this model is significative and explains 25.97% of the error variation
jpeg("fitOptometryCurved.jpg", width = 350, height = 350)
plot(optometryData$ChamberDepthMM, 
     optometryData$MeanRefractiveErrors, 
     main = "Mean Refractive Errors\n explained by Chamber Depth",
     xlab = "Chamber Depth (MM)",
     ylab = "Mean Refractive Errors")
fitOptometryCurved=lm(MeanRefractiveErrors~ChamberDepthMM+I(ChamberDepthMM^2),data=optometryData)
summary(fitOptometryCurved)
curve(-2.708+5.233*x-1.326*x^2,from=2,to=86,col='red',add=T,lwd=2)
#The lis doesn't differ much from the linear model one 
#dev.off()