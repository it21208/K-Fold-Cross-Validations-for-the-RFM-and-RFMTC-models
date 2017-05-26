source("../../MyRLibs/myRFMLib.R")
# Read data from RFM, RFMTC-Our, RFMTC-Ref data files
rfmData <- read.csv(file.choose(), header = TRUE)
ourrfmtcData <- read.csv(file.choose(), header = TRUE)
refrfmtcData <- read.csv(file.choose(), header = TRUE)
# Construct Validation Data Frame
len = nrow(rfmData)
validateData <- as.data.frame(cbind(rfmData[,c(1, 3, 5)], rep(0, times=len), rep(0, times=len)))
colnames(validateData)[4:5] <- c("RFMTC Our Res Prob", "RFMTC REF Res Prob")
# Define a reference order for the validation data frame
validateData <- validateData[order(validateData[, custIdColName]),]
# Lookup RFM Customer Response Probabilities from rfmAggrData data frame
# and fill them into the Validation data frame
for(i in 1:len) {
  validateData$`RFMTC Our Res Prob`[i] <- ourrfmtcData$E.X.L..L.1.[ourrfmtcData[, custIdColName] == validateData[i, custIdColName]]
  validateData$`RFMTC REF Res Prob`[i] <- refrfmtcData$E.X.L..L.1.[refrfmtcData[, custIdColName] == validateData[i, custIdColName]]
}
#validateData <- validateData[order(-validateData$RFM.Score, -validateData$E.X.L..L.1.),]
validateData <- validateData[order(-validateData$`RFMTC REF Res Prob`),]
plot(1:len, cumsum(validateData$RFM.Resp.Prob), 
     type = "l", col = "red", 
     main = "BloodTransfusion Full Dataset RFMTC(our/ref) vs RFM Lift Chart", ylab = "number of cumulative response", xlab = "ranking of the data according to model")
legend(0,170, # places a legend at the appropriate place 
       legend = c("RFM","RFMTC-our","RFMTC-ref"), # puts text in the legend
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5),
       col=c("red","blue","green")) # gives the legend lines the correct color and width
#validateData <- validateData[order(-validateData$E.X.L..L.1.),]
lines(1:len, cumsum(validateData$`RFMTC Our Res Prob`), col = "blue")
lines(1:len, cumsum(validateData$`RFMTC REF Res Prob`), col = "green")