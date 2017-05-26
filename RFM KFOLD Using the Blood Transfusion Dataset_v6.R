source("../../MyRLibs/myRFMLib.R")
##############  1. GLOBAL CONSTANTS #######################
#
# Application and Run Type constants
RUN_TITLE <-
  paste("BLOOD TRANSFUSION RFM K-FOLD CROSS VALIDATION", "DATASET")
m = 4
# Data File constants
CustDSFile <- "transfusion.data.txt"
TrainingDSFile <- "transfusion.trainingdata.txt"
TestDSFile <- "transfusion.testdata.txt"
hasCustID = FALSE
custChurnColIndex = 5
# Data Frame Column Name constants
recencyColName = "Recency (months)"
frequencyColName = "Frequency (times)"
monetaryColName = "Monetary (cc)"
timesColName = "Time (months)"
# RFM Segmentation Analysis & Score Calculation constants
RFM_MATRICES_PROVIDED = TRUE # if TRUE, CUSTSEGMMATRICE_LST is used
CUSTSEGMMATRICE_LST <-
  list(
    RECENCY = matrix(
      c(1, 0, 5,
        2, 3, 4,
        3, 4, 3,
        4, 11, 2,
        5, 16, 1),
      nrow = rfmRecencyClusters,
      ncol = 3,
      byrow = TRUE
    ),
    FREQUENCY = matrix(
      c(1, 1, 1,
        2, 2, 2,
        3, 3, 3,
        4, 5, 4,
        5, 8, 5),
      nrow = rfmFrequencyClusters,
      ncol = 3,
      byrow = TRUE
    ),
    MONETARY = matrix(
      c(1, 250, 1,
        2, 500, 2,
        3, 750, 3,
        4, 1250, 4,
        5, 2000, 5),
      nrow = rfmMonetaryClusters,
      ncol = 3,
      byrow = TRUE
    )
  )
rfmCoefficients = c(1, 1, 1)  # R, F, M coefficients for RFMScore
# K FOLDS Cross Validation constants
K_FOLDS <- 5
#
##############  2. MAIN PROGRAM #######################
#
printf("%s", RUN_TITLE)
printf("-------------------------------")
## PART A
# convert transaction datasets to RFM-RFMTC customer datasets

# create averare result data frame from customer file
avgResDF <- read.csv(CustDSFile, header = TRUE)
avgResDFLen <- length(avgResDF[, 1])
if (hasCustID) {
  custIDvector <- avgResDF[, 1]
} else {
  custIDvector <- 1:avgResDFLen
}
avgResDF <- as.data.frame(cbind(custIDvector,
                                avgResDF[, custChurnColIndex],
                                rep(0, times = avgResDFLen)))
names(avgResDF) <- c(custIdColName, churnColName, rfmScoreColName)
# K-Fold Cross Validation
for (i_fold in 1:K_FOLDS) {
  ## PART B
  # partition customer dataset to training & test datasets
  fileList <-
    list(
      TRAINING = paste(TrainingDSFile, i_fold, sep = "_"),
      TEST = paste(TestDSFile, i_fold, sep = "_")
    )
  RunFileName <-
    kFoldPartitionCustomerDSToTrainingTestDS(CustDSFile,
                                             hasCustID,
                                             K_FOLDS,
                                             i_fold,
                                             fileList[["TRAINING"]],
                                             fileList[["TEST"]])
  for (file in fileList) {
    ## PART C - TRAINING
    # RFM dataset preparation
    rfmtcReadyDF <- prepareRFMTCdataset(
      file,
      c(
        idColName,
        custIdColName,
        recencyColName,
        frequencyColName,
        monetaryColName,
        timesColName,
        churnColName,
        rScoreColName,
        fScoreColName,
        mScoreColName,
        rfmScoreColName
      )
    )
    if (file == fileList[["TRAINING"]]) {
      # calculate/load RFM Clustering matrices
      calcRFMSegmentationMatrices(rfmtcReadyDF,
                                  RFM_MATRICES_PROVIDED,
                                  CUSTSEGMMATRICE_LST)
    }
    # read the segmentation matrices from external files
    SEGMMATRICE_LST <- loadRFMSegmentationMatrices()
    # calculate R,F,M scores & RFM total score
    rfmtcScoredDF <-
      rfmScoreCalculation(rfmtcReadyDF, SEGMMATRICE_LST, rfmCoefficients)
    # add each customer's score in the average result data frame
    # Lookup Customer RFM-Score from rfmtcScoredDF data frame
    # and fill them into the avgResDF data frame
    for (i in 1:length(rfmtcScoredDF[, 1])) {
      rowIndx <-
        which(avgResDF[, custIdColName] == rfmtcScoredDF[i, custIdColName])
      avgResDF[rowIndx , rfmScoreColName] <-
        avgResDF[rowIndx , rfmScoreColName] +
        rfmtcScoredDF[i, rfmScoreColName]
    }
  }
}

# divide by k and round value
avgResDF[, rfmScoreColName] <-
  round(avgResDF[, rfmScoreColName] / K_FOLDS, digits = 0)
# calculate & RFM response probability & store results
avgResDF <- calculatePB_RespProb(avgResDF, m, "OUR_RFM")