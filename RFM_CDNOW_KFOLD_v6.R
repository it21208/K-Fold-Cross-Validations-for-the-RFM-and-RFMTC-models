source("../../MyRLibs/myRFMLib.R")
##############  1. GLOBAL CONSTANTS #######################
#
# Application and Run Type constants
RUN_TITLE <-
  paste("CDNOW RFM K-FOLD CROSS VALIDATION", "DATASET")
m = 6
# Data File constants
TransSampleDSFile <- "CDNOW_sample_trans.txt"
ChurnMasterFile <- "CDNOW_master_churn.txt"
CustDSFile <- "CDNOW_sample_cust.txt"
TrainingDSFile <- "CDNOW_training.txt"
TestDSFile <- "CDNOW_test.txt"
hasCustID = TRUE
custChurnColIndex = 6
# Data Frame Column Name constants
recencyColName = "Recency (months)"
frequencyColName = "Frequency (times)"
monetaryColName = "Monetary ($)"
timesColName = "Time (months)"
# RFM Segmentation Analysis & Score Calculation constants
RFM_MATRICES_PROVIDED = FALSE # if TRUE, CUSTSEGMMATRICE_LST is used
CUSTSEGMMATRICE_LST <-
  list(
    RECENCY = matrix(
      data = NA,
      nrow = rfmRecencyClusters,
      ncol = 3,
      byrow = TRUE
    ),
    FREQUENCY = matrix(
      data = NA,
      nrow = rfmFrequencyClusters,
      ncol = 3,
      byrow = TRUE
    ),
    MONETARY = matrix(
      data = NA,
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
convertTransactionToRFMTCreadyDataset(TransSampleDSFile,
                                      CustDSFile,
                                      ChurnMasterFile)

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
avgResDF <-
  calculatePB_RespProb(avgResDF, m, "OUR_RFM")