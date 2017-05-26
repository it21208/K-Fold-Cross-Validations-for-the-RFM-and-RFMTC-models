source("../../MyRLibs/myRFMLib.R")
# Application and Run Type constants
RUN_TITLE <-
   paste("CDNOW RFMTC K-FOLD CROSS VALIDATION", "DATASET")
m = 6
# Data File constants
TransSampleDSFile <-"CDNOW_sample_trans.txt"
ChurnMasterFile <-"CDNOW_master_churn.txt"
CustDSFile <-"CDNOW_sample_cust.txt"
TrainingDSFile <-"CDNOW_training.txt"
TestDSFile <-"CDNOW_test.txt"
hasCustID = TRUE
custChurnColIndex = 6
# Optimization constants
TRAINING_OPTIMALS <- "TRAINING_OPTIMALS.csv"
# Data Frame Column Name constants
recencyColName = "Recency (months)"
frequencyColName = "Frequency (times)"
monetaryColName = "Monetary ($)"
timesColName = "Time (months)"
scoreColName = eXL1ColName
# Choose one of the RFMTC Parameter Range List constants
RFMTC_PARAM_RANGE_LIST <- RFMTC_PARAM_RANGE_LIST_MED
# Choose one of the Optimization Method Vector constants
OPTIMIZATION_METHOD_VECTOR <- OPTIMIZATION_METHOD_VECTOR_SMALL
# K FOLDS Cross Validation constants
K_FOLDS <- 5
#
### 1. MAIN PROGRAM ###
printf("%s", RUN_TITLE)
printf("-------------------------------")
## PART A
# convert transaction datasets to RFM-RFMTC customer datasets
# convertTransactionToRFMTCreadyDataset(TransSampleDSFile,
#                                       CustDSFile,
#                                       ChurnMasterFile)

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
names(avgResDF) <- c(custIdColName, churnColName, scoreColName)
ourAvgResDF <- avgResDF
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
      ## PART C
      # RFMTC dataset preparation
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
            tRF1ColName,
            eXL1ColName,
            pBColName,
            eXL1_pB2ColName
         )
      )
      if (file == fileList[["TRAINING"]]) {
         # find range constraints for parameter (b)
         resList <- calc_b_RangeConstraints(rfmtcReadyDF)
         rfmtcReadyDF <- resList[[1]]
         b_RangeDetailsList <- resList[[2]]
         # set the b range in RFMTC_PARAM_RANGE_LIST by using
         # the MIN and MAX elements of list b_RangeDetailsList
         RFMTC_PARAM_RANGE_LIST[["b"]][["MIN"]] <-
            b_RangeDetailsList[["MIN"]]
         RFMTC_PARAM_RANGE_LIST[["b"]][["MAX"]] <-
            b_RangeDetailsList[["MAX"]]
         # discover optimal set of initial values (m, Q, g, b, method)
         resultList <-
            discoverOptimalInitialParams(rfmtcReadyDF,
                                         RFMTC_PARAM_RANGE_LIST,
                                         OPTIMIZATION_METHOD_VECTOR)
         # find optimum values for (Q, g, b) that minimize the objective function
         optimumParams <- findOptimumValues(resultList,
                                            rfmtcReadyDF,
                                            b_RangeDetailsList[["MIN"]],
                                            b_RangeDetailsList[["MAX"]])
         # save to external file the Optimal (m, Q, g, b)
         write.csv(
            c(
               resultList[["m"]],
               optimumParams[1],
               optimumParams[2],
               optimumParams[3]
            ),
            TRAINING_OPTIMALS
         )
      }
      # read from external file the Optimal (m, Q, g, b)
      optimumParams <- read.csv(TRAINING_OPTIMALS)
      m <- optimumParams$x[1]
      Q <- optimumParams$x[2]
      g <- optimumParams$x[3]
      b <- optimumParams$x[4]
      # calculation of E[X|L=1] with optimal (Q, g, b)
      ourRfmtcScoredDF <- exl1_OptimalCalc(c(Q, g, b), rfmtcReadyDF)
      # add each customer's score in the average result data frame
      # Lookup Customer RFM-Score from rfmtcScoredDF data frame
      # and fill them into the avgResDF data frame
      for (i in 1:length(ourRfmtcScoredDF[, 1])) {
         rowIndx <-
            which(avgResDF[, custIdColName] == ourRfmtcScoredDF[i, custIdColName])
         avgResDF[rowIndx , scoreColName] <-
            avgResDF[rowIndx , scoreColName] +
            ourRfmtcScoredDF[i, scoreColName]
      }
   }
}

# divide by k and round value
avgResDF[, scoreColName] <- avgResDF[, scoreColName] / K_FOLDS
# write data to file
write.csv(avgResDF, "OUR_RFMTC.csv", row.names = FALSE)
# divide by k and round value
ourAvgResDF[, scoreColName] <- ourAvgResDF[, scoreColName] / K_FOLDS