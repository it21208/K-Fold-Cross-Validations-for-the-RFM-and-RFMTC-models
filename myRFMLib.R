library("Rcpp", character.only = TRUE)
library("optimization", character.only = TRUE)
library("optimx", character.only = TRUE)
#
##############  1. GLOBALS & CONSTANTS #######################
#
# Data Frame Column Name constants
idColName <- "ID"
custIdColName <- "CustID"
recencyColName = "Recency"
frequencyColName = "Frequency"
monetaryColName = "Monetary"
timesColName = "Time"
churnColName = "Churn (0/1)"
rScoreColName = "R-Score"
fScoreColName = "F-Score"
mScoreColName = "M-Score"
rfmScoreColName = "RFM-Score"
tRF1ColName = "(T-R)/(F-1)"
pBColName = "P[B]"
rfmRespProbColName = "RFM Resp Prob"
eXL1ColName = "E[X[L]|L=1]"
eXL1_pB2ColName = "(E[X|L=1]-P[B])^2"
# RFM Segmentation constants
rfmRecencyClusters <- 5
rfmFrequencyClusters <- 5
rfmMonetaryClusters <- 5
# RFMTC Parameter Range List constants
RFMTC_PARAM_RANGE_LIST_FULL <-
  list(
    m = 3:6,
    Q = seq(0.01, 0.2, length.out = 4),
    g = seq(0.25, 4, length.out = 4),
    b = list(MIN = 0, MAX = 0, LEN = 4)
  )                         # has to be set dynamically before used
RFMTC_PARAM_RANGE_LIST_MED <-
  list(
    m = 3:4,
    Q = seq(0.01, 0.2, length.out = 3),
    g = seq(0.25, 4, length.out = 3),
    b = list(MIN = 0, MAX = 0, LEN = 3)
  )                         # has to be set dynamically before used
RFMTC_PARAM_RANGE_LIST_MIN <-
  list(
    m = 4:4,
    Q = seq(0.01, 0.2, length.out = 2),
    g = seq(0.25, 4, length.out = 2),
    b = list(MIN = 0, MAX = 0, LEN = 2)
  )                         # has to be set dynamically before used
# Optimization Method Vector constants
optim_sa_OptMethod <- "optim_sa"
optim_nm_OptMethod <- "optim_nm"
optimx_OptMethod <- "optimx"
OPTIMIZATION_METHOD_VECTOR_FULL <-
  c(optim_sa_OptMethod, optim_nm_OptMethod, optimx_OptMethod)
OPTIMIZATION_METHOD_VECTOR_SMALL <-
  c(optim_sa_OptMethod)
# K FOLDS Cross Validation constants
K_FOLDS <- 10

#
##############  2. FUNCTIONS #######################
#
### 2.1 BUILT CLUSTER MATRIX FOR AN ATTRIBUTE ###
# This function will create a Cluster Matrix for the specified Feature column,
# after having sorted the Feature column accordingly (sortDescending)
# The Cluster Matrix will describe the StartValue and Score for each cluster ID
createFeatureClusterMatrix <-
  function (featureColumn, noOfClusters, sortDesc) {
    # sort the featureColumn in sortDesc order, which orders values in a way
    # that the top most have the highest scores
    featureColumn <- sort(featureColumn, decreasing = sortDesc)
    # create a clusterMatrix noOfClusters rows by 3 columns (ClusterID, StartValue, Score).
    # if skipped clusters are encountered (due to a large number of equal values) these
    # rows will be removed and the clusterMatrix will contains less rows than noOfClusters
    clusterMatrix = matrix(,
                           nrow = noOfClusters,
                           ncol = 3,
                           byrow = TRUE)
    # calculate the ideal number of elements per cluster
    len <- length(featureColumn)
    clusterElements <- round(len / noOfClusters)
    # declare and initialize an index to iterate through featureColumn
    dataIdx = 1
    # declare and initialize a flag to stop processing more clusters
    noMoreClustersToProcess = FALSE
    # create a for loop with an index e.g. clusterID in the range [1:noOfClusters]
    for (clusterID in 1:noOfClusters) {
      # check if we must skip an entire cluster!
      if ((noMoreClustersToProcess) ||
          (dataIdx > clusterID * clusterElements &&
           clusterID < noOfClusters)) {
        clusterMatrix[clusterID, 1] <-
          -1 # it signals that this row should be deleted!
        next
      }
      # define the clusterID of cluster
      clusterMatrix[clusterID, 1] <- clusterID
      # define the start value of cluster
      startValue <- featureColumn[dataIdx]
      # update the dataIdx with the position of the ideal-last element of the clusterID
      if (clusterID < noOfClusters)
        dataIdx <- clusterID * clusterElements
      else
        dataIdx = len
      # retrieve the value of the ideal-last element of the clusterID
      endValue <- featureColumn[dataIdx]
      # set the start value for the cluster to endValue or startValue
      if (sortDesc)
        clusterMatrix[clusterID, 2] <- endValue
      else
        clusterMatrix[clusterID, 2] <- startValue
      # define the score value of cluster with clusterID
      clusterMatrix[clusterID, 3] <- noOfClusters - clusterID + 1
      # advance the dataIdx as many places as necessary in order to find the first
      # element that will have a differnt value from the last recorded endValue. This
      # new different value will be the startValue for the following clusterID
      for (duplicatesIndx in dataIdx:len)
        if (featureColumn[duplicatesIndx] != endValue) {
          dataIdx <- duplicatesIndx
          break
        }
      # check if there are no more cluster to process.
      noMoreClustersToProcess = (featureColumn[len] == endValue)
    }
    # remove clusterMatrix rows with negative IDs (they represent skipped clusters)
    clusterMatrix <- clusterMatrix[clusterMatrix[, 1] > 0,]
    # sort clusterMatrix with column 2 in ascending order (bigger values last, so that featureScore()
    #                                                      can start from the last row always)
    clusterMatrix <-
      clusterMatrix[order(clusterMatrix[, 2], clusterMatrix[, 3], decreasing = FALSE),]
    return(clusterMatrix)
  }
#
### 2.2 CALCULATE SCORE FOR AN ATTRIBUTE VALUE ###
#
featureScore <- function(featureValue, featureClusterMatrix)
{
  retvalue <- 0
  for (clusterID in nrow(featureClusterMatrix):1)
    if (featureValue >= featureClusterMatrix[clusterID, 2]) {
      retvalue <- featureClusterMatrix[clusterID, 3]
      break
    }
  return(retvalue)
}
#
### 2.3 CALCULATE MOVING AVERAGE  ###
#
calcMovingAverage <- function(v, m) {
  len = length(v)
  ret_v <- rep(0, times = len)
  for (i in 1:len) {
    # find the position of the start and last element to summarize
    if (i <= m) {
      startPos <- 1
      endPos <- i + m
    } else if (i >= len - m) {
      endPos <- len
      startPos <- i - m
    } else {
      startPos <- i - m
      endPos <- i + m
    }
    # accumulate the elements in range [startPos..endPos]
    sum <- 0
    for (j in startPos:endPos)
      sum <- sum + v[j]
    # find average and store it in new vector
    ret_v[i] <- sum / (endPos - startPos + 1)
  }
  return(ret_v)
}
#
### 2.4  K-FOLD PARTITION OF CUSTOMER DATA SET TO TRAINING & TEST DATA SETS ###
#
kFoldPartitionCustomerDSToTrainingTestDS <-
  function(custMasterFile,
           hasCustID,
           k,
           i_fold,
           TrainingDSFile,
           TestDSFile) {
    # print function display header
    printf("kFoldPartitionCustomerDSToTrainingTestDS()")
    # read custMasterFile
    custMDF <- read.csv(custMasterFile, header = TRUE)
    # add autonumber column to create user-ids
    if (hasCustID) {
      custMDF <- as.data.frame(cbind(1:nrow(custMDF), custMDF))
      names(custMDF)[1] <- idColName
    } else {
      custMDF <-
        as.data.frame(cbind(1:nrow(custMDF), 1:nrow(custMDF), custMDF))
      names(custMDF)[1] <- idColName
      names(custMDF)[2] <- custIdColName
    }
    # split training and test
    ### trainDF <- custMDF[custMDF[, 1] <= 500,]
    ### testDF <- custMDF[custMDF[, 1] > 500,]
    retList <- func_K_Fold(custMDF, k, i_fold)
    trainDF <- retList[["TRAINING"]]
    testDF <- retList[["TEST"]]
    # store data frames to files
    write.csv(trainDF, TrainingDSFile, row.names = FALSE)
    write.csv(testDF, TestDSFile, row.names = FALSE)
    #
    return()
  }

func_K_Fold <- function(custMDF, k, i_fold) {
  len <- length(custMDF[, 1])
  sampleSize <- trunc(len / k)
  startIndx <- 0
  endIndx <- 0
  if (i_fold == k) {
    startIndx <- (k - 1) * sampleSize + 1
    endIndx <- len
  } else {
    startIndx <- (i_fold - 1) * sampleSize + 1
    endIndx <- i_fold * sampleSize
  }
  testMDF <-
    custMDF[(custMDF[, idColName] >= startIndx) &
              (custMDF[, idColName] <= endIndx), ]
  # trainingMDF <- custMDF[custMDF[,idColName] != testMDF[,idColName],]
  trainingMDF <-
    custMDF[!(custMDF[, idColName] %in% testMDF[, idColName]), ]
  retList <- list(TRAINING = trainingMDF, TEST = testMDF)
  return(retList)
}
#
### 2.5  PARTITION CUSTOMER DATA SET TO TRAINING & TEST DATA SETS ###
#
partitionCustomerDSToTrainingTestDS_BLOODTRANSFUSION <-
  function(custMasterFile,
           TrainingDSFile,
           TestDSFile,
           RunType) {
    # RUN_TYPE is {"TRAINING" | "TEST"}
    # print function display header
    printf("partitionCustomerDSToTrainingTestDS_BLOODTRANSFUSION()")
    # read custMasterFile
    custMDF <- read.csv(custMasterFile, header = TRUE)
    # add autonumber column to create user-ids
    custMDF <-
      as.data.frame(cbind(1:nrow(custMDF), 1:nrow(custMDF), custMDF))
    names(custMDF)[1] <- idColName
    names(custMDF)[2] <- custIdColName
    # split training and test
    trainDF <- custMDF[custMDF[, 1] <= 500, ]
    testDF <- custMDF[custMDF[, 1] > 500, ]
    # store data frames to files
    write.csv(trainDF, TrainingDSFile, row.names = FALSE)
    write.csv(testDF, TestDSFile, row.names = FALSE)
    # set RunFile
    if (RunType == "TRAINING")
      RunFile <- TrainingDSFile
    else
      RunFile <- TestDSFile
    return(RunFile)
  }
#
### 2.6 PRINTF ###
#
printf <- function(...) {
  invisible(print(sprintf(...)))
}
#
### 2.7 RFM DATASET PREPARATION ###
# RunFile schema : (ID, CustID, Recency, Frequency, Monetary, Time, Churn)
prepareRFMTCdataset <- function(RunFile, dfCols) {
  # print function display header
  printf("prepareRFMTCdataset()")
  # read RunFile with header
  rfmtcDF <- read.csv(RunFile, header = TRUE)
  len <- length(rfmtcDF[, 1])
  cols <- length(colnames(rfmtcDF))
  # add autonumber column to create user-ids and
  # create the rfmtcDF from the rfmtcDF and 4 blank score columns
  rfmtcDF <- as.data.frame(cbind(
    1:nrow(rfmtcDF),
    rfmtcDF[, 2:cols],
    rep(0, times = len),
    rep(0, times = len),
    rep(0, times = len),
    rep(0, times = len)
  ))
  # define appropriate column labels
  names(rfmtcDF) <- dfCols
  return(rfmtcDF)
}
#
### 2.8 CALCULATE & STORE RFM SEGMENTATION MATRICES ###
#
calcRFMSegmentationMatrices <-
  function(rfmtcReadyDF,
           RFM_MATRICES_PROVIDED,
           CUSTSEGMMATRICE_LST,
           rfmRecencyClusters = 5,
           rfmFrequencyClusters = 5,
           rfmMonetaryClusters = 5,
           recencyFileName = "RECENCY_SEGMENTATION_MATRIX.csv",
           frequencyFileName = "FREQUENCY_SEGMENTATION_MATRIX.csv",
           monetaryFileName = "MONETARY_SEGMENTATION_MATRIX.csv") {
    printf("calcRFMSegmentationMatrices()")
    # Logic to create segmentation should be added here
    # If segmentation matrices are provided by customer logic
    # then load them from CUSTSEGMMATRICE_LST
    if (!RFM_MATRICES_PROVIDED) {
      rfmRecencyClusterMatrix <-
        createFeatureClusterMatrix(rfmtcReadyDF[, recencyColName], rfmRecencyClusters, FALSE)
      rfmFrequencyClusterMatrix <-
        createFeatureClusterMatrix(rfmtcReadyDF[, frequencyColName], rfmFrequencyClusters, TRUE)
      rfmMonetaryClusterMatrix <-
        createFeatureClusterMatrix(rfmtcReadyDF[, monetaryColName], rfmMonetaryClusters, TRUE)
    } else {
      rfmRecencyClusterMatrix <- CUSTSEGMMATRICE_LST[["RECENCY"]]
      rfmFrequencyClusterMatrix <-
        CUSTSEGMMATRICE_LST[["FREQUENCY"]]
      rfmMonetaryClusterMatrix <- CUSTSEGMMATRICE_LST[["MONETARY"]]
    }
    # save the segmentation matrices to external files
    write.csv(rfmRecencyClusterMatrix, recencyFileName)
    write.csv(rfmFrequencyClusterMatrix, frequencyFileName)
    write.csv(rfmMonetaryClusterMatrix, monetaryFileName)
  }
#
### 2.9 LOAD RFM SEGMENTATION MATRICES ###
#
loadRFMSegmentationMatrices <-
  function(recencyFileName = "RECENCY_SEGMENTATION_MATRIX.csv",
           frequencyFileName = "FREQUENCY_SEGMENTATION_MATRIX.csv",
           monetaryFileName = "MONETARY_SEGMENTATION_MATRIX.csv") {
    printf("loadRFMSegmentationMatrices()")
    # load the segmentation matrices from the external files
    rsm <- read.csv(recencyFileName)[2:4]
    fsm <- read.csv(frequencyFileName)[2:4]
    msm <- read.csv(monetaryFileName)[2:4]
    # define RFM Clustering matrice column and row names
    rownames(rsm) <-
      paste("Cluster-", 1:length(rsm[, 1]), sep = "")
    colnames(rsm) <-
      c("Cluster No", "Start Value", "Score")
    rownames(fsm) <-
      paste("Cluster-", 1:length(fsm[, 1]), sep = "")
    colnames(fsm) <-
      c("Cluster No", "Start Value", "Score")
    rownames(msm) <-
      paste("Cluster-", 1:length(msm[, 1]), sep = "")
    colnames(msm) <-
      c("Cluster No", "Start Value", "Score")
    # store segmentation matrices to a list
    SEGMMATRICE_LST <-
      list(RECENCY = rsm,
           FREQUENCY = fsm,
           MONETARY = msm)
    return(SEGMMATRICE_LST)
  }
#
### 2.10 CALCULATE R,F,M SCORES & RFM TOTAL SCORE ###
#
rfmScoreCalculation <-
  function(rfmtcReadyDF,
           SEGMMATRICE_LST,
           rfmCoefficients) {
    printf("rfmScoreCalculation()")
    # calculate R, F, M feature scores
    rfmtcReadyDF[, rScoreColName] <-
      sapply(rfmtcReadyDF[, recencyColName], function(x)
        featureScore(x, SEGMMATRICE_LST[["RECENCY"]]))
    rfmtcReadyDF[, fScoreColName] <-
      sapply(rfmtcReadyDF[, frequencyColName], function(x)
        featureScore(x, SEGMMATRICE_LST[["FREQUENCY"]]))
    rfmtcReadyDF[, mScoreColName] <-
      sapply(rfmtcReadyDF[, monetaryColName], function(x)
        featureScore(x, SEGMMATRICE_LST[["MONETARY"]]))
    # calculate RFM Total score
    rfmtcReadyDF[, rfmScoreColName] <-
      rfmCoefficients[1] * rfmtcReadyDF[, rScoreColName] +
      rfmCoefficients[2] * rfmtcReadyDF[, fScoreColName] +
      rfmCoefficients[3] * rfmtcReadyDF[, mScoreColName]
    # sort data on all feature scores in desc order
    rfmtcReadyDF <-
      rfmtcReadyDF[order(-rfmtcReadyDF[, rScoreColName],-rfmtcReadyDF[, fScoreColName],-rfmtcReadyDF[, mScoreColName]),]
    return(rfmtcReadyDF)
  }
#
### 2.11 CALCULATE P[B] & RFM RESPONSE PROBABILITY & STORE RESULTS ###
#
calculatePB_RespProb <- function(rfmtcScoredDF, m, resFileName) {
  # m comes from RFMTC training set calculations
  printf("calculatePB_RespProb")
  # sort data frame with the RFM-Score in descending order
  # RFM-Score desc & R desc & F asc & M asc
  # rfmtcScoredDF <-
  #   rfmtcScoredDF[order(-rfmtcScoredDF[, rfmScoreColName],-rfmtcScoredDF[, recencyColName],
  #                       rfmtcScoredDF[, frequencyColName],
  #                       rfmtcScoredDF[, monetaryColName]), ]
  rfmtcScoredDF <-
    rfmtcScoredDF[order(-rfmtcScoredDF[, rfmScoreColName]), ]
  rfmtcScoredDF <-
    as.data.frame(cbind(rfmtcScoredDF, calcMovingAverage(rfmtcScoredDF[, churnColName], m)))
  colnames(rfmtcScoredDF)[length(colnames(rfmtcScoredDF))] <-
    pBColName
  # Calculate RFM Segment Response Probability
  rfmAggrData <- # select RFM-Score, avg(P[B])
    # from rfmtcScoredDF
    # group by RFM-Score
    aggregate(rfmtcScoredDF[, pBColName],
              list(rfmtcScoredDF[, rfmScoreColName]),
              "mean")
  names(rfmAggrData) <- c("Segment", "Response Probability")
  # Lookup RFM Customer Response Probabilities from rfmAggrData data frame
  # and fill them into the Validation data frame
  len <- length(rfmtcScoredDF[, 1])
  rfmtcScoredDF <-
    as.data.frame(cbind(rfmtcScoredDF, rep(0, times = len)))
  colnames(rfmtcScoredDF)[length(colnames(rfmtcScoredDF))] <-
    rfmRespProbColName
  for (i in 1:len) {
    rfmAggrData_Indx <- which(rfmAggrData[, "Segment"] == rfmtcScoredDF[i, rfmScoreColName])
    rfmtcScoredDF[i, rfmRespProbColName] <- rfmAggrData[rfmAggrData_Indx, "Response Probability"]
  }
  # write data to file
  write.csv(rfmtcScoredDF,
            paste(resFileName, ".csv", sep = ""),
            row.names = FALSE)
  return(rfmtcScoredDF)
}
#
### 2.12 CONVERT TRANSACTION DATASETS TO RFM-RFMTC CUSTOMER DATASETS ###
#
convertTransactionToRFMTCreadyDataset <-
  function(transSampleFile,
           custSampleFile,
           churnMasterFile) {
    # print function display header
    printf("convertTransactionToRFMTCreadyDataset()")
    # read transSampleFile
    transSDF <- read.table(transSampleFile, header = FALSE)
    # remove ID column (col-2) and CDs column (col-4) from sample data frame
    transSDF <- as.data.frame(cbind(transSDF[, c(1, 3, 5)]))
    # set the column names for data frame
    names(transSDF) <- c(custIdColName, "Date", "Amount")
    # read churn master file
    churnDF <- read.csv(churnMasterFile, header = TRUE)
    # set the column names for churn data frame
    names(churnDF) <- c(custIdColName, churnColName)
    # tranform transaction sample data frame to customer sample data frame
    custSDF <- calcAppendRFMTC(transSDF, churnDF)
    # write custSampleFile
    write.csv(custSDF, custSampleFile, row.names = FALSE)
    return(NULL)
  }
#
### 2.13 CALCULATE & APPEND R,F,M,T,C COLUMNS TO CUSTOMER DATASET ###
#  names(transDF) is c("CustID", "Date", "Amount")
#  names(churnDF) is c("CustID", "Churn")
calcAppendRFMTC <- function(transDF, churnDF) {
  # set the max(transaction date)+1
  endDate <- as.Date("19980701", "%Y%m%d")
  # a) convert Date col from numeric to date type
  transDF[, "Date"] <-
    as.Date(as.character(transDF[, "Date"]), "%Y%m%d")
  # b) find the max transaction date per CustID
  t_maxDate <-
    aggregate(transDF[, "Date"], list(transDF[, custIdColName]), max)
  names(t_maxDate) <- c(custIdColName, "MaxDate")
  t_maxDate <- t_maxDate[order(t_maxDate[, custIdColName]),]
  # c) find the min transaction date per CustID
  t_minDate <-
    aggregate(transDF[, "Date"], list(transDF[, custIdColName]), min)
  names(t_minDate) <- c(custIdColName, "MinDate")
  t_minDate <- t_minDate[order(t_minDate[, custIdColName]),]
  # d) find the average Amount per CustID
  t_AvgAmount <-
    aggregate(transDF[, "Amount"], list(transDF[, custIdColName]), mean)
  names(t_AvgAmount) <- c(custIdColName, "AvgAmount")
  t_AvgAmount <- t_AvgAmount[order(t_AvgAmount[, custIdColName]),]
  # e) find the Frequency
  t_Freq <-
    aggregate(transDF[, custIdColName], by = list(transDF[, custIdColName]), length)
  names(t_Freq) <- c(custIdColName, "Frequency")
  t_Freq <- t_Freq[order(t_Freq[, custIdColName]),]
  # f) find the Churn
  churnDF <- churnDF[churnDF[, custIdColName] %in% transDF[, custIdColName],]
  churnDF <- churnDF[order(churnDF[, custIdColName]),]
  # sort both transDF and churnDF with ID in ascending order
  transDF <- transDF[order(transDF[, custIdColName]),]
  # g) merge columns and convert Recency and Time to months!
  custDF <-
    as.data.frame(cbind(
      churnDF[, custIdColName],
      (endDate - t_maxDate[, "MaxDate"]),
      t_Freq[, "Frequency"],
      t_AvgAmount[, "AvgAmount"],
      (endDate - t_minDate[, "MinDate"]),
      churnDF[, churnColName]
    ))
  names(custDF) <-
    c(
      custIdColName,
      recencyColName,
      frequencyColName,
      monetaryColName,
      timesColName,
      churnColName
    )
  custDF[, recencyColName] <- custDF[, recencyColName] / 30
  custDF[, timesColName] <- custDF[, timesColName] / 30
  return(custDF)
}
#
### 2.14  PARTITION CUSTOMER DATA SET TO TRAINING & TEST DATA SETS ###
#
partitionCustomerDSToTrainingTestDS_CDNOW <-
  function(CustSampleDSFile,
           TrainingDSFile,
           TestDSFile,
           RunType) {
    # RUN_TYPE is {"TRAINING" | "TEST"}
    # print function display header
    printf("partitionCustomerDSToTrainingTestDS_CDNOW()")
    # read custMasterFile
    custSDF <- read.csv(CustSampleDSFile, header = TRUE)
    # add autonumber column to create user-ids
    len <- nrow(custSDF)
    custSDF <- as.data.frame(cbind(1:len, custSDF))
    names(custSDF)[1] <- idColName
    # split training and test
    splitNo <- round(0.75 * len)
    trainDF <- custSDF[custSDF[, idColName] <= splitNo , ]
    testDF <- custSDF[custSDF[, idColName] > splitNo, ]
    # store data frames to files
    write.csv(trainDF, TrainingDSFile, row.names = FALSE)
    write.csv(testDF, TestDSFile, row.names = FALSE)
    # set RunFile
    if (RunType == "TRAINING")
      RunFile <- TrainingDSFile
    else
      RunFile <- TestDSFile
    return(RunFile)
  }
#
### 2.15 CALCULATE (T-R)/(F-1) column ###
# ... with calculation =IF(F<>1,(T-R)/(F-1),0)
func_T_R_F_1 = function(r, f, t) {
  if (f != 1)
    return((t - r) / (f - 1))
  else
    return(0)
}
#
### 2.16 CALCULATE E[X[L]|L=1] ###
# ... formula 22 of the RFMTC paper
make.func_E_X_L_1 <- function(Q, g, b) {
  function(r, f, t) {
    return((g * f / (t - r + b)) * (1 - Q) ^ (r + 1))
  }
}
#
### 2.17 CREATE OBJECTIVE FUNCTION TO MINIMIZE ###
# calculate sum of squares of residual errors
make.OBJECTIVE <- function(m, rfmtcReadyDF) {
  function(x) {
    Q <- x[1]
    g <- x[2]
    b <- x[3]
    # calculate E[X[L]|L=1]
    exl1 <- make.func_E_X_L_1(Q, g, b)
    rfmtcReadyDF[, eXL1ColName] <-
      mapply(function(r, f, t)
        exl1(r, f, t),
        rfmtcReadyDF[, recencyColName],
        rfmtcReadyDF[, frequencyColName],
        rfmtcReadyDF[, timesColName])
    # sort data on E[X[L]|L=1] in desc order
    rfmtcReadyDF <-
      rfmtcReadyDF[order(-rfmtcReadyDF[, eXL1ColName]),]
    # calculate P[B]
    rfmtcReadyDF[, pBColName] <-
      calcMovingAverage(rfmtcReadyDF[, churnColName], m)
    # calculater squares of residual errors
    rfmtcReadyDF[, eXL1_pB2ColName] <-
      (rfmtcReadyDF[, eXL1ColName] - rfmtcReadyDF[, pBColName]) ^ 2
    return(sum(rfmtcReadyDF[, eXL1_pB2ColName]))
  }
}
#
### 2.18 FIND RANGE CONSTRAINTS FOR PARAMETER (b) ###
#
calc_b_RangeConstraints <- function(rfmtcReadyDF) {
  # print function display header
  printf("calc_b_RangeConstraints()")
  rfmtcReadyDF[, tRF1ColName] <-
    mapply(function(r, f, t)
      func_T_R_F_1(r, f, t),
      rfmtcReadyDF[, recencyColName],
      rfmtcReadyDF[, frequencyColName],
      rfmtcReadyDF[, timesColName])
  # calculate the average for non-zero values
  sum <- sum(rfmtcReadyDF[, tRF1ColName])
  cnt <- sum(rfmtcReadyDF[, tRF1ColName] > 0)
  trf1_MEAN <- sum / cnt
  b_RangeDetailsList <-
    list(MEAN = trf1_MEAN,
         MIN = 0.25 * trf1_MEAN,
         MAX = 4 * trf1_MEAN)
  resList <- list(rfmtcReadyDF, b_RangeDetailsList)
  return(resList)
}
#
### 2.19 DISCOVER OPTIMAL SET OF INITIAL VALUES (m, Q, g, b) ###
###     TO USE FOR THE OPTIMIZATION ###
#
discoverOptimalInitialParams <- function(rfmtcReadyDF,
                                         RFMTC_PARAM_RANGE_LIST,
                                         OPTIMIZATION_METHOD_VECTOR) {
  # print function display header
  printf("discoverOptimalInitialParams()")
  # get parameter ranges from RFMTC parameter range list
  m_range <- RFMTC_PARAM_RANGE_LIST[["m"]]
  Q_range <- RFMTC_PARAM_RANGE_LIST[["Q"]]
  g_range <- RFMTC_PARAM_RANGE_LIST[["g"]]
  b_MIN <- RFMTC_PARAM_RANGE_LIST[["b"]][["MIN"]]
  b_MAX <- RFMTC_PARAM_RANGE_LIST[["b"]][["MAX"]]
  b_range <-
    seq(b_MIN, b_MAX, length.out = RFMTC_PARAM_RANGE_LIST[["b"]][["LEN"]])
  # calculate the number of iterations from all ranges
  attempts = length(m_range) * length(Q_range) * length(g_range) * length(b_range)
  # create the result data frame with appropriate dimensions
  resultData <-
    as.data.frame(cbind(
      c(1:attempts),
      rep(0, times = attempts),
      rep(0, times = attempts),
      rep(0, times = attempts),
      rep(0, times = attempts),
      rep(0, times = attempts),
      rep(0, times = attempts),
      rep(0, times = attempts)
    ))
  # provice appropriate column names to result data frame
  names(resultData) <-
    c("A/A",
      "m",
      "Q",
      "g",
      "b",
      optim_sa_OptMethod,
      optim_nm_OptMethod,
      optimx_OptMethod)
  #
  resultIdx <- 1
  for (m in m_range) {
    objfunc <- make.OBJECTIVE(m, rfmtcReadyDF)
    for (Q in Q_range) {
      for (g in g_range) {
        for (b in b_range) {
          resultData[resultIdx, "m"] <- m
          resultData[resultIdx, "Q"] <- Q
          resultData[resultIdx, "g"] <- g
          resultData[resultIdx, "b"] <- b
          printf("A/A = %d, m = %f, Q = %0.2f, g = %0.3f, b = %0.4f\n",
                 resultIdx,
                 m,
                 Q,
                 g,
                 b)
          if (optim_sa_OptMethod %in% OPTIMIZATION_METHOD_VECTOR) {
            # Optimization optim_sa
            resultData[resultIdx, optim_sa_OptMethod] <- optim_sa(
              fun = objfunc,
              maximization = FALSE,
              start = c(Q, g, b),
              lower = c(0.01, 0.25, b_MIN),
              upper = c(0.2, 4, b_MAX),
              trace = FALSE,
              # TRUE
              control = list(
                t0 = 100,
                nlimit = 100,
                t_min = 0.1,
                dyn_rf = FALSE,
                rf = 1,
                r = 0.7
              )
            )[2]
          }
          if (optim_nm_OptMethod %in% OPTIMIZATION_METHOD_VECTOR) {
            # Optimization optim_nm
            resultData[resultIdx, optim_nm_OptMethod] <- optim_nm(
              fun = objfunc,
              k = 3,
              start = c(Q, g, b),
              maximum = FALSE,
              trace = FALSE,
              # TRUE
              alpha = 1,
              beta = 2,
              gamma = 1 / 2,
              delta = 1 / 2,
              tol = 0.00001,
              exit = 200,
              edge = 1
            )[2]
          }
          if (optimx_OptMethod %in% OPTIMIZATION_METHOD_VECTOR) {
            # Optimization optimx
            resultData[resultIdx, optimx_OptMethod] <- optimx(
              par = c(Q, g, b),
              fn = objfunc,
              gr = NULL,
              hess = NULL,
              lower = c(0.01, 0.25, b_MIN),
              upper = c(0.2, 4, b_MAX),
              method = c("Nelder-Mead", "L-BFGS-B"),
              itnmax = NULL,
              hessian = FALSE
            )[1, 4]
          }
          resultIdx <- resultIdx + 1
        }
      }
    }
  }
  # find method with minimal value
  minVal <- -1
  minMethodName <- ""
  for (methodName in OPTIMIZATION_METHOD_VECTOR) {
    if (minVal == -1) {
      minVal <- min(resultData[, methodName])
      minMethodName <- methodName
    }
    else
      if (min(resultData[, methodName]) < minVal) {
        minVal <- min(resultData[, methodName])
        minMethodName <- methodName
      }
  }
  # sort resultData in ascending order in column <methodName>
  resultData <- resultData[order(resultData[, minMethodName]), ]
  # create result list <m, Q, g, b, "methodname">
  resList <-
    list(
      m = resultData$m[1],
      Q = resultData$Q[1],
      g = resultData$g[1],
      b = resultData$b[1],
      method = minMethodName
    )
  return(resList)
}
#
### 2.20 FIND OPTIMUM VALUES FOR (Q, g, b) THAT MINIMIZE ###
###      THE OBJECTIVE FUNCTION ###
#
findOptimumValues <- function(resList, rfmtcReadyDF, b_MIN, b_MAX) {
  # print function display header
  printf("findOptimumValues()")
  # get parameter values from resList
  m <- resList[["m"]]
  Q <- resList[["Q"]]
  g <- resList[["g"]]
  b <- resList[["b"]]
  minMethodName <- resList[["method"]]
  # construct objective function
  objfunc <- make.OBJECTIVE(m, rfmtcReadyDF)
  # choose optimization method and run it
  if (minMethodName == optim_sa_OptMethod) {
    optim_sa_res <- optim_sa(
      fun = objfunc,
      maximization = FALSE,
      start = c(Q, g, b),
      lower = c(0.01, 0.25, b_MIN),
      upper = c(0.2, 4, b_MAX),
      trace = FALSE,
      control = list(
        t0 = 100,
        nlimit = 100,
        t_min = 0.1,
        dyn_rf = FALSE,
        rf = 1,
        r = 0.7
      )
    )
    par <- optim_sa_res$par
    vector_Q_g_b = c(par[1], par[2], par[3])
  } else if (minMethodName == optim_nm_OptMethod) {
    optim_nm_res <- optim_nm(
      fun = objfunc,
      k = 3,
      start = c(Q, g, b),
      maximum = FALSE,
      trace = FALSE,
      #TRUE
      alpha = 1,
      beta = 2,
      gamma = 1 / 2,
      delta = 1 / 2,
      tol = 0.00001,
      exit = 200,
      edge = 1
    )
    par <- optim_nm_res$par
    vector_Q_g_b = c(par[1], par[2], par[3])
  } else if (minMethodName == optimx_OptMethod) {
    optimx_res <- optimx(
      par = c(Q, g, b),
      fn = objfunc,
      gr = NULL,
      hess = NULL,
      lower = c(0.01, 0.25, b_MIN),
      upper = c(0.2, 4, b_MAX),
      method = c("Nelder-Mead", "L-BFGS-B"),
      itnmax = NULL,
      hessian = FALSE
    )
    vector_Q_g_b = c(optimx_res$p1[1], optimx_res$p2[1], optimx_res$p3[1])
  }
  return(vector_Q_g_b)
}

#
### 2.21 CALCULATION OF E[X|L=1] WITH OPTIMAL (Q, g, b) ###
#
exl1_OptimalCalc <- function(x, rfmtcReadyDF) {
  # print function display header
  printf("exl1_OptimalCalc()")
  Q <- x[1]
  g <- x[2]
  b <- x[3]
  # create func_E_X_L_1() with optimal (Q, g, b)
  exl1 <- make.func_E_X_L_1(Q, g, b)
  # calculate column E[X[L]|L=1]
  rfmtcReadyDF[, eXL1ColName] <-
    mapply(function(r, f, t)
      exl1(r, f, t),
      rfmtcReadyDF[, recencyColName],
      rfmtcReadyDF[, frequencyColName],
      rfmtcReadyDF[, timesColName])
  # sort dataframe on E[X[L]|L=1] in desc order
  rfmtcReadyDF <-
    rfmtcReadyDF[order(-rfmtcReadyDF[, eXL1ColName]),]
  # calculate P[B]
  rfmtcReadyDF[, pBColName] <-
    calcMovingAverage(rfmtcReadyDF[, churnColName], m)
  # calculate squares of residual errors
  rfmtcReadyDF[, eXL1_pB2ColName] <-
    (rfmtcReadyDF[, eXL1ColName] - rfmtcReadyDF[, pBColName]) ^ 2
  return(rfmtcReadyDF)
}

# func_P_B_i <- function(churn_vector,m) {
#   new_churnVector <- 0
#   for (i in 1:length(churn_vector)){
#     if (i <= m) {
#       sum <- 0
#       for (j in i:(i+m)){sum = sum + churn_vector[j]}
#       k = i
#       while (k>1){
#         sum = sum + churn_vector[k-1]
#         k=k-1}
#       new_churnVector[i] = sum/(m+i)
#     }else if (i>=(length(churn_vector)-m)){
#       sum <- 0
#       for (j in i:(i-m)){sum = sum + churn_vector[j]}
#       k = i
#       while(k<length(churn_vector)){
#         sum = sum + churn_vector[k+1]
#         k=k+1}
#       new_churnVector[i]=sum/(length(churn_vector)-i+m+1)
#     }else{
#       sum <- 0
#       for (j in i:(i+m)) { sum = sum + churn_vector[j]}
#       for (k in (i-1):(i-m)) { sum = sum + churn_vector[k]}
#       new_churnVector[i]=sum/(2*m+1)}}
#   return(new_churnVector)}