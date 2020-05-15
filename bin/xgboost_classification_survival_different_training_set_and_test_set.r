setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)
NUMBER_OF_EXECUTIONS <- 100
TRAIN_SET_OVERSAMPLING_SYNTHETIC <- TRUE

globalROSEp <- -1


start_time <- Sys.time()

EXP_ARG_NUM <- 1
args = commandArgs(trailingOnly=TRUE)
if (length(args)<EXP_ARG_NUM) {
  stop("At least one argument must be supplied ", call.=FALSE)
} else {  
  inputDatasetFlag <- args[1]
}

if(inputDatasetFlag == "STUDY_COHORT") { 

    fileName <- "../data/dataFrameForSurvival_study_cohort_rand2109_FIXED.csv" #study cohort
    globalROSEp <- 0.45 # study cohort
    targetName <- "hospital_outcome_1alive_0dead"

} else if(inputDatasetFlag == "PRIMARY_COHORT") {

    fileName <- "../data/journal.pone.0187990.s002_EDITED_survival.csv" #primary cohort
    globalROSEp <- 0.38 # primary cohort
    targetName <- "hospital_outcome_1alive_0dead"
    
} else {
    fileName <- NULL    
}

cat("globalROSEp=", globalROSEp, "\n", sep="")
cat("inputDatasetFlag: ", inputDatasetFlag, "\n", sep="")



externalTestFileName <- "../data/pone.0200187.s002_three_features_extrapolated_only_preop_shock_present_FINAL.csv" #primary cohort
cat("externalTestFileName: ", externalTestFileName, "\n", sep="")

# https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html

cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "clusterSim", "PRROC", "e1071", "rpart",  "dplyr", "pastecs", "xgboost", "ROSE", " lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

library("easypackages")
libraries(list.of.packages)

source("./utils.r")
source("./confusion_matrix_rates.r")


NUM_METRICS <- 9
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1_score", "accuracy", "TP_rate", "TN_rate", "PPV", "NPV", "PR_AUC", "ROC_AUC")
threshold <- 0.5

# file reading
patients_data <- read.csv(fileName, header = TRUE, sep =",")
cat("Read data from file ", fileName, "\n", sep="")

external_test_patients_data <- read.csv(externalTestFileName, header = TRUE, sep =",")
cat("Read data from file ", externalTestFileName, "\n", sep="")
external_test_patients_data <- external_test_patients_data[sample(nrow(external_test_patients_data)),]  # shuffle


# let's put the target label last on the right 
patients_data <- patients_data%>%dplyr::select(-targetName,targetName)

target_index <- dim(patients_data)[2]
original_patients_data <- patients_data

# formula
allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))

execution_number <-  NUMBER_OF_EXECUTIONS
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat(">>> execution number: ", exe_i, "\n", sep="")

    # shuffle the rows
    patients_data <- patients_data[sample(nrow(patients_data)),] 

    # Allocation of the size of the training set
    perce_training_set <- 80
    size_training_set <- round(dim(patients_data)[1]*(perce_training_set/100))

    cat("perce_training_set = ",perce_training_set,"%", sep="")

    # Allocation of the training set and of the test set
    training_set <- patients_data[1:size_training_set, (1:(target_index-1))]
    training_set_including_label <- patients_data[1:size_training_set, (1:(target_index))]
    
    cat("training set BEFORE oversampling:")
    imbalance_retriever(training_set_including_label[,target_index])
    
     if(TRAIN_SET_OVERSAMPLING_SYNTHETIC == TRUE)
         {
            thisP <- globalROSEp
         
            data.rose <- ROSE(allFeaturesFormula, data = training_set_including_label, p=thisP, seed = 1)$data
            training_set <- data.rose[1:size_training_set, (1:(target_index-1))]
            training_set_including_label <- data.rose
            
            cat("training set AFTER oversampling:")
            imbalance_retriever(data.rose[,target_index])
         }         
    
    
    test_set_index_start <- size_training_set+1
    test_set_index_end <- dim(patients_data)[1]
    # test_set  <- patients_data[test_set_index_start:test_set_index_end, (1:(target_index-1))]
    test_set <- external_test_patients_data[, (1:(target_index-1))]
    
    training_labels <- training_set_including_label[1:size_training_set, target_index]   # NEW
    # test_labels <- patients_data[test_set_index_start:test_set_index_end, target_index]   # NEW
    test_labels <- external_test_patients_data[, target_index]

    print("dim(training_set)")
    print(dim(training_set))

    print("dim(test_set)")
    print(dim(test_set))

    this_max_depth <- 2
    this_eta <- 1
    this_nthread <- 2
    this_nrounds <- 2
    this_verbose <- 0
    
    bst_model <- xgboost(data = as.matrix(training_set), label=training_labels, max.depth = this_max_depth, eta = this_eta, nthread = this_nthread, nrounds = this_nrounds, objective = "binary:logistic", verbose = this_verbose)
   
    pred <- predict(bst_model, as.matrix(test_set))
    pred_test_predictions <- as.numeric(pred)

    patients_data_test_PRED_binary <- as.numeric(pred_test_predictions)

    patients_data_test_PRED_binary[patients_data_test_PRED_binary>=threshold]=1
    patients_data_test_PRED_binary[patients_data_test_PRED_binary<threshold]=0
    # mcc_outcome <- mcc(pred_test_set_labels, patients_data_test_PRED_binary)
    # confusion_matrix_rates(pred_test_set_labels, patients_data_test_PRED_binary)

    thisConfMat <- confusion_matrix_rates(test_labels, pred_test_predictions, "@@@ Test set @@@")

    if (exe_i == 1)  confMatDataFrame <-  thisConfMat
    else  confMatDataFrame <- rbind(confMatDataFrame, thisConfMat)
    
 }
 
 cat("\n\n\n=== final results ===\n")
 
 cat("Number of executions = ", execution_number, "\n", sep="")
 # statistics on the dataframe of confusion matrices
 statDescConfMatr <- stat.desc(confMatDataFrame)
 
print(dec_three(statDescConfMatr))
 
meanStdRowResults <- (statDescConfMatr)[c("mean", "std.dev"),]
cat("\n\n")
print(dec_three(meanStdRowResults))
cat("\n\n=== === === ===\n")

computeExecutionTime()

