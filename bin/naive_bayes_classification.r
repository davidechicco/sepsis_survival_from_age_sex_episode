setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)
NUMBER_OF_EXECUTIONS <- 100
TRAIN_SET_OVERSAMPLING_SYNTHETIC <- TRUE
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
} else if(inputDatasetFlag == "PRIMARY_COHORT") {
    fileName <- "../data/journal.pone.0187990.s002_EDITED_survival.csv" #primary cohort
} else {
    fileName <- NULL    
}

cat("inputDatasetFlag: ", inputDatasetFlag, "\n", sep="")
targetName <- "hospital_outcome_1alive_0dead"

# fileName <- "../data/dataFrameForSurvival_study_cohort_rand2109.csv" #study cohort

cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "PRROC", "e1071", "randomForest","class", "gmodels", "formula.tools", "dplyr", "pastecs", "ROSE", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)


source("/home/davidechicco/my_projects/General-biomedical-informatics-binary-outcome-analysis/bin/confusion_matrix_rates.r")
source("/home/davidechicco/my_projects/General-biomedical-informatics-binary-outcome-analysis/bin/utils.r")

NUM_METRICS <- 9
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1_score", "accuracy", "TP_rate", "TN_rate", "PPV", "NPV", "PR_AUC", "ROC_AUC")

threshold <- 0.5

patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

patients_data[,targetName] <- as.factor(patients_data[,targetName])

# let's put the target label last on the right 
patients_data <- patients_data%>%select(-targetName,targetName)
patients_data_original <- patients_data

# cycle of executions

execution_number <- NUMBER_OF_EXECUTIONS
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("\n\n\n>>> execution number: ", exe_i, "\n", sep="")

    patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows

    target_index <- dim(patients_data)[2]

    training_set_perce = 80
    cat("training_set_perce = ", training_set_perce, "%\n", sep="")

    # the training set is the first 60% of the whole dataset
    training_set_first_index <- 1 # NEW
    training_set_last_index <- round(dim(patients_data)[1]*training_set_perce/100) # NEW

    # the test set is the last 40% of the whole dataset
    test_set_first_index <- training_set_last_index+1 # NEW
    test_set_last_index <- dim(patients_data)[1] # NEW

#     cat("[Creating the subsets for the values]\n")
#     patients_data_train <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index)] # NEW
#     patients_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index)] # NEW
# 
#     cat("[Creating the subsets for the labels \"1\"-\"0\"]\n")
#     patients_data_train_labels <- patients_data[training_set_first_index:training_set_last_index, target_index] # NEW
#     patients_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW
    
        patients_data_train <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index)] # NEW
        patients_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index)] # NEW
    
       allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))
    
        cat("training set BEFORE oversampling:")
        imbalance_retriever(patients_data_train[,target_index])
         
         if(TRAIN_SET_OVERSAMPLING_SYNTHETIC == TRUE)
         {
            thisP <- 0.5
         
            data.rose <- ROSE(allFeaturesFormula, data = patients_data_train, p=thisP, seed = 1)$data
            patients_data_train <- data.rose
            
            cat("training set AFTER oversampling:")
            imbalance_retriever(patients_data_train[,target_index])
         }
        
        cat("[training set dimensions: ", dim(patients_data_train)[1], " patients]\n")

        cat("[test set dimensions: ", dim(patients_data_test)[1], " patients]\n")

        cat("[Creating the training set and test set for the labels \"1\"-\"0\"]\n")
        patients_data_train_labels <- patients_data_train[, target_index] # NEW
        patients_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW

        
    
    naive_bayes_model <-  naiveBayes(allFeaturesFormula, data=patients_data_train)
    
    patients_data_test_PRED <- predict((naive_bayes_model), patients_data_test)
    # patients_data_test_PRED_binary <- as.numeric(patients_data_test_PRED)-1
    patients_data_test_PRED_binary <- abs(as.numeric(patients_data_test_PRED)-2) # check what is 0 and what is 1

    patients_data_test_PRED_binary[patients_data_test_PRED_binary>=threshold]=1
    patients_data_test_PRED_binary[patients_data_test_PRED_binary<threshold]=0

    thisConfMat <- confusion_matrix_rates(patients_data_test_labels, patients_data_test_PRED_binary, "@@@ Test set @@@")
   
    if (exe_i == 1)  confMatDataFrame <-  thisConfMat
    else  confMatDataFrame <- rbind(confMatDataFrame, thisConfMat)
   
 }
 
 cat("\n\n\n=== final results ===\n")
 
 cat("Number of executions = ", execution_number, "\n", sep="")
 # statistics on the dataframe of confusion matrices
 statDescConfMatr <- stat.desc(confMatDataFrame)
meanRowResults <- (statDescConfMatr)[c("mean"),]
cat("\n\n")
print(dec_three(meanRowResults))
cat("\n\n=== === === ===\n")


end_time <- Sys.time()
total_time <- end_time - start_time
cat("total execution time: ", seconds_to_period(total_time), "\n", sep="")


