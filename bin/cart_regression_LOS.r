setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)
NUMBER_OF_EXECUTIONS <- 100

EXP_ARG_NUM <- 1
args = commandArgs(trailingOnly=TRUE)
if (length(args)<EXP_ARG_NUM) {
  stop("At least one argument must be supplied ", call.=FALSE)
} else {  
  inputDatasetFlag <- args[1]
}

if(inputDatasetFlag == "STUDY_COHORT") { 
    fileName <- "../data/dataFrameForLOS_study_cohort_rand2109.csv" #study cohort
} else if(inputDatasetFlag == "PRIMARY_COHORT") {
    fileName <- "../data/journal.pone.0187990.s002_EDITED_length_of_stay.csv" #primary cohort
} else {
    fileName <- NULL    
}


targetName <- "length_of_stay_days"


cat("inputDatasetFlag: ", inputDatasetFlag, "\n", sep="")
cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "clusterSim", "PRROC", "e1071", "rpart",  "dplyr", "pastecs",  "Metrics", "MLmetrics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")

threshold <- 0.5

# file reading
patients_data <- read.csv(fileName, header = TRUE, sep =",",  stringsAsFactors=FALSE);
cat("Read data from file ", fileName, "\n", sep="")

NUM_METRICS <- 5
resultDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(resultDataFrame) <- c("RMSE", "MAE", "MSE", "SMAPE", "R^2")

# let's put the target label last on the right 
patients_data <- patients_data%>%select(-targetName,targetName)

target_index <- dim(patients_data)[2]
original_patients_data <- patients_data

execution_number <- NUMBER_OF_EXECUTIONS
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat(">>> execution number: ", exe_i, "\n", sep="")

    # shuffle the rows
    patients_data <- patients_data[sample(nrow(patients_data)),] 

    # Allocation of the size of the training set
    perce_training_set <- 80
    size_training_set <- round(dim(patients_data)[1]*(perce_training_set/100))

    cat("perce_training_set = ",perce_training_set,"%\n", sep="")

    # Allocation of the training set and of the test set
    training_set <- (patients_data[1:size_training_set,])
    test_set_index_start <- size_training_set+1
    test_set_index_end <- dim(patients_data)[1]
    test_set  <- patients_data[test_set_index_start:test_set_index_end,]

    test_labels <- patients_data[test_set_index_start:test_set_index_end, target_index]   # NEW


    cat("dim(training_set) ")
    cat(dim(training_set), "\n")

    cat("dim(test_set) ")
    cat(dim(test_set), "\n")


    # Generation of the CART model
    allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))
    cart_model <- rpart(allFeaturesFormula, method="class", data=training_set);

    pred_test_predictions <- as.numeric(predict(cart_model, test_set, type = "class"))    
    thisResultMat <- regression_rates(test_labels, pred_test_predictions, "@@@ Test set @@@")

     if (exe_i == 1)  resultDataFrame <-  thisResultMat
    else  resultDataFrame <- rbind(resultDataFrame, thisResultMat)
    
 }
 
 cat("\n\n\n=== final results ===\n")
 
 cat("Number of executions = ", execution_number, "\n", sep="")
 # statistics on the dataframe of confusion matrices
 statDescConfMatr <- stat.desc(resultDataFrame)
meanRowResults <- (statDescConfMatr)[c("mean"),]
cat("\n\n")
print(dec_three(meanRowResults))
cat("\n\n=== === === ===\n")

