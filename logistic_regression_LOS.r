setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)

NUMBER_OF_EXECUTIONS <- 100
NORMALIZATION <- FALSE

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

list.of.packages <- c("easypackages", "PRROC", "e1071", "randomForest","class", "gmodels", "formula.tools", "dplyr", "pastecs", "ROSE", "Metrics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)


source("./confusion_matrix_rates.r")
source("./utils.r")

NUM_METRICS <- 5
resultDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(resultDataFrame) <- c("RMSE", "MAE", "MSE", "SMAPE", "R^2")

threshold <- 0.5

patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("loaded fileName = ", fileName, "\n", sep="")

# let's put the target label last on the right 
patients_data <- patients_data%>%select(-targetName,targetName)
patients_data_original <- patients_data


target_index <- dim(patients_data)[2]
original_patients_data <- patients_data

if(NORMALIZATION==TRUE){

    # let's normalize the target
    LOS_range <- max(patients_data[,target_index]) - min(patients_data[,target_index])
    patients_data[,target_index] <- (patients_data[,target_index])/LOS_range
    cat("Normalization yes\n")
}


# as.factor()
# patients_data[,target_index] <- as.factor(patients_data[,target_index])

# formula
allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))

# cycle of executions

execution_number <- NUMBER_OF_EXECUTIONS
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("\n\n\n>>> execution number: ", exe_i, "\n", sep="")

    patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows

    target_index <- dim(patients_data)[2]
    cat("target_index = ", target_index, "\n", sep="") 

    training_set_perce <- 80

    cat("training_set_perce = ", training_set_perce, "%\n", sep="")

    # the training set is the first training_set_perce% of the whole dataset
    training_set_first_index <- 1 # NEW
    training_set_last_index <- round(dim(patients_data)[1]*training_set_perce/100) # NEW

    # the test set is the last 20% of the whole dataset
    test_set_first_index <- round(dim(patients_data)[1]*training_set_perce/100)+1 # NEW
    test_set_last_index <- dim(patients_data)[1] # NEW

    cat("[Creating the subsets for the values]\n")
    prc_data_train <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index-1)] 
    prc_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index-1)] 
    prc_data_train_including_label <-  patients_data[training_set_first_index:training_set_last_index, 1:(target_index)] 

        
        cat("[training set dimensions: ", dim(prc_data_train_including_label)[1], " patients]\n")

        cat("[test set dimensions: ", dim(prc_data_test)[1], " patients]\n")

        cat("[Creating the training set and test set for the labels \"1\"-\"0\"]\n")
        prc_data_train_labels <- prc_data_train_including_label[, target_index] # NEW
        prc_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW

    cat("\n[Training the logistic regression model on training set & applying the logistic regression to test set]\n", sep="")

    logistic_reg_model_new <- glm(as.factor(length_of_stay_days) ~ (age_years + sex_0male_1female + episode_number), data=prc_data_train_including_label, family = "binomial")
    prc_data_test_pred <- predict(logistic_reg_model_new, prc_data_test, type = "response")

    prc_data_test_pred_bin <- as.numeric(prc_data_test_pred)
    
#     prc_data_test_pred_bin[prc_data_test_pred_bin>=threshold]<-1
#     prc_data_test_pred_bin[prc_data_test_pred_bin<threshold]<-0
# 
#     thisConfMat <- confusion_matrix_rates(prc_data_test_labels, prc_data_test_pred_bin, "@@@ Test set @@@")
    
    thisResultMat <- regression_rates(prc_data_test_labels, prc_data_test_pred_bin, "@@@ Test set @@@")
    
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


