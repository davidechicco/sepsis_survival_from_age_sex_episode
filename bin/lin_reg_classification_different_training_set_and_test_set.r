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

} else if(inputDatasetFlag == "PRIMARY_COHORT") {

    fileName <- "../data/journal.pone.0187990.s002_EDITED_survival.csv" #primary cohort
    globalROSEp <- 0.38 # primary cohort
    
} else {
    fileName <- NULL    
}

externalTestFileName <- "../data/pone.0200187.s002_three_features_extrapolated_only_preop_shock_present_FINAL.csv" #primary cohort
cat("externalTestFileName: ", externalTestFileName, "\n", sep="")

cat("globalROSEp=", globalROSEp, "\n", sep="")
cat("inputDatasetFlag: ", inputDatasetFlag, "\n", sep="")
targetName <- "hospital_outcome_1alive_0dead"


cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "PRROC", "e1071", "randomForest","class", "gmodels", "formula.tools", "dplyr", "pastecs", "ROSE", " lubridate")
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

# data read
patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

external_test_patients_data <- read.csv(externalTestFileName, header = TRUE, sep =",")
cat("Read data from file ", externalTestFileName, "\n", sep="")
external_test_patients_data <- external_test_patients_data[sample(nrow(external_test_patients_data)),]  # shuffle

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
    prc_data_train_including_label <-  patients_data[training_set_first_index:training_set_last_index, 1:(target_index)] 
    
    # prc_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index-1)] 
    # prc_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW
    
    prc_data_test <- external_test_patients_data[, 1:(target_index-1)] 
    prc_data_test_labels <- external_test_patients_data[, (target_index)] 


#     cat("[Creating the subsets for the labels \"1\"-\"0\"]\n")
#     prc_data_train_labels <- patients_data[training_set_first_index:training_set_last_index, target_index] # NEW
#     prc_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW
# 
#     library(class)
#     library(gmodels)
# 
#     # apply k-NN with k_best to the test set

        cat("training set BEFORE oversampling:")
        imbalance_retriever(prc_data_train_including_label[,target_index])
         
         if(TRAIN_SET_OVERSAMPLING_SYNTHETIC == TRUE)
         {
            thisP <- globalROSEp
            
            # formula
            allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))
            
            data.rose <- ROSE(allFeaturesFormula, data = prc_data_train_including_label, p=thisP, seed = 1)$data
            prc_data_train_including_label <- data.rose
            prc_data_train <- prc_data_train_including_label[, 1:(target_index-1)] 
            
            cat("training set AFTER oversampling:")
            imbalance_retriever(prc_data_train_including_label[,target_index])
         }
        
        cat("[training set dimensions: ", dim(prc_data_train_including_label)[1], " patients]\n")

        cat("[test set dimensions: ", dim(prc_data_test)[1], " patients]\n")

        cat("[Creating the training set and test set for the labels \"1\"-\"0\"]\n")
        prc_data_train_labels <- prc_data_train_including_label[, target_index] # NEW


    cat("\n[Training the linear regression model on training set & applying the linear regression to test set]\n", sep="")

    lin_reg_model_new <- lm(prc_data_train_labels ~ ., data=prc_data_train)
    prc_data_test_pred <- predict(lin_reg_model_new, prc_data_test)

    prc_data_test_pred_bin <- as.numeric(prc_data_test_pred)
    prc_data_test_pred_bin[prc_data_test_pred_bin>=threshold]<-1
    prc_data_test_pred_bin[prc_data_test_pred_bin<threshold]<-0

    thisConfMat <- confusion_matrix_rates(prc_data_test_labels, prc_data_test_pred_bin, "@@@ Test set @@@")
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
