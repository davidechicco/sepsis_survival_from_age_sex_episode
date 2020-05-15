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
cat("validationFileName: ", externalTestFileName, "\n", sep="")

print(cat("globalROSEp=", globalROSEp, "\n", sep=""))
print(cat("inputDatasetFlag: ", inputDatasetFlag, "\n", sep=""))
targetName <- "hospital_outcome_1alive_0dead"

thisKernel <- "linear"
cat("thisKernel: ", thisKernel, "\n", sep="")


cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "e1071", "PRROC", "dplyr", "pastecs", "class", "gmodels", "kernlab", "ROSE", " lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
library("easypackages")
libraries(list.of.packages)

threshold = 0.5

source("./utils.r")
source("./confusion_matrix_rates.r")

NUM_METRICS <- 9
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1_score", "accuracy", "TP_rate", "TN_rate", "PPV", "NPV", "PR_AUC", "ROC_AUC")

patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

external_test_patients_data <- read.csv(externalTestFileName, header = TRUE, sep =",")
cat("Read data from file ", externalTestFileName, "\n", sep="")
external_test_patients_data <- external_test_patients_data[sample(nrow(external_test_patients_data)),]  # shuffle

# let's put the target label last on the right 
patients_data <- patients_data%>%select(-targetName,targetName)

target_index <- dim(patients_data)[2]
original_patients_data <- patients_data

# formula
allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))

execution_number <- NUMBER_OF_EXECUTIONS
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("\n>>> execution number: ", exe_i, "\n", sep="")

    patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows

    totalElements <- dim(patients_data)[1]

    subsets_size <- totalElements

    if (subsets_size != totalElements) {
        cat("!!! ATTENTION: We are running the method on a subset of the original dataset, \n", sep="")
        cat("!!! containing only ", subsets_size, " elements \n", sep="")
        cat("!!! instead of ", totalElements, " elements \n", sep="")
    }

    patients_data <- patients_data[1:subsets_size, ]

    dataset_dim_retriever(patients_data)
    imbalance_retriever(patients_data[,target_index])

    target_index <- dim(patients_data)[2]

    training_set_perce <- 60
    cat("training_set_perce = ", training_set_perce, "% \n", sep="")
    validation_set_perce <- 20
    cat("validation_set_perce = ", validation_set_perce, "% \n", sep="")
    test_set_perce <- 100 - training_set_perce - validation_set_perce
    cat("test_set_perce = ", test_set_perce, "% \n", sep="")

    # the training set is the first 60% of the whole dataset
    training_set_first_index <- 1 # NEW
    training_set_last_index <- round(dim(patients_data)[1]*training_set_perce/100) # NEW

    # the validation set is the following 20% of the whole dataset
    validation_set_first_index <- round(dim(patients_data)[1]*training_set_perce/100)+1 # NEW
    validation_set_last_index <- round(dim(patients_data)[1]*(training_set_perce+validation_set_perce)/100) # NEW

    # the test set is the last 20% of the whole dataset
    test_set_first_index <- round(dim(patients_data)[1]*(training_set_perce+validation_set_perce)/100)+1 # NEW
    test_set_last_index <- dim(patients_data)[1] # NEW

    cat("[Creating the subsets]\n")
    patients_data_train <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index-1)] # NEW
    patients_data_train_with_labels <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index)] # NEW
    patients_data_train_labels <- patients_data[training_set_first_index:training_set_last_index, target_index] # NEW
    
     cat("training set BEFORE oversampling:")
    imbalance_retriever(patients_data_train_with_labels[,target_index])
         
         if(TRAIN_SET_OVERSAMPLING_SYNTHETIC == TRUE)
         {
            thisP <- globalROSEp
         
            data.rose <- ROSE(allFeaturesFormula, data = patients_data_train_with_labels, p=thisP, seed = 1)$data
            patients_data_train_with_labels <- data.rose
            patients_data_train <- patients_data_train_with_labels[,1:(target_index-1)]
            patients_data_train_labels <- patients_data_train_with_labels[,target_index]
            
            cat("training set AFTER oversampling:")
            imbalance_retriever(patients_data_train_with_labels[,target_index])
         }
    
    # validation set and test set
    patients_data_validation <- patients_data[validation_set_first_index:validation_set_last_index, 1:(target_index-1)] # NEW
    patients_data_validation_labels <- patients_data[validation_set_first_index:validation_set_last_index, target_index] # NEW
    
    # patients_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index-1)] # NEW
    # patients_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW

    patients_data_test <- external_test_patients_data[, 1:(target_index-1)]  # new part
    patients_data_test_labels <- external_test_patients_data[, target_index] # new part

    # The k value must be lower than the size of the training set
    maxK <- 10 #NEW

    mcc_array <- character(length(maxK))

    # NEW PART:

    c_array = c(0.001, 0.01, 0.1)
    # c_array = c(0.01, 0.1)
    mccCounter = 1

    cat("\n[Optimization of the hyper-parameter C start]\n")
    # optimizaion loop
    for(thisC in c_array)
    {
    # apply k-NN with the current K value
    # train on the training set, evaluate in the validation set by computing the MCC
    # save the MCC corresponding to the current K value
    
    cat("[Training the SVM model (with C=",thisC,") on training set & applying the SVM model to validation set]\n", sep="")
    
    svm_model <- svm(patients_data_train_labels ~ ., cost=thisC, data=patients_data_train, method = "C-classification", kernel = thisKernel)
        
    patients_data_validation_PRED <- predict(svm_model, patients_data_validation)
    
    patients_data_validation_pred_binary <- as.numeric (patients_data_validation_PRED)  
    patients_data_validation_pred_binary[patients_data_validation_pred_binary>=threshold]<-1
    patients_data_validation_pred_binary[patients_data_validation_pred_binary<threshold]<-0
    
    #   # patients_data_validation_pred_binary
    #    fg_test <- patients_data_validation_PRED[patients_data_validation_labels==1]
    #    bg_test <- patients_data_validation_PRED[patients_data_validation_labels==0]
    # 
    #    pr_curve_val <- pr.curve(scores.class0 = fg_test, scores.class1 = bg_test, curve = F)
    #    # plot(pr_curve_test)
    #    # print(pr_curve_val)
    # 
    #    roc_curve_val  <- roc.curve(scores.class0 = fg_test, scores.class1 = bg_test, curve = F)
    #    # plot(pr_curve_test)
    # print(roc_curve_val)
    
    mcc_outcome <- mcc(patients_data_validation_labels, patients_data_validation_pred_binary)
    
    cat("When C=",thisC,", the MCC value is ",mcc_outcome, "\t (worst possible: -1; best possible: +1)\n", sep="")
    
    mcc_array[mccCounter] <- mcc_outcome
    mccCounter = mccCounter + 1
    }

    # select the k corresponding to the highest MCC and call it k_best
    bestMCC <- max(mcc_array)
    bestCindex <- match(bestMCC, mcc_array)
    cat("\nThe best C value is ", c_array[bestCindex],", corresponding to MCC=", mcc_array[bestCindex],"\n", sep="")

    cat("[Optimization end]\n\n")

    cat("\n @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ \n")

    # apply k-NN with k_best to the test set

    cat("\n[Training the SVM model (with the OPTIMIZED hyper-parameter C=",c_array[bestCindex],") on training set & applying the SVM to the test set]\n", sep="")
    #patients_data_test_pred <- knn(train = patients_data_train, test = patients_data_test, cl = patients_data_train_labels, k=bestK)

    svm_model_new <- svm(patients_data_train_labels ~ ., cost=c_array[bestCindex], data=patients_data_train, method = "C-classification", kernel = thisKernel)
    patients_data_test_pred <- predict(svm_model_new, patients_data_test)
    
    thisConfMat <- confusion_matrix_rates(patients_data_test_labels, patients_data_test_pred, "@@@ Test set @@@")
    
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

