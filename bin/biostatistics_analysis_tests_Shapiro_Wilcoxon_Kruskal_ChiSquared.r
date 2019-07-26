setwd(".")

# Software originally developed by Giuseppe Jurman <jurman@fbk.eu> on 1st March 2019
# Edited by Davide Chicco <davide.chicco@gmail.com> on 4th March 2019

# /usr/bin/Rscript biostatistics_analysis_tests_Shapiro_Wilcoxon_Kruskal_ChiSquared.r "../data/patients_dataset.csv" "death_event"

# filename <- "../data/journal.pone.0187990.s002_EDITED_length_of_stay.csv"
# TARGET_LABEL <- "length_of_stay_days"

filename <- "../data/journal.pone.0187990.s002_EDITED_survival.csv"
TARGET_LABEL <- "hospital_outcome_1alive_0dead"

resultsFolderPath = "../results/"

SAVE_CORRELATIONS_PLOTS <- FALSE
SAVE_CORRELATIONS_LISTS <- FALSE

if (SAVE_CORRELATIONS_PLOTS == TRUE || SAVE_CORRELATIONS_LISTS==TRUE){
        mkDirResultsCommand <- paste0("mkdir -p ", resultsFolderPath)
        system(mkDirResultsCommand)
        cat("just run the command: ", mkDirResultsCommand, "\n", sep="")
}

# Data load

list.of.packages <- c("readr", "easypackages", "nortest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)
# 
# EXP_ARG_NUM <- 2
# 
# LATEX_MODE <- FALSE
# 
# args = commandArgs(trailingOnly=TRUE)
# if (length(args)<EXP_ARG_NUM) {
#   stop("At least two argument must be supplied (input files)", call.=FALSE)
# } else {
#   # default output file
#   filename <- args[1]
#   TARGET_LABEL <- args[2]
# }

cat("filename: ", filename, "\n", sep="")
cat("TARGET_LABEL: ", TARGET_LABEL, "\n", sep="")

#filename <- "../data/patients_dataset.csv"
#TARGET_LABEL <- "death_event"


patients_dataset <- as.data.frame(read.csv(filename))
ANDERSON_FLAG <- FALSE
SHAPIRO_LIM <- 5000
if(nrow(patients_dataset) > SHAPIRO_LIM) {
    ANDERSON_FLAG <- TRUE
}


# print(head(patients_dataset))
source("utils.r")
num_to_return <- 1
upper_num_limit <- 10000000
exe_num <- sample(1:upper_num_limit, num_to_return)

ROUND_NUM <- 6

patients_dataset[[TARGET_LABEL]] <- as.factor(patients_dataset[[TARGET_LABEL]])

BINARY_TARGET_MODE <- FALSE

if ( length(unique(patients_dataset[[TARGET_LABEL]])) == 2 ) {
    BINARY_TARGET_MODE <- TRUE
}

DEATH_LABEL <- 1
SURVIVAL_LABEL <- 0
dead_patients_data <-  patients_dataset[patients_dataset[[TARGET_LABEL]]==DEATH_LABEL, ]
survived_patients_data <-  patients_dataset[patients_dataset[[TARGET_LABEL]]==SURVIVAL_LABEL, ]

# All the outputs of the tests are stored on the alltests lists, that we print at the end of the discussion.

mycols <- names(patients_dataset)
mycols <- mycols[mycols!=TARGET_LABEL]
alltests <- list()
alltests[["Wilcoxon_rank"]] <- alltests[["Kruskal"]] <- alltests[["Chi"]] <- alltests[["Anderson"]] <- list()
for(thecol in mycols){
    if(BINARY_TARGET_MODE) alltests[["Wilcoxon_rank"]][[thecol]] <-  wilcox.test(as.formula(paste(thecol,TARGET_LABEL,sep="~")),     data=patients_dataset)
    if(BINARY_TARGET_MODE==FALSE) alltests[["Kruskal"]][[thecol]] <-  kruskal.test(as.formula(paste(thecol,TARGET_LABEL,sep="~")),     data=patients_dataset)
    alltests[["Chi"]][[thecol]] <-     chisq.test(x=as.factor(patients_dataset[,thecol]),     y=patients_dataset[[TARGET_LABEL]],    simulate.p.value = TRUE)   
    
    if (ANDERSON_FLAG==FALSE) { 
        alltests[["Shapiro"]][[thecol]] <-  shapiro.test(patients_dataset[,thecol])
    } else {
        alltests[["Anderson"]][[thecol]] <- ad.test(patients_dataset[,thecol])
    }
    
}

if (ANDERSON_FLAG==FALSE) { 
    alltests[["Shapiro"]][[TARGET_LABEL]]<- shapiro.test(as.numeric(patients_dataset[[TARGET_LABEL]]))
} else {
    alltests[["Anderson"]][[TARGET_LABEL]]<- ad.test(as.numeric(patients_dataset[[TARGET_LABEL]]))
}

# As a rule of thumb, the validity of the tests is assessed by looking at the resulting p-values.
# # We start with the Shapiro test of normality,
# cat("\n\t\t == Shapiro ==\n")
vectorShapiro <- c()
if (ANDERSON_FLAG==FALSE) {
    for(thecol in c(mycols,TARGET_LABEL)) { 
        vectorShapiro[thecol] <- alltests[["Shapiro"]][[thecol]]$p.value 
    }
}

if (ANDERSON_FLAG) {
    # cat("\n\t\t == Anderson-Darling ==\n")
    vectorAnderson <- c()
    for(thecol in c(mycols,TARGET_LABEL)) { 
        vectorAnderson[thecol] <- alltests[["Anderson"]][[thecol]]$p.value 
    }
}


# cat("\n\n\t\t == Wilcoxon_rank ==\n")
vectorWilcoxon <- c()
for(thecol in mycols) { 
   if(BINARY_TARGET_MODE)  vectorWilcoxon[thecol] <- round(alltests[["Wilcoxon_rank"]][[thecol]]$p.value, ROUND_NUM) 
    
  #  cat(names((vectorWilcoxon)[thecol]), "\t \t \t", ((vectorWilcoxon)[[thecol]]), "\n")
}
# print(vectorWilcoxon)

# cat("\n\t\t == Kruskal ==\n")
vectorKruskal <- c()
for(thecol in mycols) { 
   if(BINARY_TARGET_MODE==FALSE)  vectorKruskal[thecol] <- round(alltests[["Kruskal"]][[thecol]]$p.value, ROUND_NUM) 
    
    # cat(names((vectorKruskal)[thecol]), "\t \t \t", ((vectorKruskal)[[thecol]]), "\n")
}
# print(vectorKruskal)

# cat("\n\t\t == Chi ==\n")
vectorChi <- c()
for(thecol in mycols) { 
    vectorChi[thecol] <- round(alltests[["Chi"]][[thecol]]$p.value, ROUND_NUM)
    
    # cat(names((vectorChi)[thecol]), "\t \t \t", ((vectorChi)[[thecol]]), "\n")
}
# print(vectorChi)


sortedVectorShapiro <- sort(vectorShapiro)
sortedVectorAnderson <- sort(vectorAnderson)
sortedVectorWilcoxon <- sort(vectorWilcoxon)
sortedVectorKruskal <- sort(vectorKruskal)
sortedVectorChi <- sort(vectorChi)

LATEX_MODE <- FALSE

LATEX_SEP <- "&"
LATEX_END_OF_ROW <- "\\\\"

EMPTY_SEP <- ""
EMPTY_END_OF_ROW <- ""

SEP <- EMPTY_SEP
END_OF_ROW <- EMPTY_END_OF_ROW

if (LATEX_MODE == TRUE ) {
    SEP <- LATEX_SEP
    END_OF_ROW <- LATEX_END_OF_ROW
}


if (ANDERSON_FLAG==FALSE) {
    index <- 1
    cat("\n\t\t == Shapiro-Wilk test ==\n")
    for(thecol in names(sortedVectorShapiro)) {    
        
        cat(index, " ", SEP," \t",  names((sortedVectorShapiro)[thecol]), " ", SEP," \t ", ((sortedVectorShapiro)[[thecol]]), " ", END_OF_ROW," \n", sep="")
        index <- index + 1
    }
}

if (ANDERSON_FLAG) {
    index <- 1
    cat("\n\t\t == Anderson-Darling test ==\n")
    for(thecol in names(sortedVectorAnderson)) {    
        
        cat(index, " ", SEP," \t",  names((sortedVectorAnderson)[thecol]), " ", SEP," \t ", ((sortedVectorAnderson)[[thecol]]), " ", END_OF_ROW," \n", sep="")
        index <- index + 1
    }
}


index <- 1
if(BINARY_TARGET_MODE)  cat("\n\t\t == Mann-Whitney U test with target ", TARGET_LABEL, " ==\n", sep="")
for(thecol in names(sortedVectorWilcoxon)) { 
    
    if(BINARY_TARGET_MODE) cat(index, " ", SEP," \t",  names((sortedVectorWilcoxon)[thecol]), " ", SEP," \t ", ((sortedVectorWilcoxon)[[thecol]]), " ", END_OF_ROW," \n", sep="")
    index <- index + 1
}

index <- 1
 if(BINARY_TARGET_MODE==FALSE)  cat("\n\t\t == Kruskal-Wallis with target ", TARGET_LABEL, " ==\n", sep="")
for(thecol in names(sortedVectorKruskal)) { 
    
    if(BINARY_TARGET_MODE==FALSE) cat(index, " ", SEP," \t",  names((sortedVectorKruskal)[thecol]), " ", SEP," \t ", ((sortedVectorKruskal)[[thecol]]), " ", END_OF_ROW," \n", sep="")
    index <- index + 1
}

index <- 1
cat("\n\t\t == chi squared with target ", TARGET_LABEL, " ==\n", sep="")
for(thecol in names(sortedVectorChi)) { 
    
     cat(index, " ", SEP," \t",  names((sortedVectorChi)[thecol]), " ", SEP," \t ", ((sortedVectorChi)[[thecol]]), " ", END_OF_ROW," \n", sep="")
     index <- index + 1
}

# let's create the dataframes

# dataframeShapiro <- as.data.frame(sortedVectorShapiro)
# dataframeShapiro$pos <- c(1:dim(dataframeShapiro)[1])
# dataframeShapiro$feature <- rownames(dataframeShapiro)

if(BINARY_TARGET_MODE) {
    dataframeWilkoxon <- as.data.frame(sortedVectorWilcoxon)
    dataframeWilkoxon$pos <- c(1:dim(dataframeWilkoxon)[1])
    dataframeWilkoxon$feature <- rownames(dataframeWilkoxon)
}

if(BINARY_TARGET_MODE==FALSE) { 
    dataframeKruskal <- as.data.frame(sortedVectorKruskal)
    dataframeKruskal$pos <- c(1:dim(dataframeKruskal)[1])
    dataframeKruskal$feature <- rownames(dataframeKruskal)
}

dataframeChiSquared <- as.data.frame(sortedVectorChi)
dataframeChiSquared$pos <- c(1:dim(dataframeChiSquared)[1])
dataframeChiSquared$feature <- rownames(dataframeChiSquared)

# let's create the plots

if(SAVE_CORRELATIONS_PLOTS == TRUE) {    

    x_upper_lim <- 300
    # barPlotOfRanking(dataframeShapiro, abs(log(dataframeShapiro$"sortedVectorShapiro")), dataframeShapiro$feature, dataframeShapiro$pos, exe_num, "feature", "abs(log(Shapiro-Wilk))", x_upper_lim, resultsFolderPath)    
    
    #  x_upper_lim <- 1
    # barPlotOfRanking(dataframeWilkoxon, dataframeWilkoxon$"sortedVectorWilcoxon", dataframeWilkoxon$feature, dataframeWilkoxon$pos, exe_num, "feature", "Wilcoxon", x_upper_lim, resultsFolderPath)
    
#     x_upper_lim <- 1
#     barPlotOfRanking(dataframeKruskal, dataframeKruskal$"sortedVectorKruskal", dataframeKruskal$feature, dataframeKruskal$pos, exe_num, "feature", "Kruskal", x_upper_lim, resultsFolderPath)
#     
#     x_upper_lim <- 1
#     barPlotOfRanking(dataframeChiSquared, dataframeChiSquared$"sortedVectorChi", dataframeChiSquared$feature, dataframeChiSquared$pos, exe_num, "feature", "ChiSquared", x_upper_lim, resultsFolderPath)
    
}

if(SAVE_CORRELATIONS_LISTS == TRUE){

    
    # tableFileShapiro <- paste0(resultsFolderPath,"Shapiro_",exe_num, ".csv")
    tableFileWilcoxon <- paste0(resultsFolderPath,"Wilcoxon_",exe_num, ".csv")
    tableFileKruskal <- paste0(resultsFolderPath,"Kruskal_",exe_num, ".csv")
    tableFileChiSquared <- paste0(resultsFolderPath,"chiSquared_",exe_num, ".csv")

#    #  write.table(dataframeShapiro[,c(3,1)], file=tableFileShapiro, sep=",", col.names=TRUE, row.names=FALSE)
#     cat("Saved file ", tableFileShapiro, "\n")
#     
#     # write.table(dataframeWilkoxon[,c(3,1)], file=tableFileWilcoxon, sep=",", col.names=TRUE, row.names=FALSE)
#     # cat("Saved file ", tableFileWilcoxon, "\n")
#     
#     write.table(dataframeKruskal[,c(3,1)], file=tableFileKruskal, sep=",", col.names=TRUE, row.names=FALSE)
#     cat("Saved file ", tableFileKruskal, "\n")
#     
#     write.table(dataframeChiSquared[,c(3,1)], file=tableFileChiSquared, sep=",", col.names=TRUE, row.names=FALSE)
#     cat("Saved file ", tableFileChiSquared, "\n")

}
