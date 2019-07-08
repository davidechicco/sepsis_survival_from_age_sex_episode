# install.packages("class")
# install.packages("gmodels")

# function that normalizes
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}
  

# function that converts a string 
# https://stats.stackexchange.com/a/17995
fromStringToNumeric <- function(x_array) {

   new_x <- as.factor(x_array)
   levels(new_x) <- 1:length(levels(new_x))
   new_x_num <- as.numeric(new_x)

   return (new_x_num)
}

# fileName <- "../data/journal.pone.0187990.s002_EDITED_survival.csv"
# targetName <- "hospital_outcome_1alive_0dead"
# fileName <- "../data/journal.pone.0187990.s002_EDITED_length_of_stay.csv"
# targetName <- "length_of_stay_days"

datasetFileName <- "../data/journal.pone.0187990.s002_EDITED_length_of_stay.csv"
targetName <- "length_of_stay_days"

list.of.packages <- c("easypackages", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)


num_to_return <- 1
upper_num_limit <- 10000000
exe_num <- sample(1:upper_num_limit, num_to_return)

cat("[Reading the data file]\n")

patients_data <- read.csv(datasetFileName, stringsAsFactors = FALSE) 
cat("The input dataset file read is ", datasetFileName, "\n", sep="")

patients_data <- patients_data%>%dplyr::select(-targetName,targetName)

num_of_columns_original <- dim(patients_data)[2]
num_of_instances <- dim(patients_data)[1]
num_of_features_original <- num_of_columns_original - 1

patients_data_original <- patients_data

colnames(patients_data)

patients_data_num <- patients_data

num_of_columns <- dim(patients_data_num)[2]
num_of_features <- num_of_columns - 1

target_column_index <- grep(targetName, colnames(patients_data_num))

cat("num_of_features = ", num_of_features, "\n")
cat("the target is ", targetName ,", column index =", target_column_index, "\n")

for(i in 1:(num_of_features))
{
  patients_data_num[,i] <- fromStringToNumeric(patients_data_num[,i])
}
patients_data_num[,target_column_index] <- patients_data[,target_column_index]
# patients_data_num <- patients_data_num[sample(nrow(patients_data_num)),] # shuffle the rows


round(prop.table(table(patients_data_num[,target_column_index])) * 100, digits = 1)  # it gives the result in the percentage form rounded of to 1 decimal place( and so it’s digits = 1)

cat("[Normalizing the values of the data file (including the ", targetName, " target column)]\n", sep="")
patients_data_norm <- as.data.frame(lapply(patients_data_num[1:num_of_columns], normalize))

print(colnames(patients_data_norm))
colnames(patients_data_norm) <- colnames(patients_data_original) 
print(colnames(patients_data_norm))

datasetFileNameWithoutExtension <- strsplit(datasetFileName, ".csv")[[1]]
dataNormFile <- paste0(datasetFileNameWithoutExtension, "_NORM_", exe_num, ".csv")

print(head(patients_data_norm))

cat("The imputed dataset will be saved in the ", dataNormFile, " file\n", sep="")
# write.csv(patients_data_new_imputed, file=dataImputationFile)
write.table(patients_data_norm, file = dataNormFile, row.names=FALSE, na="", col.names=TRUE, sep=",")



