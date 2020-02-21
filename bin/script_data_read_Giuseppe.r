setwd(".")
options(stringsAsFactors = FALSE)
# cat("\014")
# set.seed(11)

num_to_return <- 1
exe_num <- sample(1:as.numeric(10000), num_to_return)

# source("./utils.r")


list.of.packages <- c("easypackages", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)




#carico i dati
NUM_NUM_COLS <- 3
NUM_STRING_COLS <- 9

mydata <- readxl::read_excel("../data/journal.pone.0187990.s002.xlsx",
                             col_types = c("numeric", "text",rep("numeric", NUM_NUM_COLS),rep("text", NUM_STRING_COLS)))

dim(mydata)  # 110204 x 14

# listo le categorie di tipo organ dysfunction, tenendo sia maiuscole che minuscole perché nel file Excel originale ci sono per errore codici con le minuscole
# quelle con 2 cifre sono macrocategorie (e.g. D65, cha vanno poi espanse in D650-D659)

# original list by Giuseppe
# organ_dys <- c("R57","I509","J80","J95","J960","N17","N990","D65","D69","K72","E872",             "r57","i509","j80","j95","j960","n17","n990","d65","d69","k72","e872")

organ_dys <- c("R570", "R571", "R572", "R578", "R579", "I509", "J80", "J950", "J951", "J952", "J953", "J954", "J955", "J958", "J959", "J960", "J9601", "J9609", "N170", "N171", "N172", "N178", "N179", "N990", "D65", "D690", "D691", "D692", "D693", "D694", "D695", "D696", "D698", "D699", "K720", "K721", "K729", "E872")

# codes retrieved from Table 1 of the paper
#
# R57   X
# I50.9 X
# J80 X
# J95 X
# J96.0  X
# N17 X
# N99.0 X
# D65 X
# D69 X
# K72 X
# E87.2




# espando le macrocategorie con le sottocategorie 0-9 (alcune non esistono, ma questo non è un pbm e non inficia il conto)
LEN_MACR_CATE <- 3
organ_dys_all <- organ_dys
# for(k in organ_dys){
#   if(str_length(k)==LEN_MACR_CATE){
#     for(w in 0:9){
#       organ_dys_all <- c(organ_dys_all,paste(k,w,sep=""))
#     }
#   }
# }

# escludo R572 perché é giá listata tra le categorie della primary search, altrimenti la si conta due volte
organ_dys_all <- organ_dys_all[!organ_dys_all %in% c("R572","r572")]

# conto tutte le righe con le categorie organ_dys_all
INDEX_COL_FIRST_CATE <- 6
INDEX_COL_LAST_CATE <- 14
INDEX_AGE <- 1
INDEX_SEX <- 2
INDEX_LOS <- 3
INDEX_SURVIVAL <- 4
INDEX_EPISODE <- 5

dataFrameForLOS <- data.frame(age_years=integer(),	
                                                                sex_0male_1female=integer(),	
                                                                episode_number=integer(),
                                                                length_of_stay_days=integer(),
                                                                stringsAsFactors=FALSE) 
dataFrameForSurvival <- data.frame(age_years=integer(),	
                                                                sex_0male_1female=integer(),	
                                                                episode_number=integer(),
                                                                hospital_outcome_1alive_0dead=integer(),
                                                                stringsAsFactors=FALSE) 

idx <- c()
for(j in INDEX_COL_FIRST_CATE:INDEX_COL_LAST_CATE){
    theseChosenRows <- which(as.data.frame(mydata[,j])[,1] %in% organ_dys_all, arr.ind = TRUE)
    idx <- c(idx, theseChosenRows)
    
    
    theseChosenRowsAsDFrameForLOS <-  as.data.frame(mydata)[theseChosenRows, c(INDEX_AGE, INDEX_SEX, INDEX_EPISODE, INDEX_LOS)]
    theseChosenRowsAsDFrameForSurvival <-  as.data.frame(mydata)[theseChosenRows, c(INDEX_AGE, INDEX_SEX, INDEX_EPISODE, INDEX_SURVIVAL)]
      
    colnames(theseChosenRowsAsDFrameForLOS) <- c("age_years", "sex_0male_1female", "episode_number", "length_of_stay_days")
    colnames(theseChosenRowsAsDFrameForSurvival) <- c("age_years", "sex_0male_1female", "episode_number", "hospital_outcome_1alive_0dead")
    
    
    theseChosenRowsAsDFrameForLOS$original_row_num <- rownames(theseChosenRowsAsDFrameForLOS)
    theseChosenRowsAsDFrameForSurvival$original_row_num <- rownames(theseChosenRowsAsDFrameForSurvival)
    
    dataFrameForLOS <- rbind(dataFrameForLOS, theseChosenRowsAsDFrameForLOS)
    dataFrameForSurvival <- rbind(dataFrameForSurvival, theseChosenRowsAsDFrameForSurvival)
    
#     print("theseChosenRowsAsDFrameForLOS")
#     print(theseChosenRowsAsDFrameForLOS)
#     print("theseChosenRowsAsDFrameForSurvival")
#     print(theseChosenRowsAsDFrameForSurvival)
}

uniqueRows_dataFrameForLOS <- dataFrameForLOS[!duplicated(dataFrameForLOS[,c("original_row_num")]),]
uniqueRows_dataFrameForSurvival <- dataFrameForSurvival[!duplicated(dataFrameForSurvival[,c("original_row_num")]),]

 # 19051 ... ci sono 591 righe in eccesso rispetto alle 18460 che dovrebbero essere nel study cohort
cat("Number of unique rows found as idx: ", (length(unique(idx))), "\n")
cat("Expected number of unique rows: 18460\n")

cat("Number of rows found in the survival dataframe: ", nrow(uniqueRows_dataFrameForLOS), "\n")
cat("Number of rows found in the LOS dataframe: ", nrow(uniqueRows_dataFrameForSurvival), "\n")


SAVE_FILES_FLAG <- FALSE

if(SAVE_FILES_FLAG) {

    fileLOS <- paste0("../data/dataFrameForLOS_", exe_num, ".csv")
    write.csv(uniqueRows_dataFrameForLOS, file=fileLOS, row.names=FALSE)
    cat("The file ", fileLOS, " has been saved\n", sep="")

    fileSurvival <- paste0("../data/dataFrameForSurvival_", exe_num, ".csv")
    write.csv(uniqueRows_dataFrameForSurvival, file=fileSurvival, row.names=FALSE)
    cat("The file ", fileSurvival, " has been saved\n", sep="")

}
