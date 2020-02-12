setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
# set.seed(11)

list.of.packages <- c("easypackages", "gtable", "grid", "gridExtra", "egg", "ggplot2", "ggpubr") # other packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library("easypackages")
libraries(list.of.packages)

fileName <- ""

primary_or_study_cohort <- "PRIMARY_COHORT"
# STUDY_COHORT or PRIMARY_COHORT

if(primary_or_study_cohort == "STUDY_COHORT"){

    fileName <- "/home/davidechicco/my_projects/sepsis_survival_in_Norway/data/dataFrameForSurvival_study_cohort_rand2109.csv"
    datasetFlag <- "study"

} else if(primary_or_study_cohort == "PRIMARY_COHORT") {

    fileName <- "/home/davidechicco/my_projects/sepsis_survival_in_Norway/data//journal.pone.0187990.s002_EDITED_survival.csv"
    datasetFlag <- "primary"

}


sepsis_datatable <- read.csv(fileName, header = TRUE, sep =",");
# sick_patients_table <- (sepsis_datatable[sepsis_datatable$class.of.diagnosis==1,])
# healthy_patients_table <- (sepsis_datatable[sepsis_datatable$class.of.diagnosis==0,])
sick_patients_table <- (sepsis_datatable[sepsis_datatable$hospital_outcome_1alive_0dead==1,])
healthy_patients_table <- (sepsis_datatable[sepsis_datatable$hospital_outcome_1alive_0dead==0,])

random_number <- sample(1:1000000, 1)

time_vector <- c(grep("age_years", colnames(sepsis_datatable)))

lun <- length(time_vector)

fontSize <- 20
max_y <- 2400

# Sick patients
 


i=1

sick_plot_list <- list()
for(index in time_vector){

  sick_patients_table$CurrentFeature = sick_patients_table[, index]
  P_this <- NULL
  feature_label <- colnames(sepsis_datatable[index])
  
  print(feature_label)
  
  P_this <- ggplot(sick_patients_table, aes(x=CurrentFeature)) + geom_histogram(fill="#CC79A7", binwidth=1) + labs(x=(feature_label), y="#patients") + ylim(0,max_y) 
  
  sick_plot_list[[length(sick_plot_list) + 1]] <- P_this
  i <- i+1
}

# sick_plot_list[[1]] <- sick_plot_list[[1]] + ggtitle("sepsis patients: boolean features") + theme(plot.title = element_text(size = fontSize+1, hjust = 0.5))

do.call("ggarrange", c(sick_plot_list, ncol=1))
#grid.arrange(sick_plot_list)
# dev.off()

outputPdfFileName <- paste("../results/plots/sepsis_", datasetFlag ,"_TIME_histograms_sick_patients_vs_", random_number, ".pdf", sep="")
ggsave(filename = outputPdfFileName)
cat("saving plot into file ", outputPdfFileName, "\n")



# Healthy patients
 
# fontSize <- 20
# max_y <- 600


i=1

healthy_plot_list <- list()
for(index in time_vector){

  healthy_patients_table$CurrentFeature = healthy_patients_table[, index]
  P_this <- NULL
  feature_label <- colnames(sepsis_datatable[index])
    
  P_this <- ggplot(healthy_patients_table, aes(x=CurrentFeature)) + geom_histogram(fill="#009E73", binwidth=1) + labs(x=(feature_label), y="#patients")  + ylim(0,max_y) 
  
  healthy_plot_list[[length(healthy_plot_list) + 1]] <- P_this
  i <- i+1
}

# healthy_plot_list[[1]] <- healthy_plot_list[[1]] + ggtitle("non-sepsis patients: boolean features") + theme(plot.title = element_text(size = fontSize+1, hjust = 0.5))

do.call("ggarrange", c(healthy_plot_list, ncol=1))
#grid.arrange(sick_plot_list)
# dev.off()

outputPdfFileName <- paste("../results/plots/sepsis_", datasetFlag ,"_TIME_histograms_healthy_patients_vs_", random_number, ".pdf", sep="")
ggsave(filename = outputPdfFileName)
cat("saving plot into file ", outputPdfFileName, "\n")
