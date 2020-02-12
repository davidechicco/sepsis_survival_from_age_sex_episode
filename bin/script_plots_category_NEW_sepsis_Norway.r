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

primary_or_study_cohort <- "STUDY_COHORT"
# STUDY_COHORT or PRIMARY_COHORT

if(primary_or_study_cohort == "STUDY_COHORT"){

    fileName <- "/home/davidechicco/my_projects/sepsis_survival_in_Norway/data/dataFrameForSurvival_study_cohort_rand2109.csv"
    datasetFlag <- "study"

} else if(primary_or_study_cohort == "PRIMARY_COHORT") {

    fileName <- "/home/davidechicco/my_projects/sepsis_survival_in_Norway/data//journal.pone.0187990.s002_EDITED_survival.csv"
    datasetFlag <- "primary"

}


sepsis_datatable <- read.csv(fileName, header = TRUE, sep =",");
sick_patients_table <- (sepsis_datatable[sepsis_datatable$hospital_outcome_1alive_0dead==1,])
healthy_patients_table <- (sepsis_datatable[sepsis_datatable$hospital_outcome_1alive_0dead==0,])

random_number <- sample(1:1000000, 1)


cate_vector <- c("episode_number")
cate_vector <- c(grep("episode_number", colnames(sepsis_datatable)), grep("sex_0male_1female", colnames(sepsis_datatable)))

# cate_vector <- c(grep("city", colnames(mesothelioma_datatable)), grep("gender", colnames(mesothelioma_datatable)), grep("habit.of.cigarette", colnames(mesothelioma_datatable)), grep("Lung.side", colnames(mesothelioma_datatable)), grep("performance.status", colnames(mesothelioma_datatable)), grep("type.of.MM", colnames(mesothelioma_datatable)))

lun <- length(cate_vector)

fontSize = 10

# Sick patients
 

i=1

sick_plot_list <- list()
for(index in cate_vector){

  temp_df = sick_patients_table
  temp_df$CurrentFeature = sick_patients_table[, index]
  P_this <- NULL
  
  P_this <- ggplot(temp_df, aes(factor(" "), fill=factor(CurrentFeature))) + geom_bar(stat="count", position="stack") + ylim(0,dim(sick_patients_table)[1])  + labs(x=" ", y=colnames(sick_patients_table[index])) + theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize), legend.text=element_text(size=fontSize), legend.title=element_text(size=fontSize), legend.key.size=unit(0.3, "cm")) + coord_flip() + theme(legend.title=element_blank()) 
  
  if (index == grep("episode_number", colnames(sepsis_datatable))) {
    P_this <- P_this +  scale_fill_discrete(labels=c("1", "2", "3", "4", "5"))
  }
   if (index == grep("sex_0male_1female", colnames(sepsis_datatable))) {
    P_this <- P_this +  scale_fill_discrete(labels=c("male", "female"))
  }
  
  
   sick_plot_list[[length(sick_plot_list) + 1]] <- P_this
  i <- i+1
}

# sick_plot_list[[1]] <- sick_plot_list[[1]] + ggtitle("sepsis patients: boolean features") + theme(plot.title = element_text(size = fontSize+1, hjust = 0.5))

do.call("ggarrange", c(sick_plot_list, ncol=1))
#grid.arrange(sick_plot_list)


outputPdfFileName <- paste("../results/plots/sepsis_", datasetFlag ,"_CATEGORY_barplots_sick_patients_vs_", random_number, ".pdf", sep="")
# pdf.options(reset = TRUE, onefile = FALSE)
# pdf(outputPdfFileName, onefile=FALSE)
ggsave(filename = outputPdfFileName)
cat("saving plot into file ", outputPdfFileName, "\n")
# dev.off()


# Healthy patients
 


i=1

healthy_plot_list <- list()
for(index in cate_vector){
  temp_df = healthy_patients_table
  temp_df$CurrentFeature = healthy_patients_table[, index]
  P_this <- NULL
    
  P_this <- ggplot(temp_df, aes(factor(" "), fill=factor(CurrentFeature))) + geom_bar(stat="count", position="stack") + ylim(0,dim(healthy_patients_table)[1]) + labs(x=" ", y=(colnames(healthy_patients_table[index])))  + theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize), legend.text=element_text(size=fontSize), legend.title=element_text(size=fontSize), legend.key.size=unit(0.3, "cm")) + coord_flip() + theme(legend.title=element_blank())
  
  if (index == grep("episode_number", colnames(sepsis_datatable))) {
    P_this <- P_this +  scale_fill_discrete(labels=c("1", "2", "3", "4", "5"))
  }
  if (index == grep("sex_0male_1female", colnames(sepsis_datatable))) {
    P_this <- P_this +  scale_fill_discrete(labels=c("male", "female"))
  }

  
  P_this$width = 20
  
  healthy_plot_list[[length(healthy_plot_list) + 1]] <- P_this
  i <- i+1
}

# healthy_plot_list[[1]] <- healthy_plot_list[[1]] + ggtitle("non-sepsis patients: boolean features") + theme(plot.title = element_text(size = fontSize+1, hjust = 0.5))

do.call("ggarrange", c(healthy_plot_list, ncol=1))
#grid.arrange(sick_plot_list)


outputPdfFileName <- paste("../results/plots/sepsis_", datasetFlag ,"_CATEGORY_barplots_healthy_patients_vs_", random_number, ".pdf", sep="")
# pdf.options(reset = TRUE, onefile = FALSE)
# pdf(outputPdfFileName, onefile=FALSE)

ggsave(filename = outputPdfFileName)
cat("saving plot into file ", outputPdfFileName, "\n")
# dev.off()
