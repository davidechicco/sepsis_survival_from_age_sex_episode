#!/bin/bash
#
#$ -cwd
#$ -S /bin/bash
#
set -o nounset -o pipefail -o errexit
set -o xtrace

random_number=$(shuf -i1-100000 -n1)
moment=$(date +"%Y-%m-%d_h%Hm%Ms%S")

resultsFolder="../results_external_dataset/"

# # linear regression
# method="lin_reg_survival"
# outputFile=""
# studyFlag="STUDY_COHORT"
# outputFile=$resultsFolder$studyFlag"_"$method"_"$moment"_rand"$random_number
# Rscript lin_reg_classification_different_training_set_and_test_set.r  $studyFlag > $outputFile 2> $outputFile
# 
# outputFile=""
# studyFlag="PRIMARY_COHORT"
# outputFile=$resultsFolder$studyFlag"_"$method"_"$moment"_rand"$random_number
# Rscript lin_reg_classification_different_training_set_and_test_set.r  $studyFlag > $outputFile 2> $outputFile


# xgboost
method="xgboost_survival"
outputFile=""
studyFlag="STUDY_COHORT"
outputFile=$resultsFolder$studyFlag"_"$method"_"$moment"_rand"$random_number
Rscript xgboost_classification_survival_different_training_set_and_test_set.r  $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile=$resultsFolder$studyFlag"_"$method"_"$moment"_rand"$random_number
Rscript xgboost_classification_survival_different_training_set_and_test_set.r  $studyFlag > $outputFile 2> $outputFile

# naive bayes
method="naive_bayes_survival"
outputFile=""
studyFlag="STUDY_COHORT"
outputFile=$resultsFolder$studyFlag"_"$method"_"$moment"_rand"$random_number
Rscript naive_bayes_classification_different_training_set_and_test_set.r  $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile=$resultsFolder$studyFlag"_"$method"_"$moment"_rand"$random_number
Rscript naive_bayes_classification_different_training_set_and_test_set.r  $studyFlag > $outputFile 2> $outputFile

# linear SVM
method="linear_SVM_survival"
outputFile=""
studyFlag="STUDY_COHORT"
outputFile=$resultsFolder$studyFlag"_"$method"_"$moment"_rand"$random_number
Rscript svm_linear_classification_different_training_set_and_test_set.r  $studyFlag > $outputFile 2> $outputFile

method="linear_SVM_survival"
outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile=$resultsFolder$studyFlag"_"$method"_"$moment"_rand"$random_number
Rscript svm_linear_classification_different_training_set_and_test_set.r  $studyFlag > $outputFile 2> $outputFile

# radial SVM
method="radial_SVM_survival"
outputFile=""
studyFlag="STUDY_COHORT"
outputFile=$resultsFolder$studyFlag"_"$method"_"$moment"_rand"$random_number
Rscript svm_radial_classification_different_training_set_and_test_set.r  $studyFlag > $outputFile 2> $outputFile

method="radial_SVM_survival"
outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile=$resultsFolder$studyFlag"_"$method"_"$moment"_rand"$random_number
Rscript svm_radial_classification_different_training_set_and_test_set.r  $studyFlag > $outputFile 2> $outputFile
