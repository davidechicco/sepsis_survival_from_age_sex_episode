#!/bin/bash
#
#$ -cwd
#$ -S /bin/bash
#
set -o nounset -o pipefail -o errexit
set -o xtrace


# linear regression
method="lin_reg_survival"
today=`date +%Y-%m-%d`
random_number=$(shuf -i1-100000 -n1)

outputFile=""
studyFlag="STUDY_COHORT"
outputFile="../results/newSurvival/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript lin_reg_classification.r $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile="../results/newSurvival/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript lin_reg_classification.r $studyFlag > $outputFile 2> $outputFile




# xgboost
method="xgboost_survival"
today=`date +%Y-%m-%d`

outputFile=""
studyFlag="STUDY_COHORT"
outputFile="../results/newSurvival/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript xgboost_classification_survival.r $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile="../results/newSurvival/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript xgboost_classification_survival.r $studyFlag > $outputFile 2> $outputFile

# naive bayes
method="naive_bayes_survival"
today=`date +%Y-%m-%d`

outputFile=""
studyFlag="STUDY_COHORT"
outputFile="../results/newSurvival/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript naive_bayes_classification.r $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile="../results/newSurvival/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript naive_bayes_classification.r $studyFlag > $outputFile 2> $outputFile

# radial SVM
method="radial_SVM_survival"
today=`date +%Y-%m-%d`

outputFile=""
studyFlag="STUDY_COHORT"
outputFile="../results/newSurvival/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript svm_radial_classification.r $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile="../results/newSurvival/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript svm_radial_classification.r $studyFlag > $outputFile 2> $outputFile

# linear SVM
method="linear_SVM_survival"
today=`date +%Y-%m-%d`

outputFile=""
studyFlag="STUDY_COHORT"
outputFile="../results/newSurvival/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript svm_linear_classification.r $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile="../results/newSurvival/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript svm_linear_classification.r $studyFlag > $outputFile 2> $outputFile
