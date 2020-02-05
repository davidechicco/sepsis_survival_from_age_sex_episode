#!/bin/bash
#
#$ -cwd
#$ -S /bin/bash
#
set -o nounset -o pipefail -o errexit
set -o xtrace


# linear regression
method="lin_reg_LOS"
today=`date +%Y-%m-%d`
random_number=$(shuf -i1-100000 -n1)

outputFile=""
studyFlag="STUDY_COHORT"
outputFile="../results/newLOS/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript lin_regression_LOS.r $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile="../results/newLOS/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript lin_regression_LOS.r $studyFlag > $outputFile 2> $outputFile


# logistic regression
method="log_reg_LOS"
today=`date +%Y-%m-%d`

outputFile=""
studyFlag="STUDY_COHORT"
outputFile="../results/newLOS/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript logistic_regression_LOS.r $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile="../results/newLOS/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript logistic_regression_LOS.r $studyFlag > $outputFile 2> $outputFile


# decision tree
method="decision_tree_LOS"
today=`date +%Y-%m-%d`

outputFile=""
studyFlag="STUDY_COHORT"
outputFile="../results/newLOS/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript cart_regression_LOS.r $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile="../results/newLOS/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript cart_regression_LOS.r $studyFlag > $outputFile 2> $outputFile

# xgboost
method="xgboost_LOS"
today=`date +%Y-%m-%d`

outputFile=""
studyFlag="STUDY_COHORT"
outputFile="../results/newLOS/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript xgboost_regression_LOS.r $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile="../results/newLOS/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript xgboost_regression_LOS.r $studyFlag > $outputFile 2> $outputFile



# naive bayes
method="naive_bayes_LOS"
today=`date +%Y-%m-%d`

outputFile=""
studyFlag="STUDY_COHORT"
outputFile="../results/newLOS/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript naive_bayes_regression_LOS.r $studyFlag > $outputFile 2> $outputFile

outputFile=""
studyFlag="PRIMARY_COHORT"
outputFile="../results/newLOS/"$studyFlag"_"$method"_"$today"_rand"$random_number
Rscript naive_bayes_regression_LOS.r $studyFlag > $outputFile 2> $outputFile
