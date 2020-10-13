# Prediction of sepsis survival from age, sex, and septic episode number
Machine learning prediction of sepsis survival from age, sex, and septic episode number

### Requirements
R programming language and platform (version 3.6.2 or greater)

### Instructions
Download the current GitHub repository.
To perform the predictions on the primary cohort, type on terminal shell:

`Rscript lin_reg_classification.r PRIMARY_COHORT`

`Rscript naive_bayes_classification.r PRIMARY_COHORT`

`Rscript svm_radial_classification.r PRIMARY_COHORT`

`Rscript svm_linear_classification.r PRIMARY_COHORT`

`Rscript xgboost_classification_survival.r PRIMARY_COHORT`

To perform the predictions on the study cohort, type on terminal shell:

`Rscript lin_reg_classification.r STUDY_COHORT`

`Rscript naive_bayes_classification.r STUDY_COHORT`

`Rscript svm_radial_classification.r STUDY_COHORT`

`Rscript svm_linear_classification.r STUDY_COHORT`

`Rscript xgboost_classification_survival.r STUDY_COHORT`

### Article
Additional information about this study is available in the following peer-reviewed published article:

> Davide Chicco, Giuseppe Jurman. "[Survival prediction of patients with sepsis from age, sex, and septic episode number alone](https://doi.org/10.1038/s41598-020-73558-3)". Scientific Reports 10, 17156 (2020). 


### Contacts
For any enquire, please write to [Davide Chicco](https://www.davidechicco.it) (Krembil Research Institute) at [davidechicco(AT)davidechicco.it](mailto:davidechicco@davidechicco.it)
