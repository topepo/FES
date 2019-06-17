# /bin/bash

R CMD BATCH --vanilla pls_derivatives.R
R CMD BATCH --vanilla cubist_standardized.R
R CMD BATCH --vanilla pls_baseline.R
R CMD BATCH --vanilla nnet_none.R
R CMD BATCH --vanilla pls_standardized.R
R CMD BATCH --vanilla svm_baseline.R
R CMD BATCH --vanilla cubist_none.R
R CMD BATCH --vanilla cubist_derivatives.R
R CMD BATCH --vanilla cubist_smoothed.R
R CMD BATCH --vanilla nnet_baseline.R
R CMD BATCH --vanilla nnet_smoothed.R
R CMD BATCH --vanilla pls_smoothed.R
R CMD BATCH --vanilla svm_derivatives.R
R CMD BATCH --vanilla nnet_derivatives.R
R CMD BATCH --vanilla cubist_baseline.R
R CMD BATCH --vanilla svm_none.R
R CMD BATCH --vanilla svm_standardized.R
R CMD BATCH --vanilla nnet_standardized.R
R CMD BATCH --vanilla svm_smoothed.R
R CMD BATCH --vanilla pls_none.R

