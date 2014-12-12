**OVERVIEW**

This is a function that uses the [SuperLearner algorithm](http://cran.r-project.org/web/packages/SuperLearner/index.html) to estimate additive scale effects between a continuous outcome variable and many binary predictors. SuperLearner is an ensemble prediction method. This algorithm uses the [G-computation estimator](http://www.ncbi.nlm.nih.gov/pubmed/21415029) discussed by Snowden et al. The benefit of this is that the user does not have to specify a model. This estimation method has many applications, particularly to genetic research.

This function is written so as to faciliate parallel processing. To generate 95% confidence intervals for the effect estimates, bootstrapping is conducted. Bootstrap samples are analyzed in parallel to speed up processing time.


**REQUIRED LIBRARIES:**

SuperLearner, plyr, foreach, doMC, arm, gam, earth, randomForest, nnet


**FUNCTION ARGUMENTS:**

dataset = the data frame used in variable selection

outcome = the continuous outcome variable to be predicted

reps = the number bootstrap samples to be created to generate the 95% confidence intervals. There is no clear rule as to how many samples should be generated, but more will be more informative. The default is 1000.

MC = the number of cores to be used in analysis. Default is 1.

**EXAMPLE FUNCTION CALL:**

Function calls look like this:

get.coeffs("simdata","Y",1000,10)