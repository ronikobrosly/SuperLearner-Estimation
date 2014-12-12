

get.coeffs <- function(dataset, outcome, reps=1000, MC=1)
{

require(SuperLearner)
require(plyr)
require(foreach)
require(doMC)
require(arm)
require(gam)
require(earth)
require(randomForest)
require(nnet)


num.predictors <- ncol(subset(get(dataset),select = -c(get(outcome))))
outcome.name <- colnames(subset(get(dataset),select = c(get(outcome))))
predictor.names <- colnames(subset(get(dataset),select = -c(get(outcome))))
predictor.names2 <- colnames(subset(get(dataset),select = -c(get(outcome))))

x <- 1:(num.predictors*3)
dim(x) <- c(num.predictors,3)

SL.library <- c ("SL.gam", "SL.mean", "SL.randomForest","SL.nnet", "SL.bayesglm", "SL.earth", "SL.stepAIC")

fit1 <- SuperLearner(
Y = get(dataset)[,outcome.name],
X = get(dataset)[,predictor.names],
SL.library = SL.library,
family = gaussian(),
verbose = F,
cvControl = SuperLearner.CV.control(V = 10, stratifyCV = F, shuffle = T, validRows = NULL)
)
 
 
for (i in (1:num.predictors))
{
predictor.names2[i] -> x[i,1]
}

 
for (i in (1:num.predictors))
{
 
Ypred <- predict(fit1)$pred
test0 <- within( get(dataset), assign(predictor.names[i], 0))
test1 <- within( get(dataset), assign(predictor.names[i], 1))
Q0 <- predict(fit1, newdata=test0)
Q1 <- predict(fit1, newdata=test1)
Q.diff <- (Q1$pred-Q0$pred)
 
round(mean(Q.diff),5) -> x[i,2]
}
 
print("", quote=F)
print("Initial effect estimation complete!", quote=F)
flush.console()




print("", quote=F);
print("Generating CI's", quote=F)
print("", quote=F)
flush.console()

registerDoMC(MC)
CIs <- foreach(j=1:reps, .combine=rbind) %dopar% 
{


haha1 <- 1:num.predictors

boot_dataset <- get(dataset)[sample(1:nrow(get(dataset)),replace=T),] 

SL_boot_fit <- SuperLearner(
Y = boot_dataset[,outcome.name],
X = boot_dataset[,predictor.names],
SL.library = SL.library,
family = gaussian(),
verbose = F,
cvControl = SuperLearner.CV.control(V = 10, stratifyCV = F, shuffle = T, validRows = NULL))


haha2 <- foreach (i=1:num.predictors, .combine=cbind) %do% 
{
Ypred <- predict(SL_boot_fit)$pred
test0 <- within(boot_dataset, assign(predictor.names[i], 0))
test1 <- within(boot_dataset, assign(predictor.names[i], 1))
Q0 <- predict(SL_boot_fit, newdata=test0)
Q1 <- predict(SL_boot_fit, newdata=test1)
Q.diff <- (Q1$pred-Q0$pred)

return(mean(Q.diff))
}

return(haha2)
}


for (i in (1:num.predictors))
{

CIs[,i] -> distribution

sd(distribution) -> sd.error
as.numeric(x[i,2]) -> beta

round((beta - (1.96*sd.error)),digits=5) -> lower
round((beta + (1.96*sd.error)),digits=5) -> upper

as.character(lower)->lower2
as.character(upper)->upper2

paste("(", lower2, ",", " ", upper2,")", sep = ""  ) -> x[i,3]

}

colnames(x) <- c("Predictor", "Effect Estimate", "95% CI")
return(x)

}


