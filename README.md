# vizrd
Visualizations for R data and models 

## Overview
Includes a number of static ggplot2 plots (plot....) and a few interactive ggivs plots (iplot....), 
along with a simple Shiny launcher package that provides summary statistics, a data table explorer, 
a heatmap plot of raw (scaled) data, some distribution based plots, and ability to download
the plots as .png.  Intended to provide a quick overview of a dataset and see how data
in each of a dataset's columns are distributed.  Also includes plots for exploring regression and classification
model fits, both performance metrics and a generalized partial effect plot function for any model type.  <br />

## Installation

```R
library(devtools) 

## Remove old version if pre-installed
if ("package:vizrd" %in% search()) { detach("package:vizrd", unload=TRUE) }
if ("vizrd" %in% rownames(installed.packages())) { remove.packages("vizrd") }

## Install most current version
devtools::install_github("wtcooper/vizrd")
```

## Data Exploration
### shiny 
Call the explore_data() function with a character vector of dataframe names in the current environment, 
or call explore_all_data() which will allow you to choose any data.frame/data.table/tbl_df once the Shiny
app launches. 

```R
data(iris)  
data(mtcars)  
explore_data(c("iris","mtcars"))  
```

### ggplot

```R
#### Explore Data #### 

## Static ggplot plots
plotDataHeatmap(iris, colNms=names(iris)[1:4])
plotDataHist(iris, colNm="Sepal.Length", binSize=.1)                       # All species
plotDataHist(iris, colNm="Sepal.Length", binSize=.1, byCol="Species")      # By species
plotDataPoints(iris, colNms=names(iris)[1:4])                              # All variables
plotDataPoints(iris, colNms=names(iris)[1], byCol="Species")               # Single column, by species
plotDataDist(iris, colNms=names(iris)[1:4])								# Distribution plot (violin + box plot)                              
plotDataDist(iris, colNms=names(iris)[1], byCol="Species")               
plotDataDist(iris, colNms=names(iris)[1:4], byCol="Species")     # facet columns with by group          

## Save all data points to a pdf
plotPointsToPDF(iris, colNms=names(iris)[1:4], totPerPage=4, pdffile="plots.pdf")
plotDistToPDF(iris, colNms=names(iris)[1:4], byCol="Species",  totPerPage=4, pdffile="plots.pdf")

```

## Model Performance and Diagnostics 

```R
#### Regression plots #### 
irisReg = iris %>% select(-Species)
trainIdx = sample(1:dim(irisReg)[1], floor(.7*dim(irisReg)[1]), replace=FALSE)
trainDat = irisReg[trainIdx,] 
testDat = irisReg[-trainIdx,]

# fit the model
modReg = lm(Sepal.Length~., data=trainDat)
predReg = data.frame(obs=testDat$Sepal.Length, pred=predict(modReg, newdata=testDat))

# make the plots
plotResids(predReg$obs, predReg$pred)
plotObsPred(predReg$obs, predReg$pred)
plotQQNorm(predReg$obs, predReg$pred)




#### Classification plots #### 
irisBin = iris %>% 
		filter(Species != "setosa") %>% 
		mutate(Species = ifelse(Species=="versicolor",0,1))

# Add some noise
swapIdx = sample(1:dim(irisBin)[1],5)
swapFnx = function(x) ifelse(x==0,1,0)
irisBin[swapIdx,"Species"]=swapFnx(irisBin[swapIdx,"Species"])

trainIdx = sample(1:dim(irisBin)[1], floor(.7*dim(irisBin)[1]), replace=FALSE)
trainDat = irisBin[trainIdx,] 
testDat = irisBin[-trainIdx,]

# fit the model
modBin = glm(Species~., data=irisBin, family=binomial)
predBin = data.frame(obs=testDat$Species, prob=predict(modBin, newdata=testDat, type="response"))
predBin = predBin %>% mutate(pred = round(prob,0)) %>% 
		mutate(obs=ifelse(obs==0,"versicolor","virginica"),
				pred=ifelse(pred==0,"versicolor","virginica"))

# make the plots
plotCM(predBin$pred, predBin$obs)
plotCMSeq(predBin$prob, predBin$obs, posLabel="virginica", negLabel="versicolor")
plotCMProbs(predBin$prob, predBin$obs, posLabel="virginica", negLabel="versicolor")
plotTPFPProbs(predBin$prob, predBin$obs, posLabel="virginica", negLabel="versicolor")
plotROC(predBin$prob, predBin$obs)
iplotROC(predBin$prob, predBin$obs)  #interactive ggvis version, more for playing with ggvis
plotLift(predBin$prob, predBin$obs, posLabel="virginica", negLabel="versicolor")
```


## Partial Effects  


### General Partial Effects 
Provides ability to construct  general partial effects plots for any model type, just need to provide a custom prediction function (predFnx=). 
Will do similar calculation to randomForest::partialEffect() by setting type="all", or as in the plotmo package by 
using type="median".  Note: if you're interested in partial effects for random forests, you should use the forestFloor 
package which provides excellent functionality.

```R
### Using caret ####

library(caret)
data(iris)

train_control <- trainControl(method="cv", number=10)
model <- train(Species~., data=iris, trControl=train_control, method="glmnet")

predFnx <- function(mod, newdata) predict(mod, newdata=newdata, type="prob")

plotPartialEffs(model, iris, predFnx, colNms=names(iris)[1:4], type="all") 



### As above but with factor variable ####

irisFac = iris
irisFac$fac = as.factor(cut(irisFac$Sepal.Length, 3, labels = FALSE))

modelFac <- train(Species~., data=irisFac, trControl=train_control, method="glmnet")

plotPartialEffs(modelFac, irisFac, predFnx, colNms=names(irisFac)[c(1:4, 6)], 
	type="all", totPerPage=15) 



### GAM with CIs ###

library(mgcv)
dat <- gamSim(5,n=200,scale=2)
gam.mod <- gam(y ~ x0 + x1 + s(x1) + s(I(x1^2)) + s(x2) + offset(x3) , data = dat)

# Create a prediction function that returns a data frame of predictions
# If want to plot CI/SE's, need to have xxx_lower and xxx_higher for each xxx named
#   prediction
gamPredFnx <- function(mod, newdata) {
	preds = data.frame(pred=as.vector(predict.gam(mod, newdata=newdata, type="response", se.fit=T)))
	preds$pred=as.vector(preds$pred.fit)
	preds$pred_lower = as.vector(preds$pred.fit-preds$pred.se.fit)
	preds$pred_upper = as.vector(preds$pred.fit+preds$pred.se.fit)
	preds[,c("pred", "pred_lower", "pred_upper")]
	
}

# Plot at median value for numeric and mode value for factor/character's
# I.e., hold covariates constant and get predictions along range of variable
plotPartialEffs(gam.mod, dat, gamPredFnx, colNms=names(dat)[2:5], CIOn=T, 
	type="median", totPerPage=9, pdffile=NULL) 

# For each value of predictor, calculate prediction as mean of predictions from all data points at that given predictor value
# I.e., for all observed covariates value combinations, get predictions along range of variable
plotPartialEffs(gam.mod, dat, gamPredFnx, colNms=names(dat)[2:5], CIOn=T, 
	type="all", totPerPage=9, pdffile=NULL) 

## Note: if have polynomial/interaction terms and want seperate effect 
##  for each (e.g., as in plot.gam()) then create model with seperate variables
## Do all variables seperate 
dat2 = dat
dat2$x1Poly = dat2$x1*dat2$x1; dat2$x1Lin = dat2$x1
gam.mod2 = gam(y ~ x0 + x1Lin + s(x1) + s(x1Poly) + s(x2) + offset(x3) , data = dat2)

plot.gam(gam.mod2, pages=1, all.terms=T, se=1)
plotPartialEffs(gam.mod2, dat2, gamPredFnx, colNms=names(dat2)[c(2,3,4,6,7)], CIOn=T, 
	type="median", totPerPage=9, pdffile=NULL) 




### Random Forest ###
require(randomForest)

## Classification 
data(iris)
iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE, proximity=TRUE)

rfPredFnx <- function(mod, newdata) {
	preds = as.data.frame(predict(mod, newdata=newdata, type="prob"))
}

plotPartialEffs(mod=iris.rf, dat=iris, predFnx=rfPredFnx, colNms=names(iris)[1:4], 
	type="median", totPerPage=9, pdffile=NULL) 
plotPartialEffs(mod=iris.rf, dat=iris, predFnx=rfPredFnx, colNms=names(iris)[1:4], 
	type="all", totPerPage=9, pdffile=NULL) 

# output pdf
plotPartialEffs(mod=iris.rf, dat=iris, predFnx=rfPredFnx, colNms=names(iris)[1:4], 
	type="median", totPerPage=9, pdffile="iriplots.pdf") 


## Regression 
library(randomForest)
a <- runif(5000, 1, 100)
b <- runif(5000, 1, 100)
c <- (1:5000)/50 + rnorm(100, mean = 0, sd = 0.1)
y <- (1:5000)/50 + rnorm(100, mean = 0, sd = 0.1)
Data <- data.frame(matrix(c(y, a, b, c), ncol = 4))
names(Data) <- c("y", "a", "b", "c")

rf.model <- randomForest(y ~ a + b + c, data = Data[sample(5000,100),],nodesize=5,ntress=2000)

rfPredFnx <- function(mod, newdata) {
	preds = data.frame(pred=predict(mod, newdata=newdata))
}

plotPartialEffs(rf.model, Data, rfPredFnx, colNms=c("a", "b", "c"), 
	type="median", totPerPage=9, pdffile=NULL) 

```

### GAM Specific
This is currently less flexible than plot.gam() -- it only does splines and not all.terms=T, but it 
was built for a report with nicer than base graphics so stuck in here.

```R

library(mgcv)
dat <- gamSim(5,n=200,scale=2)
mod <- gam(y ~ x1+ + s(x1) + s(I(x1^2)) + s(x2) + offset(x3) , data = dat)

plotGAMSplines(mod)
plotGAMSplines(mod, rug=TRUE, residuals=TRUE)   # add rug to x-axis and residuals 
```




