# vizrd
Visualizations for R data and models 

## Overview
Includes a number of static ggplot2 plots (splot....) and a few interactive ggivs plots (iplot....), 
along with a simple Shiny launcher package that provides summary statistics, a data table explorer, 
a heatmap plot of raw (scaled) data, some distribution based plots, and ability to download
the plots as .png.  Intended to provide a quick overview of a dataset and see how data
in each of a dataset's columns are distributed.  Will likely add some outlier detection
support in near term versus the crude current approach of plotting the data by standard
deviations from the mean.  <br />

## Installation

```R
library(devtools) 
devtools::install_github("wtcooper/vizrd")
```


## Shiny Use
Call the explore_data() function with a character vector of dataframe names in the current environment, 
or call explore_all_data() which will allow you to choose any data.frame/data.table/tbl_df once the Shiny
app launches. 

```R
data(iris)  
data(mtcars)  
explore_data(c("iris","mtcars"))  
```

## Other Plotting Functions 
See the functions in PlotsExplore.r and PlotsModel.r for available plots.  Includes plots for exploring the data
(heatmap, raw data points, histogram distributions), classification model evaluation (confusion matrices, ROC),
and regression models (residuals, qqplot, observed vs predicted).  Many of these are standard plots I use frequently but worked up in ggplot2.  Right now I only have plots for binomial targets, but will eventually put in multinomial plots too where appropriate.

```R
#### Explore Data #### 

## Shiny app
explore_data("iris")


## Static plots
splotDataHeatmap(iris, colNms=names(iris)[1:4])
splotDataHist(iris, colNm="Sepal.Length", binSize=.1)
splotDataPoints(iris, colNms=names(iris)[1:4])

## Save all data to a pdf
splotPointsToPDF(iris, colNms=names(iris)[1:4], totPerPage=4, pdffile="plots.pdf")


#### Regression plots #### 
irisReg = iris %>% select(-Species)
trainIdx = sample(1:dim(irisReg)[1], floor(.7*dim(irisReg)[1]), replace=FALSE)
trainDat = irisReg[trainIdx,] 
testDat = irisReg[-trainIdx,]

# fit the model
modReg = lm(Sepal.Length~., data=trainDat)
predReg = data.frame(obs=testDat$Sepal.Length, pred=predict(modReg, newdata=testDat))

# make the plots
splotResids(predReg$obs, predReg$pred)
splotObsPred(predReg$obs, predReg$pred)
splotQQNorm(predReg$obs, predReg$pred)




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
splotCM(predBin$pred, predBin$obs)
splotCMSeq(predBin$prob, predBin$obs, posLabel="virginica", negLabel="versicolor")
splotCMProbs(predBin$prob, predBin$obs, posLabel="virginica", negLabel="versicolor")
splotTPFPProbs(predBin$prob, predBin$obs, posLabel="virginica", negLabel="versicolor")
splotROC(predBin$prob, predBin$obs)
iplotROC(predBin$prob, predBin$obs)  #interactive ggvis version, more for playing with ggvis
splotLift(predBin$prob, predBin$obs, posLabel="virginica", negLabel="versicolor")
```

