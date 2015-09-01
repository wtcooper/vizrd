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
  > library(devtools)  <br />
  > devtools::install_github("wtcooper/vizrd")  <br />
  
## Shiny Use
Call the explore_data() function with a character vector of dataframe names in the current environment, 
or call explore_all_data() which will allow you to choose any data.frame/data.table/tbl_df once the Shiny
app launches. 

  > data(iris)  <br />
  > data(mtcars)  <br />
  > explore_data(c("iris","mtcars"))  <br />


## Other Plotting Functions Use
See the functions in PlotsExplore.r and PlotsModel.r for available plots.  Includes plots for exploring the data
(heatmap, raw data points, histogram distributions), classification model evaluation (confusion matrices, ROC),
and regression models (residuals, qqplot, observed vs predicted).  Many of these are standard plots I use on a daily
basis worked up in ggplot2. 
