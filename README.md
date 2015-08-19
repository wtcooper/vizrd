# vizrd
Interactive visualizations for R data 

## Overview
A simple Shiny launcher package that provides summary statistics, a data table explorer, 
a heatmap plot of raw (scaled) data, some distribution based plots, and ability to download
the plots as .png.  Intended to provide a quick overview of a dataset and see how data
in each of a dataset's columns are distributed.  Will likely add some outlier detection
support in near term versus the crude current approach of plotting the data by standard
deviations from the mean.  <br />

## Installation
  > library(devtools)  <br />
  > devtools::install_github("wtcooper/vizrd")  <br />
  
## Use
Call the explore_data() function with a character vector of dataframe names in the current environment, 
or call explore_all_data() which will allow you to choose any data.frame/data.table/tbl_df once the Shiny
app launches. 

  > data(iris)  <br />
  > data(mtcars)  <br />
  > explore_data(c("iris","mtcars"))  <br />
