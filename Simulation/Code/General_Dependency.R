#Installing Packages

#install.packages("ZIM") 
#install.packages("vctrs", type="binary") #dependency for dplyr
#install.packages("dplyr") 
#install.packages("splines") 
#install.packages("vars")
#install.packages("robCompositions")
#install.packages("xfun", type="binary")
#install.packages("data.table", type="binary")
#install.packages("tidyverse")
#install.packages("tscount")
#install.packages("parallel")
#install.packages("ggplot2")
#install.packages("VGAM")
#install.packages("here")
#install.packages("pscl")
#install.packags("miceadds)

#Loading Packages

library(ZIM) #ZIM 
library(splines) #apparently splines are needed for ZIM?
library(vars) #coda models
library(robCompositions) #coda models
library(tidyverse) #tidyverse (includes dplyr,ggplot2)
library(tscount) #models for count data; see dissertation 
library(parallel) #parallelisation
library(VGAM) #VGAMS
library(here) #for paths of plots
library(pscl) #alternative for zim models
library(miceadds) #for loading RData 
