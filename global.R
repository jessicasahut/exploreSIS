## global.R ##

# Load fun, data, libs, source files
  library(shinydashboard)
  library(shinythemes)
  library(DT)
  library(ggplot2)
  library(rcdimple)
  library(dygraphs)
  library(parsetR)
  library(dplyr)
  library(magrittr)
  library(tidyr)
  library(plotly)
  library(xts)
  library(explodingboxplotR)

# Define begin & due dates
  begin <- as.Date("2014/07/01")
  due <- as.Date("2017/9/30")

# Load de-identified data
  scrub_sis <- read.csv("data/scrub_sis.csv")
  
# Load totals
  totals <- read.csv("data/totals.csv")
  