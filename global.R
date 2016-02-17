## global.R ##

# LRP-specific code

# Load de-identified data
  scrub_sis <- read.csv("data/scrub_sis.csv")

# Make totals df
  totals <- data.frame(c("All", levels(scrub_sis$agency)))
  names(totals)[1] <- "agency"
  totals$total <- c(3826,372,740,1884,592,238) # manually assign totals to CMHs

##############################################################################

# Shared code

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