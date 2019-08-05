
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(mapproj)
library(googleVis)
library(devtools)
library(knitr)
library(jsonlite)
library(httpuv)
library(reshape)
library(reldist)
library(wesanderson)
library(ggthemes)
library(scales)
library(plotly)
library(hrbrthemes)




GHG <- read.csv(file = "./GHG_df.csv", stringsAsFactors = F)

# WorldData <- map_data('world')

# aspect_ratio <- 2.5
# height <- 7
# 
# theme_wsj(base_size = 12, color = "brown", base_family = "sans",
#           title_family = "mono")
