
library("dplyr") # Uncomment to load dplyr library.
library("readxl")      ## To read excel docs (use file.choose()
library("tidyverse")   ## Adds ggplot2 and other things
library("which")

progDir <- paste0(getwd(),"/ppGraphBuilder1.0.2.R",sep="")
dfDir <- paste0(getwd(),"/data.xlsx",sep="")
fgdata <- read_excel(dfDir)
fgdata
source(progDir)

### WHAT ASSAY IS THIS?
assayNumber <- "assay 6"

## Generate Images and CSVs
tissueSpecPerc(fgdata,assayNumber,"Root Mass")
tissueSpecPerc(fgdata,assayNumber,"Leaf Mass")
rootSpecRaw(fgdata,assayNumber,"Root Mass")
leafSpecRaw(fgdata,assayNumber,"Leaf Mass")

