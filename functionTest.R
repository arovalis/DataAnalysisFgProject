# Test

# R studio "shiny" 
# Interactive graphs


# install.packages("dplyr") # To install dplyr, uncomment this line
# install.packages("tidyverse") # To install Tidyverse, uncomment this line
# install.packages("readxl")
# install.packages("which")
library("dplyr") # Uncomment to load dplyr library.
library("readxl")      ## To read excel docs (use file.choose()
library("tidyverse")   ## Adds ggplot2 and other things
library("which")
#source("avgGraphBuilder.R")
#file.choose()
source("C:\\Users\\hphal\\Box\\Hall_Harrison_Plant_Pathology_Lab\\F. graminearum Tissue Specificity Assays\\Data Analysis\\Version 1\\ppGraphBuilder1.0.2.R")

ppGBHelp()

fgdata <- read_excel("C:\\Users\\hphal\\Box\\Hall_Harrison_Plant_Pathology_Lab\\F. graminearum Tissue Specificity Assays\\Data Analysis\\Alpha Code\\Data\\fullmergeDataset.xlsx")
fgdata

# getwd()
# assayNum <- "boopadoopa"
# csvFilename <- paste0(getwd(),"/",assayNum,"-CM-TA",".csv",sep="")
# csvFilename
##

## Best viewed at 2000x1000 pixels

tissueSpecPerc(fgdata,"assay 1","Root Mass")
tissueSpecPerc(fgdata,"assay 1","Leaf Mass")
tissueSpecPerc(fgdata,"assay 2","Root Mass")
tissueSpecPerc(fgdata,"assay 2","Leaf Mass")
tissueSpecPerc(fgdata,"assay 3","Root Mass")
tissueSpecPerc(fgdata,"assay 3","Leaf Mass")
tissueSpecPerc(fgdata,"assay 4","Root Mass")
tissueSpecPerc(fgdata,"assay 4","Leaf Mass")
tissueSpecPerc(fgdata,"assay 5","Root Mass")
tissueSpecPerc(fgdata,"assay 5","Leaf Mass")
tissueSpecPerc(fgdata,"assay 6","Root Mass")
tissueSpecPerc(fgdata,"assay 6","Leaf Mass")
### Raw Masses ###
source("C:\\Users\\hphal\\Box\\Hall_Harrison_Plant_Pathology_Lab\\F. graminearum Tissue Specificity Assays\\Data Analysis\\Version 1\\ppGraphBuilder1.0.2.R")
rootSpecRaw(fgdata,"assay 1","Root Mass")
rootSpecRaw(fgdata,"assay 2","Root Mass")
rootSpecRaw(fgdata,"assay 3","Root Mass")
rootSpecRaw(fgdata,"assay 4","Root Mass")
rootSpecRaw(fgdata,"assay 5","Root Mass")
rootSpecRaw(fgdata,"assay 6","Root Mass")

leafSpecRaw(fgdata,"assay 1","Leaf Mass")
leafSpecRaw(fgdata,"assay 2","Leaf Mass")
leafSpecRaw(fgdata,"assay 3","Leaf Mass")
leafSpecRaw(fgdata,"assay 4","Leaf Mass")
leafSpecRaw(fgdata,"assay 5","Leaf Mass")
leafSpecRaw(fgdata,"assay 6","Leaf Mass")
