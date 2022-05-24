####################################
## Harrison Hall (he/him)         ##
## hphall2@illinois.edu           ##
## 971.409.3033                   ##
## -When in doubt, restart-       ##
####################################

################################################################################
# This will be to generate the Side-By-Side Box and Whiskers
################################################################################

library("dplyr") # Uncomment to load dplyr library.
library("readxl")      ## To read excel docs (use file.choose()
library("tidyverse")   ## Adds ggplot2 and other things
library("which")
library("esquisse")

############################ DATA IMPORT ######################################
dfDir <- paste0(getwd(),"/data.xlsx",sep="")
fgdata <- read_excel(dfDir)
fgdata
### WHAT ASSAY IS THIS?
assayNum <- "Experiment 1"

###############################################################################
assayCheck <- which(fgdata == assayNum,arr.ind=TRUE)  ##Check the assay, grabs first row with this assay number
rowCheck <- paste(assayCheck[1,1]) ## Checks first row of this assay, should be control
seed <- paste(fgdata[rowCheck,1]) ## Checks seed of control
soil <- paste(fgdata[rowCheck,10]) ## Checks soil of control
fert <- paste(fgdata[rowCheck,11]) ## Checks fertilizer of control
inoc <- paste(fgdata[rowCheck,12]) ## Checks inoculation type for this assay
DAI <- paste(fgdata[rowCheck,13]) ## Checks the Days after Inoculation of the harvest
speciesName <- paste(fgdata[rowCheck,14]) ## Grab species name for tag
inocAndExpName <- substitute(paste(italic(speciesName), ' tissue specificity assays')) ## Not actually used for this chart

ylab = "Tissue Masses as a Percent of Control"
subTitle = "Treatment Above/Coleoptile Mass vs Treatment Below/Root Mass"
subTitle <- paste(assayNum,seed,soil,fert,inoc, sep = ", ")   ## Build subtitle for chart
cTitle <- paste(assayNum,": Tissue Masses Relative to Control",sep="")
filename <- paste0(assayNum,"-bw-sbs-normalized",".png",sep="")
csvFilename <- paste0(getwd(),"/",assayNum,"bw-sbs-normalized",".csv",sep="")

## Get Root Mass Control:
ctrlRootAvg <- aggregate(rootMass ~ isolate + treatment, fgdata, mean)
ctraNoA <- ctrlRootAvg[!grepl("above",ctrlRootAvg$treatment),]
ctraNoB <- ctraNoA[!grepl("below",ctraNoA$treatment),]
ControlRootAverage <- ctraNoB
ctrlRAValue <- ControlRootAverage[,3]
## Get Coleoptile Mass Control:
ctrlLeafAvg <- aggregate(coleoptileMass ~ isolate + treatment, fgdata, mean)
ctlaNoA <- ctrlLeafAvg[!grepl("above",ctrlLeafAvg$treatment),]
ctlaNoB <- ctlaNoA[!grepl("below",ctlaNoA$treatment),]
ControlLeafAverage <- ctlaNoB
ctrlLAValue <- ControlLeafAverage[,3]
############################################################################
## Get Isolate Root Masses:

rootAvg <- data.frame(isolate=fgdata$isolate,treatment=fgdata$treatment,rootMass=fgdata$rootMass)
rootNoA <- rootAvg[!grepl("above",rootAvg$treatment),]
rootNoC <- rootNoA#[!grepl("control",rootNoA$treatment),]
isolateRootNormalized <- rootNoC

## Get Isolate Coleoptile Masses:
leafAvg <- data.frame(isolate=fgdata$isolate,treatment=fgdata$treatment,coleoptileMass=fgdata$coleoptileMass)
leafNoA <- leafAvg[!grepl("below",leafAvg$treatment),]
leafNoC <- leafNoA#[!grepl("control",leafNoA$treatment),]
isolateLeafNormalized <- leafNoC

####### MASH DATA TOGETHER #########################
mergedRoots <- rbind(ControlRootAverage,isolateRootNormalized)
mergedRoots <- data.frame(mergedRoots)
mergedRoots <- data.frame(isolate=mergedRoots[,1],treatment=mergedRoots[,2],mass=mergedRoots[,3]/ctrlRAValue*100)

mergedLeaves <- rbind(ControlLeafAverage,isolateLeafNormalized)
mergedLeaves <- data.frame(mergedLeaves)
mergedLeaves <- data.frame(isolate=mergedLeaves[,1],treatment=mergedLeaves[,2],mass=mergedLeaves[,3]/ctrlLAValue*100)
NuData <- rbind(mergedRoots, mergedLeaves)
NuData[NuData==0] <- NA
NuData


widthmult <- count(mergedRoots)
widthmult
ChartWidth <- ((125*widthmult)+1000) 
ChartWidth <- ChartWidth[,1]
ChartHeight <- ((68*widthmult)+1000)
ChartHeight <- ChartHeight[,1]
DotSize <- ((widthmult*0.25)+5)
DotSize <- DotSize[,1]
fontSize <- widthmult*0.44

### BUILD THE PLOT

ggplot(NuData) +
  aes(x = isolate, y = mass, fill = treatment, drop=TRUE) +
  ylim(0,200) +
  geom_boxplot() +
  labs(x = "Isolates", y = ylab, title = cTitle,
       subtitle = subTitle, fill = "Treatment", color = "") +
  scale_fill_manual(name = " ", labels = c("Treatment Above/Leaf Mass", "Treatment Below/Root Mass","Controls"),
    values = c(above = "#DDDDDD",
               below = "#666666",
               control = "#000000")
  ) +
  theme_minimal() +
  theme(legend.position="top", 
        text = element_text(size = fontSize),
        axis.text.x = element_text(angle = 45))

ggsave(filename,width=ChartWidth,height=ChartHeight,units="px",dpi=300,bg="#FFFEFE")
write.csv(NuData,csvFilename,row.names=FALSE)
    