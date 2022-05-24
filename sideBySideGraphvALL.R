####################################
## Harrison Hall (he/him)         ##
## hphall2@illinois.edu           ##
## 971.409.3033                   ##
## -When in doubt, restart-       ##
####################################

library("dplyr") # Uncomment to load dplyr library.
library("readxl")      ## To read excel docs (use file.choose()
library("tidyverse")   ## Adds ggplot2 and other things
library("which")
library("esquisse")

############################ DATA IMPORT ######################################
dfDir <- paste0(getwd(),"/alldata.xlsx",sep="")
fgdata <- read_excel(dfDir)

### WHAT ASSAY IS THIS?
assayNum <- "all"

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
filename <- paste0(assayNum,"-side-by-side",".png",sep="")
csvFilename <- paste0(getwd(),"/",assayNum,"-sbs",".csv",sep="")

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
rootAvg <- aggregate(rootMass ~ isolate + treatment, fgdata, mean)
rootNoA <- rootAvg[!grepl("above",rootAvg$treatment),]
rootNoC <- ctraNoA[!grepl("control",ctraNoA$treatment),]
isolateRootAvg <- rootNoC
## Get Isolate Coleoptile Masses:
leafAvg <- aggregate(coleoptileMass ~ isolate + treatment, fgdata, mean)
leafNoB <- leafAvg[!grepl("below",leafAvg$treatment),]
leafNoC <- leafNoB[!grepl("control",leafNoB$treatment),]
isolateLeafAvg <- leafNoC
mergedRoots <- rbind(ControlRootAverage,isolateRootAvg)
mergedRoots <- data.frame(mergedRoots)
mergedRoots <- data.frame(isolate=mergedRoots[,1],treatment=mergedRoots[,2],mass=mergedRoots[,3]/ctrlRAValue*100)
widthmult <- count(mergedRoots)
widthmult
ChartWidth <- ((125*widthmult)+1000) 
ChartWidth <- ChartWidth[,1]
ChartHeight <- ((68*widthmult)+1000)
ChartHeight <- ChartHeight[,1]
DotSize <- ((widthmult*0.25)+5)
DotSize <- DotSize[,1]
mergedLeaves <- rbind(ControlLeafAverage,isolateLeafAvg)
mergedLeaves <- data.frame(mergedLeaves)
mergedLeaves <- data.frame(isolate=mergedLeaves[,1],treatment=mergedLeaves[,2],mass=mergedLeaves[,3]/ctrlLAValue*100)
NuData <- rbind(mergedRoots, mergedLeaves)

### BUILD THE PLOT


NuData %>%
  filter(mass > 0) %>%
  ggplot() +
  aes(x = isolate, y = mass, colour = treatment, shape = treatment) +
  ylim(0,200) + 
  geom_point(size = DotSize, alpha=.5) +
  #scale_color_hue(direction = 1) +
  labs(x = "Isolates", y = ylab, title = cTitle,
       subtitle = subTitle, fill = "Treatment", color = "") +
  scale_shape_manual(name = "Treatments & Masses", labels = c("Treatment Above/Leaf Mass", "Treatment Below/Root Mass","Controls"),values = c(18,15,19)) +
  scale_color_manual(name = "Treatments & Masses", labels = c("Treatment Above/Leaf Mass", "Treatment Below/Root Mass","Controls"),values = c("#009E73", "#D55E00","#888888")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        axis.ticks = element_line(colour = "black"),
        )

ggsave(filename,width=ChartWidth,height=ChartHeight,units="px",dpi=300,bg="#FFFEFE")
write.csv(NuData,csvFilename,row.names=FALSE)
    