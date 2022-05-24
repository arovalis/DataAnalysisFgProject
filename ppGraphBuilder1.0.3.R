####################################
## Harrison Hall (he/him)         ##
## hphall2@illinois.edu           ##
## 971.409.3033                   ##
## -When in doubt, restart-       ##
####################################

## BETA VERSION 1.0.3
## Changes:
## Now has Side-By-Side

############################ LIBRARIES #########################################
 # install.packages("dplyr") # To install dplyr, uncomment this line
 # install.packages("tidyverse") # To install Tidyverse, uncomment this line
 # install.packages("readxl")
 # install.packages("which")
 # library("dplyr") # Uncomment to load dplyr library.
 # library("readxl")      ## To read excel docs (use file.choose()
 # library("tidyverse")   ## Adds ggplot2 and other things

############################### HELP ###########################################
ppGBHelp <- function(){    ## Put help and contact info in here
  h1 <- paste("ppGraphBuilder was made by Harrison Hall, hphall2@illinois.edu, or harrisonpiercehall@gmail.com")
  h2<-paste("Available Commands are:")
  h3a<-paste("rootSpecRaw(data_file = , assay_number = , measurement = )")
  h3b<-paste("leafSpecRaw(data_file = , assay_number = , measurement = )")
  h4<-paste("Returns box&whisker plots of the raw data, respective masses")
  h5<-paste("tissueSpecPerc(data_file = , assay_number = , measurement = )")
  h6<-paste("Returns mass as a scatter plot in relation to the control mass")
  h7<-paste("Available arguments for measurements are: 'Leaf Mass' and 'Root Mass'")
  h8<-paste("data files must be excel format with a proper layout to be read")
  h9<-paste("Columns should be in this order:")
  h10<-paste("seedType, assay, isolate, treatment, block, rootMass, coleoptileMass, totalMass, germinated, soilType, fertilizer, inoculumMethod, DAI, inocSpecies")
  help = paste(h1,h2,h3a,h3b,h4,h5,h6,h7,h8,h9,h10,sep="\n\n") ## It combines and separates!
  cat(help)  ## Go Kingfishers
}

################################################################################
################################## RAW FUNC ####################################
################################################################################
rootSpecRaw <- function(data_file, assay_number, measurement)
  {
############################ DATA IMPORT #######################################
fgdata <- data_file
assayNum <- assay_number
measurement <- measurement
############################## BUILD TITLES ####################################
assayCheck <- which(fgdata == assayNum,arr.ind=TRUE)   ## This will only get data in relation to the Assay
rowCheck <- paste(assayCheck[1,1]) ## Grabs first row
seed <- paste(fgdata[rowCheck,1])  ## Should be seed used in control
soil <- paste(fgdata[rowCheck,10]) ## Should be soil used in control
fert <- paste(fgdata[rowCheck,11]) ## Should be fertilizer used in control
inoc <- paste(fgdata[rowCheck,12]) ## Should be inoculum type used in this assay
DAI <- paste(fgdata[rowCheck,13])  ## Should be days after inoculation this assay was harvested
## Now to build tag at bottom
speciesName <- paste(fgdata[rowCheck,14]) ## Grab species name  
inocAndExpName <- substitute(paste(italic(speciesName), ' tissue specificity assays')) ## Make tag
############################## ANALYSIS ########################################
if (measurement == "Root Mass"){  ## Changes a few variables based on what is being measured
  chartColor = "Oranges" ## Shades of Orange
  isTreatment = "below" ## Get this data
  notTreatment = "above" ## Do NOT get this data
  massType = "rootMass" ## grab this mass type
  ##Labels:  
  ylab = "Root Mass in Grams"
  cTitle = "Root Masses vs Control - Inoculum Below Seed"
  filename <- paste0(assayNum,"-RM-TB-RawBox",".png",sep="")

}
if (measurement == "Leaf Mass"){
  chartColor = "Greens" ## Shades of GREEN
  isTreatment = "above" ## Get this data
  notTreatment = "below" ## Do NOT get this data
  massType = "coleoptileMass" ## grab this mass type
  ##Labels:
  ylab = "Leaf Mass in Grams"
  cTitle = "Leaf Masses vs Control - Inoculum Above Seed"
  filename <- paste0(assayNum,"-RM-TA-RawBox-AreYouSure",".png",sep="")

  cat("Are you sure you didn't meant to use leafSpecRaw()?")
}
## Build subtitle
subTitle <- paste(assayNum,seed,soil,fert,inoc, sep = ", ")  ## Build subtitle based on all of the info provided
## Build the graph:
fgdata %>%
  filter(seedType %in% seed) %>% 
  filter(assay %in% assayNum) %>% 
  filter(!(treatment %in%  
             notTreatment)) %>% 
  #filter(!(massType %in% massType)) %>%
  filter(germinated %in% "Yes" | is.na(germinated)) %>%
  ggplot() + 
  # aes(x = isolate, y = massType, fill = treatment) +
  aes(x = isolate, y = rootMass, fill = treatment) + 
  geom_boxplot() +
  #ylim(0,150) +
  scale_fill_brewer(palette = chartColor, 
                    direction = 1) + 
  labs(x = "Isolates", y = ylab, title = cTitle, 
       subtitle = subTitle, caption = inocAndExpName) + 
  theme_minimal()

ggsave(filename,width=4000,height=2000,units="px",dpi=300,bg="white")
## End of function
}

leafSpecRaw <- function(data_file, assay_number, measurement)
{
  ############################ DATA IMPORT #######################################
  fgdata <- data_file
  assayNum <- assay_number
  measurement <- measurement
  ############################## BUILD TITLES ####################################
  assayCheck <- which(fgdata == assayNum,arr.ind=TRUE)   ## This will only get data in relation to the Assay
  rowCheck <- paste(assayCheck[1,1]) ## Grabs first row
  seed <- paste(fgdata[rowCheck,1])  ## Should be seed used in control
  soil <- paste(fgdata[rowCheck,10]) ## Should be soil used in control
  fert <- paste(fgdata[rowCheck,11]) ## Should be fertilizer used in control
  inoc <- paste(fgdata[rowCheck,12]) ## Should be inoculum type used in this assay
  DAI <- paste(fgdata[rowCheck,13])  ## Should be days after inoculation this assay was harvested
  ## Now to build tag at bottom
  speciesName <- paste(fgdata[rowCheck,14]) ## Grab species name  
  inocAndExpName <- substitute(paste(italic(speciesName), ' tissue specificity assays')) ## Make tag
  ############################## ANALYSIS ########################################
  if (measurement == "Root Mass"){  ## Changes a few variables based on what is being measured
    chartColor = "Oranges" ## Shades of Orange
    isTreatment = "below" ## Get this data
    notTreatment = "above" ## Do NOT get this data
    cat("Are you sure you didn't mean to use rootSpecRaw()?")
    ##Labels:  
    ylab = "Root Mass in Grams"
    cTitle = "Root Masses vs Control - Inoculum Below Seed"
    filename <- paste0(assayNum,"-CM-TB-RawBox-AreYouSure",".png",sep="")

  }
  if (measurement == "Leaf Mass"){
    chartColor = "Greens" ## Shades of GREEN
    isTreatment = "above" ## Get this data
    notTreatment = "below" ## Do NOT get this data
    massType = "coleoptileMass" ## grab this mass type
    ##Labels:
    ylab = "Leaf Mass in Grams"
    cTitle = "Leaf Masses vs Control - Inoculum Above Seed"
    filename <- paste0(assayNum,"-CM-TA-RawBox",".png",sep="")

  }
  ## Build subtitle
  subTitle <- paste(assayNum,seed,soil,fert,inoc, sep = ", ")  ## Build subtitle based on all of the info provided
  ## Build the graph:
  fgdata %>%
    filter(seedType %in% seed) %>% 
    filter(assay %in% assayNum) %>% 
    filter(!(treatment %in%  
               notTreatment)) %>% 
    #filter(!(massType %in% massType)) %>%
    filter(germinated %in% "Yes" | is.na(germinated)) %>%
    ggplot() + 
    # aes(x = isolate, y = massType, fill = treatment) +
    aes(x = isolate, y = coleoptileMass, fill = treatment) + 
    geom_boxplot() +
    scale_fill_brewer(palette = chartColor, 
                      direction = 1) + 
    labs(x = "Isolates", y = ylab, title = cTitle, 
         subtitle = subTitle, caption = inocAndExpName) + 
    theme_minimal()
ggsave(filename,width=4000,height=2000,units="px",dpi=300,bg="white")
  ## End of function
}
###############################################################################
############################## PERCENT FUNC ###################################
###############################################################################
tissueSpecPerc <- function(data_file, assay_number, measurement){
  ############################ DATA IMPORT #######################################
  fgdata <- data_file
  assayNum <- assay_number
  measurement <- measurement
  ############################## BUILD TITLES ####################################
  assayCheck <- which(fgdata == assayNum,arr.ind=TRUE)  ##Check the assay, grabs first row with this assay number
  rowCheck <- paste(assayCheck[1,1]) ## Checks first row of this assay, should be control
  seed <- paste(fgdata[rowCheck,1]) ## Checks seed of control
  soil <- paste(fgdata[rowCheck,10]) ## Checks soil of control
  fert <- paste(fgdata[rowCheck,11]) ## Checks fertilizer of control
  inoc <- paste(fgdata[rowCheck,12]) ## Checks inoculation type for this assay
  DAI <- paste(fgdata[rowCheck,13]) ## Checks the Days after Inoculation of the harvest
  ##
  speciesName <- paste(fgdata[rowCheck,14]) ## Grab species name for tag
  inocAndExpName <- substitute(paste(italic(speciesName), ' tissue specificity assays')) ## Not actually used for this chart
    ############################## ANALYSIS ########################################
if (measurement == "Root Mass"){ ## 
    chartColor = "Oranges" ## Color Orange
    isTreatment = "below" ## This treatment
    notTreatment = "above" ## NOT this one
    ##Chart Title Tweaks:
    ylab = "Root Mass as a Percent of Control"
    cTitle = "Root Masses in Relation to Control - Inoculum Below Seed"
    subTitle <- paste(assayNum,seed,soil,fert,inoc, sep = ", ")   ## Build subtitle for chart
    filename <- paste0(assayNum,"-RM-TB",".png",sep="")
    csvFilename <- paste0(getwd(),"/",assayNum,"-RM-TB",".csv",sep="")
    ### BUILD CONTROLS ###
    # Control has to be measured apart from the isolates, otherwise you can end up with a control of 0, which messes stuff up later.
    controlAvg <- xtabs(rootMass ~ isolate + assay, (aggregate(rootMass ~ isolate + assay, fgdata, mean)))  ## Grabs rootMass 
    ctrlChart <- data.frame(controlAvg) ## averages of the controls
    controlAvg <-which(ctrlChart == assayNum,arr.ind=TRUE) ## Build these control chart
    controlAvg <- paste(controlAvg[1,1]) ## Paste inside chart
    controlMass <- ctrlChart[controlAvg,3] ## Check column 3
    ############################## ANALYSIS ########################################
    # This builds a new data frame that compares masses to chosen control average
    isoAverage <- xtabs(rootMass ~ isolate + assay + treatment, (aggregate(rootMass ~ isolate + assay + treatment, fgdata, mean))) ## Now build the actual graph of data to compare to control
    nuChart <- data.frame(isoAverage) ##Grabs everything and puts into new data frame
    percentCont <- c(nuChart[,4]/controlMass)*100 ## Divides all root mass averages by the control average
    nuData <- data.frame(nuChart[,1],nuChart[,2],nuChart[,3],percentCont) ## Makes new data frame
}

if (measurement == "Leaf Mass"){
    chartColor = "Greens" ## Color Greens
    isTreatment = "above" ## Treatment above
    notTreatment = "below" ## But NOT these ones
    ##Chart Title Tweaks:
    ylab = "Leaf Mass as a Percent of Control"
    cTitle = "Leaf Masses in Relation to Control - Inoculum Above Seed"
    subTitle <- paste(assayNum,seed,soil,fert,inoc, sep = ", ")
    filename <- paste0(assayNum,"-CM-TA",".png",sep="")
    csvFilename <- paste0(getwd(),"/",assayNum,"-CM-TA",".csv",sep="")
    ### BUILD CONTROLS ###
    # Control has to be measured apart from the isolates, otherwise you can end up with a control of 0, which messes stuff up later.
    controlAvg <- xtabs(coleoptileMass ~ isolate + assay, (aggregate(coleoptileMass ~ isolate + assay, fgdata, mean)))
    ctrlChart <- data.frame(controlAvg)
    controlAvg <-which(ctrlChart == assayNum,arr.ind=TRUE)     #### Build coleoptile Mass Averages
    controlAvg <- paste(controlAvg[1,1])
    controlMass <- ctrlChart[controlAvg,3]
    ############################## ANALYSIS ########################################
    # This builds a new data frame that compares masses to chosen control average
    isoAverage <- xtabs(coleoptileMass ~ isolate + assay + treatment, (aggregate(coleoptileMass ~ isolate + assay + treatment, fgdata, mean)))  ## Grab the coleoptile Mass
    nuChart <- data.frame(isoAverage) ## put into data frame
    percentCont <- c(nuChart[,4]/controlMass)*100 #Divide all by the control mass
    nuData <- data.frame(nuChart[,1],nuChart[,2],nuChart[,3],percentCont) ## Build new data frame
}
  nuData %>% ##### BUILD THE CHART
    filter(nuChart...2. %in% assayNum) %>%  #Only the chosen assay
    filter(!(nuChart...3. %in% isTreatment)) %>%   #ignores the treatments we're not looking at
    filter(percentCont >
             0L & percentCont <= 300L) %>%   #### Because greater than 0 must be shown, it will eliminate the 0.000 controls from the treatment group, and elminate the 0.000 isolates from the control groups.
    ggplot() +                                #### Just in case someone in the future is confused about why this worked 'magically'
    aes(
      x = nuChart...1.,    ## Isolates
      y = percentCont, ## Percent compared to control
      colour = nuChart...1., ## Color ring by isolates
      fill = nuChart...3. ## Color Fill by Treatment type
    ) +
    ylim(0,200) +
    geom_point(shape = "circle filled", size = 10L) +
    scale_fill_brewer(palette = chartColor, direction = 1) +
    scale_color_hue(direction = 1) +
    labs(x = "Isolates", y = ylab, title = cTitle,
         subtitle = subTitle, fill = "Treatment", color = "Isolate") +
    theme_minimal()
ggsave(filename,width=4000,height=2000,units="px",dpi=300,bg="white")
write.csv(nuData,csvFilename,row.names=FALSE)
}


###############################################################################
############################ SIDE-BY-SIDE FUNC ################################
###############################################################################

sideBySide <-function(dataframe,experiment_num){
  ############################ DATA IMPORT ######################################
  fgdata <- dataframe
  
  ### WHAT ASSAY IS THIS?
  assayNum <- experiment_num
  
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
  ctraNoA <- ctrlRootAvg[!grepl("above",testAvg$treatment),]
  ctraNoB <- ctraNoA[!grepl("below",ctraNoA$treatment),]
  ControlRootAverage <- ctraNoB
  ctrlRAValue <- ControlRootAverage[,3]
  ## Get Coleoptile Mass Control:
  ctrlLeafAvg <- aggregate(coleoptileMass ~ isolate + treatment, fgdata, mean)
  ctlaNoA <- ctrlLeafAvg[!grepl("above",testAvg$treatment),]
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
  mergedLeaves <- rbind(ControlLeafAverage,isolateLeafAvg)
  mergedLeaves <- data.frame(mergedLeaves)
  mergedLeaves <- data.frame(isolate=mergedLeaves[,1],treatment=mergedLeaves[,2],mass=mergedLeaves[,3]/ctrlLAValue*100)
  NuData <- rbind(mergedRoots, mergedLeaves)
  
  ### BUILD THE PLOT
  
  ggplot(NuData) +
    aes(x = isolate, y = mass, colour = treatment) +
    ylim(0,200) + 
    geom_point(shape = "circle", size = 10L) +
    #scale_color_hue(direction = 1) +
    labs(x = "Isolates", y = ylab, title = cTitle,
         subtitle = subTitle, fill = "Treatment", color = "") +
    scale_color_manual(labels = c("Treatment Above/Leaf Mass", "Treatment Below/Root Mass","Controls"), values = c("#009E73", "#D55E00","#999999")) +
    theme_minimal()
  
  
  ggsave(filename,width=4000,height=2000,units="px",dpi=300,bg="white")
  write.csv(NuData,csvFilename,row.names=FALSE)
  
}


############################### INFO ###########################################
ha <- paste("Thanks for installing my function. Type ppGBHelp() to get help on using this function")
hb <- paste("If you have any errors, check to make sure tidyverse and dplyr are installed.")
welcome <-paste(ha,hb)
cat(welcome)


####################################
########### END OF CODE ############
####################################
####################################
############### TESTING ############
####################################

# fgdata <- read_excel("C:\\Users\\hphal\\Box\\Hall_Harrison_Plant_Pathology_Lab\\F. graminearum Tissue Specificity Assays\\Data Analysis\\Alpha Code\\Data\\fullmergeDataset.xlsx")
# fgdata
# tissueSpecPerc(fgdata,"assay 1","Root Mass")
# tissueSpecPerc(fgdata,"assay 1","Leaf Mass")
