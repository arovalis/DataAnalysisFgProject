

data_file <- read_excel("C:\\Users\\hphal\\Box\\Hall_Harrison_Plant_Pathology_Lab\\F. graminearum Tissue Specificity Assays\\Data Analysis\\Alpha Code\\Data\\fullmergeDataset.xlsx")
assay_number <- "assay 1"
measurement <- "Leaf Mass"


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
  ### BUILD CONTROLS ###
  # Control has to be measured apart from the isolates, otherwise you can end up with a control of 0, which messes stuff up later.
  controlAvg <- xtabs(rootMass ~ isolate + assay, (aggregate(rootMass ~ isolate + assay, fgdata, mean)))  ## Grabs rootMass 
  ctrlChart <- data.frame(controlAvg) ## averages of the controls
  controlAvg <-which(ctrlChart == assayNum,arr.ind=TRUE) ## Build these control chart
  controlAvg <- paste(controlAvg[1,1]) ## Paste inside chart
  controlMass <- ctrlChart[controlAvg,3] ## Check column 3
  ############################## ANALYSIS ########################################
  # This builds a new data frame that compares masses to chosen control average
  isoAverage <- xtabs(rootMass ~ isolate + assay + treatment, (aggregate(rootMass ~ isolate + assay + treatment, fgdata, mean, droplevels(fgdata[fgdata$treatment!="above"]))) ## Now build the actual graph of data to compare to control
  isoAverage
  nuChart <- data.frame(isoAverage) ##Grabs everything and puts into new data frame
  nuChart
  percentCont <- c(nuChart[,4]/controlMass)*100 ## Divides all root mass averages by the control average
  percentCont
  # nuData <- data.frame(nuChart[,1],nuChart[,2],nuChart[,3],percentCont) ## Makes new data frame
  nuData <- data.frame(nuChart[,1],nuChart[,2],nuChart[,3],percentCont) ## Makes new data frame
  nuData
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
  geom_point(shape = "circle filled", size = 10L) +
  scale_fill_brewer(palette = chartColor, direction = 1) +
  scale_color_hue(direction = 1) +
  labs(x = "Isolates", y = ylab, title = cTitle,
       subtitle = subTitle, fill = "Treatment", color = "Isolate") +
  theme_minimal()

