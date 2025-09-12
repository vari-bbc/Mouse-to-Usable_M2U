mergeData <- function(rawData, annoFile, numericCols) {
  startSection("Merge Data Function")
  
  nutil <- any(grepl("region", numericCols, ignore.case = T) & grepl("area", numericCols, ignore.case = T))

  if (nutil) {
    rawData$hemi <- NULL
    if (!"hemi" %in% colnames(annoFile)) {
        annoFile$hemi <- "right"
    }
  
    numericCols <- numericCols[!numericCols %in% c("Load","load")]
    numericCols <- c("load", "neuriteLoad", "InclusionsPerMM2", numericCols)
  } else {
    annoFile$hemi <- NULL
  }
  
  fullData <- left_join(rawData, annoFile, by = c("fileName"))
  fullData <- fullData[fullData$include == "Y",]
  fullData$include <- NULL
  
  endSection("Merge Data Function")

  return(list(fullData,numericCols))
      
}


checkStrata <- function(AoIs, theData){
  # returns true or false as to whether it should unlock VoI AND
  # the options that VoI can be
  
  strataOptions <- recCols[!recCols %in% c("mouse","batch")]
  strataOptions <- strataOptions[!strataOptions %in% AoIs]
  
  #if AoI is NOT mouse or batch
  if (!any(AoIs %in% c("mouse","batch"))){
    
    #if colnames of theData includes at least one possible VoI option
    if(any(colnames(theData) %in% strataOptions)){
      strataOptions <- strataOptions[strataOptions %in% colnames(theData)]
      
      return(list("openStats" = T,
                  "strataOptions" = strataOptions))
    }else{
      return(list("openStats" = F))
    }
    
  } else {
    return(list("openStats" = F))
  }
}


calculateValueTable <- function(preVar,variables,AoIStrata, VoI = NA){
  # annoCols <- variables$AoI
  regionLevel <- variables$regionLevel
  legendVar <- variables$legendVar
  rowName <- variables$rowVars
  colName <- variables$colVars
  valueVar <- variables$valueVar
  logged <- variables$logged
  multiPercent <- variables$multiPercent
  deviPercent <- variables$deviPercent
  regionsToRemove <- variables$regionsToRemove
  invert <- variables$invert
  minVal <- variables$minVal
  maxVal <- variables$maxVal
  colorPalette <- variables$colorPalette
  trim <- variables$trim

  # remove empty and NA regions
  fullData <- preVar[preVar$region != "",]
  fullData <- fullData[!is.na(fullData$region),]
  
  colnames(fullData) <- sub(" ", ".", colnames(fullData))
  
  # get keys from the tree for merging
  regionLevelKey <- tree[, c("region", "parent", "major")]
  regionIDKey <- tree[, c("region", "ABAID")]
  
  # print("VoI")
  if (!is.na(VoI)){
    if (VoI != ""){
      # create list of stat columns
      statsCols <- c("mouse","sex", "batch")
      statsCols <- statsCols[!statsCols %in% AoIStrata]
      statsCols <- statsCols[!statsCols %in% VoI]
      statsCols <- c(statsCols, AoIStrata, VoI)

      #remove batch/sex if the column doesn't exist
      if (!"batch" %in% colnames(fullData)){ statsCols <- statsCols[statsCols != "batch"] }
      if (!"sex" %in% colnames(fullData)){ statsCols <- statsCols[statsCols != "sex"] }
    }
    
  }
  
  # grab and merge together all columns needed (initial data)
  regionCol <- fullData$region
  hemiCol <- fullData$hemi
  if (all(AoIStrata != "None")){
    annoCol <- fullData[, AoIStrata]
  } else{
    annoCol <- "None"
  }
  
  
  print("create")
  
  if (!is.na(VoI)){
    if ("None" %in% statsCols){
      statsCols <- statsCols[statsCols != "None"]
    }
    if(all(statsCols %in% colnames(fullData))){
      statCol <- fullData[, statsCols]
      initialData <- data.frame(cbind(regionCol, hemiCol, statCol))
      colnames(initialData) <- c("region", "hemi", statsCols)
    } else if (annoCol != "None"){
      rm("statsCols")
      initialData <- data.frame(cbind(regionCol, hemiCol, annoCol))
      colnames(initialData) <- c("region", "hemi", AoIStrata)
    } else {
      initialData <- data.frame(cbind(regionCol, hemiCol,"mouse"))
      colnames(initialData) <- c("region", "hemi","mouse")
    }

  } else {
    initialData <- data.frame(cbind(regionCol, hemiCol, annoCol))
    colnames(initialData) <- c("region", "hemi", AoIStrata)
  }
  
  # merge in region key
  initialData <- left_join(initialData, regionLevelKey, by = "region")
  initialData <- as.data.frame(initialData)
  
  # updatedLevel ← regionLevel (keep region for math)
  if (regionLevel != "daughter"){
    initialData$updatedLevel <- initialData[,regionLevel]
  } else {
    initialData$updatedLevel <- initialData$region
  }
  
  
  print("id")
  # x ← AoI
  x <- AoIStrata
  
  initialData <- as.data.frame(initialData)
  
  # initialData$statsUniqueID ← mouse, updatedLevel, hemi
  if (!is.na(VoI)){
    initialData$uniqueID <- apply(initialData[, c("mouse", "updatedLevel", "hemi")], 1, paste, collapse = "_")
  } else {
    initialData$uniqueID <- apply(initialData[, c(x, "updatedLevel", "hemi")], 1, paste, collapse = "_")
  }
  
  
  # initialKey ← initialData[,colnames in c(uniqueIDs, updatedLevel, hemi)
  initialKey <- initialData[, colnames(initialData) %in% c("uniqueID","updatedLevel","hemi",x,VoI)]
  
  # if (!is.na(VoI)){
  #   initialKey <- initialData[, colnames(initialData) %in% c("uniqueID","updatedLevel","hemi",x,VoI)]
  # } else {
  #   initialKey <- initialData[, colnames(initialData) %in% c("uniqueID","updatedLevel","hemi",x)]
  # }
  
  
  print("value")
  if (valueVar == "load"){
    # get region and object area
    initialData$regionArea <- fullData[, grepl("region", colnames(fullData), ignore.case = TRUE) & grepl("area", colnames(fullData), ignore.case = TRUE)]
    initialData$objectArea <- fullData[, grepl("object", colnames(fullData), ignore.case = TRUE) & grepl("area", colnames(fullData), ignore.case = TRUE)]
    
    
    theTable <- initialData %>% group_by(uniqueID) %>%
            summarise(objectArea = sum(objectArea), regionArea = sum(regionArea))
    
    theTable$preModVal <- theTable$objectArea / theTable$regionArea
  
    theTable <- as.data.frame(left_join(theTable,initialKey))
    # theTable <- theTable[,!colnames(theTable) %in% "uniqueID"]
    
    theTable$region <- theTable$updatedLevel
    theTable <- theTable[,!colnames(theTable) %in% "updatedLevel"]
    
    
  } else {
    initialData$preModVal <- fullData[,valueVar]
    initialData$preModVal <- as.numeric(initialData$preModVal)
    
    theTable <- initialData %>% group_by(uniqueID) %>%
          summarise("preModVal" = mean(preModVal))
    
    #merge back in initialData
    theTable <- as.data.frame(left_join(theTable,initialKey))
    # theTable <- theTable[,!colnames(theTable) %in% "uniqueID"]
    
    theTable$region <- theTable$updatedLevel
    theTable <- theTable[,!colnames(theTable) %in% "updatedLevel"]
  }
  
  print("mod")
  
  theTable <- theTable[!is.na(theTable$preModVal),]
  
  theTable <- theTable[!(duplicated(theTable$uniqueID)),]
  
  theTable$preLimVal <- theTable$preModVal
  
  if (multiPercent){
    theTable$preLimVal <- theTable$preLimVal * 100
  }
  if (deviPercent){
    theTable$preLimVal <- theTable$preLimVal / 100
  }
  
  # log manipulation
  if (logged) {
    print("log")
  
    theTable$preLimVal <- theTable$preLimVal + 0.00001
    theTable$preLimVal <- log10(theTable$preLimVal)
  }

  theTablePostModKey <- initialKey[,!colnames(initialKey) %in% c("updatedLevel")]
  theTablePostModKey <- theTablePostModKey[!(duplicated(theTablePostModKey$uniqueID)),]
  
  
  print("joins")
  
  theTable <- theTable %>% group_by(uniqueID) %>%
          summarise(preLimVal = mean(preLimVal), region = unique(region))
  theTable <- as.data.frame(left_join(theTable, theTablePostModKey))
  
  # norm$post_lim_val ← limit pre_lim_val
  theTable$postLimVal <- theTable$preLimVal
  theTable$postLimVal[theTable$postLimVal < minVal] <- minVal
  theTable$postLimVal[theTable$postLimVal > maxVal] <- maxVal
  
  # norm ← norm[!norm$removeRegion,]
  regionsToRemove <- tree[grepl("fiber",tree$path),"region"]
  regionsToRemove <- c(regionsToRemove,tree[grepl("VS",tree$path),"region"])
  regionsToRemove <- c(regionsToRemove,variables$regionsToRemove,"VS")
  
  #Label regions to remove
  theTable$removeRegion <- theTable$region %in% regionsToRemove
  
  print("legend")
  
  if (is.na(VoI)){
    if ("daughter" %in% legendVar) {
      theTable$daughter <- theTable$region
    } else {
      theTable <- as.data.frame(left_join(theTable, regionLevelKey, by = "region"))
    }
    
    if (length(x) > 1) {
      theTable$x <- apply(theTable[, x], 1, paste, collapse = "_")
    } else {
      theTable$x <- theTable[, x]
    }
    
    theTable$y <- apply(theTable[, c("region","hemi")], 1, paste, collapse = "_")
  }
  
  return(theTable)
  
}




