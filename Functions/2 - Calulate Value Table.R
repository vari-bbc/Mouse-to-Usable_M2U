# ___________________ ----

calculateValueTable <- function(preVar, variables, AoIStrata, VoI = NA){
  # Extract variables
  regionLevel <- variables$regionLevel
  legendVar <- variables$legendVar
  valueVar <- variables$valueVar
  logged <- variables$logged
  multiPercent <- variables$multiPercent
  deviPercent <- variables$deviPercent
  regionsToRemove <- variables$regionsToRemove
  minVal <- variables$minVal
  maxVal <- variables$maxVal
  
  # Remove empty and NA regions, clean column names
  fullData <- preVar[preVar$region != "" & !is.na(preVar$region), ]
  colnames(fullData) <- sub(" ", ".", colnames(fullData))
  
  # Get keys from the tree for merging
  regionLevelKey <- tree[, c("region", "parent", "major")]
  
  # Build statsCols if VoI is provided
  statsCols <- NULL
  if (!is.na(VoI) && VoI != ""){
    statsCols <- c("mouse", "sex", "batch")
    statsCols <- setdiff(statsCols, c(AoIStrata, VoI))
    statsCols <- c(statsCols, AoIStrata, VoI)
    # Keep only columns that exist
    statsCols <- intersect(statsCols, colnames(fullData))
  }
  
  # Build initial data frame
  print("create")
  if (!is.na(VoI) && length(statsCols) > 0){
    statsCols <- setdiff(statsCols, "None")
    if(all(statsCols %in% colnames(fullData))){
      initialData <- data.frame(region = fullData$region, hemi = fullData$hemi, fullData[, statsCols])
      colnames(initialData) <- c("region","hemi",statsCols)
    } else if (all(AoIStrata != "None")){
      initialData <- data.frame(region = fullData$region, hemi = fullData$hemi, fullData[, AoIStrata])
      colnames(initialData) <- c("region","hemi",AoIStrata)
    } else {
      initialData <- data.frame(region = fullData$region, hemi = fullData$hemi, mouse = "mouse")
    }
  } else {
    if (all(AoIStrata != "None")){
      initialData <- data.frame(region = fullData$region, hemi = fullData$hemi, fullData[, AoIStrata])
      colnames(initialData) <- c("region","hemi",AoIStrata)
    } else {
      initialData <- data.frame(region = fullData$region, hemi = fullData$hemi, AoIStrata = "None")
    }
  }
  
  # Merge in region key
  initialData <- as.data.frame(left_join(initialData, regionLevelKey, by = "region"))
  
  # Set updatedLevel based on regionLevel
  initialData$updatedLevel <- if (regionLevel != "daughter") initialData[, regionLevel] else initialData$region
  
  # Create unique ID
  print("id")
  x <- AoIStrata
  initialData <- as.data.frame(initialData)
  
  idCols <- if (!is.na(VoI)){
    if ("mouse" %in% colnames(initialData)) c("mouse", "updatedLevel", "hemi") else c("updatedLevel", "hemi")
  } else {
    c(x, "updatedLevel", "hemi")
  }
  initialData$uniqueID <- apply(initialData[, idCols], 1, paste, collapse = "_")
  
  # Create initial key
  initialKey <- unique(initialData[, colnames(initialData) %in% c("uniqueID", "updatedLevel", "hemi", x, VoI)])
  
  # Calculate values
  print("value")
  if (valueVar == "load"){
    initialData$regionArea <- fullData[, grepl("region.*area", colnames(fullData), ignore.case = TRUE)]
    initialData$objectArea <- fullData[, grepl("object.*area", colnames(fullData), ignore.case = TRUE)]
    
    theTable <- initialData %>% 
      group_by(uniqueID) %>%
      summarise(objectArea = sum(objectArea), regionArea = sum(regionArea), .groups = 'drop')
    
    theTable$preModVal <- theTable$objectArea / theTable$regionArea
    
    theTable <- as.data.frame(left_join(theTable, initialKey, by = "uniqueID"))
  } else if (valueVar == "InclusionsPerMM2"){
    initialData$regionArea <- fullData[, grepl("region.*area", colnames(fullData), ignore.case = TRUE)]
    initialData$regionArea <- initialData$regionArea * .000001

    initialData$objectCount <- fullData[, grepl("object.*count", colnames(fullData), ignore.case = TRUE)]
    
    theTable <- initialData %>% 
      group_by(uniqueID) %>%
      summarise(objectCount = sum(objectCount), regionArea = sum(regionArea), .groups = 'drop')
    
    theTable$preModVal <- theTable$objectCount / theTable$regionArea
    
    theTable <- as.data.frame(left_join(theTable, initialKey, by = "uniqueID"))
  } else {
    initialData$preModVal <- as.numeric(fullData[, valueVar])
    
    theTable <- initialData %>% 
      group_by(uniqueID) %>%
      summarise(preModVal = mean(preModVal, na.rm = TRUE), .groups = 'drop')
    
    theTable <- as.data.frame(left_join(theTable, initialKey, by = "uniqueID"))
  }
  
  # Merge back initial key and set region
  theTable$region <- theTable$updatedLevel
  theTable <- theTable[, !colnames(theTable) %in% "updatedLevel"]
  
  # Remove NAs and duplicates
  print("mod")
  theTable <- theTable[!is.na(theTable$preModVal) & !duplicated(theTable$uniqueID), ]
  
  # Apply transformations
  theTable$preLimVal <- theTable$preModVal
  if (multiPercent) theTable$preLimVal <- theTable$preLimVal * 100
  if (deviPercent) theTable$preLimVal <- theTable$preLimVal / 100
  
  # Log transformation
  if (logged) {
    print("log")
    theTable$preLimVal <- log10(theTable$preLimVal + 0.00001)
  }
  
  # Final aggregation
  print("joins")
  theTablePostModKey <- unique(initialKey[, !colnames(initialKey) %in% "updatedLevel"])
  
  theTable <- theTable %>% 
    group_by(uniqueID) %>%
    summarise(preLimVal = mean(preLimVal), region = unique(region), .groups = 'drop') %>%
    left_join(theTablePostModKey, by = "uniqueID")
  
  theTable <- as.data.frame(theTable)
  
  # Apply value limits
  theTable$postLimVal <- pmin(pmax(theTable$preLimVal, minVal), maxVal)
  
  # Build regions to remove list
  regionsToRemove <- unique(c(regionsToRemove, tree[grepl("VS", tree$path), "region"], "VS"))
  theTable$removeRegion <- theTable$region %in% regionsToRemove
  
  # Add legend variables
  print("legend")
  if (is.na(VoI)){
    if ("daughter" %in% legendVar) {
      theTable$daughter <- theTable$region
    } else {
      theTable <- as.data.frame(left_join(theTable, regionLevelKey, by = "region"))
    }
    
    theTable$x <- if (length(x) > 1) apply(theTable[, x], 1, paste, collapse = "_") else theTable[, x]
    theTable$y <- apply(theTable[, c("region", "hemi")], 1, paste, collapse = "_")
  }

  theTable$hemi <- tolower(theTable$hemi)
  
  return(theTable)
}