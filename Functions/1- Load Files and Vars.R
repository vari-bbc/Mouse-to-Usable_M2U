# ___________________ ----

fileImport <- function(fileNames,paths){
  # Creates key from tree
  treeKey <- tree[, c(1, 2)]
  
  # Pulls first line to figure out if Nutil or non-Nutil
  firstLine <- readLines(paths[1], n = 1)

  rawData <- c()
  # If nutil format:
  if (grepl("Region ID;", firstLine)) {
    # Loops through each file and reads in data
    for (i in 1:length(paths)) {
      toNormal <- tidytable::fread(paths[i], sep = ";")
      toNormal <- toNormal[, 1:9]
      name <- fileNames[i]
      for (j in 2:length(toNormal[, 1])) {
        name <- rbind(name, name)
      }
      toAdd <- cbind(name, toNormal)
      rawData <- rbind(rawData, toAdd)
    }
    # Cleans up rawData
    rawData <- rawData[rawData$`Region pixels` != 0,]
    colnames(rawData)[1:2] <- c("fileName", "ABAID")
    rawData <- rawData[, c("fileName", "ABAID", "Region area", "Object count", "Object area")]
    rawData$ABAID <- as.character(rawData$ABAID)
    # Joins with tree key to get region names
    rawData <- left_join(rawData, treeKey, by = "ABAID")

  # If non-nutil format:
  } else {
    # Loops through each file and reads in data
    for (i in 1:length(paths)) {
      toNormal <- read.csv(paths[i])
      name <- fileNames[i]
      toAdd <- data.frame(fileNames[i], toNormal)
      rawData <- rbind(rawData, toAdd)
    }
    colnames(rawData)[1] <- c("fileName")
    rawData <- left_join(rawData, treeKey, by = "region")
  }

  numericCols <- colnames(rawData)[!colnames(rawData) %in% baseCols]

  return(list(rawData,numericCols))
}

# ___________________ ----

createAnnoFile <- function(rawData, recOptions, customOptions, singleHemi){
  if (singleHemi == T | "hemi" %in% colnames(rawData)){
    print("single hemi")
    table <- rawData %>% group_by(fileName) %>% summarise()
  } else {
    table <- rawData %>% group_by(fileName) %>% summarise()
    table$hemi <- NA
  }

  annoFile <- cbind(include = "Y", table)

  if (!is.null(recOptions[1])) {
    recTable <- data.frame(matrix(NA, nrow = nrow(table), ncol = length(recOptions)))
    colnames(recTable) <- recOptions
    annoFile <- cbind(annoFile, recTable)
  } 

  if (!is.null(customOptions[1])) {
    customTable <- data.frame(matrix(NA, nrow = nrow(table), ncol = length(customOptions)))
    colnames(customTable) <- customOptions
    annoFile <- cbind(annoFile, customTable)
  } 
  
  return(annoFile)
}

# ___________________ ----

mergeData <- function(rawData, annoFile, numericCols) {

  # Only use annoFile hemi
  rawData$hemi <- NULL

  # If only one hemisphere present, add hemi column to annoFile as Right
  if (!"hemi" %in% colnames(annoFile)) {
      annoFile$hemi <- "right"
  }
  
  fullData <- left_join(rawData, annoFile, by = c("fileName"))
  fullData <- fullData[fullData$include == "Y",]
  fullData$include <- NULL
  
  nutil <- any(grepl("region", numericCols, ignore.case = T) & grepl("area", numericCols, ignore.case = T))
  
  if (nutil) {
      numericCols <- numericCols[!numericCols %in% c("Load","load")]
      numericCols <- c("load","InclusionsPerMM2", numericCols) #, "neuriteLoad", "InclusionsPerMM2"
  }

  numericCols <- sub(" ", ".", numericCols)

  # Remove columns that are all NAs
  fullData <- fullData[ ,colSums(is.na(fullData)) != length(fullData$region)]

  return(list(fullData,numericCols))
}

# ___________________ ----

loadCheckpointData <- function(fullData){
  
  #remove columns that are all NAs
  fullData1 <- fullData[ ,colSums(is.na(fullData)) != length(fullData$region)]

  dataSplit <- which(colnames(fullData1) %in% c("region", "ABAID"))[2]
  
  annoDataHalf <- fullData1[, (dataSplit + 1):length(colnames(fullData1)),drop=FALSE]
  recAnnoCols <- colnames(annoDataHalf)[colnames(annoDataHalf) %in% recCols]
  customAnnoCols <- colnames(annoDataHalf)[!colnames(annoDataHalf) %in% c(recAnnoCols,"hemi")]

  annoCols <- c(recAnnoCols, customAnnoCols)

  rawData <- fullData1[, 1:(dataSplit - 1)]
  numericCols <- colnames(rawData)[!colnames(rawData) %in% baseCols & 
                                       sapply(rawData, is.numeric) &
                                       !colnames(rawData) %in% annoCols]

  

  nutil <- any(grepl("region", numericCols, ignore.case = T) & grepl("area", numericCols, ignore.case = T))
  if (nutil) {
    numericCols <- numericCols[!numericCols %in% ("Load")]
    numericCols <- c("load","InclusionsPerMM2", numericCols) #"neuriteLoad", "InclusionsPerMM2"
  }

  return(list(fullData1,numericCols, recAnnoCols, customAnnoCols))
}



