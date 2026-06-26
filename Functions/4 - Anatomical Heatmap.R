# ___________________ ----

removeLowerRegions <- function(theData,dataVar){
  if(dataVar %in% colnames(theData)){
    theData <- as.data.frame(theData)
    theData$numbers <- theData[,dataVar]
    
    # pull out regions with data as this should only work on stuff with data
    partWithData <- theData[!is.na(theData$numbers),]
    highestRegions <- unique(partWithData$region)
    
    # split up path to use in vapply
    tokens <- lapply(strsplit(tree$path, ","), function(x) {
      x <- trimws(x)
      sub("[[:punct:]]+$", "", x)
    })
    
    # use vapply to get all paths which have highest regions
    lowerRegionRows <- which(vapply(tokens, function(x) any(x %in% highestRegions), logical(1)))
    
    # Get the regions in particular that are lower
    lowerRegions <- tree[lowerRegionRows,c("region")]
    lowerRegions <- lowerRegions[!lowerRegions$region %in% highestRegions,]
    
    # Remove those lower regions
    theData <- theData[!theData$region %in% lowerRegions$region,]
    theData$numbers <- NULL
  } else {
    print("no matching data var, it's okay though!")
  }
  
  return(theData)
}


# ___________________ ----

orderTheData <- function (theData,dataVar,regionLevel){
  
  # Only remove regions if not minor or major
  if (!regionLevel %in% c("minor","major")){
    # Begin by removing any regions below your data
    theData <- removeLowerRegions(theData,dataVar)
  }
  
  # Ensure unique is a Factor and properly ordered
  
  theData$unique <- factor(theData$unique, levels = unique(theData$unique[order(theData$unique)]))
  
  return(theData)
}


# ___________________ ----

loadSVGs <- function(slices){
  baseCombinedSVGs <- data.frame()
  for (i in 1:length(slices)){
    fileName <- paste("Necessary Files/SVG_Dataframes/",slices[i], "svgFile.rds",sep = "")
    baseCombinedSVGs <- rbind(baseCombinedSVGs,readRDS(fileName))
  }
  return(baseCombinedSVGs)
}


# ___________________ ----

simplifySVGs <- function(baseCombinedSVGs,regionLevel,valueTable){
  # Subset to only displayed regions
  if (regionLevel == "minor"){
      minorRegions <- c(unique(baseCombinedSVGs$minor),"grey")
      minorRegions <- minorRegions[!is.na(minorRegions)]
      baseCombinedSVGs <- baseCombinedSVGs[baseCombinedSVGs$region %in% minorRegions,]
    } else if (regionLevel == "major"){
      majorRegions <- c(unique(baseCombinedSVGs$major),"grey")
      majorRegions <- majorRegions[!is.na(majorRegions)]
      baseCombinedSVGs <- baseCombinedSVGs[baseCombinedSVGs$region %in% majorRegions,]
    } else {
      removeRegionRows <- is.na(baseCombinedSVGs$major) & baseCombinedSVGs$region != "grey"
      baseCombinedSVGs <- baseCombinedSVGs[!removeRegionRows,]
    }

  # Only keep a hemi if it exists in the data
  if ("right" %in% unique(valueTable$hemi) & !"left" %in% unique(valueTable$hemi)){
    baseCombinedSVGs <- baseCombinedSVGs[baseCombinedSVGs$hemi == "right",]
  } else if ("left" %in% unique(valueTable$hemi) & !"right" %in% unique(valueTable$hemi)){
    baseCombinedSVGs <- baseCombinedSVGs[baseCombinedSVGs$hemi == "left",]
  }
  
  return(baseCombinedSVGs)
}


# ___________________ ----

simplifyData <- function(valueTable,baseCombinedSVGs,isSig){
  
  # Subset to basic columns needed to make plots
  valueTable <- valueTable[,c("region","hemi","postLimVal","annots","removeRegion")]
  
  # Summarize it to remove duplicate or necessary rows
  if (!isSig){
    sumPlainData <- as.data.frame(
    valueTable %>% 
      group_by(annots,region,hemi,removeRegion) %>% 
      summarise(value = mean(postLimVal,na.rm = T))
    )
  } else {
    sumPlainData <- as.data.frame(
    valueTable %>% 
      group_by(annots,region,hemi,removeRegion) %>% 
      summarise(value = unique(postLimVal,na.rm = T))
    )
  }
  
  # Pull out the regions with data
  totalRegions <- unique(sumPlainData$region)
  
  # Make sure the regions match available regions in the atlas
  # Print the ones that don't for log reasons
  print("Not found regions: ")
  print(totalRegions[!totalRegions %in% unique(baseCombinedSVGs$region)])
  totalRegions <- totalRegions[totalRegions %in% unique(baseCombinedSVGs$region)]
  
  # Make sure that the data doesn't include those not found regions
  sumPlainData <- sumPlainData[sumPlainData$region %in% totalRegions,]
  
  return(list(sumPlainData,totalRegions))
}


# ___________________ ----

combineDataAndSVGS <- function(annotCombo,sumPlainData,
                               baseCombinedSVGs,dataSVG){
  # Subset the data to just that annot and get regions
  subset <- sumPlainData[sumPlainData$annots == annotCombo,]
  subsetRegions <- unique(subset$region)
  
  # Add data to SVGs
  svgAndSubset <- left_join(baseCombinedSVGs,subset,by = c("region","hemi"))
  svgAndSubset <- svgAndSubset[!is.na(svgAndSubset$value),]
  dataSVG <- rbind(dataSVG,svgAndSubset)
  
  return(list(dataSVG,subsetRegions))
}


# ___________________ ----

getMissingOverlapData <- function(annotCombo,missingSubsetRegions,
                                  baseCombinedSVGs,dataSVG,
                                  totalRegions,removeRegionKey){
  # Make sure to only add the hemisphere if it is present in other data
  if ("right" %in% unique(baseCombinedSVGs$hemi)){
    missingFakeDataR <- data.frame(annots = annotCombo,
                                  region = missingSubsetRegions,
                                  hemi = "right",
                                  value = NA)
  } else { missingFakeDataR <- data.frame()}
  
  if ("left" %in% unique(baseCombinedSVGs$hemi)){
  missingFakeDataL <- data.frame(annots = annotCombo,
                                  region = missingSubsetRegions,
                                  hemi = "left",
                                  value = NA)
  } else { missingFakeDataL <- data.frame()}
  
  # combine the hemispheres of data
  missingFakeData <- rbind(missingFakeDataL,missingFakeDataR)
  
  # add the data together with the svgs
  missingDataSVG <- left_join(baseCombinedSVGs,missingFakeData,by = c("region","hemi"))
  
  #subset the missingData to only regions that should have data (double dipping???)
  # missingDataSVG <- missingDataSVG[missingDataSVG$region %in% totalRegions,]
  
  # match the remove region key
  missingDataSVG <- left_join(missingDataSVG,removeRegionKey)
  
  # reorganize to match for rbind
  missingDataFixCols <- missingDataSVG[,c("removeRegion","value")]
  missingDataSVG$value <- NULL
  missingDataSVG$removeRegion <- NULL
  missingDataSVG$annots <- annotCombo
  missingDataSVG <- cbind(missingDataSVG,missingDataFixCols)
  
  # combine with totalSVG
  dataSVG <- rbind(dataSVG,missingDataSVG)
  
  return(dataSVG)
}


# ___________________ ----

getNoDataSVG <- function(dataSVG,baseCombinedSVGs,svgUniqueID){
  # Create dataset for regions with no data
  totalDataUnique <- unique(paste(dataSVG$region,dataSVG$hemi,sep = "_"))
  allUniqueRegions <- unique(paste(baseCombinedSVGs$region,baseCombinedSVGs$hemi,sep = "_"))
  
  missingUnique <- allUniqueRegions[!allUniqueRegions %in% totalDataUnique]
  
  # All with no data
  noDataSVG <- baseCombinedSVGs[svgUniqueID %in% missingUnique,]
  
  return(noDataSVG)
}


# ___________________ ----

cleanUpData <- function(SVGData,removeRegionKey){
  #Fix value so it becomes Value for legend
  SVGData$Value <- SVGData$value
  SVGData$value <- NULL
  
  # Remove regions from data by making NA
  SVGData$removeRegion <- NULL
  SVGData <- left_join(SVGData,removeRegionKey)
  SVGData[SVGData$removeRegion %in% c(T,TRUE,"True"),"Value"] <- NA
  
  return(SVGData)
}


# ___________________ ----

sortKeynames <- function(theData){
  
  factorLevels <- as.numeric(unique(theData$keyName))
  factorLevels <- sort(factorLevels)
  theData$keyName <- factor(theData$keyName, levels = factorLevels)
  
  return(theData)
}


# ___________________ ----

makeHeatmap <- function(heatmapColors,SVGData,
                        noData,hasData,
                        fiberNoData,fiberData,
                        minVal,maxVal,lineWidthVar,
                        facetTextSizeX,facetTextSizeY,
                        annoCols){
  
  theHeatmap <- ggplot(SVGData, aes(x = x, y = y))
  
  # noData
  if (nrow(noData) > 0){
    theHeatmap <- theHeatmap + geom_polygon(data = noData,aes(group = unique, fill = toFill)) +
      scale_fill_manual( values = c("gray"), na.value = "grey", guide="none") +
      geom_path(data = noData,aes(group = unique),linewidth = lineWidthVar)+
      new_scale("fill")
  }
  
  # data
  theHeatmap <- theHeatmap + geom_polygon(data = hasData,aes(group = unique, fill = Value)) +
    scale_fill_gradientn(colors = heatmapColors, limits = c(minVal, maxVal),
                         na.value="grey", guide = "none") +
    geom_path(data = hasData,aes(group = unique),linewidth = lineWidthVar) +
    new_scale("fill")
  
  # fiber no data
  if (nrow(fiberNoData) > 0){
    theHeatmap <- theHeatmap + geom_polygon(data = fiberNoData,aes(group = unique, fill = toFill)) +
      scale_fill_manual( values = c("gray"), na.value = "grey", guide="none") +
      geom_path(data = fiberNoData,aes(group = unique),linewidth = lineWidthVar)+
      new_scale("fill")
  }
  
  # fiber data
  if (nrow(fiberData) > 0){
    theHeatmap <- theHeatmap + geom_polygon(data = fiberData,aes(group = unique, fill = Value)) +
      scale_fill_gradientn(colors = heatmapColors, limits = c(minVal, maxVal),
                           na.value="grey", guide = "none") +
      geom_path(data = fiberData,aes(group = unique),linewidth = lineWidthVar) 
  }
  
  # Theme stuff
  theHeatmap <- theHeatmap +
    scale_y_reverse()+
    theme_classic()+ 
    coord_fixed() + 
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          strip.text.x = element_text(size = facetTextSizeX),
          strip.text.y = element_text(size = facetTextSizeY),
          legend.key = element_rect(fill = "white", colour = "black")
    )
  
  # Grid time
  theHeatmap <- theHeatmap + 
    facet_grid2(
      cols = vars(keyName),
      rows = vars(!!!syms(annoCols)),
      labeller = label_value, #label_context
      shrink = F
    )
  
  return(theHeatmap)
}


# ___________________ ----

makeSigHeatmap <- function(heatmapColors,SVGData,
                        noData,hasData,
                        fiberNoData,fiberData,
                        minVal,maxVal,lineWidthVar,
                        facetTextSizeX,facetTextSizeY,
                        annoCols){
  
  # adjust data to color key
  hasData$Value <- factor(hasData$Value, 
                          levels = c("Significant Increase", 
                                     "Significant Decrease", 
                                     "NS"))
  
  fiberData$Value <- factor(fiberData$Value, 
                            levels = c("Significant Increase", 
                                       "Significant Decrease", 
                                       "NS"))

  colorKey <- data.frame(Value = c("Significant Increase", 
                                   "Significant Decrease", 
                                   "NS"),
                         FillColor = c("#B2182B", 
                                       "#2166AC", 
                                       "white"))

  hasData <- left_join(hasData,colorKey)
  fiberData <- left_join(fiberData,colorKey)
  
  theHeatmap <- ggplot(SVGData, aes(x = x, y = y))
  
  # noData
  if (nrow(noData) > 0){
    theHeatmap <- theHeatmap + geom_polygon(data = noData,aes(group = unique, fill = toFill)) +
      scale_fill_manual( values = c("gray"), na.value = "grey", guide="none") +
      geom_path(data = noData,aes(group = unique),linewidth = lineWidthVar)+
      new_scale("fill")
  }
  
  # data
  theHeatmap <- theHeatmap + geom_polygon(data = hasData,aes(group = unique, fill = FillColor)) +
    scale_fill_identity(name = "Significance",
                        labels = with(distinct(hasData, FillColor, Value), setNames(as.character(Value), FillColor)),
                        na.value="grey",
                        guide = guide_legend()) +
    geom_path(data = hasData,aes(group = unique),linewidth = lineWidthVar) +
    new_scale("fill")
  
  # fiber no data
  if (nrow(fiberNoData) > 0){
    theHeatmap <- theHeatmap + geom_polygon(data = fiberNoData,aes(group = unique, fill = toFill)) +
      scale_fill_manual( values = c("gray"), na.value = "grey", guide="none") +
      geom_path(data = fiberNoData,aes(group = unique),linewidth = lineWidthVar)+
      new_scale("fill")
  }
  
  # fiber data
  if (nrow(fiberData) > 0){
    theHeatmap <- theHeatmap + geom_polygon(data = fiberData,aes(group = unique, fill = FillColor)) +
      scale_fill_identity(name = "Significance",
                          labels = with(distinct(fiberData, FillColor, Value), setNames(as.character(Value), FillColor)),
                          na.value="grey",
                          guide = "none") +
      geom_path(data = fiberData,aes(group = unique),linewidth = lineWidthVar) 
  }
  
  # Theme stuff
  theHeatmap <- theHeatmap +
    scale_y_reverse()+
    theme_classic()+ 
    coord_fixed() + 
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          strip.text.x = element_text(size = facetTextSizeX),
          strip.text.y = element_text(size = facetTextSizeY),
          legend.key = element_rect(fill = "white", colour = "black")
    )
  
  # Grid time
  theHeatmap <- theHeatmap + 
    facet_grid2(
      cols = vars(keyName),
      rows = vars(!!!syms(annoCols)),
      labeller = label_value, #label_context
      shrink = F
    )
  
  return(theHeatmap)
}


# ___________________ ----

makeAnatomicalHeatmaps <- function(valueTable, slices, variables, 
                                   annoCols = NULL, isSig = F){
  # Get the anno cols
  if(is.null(annoCols)){
    annoCols <- variables$AoI
    minVal <- variables$minVal
    maxVal <- variables$maxVal
    valueTable$annots <- valueTable$x
  } else if(!isSig){
    minVal <- min(valueTable$postLimVal)
    maxVal <- max(valueTable$postLimVal)
  } else {
    # These just have to exist, but don't matter for sig heatmap
    minVal <- 0
    maxVal <- 0
  }
  
  # Get the variable options
  regionLevel <- variables$regionLevel
  invert <- variables$invert
  colorPalette <- variables$colorPalette
  viridisPalette <- variables$viridisOptions
  twoColorPalette <- c(variables$twoCol1,variables$twoCol2)
  threeColorPalette <- c(variables$threeCol1,variables$threeCol2,variables$threeCol3)
  
  # Set standard values that vary based on valueTable size
  facetTextSizeX <- 8.5
  facetTextSizeY <- -0.125*max(nchar(unique(valueTable$x)))+8.75
  lineWidthVar <- 0.1
  
  # Adjust heatmap color direction if needed
  directionValue <- -1
  if (invert == T){
    directionValue <- 1
  }
  
  
  ## Note for coloring ----

  # removeRegion are regions to grey out
  # VS needs to be white
  # fibertracks need to be on top

  
  ## Get SVG Base ----
  
  # Load the large dataframe with all slices svg data
  baseCombinedSVGs <- loadSVGs(slices)
  
  # Remove extra regions and hemispheres
  baseCombinedSVGs <- simplifySVGs(baseCombinedSVGs,regionLevel,valueTable)
  
  # Create a base unique ID of all possible regions and hemis
  svgUniqueID <- paste(baseCombinedSVGs$region,baseCombinedSVGs$hemi,sep = "_")
  
  
  ## Manipulate Data ----
  
  simplifyList <- simplifyData(valueTable,baseCombinedSVGs,isSig)
  sumPlainData <- simplifyList[[1]]
  totalRegions <- simplifyList[[2]]
  
  # Pull out the region to removeRegionKey for later merging
  removeRegionKey <- sumPlainData %>% 
    group_by(region) %>% 
    summarise(removeRegion = unique(removeRegion))
  
  # Get the different annotations we will need
  annotCombos <- unique(sumPlainData$annots)
  
  # Pull out minor or major regions
  if (regionLevel == "minor"){
    allNeededRegions <- unique(baseCombinedSVGs$minor)[!is.na(unique(baseCombinedSVGs$minor))]
  } else if (regionLevel == "major"){
    allNeededRegions <- unique(baseCombinedSVGs$major)[!is.na(unique(baseCombinedSVGs$major))]
  }
  
  dataSVG <- c()
  for (i in 1:length(annotCombos)){
    annotCombo <- annotCombos[i]
    print(annotCombo)
    
    combineList <- combineDataAndSVGS(annotCombo,sumPlainData,baseCombinedSVGs,dataSVG)
    dataSVG <- combineList[[1]]
    subsetRegions <- combineList[[2]]
    
    # Pull out regions that are in another annot, but not this one
    missingSubsetRegions <- totalRegions[!totalRegions %in% subsetRegions]
    
    if (regionLevel %in% c("minor","major")){
      additionalRegions <- allNeededRegions[!allNeededRegions %in% c(missingSubsetRegions,totalRegions)]
      missingSubsetRegions <- c(missingSubsetRegions,additionalRegions)
    }
    
    # if there are missing regions, then add them
    if (length(missingSubsetRegions) > 0){
      dataSVG <- getMissingOverlapData(annotCombo,missingSubsetRegions,
                                       baseCombinedSVGs,dataSVG,
                                       totalRegions,removeRegionKey)
    }
  }
  
  # Separate the annotations if they are a combination of AoIs and label for later
  dataSVG <- separate(dataSVG, annots, annoCols, sep = "_")
  
  # Get the svgs for regions with no data
  if (!regionLevel %in% c("minor","major")){
    noDataSVG <- getNoDataSVG(dataSVG,baseCombinedSVGs,svgUniqueID)
  } else {
    noDataSVG <- data.frame()
  }
  
  
  # Create official data and no data datasets
  SVGData <- orderTheData(dataSVG,"value",regionLevel)
  
  if (nrow(noDataSVG) > 0){
    SVGNoData <- orderTheData(noDataSVG,"",regionLevel)
  } else {
    SVGNoData <- noDataSVG
  }
  
  # Remove all extra large objects
  rm(baseCombinedSVGs)
  rm(missingDataFixCols)
  rm(missingDataSVG)
  rm(svgAndSubset)
  rm(svgUniqueID)
  rm(dataSVG)
  rm(noDataSVG)
  
  SVGData <- cleanUpData(SVGData,removeRegionKey)

  # Sort the keynames so that it is in the proper order when plotting
  SVGData <- sortKeynames(SVGData)
  
  if(nrow(SVGNoData) > 0){
    SVGNoData <- sortKeynames(SVGNoData)
  }
  
  # Seperate the fiber and data (and no data)
  fiberData <- SVGData[SVGData$regionType == "fiber",]
  hasData <- SVGData[is.na(SVGData$regionType),]
  
  fiberNoData <- SVGNoData[SVGNoData$regionType == "fiber",]
  noData <- SVGNoData[is.na(SVGNoData$regionType),]
  
  # Remove un-needed dataframe
  rm(SVGNoData)
  
  # Remove extra column
  SVGData[,c("regionType")] <- NULL
  fiberData[,c("regionType")] <- NULL
  fiberNoData[,c("regionType")] <- NULL
  hasData[,c("regionType")] <- NULL
  noData[,c("regionType")] <- NULL
  
  # Redundant coloring, but toFill is needed for plotting
  if (nrow(noData) > 0){
    noData$toFill <- "grey"
  }
  if (nrow(fiberNoData) > 0){
    fiberNoData$toFill <- "grey"
  }
  
  # Ensure only hemis with data are kept
  if (length(unique(hasData$hemi)) < 2){
    haveHemi <- unique(hasData$hemi)
    
    hasData <- hasData[hasData$hemi == haveHemi,]
    fiberData <- fiberData[fiberData$hemi == haveHemi,]
    if (nrow(noData) > 0){
      noData <- noData[noData$hemi == haveHemi,]
    }
    if (nrow(fiberNoData) > 0){
      fiberNoData <- fiberNoData[fiberNoData$hemi == haveHemi,]
    }
  }
  
  ## Make Heatmap ----
  
  # Set the color palette
  if (colorPalette == "Viridis"){
    heatmapColors <- viridis::viridis(20, option = viridisPalette, direction = directionValue)
  } else if (colorPalette == "2 Color"){
    heatmapColors <- colorRampPalette(twoColorPalette)(20)
  } else {
    heatmapColors <- colorRampPalette(threeColorPalette)(20)
  }
  
  # Override if sig heatmap
  if(isSig){
    heatmapColors <- c("#B2182B", "#2166AC", "white")
  }
  
  # Check for NA in keyname and annoCols
  fiberData <- fiberData[!is.na(fiberData$keyName),]
  
  if(!isSig){
    theHeatmap <- makeHeatmap(heatmapColors,SVGData,
                            noData,hasData,
                            fiberNoData,fiberData,
                            minVal,maxVal,lineWidthVar,
                            facetTextSizeX,facetTextSizeY,
                            annoCols)
  } else {
    theHeatmap <- makeSigHeatmap(heatmapColors,SVGData,
                            noData,hasData,
                            fiberNoData,fiberData,
                            minVal,maxVal,lineWidthVar,
                            facetTextSizeX,facetTextSizeY,
                            annoCols)
  }
  
  ## Return Heatmap ----
  return(theHeatmap)
}
