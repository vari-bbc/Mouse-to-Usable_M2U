# ___________________ ----

makeNormalHeatmaps <- function(fullData, variables){
  # Get the options
  annoCols <- variables$AoI
  regionLevel <- variables$regionLevel
  legendVar <- variables$legendVar
  rowName <- variables$rowVars
  colName <- variables$colVars
  valueVar <- variables$valueVar
  logged <- variables$logged
  percent <- variables$percent
  regionsToRemove <- variables$regionsToRemove
  invert <- variables$invert
  minVal <- variables$minVal
  maxVal <- variables$maxVal
  colorPalette <- variables$colorPalette
  trim <- variables$trim

  # View(variables)

  # remove none if left in the rownames and colnames
  if(length(rowName) > 1 & "none" %in% rowName){
    rowName <- rowName[!rowName %in% "none"]
  }
  
  if(length(colName) > 1 & "none" %in% colName){
    colName <- colName[!colName %in% "none"]
  }

  # seperate out datasets for later modification
  pivotData <- fullData[!fullData$removeRegion,]
  unmodifiedFullData <- fullData[!fullData$removeRegion,]
  forAnnotations <- fullData[!fullData$removeRegion,]

  # Pivot the data to wide format for heatmap plotting
  pivotData <- pivotData[,c("x","y","postLimVal")]
  pivotData$uniqueRowID <- NULL

  pivotData <- pivotData %>% 
    pivot_wider(names_from = y, values_from = postLimVal, 
    names_repair = "minimal")
  data <- as.data.frame(pivotData)
  rownames(data) <- data$x
  data$x <- NULL
  
  if ("current level" %in% legendVar){
    legendVar[legendVar == "current level"] <- "region"
  }

  # Create legend column in annotation data
  if (length(legendVar) > 1){
    forAnnotations$legend <- apply(forAnnotations[, legendVar], 1, paste, collapse = "_")
  } else {
    forAnnotations$legend <- forAnnotations[,legendVar]
  }

  # Create the y annotations
  annoY <- forAnnotations %>% group_by(y,legend,hemi) %>% summarise()
  annoY <- as.data.frame(annoY)
  
  # Save and remove columns for the legend separation
  rownamesY <- annoY$y
  hemiY <- annoY$hemi
  annoY$hemi <- NULL
  annoY$y <- NULL

  # Separate out the legend into individual columns
  #   It was mainly together for the group by before
  annoY <- annoY %>% separate_wider_delim(legend, delim = "_", names = legendVar)
  annoY <- as.data.frame(annoY)

  # Add back in the saved cols
  rownames(annoY) <- rownamesY
  annoY$legend <- NULL
  annoY$hemi <- hemiY

  # Do same for the x annotations
  annoX <- forAnnotations %>% group_by(x) %>% summarise()
  annoX <- as.data.frame(annoX)
  rownamesX <- annoX$x
  annoX <- annoX %>% separate_wider_delim(x, delim = "_", names = annoCols)
  annoX <- as.data.frame(annoX)
  rownames(annoX) <- rownamesX

  # Trim if supposed to
  if (trim == T){
    data <- data[,colSums(is.na(data)) < nrow(data)*0.5]
  }

  # Create the seqBreaks for the heatmap colors
  breaks <- 20
  difference <- minVal - maxVal
  breakNum <- abs(difference / (breaks + 1))
  seqBreaks <- seq(minVal, maxVal, by = breakNum)

  # Set the region colors from the ABA Tree for the cols
  ABALevels <- legendVar
  colorIDs <- tree[,c("region","colorHex")]
  unmodifiedFullData <- as.data.frame(unmodifiedFullData)
  colColors <- list()
  for (i in 1:length(ABALevels)){
    uniqueRegions <- c(unique(unmodifiedFullData[,ABALevels[i]]),"NA")
    levelColors <- colorIDs[match(uniqueRegions, colorIDs$region),2]
    # levelColors[levelColors == "NA"] <- "grey"
    levelColors <- setNames(unlist(levelColors), as.character(uniqueRegions))
    colColors[[ABALevels[i]]] <- levelColors
  }

  # Set the colors for hemi
  colColors[["hemi"]] <- setNames(c("red","blue"),c("left","right"))

  # Set the colors for the rows
  annoX <- as.data.frame(annoX)
  rowColors <- list()
  theStart <- 1
  for (i in 1:length(colnames(annoX))){
    rowItems <- as.character(unique(annoX[,i]))
    theEnd <- theStart+length(rowItems)-1
    unnamed <- thePalette[theStart:theEnd]
    namedItems <- setNames(unnamed, rowItems)
    rowColors[[colnames(annoX)[i]]] <- namedItems
    theStart <- theStart + length(rowItems)
  }

  # Combine the row and column colors
  fullColors <- append(rowColors, colColors)

  # Set row names
  if(rowName[1] != "none"){
    rowSubset <- forAnnotations[,c("x",rowName)]
    rowSubset <- rowSubset[!duplicated(rowSubset$x),]
    rownames(rowSubset) <- rowSubset$x
    rowSubset$x <- NULL
    displayRowNames <- apply(rowSubset,1,paste,collapse = "_")
  } else {
    displayRowNames <- vector(mode="character", length = length(forAnnotations[,"x"]))
  }

  # Set col names
  if (colName != "none"){
    if (any("current level" %in% colName)){ colName[colName %in% "current level"] <- "region" }
    colSubset <- forAnnotations[,c("y",colName)]

    if (length(unique(forAnnotations$hemi))>1){ colSubset <- cbind(forAnnotations[,"hemi"],colSubset) }
    colSubset <- colSubset[!duplicated(colSubset$y),]

    rownames(colSubset) <- colSubset$y
    colSubset$y <- NULL
    displayColNames <- apply(colSubset,1,paste,collapse = "_")
  } else {
    displayColNames <- vector(mode="character", length = length(forAnnotations[,"y"]))
  }

  # Set the direction of the heatmap colors
  directionValue <- -1
  if (invert == T){
    directionValue <- 1
  }

  # Set the color palette
  
  if (colorPalette == "Viridis"){
    colorOption <- variables$viridisOptions
    heatmapColors <- viridis::viridis(breaks, option = colorOption,direction = directionValue)
  } else if (colorPalette == "2 Color"){
    heatmapColors <- colorRampPalette(c(variables$twoCol1,variables$twoCol2))(breaks)
  } else {
    heatmapColors <- colorRampPalette(c(variables$threeCol1,variables$threeCol2,variables$threeCol3))(breaks)
  }

  # Make sure all annotation columns are characters and properly labeled
  annoXRowNames <- rownames(annoX)
  annoX <- as.data.frame(annoX %>% mutate(across(everything(), as.character)))
  rownames(annoX) <- annoXRowNames

  annoYRowNames <- rownames(annoY)
  annoY <- as.data.frame(annoY %>% mutate(across(everything(), as.character)))
  rownames(annoY) <- annoYRowNames

  # After trimming: Make sure anno y only has the rows in the data
  annoY <- annoY[rownames(annoY) %in% colnames(data),]

  # Order annoY by hemi and then region (ABA order)
  annoY$regionOrder <- match(annoY[,1],tree$region)
  annoY$hemiOrder <- match(annoY$hemi,c("left","right"))
  annoY <- annoY[order(annoY$hemiOrder,annoY$regionOrder,decreasing = F) ,]
  annoY$regionOrder <- NULL
  annoY$hemiOrder <- NULL

  # Remove hemi if there is only one
  if (length(unique(annoY$hemi)) < 2){
    annoY$hemi <- NULL
    fullColors <- fullColors[!names(fullColors) %in% "hemi"]
  }

  # Order the data by annoY's new order
  orderKey <- data.frame(dataCols = colnames(data))
  orderKey$dataInOrder <- match(orderKey$dataCols,rownames(annoY))
  orderKey$annoYRows <- rownames(annoY)[orderKey$dataInOrder]
  dataUpdated <- rbind(orderKey$dataInOrder,data)
  rownames(dataUpdated)[1] <- c("dataInOrder")
  dataUpdated <- dataUpdated[,order(orderKey$dataInOrder,decreasing = F)]
  data <- dataUpdated[-1,]

  # Save an option to not include the region legend
  annoYWithout <- annoY[,!colnames(annoY) %in% ABALevels, drop = FALSE]

  # Order the col and row names by anno x and anno y
  displayColNames <- displayColNames[names(displayColNames) %in% rownames(annoY)]
  displayRowNames <- displayRowNames[names(displayRowNames) %in% rownames(annoX)]
  displayColNames <- displayColNames[order(match(names(displayColNames),rownames(annoY)))]
  displayRowNames <- displayRowNames[order(match(names(displayRowNames),rownames(annoX)))]

  # Set font size
  rowFont <- 3.5
  colFont <- 3.5

  # Create heatmap for base plot
  baseHeatmap <- pheatmap(data,fontsize_row = rowFont,fontsize_col = colFont,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                       labels_col = displayColNames,labels_row = displayRowNames, color = heatmapColors, border_color=NA,
                       annotation_row = annoX,annotation_col = annoY, annotation_colors = fullColors)


  # Create heatmap for legend without ABA regions
  if ("hemi" %in% colnames(annoY)){
  legendHeatmap <- pheatmap(data,fontsize_row = rowFont,fontsize_col = colFont,cluster_rows = F, main = "Both", 
                        breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5,
                        labels_col = displayColNames,labels_row = displayRowNames, color = heatmapColors, border_color=NA,
                        annotation_row = annoX, annotation_col = annoYWithout, annotation_colors = fullColors)
  } else {
  legendHeatmap <- pheatmap(data,fontsize_row = rowFont,fontsize_col = colFont,cluster_rows = F, main = "Both", 
                        breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5,
                        labels_col = displayColNames,labels_row = displayRowNames, color = heatmapColors, border_color=NA, 
                        annotation_row = annoX, annotation_colors = fullColors)
  }

  # Combine both heatmaps!

  # Pull out the legend
  legendOnly <- RemovePartOfPlot(legendHeatmap, doKeep = TRUE, thePart = "legend")

  # Pull out the plot
  plotOnly <- RemovePartOfPlot(baseHeatmap, doKeep = FALSE, thePart = "legend")

  # layer the two
  grid.newpage()
  grid.draw(plotOnly)
  grid.draw(legendOnly)
  fullPlot <- grid.grab()

  # No row and col names and legend for quick display plot
  displayBasePlot <- pheatmap(data, cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 3,cellheight = 3,
                       show_rownames = F, show_colnames = F, 
                       color = heatmapColors,border_color=NA,
                       annotation_row = annoX,annotation_col = annoY, annotation_colors = fullColors)
  
  displayBasePlot <- RemovePartOfPlot(displayBasePlot, doKeep = FALSE, thePart = "legend")

  return(list(data, displayBasePlot, fullPlot))
}


