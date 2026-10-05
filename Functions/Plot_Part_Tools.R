# ___________________ ----
# Grid plot tools ----

# These were part of the old "Updated Shiny RMD Standards.R" file and are not
# included in EZRShiny, so they are kept here. RemovePartOfPlot is used by
# "3 - Normal Heatmap.R".

CheckPlotPartLabels <- function(thePlot){
  grid.newpage()
  grid.draw(thePlot)
  grid.force()

  # Get the tree making up the grid
  gridTree <- grid.ls()
  # Pull out full paths of each element
  allPaths <- gridTree$gPath

  return(allPaths)
}

RemovePartOfPlot <- function(thePlot, doKeep, thePart){
  grid.newpage()
  grid.draw(thePlot)
  grid.force()

  # Get the tree making up the grid
  see <- grid.ls()
  # Pull out full paths of each element
  testing <- see$gPath

  # If doKeep, then select all except the part
  if (doKeep){
    removeParts <- testing[!str_detect(testing, thePart) ]
  } else {
    removeParts <- testing[str_detect(testing, thePart) ]
  }

  # Split out by :: and remove the "layout"
  #   "layout" only exists in the full paths
  splitParts <- unlist(strsplit(removeParts, split='::', fixed=TRUE))
  removeParts <- splitParts[!str_detect(splitParts, "layout") ]

  # For each part to remove, try removing it
  for (item in removeParts) {
    tryCatch(
      expr = {
        grid.remove(item)
      },
      error = function(e){
        message('Caught an error!')
      },
      warning = function(w){
        message('Caught a warning!')
      },
      finally = {
        message('All done, quitting.')
      }
    )
  }

  # Grab what is left and return it
  return(grid.grab())
}

ViewPlotLayout <- function(thePlot){
  grid.newpage()
  grid.draw(thePlot)
  grid.force()
  toUse <- grid.grab()

  # Get plot layout
  look1 <- grid.get("layout")
  vpLayout <- look1[["vp"]][[2]][["layout"]]

  # View the plot layout
  grid.show.layout(vpLayout)

  message("If manipulating cols adjust the following in vpLayout")
  message("ncol, widths, respect.mat")

  message("")

  message("If manipulating rows adjust the following in vpLayout")
  message("nrow, heights, respect.mat")

  message("")

  message("Once you adjust the vpLayout, see example code below function for how to plot it.")

  # This example code removes the 6th column where the legend is contained for a pheatmap plot

  # grid.newpage()
  # grid.draw(originalPlot)
  # grid.force()
  # toUse <- grid.grab()
  # look1 <- grid.get("layout")
  # vpLayout <- look1[["vp"]][[2]][["layout"]]
  # vpLayout$ncol <- as.integer(5)
  # vpLayout$widths <- vpLayout$widths[-c(6)]
  # vpLayout$respect.mat <- vpLayout$respect.mat[,-c(6)]
  # grid.show.layout(vpLayout)
  # grid.newpage()
  # pushViewport(viewport(layout = vpLayout, name = "testing" ))
  #
  # look <- getGrob(toUse, "layout")
  # theGrobs <- look[["grobs"]]
  # while(length(grep("legend",theGrobs)) >0) {
  #     theGrobs[[grep("legend",theGrobs)[1]]] <- NULL
  # }
  #
  # for (i in 1:length(theGrobs)) {
  #     grid.draw(theGrobs[[i]])
  # }
  #
  # noExtras <- grid.grab()

  return(vpLayout)
}
