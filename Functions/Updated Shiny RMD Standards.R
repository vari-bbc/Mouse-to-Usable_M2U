# ___________________ ----
# Logs ----

startSection <- function(sectionName){
  print("")
  # print("----")
  # list_obj_sizes()
  print(paste0("----", sectionName, " Start"))
}

endSection <- function(sectionName){
  # print("----")
  # list_obj_sizes()
  print(paste0("----", sectionName, " Done"))
}


# ___________________ ----
# Initial Load ----

cardHeight <<- "85vh"
options(shiny.maxRequestSize=300*1024^2)
options(java.parameters = "-XmX1000000m")

bs_theme(version = 5,primary = "#005596",preset = "cosmo",
         `enable-rounded` = TRUE)


# ___________________ ----
# Parse Elements ----

# For making vectors of shiny elements into UI elements

# Format:
# tabElements(
#   c("ElementType","ElementID","ElementLabel", "TooltipText","Extra1", "Extra2"),
#   c(same here....)
# )

# 
# tabElements <- function(...){
#   allTabElements <- list(...)
#   
#   elementTypes <- sapply(allTabElements, function(x) x[1])
#   elementIDs <- sapply(allTabElements, function(x) x[2])
#   elementLabels <- sapply(allTabElements, function(x) x[3])
#   tooltipTexts <- sapply(allTabElements, function(x) x[4])
#   extra1s <- sapply(allTabElements, function(x) x[5])
#   # the following are only for selects
#   extra2s <- sapply(allTabElements, function(x) x[6])
#   extra3s <- sapply(allTabElements, function(x) x[7])
# 
#   lapply(1:length(elementTypes),function(i){
#     switch(elementTypes[i],
#       "Button" = navButton(elementIDs[i], elementLabels[i], tooltipTexts[i]),
#       
#       "Select" = navSelect(elementIDs[i], elementLabels[i], tooltipTexts[i], 
#                            extra1s[i], extra2s[i], extra3s[i]),
#       
#       "Upload" = navUpload(elementIDs[i], elementLabels[i], tooltipTexts[i], 
#                            extra1s[i]),
#       
#       "Download" = navDownload(elementIDs[i], elementLabels[i], tooltipTexts[i]),
#       
#       "Text" = navText(elementIDs[i], elementLabels[i], tooltipTexts[i]),
#       
#       "Numeric" = navNumeric(elementIDs[i], elementLabels[i], tooltipTexts[i]),
#       
#       "Checkbox" = navCheckbox(elementIDs[i], elementLabels[i], tooltipTexts[i], 
#                                extra1s[i]),
#       
#       "Color" = navColor(elementIDs[i], elementLabels[i], tooltipTexts[i]),
#       
#       "Text" = navText(elementIDs[i], elementLabels[i], tooltipTexts[i]),
#       
#       "Line" = hr(),
#       
#       "OutputTable" = navOutputTable(elementIDs[i], tooltipTexts[i]),
#       
#       "OutputPlot" = navOutputPlot(elementIDs[i], tooltipTexts[i]),
#       
#       "OutputPlotly" = navOutputPlotly(elementIDs[i], tooltipTexts[i]),
#       
#       "OutputPic" = navOutputPic(elementIDs[i], tooltipTexts[i]),
#       
#       "OutputText" = navOutputText(elementIDs[i], tooltipTexts[i]),
#       
#       print(paste0(elementTypes[i]," is not a valid element type."))
#     )
#   })
# }


# For taking a list of elements and outputting them in an RMD

# Format: RMDTabElements(list(plot1,plot2))

RMDTabElements <- function(tabContent){
  
  lapply(tabContent, function(item) {
    item_class <- class(item)[1]
    
    if (item_class == "gg" && class(item)[2] == "ggplot") {
      rmdNavOutputPlot(item) # rmdNavOutputPlotly(item)
    } else if (item_class == "data.frame") {
      rmdNavOutputTable(item)
    } else if (item_class == "character") {
      if (grepl("\\.png|\\.svg", item, ignore.case = TRUE)) {
        rmdNavOutputPic(item)
      } else {
        rmdNavOutputText(item)
      }
    } else {
      print(paste0("The item is not an available item class. It is currently: ", item_class))
      rmdNavOutputText("")
    }
  })
}


# ___________________ ----
# Load functions ----

sourceFunctions <- function(functionFolderPath) {
  #Updates the function path to ensure it ends with a "/"
  if (!grepl("/$", functionFolderPath)){
    functionFolderPath <- paste0(functionFolderPath, "/")
  }
  
  # List all R files in the specified folder
  rFiles <- list.files(path = functionFolderPath, pattern = "\\.R$", full.names = TRUE)
  rFiles <- lapply(rFiles, function(x) gsub("//", "/", x))
  
  # Source each R file
  for (file in rFiles) {
    source(here::here(file))
  }
}


# ___________________ ----
# Nav Inputs and Outputs ----

addTooltip <- function(inputLabel,tooltipText){
  if (is.na(tooltipText)){
    return(inputLabel)
  } else {
    return(list(tooltip(trigger = list(inputLabel, bs_icon("info-circle")), tooltipText)))
  }
}

navDate <- function(inputID, inputLabel, isRange, tooltipText = NA){
  labelText <- addTooltip(inputLabel, tooltipText)
  
  if (isRange){
    airDatepickerInput(inputID, labelText, range = T, 
                       clearButton = T, separator = " to ", addon = "none")
  } else {
    airDatepickerInput(inputID, labelText, range = F, 
                       clearButton = T, addon = "none")
  }
  
}

navButton <- function(inputID, inputLabel, tooltipText = NA){
  labelText <- addTooltip(inputLabel, tooltipText)
  input_task_button(width = "100%", inputID, labelText, type = "default")
}

navSelect <- function(inputID, inputLabel, SingleMulti, CreateLocked, theChoices = NULL, selected = theChoices[1], tooltipText = NA){
  labelText <- addTooltip(inputLabel, tooltipText)
  
  if(grepl("S",SingleMulti, ignore.case = T)){
    # If contains capital C or the word create, allow creation of new items
    if (grepl("C",CreateLocked) | (grepl("create",CreateLocked))){
      selectizeInput(width = "100%", inputID, labelText, choices = theChoices,
                     selected = selected, multiple = F, options = list(create = T))
    } else {
      selectizeInput(width = "100%", inputID, labelText, choices = theChoices,
                     selected = selected, multiple = F, options = list(create = F))
    }
  } else {
    if (grepl("C",CreateLocked) | (grepl("create",CreateLocked))){
      selectizeInput(width = "100%", inputID, labelText, choices = theChoices,
                     selected = selected, multiple = T,options = list(create = T))
    } else {
      selectizeInput(width = "100%", inputID, labelText, choices = theChoices,
                     selected = selected, multiple = T, options = list(create = F))
    }
  }
}

navUpload <- function(inputID, inputLabel, SingleMulti, tooltipText = NA){
  labelText <- addTooltip(inputLabel, tooltipText)
  
  if(grepl("S",SingleMulti, ignore.case = T)){
    fileInput(width = "100%",inputID, labelText, multiple = F)
  } else {
    fileInput(width = "100%",inputID, labelText, multiple = T)
  }
}

navCheckbox <- function(inputID, inputLabel, TrueFalse, tooltipText = NA){
  labelText <- addTooltip(inputLabel, tooltipText)
  
  if (grepl("T",TrueFalse, ignore.case = T)){
    input_switch(width = "100%", inputID, labelText, value = T)
  } else {
    input_switch(width = "100%", inputID, labelText, value = F)
  }
}

navDownload <- function(inputID, inputLabel, tooltipText = NA){
  labelText <- addTooltip(inputLabel, tooltipText)
  
  downloadButton(width = "100%",inputID,labelText)
}

navText <- function(inputID, inputLabel, tooltipText = NA){
  labelText <- addTooltip(inputLabel, tooltipText)
  
  textInput(width = "100%",inputID,labelText)
}

navNumeric <- function(inputID, inputLabel, theValue = 1, tooltipText = NA){
  labelText <- addTooltip(inputLabel, tooltipText)
  
  numericInput(width = "100%",inputID,labelText, value = theValue)
}

navColor <- function(inputID, inputLabel, colorValue = "white", tooltipText = NA){
  labelText <- addTooltip(inputLabel, tooltipText)
  
  colourInput(width = "100%",inputID,labelText, value = colorValue)
}

navSpanText <- function(inputLabel, tooltipText = NA){
  span(inputLabel, tooltip(bs_icon("info-circle"), tooltipText, placement = "bottom"))
}

navOutputTable <- function(outputID, tooltipText = NA){
  DTOutput(outputID)
}

navOutputPlot <- function(outputID, tooltipText = NA){
  plotOutput(outputID, width = "80%")
}

navOutputPlotly <- function(outputID, tooltipText = NA){
  plotlyOutput(outputID, width = "80%", height = "100vh")
}

navOutputGirafe <- function(outputID, tooltipText = NA){
  girafeOutput(outputID, width = "80%", height = "100vh")
}

navOutputPic <- function(outputID, tooltipText = NA){
  imageOutput(outputID, width = "80vw", height = "100vh")#, height = "100%")
}

navOutputText <- function(outputID, tooltipText = NA){
  textOutput(outputID)
}


# ___________________ ----
# RMD Outputs ----

rmdNavOutputPlotly <- function(thePlot){
  card_body(plotly::ggplotly(thePlot), fill = F, width = "75%")
}

rmdNavOutputPlot <- function(thePlot){
  x <- ggiraph::girafe(ggobj = thePlot)
  x <- girafe_options(x, opts_sizing(rescale = F,width = 1))
  
  card_body(x, fill = F, width = "75%")
}

rmdNavOutputPic <- function(picPath){
  card_image(picPath, width = "80%")
}

rmdNavOutputTable <- function(theData,caption = NULL){
  datatable(theData, caption = caption, width = "100%")
}

rmdNavOutputText <- function(theText){
  card_body(markdown(theText),fill = F, width = "75%")
}


# ___________________ ----
# Shiny Nav Base ----

UINav <- function( ...,logoFile = ""){
  page_fluid(
    useShinyjs(),
    navPadding(),
    navBar(
      
      # Logo and title (Must be: png, svg, jpg, or gif)
      navItem(img(src="VAI 2 Line White.png", height = "35vh")),
      navItem(img(src=logoFile, height = "40vh")),
      navItem(h3(appName)),
      
      
      # App Contents
      ...,
      
      
      # Dark Mode
      nav_spacer(),
      navDarkSwitch()
    )
  )
}


# ___________________ ----
# RMD Nav Base ----

UINavRMD <- function(appName,authorName, ...,logopath = ""){
  page_fluid(
    useShinyjs(),
    navPadding(),
    navBar(
      
      # Logo and title
      navItem(img(src="VAI 2 Line White.png", height = "35vh")),
      navItem(img(src=logopath, height = "40vh")),
      navItem(h3(appName)),
      navItem(p(paste0(authorName," - ", Sys.Date()))),
      
      
      # App Contents
      ...,
      
      
      # Dark Mode
      nav_spacer(),
      navDarkSwitch()
    )
  )
}


# ___________________ ----
# Basic Nav Pieces ----

navPadding <- function(){
  div(style = "padding: 5px 0px;")
}

navBar <- function(...){
  navset_bar(id = "root",padding = c("10px","0px"),bg = "#005596",
    ...
  )
}

navDarkSwitch <- function(mode = "dark"){
  nav_item( input_dark_mode(mode = mode) )
}

navItem <- function(...){
  nav_item(div(style="padding: 0px 10px", ...))
}


# ___________________ ----
# Nav Tab Levels ----

# Highest possible level of tab
# Input: tri sub tabs
triLevelTab <- function(title, ..., icon = NULL){
  nav_menu(title, icon = icon, ...)
}

# Input: sub tabs
biLevelTab <- function(title, ..., icon = NULL,id = title){
  id <- gsub(" ","", id)
  nav_panel(title, icon = icon, navset_card_underline( height = cardHeight, id = id, ...))
}

# Standalone sidebar
# Input: sidebar elements (inputs) and inputs/outputs
sidebarLevelTab <- function(title, sidebarElements = NULL, ..., icon = NULL){
  nav_panel(title = title, icon = icon, card( height = cardHeight,
    layout_sidebar(
      sidebar = sidebar(
        sidebarElements
      ),
      ...
    )
  ))
}

# Basic tab
# Input: inputs/outputs
singleTab <- function(title, ..., icon = NULL){
  nav_panel(title, icon = icon, card( height = cardHeight, ...))
}


# ___________________ ----
# Nav Sub Tabs ----

# Sub tab for a tri tab
# Input: sub tabs 
triSubTab <- function(title, ..., icon = NULL,id = title){
  id <- gsub(" ","", id)
  nav_panel(title, icon = icon, navset_card_underline( height = cardHeight, id = id,
    navItem(strong(paste0("- ",title," -"))), ...))
}

# Basic sub tab
# Input: inputs/outputs
subTab <- function(title,...,value = title){
  value <- gsub(" ","", value)
  nav_panel(title,value = value, ...)
}

# A sub sidebar for inside of tri and bi level tabs
# Input: sidebar elements (inputs) and inputs/outputs
subSidebarTab <- function(title, sidebarElements = NULL, ..., value = title, icon = NULL){
  value <- gsub(" ","", value)
  nav_panel(title = title, value = value, icon = icon,
    layout_sidebar(
      sidebar = sidebar(
        sidebarElements
      ),
      ...
    )
  )
}

# Sub Sidebar tab for a tri tab
# Input: sidebar elements (inputs) and sub tabs
triSubSidebarTab <- function(title, sidebarElements = NULL, ..., icon = NULL,id = title){
  id <- gsub(" ","", id)
  nav_panel(title = title, icon = icon,
    layout_sidebar(
      sidebar = sidebar(
        sidebarElements
      ),
      navset_card_underline( height = cardHeight, id = id,
        navItem(strong(paste0("- ",title," -"))), ...
      )
    )
  )
}

# A (sub) page with two columns
# Input: left side (inputs/outputs) and right side (inputs/outputs)
subTwoColPage <- function(leftSide, rightSide) {
  layout_columns(
    col_widths = c(6, 6),
    fillable = F,
    nav_item(leftSide),
    nav_item(rightSide)
  )
}


# ___________________ ----
# RMD Nav Sections ----

navDynamicSubTabsRMD <- function(tabContents){
  tabList <- list()
  for (i in 1:length(tabContents)){
    tabList[[i]] <- navSubTab(names(tabContents)[i],
      RMDTabElements(tabContents[[i]])
    )
  }
  return(tabList)
}


# ___________________ ----
# Deactivate and Activate Items ----

deactivateItems <- function(itemIDs){
  for (i in 1:length(itemIDs)){
    toggleState(id = itemIDs[i], condition = F)
  }
}

activateItems <- function(itemIDs){
  for (i in 1:length(itemIDs)){
    toggleState(id = itemIDs[i], condition = T)
  }
}


# ___________________ ----
# Hide and Show Tabs ----

hideNavTabs <- function(rootID, tabIDs){
  rootID <- gsub(" ","", rootID)
  tabIDs <- unlist(lapply(tabIDs, function(x) gsub(" ", "", x)))
  
  for (i in 1:length(tabIDs)){
    nav_hide(rootID, tabIDs[i])
  }
}

showNavTabs <- function(rootID, tabIDs){
  
  rootID <- gsub(" ","", rootID)
  tabIDs <- unlist(lapply(tabIDs, function(x) gsub(" ", "", x)))
  
  for (i in 1:length(tabIDs)){
    nav_show(rootID, tabIDs[i])
  }
  # Gets it to load
  nav_select(rootID, tabIDs[2])
  nav_select(rootID, tabIDs[1])
  
}


# ___________________ ----
# Helpful functions ----

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
  # View(vpLayout)

  print("If manipulating cols adjust the following in vpLayout")
  print("ncol, widths, respect.mat")

  print("")

  print("If manipulating rows adjust the following in vpLayout")
  print("nrow, heights, respect.mat")

  print("")

  print("Once you adjust the vpLayout, see example code below function for how to plot it.")
  
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

