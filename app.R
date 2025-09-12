
#GitHub

# 1.0 App Startup ----

## 1.1 Load Libraries ----
# Load all required libraries for the Shiny app
library(shiny)
library(bslib)
library(shinyjs)
library(bsicons)
library(plotly)
library(DT)
library(readr)
library(tidytable)
library(colourpicker)
library(pheatmap)
library(grid)
library(ggnewscale)
library(stringr)
library(viridis)
library(svglite)
library(tibble)
library(splitstackshape)


## 1.2 Load Functions ----
# Source custom R scripts containing helper functions for the app
functionPath <<- "Functions/" #"Functions/"

source(here::here(paste0(functionPath, "Shiny Standards.R")))
source(here::here(paste0(functionPath, "Anatomical Plotting.R")))
source(here::here(paste0(functionPath, "M2U Functions.R")))


## 1.3 Universal Vars ----
# Define global variables used throughout the app
appName <<- "M2U"
cardHeight <<- "85vh"
tree <<- readRDS("Necessary Files/flippedTreeMouse.rds")
template <<- read.csv("Necessary Files/Non-Nutil Template.csv")
recCols <<- c("mouse","sex","treatment","mpi","genotype","marker","batch")
baseCols <<- c("fileName", "region", "ABAID", "hemi")
thePalette <<- readRDS("Necessary Files/colorPalette.rds")


# . ----
# 2.0 UI ----
# Define the user interface using bslib and custom navigation helpers
ui <- page_fluid(
  
  useShinyjs(),

  navPadding(),
  navBar(

    ### 2.1 Logo and Title ----
    # Display logos in the navigation bar
    navItem(img(src="VAI 2 Line White.png", height = "35vh")),
    navItem(img(src="Small M2U logo.png", height = "40vh")),

    ### 2.2 File Import ----
    # File import tab with sub-tabs for different import/annotation steps
    navDoubleLevelTab("File Import", id = "FileImport",
      nav_item(div(style="padding: 10px 0px",
        popover(
          bs_icon("files"),
          placement = "bottom",
          multiElement(
            elementType = c("MultiUpload","Text","Button"),
            elementID = c("changeFiles","changeName","changeButton"),
            elementLabel = c("Select files to modify",
                             "Text to add to begining of file name",
                             "Modify file names")
          )
        )
      )),
      navSubTab("Initial Import","initialImport",
        multiElement(
          elementType = c("Download","MultiUpload","Download"),
          elementID = c("nnTemplate","fileInput","downloadRawData"),
          elementLabel = c("Download the Template for Non-Nutil Data",
                           "Select All Files to Import",
                           "Download the Raw Data")
        )
      ),
      navSubTab("Annotation Creation","annotationCreation",
        multiElement(
          elementType = c("MultiUpload","CheckboxF","MultiLockedSelect","MultiCreateSelect","Button","Download"),
          elementID = c("rawAnnoInput","singleHemi","annoRecOptions","annoCustomOptions","createAnnoFile","downloadAnnoFile"),
          elementLabel = c("Input Raw Data if needed",
                           "Single Hemisphere (Defaults to right)",
                           "Recommended Options",
                           "Custom Options",
                           "Create Annotation File",
                           "Download the Blank Annotation File")
        )
      ),
      navSubTab("Merge Data and Annotations","mergeData",
        multiElement(
          elementType = c("SingleUpload","SingleUpload","Download"),
          elementID = c("rawDataInput","annoFileInput","downloadCheckpointData"),
          elementLabel = c("Select your Raw Data File",
                           "Select your completed Annotation File",
                           "Download the Checkpoint for the Data")
        )
      ),
      navSubTab("Checkpoint Start","checkpointStart",
        multiElement(
          elementType = c("SingleUpload","SingleUpload"),
          elementID = c("checkpointInput","VarInput"),
          elementLabel = c("Select your Checkpoint File",
                           "Select your Variable File")
        )
      )
    ),


    ### 2.3 Variable Selection ----
    # Tab for selecting variables, transformations, and color schemes
    navDoubleLevelTab("Variable Selection", id = "VariableSelection",
      navSubTab("Layout Selection","layoutSelection",
        multiElement(
          elementType = c("MultiLockedSelect","SingleLockedSelect","MultiLockedSelect","MultiLockedSelect","SingleLockedSelect"),
          elementID = c("AoI","regionLevel","legendVar","rowVars","colVars"),
          elementLabel = c("Select Annotation(s) of Interest (AoI)",
                           "Select Region Level",
                           "Select Legend Variables (based on selected region level)",
                           "Select Row Names",
                           "Select Column Names (based on selected region level)")
        ),
        #
        
      ),
      navSubTab("Data and Transformation","dataTransformation",
        multiElement(
          elementType = c("SingleLockedSelect","CheckboxF","CheckboxF","CheckboxF","CheckboxT","MultiLockedSelect"),
          elementID = c("valueVar","logged","multiPercent","deviPercent","trim","regionsToRemove"),
          elementLabel = c("Select Column to get Data from",
                           "Log Transform",
                           "Multiply by 100",
                           "Divide by 100",
                           "Trim",
                           "Regions to ignore")
        )
      ),
      navSubTab("Color Scheme","colorScheme",
        multiElement(
          elementType = c("CheckboxF","OutputText","Numeric","Numeric","SingleLockedSelect"),
          elementID = c("invert","minMaxText","minVal","maxVal","colorPalette"),
          elementLabel = c("Invert the Color Scheme",
                           "",
                           "Set the Minimum Value of Scale",
                           "Set the Maximum Value of Scale",
                           "Select Color Palette")
        ),
        uiOutput("colorOptions")
      ),
      navSubTab("Variable Set and Save","variableSet",
        multiElement(
          elementType = c("Button","Download","Download"),
          elementID = c("varSetButton","saveVar","normDataDownload"),
          elementLabel = c("Finalize the Variables",
                           "Download the Variable Save",
                           "Download Normal Plot Data")
        )
      )
    ),
    
    
    ### 2.4 data preview ----
    # Tab for previewing data as heatmaps
    navDoubleLevelTab("Data Preview", id = "DataPreview",
      navSubSidebarTab("Normal Heatmap","normalHeatmap",
        sidebar = multiElement(
          elementType = c("Button","Download","Download"),
          elementID = c("normHeatmap","normHeatmapDownload","normJustPlotDownload"),
          elementLabel = c("Create normal heatmap",
                           "Download the data preview heatmap",
                           "Download the heatmap without the legend")
        ),
        navOutputPic("normPlot")
        
      ),
      navSubSidebarTab("Anatomical Heatmap","anatomicalHeatmap",
        sidebar = multiElement(
          elementType = c("MultiLockedSelect","Button","Download"),
          elementID = c("slices","anatomicalHeatmap","anatomicalHeatmapDownload"),
          elementLabel = c("Select Allen Brain Atlas slices to plot",
                           "Create the anatomical heatmap",
                           "Download the anatomical heatmap (click once and then wait)")
        ),
        navOutputPic("anatomicalPlot")
        
      )
    ),

    ## 2.5 Dark Mode ----
    # Add dark mode toggle
    nav_spacer(),
    navDarkSwitch()
  )

)


# . ----
# 3.0 Server ----
# Define the server logic for the Shiny app
server <- function(session, input, output) {

    ## 3.1 Global Vars ----
    # Create a reactiveValues object to store global app state
    global <- reactiveValues(

        dataFrames = list(
            rawData = c(), annoFile = c(), preVar = c(), fullData = c()
        ),
        info = list(
            numericCols = c(), recAnnoCols = c(), customAnnoCols = c(),
            annoCheck1 = F, annoCheck2 = F
        ),
        variables = list(
            #layout
            AoI = c(), regionLevel = c(), legendVar = c(), 
            rowVars = c(), colVars = c(),
            #dataTrans
            valueVar = c(), logged = F, multiPercent = F, deviPercent = F,
            trim = T, regionsNoData = c("fiber tracts"),
            #scaleOptions
            invert = F, minVal = 0, maxVal = 1,
            colorPalette = c(),
            #colorScheme
            viridisOptions = c(),
            twoCol1 = c(), twoCol2 = c(),
            threeCol1 = c(), threeCol2 = c(), threeCol3 = c()
        )

    )

    ## 3.2 Run on Start ----
    # Initial setup: hide tabs, deactivate buttons, set default selectize values
    observe({
        startSection("Start")
        
        # Hide Tabs
        hideNavTabs(rootID = "VariableSelection", 
                    tabIDs = c("layoutSelection",
                            "dataTransformation",
                            "colorScheme",
                            "variableSet"))
        
        hideNavTabs(rootID = "DataPreview",
                    tabIDs = c("normalHeatmap",
                            "anatomicalHeatmap"))
        
        #Deactivate buttons that need things to function
        deactivateItems(c("createAnnoFile",
                        "twoCol1","twoCol2","threeCol1","threeCol2","threeCol3"))
        #all downloads
        deactivateItems(c(
          "downloadRawData","downloadAnnoFile","downloadCheckpointData",
          "saveVar","normDataDownload","normHeatmapDownload",
          "normJustPlotDownload","anatomicalHeatmapDownload"
        ))
        
        # Update selectize inputs with initial values
        updateSelectizeInput(session, "annoRecOptions", choices = recCols, selected = recCols)
        updateSelectizeInput(session, "regionLevel", choices = c("daughter", "parent", "major"), selected = "daughter")
        updateSelectizeInput(session, "legendVar", choices = c("current level", "parent", "major"), selected = c("parent", "major"))
        updateSelectizeInput(session,"colVars",choices = c("none","current level", "parent", "major"),selected = "none")
        updateSelectizeInput(session, "colorPalette", choices = c("Viridis", "2 Color", "3 Color"), selected = "Viridis")
        updateSelectizeInput(session, "regionsToRemove", choices = tree$region, selected = "fiber tracts")
        updateSelectizeInput(session, "slices", choices = c(1:132), selected = c(32,46,67,75,82,96))
        
        
        endSection("Start")
    })

    ## 3.3 File Import ----

    # Template Download
    # Allow user to download the template for non-Nutil data
    output$nnTemplate <- downloadHandler(
        filename = function() {
        paste("Non-Nutil Template.csv")
        },
        content = function(file) {
        write.csv(template, file, row.names = FALSE)
        }
    )

    # File Import
    # Handle file import and parsing for both Nutil and non-Nutil data
    observeEvent(input$fileInput, {
        startSection("File Import")

        # Inputs
        the_files <- input$fileInput

        fileNames <- the_files$name
        paths <- the_files$datapath

        treeKey <- tree[, c(1, 2)]
        firstLine <- readLines(paths[1], n = 1)
        rawData <- c()

        if (grepl("Region ID;", firstLine)) {
            # Nutil format: parse and combine all files
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
            rawData <- rawData[rawData$`Region pixels` != 0,]
            colnames(rawData)[1:2] <- c("fileName", "ABAID")
            rawData <- rawData[, c("fileName", "ABAID", "Region area", "Object count", "Object area")]
            rawData$ABAID <- as.character(rawData$ABAID)
            rawData <- left_join(rawData, treeKey, by = "ABAID")
        } else {
            # Non-Nutil format: parse and combine all files
            for (i in 1:length(paths)) {
                toNormal <- read.csv(paths[i])
                name <- fileNames[i]
                toAdd <- data.frame(fileNames[i], toNormal)
                rawData <- rbind(rawData, toAdd)
            }
            colnames(rawData)[1] <- c("fileName")
            rawData <- left_join(rawData, treeKey, by = "region")
        }
        rawData <- rawData

        numericCols <- colnames(rawData)[!colnames(rawData) %in% baseCols]

        # App interactions
        enable("createAnnoFile")
        enable("downloadRawData")
        output$downloadRawData <- downloadHandler(
            filename = function() {
                paste("RawData.csv")
            },
            content = function(file) {
              disable("downloadRawData")
              write.csv(rawData, file, row.names = FALSE)
            }
        )

        # Save globals
        global$info$numericCols <- numericCols
        global$dataFrames$rawData <- rawData

        endSection("File Import")
    })

    # Raw Annotation File Import
    # Import a raw annotation file and update global state
    observeEvent(input$rawAnnoInput, {
        startSection("Annotation Raw File Import")
        
        # Inputs
        rawDataPath <- input$rawAnnoInput$datapath

        rawData <- read.csv(rawDataPath)
        numericCols <- colnames(rawData)[!colnames(rawData) %in% baseCols]

        # App interactions
        enable("createAnnoFile")

        #saved globals
        global$dataFrames$rawData <- rawData
        global$info$numericCols <- numericCols
        
        endSection("Annotation Raw File Import")
    })

    # Annotation File Creation
    # Create an annotation file based on user selections and raw data
    observeEvent(input$createAnnoFile, {
        startSection("Annotation Creation")
        
        # Load globals
        rawData <- global$dataFrames$rawData

        # Inputs
        recOptions <- input$annoRecOptions
        customOptions <- input$annoCustomOptions
        singleHemi <- input$singleHemi
        
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

        # App interactions
        enable("downloadAnnoFile")
        output$downloadAnnoFile <- downloadHandler(
            filename = function() {
                paste("AnnotationFile.csv")
            },
            content = function(file) {
                disable("downloadAnnoFile")
                write.csv(annoFile, file, row.names = FALSE)
            }
        )

        # Save globals
        global$dataFrames$annoFile <- annoFile
        
        endSection("Annotation Creation")
    })

    # Read in Raw Data
    # Import raw data and merge with annotation file if both are present
    observeEvent(input$rawDataInput, {
        startSection("Raw Data Import")
        
        # Load globals
        annoCheck1 <- global$info$annoCheck1
        annoCheck2 <- global$info$annoCheck2
        annoFile <- global$dataFrames$annoFile

        # Inputs
        rawDataPath <- input$rawDataInput$datapath

        rawData <- read.csv(rawDataPath)
        numericCols <- colnames(rawData)[!colnames(rawData) %in% baseCols]

        annoCheck1 <- T

        if (annoCheck1 & annoCheck2) {
          rawData <- rawData
          annoFile <- annoFile
          numericCols <- numericCols
          
            mergeItems <- mergeData(rawData, annoFile, numericCols)
            fullData <- as.data.frame(mergeItems[[1]])
            numericCols <- mergeItems[[2]]
            
            enable("downloadCheckpointData")
            output$downloadCheckpointData <- downloadHandler(
                filename = function() {
                  paste("CheckpointData.csv")
                },
                content = function(file) {
                  disable("downloadCheckpointData")
                  fwrite(fullData, file, row.names = FALSE)
                }
            )
            
            # App interactions
            showNavTabs(rootID = "VariableSelection", 
                        tabIDs = c("layoutSelection",
                                   "dataTransformation",
                                   "colorScheme",
                                   "variableSet"))
            numericCols <- sub(" ", ".", numericCols)
            updateSelectizeInput(session, "valueVar", choices = numericCols, selected = numericCols[1])
            
            #remove columns that are all NAs
            fullData <- fullData[ ,colSums(is.na(fullData)) != length(fullData$region)]
            
            # Save globals
            global$dataFrames$preVar <- fullData
            global$info$numericCols <- numericCols
            global$variables$valueVar <- numericCols[1]
        }

        # Save globals
        global$dataFrames$rawData <- rawData
        global$info$annoCheck1 <- annoCheck1
        global$info$numericCols <- numericCols
        
        endSection("Raw Data Import")
    })
    
    # Annotation File Import
    # Import annotation file and merge with raw data if both are present
    observeEvent(input$annoFileInput, {
        startSection("Annotation File Import")
        
        # Load globals
        annoCheck1 <- global$info$annoCheck1
        annoCheck2 <- global$info$annoCheck2
        rawData <- global$dataFrames$rawData
        numericCols <- global$info$numericCols

        # Inputs
        annoDataPath <- input$annoFileInput$datapath

        annoFile <- read.csv(annoDataPath)
        recAnnoCols <- colnames(annoFile)[colnames(annoFile) %in% recCols]
        customAnnoCols <- colnames(annoFile)[!colnames(annoFile) %in% c(recCols,"include","fileName","hemi")]
        annoCols <- c(recAnnoCols, customAnnoCols)
        
        updateSelectizeInput(session, "AoI", choices = annoCols, selected = annoCols[1])
        

        annoCheck2 <- T

        if (annoCheck1 & annoCheck2) {
          rawData <- rawData
          annoFile <- annoFile
          numericCols <- numericCols
          
            mergeItems <- mergeData(rawData, annoFile, numericCols)
            fullData <- as.data.frame(mergeItems[[1]])
            numericCols <- mergeItems[[2]]
            
            enable("downloadCheckpointData")
            output$downloadCheckpointData <- downloadHandler(
                filename = function() {
                  paste("CheckpointData.csv")
                },
                content = function(file) {
                  disable("downloadCheckpointData")
                  fwrite(fullData, file, row.names = FALSE)
                }
            )
        
            # App interactions
            showNavTabs(rootID = "VariableSelection", 
                        tabIDs = c("layoutSelection",
                                   "dataTransformation",
                                   "colorScheme",
                                   "variableSet"))
        
            updateSelectizeInput(session, "valueVar", choices = numericCols, selected = numericCols[1])
            
            #remove columns that are all NAs
            fullData <- fullData[ ,colSums(is.na(fullData)) != length(fullData$region)]
            
            # Save globals
            global$dataFrames$preVar <- fullData
            global$info$numericCols <- numericCols
            global$variables$valueVar <- numericCols[1]
        }

        #saved globals
        global$variables$AoI <- annoCols[1]
        global$dataFrames$annoFile <- annoFile
        global$info$annoCheck2 <- annoCheck2
        global$info$recAnnoCols <- recAnnoCols
        global$info$customAnnoCols <- customAnnoCols
        
        endSection("Annotation File Import")
    })

    # Checkpoint Load
    # Load a previously saved checkpoint file and update app state
    observeEvent(input$checkpointInput, {
        startSection("Checkpoint Load")
        
        # Inputs
        fullDataPath <- input$checkpointInput$datapath

        fullData <- read.csv(fullDataPath)
        
        #remove columns that are all NAs
        fullData1 <- fullData[ ,colSums(is.na(fullData)) != length(fullData$region)]

        dataSplit <- which(colnames(fullData1) %in% c("region", "ABAID"))[2]

        rawDataHalf <- fullData1[, 1:(dataSplit - 1)]
        numericCols <- colnames(rawDataHalf)[!colnames(rawDataHalf) %in% baseCols]

        annoDataHalf <- fullData1[, (dataSplit + 1):length(colnames(fullData1)),drop=FALSE]
        recAnnoCols <- colnames(annoDataHalf)[colnames(annoDataHalf) %in% recCols]
        customAnnoCols <- colnames(annoDataHalf)[!colnames(annoDataHalf) %in% c(recAnnoCols,"hemi")]

        annoCols <- c(recAnnoCols, customAnnoCols)

        nutil <- any(grepl("region", numericCols, ignore.case = T) & grepl("area", numericCols, ignore.case = T))
        if (nutil) {
            numericCols <- numericCols[!numericCols %in% ("Load")]
            numericCols <- c("load", "neuriteLoad", "InclusionsPerMM2", numericCols)
        }

        # App interactions
        updateSelectizeInput(session, "AoI", choices = annoCols, selected = annoCols[1])
        updateSelectizeInput(session, "valueVar", choices = numericCols, selected = numericCols[1])
        showNavTabs(rootID = "VariableSelection", 
                    tabIDs = c("layoutSelection",
                            "dataTransformation",
                            "colorScheme",
                            "variableSet"))

        # Save globals
        global$dataFrames$preVar <- fullData1
        global$info$numericCols <- numericCols
        global$info$recAnnoCols <- recAnnoCols
        global$info$customAnnoCols <- customAnnoCols
        global$variables$layout$AoI <- annoCols[1]
        global$variables$dataTrans$valueVar <- numericCols[1]

        endSection("Checkpoint Load")
    })

    # Variable Load
    # Load a previously saved variable set and update UI accordingly
    observeEvent(input$VarInput, {
        startSection("Variable Load")

        variables <- readRDS(input$VarInput$datapath)

        # Layout
        updateSelectizeInput(session, "AoI", choices = recCols, selected = variables$AoI)
        
        
        updateSelectizeInput(session, "regionLevel", selected = variables$regionLevel)
        updateSelectizeInput(session, "legendVar", selected = variables$legendVar)
        updateSelectizeInput(session, "rowVars", selected = variables$rowVars)
        updateSelectizeInput(session, "colVars", selected = variables$colVars)
        
        # Data Transformation
        updateSelectizeInput(session, "valueVar", selected = variables$valueVar)
        update_switch(session = session, "logged", value = variables$logged)
        update_switch(session = session, "multiPercent", value = variables$multiPercent)
        update_switch(session = session, "deviPercent", value = variables$deviPercent)
        update_switch(session = session, "trim", value = variables$trim)
        updateSelectizeInput(session, "regionsToRemove", selected = variables$regionsToRemove)

        # Color Scheme
        update_switch(session = session, "invert", value = variables$invert)
        updateNumericInput(session, "minVal", value = variables$minVal)
        updateNumericInput(session, "maxVal", value = variables$maxVal)
        
        if (length(variables$colorPalette) > 0) {
            updateSelectizeInput(session, "colorPalette", selected = variables$colorPalette)

            # Color Options
            if (variables$colorPalette == "Viridis") {
                updateSelectizeInput(session, "viridisOptions", selected = variables$viridisOptions)
            } else if (variables$colorPalette == "2 Color") {
                updateColourInput(session, "twoCol1", value = variables$twoCol1)
                updateColourInput(session, "twoCol2", value = variables$twoCol2)
            } else {
                updateColourInput(session, "threeCol1", value = variables$threeCol1)
                updateColourInput(session, "threeCol2", value = variables$threeCol2)
                updateColourInput(session, "threeCol3", value = variables$threeCol3)
            }
        } else {
          updateSelectizeInput(session, "colorPalette", selected = "Viridis")
          updateSelectizeInput(session, "viridisOptions", selected = "inferno")
          variables$colorPalette <- "Viridis"
        }
        
        output$colorOptions <- renderUI({
            if (variables$colorPalette == "Viridis"){
                tagList(
                    selectizeInput(width = "100%","viridisOptions","Select Viridis Options",
                                   choices = c("viridis","inferno","magma","plasma",
                                               "cividis","mako","rocket","turbo"),selected = "inferno",
                                   multiple = F,options = list(create = F))
                )
                
            } else if (variables$colorPalette == "2 Color"){
                tagList(
                    colourInput(width = "100%","twoCol1", "Select colour", "white"),
                    colourInput(width = "100%","twoCol2", "Select colour", "red")
                )
                
            } else {
                tagList(
                    colourInput(width = "100%","threeCol1", "Select colour", "white"),
                    colourInput(width = "100%","threeCol2", "Select colour", "orange"),
                    colourInput(width = "100%","threeCol3", "Select colour", "red")
                )
            }
        })

        global$variables <- variables

        endSection("Variable Load")
    })

    ## 3.4 Variable Selection ----

    # Observe and update global variables when user changes variable selections
    observeEvent(input$AoI,{
        print("AoI Change")
        global$variables$AoI <- input$AoI
        AoI <- input$AoI
        theData <- global$dataFrames$preVar
        
        updateSelectizeInput(session, "rowVars", choices = c("none",input$AoI), selected = "none")
    })

    observeEvent(input$regionLevel, {
        print("Region Level Change")
        global$variables$regionLevel <- input$regionLevel
    })

    observeEvent(input$legendVar, {
        print("Legend Change")
        global$variables$legendVar <- input$legendVar
    })
    
    observeEvent(input$rowVars, {
        print("rowVars Change")
        global$variables$rowVars <- input$rowVars
    })
    
    observeEvent(input$colVars, {
        print("colVars Change")
        global$variables$colVars <- input$colVars
    })
    
    observeEvent(input$valueVar, {
        print("ValueVar Change")
        global$variables$valueVar <- input$valueVar
        valueVar <- input$valueVar 
        
        if(valueVar == "load"){
        updateNumericInput(session, "minVal", value = 0)
        updateNumericInput(session, "maxVal", value = 1)
        } else if(valueVar == "neuriteLoad"){
        # add later
        } else if(valueVar == "InclusionsPerMM2"){
        # add later
        } else {
        updateNumericInput(session, "minVal", value = 0)#min(global$dataFrames$preVar[, input$valueVar], na.rm = T))
        updateNumericInput(session, "maxVal", value = 1)#max(global$dataFrames$preVar[, input$valueVar], na.rm = T))
        }
        
    })
    
    observeEvent(input$logged, {
        print("Logged Change")
        global$variables$logged <- input$logged
    })
    
    observeEvent(input$multiPercent, {
        print("multiPercent Change")
        global$variables$multiPercent <- input$multiPercent
    })
    
    observeEvent(input$deviPercent, {
        print("deviPercent Change")
        global$variables$deviPercent <- input$deviPercent
    })
    
    observeEvent(input$regionsToRemove, {
        print("regionsToRemove Change")
        global$variables$regionsToRemove <- input$regionsToRemove
    })
    
    observeEvent(input$invert, {
        print("invert Change")
        global$variables$invert <- input$invert
    })
    
    observeEvent(input$minVal, {
        print("minVal Change")
        global$variables$minVal <- input$minVal
    })
    
    observeEvent(input$maxVal, {
        print("maxVal Change")
        global$variables$maxVal <- input$maxVal
    })
    
    observeEvent(input$trim,{
        print("maxVal Change")
        global$variables$trim <- input$trim
    })
    
    observeEvent(input$colorPalette, {
        print("colorPalette Change")
        global$variables$colorPalette <- input$colorPalette
        
        output$colorOptions <- renderUI({
            if (global$variables$colorPalette == "Viridis"){
                tagList(
                selectizeInput(width = "100%","viridisOptions","Select Viridis Options",
                            choices = c("viridis","inferno","magma","plasma",
                                        "cividis","mako","rocket","turbo"),selected = "inferno",
                            multiple = F,options = list(create = F))
                )
                
            } else if (global$variables$colorPalette == "2 Color"){
                tagList(
                colourInput(width = "100%","twoCol1", "Select colour", "white"),
                colourInput(width = "100%","twoCol2", "Select colour", "red")
                )
                
            } else {
                tagList(
                colourInput(width = "100%","threeCol1", "Select colour", "white"),
                colourInput(width = "100%","threeCol2", "Select colour", "orange"),
                colourInput(width = "100%","threeCol3", "Select colour", "red")
                )
            }
        })
        
    })
    
    observeEvent(input$varInterest, {
      print("varInterest Change")
      global$variables$varInterest <- input$varInterest
    })
    
    ### 3.4.1 Variable Set ----

    #Set Variables
    # Finalize variable selection and calculate normalized data table
    observeEvent(input$varSetButton, {
        startSection("Variable Set")
        
        preVar <- global$dataFrames$preVar
        variables <- global$variables 
        
        # Get the options
        annoCols <- variables$AoI
        
        norm <- calculateValueTable(preVar,variables,annoCols)
        
        norm$hemi <- tolower(norm$hemi)

        # App interactions
        activateItems(c("saveVar","downloadAnnoFile","normDataDownload"))
        
        enable("saveVar")
        output$saveVar <- downloadHandler(
            filename = function() {
                paste("VariableSave.rds")
            },
            content = function(file) {
              disable("saveVar")
              saveRDS(variables, file)
            }
        )
        
        # App interactions
        showNavTabs(rootID = "DataPreview", 
                    tabIDs = c("normalHeatmap",
                            "anatomicalHeatmap"))

        # Save globals
        global$dataFrames$fullData <- norm
        
        
        #update text above min and max
        output$minMaxText <- renderText({
            paste0("Min Value: ", min(norm$preLimVal, na.rm = T), 
                  " | Max Value: ", max(norm$preLimVal, na.rm = T))
        })
        
        enable("normDataDownload")
        output$normDataDownload <- downloadHandler(
            filename = function() {
                paste("normData.csv")
            },
            content = function(file) {
              disable("normDataDownload")
              write.csv(norm, file, row.names = FALSE)
            }
        )
        
        endSection("Variable Set")
        
    })

    ## 3.5 Data Preview ----

    # Observe event for normal heatmap creation and rendering
    observeEvent(input$normHeatmap, {
        startSection("Normal Heatmap")
        
        fullData <- global$dataFrames$fullData
        variables <- global$variables
        
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
        
        #remove none if accidentally left in the rownames and colnames
        if(length(rowName) > 1 & "none" %in% rowName){
          rowName <- rowName[!rowName %in% "none"]
        }
        
        if(length(colName) > 1 & "none" %in% colName){
          colName <- colName[!colName %in% "none"]
        }

        forData <- fullData
        fullDataRemoved <- fullData[!fullData$removeRegion,]

        forData <- forData[!forData$removeRegion,]

        uniqueX <- unique(forData$x)
        uniqueY <- unique(forData$y)

        forData <- forData[,c("x","y","postLimVal")]

        forAnnots <- forData
        forData$uniqueRowID <- NULL

        pivotVTable <- forData %>%
            pivot_wider(names_from=y, values_from=postLimVal, names_repair = "minimal")
        pivotVTable <- as.data.frame(pivotVTable)
        rownames(pivotVTable) <- pivotVTable$x
        pivotVTable$x <- NULL
        data <- pivotVTable
        
        forAnnots <- fullData
        if (length(legendVar) > 1){
          forAnnots$legend <- apply(forAnnots[, legendVar], 1, paste, collapse = "_")
        } else {
          forAnnots$legend <- forAnnots[,legendVar]
        }
        
        annoY <- forAnnots %>% group_by(y,legend,hemi) %>%
            summarise()
        annoY <- as.data.frame(annoY)
        rownamesY <- annoY$y
        hemiY <- annoY$hemi
        annoY$hemi <- NULL
        annoY$y <- NULL
        annoY <- annoY %>% separate_wider_delim(legend, delim = "_", names = legendVar)
        annoY <- as.data.frame(annoY)
        rownames(annoY) <- rownamesY
        annoY$legend <- NULL
        annoY$hemi <- hemiY

        annoX <- forAnnots %>% group_by(x) %>%
            summarise()
        annoX <- as.data.frame(annoX)
        rownamesX <- annoX$x
        annoX <- annoX %>% separate_wider_delim(x, delim = "_", names = annoCols)
        annoX <- as.data.frame(annoX)
        rownames(annoX) <- rownamesX

        if (trim == T){
            data <- data[,colSums(is.na(data)) < nrow(data)*0.5]
        }
        
        print("1")

        breaks <- 20
        difference <- minVal-maxVal
        breakNum <- abs(difference/(breaks+1))
        seqBreaks <- seq(minVal, maxVal, by=breakNum)

        ABALevels <- legendVar
        colorIDs <- tree[,c("region","colorHex")]
        fullDataRemoved <- as.data.frame(fullDataRemoved)
        colColors <- list()
        for (i in 1:length(ABALevels)){
            uniqueRegions <- c(unique(fullDataRemoved[,ABALevels[i]]),"NA")
            levelColors <- colorIDs[match(uniqueRegions, colorIDs$region),2]
            levelColors[levelColors == "NA"] <- "grey"
            levelColors <- setNames(levelColors, as.character(uniqueRegions))
            colColors[[ABALevels[i]]] <- levelColors
        }
        colColors[["hemi"]] <- setNames(c("red","blue"),c("left","right"))
        

        annoX <- as.data.frame(annoX)
        rowColors <- list()
        theStart <- 1
        for (i in 1:length(colnames(annoX))){
            rowItems <- annoX[,i]
            unnamed <- thePalette[theStart:length(rowItems)]
            namedItems <- setNames(unnamed, as.character(unique(rowItems)))
            rowColors[[colnames(annoX)[i]]] <- namedItems
            theStart <- theStart + length(rowItems)
        }

        # annoX <<- annoX
        # annoY <<- annoY
        # forAnnots <<- forAnnots
        # View(forAnnots)
        # rowName <<- rowName
        # colName <<- colName
        
        if(rowName[1] != "none"){
          rowSubset <- forAnnots[,c("x",rowName)]
          rowSubset <- rowSubset[!duplicated(rowSubset$x),]
          rownames(rowSubset) <- rowSubset$x
          rowSubset$x <- NULL
          displayRowNames <- apply(rowSubset,1,paste,collapse = "_")
        } else {
          displayRowNames <- vector(mode="character", length = length(forAnnots[,"x"]))
        }
        
        if (colName != "none"){
          if (any("current level" %in% colName)){ colName[colName %in% "current level"] <- "region" }
          colSubset <- forAnnots[,c("y",colName)]
          if (length(unique(forAnnots$hemi))>1){ colSubset <- cbind(forAnnots[,"hemi"],colSubset) }
          colSubset <- colSubset[!duplicated(colSubset$y),]
          rownames(colSubset) <- colSubset$y
          colSubset$y <- NULL
          displayColNames <- apply(colSubset,1,paste,collapse = "_")
        } else {
          displayColNames <- vector(mode="character", length = length(forAnnots[,"y"]))
        }
        

        directionValue <- -1
        if (invert == T){
            directionValue <- 1
        }
        
        print("2")
        
        if (length(colorPalette) > 0){
            if (colorPalette == "Viridis"){
              if(is.null(input$viridisOptions)){
                colorOption <- "inferno"
              } else {
                colorOption <- input$viridisOptions
              }
              heatmapColors <- viridis::viridis(breaks, option = colorOption,direction = directionValue)
            } else if (colorPalette == "2 Color"){
                heatmapColors <- colorRampPalette(c(input$twoCol1,input$twoCol2))(breaks)
            } else {
                heatmapColors <- colorRampPalette(c(input$threeCol1,input$threeCol2,input$threeCol3))(breaks)
            }
        } else {
            heatmapColors <- viridis::viridis(breaks, option = "inferno",direction = directionValue)
        }
        
        print("3")

        annoYRowNames <- rownames(annoY)
        annoXRowNames <- rownames(annoX)

        annoX <- as.data.frame(annoX %>% mutate(across(everything(), as.character)))
        rownames(annoX) <- annoXRowNames
        annoY <- as.data.frame(annoY %>% mutate(across(everything(), as.character)))
        rownames(annoY) <- annoYRowNames

        fullColors <- append(rowColors, colColors)
        
        #only keep rows in data
        annoY <- annoY[rownames(annoY) %in% colnames(data),]
        
        #order annoY by hemi and then region (according to ABA)
        annoY$regionOrder <- match(annoY[,1],tree$region)
        annoY$hemiOrder <- match(annoY$hemi,c("left","right"))
        annoY <- annoY[order(annoY$hemiOrder,annoY$regionOrder,decreasing = F) ,]
        annoY$regionOrder <- NULL
        annoY$hemiOrder <- NULL
        
        
        #remove hemi if only one hemi
        if (length(unique(annoY$hemi)) < 2){
            annoY$hemi <- NULL
            fullColors <- fullColors[!names(fullColors) %in% "hemi"]
        }
        
        
        print("4")
        
        #order data by annoY
        orderKey <- data.frame(dataCols = colnames(data))
        orderKey$dataInOrder <- match(orderKey$dataCols,rownames(annoY))
        orderKey$annoYRows <- rownames(annoY)[orderKey$dataInOrder]
        #check order
        # orderKey$checkMatch <- orderKey$dataCols == orderKey$annoYRows
        
        dataUpdated <- rbind(orderKey$dataInOrder,data)
        rownames(dataUpdated)[1] <- c("dataInOrder")
        dataUpdated <- dataUpdated[,order(orderKey$dataInOrder,decreasing = F)]
        
        data <- dataUpdated[-1,]

        annoYWithout <- annoY[,!colnames(annoY) %in% ABALevels, drop = FALSE]
        
        # theData <<- data
        # annoY <<- annoY
        # displayColNames <<- displayColNames
        # View(displayColNames)
        
        #sort displayColNames and displayRowNames by annoY and annoX
        displayColNames <- displayColNames[names(displayColNames) %in% rownames(annoY)]
        displayRowNames <- displayRowNames[names(displayRowNames) %in% rownames(annoX)]
        displayColNames <- displayColNames[order(match(names(displayColNames),rownames(annoY)))]
        displayRowNames <- displayRowNames[order(match(names(displayRowNames),rownames(annoX)))]
        
        rowFont <- 3.5
        colFont <- 3.5

        if ("hemi" %in% colnames(annoY)){
        heatmap2 <- pheatmap(data,fontsize_row = rowFont,fontsize_col = colFont,cluster_rows = F, main = "Both", 
                            breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5,
                            labels_col = displayColNames,labels_row = displayRowNames, color = heatmapColors, border_color=NA,
                            annotation_row = annoX, annotation_col = annoYWithout, annotation_colors = fullColors)
        } else {
        heatmap2 <- pheatmap(data,fontsize_row = rowFont,fontsize_col = colFont,cluster_rows = F, main = "Both", 
                            breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5,
                            labels_col = displayColNames,labels_row = displayRowNames, color = heatmapColors, border_color=NA, 
                            annotation_row = annoX, annotation_colors = fullColors)
        }
        
        print("5")
    
        #check displayColNames and displayRowNames to 
        # displayColNames <<- displayColNames
        # displayRowNames <<- displayRowNames
        # theData <<- data
        
        heatmap1 <- pheatmap(data,fontsize_row = rowFont,fontsize_col = colFont,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                            labels_col = displayColNames,labels_row = displayRowNames, color = heatmapColors,border_color=NA,
                            annotation_row = annoX,annotation_col = annoY, annotation_colors = fullColors)

        grid.newpage()
        grid.draw(heatmap2)
        grid.force()
        see <- grid.ls()
        testing <- see$gPath
        partial <- testing[!str_detect(testing, "layout::annotation_legend") ]
        check1 <- unlist(strsplit(partial, split='::', fixed=TRUE))
        check2 <- check1[!str_detect(check1, "layout") ]

        for (item in check2) {
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
        theLegend <- grid.grab()

        grid.newpage()
        grid.draw(heatmap1)
        grid.force()
        see <- grid.ls()
        partial <- testing[str_detect(testing, "layout::annotation_legend") ]
        check1 <- unlist(strsplit(partial, split='::', fixed=TRUE))
        check2 <- check1[!str_detect(check1, "layout") ]

        for (item in check2) {
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
        theGraph <- grid.grab()

        grid.newpage()
        grid.draw(theLegend)
        grid.draw(theGraph)
        FullPlot <- grid.grab()

        heatmap3 <- pheatmap(data,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 2,cellheight = 3,
                            labels_col = displayColNames,labels_row = displayRowNames, 
                            color = heatmapColors,border_color=NA,
                            annotation_row = annoX,annotation_col = annoY, annotation_colors = fullColors)
        grid.newpage()
        grid.draw(heatmap3)
        grid.force()
        toUse <- grid.grab()
        look1 <- grid.get("layout")
        vpLayout <- look1[["vp"]][[2]][["layout"]]
        vpLayout$ncol <- as.integer(5)
        vpLayout$widths <- vpLayout$widths[-c(6)]
        vpLayout$respect.mat <- vpLayout$respect.mat[,-c(6)]
        grid.show.layout(vpLayout)
        grid.newpage()
        pushViewport(viewport(layout = vpLayout, name = "testing" ))

        look <- getGrob(toUse, "layout")
        theGrobs <- look[["grobs"]]
        while(length(grep("legend",theGrobs)) >0) {
            theGrobs[[grep("legend",theGrobs)[1]]] <- NULL
        }
        if(length(displayRowNames) > 0){
          theGrobs[[grep("row_names",theGrobs)]] <- NULL
        }
        if (length(displayColNames) > 0){
          theGrobs[[grep("col_names",theGrobs)]] <- NULL
        }
        

        for (i in 1:length(theGrobs)) {
            grid.draw(theGrobs[[i]])
        }

        noExtras <- grid.grab()
        
        ggsave(paste0(session$token,"_NormalHeatmap.png"), plot = noExtras, width = 30, height = 5)
        
        output$normPlot <- renderImage({
            list(src = paste0(session$token,"_NormalHeatmap.png"), alt = "plot wasn't made")
        }, deleteFile = T)


        # save_heatmap_pdf <- function(x, filename, width=100, height=50) {
        #     stopifnot(!missing(x))
        #     stopifnot(!missing(filename))
        #     pdf(filename, width=width, height=height)
        #     grid::grid.newpage()
        #     grid::grid.draw(x)
        #     dev.off()
        # }
        
        heatmapWidth <- length(colnames(data))/10

        enable("normHeatmapDownload")
        output$normHeatmapDownload <- downloadHandler(
            filename = function() {
                paste0("heatmap", ".pdf")
            },
            content = function(file) {
              disable("normHeatmapDownload")
              ggsave(file, plot = FullPlot, width = heatmapWidth, height = 20, limitsize = FALSE)
              print("Done Downloading Heatmap")
            }
        )

        enable("normJustPlotDownload")
        output$normJustPlotDownload <- downloadHandler(
            filename = function() {
                paste0("justPlot", ".pdf")
            },
            content = function(file) {
              disable("normJustPlotDownload")
              ggsave(file, plot = noExtras, width = heatmapWidth, height = 20, limitsize = FALSE)
              print("Done Downloading Heatmap")
            }
        )
        
        endSection("Normal Heatmap")
    })


    # Anatomical heatmap
    # Observe event for anatomical heatmap creation and rendering
    observeEvent(input$anatomicalHeatmap, {
        startSection("Anatomical Heatmap")
      
        fullData <- global$dataFrames$fullData
        variables <- global$variables 

        slices <- input$slices
        valueTable <- global$dataFrames$fullData
        annoCols <- global$variables$AoI
        
        minVal <- input$minVal
        maxVal <- input$maxVal
        
        invert <- global$variables$invert
        
        colorPalette <- global$variables$colorPalette
        viridisOptions <- input$viridisOptions
        twoCol1 <- input$twoCol1
        twoCol2 <- input$twoCol2
        threeCol1 <- input$threeCol1
        threeCol2 <- input$threeCol2
        threeCol3 <- input$threeCol3
        
        # 
        # View(valueTable)
        
        thesvg1 <- data.frame()
        for (i in 1:length(slices)){
            print(i)
            sliceName <- slices[i]
            sliceName <- as.numeric(sliceName)
            if (sliceName %in% c(1:132)){
                fileName <- paste("Necessary Files/SVG_Dataframes/",sliceName, "svgFile.rds",sep = "")
                thesvg <- readRDS(fileName)
                thesvg$lineColor <- thesvg$id
                thesvg <- thesvg[is.na(thesvg$region) == F,]
                
                #remove every other row for simplified plotting
                thesvg <- thesvg[-seq(2,(nrow(thesvg)-1),2),]
                thesvg <- thesvg[-seq(2,(nrow(thesvg)-1),4),]
                
                #add to total dataframe
                thesvg1 <- rbind(thesvg1,thesvg)
                print(unique(thesvg1$keyName))
            }
        }
        rm(thesvg)
        
        thesvg1$shapeOrder <- c(1:length(thesvg1$x))
        
        #only neccesary cols
        thesvg1 <- thesvg1[,c("keyName","unique","x","y","hemi","region","order","shapeOrder")]
        
        #get ID for annots
        MBHTableData <- valueTable
        MBHTableData$annots <- MBHTableData$x
        
        # removeRegion are regions to grey out
        # VS needs to be white
        # fibertracks need to be on top
        
        #needed data to do a basic plot = region, hemi, value, annoCols, and removeRegion
        plainAnatomicData <- MBHTableData[,c("region","hemi","postLimVal","annots","removeRegion")]
        
        sumPlainData <- plainAnatomicData %>% group_by(annots,region,hemi,removeRegion) %>% summarise(value = mean(postLimVal,na.rm = T))
        
        sumPlainData <- as.data.frame(sumPlainData)
        
        print("1")
        # list_obj_sizes()
    
        
        annotCombos <- unique(sumPlainData$annots)
        
        i <- 1
        totalSVG <- c()
        for (i in 1:length(annotCombos)){
            print(i)
            annotCombo <- annotCombos[i]
            subset <- sumPlainData[sumPlainData$annots == annotCombo,]
            subset <- separate(subset, annots, annoCols, sep = "_")
            
            svgAndSubset <- left_join(thesvg1,subset,by = c("region","hemi"))
            svgAndSubset <- svgAndSubset[!is.na(svgAndSubset$value),]
            totalSVG <- rbind(totalSVG,svgAndSubset)
        }
        
        #all missingRegions become NA
        
        totalUnique <- unique(paste(totalSVG$region,totalSVG$hemi,sep = "_"))
        otherUnique <- unique(paste(thesvg1$region,thesvg1$hemi,sep = "_"))
        
        missingUnique <- otherUnique[!otherUnique %in% totalUnique]
        svgUnique <- paste(thesvg1$region,thesvg1$hemi,sep = "_")
        
        missingSVG <- thesvg1[svgUnique %in% missingUnique,]
        
        #fill in missing regions per annotation
        #find regions in each annotation
        totalRegions <- unique(totalSVG$region)
        
        print("2")
        # list_obj_sizes()
        
        i <- 1
        missingDataRegions <- c()
        for (i in 1:length(annotCombos)){
            print(i)
            annotCombo <- annotCombos[i]
            subset <- sumPlainData[sumPlainData$annots == annotCombo,]
            subsetRegions <- unique(subset$region)
            missingSubsetRegions <- totalRegions[!totalRegions %in% subsetRegions]
            
            if ( length(missingSubsetRegions) != 0){
                
                if ("right" %in% unique(thesvg1$hemi)){
                missingFakeDataR <- data.frame(annots = annotCombo,
                                            region = missingSubsetRegions,
                                            hemi = "right",
                                            value = NA)
                } else { missingFakeDataR <- data.frame()}
                
                if ("left" %in% unique(thesvg1$hemi)){
                missingFakeDataL <- data.frame(annots = annotCombo,
                                            region = missingSubsetRegions,
                                            hemi = "left",
                                            value = NA)
                } else { missingFakeDataL <- data.frame()}
                
                missingFakeData <- rbind(missingFakeDataL,missingFakeDataR)
                missingFakeData <- separate(missingFakeData, annots, 
                                            c(annoCols), sep = "_")
                
                
                svgAndSubset <- left_join(thesvg1,missingFakeData,by = c("region","hemi"))
                svgAndSubset <- svgAndSubset[!is.na(svgAndSubset$value),]
                missingDataRegions <- rbind(missingDataRegions,svgAndSubset)
                
            }
        }
        
        if (length(missingDataRegions$annots) != 0){
            missingDataRegions$removeRegion <- FALSE
            dataSVG <- rbind(totalSVG,missingDataRegions)
        } else {
            dataSVG <- totalSVG
        }
        
        noDataSVG <- missingSVG
        
        rm(thesvg1)
        rm(missingSVG)
        
        print("3")
        # list_obj_sizes()
        
        # theData <- dataSVG
        dataVar <- "value"
        
        SVGData <- orderTheData(dataSVG,"value")
        
        SVGNoData <- orderTheData(noDataSVG,"")
        
        rm(dataSVG)
        rm(noDataSVG)
        
        whiteRegions <- "VS"
        
        #Create is... cols
        SVGData <- findFiber(SVGData)
        SVGData <- findWhite(SVGData,whiteRegions)
        
        SVGNoData <- findFiber(SVGNoData)
        SVGNoData <- findWhite(SVGNoData,whiteRegions)
        
        #fiber, remove, data, nodata
        
        dataFiber <- SVGData[SVGData$isFiber == T & SVGData$isWhite == F,]
        dataWhite <- SVGData[SVGData$isFiber == F & SVGData$isWhite == T,]
        dataData <- SVGData[SVGData$isFiber == F & SVGData$isWhite == F,]
        
        # dataFiber[,relevantAnnots] <- NULL
        dataWhite[,annoCols] <- NULL
        # dataFiber$value <- NULL
        dataWhite$value <- NULL
        
        noDataFiber <- SVGNoData[SVGNoData$isFiber == T & SVGNoData$isWhite == F,]
        noDataWhite <- SVGNoData[SVGNoData$isFiber == F & SVGNoData$isWhite == T,]
        noDataNoData <- SVGNoData[SVGNoData$isFiber == F & SVGNoData$isWhite == F,]
        
        fiberData <- dataFiber #used to rbind
        fiberNoData <- noDataFiber
        dataWhite$removeRegion <- NULL
        whiteData <- rbind(dataWhite,noDataWhite)
        
        #remove duplicate rows
        # fiberData <- fiberData[!duplicated(fiberData),]
        whiteData <- whiteData[!duplicated(whiteData),]
        
        
        hasData <- dataData
        noData <- noDataNoData
        
        rm(dataData)
        rm(noDataNoData)
        
        fiberData[,c("isFiber","isWhite")] <- NULL
        fiberNoData[,c("isFiber","isWhite")] <- NULL
        whiteData[,c("isFiber","isWhite")] <- NULL
        hasData[,c("isFiber","isWhite")] <- NULL
        noData[,c("isFiber","isWhite")] <- NULL
        
        
        sortedSlices <- slices[order(slices)]

        fiberData$keyName <- factor(fiberData$keyName, levels = sortedSlices)
        fiberNoData$keyName <- factor(fiberNoData$keyName, levels = sortedSlices)
        whiteData$keyName <- factor(whiteData$keyName, levels = sortedSlices)
        hasData$keyName <- factor(hasData$keyName, levels = sortedSlices)
        noData$keyName <- factor(noData$keyName, levels = sortedSlices)
        
        #noData = grey
        noData$toFill <- "grey"
        
        #data = viridis
        hasData$Value <- hasData$value
        
        #remove = white
        if (length(whiteData[,1]) > 0){
          whiteData$toFill <- "white"
        }
        
        
        #fiber tracts = grey (for now)
        fiberData$Value <- fiberData$value
        
        #fiberNoData = grey
        fiberNoData$toFill <- "grey"
        
        hasData[hasData$removeRegion == T,"Value"] <- NA
        
        directionValue <- -1
        if (invert == T){
            directionValue <- 1
        }
        
        #remove hemi with no data, if applicable
        if (length(unique(hasData$hemi)) < 2){
            haveHemi <- unique(hasData$hemi)
            
            hasData <- hasData[hasData$hemi == haveHemi,]
            noData <- noData[noData$hemi == haveHemi,]
            if (length(whiteData[,1]) > 0){
              whiteData <- whiteData[whiteData$hemi == haveHemi,]
            }
            fiberData <- fiberData[fiberData$hemi == haveHemi,]
            fiberNoData <- fiberNoData[fiberNoData$hemi == haveHemi,]
        
        }
        
        print("4")
        # list_obj_sizes()
        
        if(length(colorPalette) < 1){colorPalette <- "Viridis"}
        
        heatmapHeight <- length(unique(valueTable$x)) + 2
        heatmapWidth <- length(slices) + 1
        
        facetTextSizeX <- 8.5
        facetTextSizeY <- -0.125*max(nchar(unique(valueTable$x)))+8.75
        lineWidthVar <- 0.1
        
        factorLevels <- as.numeric(unique(SVGData$keyName))
        factorLevels <- sort(factorLevels)
        SVGData$keyName <- factor(SVGData$keyName, levels = factorLevels)
        # 
        factorLevels <- as.numeric(levels(noData$keyName))
        factorLevels <- sort(factorLevels)
        noData$keyName <- factor(noData$keyName, levels = factorLevels)
        
        factorLevels <- as.numeric(levels(hasData$keyName))
        factorLevels <- sort(factorLevels)
        hasData$keyName <- factor(hasData$keyName, levels = factorLevels)
        
        factorLevels <- as.numeric(levels(whiteData$keyName))
        factorLevels <- sort(factorLevels)
        whiteData$keyName <- factor(whiteData$keyName, levels = factorLevels)
        
        factorLevels <- as.numeric(levels(fiberNoData$keyName))
        factorLevels <- sort(factorLevels)
        fiberNoData$keyName <- factor(fiberNoData$keyName, levels = factorLevels)
        
        factorLevels <- as.numeric(levels(fiberData$keyName))
        factorLevels <- sort(factorLevels)
        fiberData$keyName <- factor(fiberData$keyName, levels = factorLevels)
        
        #if viridis:
        if (colorPalette == "Viridis"){
            if (is.null(input$viridisOptions)){
                heatmapColors <- "inferno"
            } else {
                heatmapColors <- input$viridisOptions
            }
                
            theHeatmap <- ggplot(SVGData, aes(x = x, y = y)) +
            #noData
            geom_polygon(data = noData,aes(group = unique, fill = toFill)) +
            scale_fill_manual( values = c("gray"), na.value = "grey", guide="none") +
            geom_path(data = noData,aes(group = unique),linewidth = lineWidthVar)+
            new_scale("fill") +
            
            #data
            geom_polygon(data = hasData,aes(group = unique, fill = Value)) +
            scale_fill_viridis(option = heatmapColors, direction = directionValue,
                                limits = c(minVal, maxVal),na.value="grey") +
            geom_path(data = hasData,aes(group = unique),linewidth = lineWidthVar) +
            new_scale("fill")
            
            #remove regions
            if (length(whiteData[,1]) > 0){
              theHeatmap <- theHeatmap + geom_polygon(data = whiteData,aes(group = unique, fill = toFill)) +
              scale_fill_manual( values = c("white"), na.value = "white", guide="none") +
              geom_path(data = whiteData,aes(group = unique),linewidth = lineWidthVar) +
              new_scale("fill")
            }
            
            #fiber no data - fiberNoData
            theHeatmap <- theHeatmap + geom_polygon(data = fiberNoData,aes(group = unique, fill = toFill)) +
            scale_fill_manual( values = c("gray"), na.value = "grey", guide="none") +
            geom_path(data = fiberNoData,aes(group = unique),linewidth = lineWidthVar)+
            new_scale("fill") +
            
            #fiber tracts
            geom_polygon(data = fiberData,aes(group = unique, fill = Value)) +
            scale_fill_viridis(option = heatmapColors, direction = directionValue,
                                limits = c(minVal, maxVal),na.value="grey",guide = "none") +
            geom_path(data = fiberData,aes(group = unique),linewidth = lineWidthVar) +
            
            #Theme stuff
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
            ) +
            
            #Grid time
            facet_grid(
                cols = vars(keyName),
                rows = vars(!!!syms(annoCols)),
                labeller = label_value, #label_context
                shrink = F
            )
            
            #ggsave(paste0("Test_AnatomicalHeatmap.png"), theHeatmap, height = length(unique(valueTable$x)) + 2, width = length(slices) + 1)
            
        } else if (colorPalette == "2 Color"){
            heatmapColors <- c(twoCol1,twoCol2)
            
            theHeatmap <- ggplot(SVGData, aes(x = x, y = y)) +
            #noData
            geom_polygon(data = noData,aes(group = unique, fill = toFill)) +
            scale_fill_manual( values = c("gray"), na.value = "grey", guide="none") +
            geom_path(data = noData,aes(group = unique),linewidth = lineWidthVar)+
            new_scale("fill") +
            
            #data
            geom_polygon(data = hasData,aes(group = unique, fill = Value)) +
            scale_fill_gradientn(colors = heatmapColors,na.value="grey") +
            geom_path(data = hasData,aes(group = unique),linewidth = lineWidthVar) +
            new_scale("fill")
            
            #remove regions
            if (length(whiteData[,1]) > 0){
              theHeatmap <- theHeatmap + geom_polygon(data = whiteData,aes(group = unique, fill = toFill)) +
              scale_fill_manual( values = c("white"), na.value = "white", guide="none") +
              geom_path(data = whiteData,aes(group = unique),linewidth = lineWidthVar) +
              new_scale("fill")
            }
            
            #fiber no data - fiberNoData
            theHeatmap <- theHeatmap + geom_polygon(data = fiberNoData,aes(group = unique, fill = toFill)) +
            scale_fill_manual( values = c("gray"), na.value = "grey", guide="none") +
            geom_path(data = fiberNoData,aes(group = unique),linewidth = lineWidthVar)+
            new_scale("fill") +
            
            #fiber tracts
            geom_polygon(data = fiberData,aes(group = unique, fill = Value)) +
            scale_fill_gradientn(colors = heatmapColors,na.value="grey",guide = "none") +
            geom_path(data = fiberData,aes(group = unique),linewidth = lineWidthVar) +
            
            #Theme stuff
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
            ) +
            
            #Grid time
            facet_grid(
                cols = vars(keyName),
                rows = vars(!!!syms(annoCols)),
                labeller = label_value, #label_context
                shrink = F
            )
            
        } else {
            heatmapColors <- c(threeCol1,threeCol2,threeCol3)
            
            theHeatmap <- ggplot(SVGData, aes(x = x, y = y)) +
            #noData
            geom_polygon(data = noData,aes(group = unique, fill = toFill)) +
            scale_fill_manual( values = c("gray"), na.value = "grey", guide="none") +
            geom_path(data = noData,aes(group = unique),linewidth = lineWidthVar)+
            new_scale("fill") +
            
            #data
            geom_polygon(data = hasData,aes(group = unique, fill = Value)) +
            scale_fill_gradientn(colors = heatmapColors,na.value="grey") +
            geom_path(data = hasData,aes(group = unique),linewidth = lineWidthVar) +
            new_scale("fill")
            
            #remove regions
            if (length(whiteData[,1]) > 0){
              theHeatmap <- theHeatmap + geom_polygon(data = whiteData,aes(group = unique, fill = toFill)) +
              scale_fill_manual( values = c("white"), na.value = "white", guide="none") +
              geom_path(data = whiteData,aes(group = unique),linewidth = lineWidthVar) +
              new_scale("fill")
            }
            
            #fiber no data - fiberNoData
            theHeatmap <- theHeatmap + geom_polygon(data = fiberNoData,aes(group = unique, fill = toFill)) +
            scale_fill_manual( values = c("gray"), na.value = "grey", guide="none") +
            geom_path(data = fiberNoData,aes(group = unique),linewidth = lineWidthVar)+
            new_scale("fill") +
            
            #fiber tracts
            geom_polygon(data = fiberData,aes(group = unique, fill = Value)) +
            scale_fill_gradientn(colors = heatmapColors,na.value="grey",guide = "none") +
            geom_path(data = fiberData,aes(group = unique),linewidth = lineWidthVar) +
            
            #Theme stuff
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
            ) +
            
            #Grid time
            facet_grid(
                cols = vars(keyName),
                rows = vars(!!!syms(annoCols)),
                labeller = label_value, #label_context
                shrink = F
            )
        }
        
        ggsave(paste0(session$token,"_AnatomicalHeatmap.png"), theHeatmap, height = heatmapHeight, width = heatmapWidth)
        
        output$anatomicalPlot <- renderImage({
            list(src = paste0(session$token,"_AnatomicalHeatmap.png"), alt = "plot wasn't made")
        }, deleteFile = T)
        
        enable("anatomicalHeatmapDownload")
        output$anatomicalHeatmapDownload <- downloadHandler(
            filename = function() {
                paste0("anatomicalHeatmap", ".pdf")
            },
            content = function(file) {
              disable("anatomicalHeatmapDownload")
              ggsave(file, theHeatmap, height = length(unique(valueTable$x)) + 2, width = length(slices) + 1)
              print("Done Downloading Heatmap")
            }
        )
        
        endSection("Anatomical Heatmap")
    })
}


# . ----
# 4.0 Run App ----
# Launch the Shiny app
shinyApp(ui = ui, server = server)



