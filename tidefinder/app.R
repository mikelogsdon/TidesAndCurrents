library(shiny)

source("global.R")

ui <- fluidPage(
   
   titlePanel("Finding Superlative Tides, NOAA Predictions"),

  # Sidebar Layout with tabs in the main panel
   sidebarLayout(
      sidebarPanel(
         sliderInput("nyears", "Time Horizon (years)", min = 1,max = 18, value = 5),
         selectInput("state", "State", choices = c("Alaska", "Washington", "Oregon", "California"), selected = "Oregon"),
         selectInput("region", "Region", choices = c("Select Region", sort(unique(stations$group[stations$state == "Oregon"]))), selected = ""),
         selectInput("station", "Station", choices = c("Select Station", sort(unique(stations$stationName[stations$state == "Oregon"]))), selected = "Select Station")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Low Tides",
            radioButtons("lowTideToggle", "List By", choices = c("Tides", "Stations"), selected = "Tides", inline = TRUE),
            conditionalPanel(
              condition = "input.lowTideToggle == 'Tides'",
              dataTableOutput("lowTideTableTides")
            ),
            conditionalPanel(
              condition = "input.lowTideToggle == 'Stations'",
              dataTableOutput("lowTideTableStations")
            )
          ),
          tabPanel(
            "High Tides",
            radioButtons("highTideToggle", "List By", choices = c("Tides", "Stations"), selected = "Tides", inline = TRUE),
            conditionalPanel(
              condition = "input.highTideToggle == 'Tides'",
              dataTableOutput("highTideTableTides")
            ),
            conditionalPanel(
              condition = "input.highTideToggle == 'Stations'",
              dataTableOutput("highTideTableStations")
            )
          ),
          tabPanel(
            "Map",
            h4("Marker Diameter Proportional to Average Tidal Range"),
            leafletOutput("leafletMap", height = "600px")
          ),
          tabPanel(
            "Tidal Pattern",
            h4("This page creates graphics showing tidal patterns for a selected station. You'll have to press the button to rebuild the plot after changing stations."),
            h5("As an example, build the plot for an Arctic Ocean site, a Puget Sound site, and then a Cook Inlet site and note the differences."),
            radioButtons("patternToggle", "Plot Station Tidal Patterns", choices = c("Monthly", "Yearly"), selected = "Monthly", inline = TRUE),
            actionButton("makeStationPlot", label = "Make Station Plot"),
            conditionalPanel(
              condition = "input.patternToggle == 'Monthly'",
              plotOutput("stationPlotMonthly", width = "100%", height = "600px")
            ),
            conditionalPanel(
              condition = "input.patternToggle == 'Yearly'",
              plotOutput("stationPlotYearly", width = "100%", height = "600px")
            )
          ),
          tabPanel(
            "Notes",
            uiOutput("notes")
          ),
          tabPanel(
            "Contact",
            h4("Use this form to submit a comment or question. Note that this project is not supported by staff or budget!"),
            textInput("name", "Name"),
            textInput("email", "Email:"),
            textAreaInput("comment", "Comment", width = "800px", height = "200px"),
            actionButton("submit", "Submit")
          )
        )
      )
   )
)


server <- function(input, output, session) {
  
  # Tee up the reactive values
  stationsAndTides <- reactiveValues()
  stationsAndTides$selectedStations <- stations[stations$state == "Oregon", ]
  stationsAndTides$lowTides <- getTides(state = "Oregon", type = "L")
  stationsAndTides$highTides <- getTides(state = "Oregon", type = "H")
  
  observeEvent(input$nyears, {
    stationsAndTides$lowTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "L", nyears = input$nyears)
    stationsAndTides$highTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "H", nyears = input$nyears)
  })
  
  # When state, region, or station changes, update select inputs
  observeEvent(input$state, {
    regionTmp <- "Select Region"; stationTmp <- "Select Station"
    if(input$region %in% stationsAndTides$selectedStations$group[stationsAndTides$selectedStations$state == input$state]) {
      regionTmp <- input$region
      stationTmp <- input$station
    }
    updateSelectInput(session, "region", choices = c("Select Region", sort(unique(stations$group[stations$state == input$state]))), selected = regionTmp)
    updateSelectInput(session, "station", choices = c("Select Station", sort(unique(stations$stationName[stations$state == input$state]))), selected = stationTmp)
    stationsAndTides$selectedStations <- findStations(input$state, "", "")
    stationsAndTides$lowTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "L", nyears = input$nyears)
    stationsAndTides$highTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "H", nyears = input$nyears)
  })
  observeEvent(input$region, {
    if(input$region != "Select Region") {
      # newStation <- input$station != stationsAndTides$selectedStations$stationName[1]
      rowsTmp <- which(stations$state == input$state & stations$group == input$region)
      selectedTmp <- "Select Station"
      if(input$station %in% stationsAndTides$selectedStations$stationName[stationsAndTides$selectedStations$group == input$region]) {
        selectedTmp <- input$station
      }
      updateSelectInput(session, "station", choices = c("Select Station", sort(unique(stations$stationName[rowsTmp]))), selected = selectedTmp)
      stationsAndTides$selectedStations <- findStations(input$state, input$region, "")
      stationsAndTides$lowTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "L", nyears = input$nyears)
      stationsAndTides$highTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "H", nyears = input$nyears)
    } else {
      updateSelectInput(session, "station", choices = c("Select Station", sort(unique(stations$stationName[stations$state == input$state]))), selected = "Select Station")
      stationsAndTides$selectedStations <- findStations(input$state, "", "")
      stationsAndTides$lowTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "L", nyears = input$nyears)
      stationsAndTides$highTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "H", nyears = input$nyears)    
    }
  })
  observeEvent(input$station, {
    if(input$station != "Select Station") {
      stationsAndTides$selectedStations <- findStations(input$state, input$region, input$station)
      stationsAndTides$lowTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "L", nyears = input$nyears)
      stationsAndTides$highTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "H", nyears = input$nyears)
    } else {
      if(input$region != "Select Region") {
        stationsAndTides$selectedStations <- findStations(input$state, input$region, "")
      } else {
        stationsAndTides$selectedStations <- findStations(input$state, "", "")
      }
      stationsAndTides$lowTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "L", nyears = input$nyears)
      stationsAndTides$highTides <- getTides(input$state, Id = stationsAndTides$selectedStations$Id, type = "H", nyears = input$nyears)
    }
  })
  
  # When they click the map update the station selection
  observeEvent(input$leafletMap_marker_click, {
    sx <- nearestStation(input$leafletMap_marker_click) 
    updateSelectInput(session, "state", selected = sx$state[1])
    newState <- sx$state[1] != input$state
    if(newState) {
      updateSelectInput(session, "region", selected = sx$group[1], choices = stations$group[stations$state == sx$state[1]])  
    } else {
      updateSelectInput(session, "region", selected = sx$group[1])
    }
    newRegion <- sx$group[1] != input$region
    if(newRegion) {
      updateSelectInput(session, "station", selected = sx$stationName[1], choices = stations$stationName[stations$group == sx$group[1]])
    } else {
      updateSelectInput(session, "station", selected = sx$stationName[1])
    }
    stationsAndTides$selectedStations <- sx
  })

  # Update the leaflet map when the selected stations change
  observe({
    sx <- stationsAndTides$selectedStations
    if(length(unique(sx$group)) == 1) {
      if(nrow(sx) > 1) {
        bx <- returnBounds(sx)
        leafletProxy("leafletMap", data = sx) %>%
          fitBounds(bx[[1]], bx[[2]], bx[[3]], bx[[4]]) %>%
          clearGroup("region") %>%
          # addCircleMarkers(~Lon, ~Lat, label = ~as.character(stationName), popup = ~as.character(details), col = "red", group = "region", radius = ~aveTidalRange)
        addCircleMarkers(~Lon, ~Lat, label = ~as.character(stationName), col = "red", group = "region", radius = ~aveTidalRange)
      } else {
        leafletProxy("leafletMap", data = stations[stations$group == unique(sx$group), ]) %>%
          clearGroup("region") %>%
          # addCircleMarkers(~Lon, ~Lat, label = ~as.character(stationName), popup = ~as.character(details), col = "red", group = "region", radius = ~aveTidalRange)
        addCircleMarkers(~Lon, ~Lat, label = ~as.character(stationName), col = "red", group = "region", radius = ~aveTidalRange)
      }

    } else if(nrow(sx) > 0) {
      # State level
      bx <- returnBounds(sx)
      leafletProxy("leafletMap", data = sx) %>%
        fitBounds(bx[[1]], bx[[2]], bx[[3]], bx[[4]]) %>%
        clearGroup("region") %>% clearGroup("station")
    }
    if(nrow(sx) == 1) {
      leafletProxy("leafletMap", data = sx) %>%
        clearGroup("station") %>%
        addCircleMarkers(~Lon, ~Lat, label = ~as.character(stationName), col = "green", group = "station", radius = ~aveTidalRange)
      # addCircleMarkers(~Lon, ~Lat, label = ~as.character(stationName), popup = ~as.character(details), col = "green", group = "station", radius = ~aveTidalRange)
    }
  })
  
  stationData <- eventReactive(input$makeStationPlot, {
    if(input$station == "Select Station") {
      showModal(modalDialog("Must Select a Station to Build this Plot"))
      return(NULL)
    } else if(input$region == "Select Region") {
      showModal(modalDialog("Must Select a State, Region, and Station to Build this Plot. (There are some cases of duplicate station names, such as Anchor Point, which is why you have to specify all three.)"))
      return(NULL)
    }
    getStationData(stations$Id[stations$state == input$state & stations$group == input$region & stations$stationName == input$station])
    # read_tides(stations$Id[stations$state == input$state & stations$group == input$region & stations$stationName == input$station])
  })
  
  stationName <- eventReactive(input$makeStationPlot, {
    input$station
  })
  
  # stationPlot <- eventReactive(input$makeStationPlot, {
  
  
  # Reactive outputs for tables to render
  lowTideTableTides <- reactive({
    makeTideSummary2(stationsAndTides$lowTides, input$nyears)
  })
  lowTideTableStations <- reactive({
    makeTideSummary3(input$state, stationsAndTides$selectedStations$Id, "L", input$nyears)
  })
  highTideTableTides <- reactive({
    makeTideSummary2(stationsAndTides$highTides, input$nyears)
  })
  highTideTableStations <- reactive({
    makeTideSummary3(input$state, stationsAndTides$selectedStations$Id, "H", input$nyears)
  })
  
  
  # Render outputs-----------------------
  # Starting with data tables
  output$lowTideTableTides <- renderDataTable({
    lowTideTableTides()
  })
  output$lowTideTableStations <- renderDataTable({
    lowTideTableStations()
  })
  output$highTideTableTides <- renderDataTable({
    highTideTableTides()
  })
  output$highTideTableStations <- renderDataTable({
    highTideTableStations()
  })
  
  # Leaflet map
  output$leafletMap <- renderLeaflet({
    leaflet(stations) %>% addTiles() %>%
      fitBounds(-124.622, 41.83, -121.9, 46.52) %>%
      # setView(lng = -123, lat = 44, zoom = 6) %>%
      # addCircleMarkers(~Lon, ~Lat, label = ~as.character(stationName), popup = ~as.character(details), radius = ~aveTidalRange)
      addCircleMarkers(~Lon, ~Lat, label = ~as.character(stationName), radius = ~aveTidalRange)
  })
  
  # ggplots
  output$stationPlotMonthly <- renderPlot({
    plotStationMonthly(stationData(), stationName())
  }, res = 112)
  
  output$stationPlotYearly <- renderPlot({
    plotStationYearly(stationData(), stationName())
  }, res = 112)
  
  output$notes <- renderUI(HTML(paste(makeNotes(), collapse = " ")))
  
  
  observeEvent(input$submit, {
    if(input$name == "" | input$email == "" | input$comment == "") {
      showModal(
        modalDialog(
          "Error. You must enter your name, email, and comment"
        )
      )
    } else {
      tmp <- try(writeComment(input$name, input$email, input$comment))
      if(inherits(tmp, "try-error")) {
        showModal(
          modalDialog(
            "Error in Comment Submission"
          )
        )
      } else {
        updateTextInput(session, "name", value = "")
        updateTextInput(session, "email", value = "")
        updateTextAreaInput(session, "comment", value = "")
        showModal(
          modalDialog(
            "Comment Received"
          )
        )
      }
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

