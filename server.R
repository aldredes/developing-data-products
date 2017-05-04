#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

server <- function(input, output, session) {

    # Function to format a number
    c_fn <- function(num, digits = 6) {
        return(format(round(num, digits = digits), nsmall = digits, big.mark = ","))
    }
    formatNum <- cmpfun(c_fn)
    
    # Function to show parts of progress indicator
    c_spp <- function(numParts, detailMsg = "", stepNo = NULL, delaySec = 0.1) {
        # Increment the progress bar, and update the detail text.
        incProgress(1/numParts, detail = paste(detailMsg, stepNo))
        # Pause for delaySec seconds to simulate a long computation.
        Sys.sleep(delaySec)
    }
    showProgressPart <- cmpfun(c_spp)
    
    # Display progress indicator while processing data ...
    withProgress(message = 'Initializing application', value = 0, {
        showProgressPart(2, "Reading spatial data ...")
        kmlFilePath <- "data/spatial/world_ISO2.kml"
        kmlLayers <- ogrListLayers(kmlFilePath)
        world <- readOGR(kmlFilePath, kmlLayers)
        showProgressPart(2, "Reading meteor data ...")
        meteors <- fread("data/nasa_meteors_scrubbed.csv", na.strings = c("NA", "#DIV/0!", ""), header = TRUE)
        meteors$latitude <- as.numeric(as.character(meteors$reclat))
        meteors$longitude <- as.numeric(as.character(meteors$reclong))
        meteors$recclass <- as.factor(meteors$recclass)
        meteors$wmk2006Class <- as.factor(meteors$wmk2006Class)
        classifications <- sort(unique(meteors$wmk2006Class))
        meteors$countryName <- as.factor(meteors$countryName)
        meteors$iso2 <- as.factor(meteors$iso2)
        # Update Location (Include, City or so); Incude Geolocation
        meteors$popUp <- paste("<b>Name: </b>", meteors$name, "<b><br>Location: </b>", meteors$countryName
                                , "<br><b>Coordinates: </b>", meteors$GeoLocation 
                                , "<br><b>Year: </b>", meteors$year, "<br><b>Class: </b>", meteors$recclass
                                , "<br><b><a href='https://en.wikipedia.org/wiki/File:Meteorite_Classification_after_Weissberg_McCoy_Krot_2006_Stony_Iron.svg'
                                    target='_blank' style='color: rgb(255, 255, 255);font-style: italic;'>Weisberg (2006)</a> Class: </b>", meteors$wmk2006Class
                                , "<br><b>Mass (in Kg): </b>", ifelse(is.na(meteors$`mass (g)`), meteors$`mass (g)`, formatNum(meteors$`mass (g)` / 10^3, 5)) , sep = "") 
        showProgressPart(2, "Completed.")
    })
    
    #----------------------------------------------------------------------------------------------------------------
    # SECTION FOR RENDERING CONTROLS
    #----------------------------------------------------------------------------------------------------------------
    # Display control panel ...
    output$controlPanel <- renderUI({
        absolutePanel(top = 10, right = 10, draggable = TRUE, fixed = TRUE, width = "275px",
                      class = "absolutePanel",
                      h4(img(src = "images/220px-Leonid_Meteor.jpg", width = "25px", height = "25px"), " Meteorite Landings", align = "center"),
                      hr(),
                      uiOutput("yearRange"), 
                      uiOutput("meteorClass"), 
                      actionLink("meteorClassLink"
                                  , "* Weisberg et al (2006) Scheme"
                                  , onclick = "window.open('https://en.wikipedia.org/wiki/File:Meteorite_Classification_after_Weissberg_McCoy_Krot_2006_Stony_Iron.svg', '_blank')"
                                 ), 
                      hr(),
                      checkboxInput("showGraph", "Stacked Area Graph by Year"),
                      selectInput("mapLayer", label = "Maps"
                                  , choices = list("Basic" = "basic"
                                                   , "Grayscale" = "grayscale"
                                                   , "Dark" = "nightmode"
                                                   , "Satellite" = "imagery"
                                  )
                                  , selected = "grayscale"
                      ),
                      radioButtons("mapType", label = NULL
                                    , choices = list("Markers" = "marker"
                                                        , "Heatmap" = "heatmap"
                                                        , "Choropleth" = "choropleth")
                                    , selected = "marker"),
                      hr(),
                      div(h5(actionLink("howToLink"
                                            , "How to use this application?"
                                            , onclick = "window.open('docs/using_meteorite_landings_app.pdf', '_blank')"
                                        )), align = "center")
        )
    })
    
    # Display "year" slider input ...
    output$yearRange <- renderUI({
        sliderInput("yearRange", "Year Recorded", min(meteors$year), max(meteors$year),
            value = c(max(meteors$year) - 25, max(meteors$year)), sep = "", ticks = FALSE
        )
    })
    
    # Display "meteor classification" checkbox group input ...
    output$meteorClass <- renderUI({
        checkboxGroupInput('meteorClass', 'Classifications *', classifications, selected = classifications)
    })

    # Display checkbox input to show "cummulative and actual no. of recorded meteorites by year" plot ...
    output$graphPane <- renderUI({
        if (input$showGraph) { 
            absolutePanel(top = 50, left = 50, draggable = TRUE, fixed = TRUE, width = "670px", height = "520px", class = "graphPanel"
                          , plotlyOutput('plotly'), div(style = "height: 50px")
                          , h5("Cumulative and Actual No. of Recorded Meteorites by Year", align = "center")
            )
        }
    })  
    
    #----------------------------------------------------------------------------------------------------------------
    # **** SECTION FOR DATA PROCESSING
    #----------------------------------------------------------------------------------------------------------------
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        subset(meteors, (year >= input$yearRange[1] & year <= input$yearRange[2]) 
                & (wmk2006Class %in% input$meteorClass))
    })

    # Data processing to create data table of counts per Country along with spatial data
    countByCountry <- reactive({
        # Display progress indicator while processing data ...
        withProgress(message = 'Computing count by country', value = 0, {
            showProgressPart(4, "Filtering data ...")
            tmp <- filteredData()
            showProgressPart(4, "Getting counts ...")
            dat <- tmp[, .(count=.N), by = list(iso2, countryName)]
            showProgressPart(4, "Setting up additional data ...")
            dat$popUp <- paste("<b>Country: </b>", dat$countryName
                               , "<br><b>No. of Records: </b>", formatNum(dat$count,0), sep = "")
            showProgressPart(4, "Merging with spatial data ...")
            dat <- merge(x = world, y = dat, by.x = "Name", by.y = "iso2", all = TRUE)
            showProgressPart(4, "Completed.")
            dat
        })
    })

    # Data processing to create data table of counts per year and classification
    countByYearClass <- reactive({
        # Display progress indicator while processing data ...
        withProgress(message = 'Computing count by year, class', value = 0, {
            showProgressPart(4, "Filtering data ...")
            tmp <- filteredData()
            dat <- data.table()
            if (nrow(tmp) > 0) {
                showProgressPart(4, "Getting counts ...")
                dat <- tmp[, .(count=.N), by = list(year, wmk2006Class)]  
                colnames(dat) <- c('id', 'class', 'count')
                showProgressPart(4, "Tidying data ...")
                dat <- dcast(dat, id ~ class)
                cols <- colnames(dat)[colSums(is.na(dat)) > 0]
                dat[ , (cols) := lapply(.SD, function(x) replace(x, which(is.na(x)), 0)), .SDcols = cols]  
                showProgressPart(4, "Sorting data ...")
                # Reorder by sum of columns from highest to lowest
                dat <- setcolorder(dat, dat[ , c(1, order(colSums(dat[ ,2:ncol(dat)], na.rm = TRUE)) + 1)])
            }
            showProgressPart(4, "Completed.")
            dat
        })
    })     
    
    # This will decide which data for the map will be used
    mapData <- reactive({
        mapType <- ifelse(is.null(input$mapType), 'marker', input$mapType)
        if (mapType == 'choropleth') countByCountry()
        else filteredData()
    })    
    
    #----------------------------------------------------------------------------------------------------------------
    # SECTION FOR RENDERING PLOTS
    #----------------------------------------------------------------------------------------------------------------
    
    # Build graaph of counts by classification thru Plotly
    # a) by Year: stacked fill charts (year in sequence)  
    # b) by Country: stacked bar charts (sorted by total no. of counts) 
    output$plotly <- renderPlotly({
        # Data for plot will be built as matrix of count, with classes as succeeding columns
        dat <- countByYearClass()
        # Build color spectrum
        colors <- substr(plasma(length(classifications), direction = 1), start = 1, stop = 7) # Remove the alpha
        p <- plot_ly()
        if (nrow(dat) > 0) {
            oldCols <- colnames(dat) # For class display
            colnames(dat) <- make.names(colnames(dat)) # Make R-syntactically valid column names
            newCols <- colnames(dat)
            nCol <- length(newCols) 
            cummDat <- dat # For cummulative table
            # Below will compute for cummulative counts for stacked chart applied only by Year
            if (ncol(dat) >= 3) { 
                for (i in 3:nCol) {
                    eval(parse(text = paste('cummDat$', newCols[i], ' <- cummDat$'
                                            , newCols[i], ' + cummDat$', newCols[i-1], sep = '')))
                }
            }
            # Build a stacked filled scatter plot
            p <- plot_ly(dat, x = as.factor(dat$id), y = 0 ##
                         , name = "id" 
                         , hoverinfo = 'text'
                         , text = dat$id
                         , fillcolor = "#000000"
                         , mode = 'none'
                         , type = 'scatter'
                         , fill = 'tozeroy') %>% 
                layout(title = ""
                       , xaxis = list(title = "", showgrid = FALSE)
                       , yaxis = list(title = "", showgrid = FALSE)
                       , showlegend = FALSE, autosize = FALSE, height = "475", width = "650"
                       , margin = list(l = 75, r = 50, b = 75, t = 50, pad = 10)
                       , paper_bgcolor = 'rgba(248, 248, 255, 0)'
                       , plot_bgcolor =  'rgba(248, 248, 255, 0)'
                ) 
            # Add each stack of data
            for (i in nCol:2) {
                p <- p %>% add_trace(y = eval(parse(text = paste('cummDat$', newCols[i], sep = '')))
                                 , name = oldCols[i]
                                 , hoverinfo = 'text+name'
                                 , text = paste("(Cum.: ", formatNum(eval(parse(text = paste('cummDat$', newCols[i], sep = ''))), 0), "; Act.: "
                                                , formatNum(eval(parse(text = paste('dat$', newCols[i], sep = ''))), 0), ")", sep = "")
                                 , fillcolor = colors[nCol + 1 - i] 
                )
            }    
        }
        p # Display the plot
    })
    
    #----------------------------------------------------------------------------------------------------------------
    # **** SECTION FOR RENDERING MAPS
    #----------------------------------------------------------------------------------------------------------------
    # Build params for Awesome icons for markers
    theIcon <- awesomeIcons(
        icon = 'fa-spinner',
        iconColor = 'lightgray',
        spin = TRUE,
        library = 'fa',
        markerColor = 'gray'
    )
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    colorpal <- reactive({
        colorNumeric("plasma", c(countByCountry()$count, 0)) # "viridis", "magma", "inferno", or "plasma".
    })
    
    # This is the base leflet object
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet(meteors, options = leafletOptions(worldCopyJump = TRUE)) %>% 
            setView(65, 35, zoom = 2) 
    })
    
    # Changes to the map happens here depending on the set of user preferences and inputs 
    observe({
        # Display progress indicator while processing data ...
        withProgress(message = 'Updating the map', value = 0, {
            showProgressPart(3, "Fetching data ...")
            mapType <- ifelse(is.null(input$mapType), 'marker', input$mapType)
            legend <- ifelse(is.null(input$showLegend), FALSE, input$showLegend) 
            showProgressPart(3, "Preparing the base map ...")
            proxy <- leafletProxy("map", data = mapData()) %>%
                clearMarkerClusters() %>%
                clearMarkers() %>%
                clearWebGLHeatmap() %>%
                clearShapes() 
            showProgressPart(3, "Adding layers and other objects ...")
            if (mapType == 'heatmap') {
                proxy %>%
                    clearControls() %>% 
                    addWebGLHeatmap(size = 150000, units = "m", opacity = 1, gradientTexture = "skyline") 
            }
            else if (mapType == 'choropleth') {
                pal <- colorpal()
                proxy %>%
                    addPolygons(fillOpacity = 0.5, 
                                fillColor =  ~pal(count), 
                                color = "black", 
                                weight = 0.5,
                                popup = ~popUp
                                ) %>%
                    clearControls() %>% 
                    addLegend(position = "bottomleft", pal = pal, values = ~count)
            }
            else {
                proxy %>%
                    clearControls() %>%
                    addAwesomeMarkers(popup = ~popUp, icon = theIcon
                                      ,   clusterOptions = markerClusterOptions(polygonOptions = list(
                                                                      color='#990000', weight = 3, stroke = FALSE, fillOpacity = 0.3
                                                                  )
                                                        ) 
                                            )   
            }
            showProgressPart(3, "Completed.")
        })
    })
    
    # Use a separate observer for map tiling
    observe({
        # Display progress indicator while processing data ...
        withProgress(message = 'Updating the map layer', value = 0, {
            showProgressPart(2, "Rendering map tiles ...")
            if (!is.null(input$mapLayer)) {
                    tileProvider <- switch(input$mapLayer
                                           , basic = providers$OpenStreetMap
                                           , grayscale = providers$CartoDB.Positron
                                           , nightmode = providers$CartoDB.DarkMatterNoLabels
                                           , imagery = providers$Esri.WorldImagery
                    )
                leafletProxy("map", data = mapData()) %>% 
                    clearTiles() %>%
                    addProviderTiles(tileProvider, options = tileOptions(minZoom = 2, detectRetina = TRUE))
            }
            showProgressPart(2, "Completed.")
        })
    })    

}