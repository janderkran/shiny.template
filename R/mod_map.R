# Module UI
  
#' @title   mod_map_ui and mod_map_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_map
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 

mod_map_ui <- function(id){
  ns <- NS(id)
  shiny::fluidPage(
    shiny::tabPanel(title = "",
                    shiny::tags$style(type = "text/css", 
                                      "html, body{width:100%;height:100%}"),
                    shinydashboard::box(title = 'Display Rasters on a Map',
                                        status = 'primary', 
                                        width = 12, 
                                        solidHeader = TRUE, 
                                        collapsible = TRUE,
                                        helpText('You can choose different maps, 
                                            put different layers and 
                                          points on top by using the drop 
                                          down buttons')
                    ), 
                    shiny::fluidRow(column(width = 4,
                                           shiny::selectInput(ns("map_type"),
                                                              label = 'Choose map base layer:',
                                                              choices = c("Balck and white" = "OpenStreetMap.BlackAndWhite",
                                                                          "Open Street Map 1" = "OpenStreetMap.DE",
                                                                          "Open Street Map 2" = "OpenStreetMap.HOT"
                                                              ), selected = "OpenStreetMap.HOT"),
                                           shiny::selectInput(ns("map_raster"),
                                                              label = 'Choose overlay raster 1:',
                                                              #names need to be equal to file name
                                                              choices = c("None" = 'none',
                                                                          "Versiegelung" = 'versiegelung'
                                                              ), selected = "none"
                                           )
                    ),
                    column(
                      width = 4,
                      shiny::selectInput(ns("map_points_1"),
                                         label = 'Choose overlay points (blue):',
                                         #names need to be equal to file name
                                         choices = c("None" = 'none',
                                                     "McDonalds" = 'mcdonalds',
                                                     "Subway" = 'subway'
                                         ), selected = "none"),
                      shiny::selectInput(ns("map_polygon"),
                                         label = 'Choose polygon:',
                                         #names need to be equal to file name
                                         choices = c("None" = 'none',
                                                     "Versiegelung mittel" = 'Versiegelung_90_medium',
                                                     "Versiegelung einfach" = 'Versiegelung_90_simple',
                                                     "Versiegelung super einfach" = 'Versiegelung_90_supersimple'
                                         ), selected = "none"
                      )),
                    column(
                      width = 4,
                      shiny::selectInput(ns("map_raster_2"),
                                         label = 'Choose overlay raster 2:',
                                         #names need to be equal to file name
                                         choices = c("None" = 'none',
                                                     "GDP" = 'gdp_raster_agg',
                                                     "Aggriculture" = 'aggriculture_raster_proj_agg',
                                                     "CDU" = 'cdu_raster_proj_agg'
                                         ), selected = "none"
                      )),
                                         
                                         shiny::selectInput(ns("map_points_2"),
                                                                                label = 'Choose overlay points (green):',
                                                                                #names need to be equal to file name
                                                                                choices = c("None" = 'none',
                                                                                            "McDonalds" = 'mcdonalds',
                                                                                            "Subway" = 'subway'
                                                                                ), selected = "none")),
                                         
                                         shiny::selectInput(ns("map_points_2"),
                                                            label = 'Choose overlay points (green):',
                                                            #names need to be equal to file name
                                                            choices = c("None" = 'none',
                                                                        "McDonalds" = 'mcdonalds',
                                                                        "Subway" = 'subway'
                                                            ), selected = "none"
                                         
                      )),
                    shiny::fluidRow(column(width = 11, 
                                           leaflet::leafletOutput(ns('map'),
                                                                  width = "100%")
                    )),
                    shiny::fluidRow(column(width = 4,
                                           shiny::sliderInput(ns('opacity'), 
                                                              "Change opacity for Raster 1",
                                                              min = 0, max = 1, value = 0.5),
                                           shiny::selectInput(ns('color'), 
                                                              "Change color for Raster 1",
                                                              choices = choices_color,
                                                              selected = "BuPu")),
                                    column(width = 4,
                                           shiny::sliderInput(ns('opacity_2'), 
                                                              "Change opacity for Raster 2",
                                                              min = 0, max = 1, value = 0.5),
                                           shiny::selectInput(ns('color_2'), 
                                                              "Change color for Raster 1",
                                                              choices = choices_color,
                                                              selected = "Greens"))
                    ),
                    shiny::tabsetPanel(
                      shiny::tabPanel("Blue", DT::dataTableOutput(ns('cross_tbl_1'))),
                      shiny::tabPanel("Green", DT::dataTableOutput(ns('cross_tbl_2')))
                    ))
                                         
}
    
# Module Server
    
#' @rdname mod_map
#' @export
#' @keywords internal
    
mod_map_server <- function(input, output, session){
  ns <- session$ns
  ### Initialise starting map
  output$map <- leaflet::renderLeaflet({
    ns <- session$ns
    
    leaflet::leaflet() %>%
      leaflet::addProviderTiles("OpenStreetMap.HOT", layerId = "tiles"
      ) %>%
      leaflet.extras::addFullscreenControl() %>%
      leaflet.extras::addSearchOSM(options = searchOptions(autoCollapse = TRUE, 
                                                           minLength = 2, 
                                                           hideMarkerOnCollapse = TRUE,
                                                           zoom = 10)) %>%
      leaflet::setView(lat = 52.526576, lng = 13.387075, zoom = 10)
    
  })
  
  ### change raster 1 on selectinput 
  shiny::observe({
    ns <- session$ns
    shiny::req(input$map_raster)
    
    if (input$map_raster != 'none'){ 
      
      ### Load pre-calculated Raster
      data_raster <- raster::raster(paste0("data/raster_projections/aggregated/", 
                                           input$map_raster,".gri"))
      pal <- leaflet::colorNumeric(palette = input$color, values(data_raster),
                                   na.color = "transparent")
      
      leaflet::leafletProxy(ns('map')) %>%
        leaflet::removeImage(layerId = "raster") %>%
        leaflet::addRasterImage(data_raster, 
                                layerId = "raster",
                                project = FALSE, 
                                maxBytes = 50 * 1024 * 1024,
                                opacity = input$opacity,
                                colors = pal) %>%
        leaflet::addLegend(values = values(data_raster),
                           pal = pal, 
                           layerId = "legend_raster",
                           title = input$map_raster)
    } else {
      leaflet::leafletProxy(ns('map')) %>%
        leaflet::removeImage(layerId = "raster") %>%
        leaflet::removeControl(layerId = "legend_raster")
    }
    
  }#, ignoreNULL = TRUE, ignoreInit = FALSE
  )
  ### change raster 2 on selectinput
  shiny::observe({
    ns <- session$ns
    shiny::req(input$map_raster_2)
    
    if (input$map_raster_2 != 'none'){ 
      
      ### Load pre-calculated Raster
      data_raster <- raster::raster(paste0("data/raster_projections/aggregated/", 
                                           input$map_raster_2,".gri"))
      pal <- leaflet::colorNumeric(palette = input$color_2, values(data_raster),
                                   na.color = "transparent")
      
      leaflet::leafletProxy(ns('map')) %>%
        leaflet::removeImage(layerId = "raster_2") %>%
        leaflet::addRasterImage(data_raster, 
                                layerId = "raster_2",
                                project = FALSE, 
                                maxBytes = 50 * 1024 * 1024,
                                opacity = input$opacity_2,
                                colors = pal) %>%
        leaflet::addLegend(values = values(data_raster),
                           pal = pal, 
                           layerId = "legend_raster_2",
                           title = input$map_raster_2)
    } else {
      leaflet::leafletProxy(ns('map')) %>%
        leaflet::removeImage(layerId = "raster_2") %>%
        leaflet::removeControl(layerId = "legend_raster_2")
    }
    
  }#, ignoreNULL = TRUE, ignoreInit = FALSE
  )
  
  ### change base map layer on select input
  shiny::observeEvent(input$map_type, {
    ns <- session$ns
    
    leaflet::leafletProxy(ns('map')) %>%
      leaflet::removeTiles(layerId = "tiles") %>%
      leaflet::addProviderTiles(input$map_type, layerId = "tiles")
    
  }#, ignoreNULL = TRUE, ignoreInit = FALSE
  )
  
  ### get shared data for points 1 (blue)
  data_points_one <- shiny::eventReactive(input$map_points_1, {
    
    data_points <- data.table::fread(paste0("data/points/",input$map_points_1, ".csv"),
                                     dec = ",")
    data_points_sd <- crosstalk::SharedData$new(data_points)
    
  })
  ### change base points_1 (blue) on select input
  shiny::observeEvent(input$map_points_1, {
    ns <- session$ns
    shiny::req(data_points_one())
    
    data_points <- data_points_one()
    
    leaflet::leafletProxy(ns('map')) %>%
      leaflet::clearGroup(group = 'points_1') %>%
      leaflet::addCircles(lng = ~lon, lat = ~lat, 
                          data = data_points, 
                          popup = c(paste0("This ", data_points$data()$name," is in ",
                                           data_points$data()$addr.city)),
                          color = 'blue',
                          group = 'points_1')
    
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  ### crosstalk table for base points_1 (blue) on select input
  output$cross_tbl_1 <- DT::renderDataTable({
    ns <- session$ns
    shiny::req(data_points_one())
    
    if (input$map_points_1 != 'none'){
      DT::datatable(data_points_one(), 
                    extensions = c('Select', 'Buttons'), 
                    options = list(
                      dom = 'Bfrtip',
                      buttons = c('selectAll', 'selectNone')
                    )
      )
    }
  }, server = FALSE)
  
  
  ### get shared data for points 2 (green)
  
  data_points_two <- shiny::eventReactive(input$map_points_2, {
    
    data_points <- data.table::fread(paste0("data/points/",input$map_points_2, ".csv"),
                                     dec = ",")
    data_points_sd <- crosstalk::SharedData$new(data_points)
    
  })
  
  ### change base points_2 (green) on select input
  shiny::observeEvent(input$map_points_2, {
    ns <- session$ns
    shiny::req(data_points_two())
    
    data_points <- data_points_two()
    
    leaflet::leafletProxy(ns('map')) %>%
      leaflet::clearGroup(group = 'points_2') %>%
      leaflet::addCircles(lng = ~lon, lat = ~lat,  
                          data = data_points, 
                          popup = c(paste0("This ", data_points$data()$name," is in ",
                                           data_points$data()$addr.city)),
                          color = 'green',
                          group = 'points_2')
    
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  ### crosstalk table for base points_2 (green) on select input
  output$cross_tbl_2 <- DT::renderDataTable({
    ns <- session$ns
    shiny::req(data_points_two())
    
    if (input$map_points_2 != 'none'){
      DT::datatable(data_points_two(),
                    extensions = c('Select', 'Buttons'), options = list(
                      dom = 'Bfrtip',
                      buttons = c('selectAll', 'selectNone'))
      )
    }
  }, server = FALSE)
  
  
  ### change polygons select input
  shiny::observeEvent(input$map_polygon, {
    ns <- session$ns
    
    if (input$map_polygon != 'none'){
      data_polygon <- rgdal::readOGR(dsn="data/polygons/Polygone Versiegelung/", 
                                     layer= input$map_polygon)
      
      leaflet::leafletProxy(ns('map')) %>%
        leaflet::clearGroup(group = 'polygon') %>%
        leaflet::addPolygons(data = data_polygon,
                             group = 'polygon')
    } else {
      leaflet::leafletProxy(ns('map')) %>%
        leaflet::clearGroup(group = 'polygon') 
    }
    
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  
}
    
## To be copied in the UI
# mod_map_ui("map_ui_1")
    
## To be copied in the server
# callModule(mod_map_server, "map_ui_1")
 
