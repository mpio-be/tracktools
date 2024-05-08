#' flagpts
#' Flag outliers interactively
#' @param d a DT returned by argos_prepare()
#' @param R point radius passed to leafltet::addCircleMarkers. 
#' @param max_speed the initial maximum speed (km/h) used as a selection criterion.
#' @param use_speed should speed be used to flag points?
#' @param shift_lon translate longitude so that it works with leaflet
#' @export
#' @examples
#' x <- dbq(q = "Select * from ARGOS.2018_PESA where tagID ='52753' ") |> argos_prepare()
#' flagpts(x)

flagpts <- function(d, R = 10, max_speed = 500, use_speed = FALSE, shift_lon = TRUE) {
  
  speed_along(d)

  d[, longitude2 := longitude]

  if(shift_lon)
    d[, longitude2 := (longitude + 360) %% 360]
  
  if (!"flag" %in% names(d)) d[, flag := NA]
  d[is.na(flag), flag := 0]

  d[, col := fcase(flag == 0, "#c4390f", flag == 1, "#6d5e59")]

  dlines <- st_as_sf(d[flag == 0], coords = c("longitude", "latitude"), crs = 4326) |>
    st_points2lines(shift_lon = shift_lon) 
    

  ui <- fluidPage(
    leafletOutput("map", height = "100vh"),
    absolutePanel(
      id = "controls", fixed = TRUE,
      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
      sliderInput(
        inputId = "maxspeed", 
        label = "Max speed (kmh)", 
        min   = 50, 
        max   = 500, 
        value = max_speed)
    )
  )


  server <- function(input, output) {

    # onStop(function() assign("flagged_points", d[(flag), pk], envir = .GlobalEnv))

    output$map <- renderLeaflet({
      leaflet() |>
        addProviderTiles("Esri.WorldGrayCanvas") |>
        addPolylines(data = dlines) |>
        addCircleMarkers(
          data        = d,
          lng         = ~longitude2,
          lat         = ~latitude,
          fillColor   = ~col,
          color       = NA,
          radius      = R,
          fillOpacity = .75,
          layerId     = ~pk
        )
    })

    observeEvent(input$maxspeed, {
      if (!is.null(input$map_marker_click)) {
        showNotification("You should adjust speed before selecting points. Start again!")
      }
    })


    observe({
      if (is.null(input$map_marker_click) && use_speed) {
        d[speed_kmh >= as.numeric(input$maxspeed), ":="(flag = 1, col = "#6d5e59")]
        d[speed_kmh <  as.numeric(input$maxspeed), ":="(flag = 0, col = "#c4390f")]
      }

      if (!is.null(input$map_marker_click)) {
        d[pk == input$map_marker_click$id, ":="(flag = 1, col = "#6d5e59")]
      }


      xlines <- st_as_sf(d[flag == 0], coords = c("longitude", "latitude"), crs = 4326) |>
        st_points2lines(shift_lon = shift_lon)


      leafletProxy("map") |>
        clearMarkers()    |>
        clearShapes()     |>
        addPolylines(
          data   = xlines, 
          weight = 2.5) |>
        addCircleMarkers(
          data        = d,
          lng         = ~longitude2,
          lat         = ~latitude,
          fillColor   = ~col,
          color       = NA,
          radius      = R,
          fillOpacity = 0.75,
          layerId     = ~pk
        )
    })
  }


  shinyApp(ui = ui, server = server)
}
