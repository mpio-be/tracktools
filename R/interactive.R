#' flagpts
#' Flag outliers interactively
#' @param d a DT returned by argos_prepare()
#' @param R point radius
#' @export
#' @examples
#' x <- dbq(q = "Select * from ARGOS.2018_PESA where tagID ='52753' ") |> argos_prepare()

#' flagpts(x)
flagpts <- function(d, R = 10) {
  speed_along(d)

  if (!"flag" %in% names(d)) d[, flag := NA]
  d[is.na(flag), flag := 0]

  d[, col := fcase(flag == 0, "#c4390f", flag == 1, "#6d5e59")]

  dlines <- st_as_sf(d[flag == 0], coords = c("longitude", "latitude"), crs = 4326) |>
    st_points2lines()

  ui <- fluidPage(
    leafletOutput("map", height = "100vh"),
    absolutePanel(
      id = "controls", fixed = TRUE,
      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
      sliderInput("maxspeed", "Max speed (kmh)", 50, 500, 300)
    )
  )


  server <- function(input, output) {
    output$map <- renderLeaflet({
      leaflet() |>
        addProviderTiles("Esri.WorldGrayCanvas") |>
        addGeodesicPolylines(data = dlines) |>
        addCircleMarkers(
          data = d,
          lng = ~longitude,
          lat = ~latitude,
          fillColor = ~col,
          color = NA,
          radius = R,
          fillOpacity = .75,
          layerId = ~pk
        )
    })

    observeEvent(input$maxspeed, {
      if (!is.null(input$map_marker_click)) {
        showNotification("You should adjust speed before selecting points. Start again!")
      }
    })


    observe({
      if (is.null(input$map_marker_click)) {
        d[speed_kmh >= as.numeric(input$maxspeed), ":="(flag = 1, col = "#6d5e59")]
        d[speed_kmh < as.numeric(input$maxspeed), ":="(flag = 0, col = "#c4390f")]
      }

      if (!is.null(input$map_marker_click)) {
        d[pk == input$map_marker_click$id, ":="(flag = 1, col = "#6d5e59")]
      }


      xlines <- st_as_sf(d[flag == 0], coords = c("longitude", "latitude"), crs = 4326) |>
        st_points2lines()


      leafletProxy("map") |>
        clearMarkers() |>
        clearShapes() |>
        addGeodesicPolylines(data = xlines) |>
        addCircleMarkers(
          data = d,
          lng = ~longitude,
          lat = ~latitude,
          fillColor = ~col,
          color = NA,
          radius = R,
          fillOpacity = .75,
          layerId = ~pk
        )
    })
  }


  shinyApp(ui = ui, server = server)
}
