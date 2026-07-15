library(shiny)
library(leaflet)
library(sf)
library(httr)
library(jsonlite)
library(geojsonsf)

# Öffentlicher Valhalla-Server (FOSSGIS/OSM) - kein API-Key nötig, aber Rate-Limit 1 req/s
VALHALLA_URL <- "https://valhalla1.openstreetmap.de/isochrone"

# Mapping: Profile-Namen -> Valhalla-Costing-Modelle
profile_to_costing <- c(
  "driving-car" = "auto",
  "cycling-regular" = "bicycle",
  "foot-walking" = "pedestrian"
)

default_points <- list(
  p1 = list(lon = 16.3738, lat = 48.2112, time = 11),   # Stephansplatz
  p2 = list(lon = 16.3428, lat = 48.2043, time = 10),  # Westbahnhof
  p3 = list(lon = 16.3725, lat = 48.1831, time = 14),   # Hauptbahnhof
  p4 = list(lon = 16.3591, lat = 48.2196, time = 9)    # Schottentor
)

ui <- fluidPage(
  titlePanel("Valhalla Isochronen & Intersections (Wien)"),
  sidebarLayout(
    sidebarPanel(
      p("Berechnung mittels öffentlicher Valhalla-API und clientseitiger Schnittmengen-Berechnung (sf)."),
      selectInput("profile", "Transportmittel (Profil):",
                  choices = c("Auto" = "driving-car", "Fahrrad" = "cycling-regular", "Fußgänger" = "foot-walking")),
      hr(),
      tags$b("Punkt 1"),
      fluidRow(
        column(4, numericInput("p1_lon", "X (Lon)", default_points$p1$lon, step = 0.001)),
        column(4, numericInput("p1_lat", "Y (Lat)", default_points$p1$lat, step = 0.001)),
        column(4, numericInput("p1_time", "Zeit (min)", default_points$p1$time, min = 3, max = 30))
      ),
      hr(),
      tags$b("Punkt 2"),
      fluidRow(
        column(4, numericInput("p2_lon", "X (Lon)", default_points$p2$lon, step = 0.001)),
        column(4, numericInput("p2_lat", "Y (Lat)", default_points$p2$lat, step = 0.001)),
        column(4, numericInput("p2_time", "Zeit (min)", default_points$p2$time, min = 3, max = 30))
      ),
      hr(),
      tags$b("Punkt 3"),
      fluidRow(
        column(4, numericInput("p3_lon", "X (Lon)", default_points$p3$lon, step = 0.001)),
        column(4, numericInput("p3_lat", "Y (Lat)", default_points$p3$lat, step = 0.001)),
        column(4, numericInput("p3_time", "Zeit (min)", default_points$p3$time, min = 3, max = 30))
      ),
      hr(),
      tags$b("Punkt 4"),
      fluidRow(
        column(4, numericInput("p4_lon", "X (Lon)", default_points$p4$lon, step = 0.001)),
        column(4, numericInput("p4_lat", "Y (Lat)", default_points$p4$lat, step = 0.001)),
        column(4, numericInput("p4_time", "Zeit (min)", default_points$p4$time, min = 3, max = 30))
      ),
      br(),
      actionButton("calc_btn", "Valhalla-Isochronen berechnen", class = "btn-success", width = "100%")
    ),
    mainPanel(
      leafletOutput("map", height = "750px")
    )
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 16.3738, lat = 48.2082, zoom = 13)
  })
  
  # Einzelne Isochrone für EINEN Punkt via Valhalla abrufen
  get_isochrone_valhalla <- function(lon, lat, time_min, costing) {
    body <- list(
      locations = list(list(lon = lon, lat = lat)),
      costing = costing,
      contours = list(list(time = time_min)),  # Valhalla erwartet Minuten, nicht Sekunden!
      polygons = TRUE,
      denoise = 0.5,
      generalize = 20
    )
    
    resp <- POST(
      VALHALLA_URL,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      add_headers("Content-Type" = "application/json"),
      encode = "json"
    )
    
    if (status_code(resp) != 200) {
      stop(paste("Valhalla-API-Fehler:", content(resp, "text", encoding = "UTF-8")))
    }
    
    geojson_sf(content(resp, "text", encoding = "UTF-8"))
  }
  
  observeEvent(input$calc_btn, {
    
    pts <- list(
      list(lon = input$p1_lon, lat = input$p1_lat, time = input$p1_time),
      list(lon = input$p2_lon, lat = input$p2_lat, time = input$p2_time),
      list(lon = input$p3_lon, lat = input$p3_lat, time = input$p3_time),
      list(lon = input$p4_lon, lat = input$p4_lat, time = input$p4_time)
    )
    
    costing <- profile_to_costing[[input$profile]]
    
    withProgress(message = 'Verbinde mit Valhalla-API...', value = 0.1, {
      
      iso_list <- vector("list", length(pts))
      
      for (i in seq_along(pts)) {
        incProgress(0.15, detail = paste("Isochrone Punkt", i))
        iso_list[[i]] <- tryCatch(
          get_isochrone_valhalla(pts[[i]]$lon, pts[[i]]$lat, pts[[i]]$time, costing),
          error = function(e) {
            showNotification(paste("Fehler bei Punkt", i, ":", e$message), type = "error")
            NULL
          }
        )
        # Rate-Limit des öffentlichen Servers beachten (max. 1 req/s)
        if (i < length(pts)) Sys.sleep(1.1)
      }
      
      if (any(sapply(iso_list, is.null))) return()
      
      incProgress(0.2, detail = "Berechne Schnittmenge (sf)...")
      
      polys <- lapply(iso_list, function(x) st_geometry(x)[[1]] |> st_sfc(crs = 4326))
      
      # --- Robuste Schnittmengenberechnung ---
      intersection <- polys[[1]]
      intersection_valid <- TRUE
      
      for (i in 2:length(polys)) {
        new_intersection <- tryCatch(
          st_intersection(intersection, polys[[i]]),
          error = function(e) NULL
        )
        if (is.null(new_intersection) || length(new_intersection) == 0) {
          intersection <- st_sfc(crs = 4326)  # explizit leer, aber gültig auswertbar
          intersection_valid <- FALSE
          break
        }
        intersection <- new_intersection
      }
      
      # Zusätzliche Absicherung: falls die letzte Intersection selbst leer ist
      if (intersection_valid && length(intersection) > 0) {
        intersection_valid <- isTRUE(!st_is_empty(intersection)[1])
      } else {
        intersection_valid <- FALSE
      }
      # --- Ende robuste Schnittmengenberechnung ---
      
      incProgress(0.2, detail = "Aktualisiere Leaflet-Karte...")
      
      proxy <- leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers()
      
      proxy %>% addMarkers(
        lng = sapply(pts, `[[`, "lon"),
        lat = sapply(pts, `[[`, "lat"),
        popup = "Startpunkt"
      )
      
      for (i in seq_along(polys)) {
        proxy %>% addPolygons(
          data = polys[[i]],
          fillColor = "#3388ff",
          fillOpacity = 0.15,
          color = "#3388ff",
          weight = 1,
          group = "Isochronen",
          popup = "Erreichbarkeitszone"
        )
      }
      
      if (intersection_valid) {
        proxy %>% addPolygons(
          data = intersection,
          fillColor = "#FF0000",
          fillOpacity = 0.6,
          color = "#D30000",
          weight = 3,
          group = "Isochronen",
          popup = "Gemeinsame Schnittmenge (Valhalla, clientseitig berechnet)"
        )
      } else {
        showNotification("Keine gemeinsame Schnittmenge für die angegebenen Zeiten gefunden.", type = "warning")
      }
    })
  })
}

shinyApp(ui = ui, server = server)