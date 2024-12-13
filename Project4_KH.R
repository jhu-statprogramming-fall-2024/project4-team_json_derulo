library(shiny)
library(googleway)
library(httr)
library(jsonlite)
library(purrr)
library(R6)
library(geosphere)
library(DT)
library(googlePolylines)

# Polyline Decoding Function
decode_pl <- function(encoded) {
  # Decodes Google Maps encoded polyline
  require(googleway)
  
  # Check if input is valid
  if (is.null(encoded) || encoded == "") {
    return(data.frame(lat = numeric(0), lon = numeric(0)))
  }
  
  # Use googleway's decode function
  decoded <- googleway::decode_pl(encoded)
  
  # Ensure the result is a data frame with lat and lon columns
  if (is.null(decoded) || nrow(decoded) == 0) {
    return(data.frame(lat = numeric(0), lon = numeric(0)))
  }
  
  return(decoded)
}


create_clinic <- function(name, address, zip, open_hours = NULL, max_GAw, max_GAd = NULL, phone, lat, long, website = NULL) {
  structure(
    list(
      name = name,
      address = address,
      zip = zip,
      open_hours = open_hours,
      max_GAw = max_GAw,
      max_GAd = max_GAd, 
      phone = phone,
      lat = lat,
      long = long,
      website = website
    ),
    class = "clinic"
  )
}

# Clinics data
##Clinics: 
clinics <- list(
  create_clinic("DuPont Clinic", "1120 19th St NW", 20036, "M-F 0900 - 1700, Sa 1000 - 1400", 32, 6, "202-844-2004", 38.90478876534545, -77.04373883435558, "https://dupontclinic.com"),
  create_clinic("carafem Health Center", "5530 Wisconsin Ave, Suite 1200", 20815, NULL, 13, 0, "855-SAY-CARA", 38.96496013009437, -77.08803977909659, "https://carafem.org"),
  create_clinic("Clinics for Abortion & Reproductive Excellence", "10401 Old Georgetown Rd, Suite 104", 20814, "M 0900 - 1700, T-F 0830 - 1700, Sa 0900 - 1530", 35, 0, "301-517-6810", 39.02660039064823, -77.12531972883426, NULL),
  create_clinic("Partners in Abortion Care", "7305 Baltimore Ave, Suite 107", 20740, NULL, 34, 0, "301-932-1222", 38.979095445474755, -76.9368688521127, "https://partnersclinic.com"),
  create_clinic("Whole Woman's Health of Baltimore", "7648 Belair Rd, Floor 1", 21236, NULL, 22, 0, "877-835-1090", 39.367552304718366, -76.52029503491501, "https://wholewomanshealth.com"),
  create_clinic("Women's Health Center of Maryland", "17204 McMullen Hwy SW", 21557, "M-Th 0800 - 1700, F 0800-1300", 16, 0, "301-709-5101", 39.562494789850064, -78.85909350884934, "https://www.womenshealthmd.org"),
  create_clinic("A Woman's Choice of Raleigh", "3305 Drake Cir", 27607, "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200", 12, 0, "919-781-6811", 35.814774106462025, -78.6940318539617, "https://www.awomanschoiceinc.com/awc-raleigh/"),
  create_clinic("A Woman's Choice of Greensboro", "2425 Randleman Rd", 27406, "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200", 12, 0, "336-273-9465", 36.03672864287146, -79.79903671534046, "https://www.awomanschoiceinc.com/awc-greensboro/"),
  create_clinic("A Woman's Choice of Charlotte", "421 N Wendover Rd", 28211, "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200", 12, 0, "704-367-2255", 35.19012178939107, -80.80493254840565, "https://www.awomanschoiceinc.com/awc-charlotte/"),
  create_clinic("A Capital Women's Health Clinic", "1511 Starling Dr", 23229, "M-F 0800 - 1700, Sa 0900 - 1200", 16, 0, "804-754-1928", 37.6056619856187, -77.56699732432077,"https://capitalwomenshealth.com"),
  create_clinic("A Woman's Choice of Danville", "159 Executive Dr, Suite E", 24541, "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200", 15, 0, "434-218-1032", 36.59342415106964, -79.43234447301381,"https://www.awomanschoiceinc.com/awc-danville/"),
  create_clinic("Bristol Women's Health", "2603 Osborne St", 24201, "M-F 0800 - 1700, Sa = 0800 - 1200", 16, 0, "276-494-0501", 36.60156709693608, -82.21738143120973, "https://bristolwomenshealth.com"),
  create_clinic("Falls Church Healthcare Center", "900 S Washington St, Suite 300", 22046, NULL, 15, 6, "703-532-2500", 38.87943123725555, -77.18239460563538, "https://fallschurchhealthcare.com"),
  create_clinic("Meadow Reproductive Health & Wellness", "1749 Old Meadow Rd, #600", 22102, "M-F 0930 - 1700", 12, 0, "703-783-3300", 38.922101211643124, -77.21019365166195, "https://meadowrepro.org"),
  create_clinic("Whole Woman's Health of Alexandria", "2839 Duke St", 22314, "M-F 0900 - 1700, Sa 0830 - 1500", 18, 0, "703-667-4064", 38.80787625143519, -77.07899630133653, "https://wholewomanshealth.com/abortion-clinics/alexandria-va/"),
  create_clinic("Whole Woman's Health of Charlottesville", "2321 Commonwealth Dr", 22901, "M-F 0800 - 1700", 18, 0, "434-973-4888", 38.07390158589749, -78.48611373072481,"https://wholewomanshealth.com/abortion-clinics/charlottesville-va/")
)

# R6 Class for User Location
Location <- R6Class("Location",
                    public = list(
                      address = NULL,
                      lat = NULL,
                      lng = NULL,
                      api_key = NULL,
                      
                      initialize = function(address, api_key) {
                        self$address <- address
                        self$api_key <- api_key
                      },
                      
                      fetch_coordinates = function() {
                        geocode_url <- paste0(
                          "https://maps.googleapis.com/maps/api/geocode/json?address=",
                          URLencode(self$address),
                          "&key=", self$api_key
                        )
                        response <- httr::GET(geocode_url)
                        geocode_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
                        
                        if (geocode_data$status == "OK" && length(geocode_data$results) > 0) {
                          self$lat <- geocode_data$results$geometry$location$lat
                          self$lng <- geocode_data$results$geometry$location$lng
                        } else {
                          stop("Invalid Address or API Error")
                        }
                      }
                    )
)

# UI
ui <- fluidPage(
  titlePanel("Find Nearest Clinic"),
  sidebarLayout(
    sidebarPanel(
      h4("Enter your location and preferences:"),
      textInput("address", "Street Address", ""),
      textInput("city", "City", ""),
      textInput("state", "State", ""),
      numericInput("zip", "Zip Code", value = NA, min = 10000, max = 99999),
      numericInput("max_gaw", "Gestational Age (weeks):", value = NA, min = 0, max = 48),
      actionButton("find", "Find Distance to Clinics")
    ),
    mainPanel(
      # Legend
      tags$div(
        style = "position: absolute; top: 10px; left: 10px; background-color: white; padding: 10px; border: 1px solid #ccc; z-index: 1000;", # Style the legend
        tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: #FF0000; margin-right: 5px;"), "Driving", tags$br(),
        tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: #00FF00; margin-right: 5px;"), "Walking", tags$br(),
        tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: #0000FF; margin-right: 5px;"), "Transit"
      ),
      google_mapOutput(outputId = "map"),
      DTOutput("clinicTable") 
    )
  )
)

# Server
server <- function(input, output, session) {
  api_key <- Sys.getenv("GOOGLE_API")
  location <- reactiveVal(NULL)
  selected_clinic <- reactiveVal(NULL)
  route_data <- reactiveVal(list())
  
  observeEvent(input$find, {
    req(input$address)
    
    loc <- Location$new(paste(input$address, input$city, input$state, input$zip, sep = ", "), api_key)
    tryCatch({
      loc$fetch_coordinates()
      location(loc)
      
      max_gaw <- ifelse(is.na(input$max_gaw), Inf, as.numeric(input$max_gaw))
      filtered_clinics <- keep(clinics, ~ .x$max_GAw >= max_gaw)
      
      distances <- map_dbl(filtered_clinics, ~ {
        distm(
          c(loc$lng, loc$lat),
          c(.x$long, .x$lat),
          fun = distHaversine
        ) / 1609.344
      })
      
      sorted_indices <- order(distances)
      sorted_clinics <- filtered_clinics[sorted_indices]
      sorted_distances <- distances[sorted_indices]
      closest_clinic <- sorted_clinics[[1]]
      
      # Added hyperlinks
      clinic_data <- data.frame(
        Name = map_chr(sorted_clinics, ~ {
          if (!is.null(.x$website) && .x$website != "") {
            paste0('<a href="', .x$website, '" target="_blank">', .x$name, '</a>')
          } else {
            .x$name
          }
        }),
        Address = map_chr(sorted_clinics, "address"),
        Distance = map_chr(sorted_distances, ~ as.character(round(.x, 1))),
        Phone = map_chr(sorted_clinics, "phone"),
        MaxGA = map_chr(sorted_clinics, ~ as.character(.x$max_GAw)),
        stringsAsFactors = FALSE
      )
      
      # Prepare map data with clinics and user location
      map_data <- data.frame(
        Latitude = c(map_dbl(sorted_clinics, ~ .x[["lat"]]), loc$lat),
        Longitude = c(map_dbl(sorted_clinics, ~ .x[["long"]]), loc$lng),
        Name = c(map_chr(sorted_clinics, ~ .x[["name"]]), "Your Location"),
        colour = c(rep("red", length(sorted_clinics)), "blue"),
        stringsAsFactors = FALSE
      )
      
      # Route modes
      route_modes <- c("driving", "walking", "transit")
      
      # Route results preparation
      route_results <- lapply(route_modes, function(mode) {
        route_url <- paste0(
          "https://maps.googleapis.com/maps/api/directions/json?",
          "origin=", URLencode(paste(loc$lat, loc$lng, sep = ",")),
          "&destination=", URLencode(paste(closest_clinic$lat, closest_clinic$long, sep = ",")),
          "&mode=", mode,
          "&key=", api_key
        )
        
        response <- httr::GET(route_url)
        route_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
        
        if (response$status_code != 200) {
          print(paste("API Error:", response$status_code, response$reason))
          return(NULL)
        }
        
        if (route_data$status != "OK") {
          print(paste("No route found for", mode))
          return(NULL)
        }
        
        # Extract route polyline
        route_polyline <- route_data$routes$overview_polyline$points
        
        # Extract route details
        route_details <- list(
          distance = route_data$routes$legs[[1]]$distance$text,
          duration = route_data$routes$legs[[1]]$duration$text,
          polyline = route_polyline
        )
        
        return(route_details)
      })
      names(route_results) <- route_modes
      
      # Render map with routes
      output$map <- renderGoogle_map({
        map <- google_map(key = api_key) %>%
          add_markers(
            data = map_data,
            lat = "Latitude",
            lon = "Longitude",
            colour = "colour",
            mouse_over = "Name"
          )
        
        # Add routes for each mode
        for (mode in route_modes) {
          if (!is.null(route_results[[mode]])) {
            # Decode the polyline
            route_coords <- decode_pl(route_results[[mode]]$polyline)
            
            # Check if coordinates are valid before adding
            if (nrow(route_coords) > 0) {
              map <- map %>%
                add_polylines(
                  data = route_coords,
                  lat = "lat",  # Explicitly specify column names
                  lon = "lon",
                  stroke_colour = switch(mode,
                                         "driving" = "#FF0000",  # Red for driving
                                         "walking" = "#00FF00",  # Green for walking
                                         "transit" = "#0000FF"   # Blue for transit
                  ),
                  stroke_weight = 3
                )
              
              
            }
          }
        }
        
        map
      })
      
      # Render interactive DataTable
      output$clinicTable <- renderDT({
        datatable(
          clinic_data,
          escape = FALSE,
          options = list(
            pageLength = 5,
            lengthMenu = c(5, 10, 15),
            searching = FALSE,
            columnDefs = list(
              list(
                targets = '_all',
                className = 'dt-center'  # Center align all columns
              )
            )
          ),
          colnames = c(
            "Clinic Name",
            "Address",
            "Distance (miles)",
            "Phone",
            "Max Gestational Age (weeks)"
          )
        )
      })
      
      # Create a modal to show route details
      showModal(modalDialog(
        title = paste("Routes to", closest_clinic$name),
        HTML(
          paste(
            "<h4>Driving Route:</h4>",
            ifelse(!is.null(route_results$driving),
                   paste("Distance:", route_results$driving$distance,
                         "<br>Duration:", route_results$driving$duration),
                   "No driving route available"),
            "<h4>Walking Route:</h4>",
            ifelse(!is.null(route_results$walking),
                   paste("Distance:", route_results$walking$distance,
                         "<br>Duration:", route_results$walking$duration),
                   "No walking route available"),
            "<h4>Transit Route:</h4>",
            ifelse(!is.null(route_results$transit),
                   paste("Distance:", route_results$transit$distance,
                         "<br>Duration:", route_results$transit$duration),
                   "No transit route available")
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
