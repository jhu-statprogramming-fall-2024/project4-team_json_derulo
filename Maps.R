####################### REVISED CODE- INCLUDES BLUE DOT FOR YOUR OWN LOCATION !!!!! ################


library(shiny)
library(googleway)
library(httr)
library(jsonlite)
library(purrr)

library(R6)

# Clinic creation function
create_clinic <- function(name, address, zip, open_hours = NULL, max_GAw, max_GAd = NULL, phone, lat, long) {
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
      long = long
    ),
    class = "clinic"
  )
}

# Clinics data
##Clinics: 
clinics <- list(
  create_clinic("DuPont Clinic", "1120 19th St NW", 20036, "M-F 0900 - 1700, Sa 1000 - 1400", 32, 6, "202-844-2004", 38.90478876534545, -77.04373883435558),
  create_clinic("carafem Health Center", "5530 Wisconsin Ave, Suite 1200", 20815, NULL, 13, 0, "855-SAY-CARA", 38.96496013009437, -77.08803977909659),
  create_clinic("Clinics for Abortion & Reproductive Excellence", "10401 Old Georgetown Rd, Suite 104", 20814, "M 0900 - 1700, T-F 0830 - 1700, Sa 0900 - 1530", 35, 0, "301-517-6810", 39.02660039064823, -77.12531972883426),
  create_clinic("Partners in Abortion Care", "7305 Baltimore Ave, Suite 107", 20740, NULL, 34, 0, "301-932-1222", 38.979095445474755, -76.9368688521127),
  create_clinic("Whole Woman's Health of Baltimore", "7648 Belair Rd, Floor 1", 21236, NULL, 22, 0, "877-835-1090", 39.367552304718366, -76.52029503491501),
  create_clinic("Women's Health Center of Maryland", "17204 McMullen Hwy SW", 21557, "M-Th 0800 - 1700, F 0800-1300", 16, 0, "301-709-5101", 39.562494789850064, -78.85909350884934),
  create_clinic("A Woman's Choice of Raleigh", "3305 Drake Cir", 27607, "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200", 12, 0, "919-781-6811", 35.814774106462025, -78.6940318539617),
  create_clinic("A Woman's Choice of Greensboro", "2425 Randleman Rd", 27406, "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200", 12, 0, "336-273-9465", 36.03672864287146, -79.79903671534046),
  create_clinic("A Woman's Choice of Charlotte", "421 N Wendover Rd", 28211, "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200", 12, 0, "704-367-2255", 35.19012178939107, -80.80493254840565),
  create_clinic("A Capital Women's Health Clinic", "1511 Starling Dr", 23229, "M-F 0800 - 1700, Sa 0900 - 1200", 16, 0, "804-754-1928", 37.6056619856187, -77.56699732432077),
  create_clinic("A Woman's Choice of Danville", "159 Executive Dr, Suite E", 24541, "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200", 15, 0, "434-218-1032", 36.59342415106964, -79.43234447301381),
  create_clinic("Bristol Women's Health", "2603 Osborne St", 24201, "M-F 0800 - 1700, Sa = 0800 - 1200", 16, 0, "276-494-0501", 36.60156709693608, -82.21738143120973),
  create_clinic("Falls Church Healthcare Center", "900 S Washington St, Suite 300", 22046, NULL, 15, 6, "703-532-2500", 38.87943123725555, -77.18239460563538),
  create_clinic("Meadow Reproductive Health & Wellness", "1749 Old Meadow Rd, #600", 22102, "M-F 0930 - 1700", 12, 0, "703-783-3300", 38.922101211643124, -77.21019365166195),
  create_clinic("Whole Woman's Health of Alexandria", "2839 Duke St", 22314, "M-F 0900 - 1700, Sa 0830 - 1500", 18, 0, "703-667-4064", 38.80787625143519, -77.07899630133653),
  create_clinic("Whole Woman's Health of Charlottesville", "2321 Commonwealth Dr", 22901, "M-F 0800 - 1700", 18, 0, "434-973-4888", 38.07390158589749, -78.48611373072481)
)


# R6 Class for Location
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

# UI definition
ui <- fluidPage(
  titlePanel("Find Nearest Clinic"),
  sidebarLayout(
    sidebarPanel(
      textInput("address", "Enter Full Address:", ""),
      numericInput("max_gaw", "Gestational Age (weeks):", value = NA, min = 0, max = 48),
      actionButton("find", "Find Distance to Clinics")
    ),
    mainPanel(
      google_mapOutput(outputId = "map"),
      verbatimTextOutput(outputId = "distances")
    )
  )
)

server <- function(input, output, session) {
  api_key <- ""  # Replace with the actual API key
  location <- reactiveVal(NULL)
  
  observeEvent(input$find, {
    req(input$address)
    
    loc <- Location$new(input$address, api_key)
    tryCatch({
      loc$fetch_coordinates()
      location(loc)
      
      max_gaw <- ifelse(is.na(input$max_gaw), NULL, input$max_gaw)
      
      # Filter clinics based on gestational age
      filtered_clinics <- Filter(function(clinic) clinic$max_GAw >= max_gaw, clinics)
      
      # Prepare data for clinic markers
      clinic_data <- data.frame(
        Latitude = sapply(filtered_clinics, `[[`, "lat"),
        Longitude = sapply(filtered_clinics, `[[`, "long"),
        Name = sapply(filtered_clinics, `[[`, "name"),
        colour = "red"  # Default color for clinics
      )
      
      # Prepare user's location marker
      user_location_data <- data.frame(
        Latitude = loc$lat,
        Longitude = loc$lng,
        Name = "Your Location",
        colour = "blue"  # Blue color for user's location
      )
      
      # Combine data
      all_data <- rbind(clinic_data, user_location_data)
      
      # Render the Google Map
      output$map <- renderGoogle_map({
        google_map(key = api_key) %>%
          add_markers(
            data = all_data,
            lat = "Latitude",
            lon = "Longitude",
            colour = "colour",
            info_window = "Name"
          )
      })
      
      # Render distances in the side panel
      output$distances <- renderPrint({
        if (length(filtered_clinics) > 0) {
          purrr::walk(filtered_clinics, function(clinic) {
            cat(sprintf(
              "%s (%s): Phone = %s, Max GA = %d weeks\n",
              clinic$name, clinic$address, clinic$phone, clinic$max_GAw
            ))
          })
        } else {
          cat("No clinics found matching the criteria.\n")
        }
      })
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
