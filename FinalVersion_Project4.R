#################################
#     PROJECT 4                 #
#Authors:Koko Hall, Camryn Cohen, and Kate Kurlandsky


library(shiny)
library(googleway)
library(httr)
library(jsonlite)
library(purrr)
library(R6)
library(geosphere)
library(DT)
library(googlePolylines)
library(shinyjs)
library(rsconnect)

# Polyline Decoding Function
# Helper Functions
decode_pl <- function(encoded) {
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

# Function to fetch route details
fetch_route_details <- function(origin_lat, origin_lng, dest_lat, dest_lng, api_key) {
  route_modes <- c("driving", "walking", "transit")
  route_results <- lapply(route_modes, function(mode) {
    route_url <- paste0(
      "https://maps.googleapis.com/maps/api/directions/json?",
      "origin=", URLencode(paste(origin_lat, origin_lng, sep = ",")),
      "&destination=", URLencode(paste(dest_lat, dest_lng, sep = ",")),
      "&mode=", mode,
      "&key=", api_key
    )
    
    response <- httr::GET(route_url)
    
    if (response$status_code != 200) {
      warning(paste("API Error:", response$status_code, response$reason))
      return(NULL)
    }
    
    route_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    
    if (route_data$status != "OK") {
      warning(paste("No route found for", mode))
      return(NULL)
    }
    
    list(
      distance = route_data$routes$legs[[1]]$distance$text,
      duration = route_data$routes$legs[[1]]$duration$text,
      polyline = route_data$routes$overview_polyline$points
    )
  })
  names(route_results) <- route_modes
  return(route_results)
}

# Create clinic function
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


#

create_state_law <- function(
    name,
    gestational_limit = NULL,
    exceptions = NULL,
    parental_consent = NULL,
    waiting_period_counseling = NULL,
    is_legal = TRUE
) {
  structure(
    list(
      name = name,
      gestational_limit = gestational_limit,
      exceptions = exceptions,
      parental_consent = parental_consent,
      waiting_period_counseling = waiting_period_counseling,
      is_legal = is_legal
    ),
    class = "state_law"
  )
}

# Create state laws data
state_laws <- list(
  create_state_law(
    name = "Virginia",
    gestational_limit = "26 weeks, 6 days from the first day of your last menstrual period",
    exceptions = "After 26 weeks, abortion is permitted to save the pregnant person's life and to preserve the pregnant person's general health (can include mental health)",
    parental_consent = "Yes; parents must be notified of decision 24 hours prior to abortion",
    waiting_period_counseling = "No"
  ),
  create_state_law(
    name = "Maryland",
    gestational_limit = "Legal throughout pregnancy",
    parental_consent = "No; however, parent or guardian must be notified before a minor has an abortion, exceptions allowed",
    waiting_period_counseling = "No"
  ),
  create_state_law(
    name = "North Carolina",
    gestational_limit = "12 weeks from the first day of last menstrual period",
    exceptions = "Abortion allowed after through week 20 in cases of rape or incest. Abortion for life-limiting fetal anomaly allowed through week 24. If the patient's life is in danger or there is a severe health risk, abortion allowed at any point",
    parental_consent = "Yes",
    waiting_period_counseling = "Yes; in-person counseling at least three days prior to abortion procedure and follow-up appointment for medication abortion patients 1-2 weeks after the abortion"
  ),
  create_state_law(
    name = "West Virginia",
    is_legal = FALSE
  ),
  create_state_law(
    name = "Washington DC",
    gestational_limit = "Legal throughout pregnancy",
    parental_consent = "No",
    waiting_period_counseling = "No"
  )
)

# Print method for state_law class
print.state_law <- function(x, ...) {
  cat("\n", x$name, "\n")
  
  if (!x$is_legal) {
    cat("\nAbortion is illegal in this state.\n")
    cat("You may be able to get an abortion in other states.\n")
    return(invisible(x))
  }
  
  cat("**Gestational Limit:** ", x$gestational_limit, "\n")
  
  if (!is.null(x$exceptions)) {
    cat("**Exceptions:** ", x$exceptions, "\n")
  }
  
  cat("**Parental Consent Required for Minors:** ", x$parental_consent, "\n")
  
  cat("**Waiting Period or Counseling:** ", x$waiting_period_counseling, "\n")
  
  invisible(x)
}


# create abortion funds object
abortion_funds <- list(
  "Washington DC" = list(
    list(
      name = "DC Abortion Fund",
      website = "https://dcabortionfund.org",
      phone = "202-452-7464",
      description = "Serves DC, Maryland, and Virginia. Offers financial assistance and case management support.",
      languages = "English, Spanish",
      hours = "Helpline hours: Monday-Friday 9am-5pm"
    )
  ),
  "Maryland" = list(
    list(
      name = "Baltimore Abortion Fund",
      website = "https://www.baltimoreabortionfund.org",
      phone = "443-297-9893",
      description = "Serves Maryland residents and those traveling to Maryland for care.",
      languages = "English, Spanish",
      hours = "Call or text helpline available 24/7"
    )
    ),
  "Virginia" = list(
    list(
      name = "Richmond Reproductive Freedom Project",
      website = "https://www.rrfp.net",
      phone = "1-888-847-1593",
      description = "Serves Virginia residents and those traveling to Virginia for care.",
      languages = "English, Spanish",
      hours = "24/7 helpline"
    ),
    list(
      name = "Blue Ridge Abortion Fund",
      website = "https://www.blueridgeabortionfund.org",
      phone = "434-963-0669",
      description = "Serves Western Virginia and surrounding areas.",
      languages = "English",
      hours = "Monday-Friday 9am-5pm"
    )
  ),
  "West Virginia" = list(
    list(
      name = "Holler Health Justice",
      website = "https://www.hollerhealthjustice.org",
      phone = " 1-833-465-5379",
      description = "Provides abortion funding for Appalachian residents."
    )
  ),
  "North Carolina" = list(
    list(
      name = "Carolina Abortion Fund",
      website = "https://www.carolinaabortionfund.org",
      phone = "855-518-4603",
      description = "Serves North Carolina residents and those traveling to NC for care.",
      languages = "English, Spanish",
      hours = "Helpline hours: Monday-Friday 9am-5pm"
    )
  )
)


Location <- R6Class(
  "Location",
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
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      #legend {
  position: absolute;
  top: 10px;
  left: 10px;
  background-color: white;
  padding: 10px;
  border: 1px solid #ccc;
  z-index: 1000;
  display: none;
}
.fund-card {
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
  background-color: #fff;
}
.fund-card h4 {
  color: #2c3e50;
  margin-top: 0;
}
.fund-card .contact-info {
  margin: 10px 0;
  padding: 5px 0;
  border-top: 1px solid #eee;
  border-bottom: 1px solid #eee;
}

.main-title {
  font-family: 'Futura';
  letter-spacing: 1px;
  font-weight: bold;
}

/* Button container styling */
.button-container {
  display: flex;
  gap: 10px;
  margin-top: 15px;
}

/* neutral background */
.tab-content {
  background-color: #f9f9f9;
  padding: 20px;
  border-radius: 0 0 4px 4px;
}
.tab-pane {
  background-color: #f9f9f9;
  padding: 15px;
  border-radius: 4px;
}
.well, .alert, .card {
  background-color: white;
  margin: 10px 0;
  padding: 15px;
  border-radius: 4px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
}
/* Style the sidebar */
.well {
  background-color: white;
}
      /* Style the sidebar */
      .well {
        background-color: white;
      }
    "))
  ),
  titlePanel(div(class = "main-title", "Abortion Clinic Finder")),
  
  tabsetPanel(
    # First tab - Find Nearest Clinic
    tabPanel("Find Nearest Clinic",
             sidebarLayout(
               sidebarPanel(
                 h4("Enter your location and gestational age:"),
                 textInput("address", "Street Address", ""),
                 textInput("city", "City", ""),
                 selectInput("state", 
                             "State", 
                             choices = c("Select a state" = "",
                                         "District of Columbia" = "Washington DC",
                                         "Maryland",
                                         "North Carolina",
                                         "Virginia",
                                         "West Virginia"),
                             selected = ""),
                 numericInput("zip", "Zip Code", value = NA, min = 10000, max = 99999),
                 numericInput("max_gaw", "Gestational Age (weeks):", value = NA, min = 0, max = 48),
                 # New button container with both buttons
                 div(class = "button-container",
                     actionButton("find", "Find Distance to Clinics"),
                     hidden(
                       actionButton("newSearch", "New Search", 
                                    onclick = "location.reload();")
                     )
                 )
               ),
               mainPanel(
                 tags$div(
                   id = "legend",
                   tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: #FF0000; margin-right: 5px;"), "Driving", tags$br(),
                   tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: #00FF00; margin-right: 5px;"), "Walking", tags$br(),
                   tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: #0000FF; margin-right: 5px;"), "Transit"
                 ),
                 google_mapOutput(outputId = "map"),
                 DTOutput("clinicTable"),
                 div(id = "route-details",
                     h4("Route Details"),
                     uiOutput("routeDetailsText")
                 )
               )
             )
    ),
    
    # Second tab - State Laws
    tabPanel("State Laws",
             fluidRow(
               column(12,
                      h3("Abortion Laws by State"),
                      selectInput("lawState", 
                                  "Select State", 
                                  choices = c("Select a state" = "",
                                              "District of Columbia" = "Washington DC",
                                              "Maryland",
                                              "North Carolina",
                                              "Virginia",
                                              "West Virginia"),
                                  selected = ""),
                      uiOutput("stateLawInfo")
               )
             )
    ),
    
    # Third tab - Abortion Funds
    tabPanel("Abortion Funds",
             fluidRow(
               column(12,
                      h3("Financial Assistance Resources"),
                      p("Select your state to see available abortion funds and financial assistance programs:"),
                      selectInput("fundState", 
                                  "Select State", 
                                  choices = c("Select a state" = "",
                                              "District of Columbia" = "Washington DC",
                                              "Maryland",
                                              "North Carolina",
                                              "Virginia",
                                              "West Virginia"),
                                  selected = ""),
                      uiOutput("fundInfo"),
                      hr(),
                      div(
                        h4("National Resources"),
                        div(class = "fund-card",
                            h4("National Abortion Federation Hotline"),
                            p("Provides referrals to quality providers and financial assistance."),
                            div(class = "contact-info",
                                p(icon("phone"), " 1-800-772-9100"),
                                p(icon("globe"), " ", a("www.prochoice.org", href = "https://prochoice.org", target = "_blank")),
                                p(icon("clock"), " Monday-Friday: 8am-7pm ET, Saturday-Sunday: 8am-4pm ET")
                            )
                        ),
                        div(class = "fund-card",
                            h4("Indigenous Women Rising"),
                            p("Abortion and Midwifery Fund for Indigenous communities."),
                            div(class = "contact-info",
                                p(icon("phone"), " 505-398-1990"),
                                p(icon("globe"), " ", a("www.iwrising.org", href = "https://www.iwrising.org", target = "_blank")),
                                p(icon("clock"), " Available 24/7")
                            )
                        )
                      )
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initialize API key
  Sys.setenv(GOOGLE_API="AIzaSyDxykvMTC74S71DCnLEZeqqf4ceU2k1Hwo")
  api_key <- Sys.getenv("GOOGLE_API")
  
  
  
  
  # Initialize reactive values
  rv <- reactiveValues(
    clinics = NULL,
    route_results = NULL,
    selected_clinic = NULL,
    initial_map_data = NULL,
    user_location = NULL
  )
  
  # Initial state - hide the New Search button
  observe({
    shinyjs::hide("newSearch")
  })
  
  # Initialize empty map
  output$map <- renderGoogle_map({
    google_map(key = api_key)
  })
  
  # Function to update route details text
  update_route_details <- function(clinic_name, route_results) {
    output$routeDetailsText <- renderUI({
      HTML(paste(
        "<h5>Routes to", clinic_name, "</h5>",
        "<h6>Driving Route:</h6>",
        ifelse(!is.null(route_results$driving),
               paste("Distance:", route_results$driving$distance,
                     "<br>Duration:", route_results$driving$duration),
               "No driving route available"),
        "<h6>Walking Route:</h6>",
        ifelse(!is.null(route_results$walking),
               paste("Distance:", route_results$walking$distance,
                     "<br>Duration:", route_results$walking$duration),
               "No walking route available"),
        "<h6>Transit Route:</h6>",
        ifelse(!is.null(route_results$transit),
               paste("Distance:", route_results$transit$distance,
                     "<br>Duration:", route_results$transit$duration),
               "No transit route available")
      ))
    })
  }
  
  # Handle Find button click
  observeEvent(input$find, {
    req(input$address)
    
    withProgress(message = 'Processing...', value = 0, {
      tryCatch({
        # First, completely reset the map
        output$map <- renderGoogle_map({
          google_map(key = api_key)
        })
        
        # Reset all reactive values
        rv$clinics <- NULL
        rv$route_results <- NULL
        rv$selected_clinic <- NULL
        rv$initial_map_data <- NULL
        rv$user_location <- NULL
        
        incProgress(0.2)
        
        # Show the legend and New Search button
        shinyjs::show("legend")
        shinyjs::show("newSearch")
        
        # Create location
        loc <- Location$new(
          paste(input$address, input$city, input$state, input$zip, sep = ", "), 
          api_key
        )
        loc$fetch_coordinates()
        
        # Store user location
        rv$user_location <- list(lat = loc$lat, lng = loc$lng)
        
        incProgress(0.4)
        
        # Filter clinics based on gestational age
        max_gaw <- ifelse(is.na(input$max_gaw), Inf, as.numeric(input$max_gaw))
        filtered_clinics <- keep(clinics, ~ .x$max_GAw >= max_gaw)
        
        # Calculate distances
        distances <- map_dbl(filtered_clinics, ~ {
          distm(
            c(loc$lng, loc$lat),
            c(.x$long, .x$lat),
            fun = distHaversine
          ) / 1609.344
        })
        
        incProgress(0.6)
        
        # Sort clinics by distance
        sorted_indices <- order(distances)
        sorted_clinics <- filtered_clinics[sorted_indices]
        sorted_distances <- distances[sorted_indices]
        
        # Store clinics in reactive values
        rv$clinics <- sorted_clinics
        
        # Prepare clinic data for table
        clinic_data <- data.frame(
          Name = map_chr(sorted_clinics, ~ {
            if (!is.null(.x$website) && .x$website != "") {
              paste0('<a href="', .x$website, '" target="_blank">', .x$name, '</a>')
            } else {
              .x$name
            }
          }),
          Address = map_chr(sorted_clinics, "address"),
          Distance = map_chr(sorted_distances, ~ paste(round(.x, 1), "miles")),
          Phone = map_chr(sorted_clinics, "phone"),
          MaxGA = map_chr(sorted_clinics, ~ as.character(.x$max_GAw)),
          Directions = paste0(
            '<button class="btn btn-link" onclick="Shiny.setInputValue(\'select_clinic\', \'',
            map_chr(sorted_clinics, function(clinic) {
              clinic$name
            }), 
            '\')">Show map distance</button>'
          ),
          stringsAsFactors = FALSE
        )
        
        incProgress(0.8)
        
        # Render clinic table
        output$clinicTable <- renderDT({
          datatable(clinic_data, 
                    escape = FALSE,
                    selection = "single",
                    options = list(
                      pageLength = 10,
                      columnDefs = list(
                        list(
                          targets = 5,  # Directions column index (0-based)
                          className = 'dt-center'
                        )
                      )
                    )
          )
        })
        
        # Prepare and store map data
        rv$initial_map_data <- data.frame(
          Latitude = c(map_dbl(sorted_clinics, ~ .x[["lat"]]), loc$lat),
          Longitude = c(map_dbl(sorted_clinics, ~ .x[["long"]]), loc$lng),
          Name = c(map_chr(sorted_clinics, ~ .x[["name"]]), "Your Location"),
          colour = c(rep("red", length(sorted_clinics)), "blue"),
          stringsAsFactors = FALSE
        )
        
        # Create a fresh map centered on the search location
        isolate({
          output$map <- renderGoogle_map({
            google_map(key = api_key, location = c(loc$lat, loc$lng), zoom = 10) %>%
              add_markers(
                data = rv$initial_map_data,
                lat = "Latitude", 
                lon = "Longitude",
                colour = "colour",
                title = "Name",
                info_window = "Name",
                id = "Name"
              )
          })
        })
        
        incProgress(1.0)
        
        # Clear route details
        output$routeDetailsText <- renderUI({
          return(NULL)
        })
        
      }, error = function(e) {
        showNotification(
          paste("Error processing your request:", as.character(e)), 
          type = "error",
          duration = 10
        )
      })
    })
  })
  
  
  # Handle marker clicks
  observeEvent(input$google_map_marker_click, {
    click <- input$google_map_marker_click
    print(paste("Clicked marker:", click$id))  # Debug print
    req(rv$clinics, rv$user_location, click)
    
    # Skip if user location marker is clicked
    if (click$id == "Your Location") return()
    
    # Find the clicked clinic
    clicked_clinic <- Find(function(clinic) {
      clean_clinic_name <- gsub("[^[:alnum:]]", "", tolower(clinic$name))
      clean_click_id <- gsub("[^[:alnum:]]", "", tolower(click$id))
      clean_clinic_name == clean_click_id
    }, rv$clinics)
    
    if (!is.null(clicked_clinic)) {
      print(paste("Found clinic:", clicked_clinic$name))  # Debug print
      # Update selected clinic
      rv$selected_clinic <- clicked_clinic
      
      # Fetch new route details
      rv$route_results <- fetch_route_details(
        rv$user_location$lat,
        rv$user_location$lng,
        clicked_clinic$lat,
        clicked_clinic$long,
        api_key
      )
      
      # Clear existing map and add markers
      google_map_update(map_id = "map") %>%
        clear_markers() %>%
        clear_polylines()
      
      google_map_update(map_id = "map") %>%
        add_markers(
          data = rv$initial_map_data,
          lat = "Latitude", 
          lon = "Longitude",
          colour = "colour",
          title = "Name",
          info_window = "Name",
          id = "Name"
        )
      
      # Clear existing routes first
      google_map_update(map_id = "map") %>%
        clear_polylines()
      
      # Add new routes
      isolate({
        if (!is.null(rv$route_results$driving)) {
          coords <- decode_pl(rv$route_results$driving$polyline)
          if (nrow(coords) > 0) {
            google_map_update(map_id = "map") %>%
              add_polylines(
                data = data.frame(coords, id = 1),
                lat = "lat",
                lon = "lon",
                id = "id",
                stroke_colour = "#FF0000",
                stroke_weight = 3
              )
          }
        }
        
        if (!is.null(rv$route_results$walking)) {
          coords <- decode_pl(rv$route_results$walking$polyline)
          if (nrow(coords) > 0) {
            google_map_update(map_id = "map") %>%
              add_polylines(
                data = data.frame(coords, id = 2),
                lat = "lat",
                lon = "lon",
                id = "id",
                stroke_colour = "#00FF00",
                stroke_weight = 3
              )
          }
        }
        
        if (!is.null(rv$route_results$transit)) {
          coords <- decode_pl(rv$route_results$transit$polyline)
          if (nrow(coords) > 0) {
            google_map_update(map_id = "map") %>%
              add_polylines(
                data = data.frame(coords, id = 3),
                lat = "lat",
                lon = "lon",
                id = "id",
                stroke_colour = "#0000FF",
                stroke_weight = 3
              )
          }
        }
      })
      
      # Update route details
      update_route_details(clicked_clinic$name, rv$route_results)
      
      # Update table selection
      clinic_index <- match(clicked_clinic$name, map_chr(rv$clinics, "name"))
      if (!is.na(clinic_index)) {
        dataTableProxy("clinicTable") %>% selectRows(clinic_index)
      }
      
      # Hide instructions
      shinyjs::hide("instructions")
    } else {
      print("No matching clinic found")  # Debug print
    }
  })
  
  # Handle table row selection
  observeEvent(input$clinicTable_rows_selected, {
    req(rv$clinics, rv$user_location)
    
    selected_clinic <- rv$clinics[[input$clinicTable_rows_selected]]
    rv$selected_clinic <- selected_clinic
    
    # Fetch new route details
    rv$route_results <- fetch_route_details(
      rv$user_location$lat,
      rv$user_location$lng,
      selected_clinic$lat,
      selected_clinic$long,
      api_key
    )
    
    # Clear existing map and add markers
    google_map_update(map_id = "map") %>%
      clear_markers() %>%
      clear_polylines()
    
    google_map_update(map_id = "map") %>%
      add_markers(
        data = rv$initial_map_data,
        lat = "Latitude", 
        lon = "Longitude",
        colour = "colour",
        title = "Name",
        info_window = "Name",
        id = "Name"
      )
    
    # Clear existing routes first
    google_map_update(map_id = "map") %>%
      clear_polylines()
    
    # Add new routes
    isolate({
      if (!is.null(rv$route_results$driving)) {
        coords <- decode_pl(rv$route_results$driving$polyline)
        if (nrow(coords) > 0) {
          google_map_update(map_id = "map") %>%
            add_polylines(
              data = data.frame(coords, id = 1),
              lat = "lat",
              lon = "lon",
              id = "id",
              stroke_colour = "#FF0000",
              stroke_weight = 3
            )
        }
      }
      
      if (!is.null(rv$route_results$walking)) {
        coords <- decode_pl(rv$route_results$walking$polyline)
        if (nrow(coords) > 0) {
          google_map_update(map_id = "map") %>%
            add_polylines(
              data = data.frame(coords, id = 2),
              lat = "lat",
              lon = "lon",
              id = "id",
              stroke_colour = "#00FF00",
              stroke_weight = 3
            )
        }
      }
      
      if (!is.null(rv$route_results$transit)) {
        coords <- decode_pl(rv$route_results$transit$polyline)
        if (nrow(coords) > 0) {
          google_map_update(map_id = "map") %>%
            add_polylines(
              data = data.frame(coords, id = 3),
              lat = "lat",
              lon = "lon",
              id = "id",
              stroke_colour = "#0000FF",
              stroke_weight = 3
            )
        }
      }
    })
    
    # Update route details
    update_route_details(selected_clinic$name, rv$route_results)
    
    # Hide instructions
    shinyjs::hide("instructions")
  })
  
  # Handle directions link clicks
  observeEvent(input$select_clinic, {
    req(rv$clinics, rv$user_location)
    print(paste("Direction click for:", input$select_clinic))  # Debug print
    
    # Find the selected clinic with more flexible matching
    selected_clinic <- Find(function(clinic) {
      clean_clinic_name <- gsub("[^[:alnum:]]", "", tolower(clinic$name))
      clean_selected_name <- gsub("[^[:alnum:]]", "", tolower(input$select_clinic))
      print(paste("Comparing:", clean_clinic_name, "with:", clean_selected_name))  # Debug print
      clean_clinic_name == clean_selected_name
    }, rv$clinics)
    
    if (!is.null(selected_clinic)) {
      print(paste("Found clinic:", selected_clinic$name))  # Debug print
      # Update selected clinic
      rv$selected_clinic <- selected_clinic
      
      # Fetch new route details
      rv$route_results <- fetch_route_details(
        rv$user_location$lat,
        rv$user_location$lng,
        selected_clinic$lat,
        selected_clinic$long,
        api_key
      )
      
      # Clear existing map and add markers
      google_map_update(map_id = "map") %>%
        clear_markers() %>%
        clear_polylines()
      
      google_map_update(map_id = "map") %>%
        add_markers(
          data = rv$initial_map_data,
          lat = "Latitude", 
          lon = "Longitude",
          colour = "colour",
          title = "Name",
          info_window = "Name",
          id = "Name"
        )
      
      # Clear existing routes first
      google_map_update(map_id = "map") %>%
        clear_polylines()
      
      # Add new routes
      isolate({
        if (!is.null(rv$route_results$driving)) {
          coords <- decode_pl(rv$route_results$driving$polyline)
          if (nrow(coords) > 0) {
            google_map_update(map_id = "map") %>%
              add_polylines(
                data = data.frame(coords, id = 1),
                lat = "lat",
                lon = "lon",
                id = "id",
                stroke_colour = "#FF0000",
                stroke_weight = 3
              )
          }
        }
        
        if (!is.null(rv$route_results$walking)) {
          coords <- decode_pl(rv$route_results$walking$polyline)
          if (nrow(coords) > 0) {
            google_map_update(map_id = "map") %>%
              add_polylines(
                data = data.frame(coords, id = 2),
                lat = "lat",
                lon = "lon",
                id = "id",
                stroke_colour = "#00FF00",
                stroke_weight = 3
              )
          }
        }
        
        if (!is.null(rv$route_results$transit)) {
          coords <- decode_pl(rv$route_results$transit$polyline)
          if (nrow(coords) > 0) {
            google_map_update(map_id = "map") %>%
              add_polylines(
                data = data.frame(coords, id = 3),
                lat = "lat",
                lon = "lon",
                id = "id",
                stroke_colour = "#0000FF",
                stroke_weight = 3
              )
          }
        }
      })
      # Update route details
      update_route_details(selected_clinic$name, rv$route_results)
      
      # Update table selection
      clinic_index <- match(selected_clinic$name, map_chr(rv$clinics, "name"))
      if (!is.na(clinic_index)) {
        dataTableProxy("clinicTable") %>% selectRows(clinic_index)
      }
      
      # Hide instructions
      shinyjs::hide("instructions")
    } else {
      print("No matching clinic found for directions click")  # Debug print
    }
  })
  
  # Add new reactive expression for state law information
  output$stateLawInfo <- renderUI({
    req(input$lawState)
    
    # Find the selected state law from our list
    selected_law <- Find(function(law) law$name == input$lawState, state_laws)
    
    if (is.null(selected_law)) {
      return(h4("No information available for this state"))
    }
    
    # Create HTML content for state laws
    if (!selected_law$is_legal) {
      HTML("<div class='alert alert-warning'>Abortion is illegal in this state.<br>You may be able to get an abortion in other states.</div>")
    } else {
      HTML(paste0(
        "<div class='well'>",
        "<p><strong>Gestational Limit:</strong> ", selected_law$gestational_limit, "</p>",
        if (!is.null(selected_law$exceptions)) 
          paste0("<p><strong>Exceptions:</strong> ", selected_law$exceptions, "</p>") else "",
        "<p><strong>Parental Consent Required for Minors:</strong> ", selected_law$parental_consent, "</p>",
        "<p><strong>Waiting Period or Counseling:</strong> ", selected_law$waiting_period_counseling, "</p>",
        "</div>"
      ))
    }
  })
  
  
  output$fundInfo <- renderUI({
    req(input$fundState)
    
    funds <- abortion_funds[[input$fundState]]
    
    if (is.null(funds)) {
      return(
        div(class = "alert alert-info",
            "No local abortion funds found for this state. Please see the national resources below or contact funds in neighboring states."
        )
      )
    }
    
    # Create HTML content for each fund
    fund_cards <- lapply(funds, function(fund) {
      div(class = "fund-card",
          h4(fund$name),
          p(fund$description),
          div(class = "contact-info",
              if (!is.null(fund$phone)) p(icon("phone"), " ", fund$phone),
              if (!is.null(fund$website)) p(icon("globe"), " ", 
                                            a(fund$website, href = fund$website, target = "_blank")),
              if (!is.null(fund$hours)) p(icon("clock"), " ", fund$hours),
              if (!is.null(fund$languages)) p(icon("language"), 
                                              " Available languages: ", fund$languages)
          )
      )
    })
    
    # Return all fund cards wrapped in a div
    do.call(tagList, fund_cards)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


