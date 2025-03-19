library(leaflet)  
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)   # For pivot_longer()

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$img(src = "images/logo1.jpg", style = "height: 110%; width: 120%; margin-left: -20px;")
  ),  
  
  dashboardSidebar(
    width = 250,
    
    # Second logo
    tags$div(
      style = "text-align:center; padding:10px;",
      tags$img(src = "images/logo2.jpg", width = "85%")
    ),
    
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("address-card")),
      menuItem("Dashboard", icon = icon("dashboard"),
               menuSubItem("Environmental", tabName = "dashboard1", icon = icon("leaf")),
               menuSubItem("Economic", tabName = "dashboard2", icon = icon("dollar-sign")),
               menuSubItem("Social", tabName = "dashboard3", icon = icon("users"))
      ),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs")), 
      menuItem("Contact/Help", tabName = "contact", icon = icon("envelope"))
    )
  ),
  
  dashboardBody(    useShinyjs(),
                    
                    # Add CSS for both light and dark themes
                    tags$head(
                      tags$style(HTML("
        /* Light Theme (Default) */
        body.light-theme {
            background-color: #f5f5f5;
            color: #333;
        }
        
        body.light-theme .content-wrapper,
        body.light-theme .right-side {
            background-color: #f5f5f5;
        }
        
        body.light-theme .box {
            background-color: white;
            color: #333;
        }
        
        body.light-theme .box-header {
            color: #333;
        }
        
        /* Dark Theme */
        body.dark-theme {
            background-color: #222;
            color: #eee;
        }
        
        body.dark-theme .content-wrapper,
        body.dark-theme .right-side {
            background-color: #333;
        }
        
        body.dark-theme .box {
            background-color: #444;
            color: #eee;
            border: 1px solid #555;
        }
        
        body.dark-theme .box-header {
            color: #eee;
        }
        
        body.dark-theme h1, 
        body.dark-theme h2, 
        body.dark-theme h3, 
        body.dark-theme h4, 
        body.dark-theme h5, 
        body.dark-theme h6, 
        body.dark-theme p, 
        body.dark-theme label {
            color: #eee;
        }
        
        body.dark-theme .form-control {
            background-color: #555;
            color: #eee;
            border: 1px solid #666;
        }
        
        body.dark-theme .selectize-input {
            background-color: #555;
            color: #eee;
            border: 1px solid #666;
        }
        
        body.dark-theme .selectize-dropdown {
            background-color: #555;
            color: #eee;
        }
        
        body.dark-theme .selectize-dropdown-content .option {
            background-color: #555;
            color: #eee;
        }
        
        body.dark-theme .selectize-dropdown-content .option.active {
            background-color: #777;
        }
        
        body.dark-theme .irs--shiny .irs-grid-text {
            color: #ccc;
        }
        
        body.dark-theme .irs--shiny .irs-grid-pol {
            background-color: #777;
        }
        
        body.dark-theme .irs--shiny .irs-line {
            background-color: #555;
            border-color: #666;
        }
      "))
                    ),
                    
                    tabItems(
                      # Dashboard Page 1 (Environmental)
                      tabItem(
                        tabName = "about",
                        h2("ABOUT Digital Health in Circular Economy (DiCE)"),
                        p("EU funded project aiming to address the issue of increasing digital health waste."),
                        p('Electronic waste (e-waste) from digital health devices is a complex and growing problem requiring a holistic solution. E-waste from healthcare products may pose biological or chemical contamination, leading to its incineration, with or without energy recovery. This means that all items are destroyed.

DiCE was created to bring key stakeholders together to address challenges associated with the growing use of digital healthcare products and increasing demand for raw materials to manufacture new electronic devices and other equipment.')
                      ),
                      # Dashboard Page 1 (Environmental)
                      tabItem(
                        tabName = "dashboard1",
                        h2("Environmental Overview"),
                        actionButton("btn1", "Environmental", class = "btn btn-custom btn-active"),
                        actionButton("btn2", "Economic", class = "btn btn-custom"),
                        actionButton("btn3", "Social", class = "btn btn-custom")
                      ),
                      
                      # Dashboard Page 2 (Economic)
                      tabItem(
                        tabName = "dashboard2",
                        h2("Economic Overview"),
                        actionButton("btn1", "Environmental", class = "btn btn-custom"),
                        actionButton("btn2", "Economic", class = "btn btn-custom btn-active"),
                        actionButton("btn3", "Social", class = "btn btn-custom")
                      ),
                      
                      # Dashboard Page 3 (Social)
                      tabItem(
                        tabName = "dashboard3",
                        h2("Social Overview"),
                        actionButton("btn1", "Environmental", class = "btn btn-custom"),
                        actionButton("btn2", "Economic", class = "btn btn-custom"),
                        actionButton("btn3", "Social", class = "btn btn-custom btn-active")
                      ),
                      
                      # Analytics Page
                      tabItem(
                        tabName = "analytics",
                        h2("Analytics Overview"),
                        fluidRow(
                          box(
                            title = "Device Distribution by Country (Map)",
                            width = 6,
                            leafletOutput("deviceMap", height = 400)
                          ),
                          box(
                            title = "Device Type Comparison by Country (Bar Chart)",
                            width = 6,
                            plotlyOutput("            deviceBarChart", height = 400)
                          )
                        )
                      ),
                      
                      # Settings Page
                      tabItem(
                        tabName = "settings",
                        h2("Settings"),
                        # User Preferences
                        box(title = "User  Preferences", width = 6, status = "primary", solidHeader = TRUE,
                            selectInput("theme", "Select Theme:", choices = c("Light", "Dark")),
                            sliderInput("fontsize", "Font Size:", min = 10, max = 24, value = 14)
                        ),
                        
                        # Action Buttons
                        fluidRow(
                          column(6, actionButton("save_settings", "Save Settings", class = "btn btn-success")),
                          column(6, actionButton("reset_settings", "Reset to Default", class = "btn btn-danger"))
                        )
                      ),
                      
                      # Add the Contact Tab
                      tabItem(
                        tabName = "contact",
                        h2("Contact/Help"),
                        p("This is the contact/help page. Please reach out to us for any queries.")
                      )
                    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  
  # Set default theme to Light
  shinyjs::addClass(selector = "body", class = "light-theme")
  
  # Observe btn1 (Environmental) click: Switch to Dashboard 1 and make btn1 active
  observeEvent(input$btn1, {
    updateTabItems(session, "tabs", "dashboard1")
  })
  
  # Observe btn2 (Economic) click: Switch to Dashboard 2 and make btn2 active
  observeEvent(input$btn2, {
    updateTabItems(session, "tabs", "dashboard2")
  })
  
  # Observe btn3 (Social) click: Switch to Dashboard 3 and make btn3 active
  observeEvent(input$btn3, {
    updateTabItems(session, "tabs", "dashboard3")
  })
  
  # Observe btn_contact click: Switch to Contact tab
  observeEvent(input$btn_contact, {
    updateTabItems(session, "tabs", "contact")
  })
  
  # Default values for settings
  default_settings <- reactiveValues(
    theme = "Light",
    fontsize = 14
  )
  
  # Observe Save Button Click
  observeEvent(input$save_settings, {
    showNotification("Settings Saved!", type = "message")
    
    # Store user preferences
    default_settings$theme <- input$theme
    default_settings$fontsize <- input$fontsize
    
    # Apply Theme
    if (input$theme == "Dark") {
      shinyjs::addClass(selector = "body", class = "dark-theme")
      shinyjs::removeClass(selector = "body", class = "light-theme")
    } else {
      shinyjs::addClass(selector = "body", class = "light-theme")
      shinyjs::removeClass(selector = "body", class = "dark-theme")
    }
    
    # Apply Font Size
    font_size_css <- paste0(".dynamic-font { font-size: ", input$fontsize, "px !important; }")
    shinyjs::runjs(paste0("var style = document.createElement('style'); style.innerHTML = '", font_size_css, "'; document.head.appendChild(style);"))
  })
  
  # Observe Reset Button Click
  observeEvent(input$reset_settings, {
    showNotification("Settings Reset!", type = "warning")
    
    # Reset all inputs to default values
    updateSelectInput(session, "theme", selected = default_settings$theme)
    updateSliderInput(session, "fontsize", value = default_settings$fontsize)
    
    # Reset theme to light
    shinyjs::addClass(selector = "body", class = "light-theme")
    shinyjs::removeClass(selector = "body", class = "dark-theme")
    
    # Reset font size to default
    shinyjs::runjs("var style = document.createElement('style'); style.innerHTML = '.dynamic-font { font-size: 14px !important; }'; document.head.appendChild(style);")
  })
  
  # Example dataset: Amount of devices used in different countries
  device_data <- data.frame(
    country = c("Germany", "France", "Italy", "Spain", "UK"),
    lat = c(51.1657, 48.8566, 41.8719, 40.4637, 55.3781),  # Latitude of countries
    lon = c(10.4515, 2.3522, 12.5674, -3.7492, -3.4360),   # Longitude of countries
    single_use = c(5000, 4200, 3800, 3000, 4500),
    reprocessed = c(1200, 1500, 1000, 800, 1300),
    multi_use = c(800, 900, 1100, 1200, 950)
  )
  
  # Render Leaflet Map
  output$deviceMap <- renderLeaflet({
    leaflet(device_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat,
        radius = ~single_use / 1000 + 5,  # Size of the marker based on single-use count
        color = "red",
        label = ~paste(country, "<br>Single-use:", single_use, "<br>Reprocessed:", reprocessed, "<br>Multi-use:", multi_use),
        fillOpacity = 0.6
      )
  })
  
  # Render Bar Chart using Plotly
  output$deviceBarChart <- renderPlotly({
    device_long <- device_data %>%
      tidyr::pivot_longer(cols = c("single_use", "reprocessed", "multi_use"), names_to = "device_type", values_to = "count")
    
    p <- ggplot(device_long, aes(x = country, y = count, fill = device_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Device Type Comparison by Country", x = "Country", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui, server)
