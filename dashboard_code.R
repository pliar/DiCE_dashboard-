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
      # Parent menu item: Dashboard with subItems
      menuItem("Dashboard", icon = icon("dashboard"),
               menuSubItem("Environmental", tabName = "dashboard1", icon = icon("leaf")),
               menuSubItem("Economic", tabName = "dashboard2", icon = icon("dollar-sign")),
               menuSubItem("Social", tabName = "dashboard3", icon = icon("users"))
      ),
      # Other menu items
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs")), 
      menuItem("Contact/Help", tabName = "contact", icon = icon("envelope"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        /* Turn Arrow 270 degrees */
        .skin-blue .sidebar-menu > li.treeview > a .fa-angle-left {
            transform: rotate(270deg);
        }
        
        /* Change the hover style for a specific menuItem */
        .main-sidebar .sidebar .sidebar-menu a:hover { 
            background-color: #FF2953 !important; 
            color: white !important; 
            border-left-color: #77F0CC; 
        }
        
        /* Make sure the parent menu gets highlighted when a sub-menu is active */ 
        .skin-blue .sidebar-menu > li > ul > li.active > a { 
            background-color: #2B2B65 !important; 
            color: white !important; 
        }
        
        .main-sidebar { 
            background-color: #c2c2d6 !important; 
        }
        
        .main-sidebar .sidebar .sidebar-menu a { 
            color: #2B2B65 !important; 
            font-weight: bold; 
        }
        
        .skin-blue .sidebar-menu > li > ul { 
            background-color: #CBCBD6 !important; 
            width: 100% !important; 
        }
        
        /* Prevent hover effects on active menu items */ 
        .skin-blue .sidebar-menu > li.active > a:hover, 
        .skin-blue .sidebar-menu > li.menu-open > a:hover { 
            background-color: #2B2B65 !important; 
            color: white !important; 
            cursor: default !important; 
            border-left-color: #77F0CC !important; 
        }
        
        .skin-blue .sidebar-menu > li.active > a { 
            background-color: #2B2B65 !important; 
            color: white !important; 
        }
        
        /* Ensure About is not dark when selected */ 
        .skin-blue .sidebar-menu > li.active.about > a { 
            background-color: transparent !important; 
            color: #2B2B65 !important; 
        }
        
        /* Ensure Dashboard is only dark when a sub-item is active */ 
        .skin-blue .sidebar-menu > li.treeview.active > a { 
            background-color: #2B2B65 !important; 
            color: white !important; 
        }
        
        /* Ensure Settings is not dark when selected */ 
        .skin-blue .sidebar-menu > li.settings.active > a { 
            background-color: transparent !important; 
            color: #2B2B65 !important; 
        }
        
        .navbar { 
            background-color: #2B2B65 !important; 
        }
        
        .btn-custom { 
            background-color: #CBCBD6 !important; 
            color: black !important; 
            border: none; 
        }
        
        .btn:hover { 
            background-color: #FF2953 !important; 
        }
        
        .btn-active { 
            pointer-events: none; 
            background-color: #2B2B65 !important; 
            color: white !important; 
            outline: none !important; 
        }
        
        /* Change the color of the parent menu item when hovering the sub-menu */ 
        .skin-blue .sidebar-menu > li.treeview:hover > a { 
            background-color: #FF2953 !important; 
            color: white !important; 
            border-left-color: #77F0CC; 
        }
        
        .btn-success { 
            background-color: #28a745 !important; 
            border-color: #28a745 !important; 
        }
        
        .btn-danger { 
            background-color: #dc3545 !important; 
            border-color: #dc3545 !important; 
        }
        
        .box { 
            border-radius: 10px !important; 
        }
        
        .shiny-notification { 
            position: fixed; 
            top: 60px; 
            right: 20px; 
            background-color: #2B2B65; 
            color: white; 
            padding: 10px; 
            border-radius: 5px; 
        }
        
        .box.box-solid.box-primary > .box-header { 
            background: #2B2B65; 
            background-color: #2B2B65; 
            border-radius: 5px 5px 0px 0px !important; 
        }
        
        .box.box-solid.box-primary { 
            border: 1px solid #2B2B65; 
            border-radius: 5px !important; 
        }
        
        .irs--shiny .irs-bar { 
            border-top: 1px solid #2B2B65; 
            border-bottom: 1px solid #2B2B65; 
            background: #2B2B65; 
        }
        
        .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { 
            color: #fff; 
            background-color: #2B2B65; 
        }
        
        input[type=checkbox], input[type=radio] { 
            background-color: #2B2B65; 
        }
        
        .skin-blue .main-header .navbar .sidebar-toggle:hover { 
            background-color: #FF2953; 
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
            plotlyOutput("deviceBarChart", height = 400)
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
  
  # Default values for settings
  default_settings <- reactiveValues(
    theme = "Light",
    fontsize = 14,
    sidebar_pos = "Left",
    email_notif = TRUE,
    push_notif = FALSE,
    username = "",
    password = ""
  )
  
  # Observe Save Button Click
  observeEvent(input$save_settings, {
    showNotification("Settings Saved!", type = "message")
    
    # Store user preferences (can be extended to store in a database)
    default_settings$theme <- input$theme
    default_settings$fontsize <- input$fontsize
    default_settings$sidebar_pos <- input$sidebar_pos
    default_settings$email_notif <- input$email_notif
    default_settings$push_notif <- input$push_notif
    default_settings$username <- input$username
    default_settings$password <- input$password
  })
  
  # Observe Reset Button Click
  observeEvent(input$reset_settings, {
    showNotification("Settings Reset!", type = "warning")
    
    # Reset all inputs to default values
    updateSelectInput(session, "theme", selected = default_settings$theme)
    updateSliderInput(session, "fontsize", value = default_settings$fontsize)
    updateRadioButtons(session, "sidebar_pos", selected = default_settings$sidebar_pos)
    updateCheckboxInput(session, "email_notif", value = default_settings$email_notif)
    updateCheckboxInput(session, "push_notif", value = default_settings$push_notif)
    updateTextInput(session, "username", value = default_settings$username)
    updateTextInput(session, "password", value = "")
  })
  
  # Function to apply settings dynamically
  observeEvent(input$save_settings, {
    showNotification("Settings Saved!", type = "message")
    
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
  
  # Reset settings to default values
  observeEvent(input$reset_settings, {
    showNotification("Settings Reset!", type = "warning")
    
    updateSelectInput(session, "theme", selected = "Light")
    updateSliderInput(session, "fontsize", value = 14)
    
    # Reset theme
    shinyjs::addClass(selector = "body", class = "light-theme")
    shinyjs::removeClass(selector = "body", class = "dark-theme")
    
    # Reset font size
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
