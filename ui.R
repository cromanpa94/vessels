
header <- dashboardHeader(title = "Vessel movements",
                          titleWidth = 200)

body <- dashboardBody(
  add_busy_spinner(spin = "dots",
                   timeout = 10,
                   height = "25px",
                   width = "25px"),
  shinyDashboardThemes(
    theme = "grey_light"
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  fluidRow(
    column(width = 4,
           box(width = NULL,title = tagList(shiny::icon("info-circle",class = 'fa-lg'), "About app"), solidHeader = T, collapsible = T, status = 'primary',
               strong("Vessel movements"),"is an interactive map built on shiny which allows you to examine basic patterns of vessel movements in the ", a('marine data set', href = 'https://drive.google.com/file/d/1IeaDpJNqfgUZzGdQmR6cz2H3EQ3_QfCV/view?usp=sharing', target = "_blank"), "To use the app, please select a vessel type from the first dropdown menu. Next, choose one of the vessels. For a given vessel, the app will display the origin and ending points of the longest trip."
           ),
           box(width = NULL, title = tagList(shiny::icon("filter",class = 'fa-lg'), "Filter Data") ,
               solidHeader = T, collapsible = T, status = 'primary',
               uiOutput("vt_selected"),
               uiOutput("vessels_selected")
           ),
           box(width = NULL, title = tagList(shiny::icon("gear",class = 'fa-lg'), "Configure") ,
               solidHeader = T, collapsible = T, status = 'primary',
               "Please select one of the three methods below. Euclidean and Haversine indexes 
               are faster than the one based on Rasters. Resolution (default is 0.001 degrees) 
                could compromise the performance of the latter method.",
               selectInput("method", "Method", choices = c("Euclidean", "Haversine", "Rasters"))
           ),
           box(width = NULL, title = tagList(shiny::icon("ruler",class = 'fa-lg'), "Information") ,
               solidHeader = T, collapsible = F, status = 'primary',
               textOutput("distance")
           ),
           box(width = NULL, title = tagList(shiny::icon("laptop-code",class = 'fa-lg'), "Code") ,
               solidHeader = T, collapsible = T, status = 'primary',
               a(icon("github",class = 'fa-lg'), ' GitHub repository', href = 'https://github.com/cromanpa94/vessels.app', target = "_blank"))
    ),
    column(width = 8,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput('Map',height = 700))
    )
  )
)

ui <- dashboardPage(skin = 'black',
                    header,
                    dashboardSidebar(disable = T),
                    body
)