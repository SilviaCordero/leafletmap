#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("helpersleaflet.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    navbarPage("Per capita GDP",
               tabPanel( "Map",
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput(inputId = "year",
                                             label = "Year:",
                                             min = 1960,
                                             max = 2019,
                                             value = 1960,
                                             sep = ""),
                                 selectInput(inputId = "region",
                                             label = "Region",
                                             choices = sort(unique(continent$continent)),
                                             selected = "World"),
                                 selectInput(inputId = "colorpal",
                                             label = "Choose color",
                                             choices = list("Sequential" = list("Blues", "Greens",
                                                                             "Oranges"),
                                                            "Diverging" = list("RdBu"),
                                                            "Categorical" = list("Set2")))
                             ),

                             mainPanel(
                                 fluidRow(align = "center", h4(textOutput("mapTitle"))),
                                 leafletOutput("map")
                             )
                         )

                ),
               tabPanel( "Timeseries",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput('countries', 'Select Country',
                                             choices = world_rob %>% pull(SOVEREIGNT) %>%
                                                 unique() %>% sort(),
                                             selected = "Switzerland")
                             ),

                             mainPanel(
                                 fluidRow(align = "center", h4(textOutput("timeseriesTitle"))),
                                 plotOutput("timeseries")
                             )
                         )

               )



    )

)




# Define server logic
server <- function(input, output) {

    output$"mapTitle" <- renderText({
        paste0("Per Capita Gross Domestic Product (", input$year, ")")
    })

    output$"timeseriesTitle" <- renderText({
        paste0("Per Capita Gross Domestic Product in ", input$countries)
    })

    gdp_year <- reactive({
        world_rob %>%
            filter(Year == input$year)
    })

    gdp_country <- reactive({
        world_rob %>%
            filter(SOVEREIGNT %in% input$countries)
    })
# observe(print(gdp_year()))

    output$map <- renderLeaflet({


        map_zoomed(data1 = gdp_year(),
                   fill_var = "quantiles",
                   input_var = input$region,
                   colpal = input$colorpal)

    })

    output$timeseries <- renderPlot({

       plot_timeseries(data1 = world_rob,
                       data2 = gdp_country(),
                       x_var = "Year",
                       y_var = "pcGDP",
                       group_var = "SOVEREIGNT")

    })

}

# Run the application
shinyApp(ui = ui, server = server)
