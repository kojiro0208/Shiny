## app.R ##
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(
                title = "Histogram", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot1", height = 250)
            ),
            
            box(
                title = "Inputs", status = "warning", solidHeader = TRUE,
                "Box content here", br(), "More box content",
                sliderInput("slider", "Slider input:", 1, 100, 50),
                textInput("text", "Text input:")
            )
        ),
        fluidRow(
            tabBox(
                title = "First tabBox",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", #height = "250px",
                tabPanel("Tab1", "First tab content"),
                tabPanel("Tab2", "Tab content 2"),
                tabPanel('たぶ3',
                         fluidRow(
                             box(
                                 title = "Histogram", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("plot2", height = 250)
                             ),
                             
                             box(
                                 title = "Inputs", status = "warning", solidHeader = TRUE,
                                 "Box content here", br(), "More box content",
                                 sliderInput("slider2", "Slider input:", 1, 100, 50),
                                 textInput("text", "Text input:")
                             )
                ))
            ),
            tabBox(
                side = "right", height = "250px",
                selected = "Tab3",
                tabPanel("Tab1", "Tab content 1"),
                tabPanel("Tab2", "Tab content 2"),
                tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
            )
        ),
        fluidRow(
            tabBox(
                # Title can include an icon
                title = tagList(shiny::icon("gear"), "tabBox status"),
                tabPanel("Tab1",
                         "Currently selected tab from first box:",
                         verbatimTextOutput("tabset1Selected")
                ),
                tabPanel("Tab2", "Tab content 2")
            )
        )
    )
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    output$plot2 <- renderPlot({
        data <- histdata[seq_len(input$slider2)]
        hist(data)
    })
}

shinyApp(ui, server)