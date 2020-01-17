#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("Ping Pong Hydrograph Code.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    sidebarLayout(
        sidebarPanel(
            h2("Inputs"),
            
            numericInput("n",
                         h4("Number of cells/students:"),
                         min = 1,
                         max = 1000,
                         value = 30),
            
            numericInput("l",
                         h4("Length of 'watershed':"),
                         min = 1,
                         max = 1000,
                         value = 10),
            
            numericInput("w",
                         h4("Width of 'watershed':"),
                         min = 1,
                         max = 1000,
                         value = 10),
            
            selectInput("type",
                        h4("Arrangement of 'watershed':"),
                        choices = list("Bottom" = "bottom",
                                       "Middle" = "middle",
                                       "Top" = "top",
                                       "Random" = "random")),
            
            actionButton("create_ws", "Create Unit Hydrograph")
        ),
        
    

        # Show a plot of the generated distribution
        mainPanel(
            h1("Ping Pong Unit Hydrographs"),
            
            h4("This app simulates the 'Ping Pong Ball Unit Hydrograph' exercise outlined in", a(href="https://www.hydrol-earth-syst-sci.net/22/2607/2018/hess-22-2607-2018.html", target="_blank", "Shulz et al. (2018)."), 
            "The exercise involves using students in a class
              to create a watershed with simple routing rules. Each student is given a plastic ball to represent 1 inch (or cm) of excess rainfall, which is
              then routed to the watershed outlet by being passed to the adjacent person in each time step. The balls that reach the outlet are then plotted
              to show the unit hydrograph for the class watershed. This simulation allows the user to create a watershed by specifying the length, width, 
              number of students, and how to fill the seats (e.g. weighted towards the top, bottom, or middle, or randomly assigned). The unit hydrograph for this
              class watershed is then shown. Subsequently, users can then specify an excess precipitation hyetograph and the program will use hydrograph
              convolution to calculate the runoff hydrograph for this storm."),
            
            br(),
            br(),
           #plotOutput("watershed", width = "600px"),
           
           #plotOutput("UH", width = "600px")
           splitLayout(cellWidths = c("50%", "50%"), plotOutput("watershed"), plotOutput("UH"))
        ),

    ),
    
    br(),
    br(),
        
    sidebarLayout(
        

            sidebarPanel(
                textInput("ppt", h4("Enter excess precip values (inches), separated by commas"),
                          "1,2,0.5"),
                
                actionButton("storm_hydrograph", "Create Storm Hydrograph")
            ),
        
            mainPanel(
                plotOutput("storm_plots", width = "1200px"),
                
                br(),
                br(),
                
                h4("Code created by Rod Lammers (rodlammers@gmail.com).")
            )
    ),
    br()
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$create_ws, {
        if (input$l * input$w < input$n){
            showNotification(h4("You don't have enough seats for your students! Increase the width or length values."),
                             type = "error")
        }else{
            ws_ras <- create_WS(input$n, input$l, input$w, input$type)
        
            output$watershed <- renderPlot({
                # # generate bins based on input$bins from ui.R
                # x    <- faithful[, 2]
                # bins <- seq(min(x), max(x), length.out = input$bins + 1)
                # 
                # # draw the histogram with the specified number of bins
                # hist(x, breaks = bins, col = 'darkgray', border = 'white')
                
                
                plot_WS(ws_ras)
            
            }, res = 72)
        
        
            output$UH <- renderPlot({
                route_WS(ws_ras, plot = TRUE)
            })
        }
    })
    
    observeEvent(input$storm_hydrograph, {
        if (input$l * input$w < input$n){
            showNotification(h4("You don't have enough seats for your students! Increase the width or length values."),
                             type = "error")
        }else{
            ppt_vals <- as.numeric(unlist(strsplit(input$ppt, ",")))
            
            ws_ras <- create_WS(input$n, input$l, input$w, input$type)
            output$storm_plots <- renderPlot({
                storm_hydrograph(ws_ras, ppt_vals)
            })
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
