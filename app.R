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
            
            actionButton("create_ws", strong("Create Unit Hydrograph"), style = "background-color: #a6bddb"),
            
            width = 3
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
           splitLayout(cellWidths = c("50%", "50%"), plotOutput("watershed"), plotOutput("UH")),
           
           width = 9
        ),

    ),
    
    br(),
    br(),
    br(),
    br(),
        
    sidebarLayout(
        sidebarPanel(h2("Animate Unit Hydrograph"),
                     
                     br(),
                     
                     fluidRow(
                         column(4, actionButton("start", strong("Start"), style = "background-color: #a6bddb")),
                         column(4, actionButton("stop", strong("Stop"), style = "background-color: #a6bddb")),
                         column(4, actionButton("next_button", strong("Next"), style = "background-color: #a6bddb"))
                     ),
                     
                     width = 3),
        
        mainPanel(plotOutput("ws_animation"),
                  
                  width = 9)
    ),
    
    br(),
    br(),
    br(),
    br(),
    
    
    sidebarLayout(
        

            sidebarPanel(
                textInput("ppt", h4("Enter excess precip values (inches), separated by commas"),
                          "1,2,0.5"),
                
                actionButton("storm_hydrograph", strong("Create Storm Hydrograph"), style = "background-color: #a6bddb"),
                
                width = 3
            ),
        
            mainPanel(
                plotOutput("storm_plots", width = "1000px"),
                
                br(),
                br(),
                
                h4(a(href = "https://github.com/rodlammers/PingPongUH", target="_blank", "Code"), "created by Rod Lammers (rodlammers@gmail.com)."),
                
                width = 9
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
            
            dist_mat <- route_WS(ws_ras, plot = FALSE)
            
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
    
    plot_values <- reactiveValues()
    plot_values$ws_ras <- NULL
    plot_values$ws_num <- NULL
    plot_values$dist_mat <- NULL
    plot_values$iteration <- 0
    
    forward <- function(){
        ws_ras <- create_WS(input$n, input$l, input$w, input$type)
        dist_mat <- route_WS(ws_ras, plot = FALSE)
        
        inputs <- animate_UH(dist_mat, ws_ras)
        
        plot_values$iteration <- ifelse(plot_values$iteration < length(inputs$ws_ras), plot_values$iteration + 1, 1)
        
        plot_values$ws_ras <- inputs$ws_ras[[plot_values$iteration]]
        plot_values$ws_num <- inputs$ws_num[[plot_values$iteration]]
        plot_values$dist_mat <- dist_mat
        
    }
    
    observeEvent(input$next_button, {
        forward()
    })
    
    session<-reactiveValues()
    session$timer<-reactiveTimer(Inf)
    session$started <- FALSE
    
    observeEvent(input$start, {
        if (input$l * input$w < input$n){
            showNotification(h4("You don't have enough seats for your students! Increase the width or length values."),
                             type = "error")
        }else{
            
            session$started <- TRUE
            # session$timer<-reactiveTimer(900)
            # observeEvent(session$timer(),{
            #     
            #     forward()
            #     
            # })
            }
        }
    )
    
    
    observeEvent(input$stop, {
        #session$timer<-reactiveTimer(Inf)
        session$started <- FALSE
    })
    
    observe({
        session$timer <- reactiveTimer(900)
        observeEvent(session$timer(),{
            if(isolate(session$started)){
                isolate(forward())
            }
        })
    })
    
    output$ws_animation <- renderPlot({
        #session$i <- min(session$i + 1, length(inputs$ws_ras))
        #animate_UH(dist_mat, ws_ras)
        # lapply(1:length(inputs$ws_ras), function(x, inputs, dist_mat){
        #     plot_animation(inputs$ws_ras[[x]], inputs$ws_num[[x]], dist_mat, x)
        # }, inputs, dist_mat)
        
        #plot_animation(inputs$ws_ras[[session$i]], inputs$ws_num[[session$i]], dist_mat, session$i)
        plot_animation(plot_values$ws_ras, plot_values$ws_num, plot_values$dist_mat, plot_values$iteration)
        
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
