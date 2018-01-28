#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)


state_ts <- read_csv("data/clean_state.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
      titlePanel("Housing price tracker"),
      
      sidebarLayout(
            sidebarPanel(
               
                  sliderInput("yearInput", "Years",
                              min = 2000,
                              max = 2017,
                              value = c(2000,2017),
                              sep = ""),
                  
                  sliderInput("priceInput", "Price range",
                              min = 30000,
                              max = 1500000, 
                              value = c(50000,1500000 ),
                              pre = "$",
                              sep = ",",
                              step = 20000),
                  
                  uiOutput("stateOutput"),
                  
                  checkboxInput("allInput", "Include all states", value = FALSE),
                  
                  checkboxGroupInput("typeInput", "Home type",
                                     choices = c("1 bedroom" = "1bedroom", 
                                                 "2 bedroom" = "2bedroom", 
                                                 "3 bedroom" = "3bedroom", 
                                                 "4 bedroom" = "4bedroom", 
                                                 "5 bedroom or more" = "5BedroomOrMore", 
                                                 "Condo and co-operatives" = "CondoCoop", 
                                                 "House" = "SingleFamilyResidence"),
                                     selected = c("1bedroom", "2bedroom",  "CondoCoop", "SingleFamilyResidence")
                  ), 
                  
                  checkboxGroupInput("tierInput", "Tier types",
                                     choices = c("Top Tier" = "TopTier", "Middle Tier" = "MiddleTier", "Bottom Tier" = "BottomTier"),
                                     selected = c("TopTier", "MiddleTier")
                  )

                  
            ),
            
            mainPanel(
                  downloadButton("graph", "Download the plot"),
                  plotOutput("distPlot"),
                  br(), 
                  br(),
                  downloadButton("downloadData", "Download the data"),
                  DT::dataTableOutput("data")
                  
            )
      )
   

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
      observeEvent(
            input$allInput,
            updateSelectInput(session,
                              inputId = "stateInput",
                              'State',
                              choices = sort(unique(state_ts$region)),
                              selected = c(unique(state_ts$region))),
            ignoreInit = T
      )
      
      output$stateOutput <- renderUI({
            selectInput("stateInput", "State",
                        sort(unique(state_ts$region)),
                        selected = c("NewYork", "Michigan", "California", "Ohio", "Texas", "Washington") ,
                        multiple = TRUE)
      })  
      
      filtered <- reactive({
            if (input$allInput){
                        state_ts %>% 
                              filter(year >= input$yearInput[1],
                                     year <= input$yearInput[2],
                                     price >= input$priceInput[1],
                                     price <= input$priceInput[2],
                                     type %in% c(input$typeInput, input$tierInput))
            } else {
                  state_ts %>% 
                        filter(year >= input$yearInput[1],
                               year <= input$yearInput[2],
                               price >= input$priceInput[1],
                               price <= input$priceInput[2],
                               region %in% input$stateInput,
                               type %in% c(input$typeInput, input$tierInput))
            }
                                     
      })
      
     
      
      
      
      plotInput <- reactive({
            p <- ggplot(filtered(), aes( x = year, y = price, fill = type)) +
                  geom_col(position = "dodge") +
                  theme_minimal() +
                  scale_y_continuous(labels = scales::dollar_format()) +
                  xlab("Year") +
                  ylab("Price - median estimated home value") +
                  ggtitle("Zillow's median estimated home value across different year") +
                  scale_fill_discrete("") +
                  theme(plot.title = element_text(size = 20, hjust = 0.5),
                        axis.text.y = element_text(size = 12, angle = 45 ),
                        axis.text.x = element_text(size = 12, angle = 90 ),
                        axis.title = element_text(size = 14),
                        legend.text = element_text(size = 10, angle = 30),
                        strip.text = element_text(size = 13))
            
            
          
      })
      
      output$distPlot <- renderPlot({
            print(plotInput())
      })
      
      output$data <- DT::renderDataTable({
            filtered()
      })
      
      output$downloadData <- downloadHandler(
            filename = "house.csv"
            ,
            content = function(file) {
                  write.csv(filtered(), file, row.names = FALSE)
            }
      )
      
      output$graph <- downloadHandler(
            filename = "graph.png"
            ,
            content = function(file) {
                  device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 150, units = "in")
                  ggsave(filename = file, plot = plotInput(), device = device)
            }
      )
      
}

# Run the application 
shinyApp(ui = ui, server = server)

