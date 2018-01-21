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
library(plotly)
library(leaflet)

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
                  
                  checkboxGroupInput("typeInput", "Home type",
                                     choices = c("1 bedroom" = "ZHVI_1bedroom", 
                                                 "2 bedroom" = "ZHVI_2bedroom", 
                                                 "3 bedroom" = "ZHVI_3bedroom", 
                                                 "4 bedroom" = "ZHVI_4bedroom", 
                                                 "5 bedroom or more" = "ZHVI_5BedroomOrMore", 
                                                 "Condo and co-operatives" = "ZHVI_CondoCoop", 
                                                 "House" = "ZHVI_SingleFamilyResidence"),
                                     selected = c("1 bedroom", "2 bedroom", "3 bedroom", "4 bedroom", "5 bedroom or more", "Condo and co-operatives", "House")
                  ), 
                  
                  checkboxGroupInput("tierInput", "Tier types",
                                     choices = c("Top Tier" = "ZHVI_TopTier", "Middle Tier" = "ZHVI_MiddleTier", "Bottom Tier" = "ZHVI_BottomTier"),
                                     selected = c("Top Tier", "Middle Tier", "Bottom Tier")
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
server <- function(input, output) {
      
      filtered <- reactive({
            state_ts <- read_csv("data/State_time_series.csv")
            state_ts$Date <- year(state_ts$Date)
            state_ts <- state_ts %>% select("year" = "Date", everything()) %>% 
                  filter(year >= input$yearInput[1],
                         year <= input$yearInput[2]) 
      })
      
      plotInput <- reactive({
            p <- ggplot(filtered(), aes_string( x = input$yearInput, y = input$yInput)) +
                  geom_point(alpha = input$alphaInput, aes(color = continent)) + 
                  theme_minimal() +
                  xlab(input$xInput) +
                  ylab(input$yInput) + 
                  ggtitle(input$titleInput) +
                  scale_color_discrete("") +
                  scale_x_continuous(labels = scales::comma_format()) + 
                  scale_y_continuous(labels = scales::comma_format()) +
                  theme(plot.title = element_text(size = 20, hjust = 0.5),
                        axis.text = element_text(size = 14),
                        axis.title = element_text(size = 16),
                        legend.text = element_text(size = 14),
                        strip.text = element_text(size = 13))
            
            if (input$facetInput){
                  p <- p + facet_wrap(~continent) 
            }
            
            if (input$logInput == "1"){
                  p 
            } else if (input$logInput == "2"){
                  (p <- p + scale_x_log10(labels = scales::comma_format()))
            } else if (input$logInput == "3"){
                  (p <- p + scale_y_log10(labels = scales::comma_format()))      
            } else {
                  (p <- p + scale_x_log10(labels = scales::comma_format()) + scale_y_log10(labels = scales::comma_format()))
            }
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

