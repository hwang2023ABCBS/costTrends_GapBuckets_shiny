# Library
library(ggplot2)
library(shiny)
library(pins)
board <- board_connect()
GapLength_MbrGapBucketLevel_AllGaps <- pin_read(board, "hwang/GapLength_MbrGapBucketLevel_AllGaps")


source("FUNC_COST_BY_GAPBUCKETS.R")


ui <- fluidPage(
  titlePanel("Interactive Plot of Gaps"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('gap_type', 'Select Gap Type', choices= unique(GapLength_MbrGapBucketLevel_AllGaps$GAP_CAT_BUCKETED))
      # sliderInput('size','point size', min = 0.2, max=5, value = 1)
    ),
    mainPanel(
      plotOutput('gapPlot'),
      plotOutput('costTrend')
    )
  )
)



# server <- function(input,output){
#   output$gapPlot <- renderPlot({
#     # filter data based on selected gap type
#     filtered_data <- GapLength_MbrGapBucketLevel_AllGaps[GapLength_MbrGapBucketLevel_AllGaps$GAP_CAT_BUCKETED==input$gap_type, ]
#     # generate the plot
#     plot(filtered_data$episode_length,
#          main= paste("Gap Lenth by Member ID for", input$gap_type),
#          xlab= "Observation Index",
#          ylab= "Gap Legth",
#          pch=19,
#          cex= input$size)
#   })
# }



server <- function(input,output){
  # filter data based on selected gap type
  filtered_data <- reactive({
    req(input$gap_type)
    GapLength_MbrGapBucketLevel_AllGaps %>% 
      filter(GAP_CAT_BUCKETED== input$gap_type)
  })
  output$gapPlot <- renderPlot({
    data <- filtered_data()
    # generate the density plot
    ggplot(data, aes(x=episode_length))+
      geom_density(fill='blue', alpha=0.5)+
      labs(title = paste("Density of Gap Lengths for", input$gap_type),
           x="Gap Length",
           y="Density")
  })
  
  # costs trend plots by gap buckets start here
  output$costTrend <- renderPlot({
    data <- filtered_data()  # reuse reactive data
    # filter data based on selected gap type
    filtered_data_cost <- func_CostByGapbuckets_subsetting(data)
    library(marginaleffects)
    library(ggplot2)
    mod_filteredCohort <- lm(ALLOWED~
                               # episode_length
                               # +Elixhuaser_Mortality+Age
                               # +Gender
                               Elixhuaser_Mortality*Age*episode_length
                             + I(Age^2)
                             ,
                             data = filtered_data_cost )
    plot_predictions(mod_filteredCohort, condition = 'episode_length')
    
    
  })
  
}


shinyApp(ui, server)