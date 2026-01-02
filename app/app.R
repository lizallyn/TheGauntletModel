library(shiny)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Pinniped Behavioral Responses and Learning in the Gauntlet"),
  "This plot shows the underlying salmon presence at the Gauntlet over a 365 day period.",
  selectInput("bounds", label = "Consumption Bounds", choices = c("Low Consumption", "High Consumption")),
  plotOutput("plot")
)

server <- function(input, output, session) {
  model_return <- reactive({
    return <- shiny_assembleTheLegos(bounds = input$bounds)
  })

  output$plot <- renderPlot({
    model_return$arrive_plot
  }, res = 96)
}

shinyApp(ui, server)