library(shiny)
library(ggplot2)

datasets <- c("economics", "faithfuld", "seals")
ui <- fluidPage(
  titlePanel("Pinniped Behavioral Responses and Learning in the Gauntlet"),
  "This plot shows the underlying salmon presence at the Gauntlet over a 365 day period.",
  plotOutput("salmon_species_plot")
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$plot <- renderPlot({
    plot(dataset())
  }, res = 96)
}

shinyApp(ui, server)