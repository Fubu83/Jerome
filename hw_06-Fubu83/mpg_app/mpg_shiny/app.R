# IFEBUNANDU JEROME OKEKE

library(shiny)
library(ggplot2)

ui <- fluidPage(
    varSelectInput("Var1", "X Variable", data = mpg),
    varSelectInput("Var2", "Y variable", data = mpg),
    varSelectInput("color", "color Variable(Categirical)", data = mpg),
    plotOutput("Scatterplot")

)

server <- function(input, output, session) {
    output$Scatterplot = renderPlot({
        ggplot(mpg, aes(x = !!input$Var1, y = !!input$Var2, color = !!input$color)) +
            geom_point() +
            theme_bw()
    })
  
}

shinyApp(ui, server)