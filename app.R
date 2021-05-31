
library(shiny)
library(ggplot2)

plot_type <- c("Density plot", "Histogram", "Frequency Polygon")

ui <- fluidPage(
    varSelectInput("var1", "X Variable", data = mtcars),
    radioButtons("type", "choose a plot type",
                 choices = plot_type),
    plotOutput("plot")
    
    
)

server <- function(input, output, session) {
    output$plot <- renderPlot({
        if (input$type == "Density plot"){
            ggplot(mtcars, aes(x = !!input$var1)) + 
                geom_density(outline.type = "full")
        }
        else if (input$type == "Histogram"){
            ggplot(mtcars, aes(x = !!input$var1)) + 
                geom_histogram()
        }
        else if (input$type == "Frequency Polygon"){
            ggplot(mtcars, aes(x = !!input$var1)) + 
                geom_freqpoly()
        }
        
    })
    
}

shinyApp(ui, server)



