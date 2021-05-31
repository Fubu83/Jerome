
library(shiny)
library(ggplot2)
library(broom)
library(tidyverse)

estate <- read.csv(file = "../data/estate.csv")

estate %>%
    
    mutate(AC = as_factor(AC),
           Pool = as_factor(Pool),
           Highway = as_factor(Highway),
           Style = as_factor(Style)) %>% 
    mutate(Price = Price/1000) %>%
    rename(`Price($K)` = Price) ->
    estate

ui <- fluidPage(titlePanel("EDA of Estate Data"),
                tabsetPanel( tabPanel(title = "Univariate",value = 1),
                             tabPanel(title = "Bivariate",value = 2),  
                             tabPanel(title = "Spreadsheet",value = 3, DT::dataTableOutput("spreadsheet")),
                             id="tab"),
                conditionalPanel(condition = "input.tab==1", 
                                 sidebarPanel(selectInput("var",label = "Variable", choices = names(estate), selected = "Pool"),
                                              checkboxInput("log", "Log_Transform?", value = FALSE, width = NULL),
                                              sliderInput(inputId = "bins", label = "Bins", min = 1, max = 100, value = 50),
                                              numericInput(inputId = "null", label = "Null Value", value = 5.5),
                                              tableOutput("t_stat")),
                                 mainPanel(
                                     plotOutput("plot1")
                                 )
                ),
                conditionalPanel(condition = "input.tab==2", 
                                 sidebarLayout(sidebarPanel(selectInput("var1",label = "X Variable?",
                                                                        choices = names(estate),
                     
                                                                                                                           selected = "Area"),
                                                            checkboxInput("log1", "Log_Transform?", value = FALSE, width = NULL),
                                                            selectInput("var2",label = "Y Variable?",
                                                                        choices = names(estate),
                                                                        selected = "`Price($K)`"),
                                                            checkboxInput("log2", "Log_Transform?", value = FALSE, width = NULL),
                                                            checkboxInput("ols1", "Fit OLS?", value = FALSE, width = NULL)),
                                               mainPanel(
                                                   plotOutput("plot2")
                                               ))
                )
)
server <- function(input, output, session) {
    df = reactive({
        t.test(x = estate[[input$var]], mu = input$null) %>%
            tidy() %>%
            select(`P-value` = p.value, Estimate = estimate, `95% Lower` = conf.low, `95% Higher` = conf.high)
    })
    
    data.frame("Variable is not numeric") %>%
        rename("data" = 1) ->
        df2
    
    output$t_stat <- renderTable({
        if (is.numeric(estate[[input$var]])) {
            df()
        }
        
        else {
            df2
        }
    })
    
    output$plot1 <- renderPlot({
        
        if (is.numeric(estate[,input$var])) {
            
            if(!input$log)
            {
                ggplot(estate, aes(x = .data[[input$var]])) +
                    geom_histogram(bins = input$bins)
            }
            else
            {
                ggplot(estate, aes(x = .data[[input$var]]))+
                    geom_histogram(bins = input$bins) +
                    scale_x_log0()
            }
        }
        
        else{
            output$table1 <- NULL
            ggplot(estate, aes(x = .data[[input$var]])) +
                geom_bar()
        }
    })
    
    output$plot2 <- renderPlot({
        
        if (is.numeric(estate[,input$var1])&&is.numeric(estate[,input$var2])) {
            p2 <- ggplot(estate, aes(x = .data[[input$var1]], y = .data[[input$var2]])) + 
                geom_point()
            if(!input$log1 && !input$log2)
            {
                p2
            }
            else if(input$log1 && !input$log2)
            {
                p2<- p2 + scale_x_log10()
                p2
            }
            else if(!input$log1 && input$log2)
            {
                p2<- p2 + scale_y_log10()
                p2
            }
            else
            {
                p2<- p2 + scale_x_log10() + scale_y_log10()
                p2
            }
            
            if(input$ols1)
            {
                p2 <- p2 +geom_smooth(method='lm', formula= y~x, se=FALSE)
                p2
            }
            else
            {
                p2
            }
            
        }
        else if (!is.numeric(estate[,input$var1])&&is.numeric(estate[,input$var2])&&!input$log1 && input$log2) {
            ggplot(estate, aes(x=.data[[input$var1]], y=.data[[input$var2]])) + 
                geom_boxplot() + scale_y_log10()
        }
        else if (is.numeric(estate[,input$var1])&&!is.numeric(estate[,input$var2])&&input$log1 && !input$log2) {
            ggplot(estate, aes(x=.data[[input$var1]], y=.data[[input$var2]])) + 
                geom_boxplot() + scale_x_log10() + ggstance::geom_boxploth() 
        }
        else if (!is.numeric(estate[,input$var1])&&is.numeric(estate[,input$var2])) {
            ggplot(estate, aes(x=.data[[input$var1]], y=.data[[input$var2]])) + 
                geom_boxplot()
        }
        else if (is.numeric(estate[,input$var1])&&!is.numeric(estate[,input$var2])) {
            ggplot(estate, aes(x=.data[[input$var1]], y=.data[[input$var2]])) + 
                geom_boxplot() + ggstance::geom_boxploth()
        }
        
        else if (!is.numeric(estate[,input$var1])&&!is.numeric(estate[,input$var2])) {
            ggplot(estate, aes(x=.data[[input$var1]], y=.data[[input$var2]])) + 
                geom_jitter()
        }
    })
    
    output$lm_results <- renderPrint({
        if(is.numeric(estate[,input$var1])&&is.numeric(estate[,input$var2])&&input$ols1) {
            lmout <- lm(formula = estate[[input$var2]]~estate[[input$var1]], data = estate)
            print(summary(lmout))
            if (!input$log1 && !input$log2) {
                lmout <- lm(formula = estate[[input$var2]]~estate[[input$var1]], data = estate)
                print(summary(lmout))
            }
        }
        else if(is.numeric(estate[,input$var1])&&is.numeric(estate[,input$var2])&&input$log1 && !input$log2) {
            lmout <- lm(formula = estate[[input$var2]]~log(estate[[input$var1]]), data = estate)
            print(summary(lmout))
        }
        else if(is.numeric(estate[,input$var1])&&is.numeric(estate[,input$var2])&&!input$log1 && input$log2) {
            lmout <- lm(formula = log(estate[[input$var2]])~estate[[input$var1]], data = estate)
            print(summary(lmout))
        }
        else if (is.numeric(estate[,input$var1])&&is.numeric(estate[,input$var2])&&input$log1 && input$log2) {
            lmout <- lm(formula = log(estate[[input$var2]])~log(estate[[input$var1]]), data = estate)
            print(summary(lmout))
        }
        
    })
   
    output$spreadsheet <- DT::renderDataTable(
        DT::datatable(data = estate[ ,map_lgl(estate, is.numeric)],
                      options = list(pageLength = 10),
                      filter = list(position = "bottom",
                                    clear = TRUE,
                                    plain = FALSE),
                      rownames = FALSE)
    )
}
shinyApp(ui = ui, server = server)