library(shinydashboard)
library(shiny)
library(latex2exp)
library(tidyverse)
#source('ui_Info.R', local = TRUE)


ui <- dashboardPage(
  dashboardHeader(title = "確率分布の学習"),
  dashboardSidebar(),
  dashboardBody(
    
    fluidRow(
      column(width = 12,
             tabBox(title = "好きな分布を選んでね",id='tab_norm',width = NULL,
                    tabPanel("正規分布",
                             fluidRow(
                               box(
                                 title = "norm",status = "primary",
                                 plotOutput("plot_norm")
                               )
                               ,
                               box(title = "param",status = 'warning',
                                   plotOutput('norm_formula'),
                                   sliderInput('mean','Mean',-10,10,100),
                                   sliderInput('sd','SD',-10,10,100))
                             )
                    )
             )
      )
    )             
  )
)

server <- function(input, output) {
  
  output$plot_norm <- renderPlot({
    data.frame(X=c(-50,50)) %>% 
      ggplot(aes(x=X))+
      stat_function(fun=dnorm, args=list(mean=input$mean, sd=input$sd))+
      #stat_function(fun=dnorm, args=list(mean=10, sd=10))+
      theme_linedraw()
  })
  
  output$norm_formula <- renderPlot({
    plot(latex2exp::TeX('1+1'))
    #plot(1,1)
  })
}

shinyApp(ui, server)