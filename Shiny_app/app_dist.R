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
                                   sliderInput('mean','Mean(mu)',-20,20,200),
                                   sliderInput('sd','SD(sigma)',0,20,100))
                             )
                    ),
                    tabPanel("二項分布",
                             fluidRow(
                               box(
                                 title = "binominal",status = "primary",
                                 plotOutput("plot_bin")
                               )
                               ,
                               box(title = "param",status = 'warning',
                                   sliderInput('prop','prop(mu)',min=0,max=1,step = 0.1,value = 0.5),
                                   sliderInput('size','size(n)',5,20,20))
                             )
                    ),
                    tabPanel("ポワソン分布",
                             fluidRow(
                               box(
                                 title = "poisson",status = "primary",
                                 plotOutput("plot_poisson")
                               )
                               ,
                               box(title = "param",status = 'warning',
                                   sliderInput('lambda','lambda',1,10,10))
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
      theme_linedraw()+
      labs(title = latex2exp::TeX('$\\frac{1}{\\sqrt{2\\pi}\\sigma}\\exp{-\\frac{(x-\\mu)^{2}}{2\\sigma^{2}}}$'))+
      theme(axis.title.y =  element_blank(),
            axis.text = element_text(size=10))
  })
  
  
  output$plot_bin <- renderPlot({
    x<-seq(0,20)
    y<-dbinom(x=x,prob=input$prop, size=input$size)
    
    ggplot()+
    geom_point(aes(x=x,y=y),data=data.frame(x=x,y=y))+
    geom_line(aes(x=x,y=y),data=data.frame(x=x,y=y))+
    theme_linedraw()+
    labs(title = latex2exp::TeX('$_{n}C_{x} \\,\\mu^{x}(1-\\mu)^{n-x}$'))+
    theme(axis.title.y =  element_blank(),
          axis.text = element_text(size=10))+
      scale_x_continuous(breaks=seq(0,20,1))
  })
  
  output$plot_poisson <- renderPlot({
    x<-seq(0,20)
    y<-dpois(x=x,lambda=input$lambda)
  
    
    ggplot()+
      geom_point(aes(x=x,y=y),data=data.frame(x=x,y=y))+
      geom_line(aes(x=x,y=y),data=data.frame(x=x,y=y))+
      theme_linedraw()+
      labs(title = latex2exp::TeX('$\\frac{\\lambda^{x}e^{-\\lambda}}{x!}$'))+
      theme(axis.title.y =  element_blank(),
            axis.text = element_text(size=10))+
      scale_x_continuous(breaks=seq(0,20,1))
  })
}

shinyApp(ui, server)