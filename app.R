#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)
library(shinydashboard)
library(shinyvalidate)
library(ggplot2)
library(plotly)
library(ggdark)
library(htmlwidgets)
source("helpers.R")
data<-read.csv('data/acft_scoring_scales.csv')


body<-dashboardBody(

  fluidRow(
  valueBoxOutput("score",width=NULL)),
  plotlyOutput("plot")
)

sidebar<-dashboardSidebar(
  # Input: Gender ----
  div(
    selectInput("gdr", "Select your gender:",
                choices = c("Female", "Male")),
    style="text-align:center"
  ),
  # Input: Age ----
  div(
    numericInput("age", "Age",17, min=17),
    style="text-align:center"
  ),
  div(
    # Input: Deadlift ----
    numericInput("mdl", "Deadlift",0),
    style="text-align:center"
  ),
  # Input: Standing Power Throw ----
  div(
    numericInput("spt", "Standing Power Throw",0, min=0),
    style="text-align:center"
  ),
  # Input: Hand Release Push-up ----
  div(
    numericInput("hrp", "Hand Release Push-up",0, min=0),
    style="text-align:center"
  ),
  # Input: Sprint/Drag/Carry ----
  verticalLayout(
    div(style="text-align:center;font-weight:bold;","Sprint-Drag-Carry"),
    splitLayout(
      numericInput("sdc_min","minutes",0, min=0),
      numericInput("sdc_sec", "seconds",0, min=0,max=69),
      cellWidths = c("50%", "50%"),
      
    )
  ),
  # Input: Plank ----
  verticalLayout(
    div(style="text-align:center;font-weight:bold;","Plank"),
    splitLayout(
      numericInput("plk_min","minutes",0, min=0),
      numericInput("plk_sec", "seconds",0, min=0, max=59),
      cellWidths = c("50%", "50%"),
      
    )
  ),
  # Input: Two Mile Run ----

  verticalLayout(
    div(style="text-align:center;font-weight:bold;","Two Mile Run"),
    splitLayout(
      numericInput("tmr_min","minutes",0, min=0),
      numericInput("tmr_sec", "seconds",0, min=0,max=59),
      cellWidths = c("50%", "50%"),
      
    )
  ),
  # Action Button ----
  div(
  actionButton("update", "Get Score"),
  style="text-align:center"
  )
  
)
header <- dashboardHeader(
  title = "ACFT Calculator"
)

ui<-dashboardPage(header, sidebar, body)
# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Data validation
  iv<-InputValidator$new()
  iv$add_rule("age",sv_gt(16))
  iv$add_rule("hrp", sv_integer())
  iv$add_rule("mdl", sv_integer())
  iv$add_rule("sdc_min",sv_integer())
  iv$add_rule("sdc_sec",sv_integer())
  iv$add_rule("sdc_sec",sv_lt(60))
  iv$add_rule("plk_min",sv_integer())
  iv$add_rule("plk_sec",sv_integer())
  iv$add_rule("plk_sec",sv_lt(60))
  iv$add_rule("tmr_min",sv_integer())
  iv$add_rule("tmr_sec",sv_integer())
  iv$add_rule("tmr_sec",sv_lt(60))
  iv$enable()

  
  
  # Reactive event to gather scores
  scores <- eventReactive(input$update, {
    req(iv$is_valid())
    gdr<-input$gdr
    age<-input$age

    # Times are actually stored as hour:minute
    sdc_val<-get_time(0, input$sdc_min, input$sdc_sec)
    plk_val<-get_time(0, input$plk_min, input$plk_sec)
    tmr_val<-get_time(0, input$tmr_min, input$tmr_sec)
    # Create lists
    mdl<-list('3 Repetition Deadlift', input$mdl, age, gdr,df)
    spt<-list('Standing Power Throw', input$spt, age, gdr,df)
    hrp<-list('Hand Release Push-Up', input$hrp, age, gdr,df)
    sdc<-list('Sprint-Drag-Carry', sdc_val, age, gdr,df)
    plank<-list('Plank', plk_val, age, gdr,df)
    run<-list('Run', tmr_val, age, gdr,df)
    
    all_events<-list(mdl,spt,hrp,sdc,plank,run)
    scores<-get_total_score(all_events, data)
    df<-get_df(all_events, scores)
  })
  
  # Generate a summary of the dataset ----

  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$score<-renderValueBox({
    total<-sum(scores()$Score)
    min_score<-min(scores()$Score)
    if (min_score>=60){
    mybox<-valueBox(
      value=total,
      subtitle="ACFT Passed",
      icon=icon("fa-solid fa-dumbbell"),
      color="green"
    )
    }
    else{
    mybox<-valueBox(
      value=total,
      subtitle="ACFT Failed",
      icon=icon("fa-solid fa-dumbbell"),
      color="red"
    )
    }
    mybox
    })
  
  
  output$plot<-renderPlotly({
    plot<-ggplot(scores(), aes(x=Event, y=Score, fill=Score>=60))+
      geom_bar(stat="identity", alpha=.8, width=.4)+
      scale_fill_manual(name = 'Score>=60', values = setNames(c('green','red'),c(T, F))) +
      scale_y_continuous(limits=c(0,102), expand = c(0, 0))+
      coord_flip()+
      xlab("")+
      geom_hline(yintercept=60, color="red")+
      geom_hline(yintercept=100, color="green")+
      theme_bw()+
      theme(legend.position = "none")
    ggplotly(plot+labs(title="ACFT Event Scores"),
             tooltip=c("Event","Score"))
})
  output$sdc_text <- renderText({"Sprint-Drag-Carry"})
}

# Create Shiny app ----
shinyApp(ui, server)

