# Dependencies
library(shiny)
library(shinydashboard)
library(shinyvalidate)
library(ggplot2)
library(plotly)
library(ggdark)
library(htmlwidgets)
source("helpers.R")

# Data
data<-read.csv('data/acft_scoring_scales.csv')

# Dashboard Header
header <- dashboardHeader(
  title = "ACFT Calculator"
)

# Dashboard Body
body<-dashboardBody(
  
  fluidRow(
    valueBoxOutput("score",width=NULL)),
  plotlyOutput("plot")
)

# Dashboard Sidebar
sidebar<-dashboardSidebar(
  # Input: Gender
  div(
    selectInput("gdr", "Select your gender:",
                choices = c("Female", "Male")),
    style="text-align:center"
  ),
  # Input: Age
  div(
    numericInput("age", "Age",17, min=17),
    style="text-align:center"
  ),
  div(
    # Input: Deadlift
    numericInput("mdl", "Deadlift",0),
    style="text-align:center"
  ),
  # Input: Standing Power Throw
  div(
    numericInput("spt", "Standing Power Throw",0, min=0),
    style="text-align:center"
  ),
  # Input: Hand Release Push-up
  div(
    numericInput("hrp", "Hand Release Push-up",0, min=0),
    style="text-align:center"
  ),
  # Input: Sprint/Drag/Carry
  verticalLayout(
    div(style="text-align:center;font-weight:bold;","Sprint-Drag-Carry"),
    splitLayout(
      numericInput("sdc_min","minutes",0, min=0),
      numericInput("sdc_sec", "seconds",0, min=0,max=59),
      cellWidths = c("50%", "50%"),
    )
  ),
  # Input: Plank
  verticalLayout(
    div(style="text-align:center;font-weight:bold;","Plank"),
    splitLayout(
      numericInput("plk_min","minutes",0, min=0),
      numericInput("plk_sec", "seconds",0, min=0, max=59),
      cellWidths = c("50%", "50%"),
    )
  ),
  # Input: Two Mile Run
  verticalLayout(
    div(style="text-align:center;font-weight:bold;","Two Mile Run"),
    splitLayout(
      numericInput("tmr_min","minutes",0, min=0),
      numericInput("tmr_sec", "seconds",0, min=0,max=59),
      cellWidths = c("50%", "50%"),
    )
  ),
  # Action button
  div(
    actionButton("update", "Get Score"),
    style="display:inline-block; display:center-align;width=98%;"
  )
  
)

# Dashboard UI: Header, Sidebar, Body
ui<-dashboardPage(header, sidebar, body)


server <- function(input, output) {
  
  # Data validation rules
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
    
    # Translate minute and seconds to just seconds
    sdc_val<-get_time(0, input$sdc_min, input$sdc_sec)
    plk_val<-get_time(0, input$plk_min, input$plk_sec)
    tmr_val<-get_time(0, input$tmr_min, input$tmr_sec)
    
    # Create lists of input information
    mdl<-list('3 Repetition Deadlift', input$mdl, age, gdr,df)
    spt<-list('Standing Power Throw', input$spt, age, gdr,df)
    hrp<-list('Hand Release Push-Up', input$hrp, age, gdr,df)
    sdc<-list('Sprint-Drag-Carry', sdc_val, age, gdr,df)
    plank<-list('Plank', plk_val, age, gdr,df)
    run<-list('Run', tmr_val, age, gdr,df)
    
    # Get scores based off of input and create dataframe
    all_events<-list(mdl,spt,hrp,sdc,plank,run)
    scores<-get_total_score(all_events, data)
    df<-get_df(all_events, scores)
  })
  
  # ValueBox for ACFT Score--Green is Pass & Red if Fail
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
  
  # Output ggplotly plot with scores
  output$plot<-renderPlotly({
    plot<-ggplot(scores(), aes(x=Event, y=Score, fill=Score>=60))+
      geom_bar(stat="identity", alpha=.8, width=.4)+
      scale_fill_manual(name = 'Score>=60', values = setNames(c('#00a65a','#dd4c39'),c(T, F))) +
      scale_y_continuous(limits=c(0,102), expand = c(0, 0))+
      coord_flip()+
      xlab("")+
      geom_hline(yintercept=60, color="#dd4c39")+
      geom_hline(yintercept=100, color="#00a65a")+
      theme_bw()+
      theme(legend.position = "none")
    ggplotly(plot+labs(title="ACFT Event Scores"),
             tooltip=c("Event","Score"))
  })
  output$sdc_text <- renderText({"Sprint-Drag-Carry"})
}

# Create Shiny app
shinyApp(ui, server)
