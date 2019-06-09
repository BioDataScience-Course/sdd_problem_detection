library(shiny)
library(shinythemes)
library(DT)
library(plotly)
SciViews::R

shinyUI(
  navbarPage(theme = shinytheme("cerulean"),
    "Dashboard",
    tabPanel("General description",
             sidebarLayout(
               sidebarPanel( width = 2,
                 htmlOutput("u_generaldescription_image1")
               ), #sidebarPanel
               mainPanel( width = 10,
                 h4("Description of database"),
                 textOutput("u_generaldescription_text1"),
                 tableOutput("u_generaldescription_datatable1"),
                 textOutput("u_generaldescription_text2"),
                 DTOutput("u_generaldescription_datatable2"),
                 hr(),
                 htmlOutput("u_generaldescription_image2")
                 )#mainPanel
               )#sidebarLayout
      ), #tabPanel
    tabPanel("Global view",
             sidebarLayout(
               sidebarPanel(width = 2,
                 radioButtons("u_globalview_radio",
                   "Select the desired graphic representation below",
                              choices = c("Submission by time",
                                          "Number of standardized attempts"),
                              selected = "Submission by time")
                 ),
               mainPanel(width = 12,
                 plotOutput("u_globalview_plot", height = "650px"),
                 verbatimTextOutput("u_globalview_caption"),
                 hr()
                 )#mainPanel
               )#sidebarLayout
      ),#tabPanel
    navbarMenu("Questionnaire",
               tabPanel("Global Score",
                        sidebarLayout(
                          sidebarPanel(width = 2,
                            "This graph shows how students responded to the questionnaires"),
                          mainPanel(width = 12,
                            plotOutput("u_globalscore_plot", height = "650px"),
                            verbatimTextOutput("u_globalscore_caption")
                          )#mainPanel
                        )#sidebarLayout
                 ),#tabPanel
               tabPanel("Number of attempts",
                 selectInput("u_numberofattempts_selectinput",
                   "Select the desired exercice below",
                 choices = unique(sdd_dt$tuto_label),
                 selected = unique(sdd_dt$tuto_label)[1],
                 selectize = F),
                        tabsetPanel(
                          tabPanel("Attempt",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                       radioButtons("u_numberofattempts_radio",
                                         "Unit :",
                                                    choices = c("percentage (%)",
                                                      "number of students"),
                                                    selected = "percentage (%)")
                                     ), #sidebarPanel
                                     mainPanel(width = 12,
                                       plotlyOutput("u_numberofattempts_plotly",
                                         height = "450px"),
                                       br(),
                                       DTOutput(outputId = "u_numberofattempts_datatable1.1" ),
                                       br()
                                     )#mainPanel
                                   )#sidebarLayout
                          ),#tabPanel
                          tabPanel("Answer",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                       "This data table shows what the students' responses"),
                                     mainPanel(width = 12, DTOutput(outputId = "u_numberofattempts_datatable2.1"))
                                   )#sidebarLayout
                            )#tabPanel
                          )#tabsetPanel
                 )#tabPanel
    ), #navBarMenu
    tabPanel("Students",
      selectInput("u_students_selectinput1",
        "Select the desired student below",
      choices = unique(sort(sdd_dt$user_name)),
      selected = unique(sort(sdd_dt$user_name))[1],
      selectize = F),
      tabsetPanel(
               tabPanel(
                 title = "Global view by questionnaire",
                 sidebarLayout(
                   sidebarPanel(width = 2,
                     htmlOutput("u_students_text")),
                   mainPanel(width = 12,
                     plotOutput("u_students_plot1.1"),
                     plotlyOutput("u_students_plotly1.2"),
                     plotlyOutput("u_students_plotly1.3"),
                     br()
                   )#mainpanel
                 )#sidebarLayout
               ),#tabPanel
               tabPanel(
                 "Exercices by student",
                 sidebarLayout(
                   sidebarPanel(width = 2,
                     selectInput("u_students_selectinput2.1",
                       "Select the desired questionnaire below",
                                 choices = unique(sort(sdd_dt$tutorial)),
                                 selected = unique(sort(sdd_dt$tutorial))[1])#selectinput
                   ), #sidebarLayout
                   mainPanel(width = 12,
                     plotlyOutput("u_students_plotly2.1"),
                     plotlyOutput("u_students_plotly2.2")
                   )#mainPanel
                 )#sidebarLayout
               )#tabPanel
             )#tabsetPanel
      )#tabPanel General Description
    )#navbarPage
  )#shinyUI
