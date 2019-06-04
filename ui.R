library(shiny)
library(shinythemes)
library(DT)
library(plotly)
SciViews::R

shinyUI(
  navbarPage(
    "Dashboard",
    tabPanel("General description",
             sidebarLayout(
               sidebarPanel(
                 imageOutput("u_generaldescription_image1")
               ),
               mainPanel(
                 h4("Description of database"),
                 textOutput("u_generaldescription_text1"),
                 tableOutput("u_generaldescription_datatable1"),
                 textOutput("u_generaldescription_text2"),
                 DTOutput("u_generaldescription_datatable2"),
                 hr(),
                 imageOutput("u_generaldescription_image2")
                 )
               )),
    tabPanel("Global view",
             sidebarLayout(
               sidebarPanel(width = 2,
                 radioButtons("u_globalview_radio", "Select the desired graphic representation below",
                              choices = c("Submission by time",
                                          "Number of standardized attempts"),
                              selected = "Submission by time")
                 ),
               mainPanel(width = 10,
                 plotOutput("u_globalview_plot", height = "900px"),
                 verbatimTextOutput("u_globalview_caption"),
                 hr()
                 )
                 )
                 ),
    navbarMenu("Questionnaire",
               tabPanel("Global Score",
                        sidebarLayout(
                          sidebarPanel(width = 2, "INSERT TEXT"),
                          mainPanel(width = 10,
                            plotOutput("u_globalscore_plot", height = "900px"),
                            verbatimTextOutput("u_globalscore_caption")
                          )
                        )
                        ),
               tabPanel("Number of attempts",
                        tabsetPanel(
                          tabPanel("Attempt",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                       selectInput("u_numberofattempts_selectinput", "Select the desired exercice below",
                                                   choices = unique(sdd_dt$tuto_label),
                                                   selected = unique(sdd_dt$tuto_label)[1],
                                                   selectize = F),
                                       radioButtons("u_numberofattempts_radio", "Unit :",
                                                    choices = c("percentage (%)", "number of students"),
                                                    selected = "percentage (%)")
                                     ),
                                     mainPanel(width = 10,
                                       plotlyOutput("u_numberofattempts_plotly", height = "500px"),
                                       br(),
                                       DTOutput(outputId = "u_numberofattempts_datatable1.1"),
                                       br()
                                     )
                                   )
                          ),
                          tabPanel("Answer",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  htmlOutput("u_numberofattempts_selected")),
                                     mainPanel(width = 10, DTOutput(outputId = "u_numberofattempts_datatable2.1"))
                                   )
                          )
                        )
               )
    ),
    tabPanel("Students",
      tabsetPanel(type = "pills",
               tabPanel(
                 title = "Global View by questionnaire",
                 sidebarLayout(
                   sidebarPanel(width = 2,
                     selectInput("u_students_selectinput1.1", "Select the desired participant below",
                                 choices = unique(sort(sdd_dt$user_name)),
                                 selected = unique(sort(sdd_dt$user_name))[1],
                                 selectize = F)
                   ),
                   mainPanel(width = 10,
                     plotOutput("u_students_plotly1.1"),
                     br(),
                     br(),
                     br(),
                     plotlyOutput("u_students_plotly1.2"),
                     plotlyOutput("u_students_plotly1.3"),
                     br()
                   )
                 )
               ),
               tabPanel(
                 "Time by student",
                 sidebarLayout(
                   sidebarPanel(width = 2,
                     selectInput("u_students_selectinput2.1", "Select the desired student below",
                                 choices = unique(sort(sdd_dt$user_name)),
                                 selected = unique(sort(sdd_dt$user_name))[1],
                                 selectize = F),
                     selectInput("u_students_selectinput2.2", "Select the desired questionnaire below",
                                 choices = unique(sort(sdd_dt$tutorial)),
                                 selected = unique(sort(sdd_dt$tutorial))[1])
                     #uiOutput("u_selectinput_quiz_stu2"),

                   ),
                   mainPanel(width = 10,
                     plotlyOutput("u_students_plotly2.1"),
                     plotlyOutput("u_students_plotly2.2")


                   )
                 )
               )
             )

    )

                 ))
