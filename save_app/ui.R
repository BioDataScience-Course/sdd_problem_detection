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
                 imageOutput("econum")
               ),
               mainPanel(
                 h4("Description of database"),
                 textOutput("nbr"),
                 tableOutput("tab_gen"),
                 textOutput("tuto_nbr"),
                 DTOutput("tab_mod"),
                 hr(),
                 imageOutput("bds")
                 )
               )),
    tabPanel("Global view",
             sidebarLayout(
               sidebarPanel(width = 3,
                 radioButtons("nb_tuto", "Select the desired graphic representation below",
                              choices = c("Submission by time",
                                          "Total number of attempts per quiz",
                                          "Number of standardized attempts"),
                              selected = "Submission by time")
                 ),
               mainPanel(width = 9,
                 plotOutput("bar_plot"),
                 verbatimTextOutput("u_caption_bar_plot"),
                 hr()
                 )
                 )
                 ),
    navbarMenu("Quiz",
               tabPanel("Global Score",
                        sidebarLayout(
                          sidebarPanel(width = NULL),
                          mainPanel(width = 12,
                            plotOutput("u_global_score")
                          )
                        )
                        ),
               tabPanel("Jordan",
                        tabsetPanel(
                          tabPanel("Attempt",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                       selectInput("tuto_lab", "Select the desired question below",
                                                   choices = unique(sdd_dt$tuto_label),
                                                   selected = unique(sdd_dt$tuto_label)[1],
                                                   selectize = F),
                                       radioButtons("ui_quiz_unit", "Unit :",
                                                    choices = c("percentage (%)", "number of students"),
                                                    selected = "percentage (%)")
                                     ),
                                     mainPanel(width = 10,
                                       plotlyOutput("plot1"),
                                       br(),
                                       DTOutput(outputId = "u_quiz_table1"),
                                       br()

                                     )
                                   )
                          ),
                          tabPanel("Answer",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  htmlOutput("u_selected_tutorial")),
                                     mainPanel(width = 10, DTOutput(outputId = "u_quiz_table2"))
                                   )
                          )
                        )
               ),
               tabPanel("Guyliann",
                        sidebarLayout(
                          sidebarPanel(width = 2,
                            selectInput("tuto", "Select the desired quiz below",
                                        choices = unique(sdd_dt$tutorial),
                                        selected = unique(sdd_dt$tutorial)[2],
                                        selectize = F)
                            ),
                          mainPanel(width = 10,
                            title = "Plot 1",
                            plotOutput("bar_plot_quiz"))
                        )
               )
    ),
    tabPanel("Students",
             tabsetPanel(type = "pills",
               tabPanel(
                 title = "Test",
                 sidebarLayout(
                   sidebarPanel(width = 2,
                     selectInput("stu", "Select the desired participant below",
                                 choices = unique(sort(sdd_dt$user_name)),
                                 selected = unique(sort(sdd_dt$user_name))[1],
                                 selectize = F)
                   ),
                   mainPanel(width = 10,
                     plotOutput("bar_plot_stu"),
                     br(),
                     br(),
                     br(),
                     plotlyOutput("plot3"),
                     plotlyOutput("plot4"),
                     br()
                   )
                 )
               ),
               tabPanel(
                 "Test2",
                 sidebarLayout(
                   sidebarPanel(width = 2,
                     htmlOutput("u_stu_name"),
                     br(),
                     selectInput("tuto1","Select the desired quiz below",
                                 choices = unique(sdd_dt$tutorial),
                                 selected = unique(sdd_dt$tutorial)[2],
                                 selectize = FALSE)
                   ),
                   mainPanel(width = 10,
                     plotOutput("bar_plot_stu1")
                   )
                 )
               )

             )

    ),
    tabPanel(
      "Test3",
      sidebarLayout(
        sidebarPanel(
          selectInput("u_selectinput_tuto_stu2", "Select the desired tutorial below",
                      choices = unique(sort(sdd_dt$tutorial)),
                      selected = unique(sort(sdd_dt$tutorial))[1]),
          #uiOutput("u_selectinput_quiz_stu2"),
          selectInput("u_selectinput_student_stu2", "Select the desired student below",
                      choices = unique(sort(sdd_dt$user_name)),
                      selected = unique(sort(sdd_dt$user_name))[1],
                      selectize = F)
        ),
        mainPanel(
          plotOutput("bar_plot_stu2"),
          plotOutput("bar_plot_stu2.2")

        )
      )
    )
                 ))
