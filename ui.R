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
               sidebarPanel(
                 radioButtons("nb_tuto", "Select the desired graphic representation below",
                              choices = c("Submission by time",
                                          "Total number of attempts per quiz",
                                          "Number of standardized attempts"),
                              selected = "Submission by time")
                 ),
               mainPanel(
                 plotOutput("bar_plot"),
                 hr(),
                 p("After analyzing the different graphs available to you, you were able to highlight tutorials that may have caused problems for students. You have at your disposal the following tabs (Quiz, Students to continue your analysis.")
                 )
                 )
                 ),
    navbarMenu("Quiz",
               tabPanel("Global Score",
                        sidebarLayout(
                          sidebarPanel(),
                          mainPanel(
                            plotOutput("u_global_score")
                          )
                        )
                        ),
               tabPanel("Jordan",
                        tabsetPanel(
                          tabPanel("Attempt",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("tuto_lab", "Select the desired questionnaire below",
                                                   choices = unique(sdd_dt$tuto_label),
                                                   selected = unique(sdd_dt$tuto_label)[1],
                                                   selectize = F),
                                       radioButtons("ui_quiz_unit", "Unit :",
                                                    choices = c("percentage (%)", "number of students"),
                                                    selected = "percentage (%)")
                                     ),
                                     mainPanel(
                                       plotlyOutput("plot1"),
                                       br(),
                                       DTOutput(outputId = "u_quiz_table1"),
                                       br()

                                     )
                                   )
                          ),
                          tabPanel("Answer",
                                   sidebarLayout(
                                     sidebarPanel(
                                       uiOutput("ui_tuto_lab")
                                     ),
                                     mainPanel(
                                       tabPanel("Subtab2",
                                                DTOutput(outputId = "u_quiz_table2")
                                       ))
                                   )
                          )
                        )
               ),
               tabPanel("Guyliann",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("tuto", "Select the desired quiz below",
                                        choices = unique(sdd_dt$tutorial),
                                        selected = unique(sdd_dt$tutorial)[2],
                                        selectize = F)
                            ),
                          mainPanel(
                            title = "Plot 1",
                            plotOutput("bar_plot_quiz"))
                        )
               )
    ),
    tabPanel("Students",
             tabsetPanel(
               tabPanel(
                 "Test",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("stu", "Select the desired participant below",
                                 choices = unique(sort(sdd_dt$user_name)),
                                 selected = unique(sort(sdd_dt$user_name))[1],
                                 selectize = F)
                   ),
                   mainPanel(
                     plotOutput("bar_plot_stu"),
                     br(),
                     plotlyOutput("plot3"),
                     br()
                   )
                 )
               ),
               tabPanel(
                 "Test2",
                 sidebarLayout(
                   sidebarPanel(
                     uiOutput("u_stu_name")
                   ),
                   mainPanel(
                     plotOutput("bar_plot_stu1")
                   )
                 )
               )
             )

    )
                 ))
