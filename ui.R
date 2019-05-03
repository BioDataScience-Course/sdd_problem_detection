library(shiny)
library(shinythemes)
library(DT)
library(plotly)
SciViews::R

shinyUI(
  navbarPage(
    "Tableau de bord",


    tabPanel("Description générale",
             sidebarLayout(
               sidebarPanel(
                 imageOutput("econum")
                 ),
               mainPanel(
                 h4("Description du tableau de données"),
                 textOutput("nbr"),
                 tableOutput("tab_gen"),
                 textOutput("tuto_nbr"),
                 DTOutput("tab_mod"),
                 hr(),
                 strong("Cette application est en cours de développement
                        au sein du service d'écologie numérique des milieux
                        aquatiques de l'Université de Mons
                        dans le cadre des cours de sciences des données"),
                 imageOutput("bds")
               )
               )),
    tabPanel("Vue globale",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("nb_tuto", "Sélectionnez la représentation graphique
                             souhaitée ci-dessous",
                             choices = c("Entrées en fonction du temps",
                                         "Nombre total d'essais par quiz",
                                         "Nombre d'essais standardisés",
                                         "Score de participation par étudiants"),
                             selected = "Entrées en fonction du temps"),
                 strong("Cette application web est en cours de développement")
                 ),
               mainPanel(
                 plotOutput("bar_plot"),
                 hr(),
                 p("Après avoir analysé les différents graphiques mis à votre
                   disposition vous avez pu mettre en évidence des tutoriels
                   ayant pu poser problème aux étudiants. Vous avez à votre
                   disposition les onglets suivants (Quiz, Etudiants)
                   afin de poursuivre votre analyse.")
                 )
                 )
             ),
    navbarMenu("Quiz",
      tabPanel("Guyliann",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("tuto", "Sélectionnez le quiz souhaité ci-dessous",
                               choices = unique(sdd_dt$tutorial),
                               selected = unique(sdd_dt$tutorial)[2],
                               selectize = F),
                   strong("Cette application web est en cours de développement")),
                 mainPanel(
                   title = "Plot 1",
                   plotOutput("bar_plot_quiz"))
               )
      ),
      tabPanel("Jordan",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("tuto_lab", "Select the desired question below",
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
                   DTOutput(outputId = "u_table")
                 )
               ))

    )
    ,
        tabPanel("Etudiants",
             sidebarLayout(
               sidebarPanel(
                 selectInput("stu", "Sélectionnez le participant souhaité ci-dessous",
                             choices = unique(sdd_dt$user_name),
                             selected = unique(sdd_dt$user_name)[1],
                             selectize = F),
                 selectInput("tuto1", "Sélectionnez le quiz souhaité ci-dessous",
                             choices = unique(sdd_dt$tutorial),
                             selected = unique(sdd_dt$tutorial)[2],
                             selectize=FALSE),
                 strong("Cette application web est en cours de développement")),
               mainPanel(
                 plotOutput("bar_plot_stu"),
                 plotOutput("bar_plot_stu1")
               )
             )
             )#,
    #inverse = TRUE
))
