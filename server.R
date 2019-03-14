
# Package --------
library(shiny)
library(shinythemes)
library(tidyverse)
library(flow)
library(chart)
library(data.table)
library(forcats)

SciViews::R

# Import dataset -----------
sdd_dt <- data.io::read("data/sdd_wrangling.rds", type = "rds")

# Server ---------
shinyServer(function(input, output) {

  ord <- rev(levels(sdd_dt$tutorial))
  sdd_dt %>.%
    filter(., label != "commentaires") %>.%
    group_by(., name) %>.%
    summarise(., student = length(unique(user_name)),
              entree = length(tuto_label), tot = length(unique(tuto_label))) -> t

  sdd_dt1 <- filter(sdd_dt, ! is.na(correct))
  sdd_dt1 <- filter(sdd_dt1, label != "commentaires")

  sdd_dt1 %>.%
    group_by(., user_name, name, tuto_label) %>.%
    summarise(., result = length(tuto_label)) %>.%
    ungroup(.) %>.%
    group_by(. ,user_name, name) %>.%
    summarise(., res = length(result)) -> ttt

  ttt <- left_join(ttt, t, by = "name")
  ttt <- mutate(ttt, score = res/tot,
                score1 = pmin(res / (tot - 2), 1),
                quiz = str_extract(name, pattern = "\\w{3}"))

  test <- rev(paste("student_", 1:40, sep = ""))

  output$nbr <- renderText({
    paste("En ce", format(Sys.time(), "%d %B %Y %X"),
          ", la base de données comprend : \n")
  })

  output$tab_gen <- renderTable({
    tibble::data_frame("Entrées" = nrow(sdd_dt),
               "Quiz" = length(unique(sdd_dt$tutorial)),
               "Etudiants" = length(unique(sdd_dt$user_name)))
  })

  output$tuto_nbr <- renderText({
    paste("La base de données comprend", length(unique(sdd_dt$tutorial)),
          "tutoriels qui sont répartis au sein des différents modules du
           cours de la science des données I : visualisation et inférence.")
  })

  output$econum <- renderImage({
    list(src = "images/EcoNum-logo.pdf",
         filetype = "image/pdf")
  }, deleteFile = FALSE)
  output$bds <- renderImage({
    list(src = "images/BioDataScience-256.png",
         filetype = "image/png",
         width = "150px")
  }, deleteFile = FALSE)
  output$umons <- renderImage({
    list(src = "images/UMONS-logo.pdf",
         filetype = "image/pdf",
         width = "150px")
  }, deleteFile = FALSE)

  output$tab_mod <- renderDataTable({
    datatable(t, colnames = c("Quiz", "Etudiants", "Nombre d'entrées",
                              "Nombre de questions par quiz"),
              rownames = FALSE, options = list(pageLength = 5))
      })

  output$bar_plot <- renderPlot({
    if (input$nb_tuto == "Entrées en fonction du temps") {
      chart::chart(data = sdd_dt,
                   fct_relevel(tutorial, ord) ~ date %fill=% tutorial) +
        ggridges::geom_density_ridges(show.legend = F) +
        labs( x = "Temps [mois]", y = "Quiz",
                       caption = "Succession de graphes de densité des entrées
                       au cours du temps par quiz") +
        theme( plot.caption = element_text(size = 14))
    } else if (input$nb_tuto == "Nombre total d'essais par quiz") {
      chart::chart(data = sdd_dt, ~ fct_relevel(tutorial, ord) %fill=% tutorial) +
        geom_bar(show.legend = F) +
        coord_flip() +
        labs(x = "Quiz", y = "Nombre d'essais",
             caption = "Nombre d'essais par quiz") +
        theme( plot.caption = element_text(size = 14))
    } else if (input$nb_tuto == "Nombre d'essais standardisés") {
      sdd_dt %>.%
        group_by(., tutorial) %>.%
        summarise(., n_tot =  length(unique(label)), n = length(label),
                  ratio = n/n_tot) %>.%
        chart::chart(data = ., ratio ~ fct_relevel(tutorial, ord) %fill=% tutorial) +
        geom_col(show.legend = F) +
        coord_flip() +
        labs(x = "Quiz", y = "Nombre d'essais standardisés",
             caption = "Nombre d'essais standardisés par le nombre
                        de questions par quiz") +
        theme(plot.caption = element_text(size = 14))
    } else {
      chart(ttt, fct_relevel(user_name, test) ~ quiz %fill=% score1) +
        geom_raster() +
        xlab("") +
        ylab("") +
        geom_hline(yintercept = (0:length(levels(ttt$user_name))) + 0.5) +
        geom_vline(xintercept = (0:length(unique(ttt$name))) + 0.5) +
        scale_fill_distiller(palette = "RdBu", direction = 1) +
        labs(caption = "Le score est le ratio de réponses soumises sur le nombre
                        total de réponses par quiz", fill = "Score") +
        theme(plot.caption = element_text(size = 14))
    }
  })

  output$bar_plot_quiz <- renderPlot({
    sdd_dt %>.%
      filter(., tutorial == input$tuto) %>.%
      chart::chart(., ~ label %fill=% event) +
      ggplot2::geom_bar() +
      ggplot2::labs( x = "Questions", y = "Nombre d'essais",
                     fill = "Evénements") +
      ggplot2::coord_flip() +
      theme(legend.position ="top")
  })

  output$bar_plot_stu <- renderPlot({
    sdd_dt %>.%
      filter(., user_name == input$stu) %>.%
      chart::chart(data = .,fct_relevel(tutorial, ord) ~ date %fill=% tutorial) +
      ggridges::geom_density_ridges(show.legend = F) +
      ggplot2::labs( x = "Temps [mois]", y = "Quiz")
  })

  output$bar_plot_stu1 <- renderPlot({
    sdd_dt %>.%
      filter(., user_name == input$stu) %>.%
      filter(., tutorial == input$tuto1) %>.%
      chart::chart(., ~ label %fill=% event) +
      ggplot2::geom_bar() +
      ggplot2::labs( x = "Questions", y = "Nombre d'essais",
                     fill = "Evénements") +
      ggplot2::coord_flip() +
      theme(legend.position = "top")
  })
}
)
