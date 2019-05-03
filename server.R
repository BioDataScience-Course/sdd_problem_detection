
# Server ---------
function(input, output) {



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
    list(src = "images/EcoNum-logo.jpg",
         filetype = "image/jpg")
  }, deleteFile = FALSE)
  output$bds <- renderImage({
    list(src = "images/BioDataScience-256.png",
         filetype = "images/png",
         width = "150px")
  }, deleteFile = FALSE)
  output$umons <- renderImage({
    list(src = "images/UMONS-logo.jpg",
         filetype = "images/jpg",
         width = "150px")
  }, deleteFile = FALSE)

  output$tab_mod <- renderDataTable({
    datatable(table_nbr_question,
              colnames = c("Quiz", "Etudiants", "Nombre d'entrées",
                           "Nombre de questions par quiz"),
              rownames = FALSE, options = list(dom = "ltip", pageLength = 5))
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
      theme(legend.position = "top")
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

  output$plot1 <- renderPlotly({
    sdd_dt %>.%
      filter(.,

             tuto_label == input$tuto_lab) %>.%
      group_by(., tuto_label, user_name ) %>.%
      summarise(., count = n() ) %>.%
      arrange(., user_name) -> df

      df <- as.data.frame(table(df$count)) %>.%
        mutate(., pct = (Freq/sum(Freq) )*100, pct = as.integer(pct),
               count = Var1) -> df

      # Choix de l'unité
      yvar = df$pct
      if ("percentage (%)" %in% input$ui_quiz_unit) {
        yvar = df$pct
        y_axis_name = "Percentage"
        info_tooltip = paste(df$pct, "%", sep = "")
      }
      if ("number of students" %in% input$ui_quiz_unit) {
        yvar = df$Freq
        y_axis_name = "Number of students"
        info_tooltip = df$Freq
      }

      plot_ly(data = df, x = df$Var1, y = yvar,
                   type = "bar", text = info_tooltip,
                   hoverinfo = "text") %>.%
        layout(., showlegend = FALSE,
                  xaxis = list(title = "Attempt"),
                  yaxis = list(title = y_axis_name)) %>.%
        config(., displayModeBar = F)
  })

  output$u_table <- renderDT({
    sdd_dt %>.%
      filter(., tuto_label == input$tuto_lab) %>.%
      group_by(., tuto_label, user_name ) %>.%
      summarise(., count = n() ) %>.%
      arrange(., user_name)  %>.%
      ungroup(.) %>.%
      mutate(., Var1 = as.factor(count)) %>.%
      dplyr::select(., user_name, count) -> df

    #Permet de faire un spread :
    #Voir : https://github.com/tidyverse/tidyr/issues/426
    df %>%
      group_by_at(vars(-user_name)) %>%
      mutate(row_id = 1:n()) %>% ungroup() %>%
      spread(count,user_name) %>%
      dplyr::select(-row_id) -> df

    datatable(df, options = list( dom = "t",
                                  initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")))
  })

}


