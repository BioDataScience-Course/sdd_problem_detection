
# Server ---------
function(input, output) {



  output$nbr <- renderText({
    paste("On this", format(Sys.time(), "%d %B %Y %X"),
          ", the database includes : \n")
  })

  output$tab_gen <- renderTable({
    tibble::data_frame("Submission" = nrow(sdd_dt),
               "Quiz" = length(unique(sdd_dt$tutorial)),
               "Students" = length(unique(sdd_dt$user_name)))
  })

  output$tuto_nbr <- renderText({
    paste("The database includes", length(unique(sdd_dt$tutorial)),
          "tutorials that are distributed within the different modules of the data science course I : visualization and inference.")
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
              colnames = c("Quiz", "Students", "Number of submission",
                           "Number of question per quiz"),
              rownames = FALSE, options = list(dom = "ltip", pageLength = 5))
      })

  output$u_caption_bar_plot <- renderText({
    if (input$nb_tuto == "Submission by time") {
    "Succession of input density graphs over time by quiz"
    }
    else if (input$nb_tuto == "Total number of attempts per quiz") {
      "Number of attempts per quiz"
    }
    else if (input$nb_tuto == "Number of standardized attempts") {
      "Number of attempts standardized by the number of questions per quiz"
    }

  })

  output$bar_plot <- renderPlot({
    if (input$nb_tuto == "Submission by time") {
      chart::chart(data = sdd_dt,
                   fct_relevel(tutorial, ord) ~ date %fill=% tutorial) +
        ggridges::geom_density_ridges(show.legend = F) +
        labs( x = "Time [month]", y = "Quiz",
                       caption = "Succession of input density graphs over time by quiz") +
        theme( plot.caption = element_text(size = 14))
    } else if (input$nb_tuto == "Total number of attempts per quiz") {
      chart::chart(data = sdd_dt, ~ fct_relevel(tutorial, ord) %fill=% tutorial) +
        geom_bar(show.legend = F) +
        coord_flip() +
        labs(x = "Quiz", y = "Number of attempts",
             caption = "Number of attempts per quiz") +
        theme( plot.caption = element_text(size = 14))
    } else if (input$nb_tuto == "Number of standardized attempts") {
      sdd_dt %>.%
        group_by(., tutorial) %>.%
        summarise(., n_tot =  length(unique(label)), n = length(label),
                  ratio = n/n_tot) %>.%
        chart::chart(data = ., ratio ~ fct_relevel(tutorial, ord) %fill=% tutorial) +
        geom_col(show.legend = F) +
        coord_flip() +
        labs(x = "Quiz", y = "Number of standardized",
             caption = "Number of attempts standardized by the number of questions per quiz") +
        theme(plot.caption = element_text(size = 14))
    }
  })

  output$bar_plot_quiz <- renderPlot({
    sdd_dt %>.%
      filter(., tutorial == input$tuto) %>.%
      chart::chart(., ~ label %fill=% event) +
      ggplot2::geom_bar() +
      ggplot2::labs( x = "Questions", y = "Number of attempts",
                     fill = "Events") +
      ggplot2::coord_flip() +
      theme(legend.position = "top")
  })

  output$bar_plot_stu <- renderPlot({
    sdd_dt %>.%
      filter(., user_name == input$stu) %>.%
      ggplot(., aes(x = date, y = fct_relevel(tutorial, ord), fill = tutorial )) +
      ggridges::geom_density_ridges(show.legend = F) +

      # Personnalisation graphique
      ggtitle(label = "Title", subtitle = "Subtitle") +
      labs(x = "Time [month]", y = "Quiz") +
      theme(panel.background = element_rect(fill = "white", colour = "white"))+
      theme(axis.title.x = element_text(family = "Canonical", face = "plain", colour = "black", size = "20")) +
      theme(axis.title.y = element_text(family = "Canonical", face = "plain", colour = "black", size = "8")) +
      theme(
        plot.title = element_text(color = "red", size = 12, face = "bold"),
        plot.caption = element_text(color = "red", size = 12, face = "bold"))

  })

  output$bar_plot_stu1 <- renderPlot({
    sdd_dt %>.%
      filter(., user_name == input$stu) %>.%
      filter(., tutorial == input$tuto1) %>.%
      chart::chart(., ~ label %fill=% event) +
      ggplot2::geom_bar() +
      ggplot2::labs( x = "Questions", y = "Number of attempts",
                     fill = "Events") +
      ggplot2::coord_flip() +
      theme(legend.position = "top")
  })

# Toutes les questions par questionnaire
  output$bar_plot_stu2 <- renderPlot({

      sdd_dt %>.%
      dplyr::filter(., tutorial == input$u_selectinput_tuto_stu2) %>.%
      dplyr::select(., date, user_name, tuto_label) %>.%
      dplyr::arrange(., date) %>.%
      group_by(., user_name, tuto_label) %>.%
      dplyr::mutate(., diff = difftime(date, date[1], units = "mins"))  %>.%
      dplyr::filter(., diff < 20) %>.%
      dplyr::mutate(., diff = round(diff, digits = 2)) %>.%
      dplyr::mutate(., max_diff = max(diff)) %>.%
      dplyr::select(., tuto_label, user_name, max_diff) -> df

    aggregate(df$max_diff, list(df$user_name, df$tuto_label), mean) %>%
      dplyr::rename(
        user_name = "Group.1",
        tuto_label = "Group.2",
        max_diff = x
      ) -> df

    plyr::ddply(df, .(tuto_label), summarize, mean_overall = mean(max_diff)) -> df_mean

    merge(df, df_mean,  by =  "tuto_label", all.y = TRUE) %>.%
      dplyr::filter(., user_name == input$u_selectinput_student_stu2) -> df

    df$mean_overall <- round(df$mean_overall, digits = 2)

    text1 <- paste("Quiz : ", df$tuto_label, "\nTime : ", df$max_diff, " min", sep = "")
    text2 <- paste("Quiz : ", df$tuto_label, "\nTime : ", df$mean_overall, " min", sep = "")

    plot_ly(data = df, x = ~tuto_label, y = ~max_diff, type = "bar", name = "student",
            text = text1,
            hoverinfo = "text+name",
            hoverlabel = list(bordercolor = "white", font = list(size = 18,color = "white")),
            marker = list(line = list(color = "rgb(8,48,107)", width = 1.5))) %>%
      add_trace(y = ~mean_overall, name = "average", text = text2, hoverlabel = list(bordercolor = "rgb(8,48,107)", font = list(size = 18,color = "black"))) %>%
      layout(., title = input$u_selectinput_student_stu2, showlegend = TRUE,
             xaxis = list(title = ""),
             yaxis = list(title = "Time [min]")) %>.%
      config(., displayModeBar = F)


  })

#Toutes les questions
  output$bar_plot_stu2.2 <- renderPlot({

    sdd_dt %>.%
      dplyr::select(., date, user_name, tuto_label) %>.%
      dplyr::arrange(., date) %>.%
      group_by(., user_name, tuto_label) %>.%
      dplyr::mutate(., diff = difftime(date, date[1], units = "mins"))  %>.%
      dplyr::filter(., diff < 20) %>.%
      dplyr::mutate(., diff = round(diff, digits = 2)) %>.%
      dplyr::mutate(., max_diff = max(diff)) %>.%
      dplyr::select(., tuto_label, user_name, max_diff) -> df

    aggregate(df$max_diff, list(df$user_name, df$tuto_label), mean) %>%
      dplyr::rename(
        user_name = "Group.1",
        tuto_label = "Group.2",
        max_diff = x
      ) -> df

    plyr::ddply(df, .(tuto_label), summarize, mean_overall = mean(max_diff)) -> df_mean

    merge(df, df_mean,  by =  "tuto_label", all.y = TRUE) %>.%
      dplyr::filter(., user_name == input$u_selectinput_student_stu2) -> df

    df$mean_overall <- round(df$mean_overall, digits = 2)

    text1 <- paste("Quiz : ", df$tuto_label, "\nTime : ", df$max_diff, " min", sep = "")
    text2 <- paste("Quiz : ", df$tuto_label, "\nTime : ", df$mean_overall, " min", sep = "")

    plot_ly(data = df, x = ~tuto_label, y = ~max_diff, type = "bar", name = "student",
            text = text1,
            hoverinfo = "text+name",
            hoverlabel = list(bordercolor = "white", font = list(size = 18,color = "white")),
            marker = list(line = list(color = "rgb(8,48,107)", width = 1.5))) %>%
      add_trace(y = ~mean_overall, name = "average", text = text2, hoverlabel = list(bordercolor = "rgb(8,48,107)", font = list(size = 18,color = "black"))) %>%
      layout(., title = input$u_selectinput_student_stu2, showlegend = TRUE,
             xaxis = list(title = ""),
             yaxis = list(title = "Time [min]")) %>.%
      config(., displayModeBar = F)


  })


  output$u_selectinput_quiz_stu2 <- renderUI({
    sdd_dt %>.%
      filter(., tutorial == input$u_selectinput_tuto_stu2) -> sdd_dt
    selectInput("s_selectinput_quiz_stu2", "Select the desired quiz below",
                choices = unique(sort(sdd_dt$tuto_label)),
                selected = unique(sort(sdd_dt$tuto_label))[1])

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
                  xaxis = list(title = "Number of attempts"),
                  yaxis = list(title = y_axis_name)) %>.%
        config(., displayModeBar = F)
  })

  output$plot3 <- renderPlotly({

    sdd_dt %>.%
      dplyr::select(., date, user_name, tutorial) %>.%
      dplyr::arrange(., date) %>.%
      group_by(., user_name, tutorial) %>.%
      dplyr::mutate(., diff = difftime(date, date[1], units = "mins"))  %>.%
      dplyr::filter(., diff < 120) %>.%
      dplyr::mutate(., diff = round(diff, digits = 2)) %>.%
      dplyr::mutate(., max_diff = max(diff)) %>.%
      dplyr::select(., tutorial, user_name, max_diff) -> df

    aggregate(df$max_diff, list(df$user_name, df$tutorial), mean) %>%
      dplyr::rename(
        user_name = "Group.1",
        tutorial = "Group.2",
        max_diff = x
      ) -> df

    plyr::ddply(df, .(tutorial), summarize, mean_overall = mean(max_diff)) -> df_mean

    merge(df, df_mean,  by =  "tutorial", all.y = TRUE) %>.%
      dplyr::filter(., user_name == input$stu) -> df

    df$mean_overall <- round(df$mean_overall, digits = 2)

    text1 <- paste("Tutorial : ", df$tutorial, "\nTime : ", df$max_diff, " min", sep = "")
    text2 <- paste("Tutorial : ", df$tutorial, "\nTime : ", df$mean_overall, " min", sep = "")

    plot_ly(data = df, x = ~tutorial, y = ~max_diff, type = "bar", name = "student",
            text = text1,
            hoverinfo = "text+name",
            hoverlabel = list(bordercolor = "white", font = list(size = 18,color = "white")),
            marker = list(line = list(color = "rgb(8,48,107)", width = 1.5))) %>%
      add_trace(y = ~mean_overall, name = "average", text = text2, hoverlabel = list(bordercolor = "rgb(8,48,107)", font = list(size = 18,color = "black"))) %>%
      layout(., title = input$stu, showlegend = TRUE,
             xaxis = list(title = ""),
             yaxis = list(title = "Time [min]")) %>.%
      config(., displayModeBar = F)

  })

  output$plot4 <- renderPlotly({

    sdd_dt %>.%
      filter(., user_name == input$stu) %>.%
      group_by(., tuto_label, user_name ) %>.%
      summarise(., count = n() ) %>.%
      arrange(., user_name) -> df


    info_tooltip = paste(" Quiz : ", df$tuto_label, "\n", "Number of attempts : ", df$count)

    plot_ly(data = df, x = df$tuto_label, y = df$count,
            type = "bar", text = info_tooltip, name = input$stu,
            hoverinfo = "text+name" ) %>.%
      layout(., title = input$stu, showlegend = FALSE,
             xaxis = list(title = ""),
             yaxis = list(title = "Number of attempts")) %>.%
      config(., displayModeBar = F)



  })

  output$u_quiz_table1 <- renderDT({

    # Sous-onglet 1

    # Création colonne "count" = nombre de tentative
    sdd_dt %>.%
      filter(., tuto_label == input$tuto_lab) %>.%
      group_by(., tuto_label, user_name ) %>.%
      summarise(., count = n() ) %>.%
      arrange(., user_name)  %>.%
      ungroup(.) %>.%
      mutate(., Var1 = as.factor(count)) %>.%
      dplyr::select(., user_name, count) -> df_count

    #Permet de faire un spread
    #Voir : https://github.com/tidyverse/tidyr/issues/426
    df_count %>%
      group_by_at(vars(-user_name)) %>%
      mutate(row_id = 1:n()) %>% ungroup() %>%
      spread(count,user_name) %>%
      dplyr::select(-row_id) -> df_attempt


    datatable(df_attempt, options = list( dom = "t",
                                  initComplete = JS( # javascript pour modifier l'apparence
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"
      )))
  })

  output$u_quiz_table2 <- renderDT({

    # Sous-onglet 2
    filter(sdd_dt, tuto_label == input$tuto_lab) -> sdd_dt

    # Création colonne "count" = nombre de tentative
    sdd_dt %>.%
      group_by(., tuto_label, user_name ) %>.%
      summarise(., count = n() ) %>.%
      arrange(., user_name)  %>.%
      ungroup(.) %>.%
      mutate(., Var1 = as.factor(count)) %>.%
      dplyr::select(., user_name, count) -> df_count


    # Information à fusionner
    sdd_dt %>.%
      dplyr::select(., user_name, data_conv, date, event) -> df_tuto_label

    # Fusion
    df_join <- merge(df_count, df_tuto_label , by = "user_name", all = TRUE)
    df_join <- arrange(df_join, count, user_name)

    datatable(df_join, options = list( dom = "ltp",
                                       initComplete = JS(
                                         "function(settings, json) {",
                                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                         "}"
                                         )
                                       ),
              rownames = FALSE, filter = "top" )
  })

  output$u_global_score <- renderPlot(
    chart(ttt, fct_relevel(user_name, test) ~ quiz %fill=% score1) +
      geom_raster() +
      xlab("") +
      ylab("") +
      geom_hline(yintercept = (0:length(levels(ttt$user_name))) + 0.5) +
      geom_vline(xintercept = (0:length(unique(ttt$name))) + 0.5) +
      scale_fill_distiller(palette = "RdBu", direction = 1) +
      labs(caption = "The score is the ratio of submitted responses to the total number of responses per quiz", fill = "Score") +
      theme(plot.caption = element_text(size = 14))
  )

  output$u_stu_name <- renderText({
    student_name <- input$stu

    paste("<b>Student selected : </b> ", student_name, sep = "<br>")
  })

  output$u_selected_tutorial <- renderText({
    tutorial_selected <- input$tuto_lab

    paste("<b>Tutorial selected : </b> ", tutorial_selected, sep = "<br>")

  })
}


