sdd_dt %>.%
  filter(., user_name == "student_19") %>.%
  group_by(., tuto_label, user_name ) %>.%
  summarise(., count = n() ) %>.%
  arrange(., user_name) -> df


info_tooltip = paste(" Quiz : ", df$tuto_label, "\n", "Number of attempts : ", df$count)

plot_ly(data = df, x = df$tuto_label, y = df$count,
        type = "bar", text = info_tooltip, name = "student_19",
        hoverinfo = "text+name" ) %>.%
  layout(., showlegend = FALSE,
         xaxis = list(title = "Number of attempts"),
         yaxis = list(title = "")) %>.%
  config(., displayModeBar = F)


