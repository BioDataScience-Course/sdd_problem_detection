library(plotly)
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
  dplyr::filter(., user_name == "student_13") -> df

df$mean_overall <- round(df$mean_overall, digits = 2)

plot_ly(
  type = "scatterpolar",
  fill = "toself"
  ) %>%
  add_trace(
    r = df$max_diff,
    theta = df$tutorial,
    name = "student"
  ) %>%
  add_trace(
    r = df$mean_overall,
    theta = df$tutorial,
    name = "average"
  )
