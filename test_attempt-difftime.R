library(plotly)
library(flow)
sdd_dt %>.%
  dplyr::select(., date, user_name, tuto_label) %>.%
  dplyr::arrange(., date) %>.%
  group_by(., user_name, tuto_label) %>.%
  dplyr::mutate(., diff = difftime(date, date[1], units = "secs"))  %>.%
  dplyr::filter(., diff < 120) %>.%
  dplyr::mutate(., diff = round(diff, digits = 2)) %>.%
  dplyr::mutate(., max_diff = max(diff)) %>.%
  dplyr::select(., tuto_label, user_name, max_diff) -> df

aggregate(df$max_diff, list(df$user_name, df$tuto_label), mean) %>%
  dplyr::rename(
    user_name = "Group.1",
    tuto_label = "Group.2",
    max_diff = x
  ) -> df

plyr::ddply(df, .(tuto_label), summarize, mean_diff = mean(max_diff)) -> df_mean

merge(df, df_mean,  by =  "tuto_label", all.y = TRUE)-> df

df$mean_diff <- round(df$mean_diff, digits = 2)
df_diff <- df
###################
sdd_dt %>.%
  dplyr::select(., user_name, tuto_label) %>.%
  group_by(., user_name, tuto_label) %>.%
  summarise(., count = n() ) %>.%
  arrange(., tuto_label) -> df

plyr::ddply(df, .(tuto_label), summarize, mean_attempt = mean(count)) -> df_mean

merge(df, df_mean,  by =  "tuto_label", all.y = TRUE) -> df

df$mean_attempt <- round(df$mean_attempt, digits = 2)
df_attempt <- df



merge(df_attempt, df_diff, by = c("tuto_label", "user_name")) -> toto

toto$max_diff <-  as.numeric(toto$max_diff)
toto <- dplyr::filter(toto, tuto_label == "sdd1.02a_calcul2")
ggplot(toto, aes(x = max_diff, y = count), color = user_name, shape = user_name) +
  geom_point() ->p
ggplotly(p)

