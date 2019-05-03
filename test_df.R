

sdd_dt %>.%
  filter(., tuto_label == "sdd1.02a_assig1") %>.%
  group_by(., tuto_label, user_name ) %>.%
  summarise(., count = n() ) -> df

  arrange(df, user_name)  %>.%
  ungroup(.) %>.%
  mutate(., Var1 = as.factor(count)) %>.%
  dplyr::select(., user_name, count) -> df

df %>%
  group_by_at(vars(-user_name)) %>%
  mutate(row_id=1:n()) %>% ungroup() %>%
  spread(count,user_name) %>%
  dplyr::select(-row_id) -> df


#
# sdd_dt %>.%
# filter(., tuto_label == "sdd1.02a_assig1") %>.%
#   group_by(., tuto_label, user_name ) %>.%
#   summarise(., count = n() ) %>.%
#   arrange(., user_name) -> df
#
# df <- as.data.frame(table(df$count)) %>.%
#   mutate(., pct = (Freq/sum(Freq) )*100, pct = as.integer(pct),
#          count = Var1) -> df
