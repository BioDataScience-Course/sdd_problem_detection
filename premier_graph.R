# Graph : x = nombre d'essai, y = fréquence par student

library(flow)
library(dplyr)
library(data.table)
library(ggplot2)


sdd_dt <- data.io::read("data/sdd_wrangling.rds", type = "rds")

sdd_dt %>.%
  group_by(., tuto_label, user_name ) %>.%
  summarise(., count = n() ) %>.%
  filter(., tuto_label == "sdd1.02a_assig1") %>.%
  arrange(., user_name)  %>.%
  ungroup(.) -> df


df1 <- mutate(df, Var1 = as.factor(count))

df <- as.data.frame(table(df$count)) %>.%
  mutate(., pct = (Freq/sum(Freq) )*100, pct = as.integer(pct),
         count = Var1) -> df


#
# df_join <- full_join(df1, df, by = c("Var1"))
#
# df_join <- mutate(df_join, pct2 = (count/sum(count))*100 )
#



group_by(df1, count) %>.%
  + arrange(., count)

df1 <- mutate(df, Var1 = as.factor(count))
df3 <- df1
df3 <- df3[, c(-1,-4)]

df3 %>%
  + group_by_at(vars(-user_name)) %>%
  + mutate(row_id=1:n()) %>% ungroup() %>%
  + spread(count,user_name) %>%
  + select(-row_id) -> df3







ggplot(df, aes(x = count, y = pct)) +
  geom_col() +
  scale_x_discrete(limits = df$count) +
  xlab("Nombre d'essais") +
  ylab("Pourcentage d'étudiant")



