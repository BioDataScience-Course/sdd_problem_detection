###############################################################
library(plotly)
library(shiny)
library(shinythemes)
library(tidyverse)
library(flow)
library(chart)
library(data.table)
library(forcats)
library(lubridate)
library(plyr)

SciViews::R

# Import dataset -----------
sdd_dt <- data.io::read("data/sdd_wrangling.rds", type = "rds")

sdd_dt <- subset(sdd_dt, event != "vidéo")
###############################################################

sdd_dt %>.%
  filter(., tuto_label == "sdd1.02a_calcul1", user_name == "student_10") %>.%
  group_by(., tuto_label, user_name ) -> df

t1 <- difftime(df$date, df$date[1])
max(t1)




#=========================================================================
sdd_dt %>.%
  arrange(., date) %>.%
  group_by(., user_name, tutorial) %>.%
  mutate(., diff = difftime(date, date[1], units = "mins"))  %>.%
  filter(., diff < 20) %>.%
  mutate(., diff2 = round(diff, digits = 2)) %>.%
  mutate(., max_diff = max(diff2)) %>.%
  mutate(., mean1 = mean(df$max_diff)) %>.%
  filter(., user_name == "student_10") -> df

t %>.%
  filter(., user_name == "student_10") -> df

df %>.%
  ungroup(.) %>.%
  dplyr::select(., "mean1", "max_diff","tutorial") %>.%
  gather(., key = "factor", value = "time", max_diff:mean1) -> p

ggplot(p, aes(x = tutorial, y = time, fill = factor)) +
  geom_col(position = "dodge")



if (df$user_name == "student_10") {
  df %>.%
    dplyr::select(., user_name, tutorial, row,diff2, max_diff, mean1) %>.%
    group_by(., user_name, tutorial) %>.%
    mutate(., mean2 = max(diff2)) -> df
  df$row <- "student"


}else {
  df %>.%
    dplyr::select(., user_name, tutorial, row,diff2, max_diff, mean1) %>.%
    group_by(., user_name, tutorial) %>.%
    mutate(., mean2 = max(diff2)) -> df
  df$row <- "mean"
}


###

df %>.%
  ungroup(.) %>.%
  dplyr::select(., "mean1", "max_diff","tutorial") %>.%
  gather(., key = "test", value = "time", max_diff:mean1) -> t


ggplot(t, aes(x = tutorial, y = time, fill = test)) +
  geom_col(position = "dodge")

p
ggplotly(p)



#=============================================

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
  dplyr::filter(., user_name == "student_10") -> df

df$mean_overall <- round(df$mean_overall, digits = 2)

text1 <- paste("Tutorial : ", df$tutorial, "\nTime :", df$max_diff, " min", sep = "")
text2 <- paste("Tutorial : ", df$tutorial, "\nTime :", df$mean_overall, " min", sep = "")



plot_ly(data = df, x = ~tutorial, y = ~max_diff, type = "bar", name = "student",
        text = text1,hoverinfo = "text+name", hoverlabel = list(bordercolor = "white", font = list(size = 18,color = "white")),marker = list(line = list(color = "rgb(8,48,107)",
                                  width = 1.5))) %>%


  add_trace(y = ~mean_overall, name = "average", text = text2, hoverlabel = list(bordercolor = "rgb(8,48,107)", font = list(size = 18,color = "black"))) %>%
  layout(., showlegend = TRUE,
       xaxis = list(title = ""),
       yaxis = list(title = "Time [min]")) %>.%
  config(., displayModeBar = F) -> p

p


df %>.%
  ungroup(.) %>.%
  dplyr::filter(., user_name == "student_10") %>.%
  gather(., key = "factor", value = "time", max_diff:mean_overall) -> df








ggplot(df, aes(x = tutorial, y = time, fill = factor)) +
  geom_col(position = "dodge") +
  xlab("Tutorial") +
  ylab("Time (min)") +
  scale_fill_manual(values = c(rep(c("gray60", "#3399FF")))) +
  labs(fill = "Time of Death") +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white")) -> p
p
# ggplotly(p) %>%
#   config(displayModeBar = F) %>%






###
df <- filter(df, tutorial == "sdd1.02b")
df <- mutate(df, average = mean(max_diff))

###



#
# df %>.%
#   group_by(.,user_name, tutorial) %>%
#   summarise_at(vars(-average), funs(mean(., na.rm = TRUE))) -> ttt
dplyr::select(., "mean_overall", "max_diff","tutorial") %>.%


df %>.%
  ungroup(.) %>.%
  gather(., key = "factor", value = "time", max_diff:mean_overall) -> p

ggplot(p, aes(x = tutorial, y = time, fill = factor)) +
  geom_col(position = "dodge")


###
mutate(., mean_overall = mean(df$max_diff)) %>.%
filter(., user_name == "student_10", tutorial == "sdd1.02a") -> df
###
df_mean
group_by(df_mean, tutorial)
mutate(df_mean, mean = mean(df_mean$mean_overall))




















mainPanel(
  tabsetPanel("Subtable1",
              tabPanel(plotOutput("bar_plot_stu"),
                       br(),
                       plotlyOutput("plot3")
              ),
              tabPanel("Subtable2",
                       plotOutput("bar_plot_stu1")
              )
  )
)
