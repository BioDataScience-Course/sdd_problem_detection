
# Package --------
library(shiny)
library(shinythemes)
library(tidyverse)
library(flow)
library(chart)
library(data.table)
library(forcats)
library(plotly)

SciViews::R

# Import dataset -----------
sdd_dt <- data.io::read("data/sdd_wrangling.rds", type = "rds")


ord <- rev(levels(as.factor(sdd_dt$tutorial)))
sdd_dt %>.%
  filter(., label != "commentaires") %>.%
  group_by(., name) %>.%
  summarise(., student = length(unique(user_name)),
            entree = length(tuto_label), tot = length(unique(tuto_label))) -> table_nbr_question

sdd_dt1 <- filter(sdd_dt, ! is.na(correct))
sdd_dt1 <- filter(sdd_dt1, label != "commentaires")

sdd_dt1 %>.%
  group_by(., user_name, name, tuto_label) %>.%
  summarise(., result = length(tuto_label)) %>.%
  ungroup(.) %>.%
  group_by(. ,user_name, name) %>.%
  summarise(., res = length(result)) -> ttt

ttt <- left_join(ttt, table_nbr_question, by = "name")
ttt <- mutate(ttt, score = res/tot,
              score1 = pmin(res / (tot - 2), 1),
              quiz = str_extract(name, pattern = "\\w{3}"))

test <- rev(paste("student_", 1:40, sep = ""))
