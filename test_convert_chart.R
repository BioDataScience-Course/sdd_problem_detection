#Changer les charts en plotly

#---------------------------------------------------------------------------------------------#
### Chart

sdd_dt %>.%
  filter(., user_name == "student_10") %>.%
  chart::chart(data = .,fct_relevel(tutorial, ord) ~ date %fill=% tutorial) +
  ggridges::geom_density_ridges(show.legend = F) +
  ggplot2::labs( x = "Time [month]", y = "Quiz") -> p

p

ggplotly(p)

#---------------------------------------------------------------------------------------------#
### Plotly

sdd_dt %>.%
  filter(., user_name == "student_10") -> df

ggplot(data = df, aes(x = date, y = fct_relevel(tutorial, ord), fill = tutorial )) +
  ggridges::geom_density_ridges(show.legend = F) +
  ggtitle(label = "Title", subtitle = "Subtitle") +
  labs(x = "Time [month]", y = "Quiz") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

ggplotly(pp)


#---------------------------------------------------------------------------------------------#
# plot_ly(data = df, x = ~tutorial, y = ~max_diff, type = "bar", name = "student",
#         text = text1,hoverinfo = "text+name", hoverlabel = list(bordercolor = "white", font = list(size = 18,color = "white")),marker = list(line = list(color = "rgb(8,48,107)",
#                                                                                                                                                          width = 1.5))) %>%
#
#
#   add_trace(y = ~mean_overall, name = "average", text = text2, hoverlabel = list(bordercolor = "rgb(8,48,107)", font = list(size = 18,color = "black"))) %>%
#   layout(., showlegend = TRUE,
#          xaxis = list(title = ""),
#          yaxis = list(title = "Time [min]")) %>.%
#   config(., displayModeBar = F) -> p
#
#---------------------------------------------------------------------------------------------#
library(ggplot2)
library(ggridges)

library(viridis)
d <- data.frame(x = rep(1:5, 3) + c(rep(0, 5), rep(0.3, 5), rep(0.6, 5)),
                y = c(rep(0, 5), rep(1, 5), rep(3, 5)),
                height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))

ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = list(color="red"))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016') -> t

t
