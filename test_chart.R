#test_chart
source("global.R")


chart::chart(data = sdd_dt,
             fct_relevel(tutorial, ord) ~ date %fill=% tutorial) +
  ggridges::geom_density_ridges(show.legend = F) +
  labs( x = "Time [month]", y = "Quiz") +
  theme( plot.caption = element_text(size = 14))

