library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(fmsb)
library(colormap)

# Création des data
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

##
radarchart(df)

# Create data
set.seed(1)
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)

# Custom the radarChart !
par(mar=c(0,0,0,0))
radarchart( data, axistype=1
)

set.seed(1)
data <-as.data.frame(matrix( c( sample( 2:20 , 10 , replace=T), sample( 2:9 , 10 , replace=T)) , ncol=10, byrow=TRUE))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )
data[2,2]=19

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <-rbind(rep(20,10) , rep(0,10) , data)

# Prepare color
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9)  )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4)  )

# Custom the radarChart !
radarchart( data, axistype=1,

  #custom polygon
  pcol=colors_border , pfcol=colors_in , plwd=4, plty=1 ,

  #custom the grid
  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=1.1,

  #custom labels
  vlcex=0.8
)

# Legend
legend(x=0.85, y=1, legend = c("Shirley", "Sonia"), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=0.9, pt.cex=1.6)
