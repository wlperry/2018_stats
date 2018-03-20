# this file is intendend to introduce you to ggplot in R

# load the libraries each time you restart R	
library(tidyverse)	
library(lubridate)	
library(scales)	
library(readxl)
library(skimr)	
	
#' read in the csv file	
#' note here we are using relative file paths to find the file	
#' try hitting "" and then tab inside of the "" - this will bring up choices	
iris.df <- read_csv("data/iris.csv")	
	
#'Lets read in the iris excel file
#'The only thing that you might need to do is specify the sheet if you use 
#'multiple sheets
#'

iris_excel.df <- read_excel("data/iris_excel.xlsx", sheet="iris_excel")	# if there is only one sheet - no worries


#' ##View Data     	
#' Look at the data with glimpse	
glimpse(iris.df)	
 	
#' Look at the top or bottle of the data	
#' you can also look at the top or `head` or bottom `tail` of the dataframe
#' what would that look like?


#' This is the start of how to make a plot in ggplot
ggplot(data=iris.df, aes(x=Petal.Length, y=Petal.Width)) +	
  geom_point()	


#' This is the same as the above plot but puts the aesthetics in the geometry
ggplot(data=iris.df) +	
  geom_point(aes(x=Petal.Length, y=Petal.Width))	



#' What is the advantage of doing the above method? 
#' How could you add a set of points for `Sepal.Length` and `Sepal.Width` to the 
#' existing graph
ggplot(data=iris.df) +	
  geom_point(aes(x=Petal.Length, y=Petal.Width))	




#' ##Colors of points that are all the same     	
ggplot(data=iris.df, aes(x=Petal.Length, y=Petal.Width)) +	
  geom_point(color="blue")	

#' Map the color to species	categories
ggplot(data=iris.df, aes(x=Petal.Length, y=Petal.Width, color=Species)) +	
  geom_point()	

#' size of points
ggplot(data=iris.df, aes(x=Petal.Length, y=Petal.Width, color=Species)) +	
  geom_point(size=3)	




#' ##Size of points based on Petal Width and color based on species 	
ggplot(data=iris.df, aes(x=Petal.Length, y=Petal.Width, size=Petal.Width, color=Species)) +	
  geom_point()	
 	


#' ##Shapes of Points    	
ggplot(data=iris.df, aes(x=Petal.Length, y=Petal.Width, shape=Species, color=Species)) +	
  geom_point(size=3)	
	


#' What is intersting to do is to make multiple plots based on one of the grouping 
#' characters to see the distribution of points in each category
#' ## Facetting 	
#' Now the cool thing to be able to look at data is to be able to facet or look at subplots of all of the data at once	
ggplot(data=iris.df) +	
  geom_point(aes(x=Petal.Length, y=Petal.Width, shape=Species, color=Species)) +	
  facet_grid(. ~ Species)	
 	
ggplot(data=iris.df) +	
  geom_point(aes(x=Petal.Length, y=Petal.Width, shape=Species, color=Species)) +	
  facet_grid(Species ~ ., scales = "free") 	

#' How do the above gaphs works - not that there is always one axis that the scales are the same
#' We can use Facet wrap to get around this a bit easier.  
	
#' Facet wrap is slightly different and will allow you to have all free scales and facet by species	
ggplot(data=iris.df, aes(x=Petal.Length, y=Petal.Width, shape=Species, color=Species)) +	
  geom_point() +	
  facet_wrap("Species", scales = "free")	

	
#' ## Adding Titles      	
#' Now to add in some of the basic things you might want to have on there to make 
#' it look nicer. We will work more on this later to get it publication quality 
#' but this is a start.  	
ggplot(data=iris.df, aes(x=Petal.Length, y=Petal.Width, shape=Species, color=Species)) +	
  geom_point() +	
  ggtitle("Petal Shape") +	
  labs(x="Petal Length", y="Petal Width")	

 	
#' ##Time series plots    	
streams.df <- read_csv("data/streams_q_gage_no3.csv", guess_max = 10000)	
	
glimpse(streams.df)	


#' ##Basic time series plot	
ggplot(streams.df, aes(datetime, no3_mgl, color=site)) +	
  geom_point()	

#' ##Scales - time series	
ggplot(streams.df, aes(datetime, no3_mgl, color=site)) +	
  geom_point() +	
  scale_y_continuous()+ #expand=c(0,0), breaks=c(0,30,60)	
     scale_x_datetime(date_breaks = "1 month",	
                  limits = ymd_hms(c('2017-01-01 00:00:00','2017-12-31 00:00:00')),	
                  labels=date_format("%Y-%b-%d"), expand=c(0,0))	

#' ##Themes	
ggplot(streams.df, aes(datetime, no3_mgl, color=site)) +	
  geom_point() +	
  scale_y_continuous()+ #expand=c(0,0), breaks=c(0,30,60)	
     scale_x_datetime(date_breaks = "1 month",	
                  limits = ymd_hms(c('2017-01-01 00:00:00','2017-12-31 00:00:00')),	
                  labels=date_format("%Y-%b-%d"), expand=c(0,0)) +	
  theme_bw()	


#' ##Theme - adjust text	
ggplot(streams.df, aes(datetime, no3_mgl, color=site)) +	
  geom_point() +	
  scale_y_continuous()+ #expand=c(0,0), breaks=c(0,30,60)	
     scale_x_datetime(date_breaks = "1 month",	
                  limits = ymd_hms(c('2017-01-01 00:00:00','2017-12-31 00:00:00')),	
                  labels=date_format("%Y-%b-%d"), expand=c(0,0)) +	
  theme(	
       # LABLES APPEARANCE	
       axis.title.x=element_text(size=12, face="bold"),	
       axis.title.y=element_text(size=12, face="bold"),	
       axis.text.x = element_text(size=10, face="bold", angle=45, hjust=1),	
       axis.text.y = element_text(size=10, face="bold"))	

	
#' ##Final time series plot	
#' So lets make all the changes to the graph that we might want to do	
ggplot(streams.df, aes(datetime, no3_mgl, color=site)) +	
  geom_point() +	
  scale_y_continuous()+ #expand=c(0,0), breaks=c(0,30,60)	
     scale_x_datetime(date_breaks = "1 month",	
                  limits = ymd_hms(c('2017-01-01 00:00:00','2017-12-31 00:00:00')),	
                  labels=date_format("%Y-%b-%d"), expand=c(0,0)) +	
  scale_color_manual (name="Sites",	
                      labels=c("Indian Cr.", "Vermillion Cr."),	
                      values=c("red","darkblue")) +	
  labs(x = "", y = "Nitrate NO3 (mg L^-1") +	
theme(	
    # LABLES APPEARANCE	
    axis.title.x=element_text(size=12, face="bold"),	
    axis.title.y=element_text(size=12, face="bold"),	
    axis.text.x = element_text(size=10, face="bold", angle=45, hjust=1),	
    axis.text.y = element_text(size=10, face="bold"),	
 # plot.title = element_text(hjust = 0.5, colour="black", size=22, face="bold"),	
 # LEGEND	
 # legend.position="none",	
 # LEGEND TEXT	
 legend.text = element_text(colour="black", size = 12, face = "bold"),	
 # LEGEND TITLE	
 legend.title = element_text(colour="black", size=14, face="bold"),	
 # LEGEND POSITION AND JUSTIFICATION	
 # legend.justification=c(0.1,1),	
 # legend.position=c(0.02,.99),	
 # PLOT COLORS	
 # REMOVE BOX BEHIND LEGEND SYMBOLS	
 # REMOVE LEGEND BOX	
  legend.key = element_rect(fill = "transparent", colour = "transparent"),	
 # REMOVE LEGEND BOX	
  legend.background = element_rect(fill = "transparent", colour = "transparent"),	
 # #REMOVE PLOT FILL AND GRIDS	
 # panel.background=element_rect(fill = "transparent", colour = "transparent"),	
 # # removes the window background	
 # plot.background=element_rect(fill="transparent",colour=NA),	
 # # removes the grid lines	
 # panel.grid.major = element_blank(),	
 # panel.grid.minor = element_blank(),	
 # ADD AXES LINES AND SIZE	
 axis.line.x = element_line(color="black", size = 0.3),	
 axis.line.y = element_line(color="black", size = 0.3),	
 # ADD PLOT BOX	
 panel.border = element_rect(colour = "black", fill=NA, size=0.3))	
