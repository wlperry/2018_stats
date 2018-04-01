## ---- message=FALSE, warning=FALSE---------------------------------------

# install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")

# load the libraries each time you restart R
library(tidyverse)
library(lubridate)
library(scales)
library(skimr)
library(patchwork)

# read in the file
iris.df <- read_csv("data/iris.csv")

glimpse(iris.df)

## ------------------------------------------------------------------------
skim(iris.df)

## ------------------------------------------------------------------------
#Hitograms with GGPLOT
ggplot(data=iris.df, aes(iris.df$Sepal.Length)) + 
  geom_histogram() +
  labs (title="Sepal Length", x="Sepal Length", y="Count")

## ------------------------------------------------------------------------
#Hitograms with GGPLOT
ggplot(data=iris.df, aes(iris.df$Sepal.Length)) + 
  geom_histogram(col="blue", fill="blue", alpha=.7, binwidth = 1) +
  labs (title="Sepal Length", x="Sepal Length", y="Count")

## ------------------------------------------------------------------------
###What if you want to look at all of the species separately
ggplot(data=iris.df, aes(iris.df$Sepal.Length)) + 
  geom_histogram(binwidth = .5,  col="blue", fill="blue", alpha=.7) +
  labs (title="Sepal Length", x="Sepal Length", y="Count") + 
  facet_wrap(~Species, scales = "free") # allows the scales to be free and you can comment this out to see what happens


## ------------------------------------------------------------------------
ggplot(data=iris.df, aes(x= Species, y=Sepal.Length)) + 
  geom_dotplot(binwidth=0.1, col="blue", fill="blue", alpha=.7, stackdir = "center", binaxis = "y") +
  labs (title="Sepal Length", x="Sepal Length", y="Count") 

## ------------------------------------------------------------------------
# Box and Whisker plots with limits ####
ggplot(iris.df, aes(x = Species, y = Sepal.Length))+
  geom_boxplot() 

## ------------------------------------------------------------------------
ggplot(data=iris.df, aes(x= Species, y=Sepal.Length)) + 
  geom_violin(col="blue", fill="blue", alpha=.7, stackdir = "center", binaxis = "y") +
  labs (title="Sepal Length")+
  labs (x="Sepal Length", y="Count") 

## ------------------------------------------------------------------------

violin.plot <- ggplot(data=iris.df, aes(x= Species, y=Sepal.Length)) + 
  geom_violin(col="blue", fill="blue", alpha=.7) +
  labs (title="Sepal Length")+
  labs (x="Sepal Length", y="Count") 
violin.plot

#Save the file with sizes
ggsave("figures/violin.plot.tiff",plot=violin.plot, width=6, height=6, dpi = 300)




## ------------------------------------------------------------------------
ggplot(iris.df, aes(x=Species)) +
  geom_point(aes(y=Sepal.Length))

## ------------------------------------------------------------------------
ggplot(iris.df, aes(x=Species)) +
  geom_point(aes(y=Sepal.Length), color="blue") +
  geom_point(aes(y=Sepal.Width), color="red")

## ------------------------------------------------------------------------
ggplot(iris.df, aes(x=Species))+
  stat_summary(aes(y=Sepal.Length), fun.y=mean, geom='point', shape=23, size=3, color="red", fill="red") 

## ------------------------------------------------------------------------
ggplot(iris.df, aes(x=Species))+
  stat_summary(aes(y=Sepal.Length), fun.y=mean, geom='point', shape=23, size=3, color="red", fill="red") +
    stat_summary(aes(y=Sepal.Width), fun.y=mean, geom='point', shape=23, size=3, color="red", fill="blue") 

## ------------------------------------------------------------------------
ggplot(iris.df, aes(x=Species))+
  stat_summary(aes(y=Sepal.Length), fun.y=mean, geom='point', shape=23, size=3, color="red", fill="red") +
    stat_summary(aes(y=Sepal.Width), fun.y=mean, geom='point', shape=23, size=3, color="blue", fill="blue") +
   stat_summary(aes(y=Sepal.Length), fun.data = mean_se, geom = "errorbar", color="red", width=.1) +
    stat_summary(aes(y=Sepal.Width),fun.data = mean_se, geom = "errorbar", color="blue", width=.1) 


## ------------------------------------------------------------------------
ggplot(iris.df, aes(x=Species))+
  stat_summary(aes(y=Sepal.Length), fun.y=mean, geom='point', shape=23, size=3, color="red", fill="red") +
    stat_summary(aes(y=Sepal.Width), fun.y=mean, geom='point', shape=23, size=3, color="blue", fill="blue") +
   stat_summary(aes(y=Sepal.Length), fun.data = mean_sdl, geom = "errorbar", color="red", width=.1) +
    stat_summary(aes(y=Sepal.Width),fun.data = mean_sdl, geom = "errorbar", color="blue", width=.1) 


## ------------------------------------------------------------------------
ggplot(iris.df, aes(x=Species))+
  stat_summary(aes(y=Sepal.Length), fun.y=mean, geom='point', shape=23, size=3, color="red", 
               fill="red", position=position_nudge(x = 0.12, y = 0)) +
    stat_summary(aes(y=Sepal.Width), fun.y=mean, geom='point', shape=23, size=3, color="blue", 
                 fill="blue") +
   stat_summary(aes(y=Sepal.Length), fun.data = mean_sdl, geom = "errorbar", color="red", width=0.1,
                position=position_nudge(x = 0.12, y = 0)) +
    stat_summary(aes(y=Sepal.Width),fun.data = mean_sdl, geom = "errorbar", color="blue", width=0.1) 


