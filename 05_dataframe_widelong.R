#' ---	
#' title: "Data Modification"	
#' author: "Bill Perry"	
#' date: "2018/03/14"	
#' output:	
#'   html_document:	
#'     toc: true	
#'     toc_float: true	
#' ---	
#' ## Dataframe wide to long and long to wide 	
#' I seem to always have to go back and read how to do this 
#' over and over so I am trying to get it down here so I can use 
#' this as a reference and others might find it helpful.	
#' 	
#' Wide format is how you would usually enter data in excel with 
#' treatment down the left had side or some other category and 
#' variables for that treatment along the top row. This if a fine 
#' way to enter data but is not often that useful in R and you need 
#' to switch it to long format. I will post examples soon	
#' 	
#' 	
#' ##Load libraries	
#' We will read in the main files and load the libraries as 
#' we have worked with so far.  I will do the iris.df first 
#' and then go to others that may be more complicated.	
#' 	



	
# load the libraries each time you restart R	
library(tidyverse)	
library(lubridate)	
library(scales)	
library(skimr)	
library(janitor)	
library(patchwork)	
library(reshape2)	



	
#' ##Read in file     	
#' So a lot of us have to deal with grades but you will get the idea 
#' of how this works with a simple dataframe and then we can go on.	
#' 	
#' 	
# read in the file	
grades.df <- read_csv("data/grades.csv") %>%	
  clean_names() %>%	
  remove_empty(c("rows", "cols")) 	
	
glimpse(grades.df)	

 	
#' ##Wide format file     	
#' 	
#' This file is something that we might hae entered the grades in and it is something
#' we are used to doing in excel. That is not bad but it limits what we 
#' can do fast with the file.         	
#' 
#' ### Graphing in wide format   	
#' So we want to make histograms of the grades and see what they are doing in each of the homeworks.  We would have to make a plot for each of them. Ugggh... This is doable though and fine but not tidy and easy.   	
#' 	
grades.df %>% ggplot(aes(hw_1)) +	
  geom_histogram(binwidth = 5 ) + 	
  scale_x_continuous(breaks=seq(0,100,5)) +	
  labs(x="Points", y="Number of Students")	
#' 	
#' 	
#' ##Convert to *long* format     	
#' Note that it is a bit differnet now. you hae students going down 
#' the left, item which has each homework for each student and the 
#' grades on the right. Studnet 1 is listed 6 times for each of the 6 homeworks.  
#' Why on earth would we do this!!!!!?????	
#' 	

grades_long.df <- grades.df %>% 	
  gather(item, grade, -student_id) %>%	
  mutate(item = as.factor(item)) %>%	
  arrange(student_id, item)	
	
	



	
#' ## Why *long* format      	
#' Well the reason you need this format is for several types of analyses 
#' and it also makes graphing and working with the data easier. Here is an 
#' example to show all the graphs for eah homework in a quick way...      	

      	
#' Remember we can use filter and select to do things in ggplot 
#' with before the plot command too...	
#' 	


grades_long.df %>% 	
  ggplot(aes(grade)) +	
  geom_histogram(binwidth = 5 ) + 	
  scale_x_continuous(breaks=seq(0,100,10)) +	
  labs(x="Points", y="Number of Students") +	
  facet_wrap("item",  scales = "free")	


#' Lets say you only wanted to show homework 1 - 3 in the above plot. 
#' How could you do this easily. Note that the `|` is or so we can choose homework 1 or 2 or 3.	
#' 	


grades_long.df %>% # filter("stuff here") %>%	
  ggplot(aes(grade)) +	
  geom_histogram(binwidth = 5 ) + 	
  scale_x_continuous(breaks=seq(0,100,10)) +	
  labs(x="Points", y="Number of Students") +	
  facet_wrap("item",  scales = "free")	

	
#' 	
#' ##Long to wide format - Spread	
#' So we can convert this back to wide format just as easily.  
#' In doing this you just need to indicate what the category is to 
#' put along the top and the measure.      	



grades_wide.df <- grades_long.df %>% 	
  spread(item, grade)	
	
glimpse(grades_wide.df)	


#' ##Examples using iris.df     	
#' So above were examples using the grades dataframe which is totally made up. 
#' Below it is done using the iris dataframe that we have played with before. 	
#' 	
#' ###Janitor importer 	
#' So often when we read in a file especially from excel.   	
#' This will       	
#' 	
#' * convert all names to lower snake case	
#' * remove all special characters	
#' * remove empty columns and rows that plague excel sheets	
#'    	
#' 	
#' 	
# read in the file	
iris.df <- read_csv("data/iris.csv") %>%	
  clean_names() %>%	
  remove_empty(c("rows", "cols")) 	
	
glimpse(iris.df)	

 	
#' 	
#' Because for some reason there is no variable to identify the individual 
#' we have to create one. Without this there is no way to go from long to wide format 
#' as R does not know what values belong to which species if there are no unique identifiers.   	
#' 	
#' 	
# this will add an index to the dataframe so you know what individual is which	

iris.df <- iris.df %>% mutate(individual = row_number())	
	

	
#' ###Iris to Long format        	
#' So in this section you indicate that you want variables for trait which is along 
#' the top and for some measure or response variable. You then select the columns that 
#' will go down the left for each individual and you do this with the 
#' - sign followed by the variable.	

	
iris_long.df <- iris.df %>% gather(trait, measure, -species, - individual)	
	


#' ### Iris Spread long to wide format     	
#' Again, here you would select the column that will become the header or 
#' variable name and the response variable to go under that and the other variables 
#' will stay along the left column.   	

	
iris_wide.df <- iris_long.df %>% 	
  spread(trait, measure)	


#' ##Unite and Separate a variable     	
#' So sometimes we want to make a variable out of two variables for 
#' grouping or graphing. This can be done easily using the unite 
#' command. Separate is the opposite.	
#' 	
#' 	
#' ###Unite	
#' This will create a variable that is listed first in quotes from the 
#' variables listed after it. So here we are going to make an individual 
#' and species variable from the two separate variables and will also 
#' separate these using the "_" separator which makes it easy to break 
#' that apart.	

	
iris_unite.df <- iris.df %>% 	
  unite("ind_species", individual, species, sep="_")	


#' ###Separate      	
#' We can also separate a variable with a separator in it into its 
#' respective parts. First we indicate the variable to split and then 
#' the variables we what to break it into in a concatenated lists 
#' indicated by `c("1", "2")`	

	
iris_spread.df <- iris_unite.df %>% 	
  separate(ind_species, into=c("individual", "species"), sep="_")	


