
#' ## Data Modification and cleaning	
#' So the pattern of this set of code so far has been how to look at the data visually and how it looks and to see outliers.  The next goal is to be able to clean the data or remove, reoranize or reformat the data and add new transformed varaibles.     	
#' 	
#' 	
#' ##Load libraries	
#' We will read in the main files and load the libraries as we have worked with so far.   	
#' 
# load the libraries each time you restart R	
library(tidyverse)	
library(lubridate)	
library(scales)	
library(skimr)	
library(janitor)	
library(patchwork)	
library(readxl)	

	
# read in the file:	
mc.df <- read_excel("data/mc.xlsx")	
	
# Read in the second file:	



	
# look at the data using glimpse()

	


#' ##Joining dataframes together	
#' So lets say we have these two dataframes and we want to work with them together - 
#' how do we do this?	
#' 	
#' ###Rbind or append	
#' We can bind the two dataframes together as long as they have the same column names 
#' and same column numbers. This will *append* a dataframe to the other. Now when we 
#' do code we can work with it all at once. 
#' We can also separate the data into smaller dataframes later. 
#' Use df <- rbind(df1, df2) then use glimpse() to look at it.




#' ##The pipe	
#' So lets start thinking about how to string a lot of commands 
#' together. Before this we have assigned items to a name or 
#' environmental varaible with ```<-``` bu now what we want 
#' to do is sort of the opposite.  We want to take an object 
#' or output of an object and pass it to the right to do new 
#' things with the output. We do this using the pipe command ```%>%```. 
#' This takes what is on the left and passes it to the right.    	




#' ##Mutate	
#' So lets say in this dataframe we wanted to calculate the Dry Weight of a sample by subracting the Tare weight from Dry Weight     	
#'         	
#'  mc_smc.df <- mc_smc.df %>% mutate(variable = varaible - variable))      	


	


#' So here we made a new column and its all set. Lets say we wanted to calculate the Dry Wt per liter   	
	
	
# mc_smc.df <- mc_smc.df %>% mutate(variable = varaible / variable))	
	



 	
#' So that has done a transformation on two separate variables and would take a while to do one by one. We can also link the mutations together with commas and it will do the same thing above and overwrite the variables again	
#
# mc_smc.df <- mc_smc.df %>% 	
#                   mutate(	
#                          variable = varaible - variable),	
#                           variable = varaible / variable)	
#                        )	





#' ## Date Times	
#' So if you look at the data you will see that datetieme is a 
#' character variable and not a date time. We can use the same 
#' method above to convert that.    	
#' 	
#' We will use the lubridate package to do this...      	
#' y = year     	
#' m = month    	
#' d = day    	
#' h = hour     	
#' m = minute    	
#' s = second     	
#' <br>	
#' You put these in order that they appear in the excel cell 
#' and note that if you open a csv and save it again from excel 
#' it will convert it to Month - Day - Year Hour: Minute: Second 
#' so be aware if there is an error here.    	
#' 	
# you can make dates and time using lubridate	
# This would be the traditional way and maybe a bit slower.	
# mc.df$datetime <- mdy_hm(mc.df$datetime)	
	
# df <- df %>% 	
#  mutate(	
#          variable = mdy_hm(varaible)	
#         )	
#
# There is a way we can change many variaibles at a time	


	
#' ##Modifying dates	
#' So we can now work with this datetime column really easily and pull 
#' out parts of it to work with further	
#' mc_smc.df <- mc_smc.df %>% 	
#       mutate(	
#             year = year(datetime),	
#             month = month(datetime),	
#             day = day(datetime),	
#             julian_day = yday(datetime)	
#             )	
# 

mc_smc.df <- mc_smc.df %>% 	
  mutate(	
         year = year(datetime),	
         month = month(datetime),	
         day = day(datetime),	
         julian_day = yday(datetime)	
  )	



#' ## Filtering data   	
#' What if you wanted to work with only one set of information in
#' the different variables. 
#' 
#' df <- df %>% filter(year == xxxx)

mc_smc_2015.df <- mc_smc.df %>% filter(year==2015)	
	
head(mc_smc_2015.df)	




#'Use & to select two things
#' 	 	
mc_2015.df <- mc_smc.df %>% filter(year==2015 & site=="mc") 	
	
head(mc_smc_2015.df)	



#' ## Pipes and GGPlot	
#'  	
#' Lets try this with a plot.  
#' What happens here is it takes the dataframe 
#' and then passes it to the ggplot commands. 
#' So the dataframe is removed from the ggplot commands. 
#' I will show you why in a second that this is really cool.	
#' 
#' mc_smc.df %>% 	
#' ggplot(aes(x=julian_day, y=w_temp_c, color=site)) +	geom_point() 
#' 
#' try typing this one or you can un commnet the code and run it.

	

	
#' So what we can also do is rather than making new dataframes 
#' all the time for graphs we can use the pipe command and 
#' lead the data into GGPlot	
 	
mc_smc.df %>% filter(year==2015 & site=="mc") %>%	
  ggplot(aes(x=julian_day, y=dry_wt_g_l, color=site)) +	
  geom_point() 	


 	
#' Lets assume you wanted to select all dry weight and volume filtered  you can use things like: 	
#' ````starts_with(), ends_with(), contains(),  everything()```` in various combinations such as	
#' 	
mc_smc_drywt.df <- mc_smc.df %>% 	
  select(datetime, julian_day, volume_filtered_l, starts_with("dry"))	
#' 	
#' 	
#' 	
#' 	
#' ##What if you wanted see all NA values in a particular variable....	
#' 	you use df %>% filter(is.na(variable))	
#' 	
#' 	

#' 	
#' 	
#' ##What if you wanted see all NA values	
#' you use df %>% filter(!is.na(variable))		



	
#' ##Groups and summmarize data    	
#' 	
#' Now a really cool thing we can do is to group data and then look at summairies of the data or use it later down the road.	
#' 	
#' So lets say we wanted to see the mean temperature by month in each stream	
#' 	
mc_smc.df %>% group_by(site, year, month) %>%	
  mutate(	
    mean_temp_c = mean(w_temp_c)	
  )	
	


#' We can also use the summarize function but it leads to something different. What you get is the grouping variable and the varaibles you are summarizing. I have found a neat way to get others and that is using the `first(varaible)` summarize term to get the first value of a group.   	
#' 	mc_smc.df %>% group_by(site, year, month) %>%	
# summarize(	
#   new variable = mean(variable)	
#     )	
	



#' You should get used to adding in the term *`na.rm=TRUE`* on all of your calculations. If you don't you can see the results below. 	
#' 	
	
mc_smc.df %>% group_by(site, year, month) %>%	
  summarize(	
    mean_temp_c = mean(w_temp_c),	
    mean_temp_c_2 = mean(w_temp_c, na.rm=TRUE)	
  )	
	


	
#' Finally we could also use this for plotting	
#' 	
	
mc_smc.df %>% group_by(site, year, month) %>%	
  summarize(	
    julian_day = first(julian_day),	
    mean_temp_c_2 = mean(w_temp_c, na.rm=TRUE)	
  ) %>%	
  ggplot( aes(x=julian_day, y=mean_temp_c_2))+	
  geom_point()	
	



#' ##Clean data     	
#' So lets finally use the *janitor* pacakge to clean up this data. This will make working with data sets from other users a bit easier to work with quickly rather than reformatting all the values manually.    	
#' <br>    	
#' Note that this will get rid of empty columns and rows if from an excel sheet and will clean up the variable names.   	
#' 	
#' ### Note above what you get - you get all of the special characters removed, capitilizations are now lower case and its in snake case. It also removes any empty rows and columns that you get from excel often.	


iris.df <- read_csv("data/iris.csv") %>%	
  clean_names() %>%	
  remove_empty(c("rows", "cols"))	
	
head(iris.df)	


	

