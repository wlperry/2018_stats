

#' ##Install packages	
#' We will read in the main files and load the libraries as we have worked with so far.   	
# #Install Packages ----	
# install.packages("tidyverse")	
# install.packages("lubridate")	
# install.packages("scales")	
# install.packages("readxl")	
# install.packages("survminer")	
# install.packages("survival")	
# install.packages("patchwork")	
# install.packages("broom")	
# ANOVA specific	
# install.packages("car")	
# install.packages("emmeans")	
# install.packages("multcompView")	
	
	
#Load libraries ----	
library(tidyverse)	
library(lubridate)	
library(scales)	
library(readxl)	
library(skimr)	
library(broom)	
library(janitor)	
# library(zoo)	
library(patchwork)	
library(car)	
library(emmeans)	
library(multcompView)	
	

#' ## Read in the file	
gene_exp.df <- read_excel("data/Example data.XLSX", sheet="Gene expression analysis") %>%	
  clean_names() %>%	
  remove_empty(c("rows", "cols")) 	
	
glimpse(gene_exp.df)	

	
#' ##Summary Statistics for the better look	
# the data you want to look at	
	
skim(gene_exp.df)	
	

#' ##Look at the data	
#' We could do this in the wide format but it is a lot easier in long format	
#' ## Wide to long format	

gene_exp_long.df <- gene_exp.df %>%	
  gather(	
    treatment, # this will take all the row headings into a column	
    expression, # this will convert all the measures into a column called expression	
    -experiment, # the - sign tells tidyr not to move those columns	
    -individual	
  ) 	
	
glimpse(gene_exp_long.df)	
	
# we could spread it back out to what it was	
# gene_exp_wide.df <- gene_exp_long.df %>%	
#   spread(treatment, expression)	
	
	
#' ## Look at data	
glimpse(gene_exp_long.df)	

	
#' ##Convert to factors	
#' So to do this the experiment and individuals are numeric and need to 
#' be converted to categories or factors...	
#' ## Wide data to factors	

# convert things to factors or do calculations	
gene_exp.df <- gene_exp.df %>%	
  mutate(	
    experiment = as.factor(experiment),	
    individual = as.factor(individual)	
  )	
	
# now look at it	
skim(gene_exp.df)	
	
 	
#' ## Long data to factors	
#need to make treatment a factor	
gene_exp_long.df <- gene_exp_long.df %>%	
mutate(	
  experiment = as.factor(experiment),	
    individual = as.factor(individual),	
    treatment = as.factor(treatment)	
  )	
	
# now look at it	
skim(gene_exp_long.df)	
	
	
#' ##Now to graph the long format data	
# Note the new format allows us to make coding a lot faster	
gene_exp_long.df %>% 	
  group_by(treatment)  %>%	
  ggplot(aes(treatment, expression)) +	
  geom_boxplot()	


#' ## T-Tests are a great way to compare two means. But they can be a bit limited.	
#' There are several types of T tests and there are a few assumptions	
#' Normality	
#' Heterogeneity of Variances	
#' and a few others we will talk about....	


#' ###Test for normality of each group	
#' To do this in wide format it is straight forward  	
#' Here these should be non-signficant so they are not deviating from a normal distribution.   	
# Test for normality of each group wide format	
shapiro.test(gene_exp.df$control)	
shapiro.test(gene_exp.df$treat1)	
shapiro.test(gene_exp.df$treat2)	
shapiro.test(gene_exp.df$treat3)	
	

#' ###Test for homogeneity of variances  	
#' Best to use the Levenes test compared to the Bartlet test  	
#' uses the car package   	
#' http://www.cookbook-r.com/Statistical_analysis/Homogeneity_of_variance/   	
#' 	
#Test for homogeneity of variances	
leveneTest(expression ~ treatment, data=gene_exp_long.df)	
	
#Compared to the Bartlet test	
bartlett.test(expression ~ treatment, data=gene_exp_long.df)	
	
# With multiple independent variables, the interaction() function must be used to 
# collapse the IV’s into a single variable with all combinations of the factors. 
# If it is not used,then the will be the wrong degrees of freedom, 
# and the p-value will be wrong.	
# This is for ANOVA but here for reference
# bartlett.test(expression ~ interaction(treatment,experiment), data=gene_exp_long.df)	
	
	

#' ###Paired T-Tests wide format	
# Paired T-Test - Welch 2 sample -----	
t.test(gene_exp.df$control, gene_exp.df$treat1, paired=TRUE) 



#' ###Paired T-Tests long format	
# Paired T-Test - Welch 2 sample -----	
# Welch t-test	
# t.test(variable ~ group, dataframe)	
	
# need to make a dataframe with only 2 groups	
cont_t1.df <- gene_exp_long.df %>%	
  filter(treatment == "control" | treatment == "treat1")	
	
t.test(expression ~ treatment, cont_t1.df, paired=TRUE) # Note the 1 and 2 are in quotes so R knows its the column	

	
#' ###Two Sample T-Test - in wide format	
#' By default, t.test does not assume equal variances	
# Welch t-test	
t.test(gene_exp.df$control, gene_exp.df$treat1, var.equal=FALSE)	

	
#' ###Two Sample T-Test - in long format	
#' By default, t.test does not assume equal variances	
# Welch t-test	
t.test(expression ~ treatment, cont_t1.df, var.equal=FALSE)	



#' ## One way Anova	
#' ## Great Reading	
#' A Great coverage of this material is Dolph Schluters page	
#' https://www.zoology.ubc.ca/~schluter/R/fit-model/	
#' 	
#' The key thing here is the use of the car package as it is essential for 
#' unbalanced designs and the use of Type III sum of squares otherwise Type I 
#' sum of squares are used which is rarely good. Weather to use 
#' Type II or III is a contentious issue and we will just go with Type III 
#' for all of our work	
#' ## Bartlets test for homogeneity of varaince	
#' 	
#Compared variances uisng  Bartlet test	
# significant means they differ in varainces but this is close	
bartlett.test(expression ~ treatment, data=gene_exp_long.df)	


#' ## ANOVA	
#' Now we can run an ANOVA as long as the categories we are testing are factors. 
#' When doing this we will test to see if any of the means are different but will 
#' not be able to tell what is different yet. Thatis the next step	

# ANOVA - ONE WAY	
# you can do an anova as an anova and not the linear model	
#Run the anova and store it in the model in Values	
expression.model.aov = aov(expression ~ treatment, data=gene_exp_long.df)	
	
#Obtain the anova table	
anova(expression.model.aov)	
	
#' ## Save the model for use in word	
	
# save model to a text file for excel or whatever	
tidy(expression.model.aov)	
	
#You can copy this out or save it as an object and then save it as a csv file	
# save the model	
# tidy_expression.model.aov <- tidy(expression.model.aov)	
# write_csv(expression.model.aov, "tidy_anova_expression.csv")	
	

#' ## Plot Residuals	
# Plot residuals	
#Base R plots	
plot(fitted(expression.model.aov), residuals(expression.model.aov))	

	
#' ## Histogram of residuals	
#Histogram of residuals	
hist(residuals(expression.model.aov), 	
     col="darkgray")	

 	
#' ## Check for Normality	
# check for normally distributed data	
qqnorm(expression.model.aov$res)	

	
#' ## Statistical Test of Normality	
#Test for normality of residuals	
shapiro.test(expression.model.aov$res)	


 	
#' ## Post F tests of an ANOVA	
# Post F tests	
# Comparisons of species	
lsm = emmeans(expression.model.aov, 	
              "treatment",	
              adjust="bonferroni")	
	
### Means sharing a letter in .group are not significantly different	
#Note that this requires multcompView	
cld(lsm, 	
    alpha=.05,	
    Letters=letters)	

	
# Now you have a statistical test of how the means compare	
gene_exp_long.df %>% 	
  group_by(treatment)  %>%	
  ggplot(aes(treatment, expression)) +	
  geom_boxplot()	

 	
# what if you wanted a bar plot	
ggplot(gene_exp_long.df, aes(x= treatment))+	
  stat_summary(aes(y=expression), fun.y=mean, geom='bar', color="black", 	
               fill="blue") +	
  stat_summary(aes(y=expression), fun.data = mean_se, geom = "errorbar", color="black", width=0.2)	
