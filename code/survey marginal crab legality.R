###RKC survey borderline crab analysis##
##coxa-carapace width##
# Alexandra Reich
# 11/12/25
# Objective: find the rate at which a crab of a given carapace length is legal, using survey data from all years.

##the data was pulled from OceanAK, from the start of survey to 2025 - filtered for male and crab from 138 to 155 CL
## should contain the carapace length (CL) and legality of the crab (1, 2, or 3 for legal, sublegal, and unknown)

## On the survey (at least recently), a male crab between 138 and 155 CL is checked for legality with the 7 inch stick.
## I am not sure how far back the checking for legality goes, but I'll find out shortly



# load libraries
library(dplyr)
library(ggplot2)
library(cowplot)

# load data
survey_data <- read.csv("data/SURVEY RKC marginal crab.csv")

#look at data
unique(survey_data$Legal.Size.Code) #there are some NA's

temp <-survey_data %>% filter(Year==1986) #looks like the legal size testing starts in 1986. That's a ton of data.
unique(temp$Legal.Size.Code)


#filter data
df2 <- survey_data %>% 
  filter(Legal.Size.Code!=3) %>% #get rid of the unknowns
  filter(!is.na(Legal.Size.Code)) %>% #get rid of the NA's
  select(Year, Location, Density.Strata.Code, Recruit.Status, Length.Millimeters, Legal.Size.Code) %>% #just keep columns that I need
  mutate(Legal.Size.Code = factor(Legal.Size.Code)) #that needs to be a factor
#oof should year be a factor too?

##exploratory graphs
# density graph -not that I've pre-selected the legnths so this is not the full distribution of crab
ggplot(df2) + aes(x=Length.Millimeters) + geom_density() +theme_cowplot()

ggplot(df2) + aes(x=Length.Millimeters) + geom_density() +theme_cowplot() + facet_wrap(~Location)

ggplot(df2) + aes(x=Length.Millimeters, color=Legal.Size.Code, fill = Legal.Size.Code) + geom_density(alpha=0.3) +theme_cowplot() #that makes sense
# at 155mm CL almost all of the crab are legal.

#does this change between years? between locations?
ggplot(df2) + aes(x=Length.Millimeters, color=Legal.Size.Code, fill = Legal.Size.Code) + geom_density(alpha=0.3) +theme_cowplot() +facet_wrap(~Year)
#yes changes by year, but I dont see any drastic patterns
ggplot(df2) + aes(x=Length.Millimeters, color=Legal.Size.Code, fill = Legal.Size.Code) + geom_density(alpha=0.3) +theme_cowplot() +facet_wrap(~Location)
#ok by location is a bit more interesting. Maybe I should include that in the model

################################################################
#binomial model
mod_b <- glm(Legal.Size.Code ~ Length.Millimeters, data = df2, family = binomial) #use CL to predict legality in these crab size ranges
summary(mod_b)

mod_global <-glm(Legal.Size.Code ~ Length.Millimeters + factor(Year) + Location, data = df2, family = binomial)

#mod_global_int <- glm(Legal.Size.Code ~ Length.Millimeters*Year*Location, data = df2, family = binomial) #overfitting or something

mod_global_int2 <- glm(Legal.Size.Code ~ Length.Millimeters + Year*Location, data = df2, family = binomial)
summary(mod_global_int2)

mod_b2 <- glm(Legal.Size.Code ~ Length.Millimeters + Location, data = df2, family = binomial) #most likely for our purposes I think
mod_b3 <- glm(Legal.Size.Code ~ Length.Millimeters + factor(Year), data = df2, family = binomial) 

AIC(mod_b, mod_global, mod_global_int2, mod_b2, mod_b3)
BIC(mod_b, mod_global, mod_global_int2, mod_b2, mod_b3) #I'm going with mod_b2

#ooh, area could be a random effects (does glm DO ranefs??)
### "If it is worth doing, it is worth overdoing."
#library(lme4)
#mod_b4 <- glmer(Legal.Size.Code ~ Length.Millimeters + (1|Location), data = df2, family = binomial)
#ok that goes poorly


#anyway, I chose mod_b2 because based on the graphs, it makes sense that location impacts how freqeuently a crab at a given CL is legal


#graph the binomial model

#a table with rates of legality
