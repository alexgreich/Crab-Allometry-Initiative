### Phase 3 of red king crab allometry project
##Opportunistic port sampling data exploration and analysis
## Alex Reich, alex.reich@alaska.gov

## We opportunistically tested the coxa of Port Sampling crab at 31mm and 32mm
## We started with crab of all sizes, then quickly realized we should aim for the borderline crab sizes (138 - 155mm)
### so I'll filter the crab larger than 155mm out of the analysis.
## Since these crab were part of the commercial fishery, they are all pre-selected to be legal size
###  we did check carapace width with the 7inch stick if a crab looked borderline.

#load libraries
library(dplyr)
library(tidyverse)
library(cowplot)
#library(adfgcolors)
library(PNWColors)
library(adfggraph) #why not...

#read in data
port_dat <-read.csv("data/trooper test data/Legal_coxa_testing_comm_port_sampling_25.csv")

port_dat1 <- port_dat %>% filter(Carapace.length..mm. < 156) #155 or under for carapace length.

names(port_dat1)
length(port_dat1$Carapace.length..mm.)

#exploratory plots
##length of crab density plot or histogram
ggplot(port_dat1) + aes(x=Carapace.length..mm.) + geom_density() #ok... we got a good amount of larger crab, and no crab under 142

#make a plot like phase 2, indentifying the flase postives, negatives, for 31 and 32
View(port_dat1)


#restructure dataframe so that I indentify true postives, false postives, true negatives, false negatives for 31 and 32
names(port_dat1)

#port_dat1_31 <- port_dat1 %>% select

port_dat_31_results <- port_dat1 %>%
  mutate(false_positive = if_else(Stick.legal == 1 & Legal...31mm == 2, 1, 0), #it was legal, and the coxa is wrong (BAD!!)
         true_negative = if_else(Stick.legal == 1 & Legal...31mm == 1, 1, 0)# #it was legal, coxa says it's legal too
         ) %>% #next select for just the columns you need AGR here!!
  select(-Legal..32mm) %>%
  rename(Coxa.legal = Legal...31mm) %>%
  mutate(Test.threshold = 31)

port_dat_32_results <- port_dat1 %>%
  mutate(false_positive = if_else(Stick.legal == 1 & Legal..32mm == 2, 1, 0), #it was legal, and the coxa is wrong (BAD!!)
         true_negative = if_else(Stick.legal == 1 & Legal..32mm == 1, 1, 0)#, #it was legal, coxa says it's legal too
  ) %>%
  select(-Legal...31mm) %>%
  rename(Coxa.legal = Legal..32mm) %>%
  mutate(Test.threshold = 32)

#next, combine these
dat_comb <- rbind(port_dat_31_results, port_dat_32_results) %>%
  mutate(Test.threshold = factor(Test.threshold))


#AGR HERE!! - making my graph. maybe need to pull some more stuff from Fig 1
dat_comb %>%
  mutate(class = case_when(
    #true_positive == 1 ~ "Harvested illegally, got a ticket (GOOD)", #true positive
    false_positive == 1 ~ "Harvested legally, got a ticket (BAD)", #false positive
    true_negative == 1 ~ "Harvested legally, did not get a ticket (GOOD)"#, #true negatice
    #false_negative == 1 ~ "Harvested illegally, did not get a ticket (BAD)" #false negative
  ),
  Region ="",
  # set factor levels in the order you want them stacked (bottom → top)
  class = factor(class, levels = c(
    "Harvested legally, got a ticket (BAD)",   # VERY BAD goes on top
    #"Harvested illegally, did not get a ticket (BAD)"#,
    #"Harvested illegally, got a ticket (GOOD)"#,
    "Harvested legally, did not get a ticket (GOOD)"
  ))
  ) %>%
  count(class, coxa.width.tested) %>%
  group_by(coxa.width.tested) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = coxa.width.tested, y = prop, fill = class)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(
    #"Harvested illegally, got a ticket (GOOD)" = "#00496f", # "#009E73", #true posisitve
    "Harvested legally, did not get a ticket (GOOD)" = "#0f85a0", # true negative
    "Harvested legally, got a ticket (BAD)" = "#dd4124"#, #false positive
    #"Harvested illegally, did not get a ticket (BAD)" = "#ed8b00" #false negative
  )) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Crab Measurement Test Results",
    y = "Percent of observations",
    x = "Coxa test threshold (mm)",
    fill = ""
  ) +
  theme_minimal(base_size = 14) -> fig_P3


#this was really only a test for false positive vs. true positive...
pnw_palettes$Bay #note the palette color ID #'s

#combine the 31 and 32mm results into one df please.


