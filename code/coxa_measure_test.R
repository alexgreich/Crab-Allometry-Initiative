###Test coxa measurements- Red King crab
#Alex Reich

library(tidyverse)


##first data set collected June 2025 in st. James Bay by Chris (and Trooper??)
dat1 <- read.csv("data/trooper test data/enforcement test data.csv")
#View(dat1)
names(dat1) #looks fine
unique(dat1$Carapace.width) #should I select for a range of crab? 178 is legal
range(na.omit(dat1$Carapace.width))
##what does a CL of 138 to 155 correspond to in CW? TK filter for that!!
##this actually looks fine -  154 to 198 in length- looks like chris put thought into the measurement range
##if anything we might want to make the lower end 160mm CW. - based on graph exploration of the June 25 and early (2017) survey data
##yeah, I'm going to cut off below 160 in CW- or at least add code for it that can be turned off or on
#dat1 <- dat1 %>% filter(Carapace.width > 159)


#second data set collected July 2025 in Juneau area
dat2 <- read.csv("data/trooper test data/Legal_coxa_testing.csv")
#note there's some values missing- ask Chris about this -FLAG
##it's non essential info but Chris was totes like "I'll fix it!" and then forgot
names(dat2)
#View(dat2)
unique(dat2$Carapace.length..mm.) #oh, these were targeted small crab. nice. 


#well, these datasets are set up differently
##so I'll treat them differently.
##And, can combine and look at 32-legality together at some point...


###Anyway, it's graph time
##what do I want to graph?
#Carapace width size vs...
names(dat1)
#test data- coxa - THE REAL TEST FOR DAT 1
ggplot(dat1) + aes(y=Carapace.width, x=factor(Coxa.legal))+ geom_point(aes(color=factor(Stick.legal)))+
  geom_hline(yintercept=178, color="red", linetype = "dashed")+
  facet_wrap(~factor(Coxa.legal.threshold.used))
#the test data -stick
ggplot(dat1) + aes(y=Carapace.width, x=factor(Stick.legal))+ geom_point()+
  geom_hline(yintercept=178, color="red", linetype = "dashed")+
  facet_wrap(~factor(Coxa.legal.threshold.used))
##at 32- shows one false negative and 0 false postives
dat1$Stick.legal/dat1$Coxa.legal #not that. Some way to tell how often stick was right at 32.


#with dat 2
names(dat2)
#View(dat2)

dat2$Stick.legal <- as.factor(dat2$Stick.legal)

ggplot(dat2) + aes(y= Carapace.length..mm., x= factor(Legal..32mm)) + geom_point(aes(color=Stick.legal)) + 
  geom_hline(yintercept = 138, color = "red", linetype = "dashed")+ #not a hard line here...
  geom_hline(yintercept = 145, color = "red", linetype = "dashed")

ggplot(dat2) + aes(y= Carapace.length..mm., x= factor(Legal..33mm)) + geom_point(aes(color=Stick.legal)) + 
  geom_hline(yintercept = 138, color = "red", linetype = "dashed")+ #not a hard line here...
  geom_hline(yintercept = 145, color = "red", linetype = "dashed")

ggplot(dat2) + aes(y= Carapace.length..mm., x= factor(Legal...31mm)) + geom_point(aes(color=Stick.legal)) + 
  geom_hline(yintercept = 138, color = "red", linetype = "dashed")+ #not a hard line here...
  geom_hline(yintercept = 145, color = "red", linetype = "dashed")

#theme adfg test- an aside
library(adfggraph)
library(extrafont)
#font_import()
loadfonts(device = "win")
ggplot(dat2) + aes(y= Carapace.length..mm., x= factor(Legal...31mm)) + geom_point(aes(color=Stick.legal)) + 
  geom_hline(yintercept = 138, color = "red", linetype = "dashed")+ #not a hard line here...
  geom_hline(yintercept = 145, color = "red", linetype = "dashed") + theme_adfg()

#initial what I'm seeing from here: 32 has one strike (in dat 2). 31 might be better
##what we don't want is false postives. We don't want coxa to say "this aint legal (2)" if it IS legal.
##that happens in dat 2, 33mm 4 times and dat 2, 32 mm, 1 time. 31mm had no false positives so far.

#make these graphs better, then send to spencer, chris with 31mm or smaller as the proposed minimum coxa size. 32mm has one miss

#combine datasets and look at coxa legal vs. stick legal at 31 and 32
names(dat1)
names(dat2)

dat1_31 <- dat1 %>% filter(Coxa.legal.threshold.used == 31) %>% select(Location, Stick.legal, Coxa.legal)
dat1_32 <- dat1 %>% filter(Coxa.legal.threshold.used == 31) %>% select(Location, Stick.legal, Coxa.legal)

dat2$Location <- "Juneau"
dat2_31 <- dat2 %>% select(Stick.legal, Legal...31mm, Location)
dat2_31$Coxa.legal<- dat2_31$Legal...31mm
dat2_31 <- dat2_31 %>% select(-Legal...31mm)

dat2_32 <- dat2 %>% select(Stick.legal, Legal..32mm, Location)
dat2_32$Coxa.legal<- dat2_32$Legal..32mm
dat2_32 <- dat2_32 %>% select(-Legal..32mm)

dat2_33 <- dat2 %>% select(Stick.legal, Legal..33mm, Location)
dat2_33$Coxa.legal<- dat2_33$Legal..33mm
dat2_33 <- dat2_33 %>% select(-Legal..33mm)

dat_31 <- rbind(dat1_31, dat2_31)
dat_32 <- rbind(dat1_32, dat2_32)
dat_33 <- dat2_33

#dat_31$ID <-  c(1:length(dat_31$Stick.legal))
#dat_32$ID <-  c(1:length(dat_32$Stick.legal))

#add false positive columns
dat_31 <- dat_31 %>%
  mutate(false_positive = if_else(Stick.legal == 1 & Coxa.legal == 2, 1, 0)) #it was legal, and the coxa is wrong (BAD!!)

dat_32 <- dat_32 %>%
  mutate(false_positive = if_else(Stick.legal == 1 & Coxa.legal == 2, 1, 0))

dat_33 <- dat_33 %>%
  mutate(false_positive = if_else(Stick.legal == 1 & Coxa.legal == 2, 1, 0))
dat_31
dat_32
dat_33

#add false negative, true postive, true negative columns
dat_31 <- dat_31 %>%
  mutate(true_positive = if_else(Stick.legal == 1 & Coxa.legal == 1, 1, 0), #it was legal, coxa says it's legal too
         false_negative = if_else(Stick.legal == 2 & Coxa.legal == 1, 1, 0), #they got away with it (not as bad as false positive)
         true_negative = if_else(Stick.legal == 2 & Coxa.legal == 2, 1, 0), #stick and coxa agree that it's sublegal
           )

dat_32 <- dat_32 %>%
  mutate(true_positive = if_else(Stick.legal == 1 & Coxa.legal == 1, 1, 0), #it was legal, coxa says it's legal too
         false_negative = if_else(Stick.legal == 2 & Coxa.legal == 1, 1, 0), #they got away with it (not as bad as false positive)
         true_negative = if_else(Stick.legal == 2 & Coxa.legal == 2, 1, 0), #stick and coxa agree that it's sublegal
  )

dat_33 <- dat_33 %>%
  mutate(true_positive = if_else(Stick.legal == 1 & Coxa.legal == 1, 1, 0), #it was legal, coxa says it's legal too
         false_negative = if_else(Stick.legal == 2 & Coxa.legal == 1, 1, 0), #they got away with it (not as bad as false positive)
         true_negative = if_else(Stick.legal == 2 & Coxa.legal == 2, 1, 0), #stick and coxa agree that it's sublegal
  )

#combine
dat_31 <- dat_31 %>% mutate(coxa.width.tested = 31)
dat_32 <- dat_32 %>% mutate(coxa.width.tested = 32)
dat_33 <- dat_33 %>% mutate(coxa.width.tested = 33)

dat_all <- rbind(dat_31, dat_32, dat_33)
dat_all$coxa.width.tested <- as.factor(dat_all$coxa.width.tested)


#data visualize
library(cowplot)
dat_all %>%
  mutate(class = case_when(
    true_positive == 1 ~ "Harvested illegally, got a ticket (GOOD)", #true positive
    false_positive == 1 ~ "Harvested legally, got a ticket (BAD)", #false positive
    true_negative == 1 ~ "Harvested legally, did not get a ticket (GOOD)", #true negatice
    false_negative == 1 ~ "Harvested illegally, did not get a ticket (BAD)" #false negative
  )) %>%
  count(Location, class, coxa.width.tested) %>%
  group_by(Location, coxa.width.tested) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = Location, y = prop, fill = class)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(
    "Harvested illegally, got a ticket (GOOD)" = "#009E73", #true posisitve
    "Harvested legally, did not get a ticket (GOOD)" = "#56B4E9", # true negative
    "Harvested legally, got a ticket (BAD)" = "#E69F00", #false positive
    "Harvested illegally, did not get a ticket (BAD)" = "#D55E00" #false negative
  )) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Agreement and Error Types by Location",
    y = "Percent of observations",
    x = "",
    fill = ""
  ) +
  theme_minimal(base_size = 14)+
  facet_wrap(~ coxa.width.tested) -> fig_two

ggsave("figures/fig_two.png", fig_two, width =10, height = 6, dpi = 300)

#sum over location
dat_all %>%
  mutate(class = case_when(
    true_positive == 1 ~ "Harvested illegally, got a ticket (GOOD)", #true positive
    false_positive == 1 ~ "Harvested legally, got a ticket (BAD)", #false positive
    true_negative == 1 ~ "Harvested legally, did not get a ticket (GOOD)", #true negatice
    false_negative == 1 ~ "Harvested illegally, did not get a ticket (BAD)" #false negative
  ),
  Region ="") %>%
  count(Region, class, coxa.width.tested) %>%
  group_by(Region, coxa.width.tested) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = coxa.width.tested, y = prop, fill = class)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(
    "Harvested illegally, got a ticket (GOOD)" = "#009E73", #true posisitve
    "Harvested legally, did not get a ticket (GOOD)" = "#56B4E9", # true negative
    "Harvested legally, got a ticket (BAD)" = "#E69F00", #false positive
    "Harvested illegally, did not get a ticket (BAD)" = "#D55E00" #false negative
  )) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Agreement and Error Types",
    y = "Percent of observations",
    x = "",
    fill = ""
  ) +
  theme_minimal(base_size = 14) -> fig_one

ggsave("figures/fig_one.png", fig_one, width =10, height = 6, dpi = 300)

######
#Neat, that should be good enough in the data viz department. Now let's get some stats.
dat_sum <- dat_all %>%
  mutate(class = case_when(
    true_positive == 1 ~ "True positive (sensitivity)",
    false_positive == 1 ~ "False positive",
    true_negative == 1 ~ "True negative (specificity)",
    false_negative == 1 ~ "False negative"
  ),
  Region ="") %>%
  count(Region, class, coxa.width.tested) %>%
  group_by(Region, coxa.width.tested) %>%
  mutate(prop = n / sum(n))%>%
  ungroup()

#hmm. let's make some tables

#Table 1: what proportion of 31mm, 32mm, and 33mm are correct
temp1 <- dat_sum %>% filter(coxa.width.tested == 31) %>% select(-Region)
write.csv(temp1, "results/coxa test 31.csv")

temp2 <- dat_sum %>% filter(coxa.width.tested == 32)  %>% select(-Region)
write.csv(temp2, "results/coxa test 32.csv")

temp3 <- dat_sum %>% filter(coxa.width.tested == 33)  %>% select(-Region)
write.csv(temp3, "results/coxa test 33.csv")


#test??
#sensitivity and specificity
library(broom)

results <- dat_all %>%
  summarise(
    sens_31 = mean(Coxa.test31[Stick.legal == 1]), # true positive rate
    spec_31 = mean(1 - Coxa.test31[Stick.legal == 0]),
    sens_32 = mean(Coxa.test32[Stick.legal == 1]),
    spec_32 = mean(1 - Coxa.test32[Stick.legal == 0]),
    sens_33 = mean(Coxa.test33[Stick.legal == 1]),
    spec_33 = mean(1 - Coxa.test33[Stick.legal == 0])
  )
