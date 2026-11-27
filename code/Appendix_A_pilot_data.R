##Appendix A: Pilot data
#Alex Reich (alex.reich@alaska.gov)
# 11/27/26
# This code reads in and analyses the pilot data for the RKC allometry project. The pilot data was collected in 2017 and includes measurements of carapace length, carapace width, coxa length, and mera length.
# The objective is to determine if coxa width or merus length is a better indicator of crab legality
# Spencer had analyzed this data previously and determined that coxa width is better than merus length. Here I am formatting that analysis into a publishable format (and I dont know where her code is)

#librarys
library(tidyverse)
library(ggplot2)

#read in data
dat <- read.csv("data/RKC_allometry_pilot_data_filtered.csv")
#View(dat)
dim(dat)

names(dat)
#filter for males and remove all entries with a NA
dat_M <- dat %>% filter(Sex=="M") %>%
  drop_na()
#View(dat_M)
dim(dat_M)

#mera correlations
merus_cor_1R <- cor(dat_M$Right.merus.1st.walking, dat_M$Carapace.width.including.spines..mm.)
merus_cor_1L <- cor(dat_M$Left.merus.1st.walking, dat_M$Carapace.width.including.spines..mm.)
merus_cor_2R <- cor(dat_M$Right.merus.2nd.walking, dat_M$Carapace.width.including.spines..mm.)
merus_cor_2L <- cor(dat_M$Left.merus.2nd.walking, dat_M$Carapace.width.including.spines..mm.)
merus_cor_3R <- cor(dat_M$Right.merus.3rd.walking, dat_M$Carapace.width.including.spines..mm.)
merus_cor_3L <- cor(dat_M$Left.merus.3rd.walking, dat_M$Carapace.width.including.spines..mm.)

#coxa correlations
coxae_cor_1R <- cor(dat_M$Right.Coxa.1st, dat_M$Carapace.width.including.spines..mm.)
coxae_cor_1L <- cor(dat_M$Left.Coxa.1st, dat_M$Carapace.width.including.spines..mm.)
coxae_cor_2R <- cor(dat_M$Right.Coxa.2nd, dat_M$Carapace.width.including.spines..mm.)
coxae_cor_2L <- cor(dat_M$Left.Coxa.2nd, dat_M$Carapace.width.including.spines..mm.)
coxae_cor_3R <- cor(dat_M$Right.Coxa.3rd, dat_M$Carapace.width.including.spines..mm.)
coxae_cor_3L <- cor(dat_M$Left.Coxa.3rd, dat_M$Carapace.width.including.spines..mm.)

#table comparing merus and coxa correlations
table_A <- data.frame(
  "Leg" = c("1st", "1st", "2nd", "2nd", "3rd", "3rd"),
  "Side" = c("Right", "Left", "Right", "Left", "Right", "Left"),
  "Mera" = c(merus_cor_1R, merus_cor_1L, merus_cor_2R, merus_cor_2L, merus_cor_3R, merus_cor_3L),
  "Coxa" = c(coxae_cor_1R, coxae_cor_1L, coxae_cor_2R, coxae_cor_2L, coxae_cor_3R, coxae_cor_3L)
)

#round table A to 2 decimal points
table_A.1 <- table_A %>%
  mutate(Merus = round(Merus, 2),
         Coxa = round(Coxa, 2))

write_csv(table_A.1, "results/pilot_data_correlations.csv")


#graph data

#now look at a corrlation table separated by each area
table_A_2 <- dat_M %>%
  group_by(Area) %>%
  summarise(
    merus_cor_1R = cor(Right.merus.1st.walking, Carapace.width.including.spines..mm., use = "complete.obs"),
    merus_cor_1L = cor(Left.merus.1st.walking, Carapace.width.including.spines..mm., use = "complete.obs"),
    merus_cor_2R = cor(Right.merus.2nd.walking, Carapace.width.including.spines..mm., use = "complete.obs"),
    merus_cor_2L = cor(Left.merus.2nd.walking, Carapace.width.including.spines..mm., use = "complete.obs"),
    merus_cor_3R = cor(Right.merus.3rd.walking, Carapace.width.including.spines..mm., use = "complete.obs"),
    merus_cor_3L = cor(Left.merus.3rd.walking, Carapace.width.including.spines..mm., use = "complete.obs"),
    coxae_cor_1R = cor(Right.Coxa.1st, Carapace.width.including.spines..mm., use = "complete.obs"),
    coxae_cor_1L = cor(Left.Coxa.1st, Carapace.width.including.spines..mm., use = "complete.obs"),
    coxae_cor_2R = cor(Right.Coxa.2nd, Carapace.width.including.spines..mm., use = "complete.obs"),
    coxae_cor_2L = cor(Left.Coxa.2nd, Carapace.width.including.spines..mm., use = "complete.obs"),
    coxae_cor_3R = cor(Right.Coxa.3rd, Carapace.width.including.spines..mm., use = "complete.obs"),
    coxae_cor_3L = cor(Left.Coxa.3rd, Carapace.width.including.spines..mm., use = "complete.obs")
  )
#View(table_A_2)
#round the correlations to 2 decimal points
table_A_2 <- table_A_2 %>%
  mutate(across(starts_with("merus_cor"), ~ round(., 2)),
         across(starts_with("coxae_cor"), ~ round(., 2)))

#include number of observations for each area
table_A_2 <- table_A_2 %>%
  left_join(dat_M %>%
              group_by(Area) %>%
              summarise(n = n()), by = "Area")

#reformat the table to be group by side so it fits on a landscape page better
table_A_2 <- table_A_2 %>%
  select(Area, n, merus_cor_1R, merus_cor_1L, coxae_cor_1R, coxae_cor_1L,
         merus_cor_2R, merus_cor_2L, coxae_cor_2R, coxae_cor_2L,
         merus_cor_3R, merus_cor_3L, coxae_cor_3R, coxae_cor_3L) %>%
  rename(
    "Merus 1st R" = merus_cor_1R,
    "Merus 1st L" = merus_cor_1L,
    "Coxa 1st R" = coxae_cor_1R,
    "Coxa 1st L" = coxae_cor_1L,
    "Merus 2nd R" = merus_cor_2R,
    "Merus 2nd L" = merus_cor_2L,
    "Coxa 2nd R" = coxae_cor_2R,
    "Coxa 2nd L" = coxae_cor_2L,
    "Merus 3rd R" = merus_cor_3R,
    "Merus 3rd L" = merus_cor_3L,
    "Coxa 3rd R" = coxae_cor_3R,
    "Coxa 3rd L" = coxae_cor_3L
  )

#save
write_csv(table_A_2, "results/pilot_data_correlations_by_area.csv")


#make it so that the table is in long format
table_A_2_long <- table_A_2 %>% 
  pivot_longer(cols = -Area, names_to = c("Measurement", "Leg", "Side"), names_sep = "_", values_to = "Correlation")
View(table_A_2_long)

#round teh correlations to 2 decimal points
table_A_2_long <- table_A_2_long %>%
  mutate(Correlation = round(Correlation, 2))

#write_csv(table_A_2_long, "results/pilot_data_correlations_by_area.csv")

#include number of observations for each area
table_A2_long.2 <- dat_M %>%
  group_by(Area) %>%
  summarise(n = n()) %>%
  left_join(table_A_2_long, by = "Area")
#View(table_A2_long.2)
