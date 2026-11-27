######Table 1 and other additions#####
###Alex Reich 6/5/26
### Based on Allometry Code AGR cleanup, I will pull the raw (well, spencer-cleaned) data and make a Table 1 (2017 data collection)


library(tidyverse)
library(ggplot2)
library(adfggraph)

#pulled from Spencer's code:
newdata <- read.csv("data/full crab allometry dataset_USE.csv", header = TRUE)
#View(newdata)  

males <- subset(newdata, Sex == "M")
#View(males)
males[, 'Year'] <- as.factor(males[, 'Year'])
#males[, 'Species'] <- as.factor(males[, 'Species'])
males[, 'Sample.Number'] <- as.factor(males[, 'Sample.Number'])

#View(males)

#filter for those collected in 2017
males_2017 <- males %>% filter (Year == 2017)

#histogram - look at my data- looks intentional, idk
ggplot(males_2017) + aes(Carapace.width) + geom_density()
ggplot(males_2017) + aes(Carapace.width) + geom_histogram(binwidth = 10)
ggplot(males_2017) + aes(Carapace.width) + geom_histogram(binwidth = 10) + facet_wrap(~Area)

dim(males_2017) #221 

#data summary
length(males_2017$Carapace.width)
summary(males_2017$Carapace.width)
#how many males by area?
males_2017 %>% group_by(Area) %>% summarise(count = n())

#peek at 2025
males_2025 <- males %>% filter(Year == 2025)
ggplot(males_2025) + aes(Carapace.width) + geom_density()

#make it a table
males_2017_table_1 <-   males_2017 %>%
  mutate(
    CW_bin = floor(Carapace.width / 10) * 10
  ) %>%
  group_by(CW_bin) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(CW_label = paste0(CW_bin, "-", CW_bin + 9)) %>%
  arrange(CW_bin)

males_2017_table_2 <-   males_2017 %>%
  mutate(
    CW_bin = floor(Carapace.width / 10) * 10
  ) %>%
  group_by(CW_bin, Area) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(CW_label = paste0(CW_bin, "-", CW_bin + 9)) %>%
  arrange(CW_bin)

males_2017_table_e <- males_2017_table_2 %>% filter(Area == "Excursion")
males_2017_table_p <- males_2017_table_2 %>% filter(Area == "Peril Strait")
males_2017_table_j <- males_2017_table_2 %>% filter(Area == "St James Bay")
males_2017_table_s <- males_2017_table_2 %>% filter(Area == "Seymour")
males_2017_table_g <- males_2017_table_2 %>% filter(Area == "Gambier")

males_2017_table_wide <- males_2017 %>%
  mutate(
    CW_bin = floor(Carapace.width / 10) * 10,
    CW_label = paste0(CW_bin, "-", CW_bin + 9)
  ) %>%
  group_by(CW_label, CW_bin, Area) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Area,
    values_from = count,
    values_fill = 0,
    names_prefix = "count_"
  ) %>%
  arrange(CW_bin) %>%
  select(-CW_bin) %>%
  mutate(count_total = rowSums(across(starts_with("count_"))))

write.csv(males_2017_table_wide, "results/table_2017_sample_size.csv")


males_2025_table <-   males_2025 %>%
  mutate(
    CW_bin = floor(Carapace.width / 10) * 10
  ) %>%
  group_by(CW_bin) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(CW_label = paste0(CW_bin, "-", CW_bin + 9)) %>%
  arrange(CW_bin)

males_2025_table_wide <- males_2025 %>%
  mutate(
    CW_bin = floor(Carapace.width / 10) * 10,
    CW_label = paste0(CW_bin, "-", CW_bin + 9)
  ) %>%
  group_by(CW_label, CW_bin, Area) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Area,
    values_from = count,
    values_fill = 0,
    names_prefix = "count_"
  ) %>%
  arrange(CW_bin) %>%
  select(-CW_bin) %>%
  mutate(count_total = rowSums(across(starts_with("count_"))))


###data summary
#males - my data frame
dim(males_2025)
dim(males_2017)
summary(males$Carapace.width)

yy <- ggplot(males) + aes(x= Carapace.width) + geom_histogram(bins = 10) +theme_adfg(box=FALSE)+
  labs(x = "Carapace Width (mm)", y = "Count")
ggsave("figures/2017_2025_histogram_nobox.png", yy, width = 6, height = 4)


#######################################################################################################
##let's check out the by-area correlation for coxa width-carapace length for both 2017 and 2025
males.cor <- cor(males[, unlist(lapply(males, is.numeric))], use = "complete.obs")
males_cor_2017 <- cor(males_2017[, unlist(lapply(males_2017, is.numeric))], use = "complete.obs")
males_cor_2025 <- cor(males_2017[, unlist(lapply(males_2017, is.numeric))], use = "complete.obs")

unique(males$Area)

#St. James bay
Area <- males %>% filter(Area == "St James Bay")
L3_J<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.LEFT, use = "complete.obs")
R3_J<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.RIGHT, use = "complete.obs")
L2_J<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.LEFT, use = "complete.obs")
R2_J<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.RIGHT, use = "complete.obs")
L1_J<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.LEFT, use = "complete.obs")
R1_J<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.RIGHT, use = "complete.obs")
n_james <- nrow(Area)

#Excursion
Area <- males %>% filter(Area == "Excursion")
L3_E<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.LEFT, use = "complete.obs")
R3_E<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.RIGHT, use = "complete.obs")
L2_E<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.LEFT, use = "complete.obs")
R2_E<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.RIGHT, use = "complete.obs")
L1_E<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.LEFT, use = "complete.obs")
R1_E<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.RIGHT, use = "complete.obs")
n_excursion <- nrow(Area)

#Juneau
Area <- males %>% filter(Area == "Juneau")
L3_Ju<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.LEFT, use = "complete.obs")
R3_Ju<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.RIGHT, use = "complete.obs")
L2_Ju<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.LEFT, use = "complete.obs")
R2_Ju<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.RIGHT, use = "complete.obs")
L1_Ju<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.LEFT, use = "complete.obs")
R1_Ju<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.RIGHT, use = "complete.obs")
n_Juneau <- nrow(Area)

#Peril
Area <- males %>% filter(Area == "Peril Strait")
L3_P<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.LEFT, use = "complete.obs")
R3_P<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.RIGHT, use = "complete.obs")
L2_P<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.LEFT, use = "complete.obs")
R2_P<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.RIGHT, use = "complete.obs")
L1_P<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.LEFT, use = "complete.obs")
R1_P<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.RIGHT, use = "complete.obs")
n_Peril <- nrow(Area)

#Seymour
Area <- males %>% filter(Area == "Seymour")
L3_S<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.LEFT, use = "complete.obs")
R3_S<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.RIGHT, use = "complete.obs")
L2_S<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.LEFT, use = "complete.obs")
R2_S<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.RIGHT, use = "complete.obs")
L1_S<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.LEFT, use = "complete.obs")
R1_S<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.RIGHT, use = "complete.obs")
n_Seymour <- nrow(Area)

#Gambier
Area <- males %>% filter(Area == "Gambier")
L3_G<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.LEFT, use = "complete.obs")
R3_G<- cor(Area$Carapace.width, Area$Coxa.walking.leg.3.RIGHT, use = "complete.obs")
L2_G<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.LEFT, use = "complete.obs")
R2_G<-cor(Area$Carapace.width, Area$Coxa.walking.leg.2.RIGHT, use = "complete.obs")
L1_G<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.LEFT, use = "complete.obs")
R1_G<-cor(Area$Carapace.width, Area$Coxa.walking.leg.1.RIGHT, use = "complete.obs")
n_Gambier <- nrow(Area)

#QC overall correlations - all checks out :)
males_clean <- males %>% filter(is.finite(Coxa.walking.leg.3.RIGHT))
cor(males_clean$Carapace.width, males_clean$Coxa.walking.leg.3.LEFT)
cor(males_clean$Carapace.width, males_clean$Coxa.walking.leg.3.RIGHT)
cor(males_clean$Carapace.width, males_clean$Coxa.walking.leg.2.LEFT)
cor(males_clean$Carapace.width, males_clean$Coxa.walking.leg.2.RIGHT)
cor(males_clean$Carapace.width, males_clean$Coxa.walking.leg.1.LEFT)
cor(males_clean$Carapace.width, males_clean$Coxa.walking.leg.1.RIGHT)

#make this a table:

areas <- c("St James Bay", "Excursion", "Juneau", "Peril Strait", "Seymour", "Gambier")
coxa_vars <- c("Coxa.walking.leg.1.RIGHT", "Coxa.walking.leg.2.RIGHT", "Coxa.walking.leg.3.RIGHT",
               "Coxa.walking.leg.1.LEFT",  "Coxa.walking.leg.2.LEFT",  "Coxa.walking.leg.3.LEFT")
row_names <- c("1R", "2R", "3R", "1L", "2L", "3L")

cor_table <- sapply(areas, function(a) {
  df <- males %>% filter(Area == a)
  c(sapply(coxa_vars, function(v) cor(df$Carapace.width, df[[v]], use = "complete.obs")),
    N = nrow(df))
})

rownames(cor_table) <- c(row_names, "N")
cor_table <- round(cor_table, 3)

write.csv(cor_table, "results/coxa_cw_correlations_by_area.csv")

##graph st james - those are some weirdly weak correlations


##Run linear models - AGR 11/25/26
### separate by Legs please.
#View(males)
males <- males %>% mutate(Area = as.factor(Area))
####RIGHT

mod_R2_glob <- lm(Coxa.walking.leg.2.RIGHT ~ Carapace.width*Area, data=males)
summary(mod_R2_glob)
###interaction effect sign for cw and st 
mod_R2_area <- lm(Coxa.walking.leg.2.RIGHT ~ Carapace.width + Area, data=males)
summary(mod_R2_area)

mod_R2_base <- lm(Coxa.walking.leg.2.RIGHT ~ Carapace.width, data=males)
summary(mod_R2_base)

AIC(mod_R2_glob, mod_R2_area, mod_R2_base)
AICc(mod_R2_glob, mod_R2_area, mod_R2_base)

library(performance)
performance(mod_R2_glob)
performance(mod_R2_area)
performance(mod_R2_base)

##try without seymour, gambier
#males_test <- males %>% filter(Area != "Seymour" & Area != "Gambier")
#mod_R2_glob_test <- lm(Coxa.walking.leg.2.RIGHT ~ Carapace.width*Area, data=males_test)
#mod_R2_area_test <- lm(Coxa.walking.leg.2.RIGHT ~ Carapace.width+Area, data=males_test) 
#mod_R2_base_test <- lm(Coxa.walking.leg.2.RIGHT ~ Carapace.width, data=males_test)
#AIC(mod_R2_glob_test, mod_R2_area_test, mod_R2_base_test) #oh dang, the additive model fit best
#performance(mod_R2_glob_test)
#performance(mod_R2_area_test)
#performance(mod_R2_base_test)





####LEFT


mod_null <- lm(Coxa.walking.leg.2.RIGHT ~ Carapace.width + Area, data=males)
AIC(mod_null, mod)


########
#check out St. James
############
#crab size compared to the others- look at graphs
ggplot(males) + aes(Carapace.width) + geom_density() + facet_wrap(~Area)
ggplot(males) + aes(x= Carapace.width, y=Coxa.walking.leg.2.RIGHT) + geom_point() + geom_smooth(method="lm") + facet_wrap(~Area) 

                                                                                                                          