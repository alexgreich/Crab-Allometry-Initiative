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

#histogram - look at my data
ggplot(males_2017) + aes(Carapace.width) + geom_density()
ggplot(males_2017) + aes(Carapace.width) + geom_histogram(binwidth = 10)

#data summary
length(males_2017$Carapace.width)
summary(males_2017$Carapace.width)

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
print(round(cor_table, 3))


##and run an ANCOVA
