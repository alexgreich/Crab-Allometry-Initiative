######Table 1 and other additions#####
###Alex Reich 6/5/26
### Based on Allometry Code AGR cleanup, I will pull the raw (well, spencer-cleaned) data and make a Table 1 (2017 data collection)


library(tidyverse)
library(ggplot2)

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


