library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(PerformanceAnalytics)
library(reshape2)

rkc <- read.csv("data/RKC_allometry_pilot_data_filtered.csv", header = TRUE)
#View(rkc)

rkc.males <- subset(rkc, Sex == "M")
#View(rkc.males)
rkc.males[, 'Year'] <- as.factor(rkc.males[, 'Year'])
rkc.males[, 'Species'] <- as.factor(rkc.males[, 'Species'])
rkc.males[, 'Number'] <- as.factor(rkc.males[, 'Number'])

#AGR here 11/25/25
##CL CW graph
ggplot(rkc.males) + aes(x=Carapace.length..mm., y= Carapace.width.including.spines..mm.) + geom_point()+
  geom_smooth(method = "lm", se=TRUE) + geom_vline(xintercept = 155, color="red", linetype = "dashed")+ geom_vline(xintercept = 138, color="red", linetype = "dashed")+
  geom_hline(yintercept = 178, color = "skyblue", linetype = "dashed")+
  geom_hline(yintercept = 160, color = "green", linetype = "dashed")+
  geom_hline(yintercept = 198, color = "green", linetype = "dashed")+
  geom_hline(yintercept = 154, color = "purple", linetype = "dashed")


#what is the SE of the dataset?
mean_rkc_cw <- mean(rkc.males$Carapace.width.including.spines..mm.)
sd_rkc_cw <- sd(rkc.males$Carapace.width.including.spines..mm.)

mean_rkc_cw -  1.96*sd_rkc_cw
mean_rkc_cw +  1.96*sd_rkc_cw

#se of data filteres
rkc_m_filtered <- rkc.males %>% filter(Carapace.length..mm. > 137 & Carapace.length..mm. < 156)

mean_rkc_cw_filt <- mean(rkc_m_filtered$Carapace.width.including.spines..mm.)
sd_rkc_cw_filt <- sd(rkc_m_filtered$Carapace.width.including.spines..mm.)
mean_rkc_cw_filt - 1.96*sd_rkc_cw_filt
mean_rkc_cw_filt + 1.96*sd_rkc_cw_filt
#CI is 161.33 to 194.22 mm for cw within the 138-155 threshold of interest
  
  #AGR end

rkc.males.cor <- cor(rkc.males[, unlist(lapply(rkc.males, is.numeric))], use = "complete.obs")
#View(rkc.males.cor)

res <- melt(rkc.males.cor)
res$type=apply(res,1,function(x) 
  paste(sort(c(as.character(x[1]),as.character(x[2]))),collapse="*"))  
res=unique(res[,c("type","value")])  
#View(res)

#chart.Correlation(rkc.males.cor, histogram = TRUE, method = "pearson")

#corrplot(rkc.males.cor, type = "lower", order = "original", diag = FALSE)
#corPlot(rkc.males.cor, method = "pearson")

plot(rkc.males$Carapace.width.including.spines..mm., rkc.males$Right.Coxa.3rd, pch = 19, col = "lightblue")
abline(lm(rkc.males$Right.Coxa.3rd ~ rkc.males$Carapace.width.including.spines..mm.), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(rkc.males$Carapace.width.including.spines..mm., rkc.males$Right.Coxa.3rd), 2)), x = 100, y = 45)


data <- rkc.males[, 6:20]
groups <- rkc.males[, 1:5]
pairs(data)
plot(data)

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  his <- hist(x, plot = FALSE)
  breaks <- his$breaks
  nB <- length(breaks)
  y <- his$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
  # lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
}

# Creating the scatter plot matrix
pairs(data,
      upper.panel = NULL, # Disabling the upper panel
      diag.panel = panel.hist) 

# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  use = "complete.obs"
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y, use = "complete.obs")) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# Plotting the correlation matrix
pairs(data,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines


model <- lm(rkc.males$Right.Coxa.1st ~ rkc.males$Carapace.width.including.spines..mm.)
summary(model)

model <- lm(rkc.males$Right.Coxa.2nd ~ rkc.males$Carapace.width.including.spines..mm.)
summary(model)

model <- lm(rkc.males$Right.Coxa.3rd ~ rkc.males$Carapace.width.including.spines..mm.)
summary(model)

model <- lm(rkc.males$Left.Coxa.1st ~ rkc.males$Carapace.width.including.spines..mm.)
summary(model)

model <- lm(rkc.males$Left.Coxa.2nd ~ rkc.males$Carapace.width.including.spines..mm.)
summary(model)

model <- lm(rkc.males$Left.Coxa.3rd ~ rkc.males$Carapace.width.including.spines..mm.)
summary(model)


rkc.females <- subset(rkc, Sex == "F")
#rkc.females.cor <- cor(rkc.females[, unlist(lapply(rkc.males, is.numeric))], na.rm = FALSE)
rkc.females.cor <- cor(rkc.females[, unlist(lapply(rkc.males, is.numeric))])
View(rkc.females.cor)

corrplot(rkc.males.cor)

plot(rkc.males$Carapace.width.including.spines..mm., rkc.males$Right.Coxa.2nd)
abline()


ggplot(data = rkc.males, aes(x = Carapace.width.including.spines..mm., y = Right.Coxa.3rd, color = Area)) +
  labs(x = "Carapace width (mm)", y = "3rd right coxa (mm)") +
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
    
hist(rkc.males$Carapace.width.including.spines..mm., breaks = 20, xlab = "Carapace width (mm)", col = "lightgrey")    


## attempting a power analysis - agr- why are we doing this, we already have the data.
#install.packages("pwr")
library(pwr)
cohen.ES(test = "r", size = "large")
#agr below does not work for me
pwr.r.test(r = 0.98, sig.level = 0.05, power = 0.7)

pwr.f2.test(u = 1, f2 = 0.96/(1-0.96), sig.level = 0.001, power = 0.9)


ggplot(rkc.males, aes(x = Carapace.width.including.spines..mm., y = Right.Coxa.3rd)) + 
  geom_point() +
  geom_ribbon(aes(x = Carapace.width.including.spines..mm., y = Right.Coxa.3rd, ymin = obs_l95, ymax = obs_u95))
  

# data set including 2025 juneau + st james bay crab

newdata <- read.csv("data/full crab allometry dataset_USE.csv", header = TRUE)
View(newdata)  

males <- subset(newdata, Sex == "M")
View(males)
males[, 'Year'] <- as.factor(males[, 'Year'])
#males[, 'Species'] <- as.factor(males[, 'Species'])
males[, 'Sample.Number'] <- as.factor(males[, 'Sample.Number'])
  
males.cor <- cor(males[, unlist(lapply(males, is.numeric))], use = "complete.obs")
View(males.cor)

res <- melt(males.cor)
res$type=apply(res,1,function(x) 
  paste(sort(c(as.character(x[1]),as.character(x[2]))),collapse="*"))  
res=unique(res[,c("type","value")])  
View(res)

#chart.Correlation(males.cor, histogram = TRUE, method = "pearson")

#corrplot(males.cor, type = "lower", order = "original", diag = FALSE)
#corPlot(males.cor, method = "pearson")

plot(males$Carapace.width, males$Coxa.walking.leg.3.RIGHT, pch = 19, col = "lightblue")
abline(lm(males$Coxa.walking.leg.3.RIGHT ~ males$Carapace.width), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(males$Carapace.width, males$Coxa.walking.leg.3.RIGHT), 2)), x = 100, y = 45)


data <- males[, 8:15]
groups <- males[, 1:6]
pairs(data)
plot(data)

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  his <- hist(x, plot = FALSE)
  breaks <- his$breaks
  nB <- length(breaks)
  y <- his$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
  # lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
}

# Creating the scatter plot matrix
pairs(data,
      upper.panel = NULL, # Disabling the upper panel
      diag.panel = panel.hist) 

# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  use = "complete.obs"
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y, use = "complete.obs")) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# Plotting the correlation matrix
pairs(data,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines


model <- lm(males$Coxa.walking.leg.1.RIGHT ~ males$Carapace.width)
summary(model)

model <- lm(males$Coxa.walking.leg.2.RIGHT ~ males$Carapace.width)
summary(model)

model <- lm(males$Coxa.walking.leg.3.RIGHT ~ males$Carapace.width)
summary(model)

model <- lm(males$Coxa.walking.leg.1.LEFT ~ males$Carapace.width)
summary(model)

model <- lm(males$Coxa.walking.leg.2.LEFT ~ males$Carapace.width)
summary(model)

model <- lm(males$Coxa.walking.leg.3.LEFT ~ males$Carapace.width)
summary(model)


plot(males$Carapace.width ~ males$Coxa.walking.leg.2.RIGHT)
abline()



ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.3.RIGHT, shape = Area)) +
  labs(x = "Carapace width (mm)", y = "3rd right coxa (mm)") +
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

par(mfrow= c(3,2) )

r1<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.1.RIGHT, color = Legal)) +
  geom_vline(xintercept = 178) +
  geom_hline(yintercept = 30) + geom_hline(yintercept = 31) +geom_hline(yintercept = 32) +
  geom_hline(yintercept = 33) + geom_hline(yintercept = 34) + geom_hline(yintercept = 35) +
  labs(x = "Carapace width (mm)", y = "1st right coxa width (mm)") +
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

r2<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.2.RIGHT, color = Legal)) +
  geom_vline(xintercept = 178) +
  geom_hline(yintercept = 30) + geom_hline(yintercept = 31) +geom_hline(yintercept = 32) +
  geom_hline(yintercept = 33) + geom_hline(yintercept = 34) + geom_hline(yintercept = 35) +
  labs(x = "Carapace width (mm)", y = "2nd right coxa width (mm)") +
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

r3<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.3.RIGHT, color = Legal)) +
  geom_vline(xintercept = 178) +
  geom_hline(yintercept = 30) + geom_hline(yintercept = 31) +geom_hline(yintercept = 32) +
  geom_hline(yintercept = 33) + geom_hline(yintercept = 34) + geom_hline(yintercept = 35) +
  labs(x = "Carapace width (mm)", y = "3rd right coxa width (mm)") +
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

l1<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.1.LEFT, color = Legal)) +
  geom_vline(xintercept = 178) +
  geom_hline(yintercept = 30) + geom_hline(yintercept = 31) +geom_hline(yintercept = 32) +
  geom_hline(yintercept = 33) + geom_hline(yintercept = 34) + geom_hline(yintercept = 35) +
  labs(x = "Carapace width (mm)", y = "1st left coxa width (mm)") +
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

l2<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.2.LEFT, color = Legal)) +
  geom_vline(xintercept = 178) +
  geom_hline(yintercept = 30) + geom_hline(yintercept = 31) +geom_hline(yintercept = 32) +
  geom_hline(yintercept = 33) + geom_hline(yintercept = 34) + geom_hline(yintercept = 35) +
  labs(x = "Carapace width (mm)", y = "2nd left coxa width (mm)") +
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

l3<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.3.LEFT, color = Legal)) +
  geom_vline(xintercept = 178) +
  geom_hline(yintercept = 30) + geom_hline(yintercept = 31) +geom_hline(yintercept = 32) +
  geom_hline(yintercept = 33) + geom_hline(yintercept = 34) + geom_hline(yintercept = 35) +
  labs(x = "Carapace width (mm)", y = "3rd left coxa width (mm)") +
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


library(cowplot)
plot_grid(r1, r2, r3, l1, l2, l3, align = "v", ncol = 3, rel_heights = c(.5, .5))


par(mfrow = c(1, 1))

hist(males$Carapace.width, breaks = 20, xlab = "Carapace width (mm)", col = "lightgrey")    


# males by area
r1 <- #males %>% filter(Sex == "M" & is.na(Coxa.walking.leg.1.RIGHT) == FALSE) %>%
  ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.1.RIGHT)) +
  geom_point() +
  theme_bw() +
  geom_vline(xintercept = 178, linewidth = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 50)) +
  scale_x_continuous(limits = c(50, 250)) +
  facet_wrap(vars(Area)) +
  annotate("text", x = 180, y = 15, label = "Legal", angle = 90) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, level = 0.95) +
  stat_cor(aes(label = paste(..rr.label..)), 
           r.accuracy = 0.01,
           label.x = 60, label.y = 45, size = 4) #+
#stat_regline_equation(aes(label = ..eq.label..), 
#label.x = 0, label.y = 45, size = 4)



prop.table(table(males$Coxa.walking.leg.1.RIGHT))


#another Alex manipulation
#which is the largest coxa??
names(males)
mean(na.omit(males$Coxa.walking.leg.1.RIGHT))
mean(na.omit(males$Coxa.walking.leg.2.RIGHT))
mean(na.omit(males$Coxa.walking.leg.3.RIGHT))
mean(na.omit(males$Coxa.walking.leg.1.LEFT))
mean(na.omit(males$Coxa.walking.leg.2.LEFT))
mean(na.omit(males$Coxa.walking.leg.3.LEFT))

median(na.omit(males$Coxa.walking.leg.1.RIGHT))
median(na.omit(males$Coxa.walking.leg.2.RIGHT))
median(na.omit(males$Coxa.walking.leg.3.RIGHT))
median(na.omit(males$Coxa.walking.leg.1.LEFT))
median(na.omit(males$Coxa.walking.leg.2.LEFT))
median(na.omit(males$Coxa.walking.leg.3.LEFT))

#second two legs. WL 2 works fine. WL 3 doesnt look bigger. It is more tightly correlated to CW, slightly

ggplot(males) + aes() + #AGR WIP
  geom_point(aes(x=Coxa.walking.leg.2.RIGHT, y= Carapace.width), color = "purple", alpha = 0.3) + #I see more variation in purple
  geom_smooth(aes(x=Coxa.walking.leg.2.RIGHT, y= Carapace.width), color = "purple", method = "lm")+
  geom_point(aes(x=Coxa.walking.leg.3.RIGHT, y= Carapace.width), color = "orange", alpha = 0.2)+
  geom_smooth(aes(x=Coxa.walking.leg.3.RIGHT, y= Carapace.width), color = "orange", method = "lm")

#Alex manipulations
##so coxa at 31 or 32
# - less than 31 or 32 being sublegal
View(males)
#how many males at <31 are legal? Going to use the max coxa size
library(tidyverse)
less_than_31 <- males %>%
  rowwise() %>%
  mutate(max_coxa =  max(c_across(c(Coxa.walking.leg.1.RIGHT, Coxa.walking.leg.2.RIGHT, Coxa.walking.leg.3.RIGHT, Coxa.walking.leg.1.LEFT, Coxa.walking.leg.2.LEFT, Coxa.walking.leg.3.LEFT)))
         ) %>%
  ungroup() %>%
  select(Sample.Number, Year, Area, Sex, Legal, Carapace.length, Carapace.width, Comments, max_coxa) %>%
  filter(max_coxa < 31)
#how many of these are legal?
less_than_31 %>% filter(Legal != "Sublegal") #none of them...

#how many males at <32 are legal?
less_than_32 <- males %>%
  rowwise() %>%
  mutate(max_coxa =  max(c_across(c(Coxa.walking.leg.1.RIGHT, Coxa.walking.leg.2.RIGHT, Coxa.walking.leg.3.RIGHT, Coxa.walking.leg.1.LEFT, Coxa.walking.leg.2.LEFT, Coxa.walking.leg.3.LEFT)))
  ) %>%
  ungroup() %>%
  select(Sample.Number, Year, Area, Sex, Legal, Carapace.length, Carapace.width, Comments, max_coxa) %>%
  filter(max_coxa < 32)
#how many of these are legal?
less_than_32 %>% filter(Legal != "Sublegal") #two of them
#what percentage is this?
str(males)
2/824 #= 0.2%


# a statistical test here?
# can coxa width tell legal or nonlegal? Yeah it can. Can a coxa width of 32 tell legal or nonlegal
# legality ~ coxa width = 32

#what's the 99% CI's on spencer's models? She didn't use max coxa, I can do that here.
names(males)
males_2 <- males %>%
  rowwise() %>%
  mutate(max_coxa =  max(c_across(c(Coxa.walking.leg.1.RIGHT, Coxa.walking.leg.2.RIGHT, Coxa.walking.leg.3.RIGHT, Coxa.walking.leg.1.LEFT, Coxa.walking.leg.2.LEFT, Coxa.walking.leg.3.LEFT)))
  ) %>%
  ungroup()

#does max coxa predict legality?

#does max coxa predict carapace width
mod <- lm(data=males_2, Carapace.width~max_coxa)
summary(mod)

#mod2 <- lm(data=males_2, Carapace.width~log(max_coxa))#nope
#summary(mod2)

ggplot(males_2) + aes(x=max_coxa)+ geom_density()

(ggplot(males_2) + aes(x=max_coxa, y= Carapace.width) + geom_point()+
  geom_smooth(method="lm")+ geom_hline(yintercept = 178, color="red", linetype="dashed")+
  geom_vline(xintercept = 32, color="purple", linetype="dotdash", size=1)+
  geom_vline(xintercept = 31, color="purple", linetype="dotdash", size=1) -> max_coxa_graph)

ggsave("figures/max coxa graph.png", max_coxa_graph, width =10, height = 6, dpi = 300)

#ggplot(males_2) + aes(x=log(max_coxa), y= Carapace.width) + geom_point()+
 # geom_smooth(method="lm")+ geom_hline(yintercept = 178, color="red", linetype="dashed")+
  #geom_vline(xintercept = log(32), color="purple", linetype="dotdash", size=1)+
  #geom_vline(xintercept = log(31), color="purple", linetype="dotdash", size=1)



#what are the 99% CI's at a max coxa of 31? 32?
##with a se of 0.04461 on max 
confint(mod, level = 0.99)
confint(mod)


#predict 
newdat <- data.frame(max_coxa = 32)
predict(mod, newdat, interval = "confidence", level= 0.95)
predict(mod, newdat, interval = "confidence", level= 0.99)

newdat <- data.frame(max_coxa = 31)
predict(mod, newdat, interval = "confidence", level= 0.95)
predict(mod, newdat, interval = "confidence", level= 0.99)

#so the CI's are pretty tight, far away from 178 (the legal threshold)

#what if a binomial model, legal or not legal?
##dplyr that so 0 if not legal and 1 if legal
unique(males_2$Legal)
males_2 <- males_2 %>%
  mutate(legal_bin = if_else(Legal == "Legal", 1, 0))

mod3 <- glm(legal_bin ~ max_coxa, data = males_2, family = binomial)
summary(mod3)

#predicted probability for binomial model
newdat <- data.frame(max_coxa = c(28, 29, 30, 31, 32, 33))
rate_predicted <- predict(mod3, newdata = newdat, type = "response")
#so 31 has a 10% prob of being legal...
#32 has a 24% prob of being legal? How is that??
#make that a table
max_coxa = c(28, 29, 30, 31, 32, 33)
binom_df<-data.frame(max_coxa, rate_predicted)
write.csv(binom_df, "results/binom_df.csv")



#CI's
confint(mod3)
newdat <- data.frame(max_coxa = 32)
pred <- predict(mod3, newdata = newdat, type = "link", se.fit = TRUE)
# Transform from log-odds to probability scale
fit <- plogis(pred$fit)
lwr <- plogis(pred$fit - 1.96 * pred$se.fit)
upr <- plogis(pred$fit + 1.96 * pred$se.fit)
data.frame(max_coxa = 32, fit, lwr, upr)


newdat <- data.frame(max_coxa = 31)
pred <- predict(mod3, newdata = newdat, type = "link", se.fit = TRUE)
# Transform from log-odds to probability scale
fit <- plogis(pred$fit)
lwr <- plogis(pred$fit - 1.96 * pred$se.fit)
upr <- plogis(pred$fit + 1.96 * pred$se.fit)
data.frame(max_coxa = 31, fit, lwr, upr)


newdat <- data.frame(max_coxa = 33)
pred <- predict(mod3, newdata = newdat, type = "link", se.fit = TRUE)
# Transform from log-odds to probability scale
fit <- plogis(pred$fit)
lwr <- plogis(pred$fit - 1.96 * pred$se.fit)
upr <- plogis(pred$fit + 1.96 * pred$se.fit)
data.frame(max_coxa = 33, fit, lwr, upr) #.46


#plot binomial
(ggplot(males_2, aes(x = max_coxa, y = legal_bin)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) -> binom_plot)
ggsave("figures/binomial plot.png", binom_plot, width =10, height = 6, dpi = 300)


#how many obs are at 31 and 32?
dim(males_2 %>% filter(max_coxa == 31)) #40 obs
dim(males_2 %>% filter(max_coxa == 32)) #43 obs


#half baked attempt
thresholds <- 28:33  # the coxa thresholds you care about







