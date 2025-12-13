#AGR cleaning up spencer's exploratory dataset by a similar name
#12/12/25

library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(PerformanceAnalytics)
library(reshape2)
library(dplyr)

# data set including 2025 juneau + st james bay crab

newdata <- read.csv("data/full crab allometry dataset_USE.csv", header = TRUE)
#View(newdata)  

males <- subset(newdata, Sex == "M")
#View(males)
males[, 'Year'] <- as.factor(males[, 'Year'])
#males[, 'Species'] <- as.factor(males[, 'Species'])
males[, 'Sample.Number'] <- as.factor(males[, 'Sample.Number'])
  
males.cor <- cor(males[, unlist(lapply(males, is.numeric))], use = "complete.obs")
#View(males.cor)

res <- melt(males.cor)
res$type=apply(res,1,function(x) 
  paste(sort(c(as.character(x[1]),as.character(x[2]))),collapse="*"))  
res=unique(res[,c("type","value")])  
#View(res)

#chart.Correlation(males.cor, histogram = TRUE, method = "pearson")

#corrplot(males.cor, type = "lower", order = "original", diag = FALSE)
#corPlot(males.cor, method = "pearson")

plot(males$Carapace.width, males$Coxa.walking.leg.3.RIGHT, pch = 19, col = "lightblue")
abline(lm(males$Coxa.walking.leg.3.RIGHT ~ males$Carapace.width), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(males$Carapace.width, males$Coxa.walking.leg.3.RIGHT), 2)), x = 100, y = 45)

####AGR START 12/12/25
males_borderline <- males %>% filter(Carapace.length > 137 & Carapace.length < 156) #CL from 138-155
CW_mean <- mean(males_borderline$Carapace.width)
CW_sd <- sd(males_borderline$Carapace.width)
CW_mean - 1.96*CW_sd #160
CW_mean + 1.96*CW_sd #191 , this is 192 at 2*sd

Legality <- 178 #7 inches

Legality - 2*CW_sd #162
Legality + 2*CW_sd #194 #both of these- same at 2*sd same results as 1.96*sd

#####AGR END 12/12/25

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

#par(mfrow= c(3,2) )
library(adfggraph)

r1<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.1.RIGHT, color = Legal)) +
  geom_vline(xintercept = 178) +
  #geom_hline(yintercept = 30) + 
  geom_hline(yintercept = 31, linetype= "dashed") +
  geom_hline(yintercept = 32, linetype = "dashed") +
  geom_hline(yintercept = 33, linetype = "dashed") + 
  #geom_hline(yintercept = 34) + 
  #geom_hline(yintercept = 35) +
  #labs(x = "Carapace width (mm)", y = "1st right coxa width (mm)") +
  labs(x = NULL, y = "1st right coxa width (mm)") +
  #ggtitle("Right coxa")+
  geom_point(alpha = 0.5) + 
  scale_y_continuous(limits = c(11,51), expand = c(0,0), breaks = c(20, 30, 40, 50))+ #let's set a consistent y axis.
  scale_color_manual(values = c("#6FB9D6", "#FF5A3C"), guide="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    theme_adfg(font_family="Arial", box = FALSE) #might have to change point size.

r2<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.2.RIGHT, color = Legal)) +
  geom_vline(xintercept = 178) +
  #geom_hline(yintercept = 30) + 
  geom_hline(yintercept = 31, linetype= "dashed") +
  geom_hline(yintercept = 32, linetype = "dashed") +
  geom_hline(yintercept = 33, linetype = "dashed") + 
  #geom_hline(yintercept = 34) + 
  #geom_hline(yintercept = 35) +
  #labs(x = "Carapace width (mm)", y = "1st right coxa width (mm)") +
  labs(x = NULL, y = "2nd right coxa width (mm)") +
  geom_point(alpha=0.5) + 
  scale_y_continuous(limits = c(11,51), expand = c(0,0), breaks = c(20, 30, 40, 50))+ #let's set a consistent y axis.
  scale_color_manual(values = c("#6FB9D6", "#FF5A3C"), guide="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme_adfg(font_family="Arial", box = FALSE) #might have to change point size.

r3<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.3.RIGHT, color = Legal)) +
  geom_vline(xintercept = 178) +
  #geom_hline(yintercept = 30) + 
  geom_hline(yintercept = 31, linetype= "dashed") +
  geom_hline(yintercept = 32, linetype = "dashed") +
  geom_hline(yintercept = 33, linetype = "dashed") + 
  #geom_hline(yintercept = 34) + 
  #geom_hline(yintercept = 35) +
  labs(x = "Carapace width (mm)", y = "3rd right coxa width (mm)") +
  #labs(x = NULL, y = "1st right coxa width (mm)") +
  geom_point(alpha=0.5) + 
  scale_y_continuous(limits = c(11,51), expand = c(0,0), breaks = c(20, 30, 40, 50))+ #let's set a consistent y axis.
  scale_color_manual(values = c("#6FB9D6", "#FF5A3C"), guide="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme_adfg(font_family="Arial", box = FALSE) #might have to change point size.

l1<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.1.LEFT, color = Legal)) +
  geom_vline(xintercept = 178) +
  #geom_hline(yintercept = 30) + 
  geom_hline(yintercept = 31, linetype= "dashed") +
  geom_hline(yintercept = 32, linetype = "dashed") +
  geom_hline(yintercept = 33, linetype = "dashed") + 
  #geom_hline(yintercept = 34) + 
  #geom_hline(yintercept = 35) +
  #labs(x = "Carapace width (mm)", y = "1st right coxa width (mm)") +
  labs(x = NULL, y = "1st left coxa width (mm)") +
  geom_point(alpha=0.5) + 
  scale_y_continuous(limits = c(11,51), expand = c(0,0), breaks = c(20, 30, 40, 50))+ #let's set a consistent y axis.
  scale_color_manual(values = c("#6FB9D6", "#FF5A3C"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme_adfg(font_family="Arial", box = FALSE, legend.position = c(0.15, 0.93), legend.title=element_blank(),
             legend.background = element_rect(
               fill = "white", color="black", linewidth = 0.3
             )) #might have to change point size.

l2<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.2.LEFT, color = Legal)) +
  geom_vline(xintercept = 178) +
  #geom_hline(yintercept = 30) + 
  geom_hline(yintercept = 31, linetype= "dashed") +
  geom_hline(yintercept = 32, linetype = "dashed") +
  geom_hline(yintercept = 33, linetype = "dashed") + 
  #geom_hline(yintercept = 34) + 
  #geom_hline(yintercept = 35) +
  #labs(x = "Carapace width (mm)", y = "1st right coxa width (mm)") +
  labs(x = NULL, y = "2nd left coxa width (mm)") +
  geom_point(alpha=0.5) + 
  scale_y_continuous(limits = c(11,51), expand = c(0,0), breaks = c(20, 30, 40, 50))+ #let's set a consistent y axis.
  scale_color_manual(values = c("#6FB9D6", "#FF5A3C"), guide="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme_adfg(font_family="Arial", box = FALSE) #might have to change point size.

l3<-ggplot(data = males, aes(x = Carapace.width, y = Coxa.walking.leg.3.LEFT, color = Legal)) +
  geom_vline(xintercept = 178) +
  #geom_hline(yintercept = 30) + 
  geom_hline(yintercept = 31, linetype= "dashed") +
  geom_hline(yintercept = 32, linetype = "dashed") +
  geom_hline(yintercept = 33, linetype = "dashed") + 
  #geom_hline(yintercept = 34) + 
  #geom_hline(yintercept = 35) +
  labs(x = "Carapace width (mm)", y = "3rd left coxa width (mm)") +
  #labs(x = NULL, y = "1st right coxa width (mm)") +
  geom_point(alpha=0.5) + 
  scale_y_continuous(limits = c(11,51), expand = c(0,0), breaks = c(20, 30, 40, 50))+ #let's set a consistent y axis.
  scale_color_manual(values = c("#6FB9D6", "#FF5A3C"), guide="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme_adfg(font_family="Arial", box = FALSE) #might have to change point size.


#library(cowplot)
#base <- plot_grid(r1, r2, r3, l1, l2, l3, align = "v", ncol = 3, rel_heights = c(.5, .5))
#base <- plot_grid(r1, l1, r2, l2, r3, l3, ncol = 2)#make it vertical
#soemthing is wrong with me fonts- agr
#gather legends, put within one of the plots
#theme_adfg
#R2 values in the top R corner. (counts as my sig test I think)

library(patchwork)
  (l1 + r1) /
  (l2 + r2) /
  (l3 + r3) -> paper_fig

ggsave(plot = paper_fig, filename = "figures/Coxa width carapace width paper version.png", width =12, height = 16, dpi = 300) #idk what height

#poster edit: title is Left coxa; Right coxa
#poster y axis: first, second, thir

#AGR here. This graph would work.

#POSTER EDIT:
l1 <- l1 + labs(y=NULL)
l2 <- l2 + labs(y=NULL)
l3 <- l3 + labs(y=NULL)

r1 <- r1 + labs(y=NULL)
r2 <- r2 + labs(y=NULL)
r3 <- r3 + labs(y=NULL)

  (l1 + r1) /
  (l2 + r2) /
  (l3 + r3) -> nolabs

ggsave(plot = nolabs, filename = "figures/Coxa width carapace width poster version.png", width =12, height = 16, dpi = 300) #idk what height


#ggsave that poster


#par(mfrow = NULLpar(mfrow = c(1, 1))

hist(males$Carapace.width, breaks = 20, xlab = "Carapace width (mm)", col = "lightgrey")    


# males by area
library(ggpubr)
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
#View(males)
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


###AGR 12/12/25
##I need to put something in the poster.
##about correlation
##I could 1: show carapace/length correlation for all legs
## 2: show carapace/correlation for the second walking leg -is correlation the tightest here
## 3 : show carapace/correlation for the max leg

##I think #2 + a table of CL and coxa correlation for all (L+R legs, 1,2,3)

#the graph:


#the table:




