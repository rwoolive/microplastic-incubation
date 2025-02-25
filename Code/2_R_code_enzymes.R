rm(list = ls(all = TRUE))
graphics.off()
shell("cls")


# Load necessary libraries
library(readxl)
#library(tibble)
library(dplyr)
#library(lubridate)
library(ggplot2)
library(zoo)

#install.packages("ggpubr") #an extension of ggplot2
library(ggpubr)

#install.packages("patchwork")
library(patchwork)

#install.packages("multcomp")
library(multcomp) # for contrast
library(multcompView)#for abc

#install.packages("car")  # for levene's test for homogenety of variance
library(car)

# library(car)
library(lattice)     # for densityplot()
#library(nlme)
library(lme4)

# colors for differnt levels 
colplas <- c("black", "red", "orange", "blue", "purple")
colnit <- c("lightblue","salmon")
colplas <- c("seashell2","goldenrod4")


# CO2 data
dat_CO2 <- read.csv("Processed-data/CO2Flux_Long_Interpolated_Cumulative.csv")
unique(dat_CO2$Tube) # Jars 121-160
# factors
dat_CO2 <- dat_CO2 %>%
  mutate(Plastic = factor(Plastic,levels = c("NONE", "LDPE", "PBS", "PLA", "PLA/PHA")))%>%
  mutate(Nitrogen = factor(Plastic,levels = c("N0", "N1")))


# Soil data
#data <- read_excel("Raw-data/Enzyme data for avi MP incubation soils.xlsx", sheet='final enzyme data')
dat_soil <- read.csv("Processed-data/All Wet Chemistry Data_final.csv")
unique(dat_soil$Jar.No) # Jars 1-160
# factors
dat_soil <- dat_soil %>%
  mutate(Plastic = factor(Plastic,levels = c("NONE", "LDPE", "PBS", "PLA", "PLA/PHA")))%>%
  mutate(Nitrogen = factor(Nitrogen,levels = c("N0", "N1")))%>%
  mutate(Time = factor(Time,levels = c("5D", "15D", "30D", "193D")))










#Make a Bar plot
####For BG ###
# Calculate mean and standard error
se_dat_soil <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_BG = mean(`BG`),
            se_BG = sd(`BG`) / sqrt(n())) %>%
  ungroup()

# Merge the original dat_soil with the standard error dat_soil
dat_soil_with_se <- merge(dat_soil, se_dat_soil, by = c("Time", "Plastic", "Nitrogen"))

# Create the bar diagram with dat_soil points and error bars
p1 <- ggplot(dat_soil_with_se, aes(x = Plastic, y = mean_BG, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(.~Time) +
  scale_fill_manual(values = colnit) + 
  geom_errorbar(aes(ymin = mean_BG - se_BG, ymax = mean_BG + se_BG),
                position = position_dodge(width = 0.8), width = 0.25) +
  # geom_point(aes(y = `BG nmols/g/h`), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
  #            color = "black", size = 1) +
  labs(x = "Plastic",
       y = expression(paste(beta,"-glucosidase (nmol g"^-1," hr"^-1,")"))) +
  theme_bw()
p1
p1 <- p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"),
                 axis.title.y = element_text(size=8))
# p1 <- p1 + theme(axis.text.y = element_text(size=13,color="black"))
# p1 <- p1 + theme(text = element_text(size = 15, color="black"))
ggsave(p1, file="Figures/enzymes-BG-average-se.png",width = 6, height = 3, dpi = 300)



#Make a Bar plot
####For CB ###
# Calculate mean and standard error
se_dat_soil <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_CB = mean(`CB nmols/g/h`),
            se_CB = sd(`CB nmols/g/h`) / sqrt(n())) %>%
  ungroup()

# Merge the original dat_soil with the standard error dat_soil
dat_soil_with_se <- merge(dat_soil, se_dat_soil, by = c("Time", "Plastic", "Nitrogen"))

# Create the bar diagram with dat_soil points and error bars
p1 <- ggplot(dat_soil_with_se, aes(x = Plastic, y = mean_CB, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(.~Time) +
  scale_fill_manual(values = colnit) + 
  geom_errorbar(aes(ymin = mean_CB - se_CB, ymax = mean_CB + se_CB),
                position = position_dodge(width = 0.8), width = 0.25) +
  # geom_point(aes(y = `CB nmols/g/h`), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
  #            color = "black", size = 1) +
  labs(x = "Plastic",
       y = expression(paste(beta,"-D-cellobiosidase (nmol g"^-1," hr"^-1,")"))) +
  theme_bw()
p1
p1 <- p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"),
                 axis.title.y = element_text(size=8))
# p1 <- p1 + theme(axis.text.y = element_text(size=13,color="black"))
# p1 <- p1 + theme(text = element_text(size = 15, color="black"))
ggsave(p1, file="Figures/enzymes-CB-average-se.png",width = 6, height = 3, dpi = 300)






#Make a Bar plot
####For XYL ###
# Calculate mean and standard error
se_dat_soil <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_XYL = mean(`XYL nmols/g/h`),
            se_XYL = sd(`XYL nmols/g/h`) / sqrt(n())) %>%
  ungroup()

# Merge the original dat_soil with the standard error dat_soil
dat_soil_with_se <- merge(dat_soil, se_dat_soil, by = c("Time", "Plastic", "Nitrogen"))

# Create the bar diagram with dat_soil points and error bars
p1 <- ggplot(dat_soil_with_se, aes(x = Plastic, y = mean_XYL, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(.~Time) +
  scale_fill_manual(values = colnit) + 
  geom_errorbar(aes(ymin = mean_XYL - se_XYL, ymax = mean_XYL + se_XYL),
                position = position_dodge(width = 0.8), width = 0.25) +
  # geom_point(aes(y = `XYL nmols/g/h`), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
  #            color = "black", size = 1) +
  labs(x = "Plastic",
       y = expression(paste(beta,"-xylosidase (nmol g"^-1," hr"^-1,")"))) +
  theme_bw()
p1
p1 <- p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"),
                 axis.title.y = element_text(size=8))
# p1 <- p1 + theme(axis.text.y = element_text(size=13,color="black"))
# p1 <- p1 + theme(text = element_text(size = 15, color="black"))
ggsave(p1, file="Figures/enzymes-XYL-average-se.png",width = 6, height = 3, dpi = 300)





#Make a Bar plot
####For LAP ###
# Calculate mean and standard error
se_dat_soil <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_LAP = mean(`LAP nmols/g/h`),
            se_LAP = sd(`LAP nmols/g/h`) / sqrt(n())) %>%
  ungroup()

# Merge the original dat_soil with the standard error dat_soil
dat_soil_with_se <- merge(dat_soil, se_dat_soil, by = c("Time", "Plastic", "Nitrogen"))

# Create the bar diagram with dat_soil points and error bars
p1 <- ggplot(dat_soil_with_se, aes(x = Plastic, y = mean_LAP, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(.~Time) +
  scale_fill_manual(values = colnit) + 
  geom_errorbar(aes(ymin = mean_LAP - se_LAP, ymax = mean_LAP + se_LAP),
                position = position_dodge(width = 0.8), width = 0.25) +
  # geom_point(aes(y = `LAP nmols/g/h`), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
  #            color = "black", size = 1) +
  labs(x = "Plastic",
       y = expression(paste("Leucine aminopeptidase (nmol g"^-1," hr"^-1,")"))) +
  theme_bw()
p1
p1 <- p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"),
                 axis.title.y = element_text(size=8))
# p1 <- p1 + theme(axis.text.y = element_text(size=13,color="black"))
# p1 <- p1 + theme(text = element_text(size = 15, color="black"))
ggsave(p1, file="Figures/enzymes-LAP-average-se.png",width = 6, height = 3, dpi = 300)

