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
#library(ggpubr)

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
#library(lme4)

# colors for differnt levels 
colplas <- c("black", "red", "orange", "blue", "purple")
colnit <- c("lightblue","salmon")
colplas <- c("seashell2","goldenrod4")


#data <- read_excel("C:/UTK one drive/OneDrive - University of Tennessee/Biosystems Engineering/Mn Project/Mn Lab Incubation/Supplemental Experiment/All Soil Parameters_suppl.xlsx", sheet='SoilProp')

#data <- read_excel("C:/UTK one drive/OneDrive - University of Tennessee/Biosystems Engineering/Microplastic Project/Incubation data/Enzyme data/Enzyme data for avi MP incubation soils.xlsx", sheet='enzymedataplot')

data <- read_excel("Raw-data/Enzyme data for avi MP incubation soils.xlsx", sheet='final enzyme data')
data <- data[-which(data$Time=="T0"),]
head(data)
#C:\UTK one drive\OneDrive - University of Tennessee\Biosystems Engineering\Microplastic Project\Incubation data\Enzyme data

str(data)

data$Plastic <- as.factor(data$Plastic)
data$Plastic <- factor(data$Plastic, levels(data$Plastic)[c(2,1,3,4,5)]) # to control my order of treatments
data <- data %>%
  mutate(Time = recode(Time,  "T1" = "5D",  "T2" = "15D", "T3" = "30D", "T4" = "193D"))  # Rename Times
data$Time <- as.factor(data$Time) # to control my order of time
data$Nitrogen <- as.factor(data$Nitrogen) # to control my order of time

# dataN0 <- subset(data, Nitrogen=="N0")
# dataN1 <- subset(data, Nitrogen=="N1")
# 
# 
# data5 <- subset(data, Time=="5D")
# data15 <- subset(data, Time=="15D")
# data30 <- subset(data, Time=="30D")
# data193 <- subset(data, Time=="193D")



#Make a Bar plot
####For BG ###
# Calculate mean and standard error
se_data <- data %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_BG = mean(`BG nmols/g/h`),
            se_BG = sd(`BG nmols/g/h`) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(data, se_data, by = c("Time", "Plastic", "Nitrogen"))

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Plastic, y = mean_BG, fill = Nitrogen)) +
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
se_data <- data %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_CB = mean(`CB nmols/g/h`),
            se_CB = sd(`CB nmols/g/h`) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(data, se_data, by = c("Time", "Plastic", "Nitrogen"))

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Plastic, y = mean_CB, fill = Nitrogen)) +
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
se_data <- data %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_XYL = mean(`XYL nmols/g/h`),
            se_XYL = sd(`XYL nmols/g/h`) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(data, se_data, by = c("Time", "Plastic", "Nitrogen"))

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Plastic, y = mean_XYL, fill = Nitrogen)) +
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
se_data <- data %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_LAP = mean(`LAP nmols/g/h`),
            se_LAP = sd(`LAP nmols/g/h`) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(data, se_data, by = c("Time", "Plastic", "Nitrogen"))

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Plastic, y = mean_LAP, fill = Nitrogen)) +
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

