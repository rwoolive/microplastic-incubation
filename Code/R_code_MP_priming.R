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

#data <- read_excel("C:/UTK one drive/OneDrive - University of Tennessee/Biosystems Engineering/Microplastic Project/Incubation data/All Wet Chemistry Data_final.xlsx", sheet='Prim')

data <- read_excel("Raw-data/All Wet Chemistry Data_final.xlsx", sheet='Priming')

head(data)

str(data)



data$Treatment <- factor(data$Treatment, levels = c( "LDPE-N0", "PBS-N0", "PLA/PHA-N0", "PLA-N0", "LDPE-N1", "PBS-N1", "PLA/PHA-N1", "PLA-N1")) # to control my order of treatments
#data$Time <- factor(data$Time, levels = c("5D", "15D", "30D", "193D")) # to control my order of time
data$Nitrogen <- factor(data$Nitrogen, levels = c("N0", "N1")) # to control my order of time



dataN0 <- subset(data, Nitrogen=="N0")
dataN1 <- subset(data, Nitrogen=="N1")




############################
###CO2 Priming ###
###########################
#Full model with correct error structure

Primemod1<-aov(Prime2~Nitrogen*Plastic, data=data)
summary(Primemod1)
anova(Primemod1)

# histogram
hist(Primemod1$residuals)

# QQ-plot
qqPlot(Primemod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(Primemod1$residuals)
#P-value of the Shapiro-Wilk test on the residuals is > than  α=  5%, so we residuals  seem to follow a normal distribution (p-value >0.05).


ks.test(Primemod1$residuals, "pnorm", mean = mean(Primemod1$residuals),
        sd = sd(Primemod1$residuals))


# Kolmogorov–Smirnov test with p>0.05 indicates normal distribution

leveneTest(Prime~Nitrogen*Plastic, data=data)

#The p-value being larger than the significance level of 0.05, we do not reject the null hypothesis, so we cannot reject the hypothesis that variances are equal between treatments (p-value >0.05).

#This result is also in line with the visual approach, so the homogeneity of variances is met both visually and formally.


tukeyPrime<-TukeyHSD(Primemod1)
tukey.cldPrime <- multcompLetters4(Primemod1, tukeyPrime)
print(tukey.cldPrime)





#Make a Bar plot
####For Prime ###
# Calculate mean and standard error
se_data <- data %>%
  group_by(Treatment) %>%
  summarize(mean_Prime = mean(Prime2),
            se_Prime = sd(Prime2) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(data, se_data, by = c("Treatment"))

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Treatment, y = mean_Prime, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_Prime - se_Prime, ymax = mean_Prime + se_Prime),
                position = position_dodge(width = 0.8), width = 0.25) +
  #geom_point(aes(y = Prime), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
  #         color = "black", size = 1) +
  labs(title = "Priming",
       x = " ",
       y = "Priming (ug C/g soil)",
       fill = "Treatment") +
  theme_minimal() +
  scale_fill_manual(values = c("NONE-N0" = "dodgerblue", "LDPE-N0" = "dodgerblue3", "PBS-N0" = "blue", "PLA/PHA-N0" = "blue3","PLA-N0" = "blue4",
                               "NONE-N1" =   "indianred3", "LDPE-N1" = "salmon", "PBS-N1" = "red", "PLA/PHA-N1" = "red3" ,"PLA-N1" = "red4")) + 
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p1 <- p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=13,color="black"))
p1 <- p1 + theme(axis.text.y = element_text(size=13,color="black"))
p1 <- p1 + theme(text = element_text(size = 15, color="black"))
print(p1)