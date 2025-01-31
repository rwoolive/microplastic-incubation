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

data <- read_excel("C:/UTK one drive/OneDrive - University of Tennessee/Biosystems Engineering/Microplastic Project/Incubation data/All Wet Chemistry Data_final.xlsx", sheet='CumCO2')
head(data)

str(data)



data$Treatment <- factor(data$Treatment, levels = c("NONE-N0", "LDPE-N0", "PBS-N0", "PLA/PHA-N0", "PLA-N0", "NONE-N1", "LDPE-N1", "PBS-N1", "PLA/PHA-N1", "PLA-N1")) # to control my order of treatments
#data$Time <- factor(data$Time, levels = c("5D", "15D", "30D", "193D")) # to control my order of time
data$Nitrogen <- factor(data$Nitrogen, levels = c("N0", "N1")) # to control my order of time



dataN0 <- subset(data, Nitrogen=="N0")
dataN1 <- subset(data, Nitrogen=="N1")




############################
###CO@###
###########################
#Full model with correct error structure

CumCO2mod1<-aov(CumCO2~Nitrogen*Plastic, data=data)
summary(CumCO2mod1)
anova(CumCO2mod1)

# histogram
hist(CumCO2mod1$residuals)

# QQ-plot
qqPlot(CumCO2mod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(CumCO2mod1$residuals)
#P-value of the Shapiro-Wilk test on the residuals is > than  α=  5%, so we residuals  seem to follow a normal distribution (p-value >0.05).


ks.test(CumCO2mod1$residuals, "pnorm", mean = mean(CumCO2mod1$residuals),
        sd = sd(CumCO2mod1$residuals))


# Kolmogorov–Smirnov test with p>0.05 indicates normal distribution

leveneTest(CumCO2~Nitrogen*Plastic, data=data)

#The p-value being larger than the significance level of 0.05, we do not reject the null hypothesis, so we cannot reject the hypothesis that variances are equal between treatments (p-value >0.05).

#This result is also in line with the visual approach, so the homogeneity of variances is met both visually and formally.


tukeyCumCO2<-TukeyHSD(CumCO2mod1)
tukey.cldCumCO2 <- multcompLetters4(CumCO2mod1, tukeyCumCO2)
print(tukey.cldCumCO2)

