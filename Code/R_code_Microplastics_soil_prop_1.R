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

data <- read_excel("C:/UTK one drive/OneDrive - University of Tennessee/Biosystems Engineering/Microplastic Project/Incubation data/All Wet Chemistry Data_final.xlsx", sheet='all_data_R')
head(data)

str(data)



data$Treatment <- factor(data$Treatment, levels = c("NONE-N0", "LDPE-N0", "PBS-N0", "PLA/PHA-N0", "PLA-N0", "NONE-N1", "LDPE-N1", "PBS-N1", "PLA/PHA-N1", "PLA-N1")) # to control my order of treatments
data$Time <- factor(data$Time, levels = c("5D", "15D", "30D", "193D")) # to control my order of time
data$Nitrogen <- factor(data$Nitrogen, levels = c("N0", "N1")) # to control my order of time



dataN0 <- subset(data, Nitrogen=="N0")
dataN1 <- subset(data, Nitrogen=="N1")


data5 <- subset(data, Time=="5D")
data15 <- subset(data, Time=="15D")
data30 <- subset(data, Time=="30D")
data193 <- subset(data, Time=="193D")


dataN05 <- subset(dataN0, Time=="5D")
dataN015 <- subset(dataN0, Time=="15D")
dataN050 <- subset(dataN0, Time=="30D")
dataN0193 <- subset(dataN0, Time=="193D")

dataN15 <- subset(dataN0, Time=="5D")
dataN115 <- subset(dataN0, Time=="15D")
dataN150 <- subset(dataN0, Time=="30D")
dataN1193 <- subset(dataN0, Time=="193D")


#Make a Bar plot
####For DOC ###
# Calculate mean and standard error
se_data <- data %>%
  group_by(Time, Treatment) %>%
  summarize(mean_DOC = mean(DOC),
            se_DOC = sd(DOC) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(data, se_data, by = c("Time", "Treatment"))

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_DOC, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_DOC - se_DOC, ymax = mean_DOC + se_DOC),
                position = position_dodge(width = 0.8), width = 0.25) +
  #geom_point(aes(y = DOC), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
  #         color = "black", size = 1) +
  labs(title = "Dissolved Organic Carbon",
       x = " ",
       y = "Dissolved Organic Carbon (mg C/kg soil)",
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





# Create the bar diagram with data points and error bars
p11 <- ggplot(data_with_se, aes(x = Treatment, y = mean_DOC, fill = Time)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_DOC - se_DOC, ymax = mean_DOC + se_DOC),
                position = position_dodge(width = 0.8), width = 0.25) +
  #geom_point(aes(y = DOC), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
  #          color = "black", size = 1) +
  labs(title = "Dissolved Organic Carbon",
       x = " ",
       y = "Dissolved Organic Carbon (mg C/kg soil)",
       fill = "Treatment") +
  theme_minimal() +
  scale_fill_manual(values = c("5D" = "blue", "15D" = "blue2", "30D" = "blue3", "193D" = "blue4")) + 
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p11 <- p11 + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=13,color="black"))
p11 <- p11 + theme(axis.text.y = element_text(size=13,color="black"))
p11 <- p11 + theme(text = element_text(size = 15, color="black"))
print(p11)




############################
###DOC###
###########################
#Full model with correct error structure

DOCmod1<-aov(DOC~Nitrogen*Plastic*Time, data=data)
summary(DOCmod1)
anova(DOCmod1)

# histogram
hist(DOCmod1$residuals)

# QQ-plot
qqPlot(DOCmod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(DOCmod1$residuals)
#P-value of the Shapiro-Wilk test on the residuals is lower than  α=  5%, so we residuals do not seem to follow a normal distribution (p-value >0.05).


ks.test(DOCmod1$residuals, "pnorm", mean = mean(DOCmod1$residuals),
        sd = sd(DOCmod1$residuals))


# Kolmogorov–Smirnov test with p>0.05 indicates normal distribution

leveneTest(DOC~Nitrogen*Plastic*Time, data=data)

#The p-value being larger than the significance level of 0.05, we do not reject the null hypothesis, so we cannot reject the hypothesis that variances are equal between treatments (p-value >0.05).

#This result is also in line with the visual approach, so the homogeneity of variances is met both visually and formally.


#TukeyHSD(DOCmod1)




DOC15<-aov(DOC~Nitrogen*Plastic, data=data5)
summary(DOC15)
anova(DOC15)
tukeyDOC5<-TukeyHSD(DOC15)
tukey.cldDOC5 <- multcompLetters4(DOC15, tukeyDOC5)
print(tukey.cldDOC5)


DOC115<-aov(DOC~Nitrogen*Plastic, data=data15)
summary(DOC115)
anova(DOC115)
tukeyDOC15<-TukeyHSD(DOC115)
tukey.cldDOC15 <- multcompLetters4(DOC115, tukeyDOC15)
print(tukey.cldDOC15)


DOC130<-aov(DOC~Nitrogen*Plastic, data=data30)
summary(DOC130)
anova(DOC130)
tukeyDOC30<-TukeyHSD(DOC130)
tukey.cldDOC30 <- multcompLetters4(DOC130, tukeyDOC30)
print(tukey.cldDOC30)


DOC1193<-aov(DOC~Nitrogen*Plastic, data=data193)
summary(DOC1193)
anova(DOC1193)
tukeyDOC193<-TukeyHSD(DOC1193)
tukey.cldDOC193 <- multcompLetters4(DOC1193, tukeyDOC193)
print(tukey.cldDOC193)





#Make a Bar plot
####For DOC ###
# Calculate mean and standard error
se_data <- data %>%
  group_by(Time, Treatment) %>%
  summarize(mean_DOC = mean(DOC),
            se_DOC = sd(DOC) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(data, se_data, by = c("Time", "Treatment"))

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_DOC, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_DOC - se_DOC, ymax = mean_DOC + se_DOC),
                position = position_dodge(width = 0.8), width = 0.25) +
  #geom_point(aes(y = DOC), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
  #         color = "black", size = 1) +
  labs(title = "Dissolved Organic Carbon",
       x = " ",
       y = "Dissolved Organic Carbon (mg C/kg soil)",
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





# Create the bar diagram with data points and error bars
p11 <- ggplot(data_with_se, aes(x = Treatment, y = mean_DOC, fill = Time)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_DOC - se_DOC, ymax = mean_DOC + se_DOC),
                position = position_dodge(width = 0.8), width = 0.25) +
  #geom_point(aes(y = DOC), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
  #          color = "black", size = 1) +
  labs(title = "Dissolved Organic Carbon",
       x = " ",
       y = "Dissolved Organic Carbon (mg C/kg soil)",
       fill = "Treatment") +
  theme_minimal() +
  scale_fill_manual(values = c("5D" = "blue", "15D" = "blue2", "30D" = "blue3", "193D" = "blue4")) + 
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p11 <- p11 + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=13,color="black"))
p11 <- p11 + theme(axis.text.y = element_text(size=13,color="black"))
p11 <- p11 + theme(text = element_text(size = 15, color="black"))
print(p11)



############################
###MBC###
###########################
#Full model with correct error structure
MBCMod1<-aov(MBC~Nitrogen*Plastic*Time, data=data)
summary(MBCMod1)
anova(MBCMod1)

# histogram
hist(MBCMod1$residuals)

# QQ-plot
qqPlot(MBCMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(MBCMod1$residuals)
#P value is < than 0.05 meaning the data is not normally distributed.


ks.test(MBCMod1$residuals, "pnorm", mean = mean(MBCMod1$residuals),
        sd = sd(MBCMod1$residuals))


leveneTest(MBC~Nitrogen*Plastic*Time, data=data
)
# p value is < 0.05 indicating no homogeneity of variance

#TukeyHSD(MBCMod1)


MBC15<-aov(MBC~Nitrogen*Plastic, data=data5)
summary(MBC15)
anova(MBC15)
tukeyMBC5<-TukeyHSD(MBC15)
tukey.cldMBC5 <- multcompLetters4(MBC15, tukeyMBC5)
print(tukey.cldMBC5)


MBC115<-aov(MBC~Nitrogen*Plastic, data=data15)
summary(MBC115)
anova(MBC115)
tukeyMBC15<-TukeyHSD(MBC115)
tukey.cldMBC15 <- multcompLetters4(MBC115, tukeyMBC15)
print(tukey.cldMBC15)


MBC130<-aov(MBC~Nitrogen*Plastic, data=data30)
summary(MBC130)
anova(MBC130)
tukeyMBC30<-TukeyHSD(MBC130)
tukey.cldMBC30 <- multcompLetters4(MBC130, tukeyMBC30)
print(tukey.cldMBC30)


MBC1193<-aov(MBC~Nitrogen*Plastic, data=data193)
summary(MBC1193)
anova(MBC1193)
tukeyMBC193<-TukeyHSD(MBC1193)
tukey.cldMBC193 <- multcompLetters4(MBC1193, tukeyMBC193)
print(tukey.cldMBC193)







##Since the resuduals were not normally distributed, we log transformed the data 
#Full model with correct error structure
data$LMBC<-log(1+data$MBC)
LMBCMod1<-aov(LMBC~Nitrogen*Plastic*Time, data=data)
summary(LMBCMod1)
anova(LMBCMod1)

# histogram
hist(LMBCMod1$residuals)

# QQ-plot
qqPlot(LMBCMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(LMBCMod1$residuals)
#P value is < than 0.05 meaning the data is not normally distributed.

ks.test(LMBCMod1$residuals, "pnorm", mean = mean(LMBCMod1$residuals),
        sd = sd(LMBCMod1$residuals))
# The p value is > than 0.05 meaning the data is normally distributed

leveneTest(LMBC~Nitrogen*Plastic*Time, data=data
)
# The p value is > than 0.05 meaning the variance are homogenous

#TukeyHSD(LMBCMod1)





#Make a Bar plot
####For MBC ###
# Calculate mean and standard error
se_data <- data %>%
  group_by(Time, Treatment) %>%
  summarize(mean_MBC = mean(MBC),
            se_MBC = sd(MBC) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(data, se_data, by = c("Time", "Treatment"))

# Create the bar diagram with data points and error bars
P2 <- ggplot(data_with_se, aes(x = Time, y = mean_MBC, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_MBC - se_MBC, ymax = mean_MBC + se_MBC),
                position = position_dodge(width = 0.8), width = 0.25) +
  geom_point(aes(y = MBC), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             color = "black", size = 1) +
  labs(title = "Microbial Biomass Carbon",
       x = " ",
       y = "Microbial Biomass Carbon (mg C/kg soil)",
       fill = "Treatment") +
  scale_fill_manual(values = c("NONE-N0" = "dodgerblue", "LDPE-N0" = "dodgerblue3", "PBS-N0" = "blue", "PLA/PHA-N0" = "blue3","PLA-N0" = "blue4",
                               "NONE-N1" =   "indianred3", "LDPE-N1" = "salmon", "PBS-N1" = "red", "PLA/PHA-N1" = "red3" ,"PLA-N1" = "red4")) + 
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
P2 <- P2 + theme(axis.text.x = element_text(angle = 0, hjust = 1, size=13,color="black"))
P2 <- P2 + theme(axis.text.y = element_text(size=13,color="black"))
P2 <- P2 + theme(text = element_text(size = 15, color="black"))
print(P2)




############################
###Ammonium###
###########################
#Full model with correct error structure
AmmonMod1<-aov(Ammonium~Nitrogen*Plastic*Time, data=data)
summary(AmmonMod1)
anova(AmmonMod1)

# histogram
hist(AmmonMod1$residuals)

# QQ-plot
qqPlot(AmmonMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(AmmonMod1$residuals)
#P value is less than 0.05 meaning the data is not normally distributed.



leveneTest(Ammonium~Nitrogen*Plastic*Time, data=data
)
# The p value is less than 0.05 meaning the variance are not homogenous

#TukeyHSD(AmmonMod1)


##Since the resuduals were not normally distributed, we log transformed the data 
#Full model with correct error structure
data$Lammonium<-log(1+data$Ammonium)
LammonMod1<-aov(Lammonium~Nitrogen*Plastic*Time, data=data)
summary(LammonMod1)
anova(LammonMod1)

# histogram
hist(LammonMod1$residuals)

# QQ-plot
qqPlot(LammonMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(LammonMod1$residuals)
#P value is < than 0.05 meaning the data is not normally distributed.

ks.test(LammonMod1$residuals, "pnorm", mean = mean(LammonMod1$residuals),
        sd = sd(LammonMod1$residuals))
# The p value is > than 0.05 meaning the data is normally distributed

leveneTest(Lammonium~Nitrogen*Plastic*Time, data=data
)
# The p value is > than 0.05 meaning the variance are homogenous

#TukeyHSD(LammonMod1)



Ammonium15<-aov(Ammonium~Nitrogen*Plastic, data=data5)
summary(Ammonium15)
anova(Ammonium15)
tukeyAmmonium5<-TukeyHSD(Ammonium15)
tukey.cldAmmonium5 <- multcompLetters4(Ammonium15, tukeyAmmonium5)
print(tukey.cldAmmonium5)


Ammonium115<-aov(Ammonium~Nitrogen*Plastic, data=data15)
summary(Ammonium115)
anova(Ammonium115)
tukeyAmmonium15<-TukeyHSD(Ammonium115)
tukey.cldAmmonium15 <- multcompLetters4(Ammonium115, tukeyAmmonium15)
print(tukey.cldAmmonium15)


Ammonium130<-aov(Ammonium~Nitrogen*Plastic, data=data30)
summary(Ammonium130)
anova(Ammonium130)
tukeyAmmonium30<-TukeyHSD(Ammonium130)
tukey.cldAmmonium30 <- multcompLetters4(Ammonium130, tukeyAmmonium30)
print(tukey.cldAmmonium30)


Ammonium1193<-aov(Ammonium~Nitrogen*Plastic, data=data193)
summary(Ammonium1193)
anova(Ammonium1193)
tukeyAmmonium193<-TukeyHSD(Ammonium1193)
tukey.cldAmmonium193 <- multcompLetters4(Ammonium1193, tukeyAmmonium193)
print(tukey.cldAmmonium193)




#Make a Bar plot
####For Ammonium ###
# Calculate mean and standard error
se_data <- data %>%
  group_by(Time, Treatment) %>%
  summarize(mean_Ammonium = mean(Ammonium),
            se_Ammonium = sd(Ammonium) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(data, se_data, by = c("Time", "Treatment"))

# Create the bar diagram with data points and error bars
P3 <- ggplot(data_with_se, aes(x = Time, y = mean_Ammonium, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_Ammonium - se_Ammonium, ymax = mean_Ammonium + se_Ammonium),
                position = position_dodge(width = 0.8), width = 0.25) +
  # geom_point(aes(y = Ammonium), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
  #         color = "black", size = 1) +
  labs(title = "Soil Ammonium content",
       x = " ",
       y = "Ammonium (mg C/kg soil)",
       fill = "Treatment") +
  scale_fill_manual(values = c("NONE-N0" = "dodgerblue", "LDPE-N0" = "dodgerblue3", "PBS-N0" = "blue", "PLA/PHA-N0" = "blue3","PLA-N0" = "blue4",
                               "NONE-N1" =   "indianred3", "LDPE-N1" = "salmon", "PBS-N1" = "red", "PLA/PHA-N1" = "red3" ,"PLA-N1" = "red4")) + 
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
P3 <- P3 + theme(axis.text.x = element_text(angle = 0, hjust = 1, size=13,color="black"))
P3 <- P3 + theme(axis.text.y = element_text(size=13,color="black"))
P3 <- P3 + theme(text = element_text(size = 15, color="black"))
print(P3)



############################
###Nitrate###
###########################
#Full model with correct error structure
NitrateMod1<-aov(Nitrate~Nitrogen*Plastic*Time, data=data)
summary(NitrateMod1)
anova(NitrateMod1)
# histogram
hist(NitrateMod1$residuals)

# QQ-plot
qqPlot(NitrateMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(NitrateMod1$residuals)
#P value is less than 0.05 meaning the data is not normally distributed.

ks.test(NitrateMod1$residuals, "pnorm", mean = mean(NitrateMod1$residuals),
        sd = sd(NitrateMod1$residuals))
# The p value is < than 0.05 meaning the data is not normally distributed.

leveneTest(Nitrate~Nitrogen*Plastic*Time, data=data
)
# The p value is > than 0.05 meaning the variance are  homogenous

#TukeyHSD(NitrateMod1)


#Since the normality assumption was not meet, We log tranformed nitrate data

data$Lnitrate<-log(1+data$Nitrate)

LnitrateMod1<-aov(Lnitrate~Nitrogen*Plastic*Time, data=data)
summary(LnitrateMod1)
anova(LnitrateMod1)
# histogram
hist(LnitrateMod1$residuals)

# QQ-plot
qqPlot(LnitrateMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(LnitrateMod1$residuals)
#P value is less than 0.05 meaning the data is not normally distributed.

ks.test(LnitrateMod1$residuals, "pnorm", mean = mean(LnitrateMod1$residuals),
        sd = sd(LnitrateMod1$residuals))
#P value is < than 0.05 meaning the data is not normally distributed. (we can use it because our N>35)

leveneTest(Lnitrate~Nitrogen*Plastic*Time, data=data)


# The p value is less than 0.05 meaning the variance are not homogenous

#TukeyHSD(LnitrateMod1)


Lnitrate18<-aov(Lnitrate~Nitrogen*Plastic, data=data8)
summary(Lnitrate18)
anova(Lnitrate18)
tukeyLnitrate8<-TukeyHSD(Lnitrate18)
tukey.cldLnitrate8 <- multcompLetters4(Lnitrate18, tukeyLnitrate8)
print(tukey.cldLnitrate8)


Lnitrate115<-aov(Lnitrate~Nitrogen*Plastic, data=data15)
summary(Lnitrate115)
anova(Lnitrate115)
tukeyLnitrate15<-TukeyHSD(Lnitrate115)
tukey.cldLnitrate15 <- multcompLetters4(Lnitrate115, tukeyLnitrate15)
print(tukey.cldLnitrate15)


Lnitrate151<-aov(Lnitrate~Nitrogen*Plastic, data=data51)
summary(Lnitrate151)
anova(Lnitrate151)
tukeyLnitrate51<-TukeyHSD(Lnitrate151)
tukey.cldLnitrate51 <- multcompLetters4(Lnitrate151, tukeyLnitrate51)
print(tukey.cldLnitrate51)








#Do BOxcox analysis to see what tranformation works
library(MASS)
b <- boxcox(lm(data$Nitrate ~ 1))
# Exact lambda
lambda <- b$x[which.max(b$y)]
lambda
newtranfNitrate <- (data$Nitrate ^ lambda - 1) / lambda
NitrateMod1T<-aov(newtranfNitrate~Nitrogen*Plastic*Time, data=data)
summary(NitrateMod1T)
anova(NitrateMod1T)


# histogram
hist(NitrateMod1T$residuals)

# QQ-plot
qqPlot(NitrateMod1T$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(NitrateMod1T$residuals)
#P value is less than 0.05 meaning the data is not normally distributed.

leveneTest(newtranfNitrate~Nitrogen*Plastic*Time, data=data
)

#TukeyHSD(NitrateMod1)




# Since normality assumption couldn't be met, Compute Kruskal-Wallis test
kruskal.test(Nitrate ~ Treatment, data = data8)
#We can conclude that there are significant differences between the treatment groups because the p-value is less than the significance criterion of 0.05.
#pairwise.wilcox.test() can be used to calculate pairwise comparisons between group levels with different testing corrections.
pairwise.wilcox.test(data8$Nitrate, data8$Treatment,
                     p.adjust.method = "BH")


# Since normality assumption couldn't be met, Compute Kruskal-Wallis test
kruskal.test(Nitrate ~ Treatment, data = data15)
#We can conclude that there are significant differences between the treatment groups because the p-value is less than the significance criterion of 0.05.
#pairwise.wilcox.test() can be used to calculate pairwise comparisons between group levels with different testing corrections.
pairwise.wilcox.test(data15$Nitrate, data15$Treatment,
                     p.adjust.method = "BH")


# Since normality assumption couldn't be met, Compute Kruskal-Wallis test
kruskal.test(Nitrate ~ Treatment, data = data51)
#We can conclude that there are significant differences between the treatment groups because the p-value is less than the significance criterion of 0.05.
#pairwise.wilcox.test() can be used to calculate pairwise comparisons between group levels with different testing corrections.
pairwise.wilcox.test(data51$Nitrate, data51$Treatment,
                     p.adjust.method = "BH")










# Since normality assumption couldn't be met, Compute Kruskal-Wallis test
kruskal.test(Lnitrate ~ Treatment, data = data8)
#We can conclude that there are significant differences between the treatment groups because the p-value is less than the significance criterion of 0.05.
#pairwise.wilcox.test() can be used to calculate pairwise comparisons between group levels with different testing corrections.
pairwise.wilcox.test(data8$Lnitrate, data8$Treatment,
                     p.adjust.method = "BH")


# Since normality assumption couldn't be met, Compute Kruskal-Wallis test
kruskal.test(Lnitrate ~ Treatment, data = data15)
#We can conclude that there are significant differences between the treatment groups because the p-value is less than the significance criterion of 0.05.
#pairwise.wilcox.test() can be used to calculate pairwise comparisons between group levels with different testing corrections.
pairwise.wilcox.test(data15$Lnitrate, data15$Treatment,
                     p.adjust.method = "BH")


# Since normality assumption couldn't be met, Compute Kruskal-Wallis test
kruskal.test(Lnitrate ~ Treatment, data = data51)
#We can conclude that there are significant differences between the treatment groups because the p-value is less than the significance criterion of 0.05.
#pairwise.wilcox.test() can be used to calculate pairwise comparisons between group levels with different testing corrections.
pairwise.wilcox.test(data51$Lnitrate, data51$Treatment,
                     p.adjust.method = "BH")




#Make a Bar plot
####For Nitrate ###
# Calculate mean and standard error
se_data <- data %>%
  group_by(Time, Treatment) %>%
  summarize(mean_Nitrate = mean(Nitrate),
            se_Nitrate = sd(Nitrate) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(data, se_data, by = c("Time", "Treatment"))

# Create the bar diagram with data points and error bars
P4 <- ggplot(data_with_se, aes(x = Time, y = mean_Nitrate, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_Nitrate - se_Nitrate, ymax = mean_Nitrate + se_Nitrate),
                position = position_dodge(width = 0.8), width = 0.25) +
  labs(title = "Soil Nitrate Content",
       x = " ",
       y = "Nitrate (mg C/kg soil)",
       fill = "Treatment") +
  scale_fill_manual(values = c("NONE-N0" = "dodgerblue", "LDPE-N0" = "dodgerblue3", "PBS-N0" = "blue", "PLA/PHA-N0" = "blue3","PLA-N0" = "blue4",
                               "NONE-N1" =   "indianred3", "LDPE-N1" = "salmon", "PBS-N1" = "red", "PLA/PHA-N1" = "red3" ,"PLA-N1" = "red4")) + 
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
P4 <- P4 + theme(axis.text.x = element_text(angle = 0, hjust = 1, size=13,color="black"))
P4 <- P4 + theme(axis.text.y = element_text(size=13,color="black"))
P4 <- P4 + theme(text = element_text(size = 15, color="black"))
print(P4)




############################
### soil pH ###
###########################
#Full model with correct error structure
pHMod1<-aov(pH~Nitrogen*Plastic*Time, data=data)
summary(pHMod1)
anova(pHMod1)
#TukeyHSD(pHMod1)


# histogram
hist(pHMod1$residuals)

# QQ-plot
qqPlot(pHMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(pHMod1$residuals)
#P value is less than 0.0108 meaning the data is not normally distributed.




leveneTest(pH~Nitrogen*Plastic*Time, data=data)




pH15<-aov(pH~Nitrogen*Plastic, data=data5)
summary(pH15)
anova(pH15)
tukeypH5<-TukeyHSD(pH15)
tukey.cldpH5 <- multcompLetters4(pH15, tukeypH5)
print(tukey.cldpH5)


pH115<-aov(pH~Nitrogen*Plastic, data=data15)
summary(pH115)
anova(pH115)
tukeypH15<-TukeyHSD(pH115)
tukey.cldpH15 <- multcompLetters4(pH115, tukeypH15)
print(tukey.cldpH15)


pH130<-aov(pH~Nitrogen*Plastic, data=data30)
summary(pH130)
anova(pH130)
tukeypH30<-TukeyHSD(pH130)
tukey.cldpH30 <- multcompLetters4(pH130, tukeypH30)
print(tukey.cldpH30)


pH1193<-aov(pH~Nitrogen*Plastic, data=data193)
summary(pH1193)
anova(pH1193)
tukeypH193<-TukeyHSD(pH1193)
tukey.cldpH193 <- multcompLetters4(pH1193, tukeypH193)
print(tukey.cldpH193)







# the last column of data is a factor level. We don't include it in the code
mat_1 <-as.dist(round(cor(data[,9:25]),2))
data1<-data[9:25]
mat_1
write.csv(mat_1, "C:/UTK one drive/OneDrive - University of Tennessee/Biosystems Engineering/Mn Project/Mn Lab Incubation/Supplemental Experiment/correlation.csv")
install.packages("Hmisc")
library("Hmisc")

data_rcorr <-as.matrix(data[, 9: 25])

mat_2 <-rcorr(data_rcorr)
# mat_2 <-rcorr(as.matrix(data)) returns the same output
install.packages("GGally")
library(GGally)
ggcorr(data1)
ggcorr(data1,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")


#Make a Bar plot
####For pH ###
# Calculate mean and standard error
se_data <- data %>%
  group_by(Time, Treatment) %>%
  summarize(mean_pH = mean(pH),
            se_pH = sd(pH) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(data, se_data, by = c("Time", "Treatment"))

# Create the bar diagram with data points and error bars
P5 <- ggplot(data_with_se, aes(x = Time, y = mean_pH, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_pH - se_pH, ymax = mean_pH + se_pH),
                position = position_dodge(width = 0.8), width = 0.25) +
  #  geom_point(aes(y = pH), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
  #             color = "black", size = 1) +
  labs(title = "Soil pH",
       x = " ",
       y = "pH",
       fill = "Treatment") +
  scale_fill_manual(values = c("NONE-N0" = "dodgerblue", "LDPE-N0" = "dodgerblue3", "PBS-N0" = "blue", "PLA/PHA-N0" = "blue3","PLA-N0" = "blue4",
                               "NONE-N1" =   "indianred3", "LDPE-N1" = "salmon", "PBS-N1" = "red", "PLA/PHA-N1" = "red3" ,"PLA-N1" = "red4")) + 
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
P5 <- P5 + theme(axis.text.x = element_text(angle = 0, hjust = 1, size=13,color="black"))
P5 <- P5 + theme(axis.text.y = element_text(size=13,color="black"))
P5 <- P5 + theme(text = element_text(size = 15, color="black"))
P5 <- P5 +  coord_cartesian(ylim=c(4,7.5))
print(P5)






############################
###BG###
###########################
#Full model with correct error structure
BGMod1<-aov(BG~Nitrogen*Plastic*Time, data=data)
summary(BGMod1)
anova(BGMod1)

# histogram
hist(BGMod1$residuals)

# QQ-plot
qqPlot(BGMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(BGMod1$residuals)
#P value is < than 0.05 meaning the data is not normally distributed.


ks.test(BGMod1$residuals, "pnorm", mean = mean(BGMod1$residuals),
        sd = sd(BGMod1$residuals))


leveneTest(BG~Nitrogen*Plastic*Time, data=data
)
# p value is < 0.05 indicating no homogeneity of variance

#TukeyHSD(BGMod1)


BG15<-aov(BG~Nitrogen*Plastic, data=data5)
summary(BG15)
anova(BG15)
tukeyBG5<-TukeyHSD(BG15)
tukey.cldBG5 <- multcompLetters4(BG15, tukeyBG5)
print(tukey.cldBG5)


BG115<-aov(BG~Nitrogen*Plastic, data=data15)
summary(BG115)
anova(BG115)
tukeyBG15<-TukeyHSD(BG115)
tukey.cldBG15 <- multcompLetters4(BG115, tukeyBG15)
print(tukey.cldBG15)


BG130<-aov(BG~Nitrogen*Plastic, data=data30)
summary(BG130)
anova(BG130)
tukeyBG30<-TukeyHSD(BG130)
tukey.cldBG30 <- multcompLetters4(BG130, tukeyBG30)
print(tukey.cldBG30)


BG1193<-aov(BG~Nitrogen*Plastic, data=data193)
summary(BG1193)
anova(BG1193)
tukeyBG193<-TukeyHSD(BG1193)
tukey.cldBG193 <- multcompLetters4(BG1193, tukeyBG193)
print(tukey.cldBG193)






############################
###CB###
###########################
#Full model with correct error structure
CBMod1<-aov(CB~Nitrogen*Plastic*Time, data=data)
summary(CBMod1)
anova(CBMod1)

# histogram
hist(CBMod1$residuals)

# QQ-plot
qqPlot(CBMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(CBMod1$residuals)
#P value is < than 0.05 meaning the data is not normally distributed.


ks.test(CBMod1$residuals, "pnorm", mean = mean(CBMod1$residuals),
        sd = sd(CBMod1$residuals))


leveneTest(CB~Nitrogen*Plastic*Time, data=data
)
# p value is < 0.05 indicating no homogeneity of variance

#TukeyHSD(CBMod1)


CB15<-aov(CB~Nitrogen*Plastic, data=data5)
summary(CB15)
anova(CB15)
tukeyCB5<-TukeyHSD(CB15)
tukey.cldCB5 <- multcompLetters4(CB15, tukeyCB5)
print(tukey.cldCB5)


CB115<-aov(CB~Nitrogen*Plastic, data=data15)
summary(CB115)
anova(CB115)
tukeyCB15<-TukeyHSD(CB115)
tukey.cldCB15 <- multcompLetters4(CB115, tukeyCB15)
print(tukey.cldCB15)


CB130<-aov(CB~Nitrogen*Plastic, data=data30)
summary(CB130)
anova(CB130)
tukeyCB30<-TukeyHSD(CB130)
tukey.cldCB30 <- multcompLetters4(CB130, tukeyCB30)
print(tukey.cldCB30)


CB1193<-aov(CB~Nitrogen*Plastic, data=data193)
summary(CB1193)
anova(CB1193)
tukeyCB193<-TukeyHSD(CB1193)
tukey.cldCB193 <- multcompLetters4(CB1193, tukeyCB193)
print(tukey.cldCB193)





############################
###LAP###
###########################
#Full model with correct error structure
LAPMod1<-aov(LAP~Nitrogen*Plastic*Time, data=data)
summary(LAPMod1)
anova(LAPMod1)

# histogram
hist(LAPMod1$residuals)

# QQ-plot
qqPlot(LAPMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(LAPMod1$residuals)
#P value is < than 0.05 meaning the data is not normally distributed.


ks.test(LAPMod1$residuals, "pnorm", mean = mean(LAPMod1$residuals),
        sd = sd(LAPMod1$residuals))


leveneTest(LAP~Nitrogen*Plastic*Time, data=data
)
# p value is < 0.05 indicating no homogeneity of variance

#TukeyHSD(LAPMod1)


LAP15<-aov(LAP~Nitrogen*Plastic, data=data5)
summary(LAP15)
anova(LAP15)
tukeyLAP5<-TukeyHSD(LAP15)
tukey.cldLAP5 <- multcompLetters4(LAP15, tukeyLAP5)
print(tukey.cldLAP5)


LAP115<-aov(LAP~Nitrogen*Plastic, data=data15)
summary(LAP115)
anova(LAP115)
tukeyLAP15<-TukeyHSD(LAP115)
tukey.cldLAP15 <- multcompLetters4(LAP115, tukeyLAP15)
print(tukey.cldLAP15)


LAP130<-aov(LAP~Nitrogen*Plastic, data=data30)
summary(LAP130)
anova(LAP130)
tukeyLAP30<-TukeyHSD(LAP130)
tukey.cldLAP30 <- multcompLetters4(LAP130, tukeyLAP30)
print(tukey.cldLAP30)


LAP1193<-aov(LAP~Nitrogen*Plastic, data=data193)
summary(LAP1193)
anova(LAP1193)
tukeyLAP193<-TukeyHSD(LAP1193)
tukey.cldLAP193 <- multcompLetters4(LAP1193, tukeyLAP193)
print(tukey.cldLAP193)




############################
###XYL###
###########################
#Full model with correct error structure
XYLMod1<-aov(XYL~Nitrogen*Plastic*Time, data=data)
summary(XYLMod1)
anova(XYLMod1)

# histogram
hist(XYLMod1$residuals)

# QQ-plot
qqPlot(XYLMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(XYLMod1$residuals)
#P value is < than 0.05 meaning the data is not normally distributed.


ks.test(XYLMod1$residuals, "pnorm", mean = mean(XYLMod1$residuals),
        sd = sd(XYLMod1$residuals))


leveneTest(XYL~Nitrogen*Plastic*Time, data=data
)
# p value is < 0.05 indicating no homogeneity of variance

#TukeyHSD(XYLMod1)


XYL15<-aov(XYL~Nitrogen*Plastic, data=data5)
summary(XYL15)
anova(XYL15)
tukeyXYL5<-TukeyHSD(XYL15)
tukey.cldXYL5 <- multcompLetters4(XYL15, tukeyXYL5)
print(tukey.cldXYL5)


XYL115<-aov(XYL~Nitrogen*Plastic, data=data15)
summary(XYL115)
anova(XYL115)
tukeyXYL15<-TukeyHSD(XYL115)
tukey.cldXYL15 <- multcompLetters4(XYL115, tukeyXYL15)
print(tukey.cldXYL15)


XYL130<-aov(XYL~Nitrogen*Plastic, data=data30)
summary(XYL130)
anova(XYL130)
tukeyXYL30<-TukeyHSD(XYL130)
tukey.cldXYL30 <- multcompLetters4(XYL130, tukeyXYL30)
print(tukey.cldXYL30)


XYL1193<-aov(XYL~Nitrogen*Plastic, data=data193)
summary(XYL1193)
anova(XYL1193)
tukeyXYL193<-TukeyHSD(XYL1193)
tukey.cldXYL193 <- multcompLetters4(XYL1193, tukeyXYL193)
print(tukey.cldXYL193)





############################
###SOC###
###########################
#Full model with correct error structure
SOCMod1<-aov(SOC~Nitrogen*Plastic*Time, data=data)
summary(SOCMod1)
anova(SOCMod1)

# histogram
hist(SOCMod1$residuals)

# QQ-plot
qqPlot(SOCMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(SOCMod1$residuals)
#P value is < than 0.05 meaning the data is not normally distributed.


ks.test(SOCMod1$residuals, "pnorm", mean = mean(SOCMod1$residuals),
        sd = sd(SOCMod1$residuals))


leveneTest(SOC~Nitrogen*Plastic*Time, data=data
)
# p value is < 0.05 indicating no homogeneity of variance

#TukeyHSD(SOCMod1)


SOC15<-aov(SOC~Nitrogen*Plastic, data=data5)
summary(SOC15)
anova(SOC15)
tukeySOC5<-TukeyHSD(SOC15)
tukey.cldSOC5 <- multcompLetters4(SOC15, tukeySOC5)
print(tukey.cldSOC5)


SOC115<-aov(SOC~Nitrogen*Plastic, data=data15)
summary(SOC115)
anova(SOC115)
tukeySOC15<-TukeyHSD(SOC115)
tukey.cldSOC15 <- multcompLetters4(SOC115, tukeySOC15)
print(tukey.cldSOC15)


SOC130<-aov(SOC~Nitrogen*Plastic, data=data30)
summary(SOC130)
anova(SOC130)
tukeySOC30<-TukeyHSD(SOC130)
tukey.cldSOC30 <- multcompLetters4(SOC130, tukeySOC30)
print(tukey.cldSOC30)


SOC1193<-aov(SOC~Nitrogen*Plastic, data=data193)
summary(SOC1193)
anova(SOC1193)
tukeySOC193<-TukeyHSD(SOC1193)
tukey.cldSOC193 <- multcompLetters4(SOC1193, tukeySOC193)
print(tukey.cldSOC193)

