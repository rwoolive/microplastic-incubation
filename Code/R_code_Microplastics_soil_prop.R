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
data30 <- subset(data, Time=="50D")
data193 <- subset(data, Time=="193D")


dataN05 <- subset(dataN0, Time=="5D")
dataN015 <- subset(dataN0, Time=="15D")
dataN050 <- subset(dataN0, Time=="50D")
dataN0193 <- subset(dataN0, Time=="193D")

dataN15 <- subset(dataN0, Time=="5D")
dataN115 <- subset(dataN0, Time=="15D")
dataN150 <- subset(dataN0, Time=="50D")
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

DOCmod1<-aov(DOC~Nitrogen*Manganese*Time, data=data)
summary(DOCmod1)
anova(DOCmod1)

# histogram
hist(DOCmod1$residuals)

# QQ-plot
qqPlot(DOCmod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(DOCmod1$residuals)


ks.test(DOCmod1$residuals, "pnorm", mean = mean(DOCmod1$residuals),
        sd = sd(DOCmod1$residuals))

#P-value of the Shapiro-Wilk test on the residuals is larger than the usual significance level of  α=  5%, so we do not reject the hypothesis that residuals follow a normal distribution (p-value >0.05).

# In our case, the normality assumption is thus met both visually and formally.



leveneTest(DOC~Nitrogen*Manganese*Time, data=data
)

#The p-value being larger than the significance level of 0.05, we do not reject the null hypothesis, so we cannot reject the hypothesis that variances are equal between treatments (p-value >0.05).

#This result is also in line with the visual approach, so the homogeneity of variances is met both visually and formally.


TukeyHSD(DOCmod1)




DOC18<-aov(DOC~Nitrogen*Manganese, data=data8)
summary(DOC18)
anova(DOC18)
tukeyDOC8<-TukeyHSD(DOC18)
tukey.cldDOC8 <- multcompLetters4(DOC18, tukeyDOC8)
print(tukey.cldDOC8)


DOC115<-aov(DOC~Nitrogen*Manganese, data=data15)
summary(DOC115)
anova(DOC115)
tukeyDOC15<-TukeyHSD(DOC115)
tukey.cldDOC15 <- multcompLetters4(DOC115, tukeyDOC15)
print(tukey.cldDOC15)


DOC151<-aov(DOC~Nitrogen*Manganese, data=data51)
summary(DOC151)
anova(DOC151)
tukeyDOC51<-TukeyHSD(DOC151)
tukey.cldDOC51 <- multcompLetters4(DOC151, tukeyDOC51)
print(tukey.cldDOC51)






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
MBCMod1<-aov(MBC~Nitrogen*Manganese*Time, data=data)
summary(MBCMod1)
anova(MBCMod1)

# histogram
hist(MBCMod1$residuals)

# QQ-plot
qqPlot(MBCMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(MBCMod1$residuals)
#P value is > than 0.05 meaning the data is  normally distributed.

leveneTest(MBC~Nitrogen*Manganese*Time, data=data
)
# p value is > 0.05 indicating homogeneity of variance

#TukeyHSD(MBCMod1)




MBC18<-aov(MBC~Nitrogen*Manganese, data=data8)
summary(MBC18)
anova(MBC18)
tukeyMBC8<-TukeyHSD(MBC18)
tukey.cldMBC8 <- multcompLetters4(MBC18, tukeyMBC8)
print(tukey.cldMBC8)


MBC115<-aov(MBC~Nitrogen*Manganese, data=data15)
summary(MBC115)
anova(MBC115)
tukeyMBC15<-TukeyHSD(MBC115)
tukey.cldMBC15 <- multcompLetters4(MBC115, tukeyMBC15)
print(tukey.cldMBC15)


MBC151<-aov(MBC~Nitrogen*Manganese, data=data51)
summary(MBC151)
anova(MBC151)
tukeyMBC51<-TukeyHSD(MBC151)
tukey.cldMBC51 <- multcompLetters4(MBC151, tukeyMBC51)
print(tukey.cldMBC51)




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
AmmonMod1<-aov(Ammonium~Nitrogen*Manganese*Time, data=data)
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

leveneTest(Ammonium~Nitrogen*Manganese*Time, data=data
)
# The p value is less than 0.05 meaning the variance are not homogenous

#TukeyHSD(AmmonMod1)


##Since the resuduals were not normally distributed, we log transformed the data 
#Full model with correct error structure
LammonMod1<-aov(Lammonium~Nitrogen*Manganese*Time, data=data)
summary(LammonMod1)
anova(LammonMod1)

# histogram
hist(LammonMod1$residuals)

# QQ-plot
qqPlot(LammonMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(LammonMod1$residuals)
#P value is > than 0.05 meaning the data is normally distributed.

leveneTest(Lammonium~Nitrogen*Manganese*Time, data=data
)
# The p value is > than 0.05 meaning the variance are homogenous

#TukeyHSD(LammonMod1)




Lammon18<-aov(Lammonium~Nitrogen*Manganese, data=data8)
summary(Lammon18)
anova(Lammon18)
tukeyLammonium8<-TukeyHSD(Lammon18)
tukey.cldLammonium8 <- multcompLetters4(Lammon18, tukeyLammonium8)
print(tukey.cldLammonium8)


Lammon115<-aov(Lammonium~Nitrogen*Manganese, data=data15)
summary(Lammon115)
anova(Lammon115)
tukeyLammonium15<-TukeyHSD(Lammon115)
tukey.cldLammonium15 <- multcompLetters4(Lammon115, tukeyLammonium15)
print(tukey.cldLammonium15)


Lammon151<-aov(Lammonium~Nitrogen*Manganese, data=data51)
summary(Lammon151)
anova(Lammon151)
tukeyLammonium51<-TukeyHSD(Lammon151)
tukey.cldLammonium51 <- multcompLetters4(Lammon151, tukeyLammonium51)
print(tukey.cldLammonium51)



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
NitrateMod1<-aov(Nitrate~Nitrogen*Manganese*Time, data=data)
summary(NitrateMod1)
anova(NitrateMod1)
# histogram
hist(NitrateMod1$residuals)

# QQ-plot
qqPlot(NitrateMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(NitrateMod1$residuals)
#P value is less than 0.0108 meaning the data is not normally distributed.

ks.test(NitrateMod1$residuals, "pnorm", mean = mean(NitrateMod1$residuals),
        sd = sd(NitrateMod1$residuals))

leveneTest(Nitrate~Nitrogen*Manganese*Time, data=data
)
# The p value is less than 0.05 meaning the variance are not homogenous

#TukeyHSD(NitrateMod1)


#Since the normality assumption was not meet, We log tranformed nitrate data
LnitrateMod1<-aov(Lnitrate~Nitrogen*Manganese*Time, data=data)
summary(LnitrateMod1)
anova(LnitrateMod1)
# histogram
hist(LnitrateMod1$residuals)

# QQ-plot
qqPlot(LnitrateMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(LnitrateMod1$residuals)
#P value is less than 0.0108 meaning the data is not normally distributed.

ks.test(LnitrateMod1$residuals, "pnorm", mean = mean(LnitrateMod1$residuals),
        sd = sd(LnitrateMod1$residuals))
#P value is > than 0.05 meaning the data is normally distributed. (we can use it because our N>35)

leveneTest(Lnitrate~Nitrogen*Manganese*Time, data=data)


# The p value is less than 0.05 meaning the variance are not homogenous

#TukeyHSD(LnitrateMod1)


Lnitrate18<-aov(Lnitrate~Nitrogen*Manganese, data=data8)
summary(Lnitrate18)
anova(Lnitrate18)
tukeyLnitrate8<-TukeyHSD(Lnitrate18)
tukey.cldLnitrate8 <- multcompLetters4(Lnitrate18, tukeyLnitrate8)
print(tukey.cldLnitrate8)


Lnitrate115<-aov(Lnitrate~Nitrogen*Manganese, data=data15)
summary(Lnitrate115)
anova(Lnitrate115)
tukeyLnitrate15<-TukeyHSD(Lnitrate115)
tukey.cldLnitrate15 <- multcompLetters4(Lnitrate115, tukeyLnitrate15)
print(tukey.cldLnitrate15)


Lnitrate151<-aov(Lnitrate~Nitrogen*Manganese, data=data51)
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
NitrateMod1T<-aov(newtranfNitrate~Nitrogen*Manganese*Time, data=data)
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

leveneTest(newtranfNitrate~Nitrogen*Manganese*Time, data=data
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
###Instantaneous N2O###
###########################
#Full model with correct error structure
InstN2OMod1<-aov(InstN2O~Nitrogen*Manganese*Time, data=data)
summary(InstN2OMod1)
anova(InstN2OMod1)
TukeyHSD(InstN2OMod1)




############################
### soil pH ###
###########################
#Full model with correct error structure
pHMod1<-aov(pH~Nitrogen*Manganese*Time, data=data)
summary(pHMod1)
anova(pHMod1)
TukeyHSD(pHMod1)


# histogram
hist(pHMod1$residuals)

# QQ-plot
qqPlot(pHMod1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(pHMod1$residuals)
#P value is less than 0.0108 meaning the data is not normally distributed.

leveneTest(pH~Nitrogen*Manganese*Time, data=data
)


pHMod18<-aov(pH~Nitrogen*Manganese, data=data8)
summary(pHMod18)
anova(pHMod18)
tukeypH8<-TukeyHSD(pHMod18)
tukey.cldpH8 <- multcompLetters4(pHMod18, tukeypH8)
print(tukey.cldpH8)


pHMod115<-aov(pH~Nitrogen*Manganese, data=data15)
summary(pHMod115)
anova(pHMod115)
tukeypH15<-TukeyHSD(pHMod115)
tukey.cldpH15 <- multcompLetters4(pHMod115, tukeypH15)
print(tukey.cldpH15)


pHMod151<-aov(pH~Nitrogen*Manganese, data=data51)
summary(pHMod151)
anova(pHMod151)
tukeypH51<-TukeyHSD(pHMod151)
tukey.cldpH51 <- multcompLetters4(pHMod151, tukeypH51)
print(tukey.cldpH51)




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

