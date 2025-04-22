rm(list = ls(all = TRUE))
graphics.off()
#shell("cls")


# Load necessary libraries
library(readxl)
#library(tibble)
library(dplyr)
#library(lubridate)
library(ggplot2)
library(zoo)
library(ggpubr)
library(patchwork)
library(multcomp) # for contrast
library(multcompView)#for abc
library(car)
library(lattice)     # for densityplot()
library(nlme)
#library(lme4)
library(emmeans)

# colors for differnt levels 
colplas <- c("goldenrod2", "indianred1", "steelblue", "lightseagreen", "slateblue1")
colnit <- c("lightblue","salmon")
alphanit <- c(0.1,0.7)
colderive <- c("seashell2","goldenrod4")



# labels for response variables
a1 <- expression(paste("Total CO"[2], " (", mu,"g g"^-1,")"))
a1s <- expression(paste("Std. total CO"[2]))
a2 <- expression(paste("Plastic-derived CO"[2], " (", mu,"g g"^-1,")"))
a2s <- expression(paste("Std. plastic-derived CO"[2]))
a3 <- expression(paste("SOM-derived CO"[2], " (", mu,"g g"^-1,")"))
a3s <- expression(paste("Std. SOM-derived CO"[2]))
a4 <- expression(paste("Priming (", mu,"g g"^-1,") "))
a5 <- expression(paste("MBC (mg kg"^-1,")"))
a6 <- expression(paste("DOC (mg kg"^-1,")"))
a7 <- expression(paste("C-acquiring enzymes (", mu,"mol g"^-1," hr"^-1,")"))
a8 <- expression(paste("LAP activity (", mu,"mol g"^-1," hr"^-1,")"))
a9 <- expression(paste("NH"[4]^"+", "-N (mg mg"^-1,")"))
a10 <- expression(paste("NO"[3]^"-", "-N (mg kg"^-1,")"))
a11 <- expression(paste("pH"))
a12 <- expression(paste("Total SOC (g kg"^-1,")"))
a12s <- expression(paste("Std. SOC"))
a13 <- expression(paste("Total N (g kg"^-1,")"))
dat_names <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)




dat_CO2 <- read.csv("Processed-data/dat_CO2.csv")
dat_soil <- read.csv("Processed-data/dat_soil.csv")
testlet_all <- read.csv(paste0("Model-output/tukey/plastic_nitrogen.csv"))
testlet_all <- testlet_all[-1,]
mod.anova <- read.csv(paste0("Model-output/anova/plastic_nitrogen.csv"))


# factors
dat_CO2 <- dat_CO2 %>%
  mutate(Plastic = factor(Plastic,levels = c("NONE", "LDPE", "PBS", "PLA", "PLA/PHA")))%>%
  mutate(Nitrogen = factor(Nitrogen,levels = c("N0", "N1")))%>%
  mutate(Time = day)%>%
  mutate(Rep = factor(Rep))

dat_soil <- dat_soil %>%
  mutate(Plastic = factor(Plastic,levels = c("NONE", "LDPE", "PBS", "PLA", "PLA/PHA")))%>%
  mutate(Nitrogen = factor(Nitrogen,levels = c("N0", "N1")))%>%
  mutate(Time = plyr::revalue(Time, c("5D" = "Day 5", "15D" = "Day 15", "30D" = "Day 30", "193D" = "Day 193")))%>%
  mutate(Time = factor(Time, levels=c("Day 5", "Day 15", "Day 30", "Day 193"))) %>%
  mutate(Rep = factor(Rep))

testlet_all <- testlet_all %>%
  mutate(Plastic = factor(Plastic,levels = c("NONE", "LDPE", "PBS", "PLA", "PLA/PHA")))%>%
  mutate(Nitrogen = factor(Nitrogen,levels = c("N0", "N1")))%>%
  mutate(Time = factor(timepoint))%>%
  mutate(Time = plyr::revalue(Time, c("5" = "Day 5", "15" = "Day 15", "30" = "Day 30", "193" = "Day 193")))

mod.anova <- mod.anova %>%
  mutate(Plastic = factor(Plastic,levels = c("NONE", "LDPE", "PBS", "PLA", "PLA/PHA")))%>%
  mutate(Nitrogen = factor(Nitrogen,levels = c("N0", "N1")))%>%
  mutate(Time = factor(time))%>%
  mutate(Time = plyr::revalue(Time, c("1" = "Day 5", "2" = "Day 15", "3" = "Day 30", "4" = "Day 193")))



width_1 <- 12
size_labels <- 4
font_1 <- 18




#### FIGURE 1

# Make a Bar plot
#### For cumulative_CO2 ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_cumulative_CO2 = mean(cumulative_CO2),
            se_cumulative_CO2 = sd(cumulative_CO2) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="cumulative_CO2"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                          testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                          testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_cumulative_CO2, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_cumulative_CO2 - se_cumulative_CO2, ymax = mean_cumulative_CO2 + se_cumulative_CO2),
                position = position_dodge(width=0.95), width = 0.25) +
  labs(x = "Incubation time (d)",
       y = dat_names[1]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "A") +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  geom_label(aes(label=letters, y= mean_cumulative_CO2 - se_cumulative_CO2+250), position=position_dodge(width=0.95),
             hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)

p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/cumulative_CO2.png",width = width_1, height = 3.5, dpi = 300)


# Make a line plot
#### For cumulative_CO2 ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_cumulative_CO2, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "Incubation time (d)",y = dat_names[1]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "A")

p1
ggsave(p1, file="Figures/cumulative_CO2_lineplot.png",width = 6, height = 4, dpi = 300)






# Make a Bar plot
#### For cumulative_CO2_plastic ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_cumulative_CO2_plastic = mean(cumulative_CO2_plastic),
            se_cumulative_CO2_plastic = sd(cumulative_CO2_plastic) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="cumulative_CO2_plastic"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA
data_with_se <- data_with_se %>% filter(Plastic != "NONE")

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_cumulative_CO2_plastic, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_cumulative_CO2_plastic - se_cumulative_CO2_plastic, ymax = mean_cumulative_CO2_plastic + se_cumulative_CO2_plastic),
                position = position_dodge(width=0.95), width = 0.25) +
  labs(x = "Incubation time (d)",
       y = dat_names[2]) +
  theme_minimal() +
  scale_fill_manual(values = colplas[2:5]) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_cumulative_CO2_plastic + se_cumulative_CO2_plastic+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "B") +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  geom_label(aes(label=letters, y= mean_cumulative_CO2_plastic - se_cumulative_CO2_plastic+100), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)

p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/cumulative_CO2_plastic.png",width = width_1, height = 3.5, dpi = 300)


# Make a line plot
#### For cumulative_CO2_plastic ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_cumulative_CO2_plastic, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "Incubation time",y = dat_names[2]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "B") 

p1
ggsave(p1, file="Figures/cumulative_CO2_plastic_lineplot.png",width = 6, height = 4, dpi = 300)












# Make a Bar plot
#### For cumulative_CO2_native ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_cumulative_CO2_native = mean(cumulative_CO2_native),
            se_cumulative_CO2_native = sd(cumulative_CO2_native) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="cumulative_CO2_native"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_cumulative_CO2_native, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_cumulative_CO2_native - se_cumulative_CO2_native, ymax = mean_cumulative_CO2_native + se_cumulative_CO2_native),
                position = position_dodge(width=0.95), width = 0.25) +
   labs(x = "Incubation time (d)",
       y = dat_names[3]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  labs(tag = "C") +
  geom_label(aes(label=letters, y= mean_cumulative_CO2_native - se_cumulative_CO2_native+100), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)

p1<- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/cumulative_CO2_native.png",width = width_1, height = 3.5, dpi = 300)


# Make a line plot
#### For cumulative_CO2_native ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_cumulative_CO2_native, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "Incubation time",y = dat_names[3]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "C") 

p1
ggsave(p1, file="Figures/cumulative_CO2_native_lineplot.png",width = 6, height = 4, dpi = 300)








# Make a Bar plot
#### For SOC ###
# Calculate mean and standard error
se_data <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_SOC = mean(SOC),
            se_SOC = sd(SOC) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_soil, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="SOC"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
for(i in 1:dim(data_with_se)[1]){
  data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$Time==data_with_se$Time[i] & 
                                                          testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                          testlet_all_1$Plastic==data_with_se$Plastic[i])]
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_SOC*10, fill = Plastic, alpha=Nitrogen)) + # *10 inorder to convert to g kg-1
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = 10*(mean_SOC - se_SOC), ymax = 10*(mean_SOC + se_SOC)),
                position = position_dodge(width=0.95), width = 0.25) +
  labs(x = "Incubation time",
       y = dat_names[12]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "D") +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  geom_label(aes(label=letters, y=10* (mean_SOC - se_SOC)+4), position=position_dodge(width=0.95), 
             hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend=F)

p1<- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/SOC.png",width = width_1, height = 3.5, dpi = 300)




# Make a line plot
#### For SOC ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_SOC*10, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  geom_errorbar(aes(ymin = mean_SOC - se_SOC, ymax = mean_SOC + se_SOC), linetype=1, width=0) +
  labs(x = "Incubation time",y = dat_names[12]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "D") 

p1
ggsave(p1, file="Figures/SOC_lineplot.png",width = 8, height = 3, dpi = 300)








###### FIGURE S2



# Make a Bar plot
#### For cumulative_CO2_std ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_cumulative_CO2_std = mean(cumulative_CO2_std),
            se_cumulative_CO2_std = sd(cumulative_CO2_std) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="cumulative_CO2_std"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
data_with_se <- data_with_se[-which(data_with_se$Plastic=="NONE"),]
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA


# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_cumulative_CO2_std, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_cumulative_CO2_std - se_cumulative_CO2_std, ymax = mean_cumulative_CO2_std + se_cumulative_CO2_std),
                position = position_dodge(width=0.95), width = 0.25) +
  labs(x = "Incubation time (d)",
       y = a1s) +
  theme_minimal() +
  scale_fill_manual(values = colplas[2:5]) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "A") +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  geom_label(aes(label=letters, y= mean_cumulative_CO2_std - se_cumulative_CO2_std+0.025), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)
p1<- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/cumulative_CO2_std.png",width = width_1, height = 3.5, dpi = 300)


# Make a line plot
#### For cumulative_CO2_std ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_cumulative_CO2_std, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_hline(yintercept=0) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas[2:5]) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "Incubation day",y = a1s) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "A")


p1
ggsave(p1, file="Figures/cumulative_CO2_std_lineplot.png",width = 9, height = 3, dpi = 300)









# Make a Bar plot
#### For cumulative_CO2_plastic_std ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_cumulative_CO2_plastic_std = mean(cumulative_CO2_plastic_std),
            se_cumulative_CO2_plastic_std = sd(cumulative_CO2_plastic_std) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="cumulative_CO2_plastic_std"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
data_with_se <- data_with_se[-which(data_with_se$Plastic=="NONE"),]
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA


# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_cumulative_CO2_plastic_std, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_cumulative_CO2_plastic_std - se_cumulative_CO2_plastic_std, ymax = mean_cumulative_CO2_plastic_std + se_cumulative_CO2_plastic_std),
                position = position_dodge(width=0.95), width = 0.25) +
  labs(x = "Incubation time (d)",
       y = a2s) +
  theme_minimal() +
  scale_fill_manual(values = colplas[2:5]) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "B") +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  geom_label(aes(label=letters, y= mean_cumulative_CO2_plastic_std - se_cumulative_CO2_plastic_std+0.01), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)
p1<- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/cumulative_CO2_plastic_std.png",width = width_1, height = 3.5, dpi = 300)


# Make a line plot
#### For cumulative_CO2_plastic_std ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_cumulative_CO2_plastic_std, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_hline(yintercept=0) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas[2:5]) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "Incubation day",y = a2s) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "B")


p1
ggsave(p1, file="Figures/cumulative_CO2_plastic_std_lineplot.png",width = 9, height = 3, dpi = 300)











# Make a Bar plot
#### For cumulative_CO2_native_std ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_cumulative_CO2_native_std = mean(cumulative_CO2_native_std),
            se_cumulative_CO2_native_std = sd(cumulative_CO2_native_std) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="cumulative_CO2_native_std"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
data_with_se <- data_with_se[-which(data_with_se$Plastic=="NONE"),]
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA


# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_cumulative_CO2_native_std, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_cumulative_CO2_native_std - se_cumulative_CO2_native_std, ymax = mean_cumulative_CO2_native_std + se_cumulative_CO2_native_std),
                position = position_dodge(width=0.95), width = 0.25) +
  labs(x = "Incubation time (d)",
       y = a3s) +
  theme_minimal() +
  scale_fill_manual(values = colplas[2:5]) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "C") +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  geom_label(aes(label=letters, y= mean_cumulative_CO2_native_std - se_cumulative_CO2_native_std+0.01), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)
p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/cumulative_CO2_native_std.png",width = width_1, height = 3.5, dpi = 300)


# Make a line plot
#### For cumulative_CO2_native_std ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_cumulative_CO2_native_std, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_hline(yintercept=0) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas[2:5]) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "Incubation day",y = a3s) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "C")


p1
ggsave(p1, file="Figures/cumulative_CO2_native_std_lineplot.png",width = 9, height = 3, dpi = 300)







# Make a Bar plot
#### For SOC_std ###
# Calculate mean and standard error
se_data <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_SOC_std = mean(SOC_std),
            se_SOC_std = sd(SOC_std) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_soil, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="SOC_std"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
data_with_se <- data_with_se %>%
  mutate(timepoint = case_when(
    Time == 1 ~ 5,
    Time == 2 ~ 15,
    Time == 3 ~ 30,
    Time == 4 ~ 193
  ))

for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$timepoint[i] %in% unique(testlet_all_1$timepoint) & data_with_se$Plastic[i] %in% unique(testlet_all_1$Plastic))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$timepoint[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA
data_with_se <- data_with_se %>% filter(Plastic!="NONE")

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$timepoint %in% c(5,15,30,193)),], aes(x = as.factor(timepoint), y = mean_SOC_std, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_SOC_std - se_SOC_std, ymax = mean_SOC_std + se_SOC_std),
                position = position_dodge(width=0.95), width = 0.25) +
  labs(x = "Incubation time (d)",
       y = a12s) +
  #scale_y_continuous(breaks = c(0,0.2, 0.4, 0.6, 0.8, 1)) +
  theme_minimal() +
  scale_fill_manual(values = colplas[2:5]) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  labs(tag = "D") +
  geom_label(aes(label=letters, y= mean_SOC_std - se_SOC_std+0.2), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)

p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/SOC_std.png",width = width_1, height = 3.5, dpi = 300)




# Make a line plot
#### For SOC_std ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_SOC_std, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas[2:5]) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "Incubation time",
       y = a12s) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "D") 

p1
ggsave(p1, file="Figures/SOC_std_lineplot.png",width = 6, height = 4, dpi = 300)










#### FIGURE 3


# Make a Bar plot
#### For priming ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_priming = mean(priming),
            se_priming = sd(priming) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="priming"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
data_with_se <- data_with_se[-which(data_with_se$Plastic=="NONE"),]
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA


# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_priming, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_priming - se_priming, ymax = mean_priming + se_priming),
                position = position_dodge(width=0.95), width = 0.25) +
  labs(x = "Incubation time (d)",
       y = expression(paste("Daily priming (", mu,"g g"^-1," day"^-1,")"))) +
  theme_minimal() +
  scale_fill_manual(values = colplas[2:5]) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "A") +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  geom_label(aes(label=letters, y= mean_priming - se_priming+1.5), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)
p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/daily_priming.png",width = width_1, height = 3.5, dpi = 300)


# Make a line plot
#### For priming ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_priming, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_hline(yintercept=0) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas[2:5]) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "Incubation day",y = expression(paste("Daily priming (", mu,"g g"^-1," day"^-1,")"))) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "A")

p1 <- p1 + theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
           axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
           legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/daily_priming_lineplot.png",width = width_1, height = 4, dpi = 300)







# Make a Bar plot
#### For cumulative_priming ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_cumulative_priming = mean(cumulative_priming),
            se_cumulative_priming = sd(cumulative_priming) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="cumulative_priming"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
data_with_se <- data_with_se[-which(data_with_se$Plastic=="NONE"),]
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA


# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_cumulative_priming, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_cumulative_priming - se_cumulative_priming, ymax = mean_cumulative_priming + se_cumulative_priming),
                position = position_dodge(width=0.95), width = 0.25) +
  labs(x = "Incubation time (d)",
       y = expression(paste("Cumulative priming (", mu,"g g"^-1,") "))) +
  theme_minimal() +
  scale_fill_manual(values = colplas[2:5]) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  labs(tag = "B") +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  geom_label(aes(label=letters, y= mean_cumulative_priming - se_cumulative_priming+50), position=position_dodge(width=0.95), 
             hjust=0.5,size=3.5, fontface="bold", label.size = NA, show.legend = F)

p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/cumulative_priming.png",width = width_1, height = 3.5, dpi = 300)




# Make a line plot
#### For cumulative_priming ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_cumulative_priming, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_hline(yintercept=0) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas[2:5]) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "Incubation time",y = expression(paste("Cumulative priming (", mu,"g g"^-1,") "))) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  labs(tag = "B") 

p1
ggsave(p1, file="Figures/cumulative_priming_lineplot.png",width = 6, height = 4, dpi = 300)



















# Make a Bar plot
#### For cumulative_priming_relative ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_cumulative_priming_relative = mean(cumulative_priming_relative),
            se_cumulative_priming_relative = sd(cumulative_priming_relative) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="cumulative_priming_relative"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
data_with_se <- data_with_se[-which(data_with_se$Plastic=="NONE"),]
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA


# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_cumulative_priming_relative, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_cumulative_priming_relative - se_cumulative_priming_relative, ymax = mean_cumulative_priming_relative + se_cumulative_priming_relative),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = cumulative_priming_relative), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time (d)",
       y = expression(paste("Relative priming (%) "))) +
  theme_minimal() +
  scale_fill_manual(values = colplas[2:5]) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_cumulative_priming_relative + se_cumulative_priming_relative+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "B") +
  geom_label(aes(label=letters, y= mean_cumulative_priming_relative - se_cumulative_priming_relative+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)

p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/cumulative_priming_relative.png",width = width_1, height = 3.5, dpi = 300)




# Make a line plot
#### For cumulative_priming_relative ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_cumulative_priming_relative, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_hline(yintercept=0) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas[2:5]) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  #geom_errorbar(aes(ymin = mean_cumulative_priming_relative - se_cumulative_priming_relative, ymax = mean_cumulative_priming_relative + se_cumulative_priming_relative), linetype=1, width=0) +
  #geom_point(aes(y = cumulative_priming_relative), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",y = expression(paste("Cumulative priming (%) "))) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_cumulative_priming_relative + se_cumulative_priming_relative+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "B") #+
#geom_label(aes(label=letters, y= mean_cumulative_priming_relative - se_cumulative_priming_relative-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/cumulative_priming_relative_lineplot.png",width = 6, height = 4, dpi = 300)
















# Make a Bar plot
#### For daily_CO2 ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_daily_CO2 = mean(daily_CO2),
            se_daily_CO2 = sd(daily_CO2) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="daily_CO2"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_daily_CO2, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_daily_CO2 - se_daily_CO2, ymax = mean_daily_CO2 + se_daily_CO2),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = daily_CO2), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time (d)",
       y = dat_names[1]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_daily_CO2 + se_daily_CO2+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "A") +
  geom_label(aes(label=letters, y= mean_daily_CO2 - se_daily_CO2+5), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)

p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/daily_CO2.png",width = width_1, height = 3.5, dpi = 300)




# Make a line plot
#### For daily_CO2 ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_daily_CO2, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  #geom_errorbar(aes(ymin = mean_daily_CO2 - se_daily_CO2, ymax = mean_daily_CO2 + se_daily_CO2), linetype=1, width=0) +
  #geom_point(aes(y = daily_CO2), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time (d)",y = expression(paste("CO"[2], " flux (", mu, "g g"^-1, "day"^-1,")"))) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_daily_CO2 + se_daily_CO2+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "A") #+
#geom_label(aes(label=letters, y= mean_daily_CO2 - se_daily_CO2-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/daily_CO2_lineplot.png",width = 8, height = 4, dpi = 300)











# Make a Bar plot
#### For daily_CO2_plastic ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_daily_CO2_plastic = mean(daily_CO2_plastic),
            se_daily_CO2_plastic = sd(daily_CO2_plastic) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="daily_CO2_plastic"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_daily_CO2_plastic, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_daily_CO2_plastic - se_daily_CO2_plastic, ymax = mean_daily_CO2_plastic + se_daily_CO2_plastic),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = daily_CO2_plastic), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time (d)",
       y = dat_names[2]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_daily_CO2_plastic + se_daily_CO2_plastic+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "B") +
  geom_label(aes(label=letters, y= mean_daily_CO2_plastic - se_daily_CO2_plastic+2), 
             position=position_dodge(width=0.95), hjust=0.5,size=2, fontface="bold", label.size = NA, show.legend = F)

p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/daily_CO2_plastic.png",width = width_1, height = 3.5, dpi = 300)




# Make a line plot
#### For daily_CO2_plastic ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_daily_CO2_plastic, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  #geom_errorbar(aes(ymin = mean_daily_CO2_plastic - se_daily_CO2_plastic, ymax = mean_daily_CO2_plastic + se_daily_CO2_plastic), linetype=1, width=0) +
  #geom_point(aes(y = daily_CO2_plastic), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",y = dat_names[2]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_daily_CO2_plastic + se_daily_CO2_plastic+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "B") #+
#geom_label(aes(label=letters, y= mean_daily_CO2_plastic - se_daily_CO2_plastic-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/daily_CO2_plastic_lineplot.png",width = 6, height = 4, dpi = 300)












# Make a Bar plot
#### For daily_CO2_native ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_daily_CO2_native = mean(daily_CO2_native),
            se_daily_CO2_native = sd(daily_CO2_native) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="daily_CO2_native"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_daily_CO2_native, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_daily_CO2_native - se_daily_CO2_native, ymax = mean_daily_CO2_native + se_daily_CO2_native),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = daily_CO2_native), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time (d)",
       y = dat_names[3]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_daily_CO2_native + se_daily_CO2_native+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "C") +
  geom_label(aes(label=letters, y= mean_daily_CO2_native - se_daily_CO2_native+1.5), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)

p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/daily_CO2_native.png",width = width_1, height = 3.5, dpi = 300)






# Make a line plot
#### For daily_CO2_native ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_daily_CO2_native, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  #geom_errorbar(aes(ymin = mean_daily_CO2_native - se_daily_CO2_native, ymax = mean_daily_CO2_native + se_daily_CO2_native), linetype=1, width=0) +
  #geom_point(aes(y = daily_CO2_native), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",y = dat_names[3]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_daily_CO2_native + se_daily_CO2_native+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "C") #+
#geom_label(aes(label=letters, y= mean_daily_CO2_native - se_daily_CO2_native-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/daily_CO2_native_lineplot.png",width = 6, height = 4, dpi = 300)














# Make a Bar plot
#### For priming_relative ###
# Calculate mean and standard error
se_data <- dat_CO2 %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_priming_relative = mean(priming_relative),
            se_priming_relative = sd(priming_relative) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_CO2, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="priming_relative"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
data_with_se$letters <- rep(NA, dim(data_with_se)[1])
data_with_se$Time <- as.numeric(data_with_se$Time)
data_with_se <- data_with_se[-which(data_with_se$Plastic=="NONE"),]
testlet_all_1$timepoint <- as.numeric(testlet_all_1$timepoint)
for(i in 1:dim(data_with_se)[1]){
  if(data_with_se$Time[i] %in% unique(testlet_all_1$timepoint))
  {data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$timepoint==data_with_se$Time[i] & 
                                                           testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                           testlet_all_1$Plastic==data_with_se$Plastic[i])]}
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA


# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se[which(data_with_se$Time %in% c(5,15,30,193)),], aes(x = as.factor(Time), y = mean_priming_relative, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_priming_relative - se_priming_relative, ymax = mean_priming_relative + se_priming_relative),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = priming_relative), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time (d)",
       y = expression(paste("Relative priming (%)"))) +
  theme_minimal() +
  scale_fill_manual(values = colplas[2:5]) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_priming_relative + se_priming_relative+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "A") +
  geom_label(aes(label=letters, y= mean_priming_relative - se_priming_relative+0.3), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)

p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/daily_priming_relative.png",width = width_1, height = 3.5, dpi = 300)




# Make a line plot
#### For priming_relative ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_priming_relative, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_hline(yintercept=0) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas[2:5]) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  #geom_errorbar(aes(ymin = mean_priming_relative - se_priming_relative, ymax = mean_priming_relative + se_priming_relative), linetype=1, width=0) +
  #geom_point(aes(y = priming_relative), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation day",y = expression(paste("Daily priming (%)"))) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_priming_relative + se_priming_relative+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "A") #+
#geom_label(aes(label=letters, y= mean_priming_relative - se_priming_relative-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/daily_priming_relative_lineplot.png",width = 9, height = 3, dpi = 300)






















# Make a Bar plot
#### For TN ###
# Calculate mean and standard error
se_data <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_TN = mean(TN),
            se_TN = sd(TN) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_soil, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="TN"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
for(i in 1:dim(data_with_se)[1]){
  data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$Time==data_with_se$Time[i] & 
                                                          testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                          testlet_all_1$Plastic==data_with_se$Plastic[i])]
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_TN*10, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = (mean_TN - se_TN)*10, ymax = (mean_TN + se_TN)*10),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = TN), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",
       y = dat_names[13]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  labs(tag = "C") +
  geom_label(aes(label=letters, y= 10*(mean_TN - se_TN)+0.2), position=position_dodge(width=0.95), 
             hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend=F)

p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/TN.png",width = width_1, height = 3.5, dpi = 300)




# Make a line plot
#### For TN ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_TN, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  geom_errorbar(aes(ymin = mean_TN - se_TN, ymax = mean_TN + se_TN), linetype=1, width=0) +
  #geom_point(aes(y = TN), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",y = dat_names[13]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_TN + se_TN+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "C") #+
#geom_label(aes(label=letters, y= mean_TN - se_TN-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/TN_lineplot.png",width = 8, height = 3, dpi = 300)







# Make a Bar plot
#### For MBC ###
# Calculate mean and standard error
se_data <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_MBC = mean(MBC),
            se_MBC = sd(MBC) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_soil, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="MBC"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
for(i in 1:dim(data_with_se)[1]){
  data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$Time==data_with_se$Time[i] & 
                                                          testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                          testlet_all_1$Plastic==data_with_se$Plastic[i])]
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_MBC, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_MBC - se_MBC, ymax = mean_MBC + se_MBC),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = MBC), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",
       y = dat_names[5]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  labs(tag = "A") +
  geom_label(aes(label=letters, y= mean_MBC - se_MBC-20), position=position_dodge(width=0.95), 
             hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F) 

p1 <- p1 + 
  theme(text = element_text(size=font_1, family = "Arial"), axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/MBC.png",width = width_1, height = 3.5, dpi = 300)




# Make a line plot
#### For MBC ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_MBC, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  geom_errorbar(aes(ymin = mean_MBC - se_MBC, ymax = mean_MBC + se_MBC), linetype=1, width=0) +
  #geom_point(aes(y = MBC), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",y = dat_names[5]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_MBC + se_MBC+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "A") #+
  #geom_label(aes(label=letters, y= mean_MBC - se_MBC-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/MBC_lineplot.png",width = 8, height = 3, dpi = 300)





# Make a Bar plot
#### For DOC ###
# Calculate mean and standard error
se_data <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_DOC = mean(DOC),
            se_DOC = sd(DOC) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_soil, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="DOC"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
for(i in 1:dim(data_with_se)[1]){
  data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$Time==data_with_se$Time[i] & 
                                                          testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                          testlet_all_1$Plastic==data_with_se$Plastic[i])]
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_DOC, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_DOC - se_DOC, ymax = mean_DOC + se_DOC),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = DOC), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",
       y = dat_names[6]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15)) +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  labs(tag = "B") +
  geom_label(aes(label=letters, y= mean_DOC - se_DOC-5), position=position_dodge(width=0.95), 
             hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)

  
p1 <- p1 + 
  theme(text = element_text(size=font_1, family = "Arial"), axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/DOC.png",width = width_1, height = 3.5, dpi = 300)



# Make a line plot
#### For DOC ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_DOC, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  geom_errorbar(aes(ymin = mean_DOC - se_DOC, ymax = mean_DOC + se_DOC), linetype=1, width=0) +
  #geom_point(aes(y = DOC), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",y = dat_names[6]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_DOC + se_DOC+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "B") #+
#geom_label(aes(label=letters, y= mean_DOC - se_DOC-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/DOC_lineplot.png",width = 8, height = 3, dpi = 300)





# Make a Bar plot
#### For cacq ###
# Calculate mean and standard error
se_data <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_cacq = mean(cacq),
            se_cacq = sd(cacq) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_soil, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="cacq"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
for(i in 1:dim(data_with_se)[1]){
  data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$Time==data_with_se$Time[i] & 
                                                          testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                          testlet_all_1$Plastic==data_with_se$Plastic[i])]
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_cacq, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_cacq - se_cacq, ymax = mean_cacq + se_cacq),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = cacq), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",
       y = dat_names[7]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 12)) +
  # geom_text(aes(label = letters, y= mean_MBC + se_MBC+20), hjust=0.5, width=0.95,#alpha=1,
  #           position = position_dodge(width=0.95), size=3)  +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  labs(tag = "C") +
  geom_label(aes(label=letters, y= mean_cacq - se_cacq-5), position=position_dodge(width=0.95), 
             hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)


p1 <- p1 + 
  theme(text = element_text(size=font_1, family = "Arial"),
    axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=13, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/cacq.png",width = width_1, height = 3.5, dpi = 300)


# Make a line plot
#### For cacq ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_cacq, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  geom_errorbar(aes(ymin = mean_cacq - se_cacq, ymax = mean_cacq + se_cacq), linetype=1, width=0) +
  #geom_point(aes(y = cacq), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",y = dat_names[7]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 12))  +   
  #geom_text(aes(label=letters, y= mean_cacq + se_cacq+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "C") #+
#geom_label(aes(label=letters, y= mean_cacq - se_cacq-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/cacq_lineplot.png",width = 8, height = 3, dpi = 300)











# Make a Bar plot
#### For LAP ###
# Calculate mean and standard error
se_data <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_LAP = mean(LAP),
            se_LAP = sd(LAP) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_soil, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="LAP"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
for(i in 1:dim(data_with_se)[1]){
  data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$Time==data_with_se$Time[i] & 
                                                          testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                          testlet_all_1$Plastic==data_with_se$Plastic[i])]
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_LAP, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_LAP - se_LAP, ymax = mean_LAP + se_LAP),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = LAP), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",
       y = dat_names[8]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15)) +
  # geom_text(aes(label = letters, y= mean_LAP + se_LAP+20), hjust=0.5, width=0.95,#alpha=1,
  #           position = position_dodge(width=0.95), size=3)  +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  labs(tag = "D") +
  geom_label(aes(label=letters, y= mean_LAP - se_LAP-10), position=position_dodge(width=0.95), 
             hjust=0.5,size=size_labels, fontface="bold", label.size = NA, show.legend = F)


p1 <- p1 + 
  theme(text = element_text(size=font_1, family = "Arial"), axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/LAP.png",width = width_1, height = 3.5, dpi = 300)



# Make a line plot
#### For LAP ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_LAP, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  geom_errorbar(aes(ymin = mean_LAP - se_LAP, ymax = mean_LAP + se_LAP), linetype=1, width=0) +
  #geom_point(aes(y = LAP), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",y = dat_names[8]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_LAP + se_LAP+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "D") #+
#geom_label(aes(label=letters, y= mean_LAP - se_LAP-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/LAP_lineplot.png",width = 8, height = 3, dpi = 300)





















# Make a Bar plot
#### For Ammonium ###
# Calculate mean and standard error
se_data <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_Ammonium = mean(Ammonium),
            se_Ammonium = sd(Ammonium) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_soil, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="Ammonium"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
for(i in 1:dim(data_with_se)[1]){
  data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$Time==data_with_se$Time[i] & 
                                                          testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                          testlet_all_1$Plastic==data_with_se$Plastic[i])]
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_Ammonium, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_Ammonium - se_Ammonium, ymax = mean_Ammonium + se_Ammonium),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = Ammonium), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",
       y = dat_names[9]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15)) +
  # geom_text(aes(label = letters, y= mean_Ammonium + se_Ammonium+20), hjust=0.5, width=0.95,#alpha=1,
  #           position = position_dodge(width=0.95), size=3)  +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  labs(tag = "A") +
  geom_label(aes(label=letters, y= mean_Ammonium + se_Ammonium+1.5), position=position_dodge(width=0.95), 
             hjust=0.5, vjust=1,size=3, fontface="bold", label.size = NA, show.legend = F)


p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/Ammonium.png",width = width_1, height = 3.5, dpi = 300)



# Make a line plot
#### For Ammonium ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_Ammonium, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  geom_errorbar(aes(ymin = mean_Ammonium - se_Ammonium, ymax = mean_Ammonium + se_Ammonium), linetype=1, width=0) +
  #geom_point(aes(y = Ammonium), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",y = dat_names[9]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_Ammonium + se_Ammonium+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "A") #+
#geom_label(aes(label=letters, y= mean_Ammonium - se_Ammonium-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/Ammonium_lineplot.png",width = 8, height = 3, dpi = 300)



















# Make a Bar plot
#### For Nitrate ###
# Calculate mean and standard error
se_data <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_Nitrate = mean(Nitrate),
            se_Nitrate = sd(Nitrate) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_soil, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="Nitrate"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
for(i in 1:dim(data_with_se)[1]){
  data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$Time==data_with_se$Time[i] & 
                                                          testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                          testlet_all_1$Plastic==data_with_se$Plastic[i])]
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_Nitrate, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_Nitrate - se_Nitrate, ymax = mean_Nitrate + se_Nitrate),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = Nitrate), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",
       y = dat_names[10]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15)) +
  # geom_text(aes(label = letters, y= mean_Nitrate + se_Nitrate+20), hjust=0.5, width=0.95,#alpha=1,
  #           position = position_dodge(width=0.95), size=3)  +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  labs(tag = "B") +
  geom_label(aes(label=letters, y= mean_Nitrate + se_Nitrate+15), position=position_dodge(width=0.95), 
             hjust=0.5, vjust=1,size=3, fontface="bold", label.size = NA, show.legend = F)


p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/Nitrate.png",width = width_1, height = 3.5, dpi = 300)



# Make a line plot
#### For Nitrate ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_Nitrate, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  geom_errorbar(aes(ymin = mean_Nitrate - se_Nitrate, ymax = mean_Nitrate + se_Nitrate), linetype=1, width=0) +
  #geom_point(aes(y = Nitrate), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",y = dat_names[10]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_Nitrate + se_Nitrate+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "B") #+
#geom_label(aes(label=letters, y= mean_Nitrate - se_Nitrate-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1 
ggsave(p1, file="Figures/Nitrate_lineplot.png",width = 8, height = 3, dpi = 300)















# Make a Bar plot
#### For pH ###
# Calculate mean and standard error
se_data <- dat_soil %>%
  group_by(Time, Plastic, Nitrogen) %>%
  summarize(mean_pH = mean(pH),
            se_pH = sd(pH) / sqrt(n())) %>%
  ungroup()

# Merge the original data with the standard error data
data_with_se <- merge(dat_soil, se_data, by = c("Time", "Plastic", "Nitrogen"))
# add tukey's letters
testlet_all_1 <- testlet_all[which(testlet_all$resp=="pH"),]
testlet_all_1 <- testlet_all_1[order(testlet_all_1$Time, testlet_all_1$Plastic, testlet_all_1$Nitrogen),]
for(i in 1:dim(data_with_se)[1]){
  data_with_se$letters[i] <- testlet_all_1$.group[which(testlet_all_1$Time==data_with_se$Time[i] & 
                                                          testlet_all_1$Nitrogen==data_with_se$Nitrogen[i] & 
                                                          testlet_all_1$Plastic==data_with_se$Plastic[i])]
}
data_with_se$letters[which(data_with_se$Rep %in% c(2:4))] <- NA

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_pH, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.95), width=0.95) +
  geom_errorbar(aes(ymin = mean_pH - se_pH, ymax = mean_pH + se_pH),
                position = position_dodge(width=0.95), width = 0.25) +
  #geom_point(aes(y = pH), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",
       y = dat_names[11]) +
  theme_minimal() +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size=13),text = element_text(size = 15)) +
  # geom_text(aes(label = letters, y= mean_pH + se_pH+20), hjust=0.5, width=0.95,#alpha=1,
  #           position = position_dodge(width=0.95), size=3)  +
  geom_vline(xintercept = c(1.5,2.5,3.5), color="gray44", linetype="dotted") +
  labs(tag = "D") +
  geom_label(aes(label=letters, y= mean_pH + se_pH+0.25), position=position_dodge(width=0.95), 
             hjust=0.5, vjust=0,size=size_labels, fontface="bold", label.size = NA, show.legend = F)


p1 <- p1 + 
  theme(axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
        axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
        legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
ggsave(p1, file="Figures/pH.png",width = width_1, height = 3.5, dpi = 300)



# Make a line plot
#### For pH ###

# Create the bar diagram with data points and error bars
p1 <- ggplot(data_with_se, aes(x = Time, y = mean_pH, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  geom_errorbar(aes(ymin = mean_pH - se_pH, ymax = mean_pH + se_pH), linetype=1, width=0) +
  #geom_point(aes(y = pH), position = position_jitterdodge(jitter.width = 0.1, dodge.width=0.95),
  #         color = "black", size = 1) +
  #facet_wrap(.~Time, nrow=1) +
  labs(x = "Incubation time",y = dat_names[11]) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #geom_text(aes(label=letters, y= mean_pH + se_pH+20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels)+
  labs(tag = "D") #+
#geom_label(aes(label=letters, y= mean_pH - se_pH-20), position=position_dodge(width=0.95), hjust=0.5,size=size_labels, fontface="bold", label.size = NA)



p1
ggsave(p1, file="Figures/pH_lineplot.png",width = 8, height = 3, dpi = 300)








