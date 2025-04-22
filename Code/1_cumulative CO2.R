
### This code calculates daily CO2 flux (ug CO2-C g dry soil-1 day-1) from 
### instantaneous flux (ug CO2-C g dry soil-1 hr-1), interpolates days in between 
### gas sampling, and then calculates cumulative CO2
### emissions across time (ug CO2-C g dry soil-1). It also calculates the 
### amount of CO2-C derived from microplastics vs. native SOM. 

rm(list=ls())
# install libraries, run if needed
# install.packages(c("tibble", "reshape2", "dplyr", "ggplot2", "forecast",
#                    "tidyr", "tidyverse", "readxl", "openxlsx"))


# Load necessary libraries
library(lubridate)
library(ggplot2)
library(zoo)
library(tibble)
library(reshape2)
library(dplyr)
library(ggplot2)
library(forecast)
library(tidyr)
library(tidyverse)
library(readxl)
library(openxlsx)



# colors for different treatments 
colplas <- c("goldenrod2", "indianred1", "steelblue", "lightseagreen", "slateblue1")
colnit <- c("lightblue","salmon")
alphanit <- c(0.3,0.7)
colderive <- c("seashell2","goldenrod4")


### read in data on plastic-C added to soils
plastic_initial <- read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", 
                           sheet='13C and CN of plastics', range = "C70:U75")




### read in CO2 flux data: ug CO2-C g dry soil-1 hr-1
emission_data <- read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", 
                            sheet='LOOKUP_CO2', range = "A2:AH42")
### read in CO2 flux data: ug CO2-C g dry soil-1 hr-1
emission_data_13C <- read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", 
                            sheet='LOOKUP_CO2_13C', range = "A2:AH42")
### read in data on proportion of emissions that are derived from plastic
plastic_data <- read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", 
                            sheet='LOOKUP_plas', range = "A2:AH42")
# replace negative values with 0 (we cannot have a negative proportion)
plastic_data[plastic_data < 0] <- 0







# add treatment information to emission data
trt <- read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", 
                            sheet='LOOKUP_CO2', range = "AJ2:AK42")
colnames(trt) <- c("Plastic", "Nitrogen")
emission_data <- cbind(trt, emission_data)
emission_data_13C <- cbind(trt, emission_data_13C)

# add date information
date <- as.matrix(read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", col_names = FALSE,
                  sheet='LOOKUP_CO2', range = "B1:AH1"))
colnames(date) <- c(1:33)
date.df <- data.frame(date = date[1,])
dates <- data.frame(day = colnames(emission_data)[c(4:36)], 
                    date = format(as.Date((date.df$date)), "%m/%d/%Y"))
colnames(emission_data)[c(4:36)] <- format(as.Date((date.df$date)), "%m/%d/%Y")



# calculate plastic emissions: total emission multiplied by proportion
emission_plastic <- cbind(emission_data[,c(1:3)], emission_data[c(4:36)]*plastic_data[c(2:34)])

### transform emission data to long format
emission_data_long <- reshape2::melt(emission_data, measure.vars=4:36, 
                                     variable.name="Date", value.name="CO2Flux")
emission_data_13C_long <- reshape2::melt(emission_data_13C, measure.vars=4:36, 
                                     variable.name="Date", value.name="delta13C")
emission_plastic_long <- reshape2::melt(emission_plastic, measure.vars=4:36, 
                                        variable.name="Date", value.name="CO2Flux_plastic")

# combine total and plastic-derived emissions
emission_data_long$CO2Flux_plastic <- emission_plastic_long$CO2Flux_plastic

# add day
emission_data_long$Day <- rep(NA, dim(emission_data_long)[1])
for(i in 1:dim(emission_data_long)[1]) {
  emission_data_long$Day[i] <- dates$day[which(emission_data_long$Date[i]==dates$date)]
}
emission_data_13C_long$Day <- rep(NA, dim(emission_data_13C_long)[1])
for(i in 1:dim(emission_data_13C_long)[1]) {
  emission_data_13C_long$Day[i] <- dates$day[which(emission_data_13C_long$Date[i]==dates$day)]
}




# Make a Bar plot
#### For daily_CO2 ###
# Calculate mean and standard error
se_data <- emission_data_13C_long %>%
  group_by(Day, Plastic, Nitrogen) %>%
  summarize(mean_13C = mean(delta13C),
            se_13C = sd(delta13C) / sqrt(n())) %>%
  ungroup()

# # Merge the original data with the standard error data
# data_with_se <- merge(emission_data_13C_long, se_data, by = c("Day", "Plastic", "Nitrogen"))

# Make a line plot
#### For 13C ###

se_data$Day <- as.factor(as.numeric(se_data$Day))
se_data$Plastic <- as.factor(se_data$Plastic)
se_data$Plastic <- factor(se_data$Plastic, levels(se_data$Plastic)[c(2,1,3,4,5)])

# Create the bar diagram with data points and error bars
p1 <- ggplot(se_data, aes(x = Day, y = mean_13C, color=Plastic, linetype=Nitrogen, group = interaction(Plastic, Nitrogen))) +
  geom_line(linewidth=1) +
  scale_color_manual(values = colplas) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "Incubation time (d)",y = expression(paste(delta,""^13, "C (‰)"))) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=13),text = element_text(size = 15))  +   
  #labs(tag = "A") +
  scale_y_continuous(breaks=seq(from=-16,to=-30, by=-2), limits=c(-30, -16))  +
  scale_x_discrete(breaks=levels(se_data$Day)[seq_along(levels(se_data$Day)) %% 2 > 0])   
  

p1
ggsave(p1, file="Figures/13C_lineplot.png",width = 8, height = 4, dpi = 300)









# convert instantaneous rates (ug CO2-C g dry soil-1 hr-1) into daily rates (ug CO2-C g dry soil-1 day-1)
emission_data_long$daily_CO2 <- emission_data_long$CO2Flux*24
emission_data_long$daily_CO2_plastic <- emission_data_long$CO2Flux_plastic*24
# export data
write.csv(emission_data_long, "Processed-data/CO2Flux_Long.csv", row.names=FALSE)


# Convert to a tibble
emission_data_long <- as_tibble(emission_data_long)

# Make as date
emission_data_long$Date <- as.Date(emission_data_long$Date, "%m/%d/%Y")

# list of tubes in each treatment combo
emission_data_long_grouped <- emission_data_long %>%
  group_by(Plastic, Nitrogen) %>%
  summarise(Tube = list(unique(Tube)), .groups = "drop")
  
# Interpolate missing days for each treatment combination
complete_data <- emission_data_long %>%
  group_by(Tube) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "days")) %>%
  mutate(day = as.integer(zoo::na.approx(Day))) %>% # day
  mutate(daily_CO2 = zoo::na.approx(daily_CO2)) %>% # total CO2
  mutate(daily_CO2_plastic = zoo::na.approx(daily_CO2_plastic)) %>% # plastic-derived CO2
  ungroup()



# add missing info for treatments
for(i in 1:dim(emission_data_long_grouped)[1]) {
  complete_data$Plastic[which(complete_data$Tube %in% emission_data_long_grouped$Tube[[i]])] <- emission_data_long_grouped$Plastic[i]
  complete_data$Nitrogen[which(complete_data$Tube %in% emission_data_long_grouped$Tube[[i]])] <- emission_data_long_grouped$Nitrogen[i]
}



# calculate native som-derived CO2
complete_data$daily_CO2_native <- complete_data$daily_CO2-complete_data$daily_CO2_plastic

### calculate priming
# first find the native SOC respired when no plastic is added (cumulative ug C g dry soil-1 day-1)
native_respired <- complete_data %>%
  filter(Plastic=="NONE") %>%
  group_by(Date, Nitrogen) %>%
  summarise(daily_CO2_native = mean(daily_CO2_native, na.rm = TRUE), .groups = "drop")


# calculate the change in native SOC respired when plastic is added (cumulative ug C g dry soil-1 day-1)
complete_data$priming <- rep(NA, dim(complete_data)[1])
complete_data$priming_relative <- rep(NA, dim(complete_data)[1])
for(i in 1:dim(complete_data)[1]){
  complete_data$priming[i] <- complete_data$daily_CO2_native[i] - native_respired$daily_CO2_native[which(native_respired$Nitrogen==complete_data$Nitrogen[i] & native_respired$Date==complete_data$Date[i])]
  complete_data$priming_relative[i] <- (complete_data$daily_CO2_native[i] - native_respired$daily_CO2_native[which(native_respired$Nitrogen==complete_data$Nitrogen[i] & native_respired$Date==complete_data$Date[i])])/native_respired$daily_CO2_native[which(native_respired$Nitrogen==complete_data$Nitrogen[i] & native_respired$Date==complete_data$Date[i])]
}
complete_data$priming[which(complete_data$Plastic=="NONE")] <- NA
complete_data$priming_relative[which(complete_data$Plastic=="NONE")] <- NA


# export data
write.csv(complete_data, "Processed-data/CO2Flux_Long_Interpolated.csv", row.names=FALSE)




# Calculate cumulative CO2 emissions for each treatment combination
cumulative_data <- complete_data %>%
  group_by(Tube) %>%
  mutate(cumulative_CO2 = cumsum(daily_CO2)) %>%
  mutate(cumulative_CO2_plastic = cumsum(daily_CO2_plastic)) %>%
  mutate(cumulative_CO2_native = cumsum(daily_CO2_native)) %>%
  mutate(cumulative_priming = cumsum(priming)) %>%
  mutate(cumulative_priming_relative = cumsum(priming_relative)) %>%
  ungroup()

# add info on amount of initial plastic (ug plastic-C g dry soil-1)
cumulative_data$initial_plastic_c <- rep(NA, dim(cumulative_data)[1])
for(i in 1:dim(cumulative_data)[1]) {
  cumulative_data$initial_plastic_c[i] <- plastic_initial$'ug plastic-C added per gdw soil'[which(plastic_initial$plastic==cumulative_data$Plastic[i])]
}
# calculate the proportion of plastic-C has been respired
cumulative_data$prop_plas_c_resp <- cumulative_data$cumulative_CO2_plastic/cumulative_data$initial_plastic_c
cumulative_data$prop_plas_c_resp[which(cumulative_data$Plastic=="NONE")] <- NA





# export data
write.csv(cumulative_data, "Processed-data/CO2Flux_Long_Interpolated_Cumulative.csv", row.names=FALSE)




##### Plot CO2 emission data: cumulative total CO2
# reorder levels of plastic
cumulative_data <- read.csv("Processed-data/CO2Flux_Long_Interpolated_Cumulative.csv")
cumulative_data$Plastic <- as.factor(cumulative_data$Plastic)
cumulative_data$Plastic <- factor(cumulative_data$Plastic, levels(cumulative_data$Plastic)[c(2,1,3,4,5)])
cumulative_data$Date <- as.Date(cumulative_data$Date)

# Plot cumulative CO2 emissions for all cups
p <- ggplot(cumulative_data, aes(x = Date, y = cumulative_CO2, group=Tube, color = Plastic)) +
  geom_line(aes(linetype=Nitrogen)) +
  scale_color_manual(values=colplas) +
  scale_linetype_manual(values = c("dashed","solid")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Monthly labels (e.g., "Jan")
  labs(x = "Date", y = bquote("Cumulative CO" [2] * " Emissions (" * mu * "g C g"^-1 * "dry soil)")) +
  theme_bw()
p

ggsave(p, file="Figures/CO2-cumulative-cup.png",width = 6, height = 4, dpi = 300)

# Plot cumulative priming for all cups
p <- ggplot(cumulative_data[-which(cumulative_data$Plastic=="NONE"),], aes(x = Date, y = cumulative_priming, group=Tube, color = Plastic)) +
  geom_hline(yintercept = 0)+
  geom_line(aes(linetype=Nitrogen)) +
  scale_color_manual(values=colplas[2:5], breaks = levels(cumulative_data$Plastic)[c(2:5)]) +
  scale_linetype_manual(values = c("dashed","solid")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Monthly labels (e.g., "Jan")
  labs(x = "Date", y = bquote("Cumulative priming (" * mu * "g C g"^-1 * "dry soil)")) +
  theme_bw() 
p

ggsave(p, file="Figures/priming-cumulative-cup.png",width = 6, height = 4, dpi = 300)


# Plot cumulative priming_relative for all cups
p <- ggplot(cumulative_data[-which(cumulative_data$Plastic=="NONE"),], aes(x = Date, y = cumulative_priming_relative, group=Tube, color = Plastic)) +
  geom_hline(yintercept = 0)+
  geom_line(aes(linetype=Nitrogen)) +
  scale_color_manual(values=colplas[2:5], breaks = levels(cumulative_data$Plastic)[c(2:5)]) +
  scale_linetype_manual(values = c("dashed","solid")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Monthly labels (e.g., "Jan")
  labs(x = "Date", y = bquote("Cumulative relative priming (%)")) +
  theme_bw() 
p

ggsave(p, file="Figures/priming_relative-cumulative-cup.png",width = 6, height = 4, dpi = 300)


# Summarize data to calculate treatment averages
cumulative_avg <- cumulative_data %>%
  group_by(Date, Plastic, Nitrogen) %>%
  summarise(mean_CO2 = mean(cumulative_CO2, na.rm = TRUE),
            sd_CO2 = sd(cumulative_CO2, na.rm = TRUE),
            mean_CO2_plastic = mean(cumulative_CO2_plastic, na.rm = TRUE),
            sd_CO2_plastic = sd(cumulative_CO2_plastic, na.rm = TRUE),
            mean_CO2_plastic_prop = mean(prop_plas_c_resp, na.rm = TRUE),
            sd_CO2_plastic_prop = sd(prop_plas_c_resp, na.rm = TRUE),
            mean_CO2_native = mean(cumulative_CO2_native, na.rm = TRUE),
            sd_CO2_native = sd(cumulative_CO2_native, na.rm = TRUE),
            mean_priming = mean(cumulative_priming, na.rm = TRUE),
            se_priming = sd(cumulative_priming, na.rm = TRUE)/2,
            mean_priming_relative = mean(cumulative_priming_relative, na.rm = TRUE),
            se_priming_relative = sd(cumulative_priming_relative, na.rm = TRUE)/2, .groups = "drop")
cumulative_avg <- cumulative_avg[which(cumulative_avg$Date %in% date.df$date),]

write.csv(cumulative_avg, "Processed-data/CO2Flux_Long_Interpolated_Cumulative_avg.csv")


# Plot cumulative CO2 emissions averages
p <- ggplot(cumulative_avg, aes(x = Date, y = mean_CO2, color = Plastic)) +
  geom_errorbar(aes(ymin = mean_CO2 - sd_CO2, ymax = mean_CO2 + sd_CO2), width=0, size=0.5) +
  geom_line(aes(linetype=Nitrogen), size=0.5) + 
  scale_color_manual(values=colplas) +
  scale_linetype_manual(values = c("dashed","solid")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Monthly labels (e.g., "Jan")
  labs(x = "Date", y = bquote("Cumulative CO" [2] * " Emissions (" * mu * "g C g"^-1 * "dry soil)")) +
  theme_bw()
p

ggsave(p, file="Figures/CO2-cumulative-average-sd.png",width = 6, height = 4, dpi = 300)

# Plot cumulative priming  averages
p <- ggplot(cumulative_avg[-which(cumulative_avg$Plastic=="NONE"),], aes(x = Date, y = mean_priming, color = Plastic)) +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin = mean_priming - se_priming, ymax = mean_priming + se_priming), width=2, size=0.5) +
  geom_line(aes(linetype=Nitrogen), size=0.5) + 
  scale_color_manual(values=colplas[c(2:5)]) +
  scale_linetype_manual(values = c("dashed","solid")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Monthly labels (e.g., "Jan")
  labs(x = "Date", y = bquote("Cumulative priming (" * mu * "g C g"^-1 * "dry soil)")) +
  theme_bw()
p

ggsave(p, file="Figures/priming-cumulative-average-se.png",width = 6, height = 4, dpi = 300)




# Plot cumulative priming_relative  averages
p <- ggplot(cumulative_avg[-which(cumulative_avg$Plastic=="NONE"),], aes(x = Date, y = mean_priming_relative, color = Plastic)) +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin = mean_priming_relative - se_priming_relative, ymax = mean_priming_relative + se_priming_relative), width=2, size=0.5) +
  geom_line(aes(linetype=Nitrogen), size=0.5) + 
  scale_color_manual(values=colplas[c(2:5)]) +
  scale_linetype_manual(values = c("dashed","solid")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Monthly labels (e.g., "Jan")
  labs(x = "Date", y = bquote("Cumulative relative priming (%)")) +
  theme_bw()
p

ggsave(p, file="Figures/priming_relative-cumulative-average-se.png",width = 6, height = 4, dpi = 300)




#### Bar plot for cumulative CO2 emissions by plastic type and nitrogen level with sd
font_1 <- 18
cumulative_avg_last <- cumulative_avg[cumulative_avg$Date=="2023-09-14",] 
p <- ggplot(cumulative_avg_last, aes(x = Plastic, y = mean_CO2, fill = Plastic, alpha=Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), size=0.2) +
  geom_errorbar(aes(ymin = mean_CO2 - sd_CO2,ymax = mean_CO2 + sd_CO2), 
    position = position_dodge(width = 0.9), size=0.4, width = 0.25) +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  labs(x = "Plastic", y = bquote("Cumulative CO" [2] * " (" * mu * "g C g"^-1 * "dry soil)")) +
  theme_minimal() + theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                          axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
                          axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
                          legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial"))
p

ggsave(p, file="Figures/CO2-cumulative-bars-average-sd.png",width = 8, height = 5, dpi = 300)







#### Bar plot for cumulative priming by plastic type and nitrogen level with sd
cumulative_avg_last <- cumulative_avg[cumulative_avg$Date=="2023-09-14",] 
p <- ggplot(cumulative_avg_last[-which(cumulative_avg_last$Plastic=="NONE"),], aes(x = Plastic, y = mean_priming, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color="black", size=0.2) +
  geom_errorbar(aes(ymin = mean_priming - se_priming,ymax = mean_priming + se_priming), 
                position = position_dodge(width = 0.9), size=0.4, width = 0.25) +
  scale_fill_manual(values=colnit) +
  labs(x = "Plastic", y = bquote("Cumulative priming (" * mu * "g C g"^-1 * "dry soil)")) +
  theme_bw()
p

ggsave(p, file="Figures/priming-cumulative-bars-average-se_plastic-c.png",width = 6, height = 4, dpi = 300)

#### Bar plot for cumulative priming_relative by plastic type and nitrogen level with sd
cumulative_avg_last <- cumulative_avg[cumulative_avg$Date=="2023-09-14",] 
p <- ggplot(cumulative_avg_last[-which(cumulative_avg_last$Plastic=="NONE"),], aes(x = Plastic, y = mean_priming_relative, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color="black", size=0.2) +
  geom_errorbar(aes(ymin = mean_priming_relative - se_priming_relative, ymax = mean_priming_relative + se_priming_relative), 
                position = position_dodge(width = 0.9), size=0.4, width = 0.25) +
  scale_fill_manual(values=colnit) +
  labs(x = "Plastic", y = bquote("Cumulative relative priming (%)")) +
  theme_bw()
p

ggsave(p, file="Figures/priming_relative-cumulative-bars-average-se_plastic-c.png",width = 6, height = 4, dpi = 300)



#### Bar plot for cumulative proportion of plastic c respired as CO2  by plastic type and nitrogen level with sd
cumulative_avg_last <- cumulative_avg[cumulative_avg$Date=="2023-09-14",] 
p <- ggplot(cumulative_avg_last, aes(x = Plastic, y = mean_CO2_plastic_prop, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color="black", size=0.2) +
  geom_errorbar(aes(ymin = mean_CO2_plastic_prop - sd_CO2_plastic_prop,ymax = mean_CO2_plastic_prop + sd_CO2_plastic_prop), 
                position = position_dodge(width = 0.9), size=0.4, width = 0.25) +
  scale_fill_manual(values=colnit) +
  labs(x = "Plastic", y = bquote("Proportion of plastic-C respired")) +
  theme_bw()
p

ggsave(p, file="Figures/CO2-cumulative-bars-average-sd_plastic-c.png",width = 6, height = 4, dpi = 300)


#### Bar plot for cumulative proportion of plastic c mineralized by plastic type and nitrogen level with sd
cumulative_avg_last <- cumulative_avg[cumulative_avg$Date=="2023-09-14",] 
p <- ggplot(cumulative_avg_last, aes(x = Plastic, y = mean_CO2_plastic_prop, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color="black", size=0.2) +
  geom_errorbar(aes(ymin = (mean_CO2_plastic_prop - sd_CO2_plastic_prop),ymax = (mean_CO2_plastic_prop + sd_CO2_plastic_prop)), 
                position = position_dodge(width = 0.9), size=0.4, width = 0.25) +
  scale_fill_manual(values=colnit) +
  labs(x = "Plastic", y = bquote("Proportion of plastic-C mineralized")) +
  theme_bw() +
  labs(tag="A)")
p

ggsave(p, file="Figures/CO2-cumulative-bars-average-sd_plastic-c-mineralized.png",width = 5, height = 3, dpi = 300)


#### Bar plot for cumulative proportion of plastic c mineralized by plastic type and nitrogen level with sd
p <- ggplot(cumulative_avg_last, aes(x = Plastic, y = mean_CO2_plastic_prop, fill = Plastic, alpha=Nitrogen)) +
  theme_bw() + theme( panel.grid.major = element_blank(), text = element_text(size=font_1, family = "Arial"),# panel.border = element_rect(colour = "black"),
                      panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                      axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
                      axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
                      legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), size=0.2) +
  geom_errorbar(aes(ymin = mean_CO2_plastic_prop - sd_CO2_plastic_prop,ymax = mean_CO2_plastic_prop + sd_CO2_plastic_prop), 
                position = position_dodge(width = 0.9), size=0.4, width = 0.25) +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  labs(x = "Plastic", y = bquote("Proportion of plastic-C mineralized"), tag = "A") 
p

ggsave(p, file="Figures/CO2-cumulative-bars-average-sd_plastic-c.png",width = 8, height = 5, dpi = 300)










#### Bar plot for cumulative proportion of SOM-c mineralized in soil by plastic type and nitrogen level with sd
cumulative_avg_last <- cumulative_avg[cumulative_avg$Date=="2023-09-14",] 
cumulative_avg_last$mean_CO2_native_prop <- cumulative_avg_last$mean_CO2_native/(11.54*1000)
cumulative_avg_last$sd_CO2_native_prop <- cumulative_avg_last$sd_CO2_native/(11.54*1000)
p <- ggplot(cumulative_avg_last, aes(x = Plastic, y = mean_CO2_native_prop, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color="black", size=0.2) +
  geom_errorbar(aes(ymin = (mean_CO2_native_prop - sd_CO2_native_prop),ymax = (mean_CO2_native_prop + sd_CO2_native_prop)), 
                position = position_dodge(width = 0.9), size=0.4, width = 0.25) +
  scale_fill_manual(values=colnit) +
  labs(x = "Plastic", y = bquote("Proportion of soil-C mineralized")) +
  theme_bw() +
  labs(tag="B)")
p

ggsave(p, file="Figures/CO2-cumulative-bars-average-sd_soil-c-mineralized.png",width = 5, height = 3, dpi = 300)




#### Bar plot for cumulative proportion of SOM-c mineralized in soil by plastic type and nitrogen level with sd
p <- ggplot(cumulative_avg_last, aes(x = Plastic, y = mean_CO2_native_prop, fill = Plastic, alpha=Nitrogen)) +
  theme_bw() + theme( panel.grid.major = element_blank(), text = element_text(size=font_1, family = "Arial"),# panel.border = element_rect(colour = "black"),
                          panel.grid.minor = element_blank(), #axis.line = element_line(colour = "black"),
                          axis.text.x = element_text(size=font_1, family = "Arial"), axis.text.y = element_text(size=font_1, family = "Arial"), 
                          axis.title.x = element_text(size=font_1, family = "Arial"), axis.title.y = element_text(size=font_1, family = "Arial"), 
                          legend.text = element_text(size=font_1, family = "Arial"), legend.title = element_text(size=font_1, family = "Arial")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), size=0.2) +
  geom_errorbar(aes(ymin = mean_CO2_native_prop - sd_CO2_native_prop,ymax = mean_CO2_native_prop + sd_CO2_native_prop), 
                position = position_dodge(width = 0.9), size=0.4, width = 0.25) +
  scale_fill_manual(values = colplas) + 
  scale_alpha_manual(values = alphanit) + 
  labs(x = "Plastic", y = bquote("Proportion of SOM-C mineralized"), tag = "B") 
 p

ggsave(p, file="Figures/CO2-cumulative-bars-average-sd_soil-c-mineralized.png",width = 8, height = 5, dpi = 300)




#### Bar plot for plastic- vs. native-derived CO2 emissions by plastic type and nitrogen level with sd
cumulative_avg_last <- cumulative_avg[cumulative_avg$Date=="2023-09-14",] 

# long format
cumulative_avg_last_m <- cumulative_avg_last %>%
  pivot_longer(cols = c("mean_CO2_plastic", "mean_CO2_native"),  # Specify the columns to pivot
               names_to = "Subcategory",           # The name for the new column
               values_to = "mean_CO2_stack")  
cumulative_avg_last_se <- cumulative_avg_last %>%
  pivot_longer(cols = c("sd_CO2_plastic", "sd_CO2_native"),  # Specify the columns to pivot
               names_to = "Subcategory",           # The name for the new column
               values_to = "sd_CO2_stack")  
cumulative_avg_last_m$sd_CO2_stack <- cumulative_avg_last_se$sd_CO2_stack
cumulative_avg_last <- cumulative_avg_last_m

# rename column for plastic- vs. native som-derived CO2-C
cumulative_avg_last <- cumulative_avg_last %>%
  mutate(Subcategory = dplyr::recode(Subcategory, 
                       "mean_CO2_plastic" = "Plastic", 
                       "mean_CO2_native" = "Native"))  # Rename subcategories
 cumulative_avg_last$Subcategory <- as.factor(cumulative_avg_last$Subcategory)
 cumulative_avg_last$Subcategory <- factor(cumulative_avg_last$Subcategory, levels(cumulative_avg_last$Subcategory)[c(2,1)])
 cumulative_avg_last <- cumulative_avg_last[order(rev(cumulative_avg_last$Subcategory)),]


# Calculate cumulative sum for stacking (use this to correctly position error bars)
cumulative_avg_last <- cumulative_avg_last %>%
  group_by(Nitrogen, Plastic) %>%
  mutate(
    ypos = cumsum(mean_CO2_stack), #- mean_CO2_stack / 2,  # Middle of the stacked bars
    ymin = ypos - sd_CO2_stack,                   # Lower bound of the error bar
    ymax = ypos + sd_CO2_stack                    # Upper bound of the error bar
  )
cumulative_avg_last$ymin[which(cumulative_avg_last$Plastic=="NONE" & cumulative_avg_last$Subcategory=="Plastic")] <- NA
cumulative_avg_last$ymax[which(cumulative_avg_last$Plastic=="NONE" & cumulative_avg_last$Subcategory=="Plastic")] <- NA

p <- ggplot(cumulative_avg_last, aes(x = Nitrogen, y = mean_CO2_stack, fill = Subcategory)) +
  geom_bar(stat = "identity", position = "stack", color="black", size=0.1) +
  facet_grid(.~Plastic) +
  scale_fill_manual(values=colderive) +
  labs(x = "Plastic", y = bquote("Cumulative CO" [2] * " Emissions (" * mu * "g C g"^-1 * "dry soil)")) +
  theme_bw() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), size=0.2,        # Add error bars
                #position = position_stack(vjust = 0.5),  # Position error bars on stacked bars
                width = 0.05) 
  
p

ggsave(p, file="Figures/CO2-cumulative-bars-average-sd-stacked.png",width = 6, height = 4, dpi = 300)
 




write.csv(cumulative_avg_last, "Processed-data/CO2_cumulative_average_final.csv")





#### Bar plot for plastic- vs. native-derived CO2 emissions by plastic type and nitrogen level with sd
cumulative_avg_last <- cumulative_avg[cumulative_avg$Date=="2023-09-14",] 

# long format
cumulative_avg_last_m <- cumulative_avg_last %>%
  pivot_longer(cols = c("mean_CO2_plastic", "mean_CO2_native"),  # Specify the columns to pivot
               names_to = "Subcategory",           # The name for the new column
               values_to = "mean_CO2_stack")  
cumulative_avg_last_se <- cumulative_avg_last %>%
  pivot_longer(cols = c("sd_CO2_plastic", "sd_CO2_native"),  # Specify the columns to pivot
               names_to = "Subcategory",           # The name for the new column
               values_to = "sd_CO2_stack")  
cumulative_avg_last_m$sd_CO2_stack <- cumulative_avg_last_se$sd_CO2_stack
cumulative_avg_last <- cumulative_avg_last_m

# rename column for plastic- vs. native som-derived CO2-C
cumulative_avg_last <- cumulative_avg_last %>%
  mutate(Subcategory = dplyr::recode(Subcategory, 
                       "mean_CO2_plastic" = "Plastic", 
                       "mean_CO2_native" = "Native"))  # Rename subcategories
 cumulative_avg_last$Subcategory <- as.factor(cumulative_avg_last$Subcategory)
 cumulative_avg_last$Subcategory <- factor(cumulative_avg_last$Subcategory, levels(cumulative_avg_last$Subcategory)[c(2,1)])
 cumulative_avg_last <- cumulative_avg_last[order(rev(cumulative_avg_last$Subcategory)),]


# Calculate cumulative sum for stacking (use this to correctly position error bars)
cumulative_avg_last <- cumulative_avg_last %>%
  group_by(Nitrogen, Plastic) %>%
  mutate(
    ypos = cumsum(mean_CO2_stack), #- mean_CO2_stack / 2,  # Middle of the stacked bars
    ymin = ypos - sd_CO2_stack,                   # Lower bound of the error bar
    ymax = ypos + sd_CO2_stack                    # Upper bound of the error bar
  )
cumulative_avg_last$ymin[which(cumulative_avg_last$Plastic=="NONE" & cumulative_avg_last$Subcategory=="Plastic")] <- NA
cumulative_avg_last$ymax[which(cumulative_avg_last$Plastic=="NONE" & cumulative_avg_last$Subcategory=="Plastic")] <- NA

p <- ggplot(cumulative_avg_last, aes(x = Nitrogen, y = mean_CO2_stack, fill = Subcategory)) +
  geom_bar(stat = "identity", position = "stack", color="black", size=0.1) +
  facet_grid(.~Plastic) +
  scale_fill_manual(values=colderive) +
  labs(x = "Plastic", y = bquote("Cumulative CO" [2] * " Emissions (" * mu * "g C g"^-1 * "dry soil)")) +
  theme_bw() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), size=0.2,        # Add error bars
                #position = position_stack(vjust = 0.5),  # Position error bars on stacked bars
                width = 0.05) 
  
p

ggsave(p, file="Figures/CO2-cumulative-bars-average-sd-stacked.png",width = 6, height = 4, dpi = 300)
 

write.csv(cumulative_avg_last, "Processed-data/CO2_cumulative_average_final.csv")

















