
rm(list=ls())

# Load necessary libraries
library(lubridate)
library(ggplot2)
library(zoo)
install.packages("tibble")
library(tibble)
install.packages("reshape2")
library(reshape2)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("forecast")
library(forecast)
install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
library(readxl)
install.packages("openxlsx")
library(openxlsx)



# colors for differnt levels 
colplas <- c("black", "red", "orange", "blue", "purple")
colnit <- c("lightblue","salmon")
colplas <- c("seashell2","goldenrod4")







### read in CO2 flux data: ug CO2-C g dry soil-1 hr-1
emission_data <- read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", 
                            sheet='LOOKUP_CO2', range = "A2:AH42")
### read in data on proportion of emissions that are derived from plastic
plastic_data <- read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", 
                            sheet='LOOKUP_plas', range = "A2:AH42")
# replace negative values with NA
plastic_data[plastic_data < 0] <- 0



# add treatment information
trt <- read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", 
                            sheet='LOOKUP_CO2', range = "AJ2:AK42")
colnames(trt) <- c("Plastic", "Nitrogen")
emission_data <- cbind(trt, emission_data)

# add date information
date <- as.matrix(read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", col_names = FALSE,
                  sheet='LOOKUP_CO2', range = "B1:AH1"))
colnames(date) <- c(1:33)
date.df <- data.frame(date = date[1,])

# dates
colnames(emission_data)[c(4:36)] <- format(as.Date((date.df$date)), "%m/%d/%Y")

# calculate plastic emissions
emission_plastic <- cbind(emission_data[,c(1:3)], emission_data[c(4:36)]*plastic_data[c(2:34)])

### transform to long format
emission_data_long <- reshape2::melt(emission_data, measure.vars=4:36, variable.name="Date", value.name="CO2Flux")
emission_plastic_long <- reshape2::melt(emission_plastic, measure.vars=4:36, variable.name="Date", value.name="CO2Flux_plastic")

# combine
emission_data_long$CO2Flux_plastic <- emission_plastic_long$CO2Flux_plastic

# and convert into ug CO2-C g dry soil-1 day-1
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
  
# Interpolate missing days for each treatment combination: total CO2
complete_data <- emission_data_long %>%
  group_by(Tube) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "days")) %>%
  mutate(daily_CO2 = zoo::na.approx(daily_CO2)) %>% # total CO2
  mutate(daily_CO2_plastic = zoo::na.approx(daily_CO2_plastic)) %>% # plastic-derived CO2
  ungroup()



# update treatments
for(i in 1:dim(emission_data_long_grouped)[1]) {
  complete_data$Plastic[which(complete_data$Tube %in% emission_data_long_grouped$Tube[[i]])] <- emission_data_long_grouped$Plastic[i]
  complete_data$Nitrogen[which(complete_data$Tube %in% emission_data_long_grouped$Tube[[i]])] <- emission_data_long_grouped$Nitrogen[i]
}

# calculate native som-derived CO2
complete_data$daily_CO2_native <- complete_data$daily_CO2-complete_data$daily_CO2_plastic

# export data
write.csv(complete_data, "Processed-data/CO2Flux_Long_Interpolated.csv", row.names=FALSE)


# Calculate cumulative CO2 emissions for each treatment combination
cumulative_data <- complete_data %>%
  group_by(Tube) %>%
  mutate(cumulative_CO2 = cumsum(daily_CO2)) %>%
  mutate(cumulative_CO2_plastic = cumsum(daily_CO2_plastic)) %>%
  mutate(cumulative_CO2_native = cumsum(daily_CO2_native)) %>%
  ungroup()
# export data
write.csv(cumulative_data, "Processed-data/CO2Flux_Long_Interpolated_Cumulative.csv", row.names=FALSE)




##### Plot CO2 emission data
# reorder levels of plastic
cumulative_data$Plastic <- as.factor(cumulative_data$Plastic)
cumulative_data$Plastic <- factor(cumulative_data$Plastic, levels(cumulative_data$Plastic)[c(2,1,3,4,5)])

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


# Summarize data to calculate treatment averages
cumulative_avg <- cumulative_data %>%
  group_by(Date, Plastic, Nitrogen) %>%
  summarise(mean_CO2 = mean(cumulative_CO2, na.rm = TRUE),
            sd_CO2 = sd(cumulative_CO2, na.rm = TRUE),
            mean_CO2_plastic = mean(cumulative_CO2_plastic, na.rm = TRUE),
            sd_CO2_plastic = sd(cumulative_CO2_plastic, na.rm = TRUE),
            mean_CO2_native = mean(cumulative_CO2_native, na.rm = TRUE),
            sd_CO2_native = sd(cumulative_CO2_native, na.rm = TRUE), .groups = "drop")
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


# Bar plot for cumulative CO2 emissions by plastic type and nitrogen level with sd
p <- ggplot(cumulative_avg[cumulative_avg$Date=="2023-09-14",], aes(x = Plastic, y = mean_CO2, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_CO2 - sd_CO2,ymax = mean_CO2 + sd_CO2),
    position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values=colnit) +
  labs(x = "Plastic", y = bquote("Cumulative CO" [2] * " Emissions (" * mu * "g C g"^-1 * "dry soil)")) +
  theme_bw()
p

ggsave(p, file="Figures/CO2-cumulative-bars-average-sd.png",width = 6, height = 4, dpi = 300)

# Bar plot for plastic- vs. native-derived CO2 emissions by plastic type and nitrogen level with sd
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

cumulative_avg_last <- cumulative_avg_last %>%
  mutate(Subcategory = recode(Subcategory, 
                       "mean_CO2_plastic" = "Plastic", 
                       "mean_CO2_native" = "Native"), )  # Rename subcategories
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
  geom_bar(stat = "identity", position = "stack", color="black") +
  facet_grid(.~Plastic) +
  scale_fill_manual(values=colplas) +
  labs(x = "Plastic", y = bquote("Cumulative CO" [2] * " Emissions (" * mu * "g C g"^-1 * "dry soil)")) +
  theme_bw() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),        # Add error bars
                #position = position_stack(vjust = 0.5),  # Position error bars on stacked bars
                width = 0.1) 
  
p

ggsave(p, file="Figures/CO2-cumulative-bars-average-sd-stacked.png",width = 6, height = 4, dpi = 300)
 

write.csv(cumulative_avg_last, "Processed-data/CO2_cumulative_average_final.csv")

