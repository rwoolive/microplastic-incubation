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

# data <- read_excel("C:/UTK one drive/OneDrive - University of Tennessee/Biosystems Engineering/Microplastic Project/Incubation data/All Wet Chemistry Data_final.xlsx", sheet='CumCO2')
# head(data)
# str(data)
# data$Treatment <- factor(data$Treatment, levels = c("NONE-N0", "LDPE-N0", "PBS-N0", "PLA/PHA-N0", "PLA-N0", "NONE-N1", "LDPE-N1", "PBS-N1", "PLA/PHA-N1", "PLA-N1")) # to control my order of treatments
# #data$Time <- factor(data$Time, levels = c("5D", "15D", "30D", "193D")) # to control my order of time
# data$Nitrogen <- factor(data$Nitrogen, levels = c("N0", "N1")) # to control my order of time
# dataN0 <- subset(data, Nitrogen=="N0")
# dataN1 <- subset(data, Nitrogen=="N1")


### replicate data
rs <- data <- read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", sheet='sample numbering')

### CO2 data
dat_CO2 <- read.csv("Processed-data/CO2Flux_Long_Interpolated_Cumulative.csv")
unique(dat_CO2$Tube) # Jars 121-160

# assign replicate values
dat_CO2$Rep <- rep(NA, dim(dat_CO2)[1])
for(i in 1:dim(dat_CO2)[1]){
  dat_CO2$Rep[i] <- rs$Replicate[which(rs$`Jar No`==dat_CO2$Tube[i])]
}

# factors
dat_CO2 <- dat_CO2 %>%
  mutate(Plastic = factor(Plastic,levels = c("NONE", "LDPE", "PBS", "PLA", "PLA/PHA")))%>%
  mutate(Nitrogen = factor(Nitrogen,levels = c("N0", "N1")))%>%
  mutate(Rep = factor(Rep))

# standardize cumulative, plastic-derived, and SOM-derived CO2 emissions by plastic-C added
og_c <- data <- read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", 
                           sheet='13C and CN of plastics', range="Q77:U82")
dat_CO2$original_c <- rep(NA, dim(dat_CO2)[1])
for(i in 1:dim(dat_CO2)[1]){
  dat_CO2$c_added[i] <- og_c$`mg plastic-C added per gdw soil`[which(og_c$plastic==dat_CO2$Plastic[i])]
}
dat_CO2$cumulative_CO2_std <- (dat_CO2$cumulative_CO2)/(dat_CO2$c_added*1000) # CO2-C emitted (ug g-1) / plastic-C added (ug g-1)
dat_CO2$cumulative_CO2_std[which(dat_CO2$Plastic=="NONE")] <- NA
dat_CO2$cumulative_CO2_plastic_std <- (dat_CO2$cumulative_CO2_plastic)/(dat_CO2$c_added*1000) # plastic CO2-C emitted (ug g-1) / plastic-C added (ug g-1)
dat_CO2$cumulative_CO2_plastic_std[which(dat_CO2$Plastic=="NONE")] <- NA
dat_CO2$cumulative_CO2_native_std <- (dat_CO2$cumulative_CO2_native)/(dat_CO2$c_added*1000) # native CO2-C emitted (ug g-1) / plastic-C added (ug g-1)
dat_CO2$cumulative_CO2_native_std[which(dat_CO2$Plastic=="NONE")] <- NA
dat_CO2 %>% 
  filter(day %in% c(5,15,30,193)) %>% 
  group_by(Plastic, day) %>% 
  summarise(mean = mean(cumulative_CO2_std, na.rm=T), 
            mean_p = mean(cumulative_CO2_plastic_std, na.rm=T),
            mean_n = mean(cumulative_CO2_native_std, na.rm=T))

write.csv(dat_CO2, "Processed-data/dat_CO2.csv")

# subset data by nitrogen treatment and day
dat_CO2_5 <- subset(dat_CO2, day==5)
dat_CO2_15 <- subset(dat_CO2, day==15)
dat_CO2_30 <- subset(dat_CO2, day==30)
dat_CO2_193 <- subset(dat_CO2, day==193)

dat_CO2_list <- list("dat_CO2_5"=dat_CO2_5, "dat_CO2_15"=dat_CO2_15,
                     "dat_CO2_30"=dat_CO2_30, "dat_CO2_193"=dat_CO2_193)
resp_CO2 <- c("cumulative_CO2", "daily_CO2",
              "cumulative_CO2_plastic", "daily_CO2_plastic",
              "cumulative_CO2_native", "daily_CO2_native",
              "cumulative_priming", "priming",
              "cumulative_priming_relative", "priming_relative",
              "cumulative_CO2_std", "cumulative_CO2_plastic_std", "cumulative_CO2_native_std")


### Soil data
#data <- read_excel("Raw-data/Enzyme data for avi MP incubation soils.xlsx", sheet='final enzyme data')
dat_soil <- read.csv("Processed-data/All Wet Chemistry Data_final.csv")
unique(dat_soil$Jar.No) # Jars 1-160
dat_soil$day <- as.integer(gsub(x=dat_soil$Time, pattern="D", replacement=""))
dat_soil$cacq <- dat_soil$BG + dat_soil$CB + dat_soil$XYL

# assign replicate values
dat_soil$Rep <- rep(NA, dim(dat_soil)[1])
for(i in 1:dim(dat_soil)[1]){
  dat_soil$Rep[i] <- rs$Replicate[which(rs$`Jar No`==dat_soil$Jar.No[i])]
}

# factors
dat_soil <- dat_soil %>%
  mutate(Plastic = factor(Plastic,levels = c("NONE", "LDPE", "PBS", "PLA", "PLA/PHA")))%>%
  mutate(Nitrogen = factor(Nitrogen,levels = c("N0", "N1")))%>%
  mutate(Rep = factor(Rep))

# standardize SOC by plastic-C added
og_c <- data <- read_excel("Raw-data/Microplastic_incubation_CO2_data_5-6-2023_a.xlsx", 
                           sheet='13C and CN of plastics', range="Q77:U82")
dat_soil$original_c <- rep(NA, dim(dat_soil)[1])
for(i in 1:dim(dat_soil)[1]){
  dat_soil$c_added[i] <- og_c$`mg plastic-C added per gdw soil`[which(og_c$plastic==dat_soil$Plastic[i])]
}
dat_soil$SOC_std <- (dat_soil$SOC*10)/dat_soil$c_added # SOC / plastic-C added
dat_soil$SOC_std[which(dat_soil$Plastic=="NONE")] <- NA
dat_soil %>% 
  group_by(Plastic, Time1) %>% 
  summarise(mean = mean(SOC_std, na.rm=T))

write.csv(dat_soil, "Processed-data/dat_soil.csv")

# subset data by nitrogen treatment and day
dat_soil_5 <- subset(dat_soil, day==5)
dat_soil_15 <- subset(dat_soil, day==15)
dat_soil_30 <- subset(dat_soil, day==30)
dat_soil_193 <- subset(dat_soil, day==193)

dat_soil_list <- list("dat_soil_5"=dat_soil_5, "dat_soil_15"=dat_soil_15,
                     "dat_soil_30"=dat_soil_30, "dat_soil_193"=dat_soil_193)
resp_soil <- c("SOC",  "TN", "MBC", "DOC", "cacq", "LAP", "Ammonium", "Nitrate", "pH", "SOC_std")








##### anova output dataframe ##### 
timepoints <- 4
times <- c("5", "15", "30", "193")
responses <- c(resp_CO2, resp_soil)
responsen <- length(responses)
p <- timepoints*responsen
mod.anova <- data.frame(response=rep(NA,p),
                        time=rep(NA,p),
                        shapiro.W=rep(NA,p),
                        shapiro.p=rep(NA,p),
                        Plastic=rep(NA,p), 
                        Nitrogen=rep(NA,p), 
                        Plastic_Nitrogen=rep(NA,p),
                        Plastic.sig=rep(NA,p), 
                        Nitrogen.sig=rep(NA,p), 
                        Plastic_Nitrogen.sig=rep(NA,p))

testlet_all <- data.frame(Plastic=NA, Nitrogen=NA, response=NA, SE=NA, df=NA, lower.CL=NA, upper.CL=NA, .group=NA, resp=NA, timepoint=NA)
testlet_plastic <- data.frame(Plastic=NA, response=NA, SE=NA, df=NA, lower.CL=NA, upper.CL=NA, .group=NA, resp=NA, timepoint=NA)
testlet_nitrogen <- data.frame(Nitrogen=NA, response=NA, SE=NA, df=NA, lower.CL=NA, upper.CL=NA, .group=NA, resp=NA, timepoint=NA)
sig.levs <- c(0.001, 0.01, 0.05, 0.1)
########################################
### Linear models and two-way ANOVA ###
#######################################

'%!in%' <- function(x,y)!('%in%'(x,y))


for(i in 1:responsen){ # for all responses

  for(t in 1:timepoints){ # for all responses within each timepoint
    ## Response 
    resp <- responses[i]
    
    ## Data
    if(resp %in% resp_CO2){df <- dat_CO2_list}
    if(resp %in% resp_soil){df <- dat_soil_list}
    
    mod.anova$response[t+(4*(i-1))] <- resp
    mod.anova$time[t+(4*(i-1))] <- t
    
      
    ## Model 
    mod1<-aov(formula =  as.formula(paste("(", resp, ") ~ Plastic * Nitrogen ")),
              data = df[[t]])
   
    stest <- shapiro.test(mod1$residuals)
    mod.anova$shapiro.W[t+(4*(i-1))] <- stest$statistic
    mod.anova$shapiro.p[t+(4*(i-1))] <- stest$p.value
    
    ## If residuals non-normal, model with sqrt-transformed data (cannot include priming because this interferes with positive/negative values)
    if(stest$p.value<0.05 & resp %!in% c("cumulative_priming", "priming", "cumulative_priming_relative", "priming_relative")){
      mod1<-aov(formula =  as.formula(paste("sqrt(", resp, ") ~ Plastic * Nitrogen")),
                data = df[[t]])
      stest <- shapiro.test(mod1$residuals)
      mod.anova$shapiro.W[t+(4*(i-1))] <- stest$statistic
      mod.anova$shapiro.p[t+(4*(i-1))] <- stest$p.value
    }

    ## If residuals non-normal, model with cube root-transformed data
    if(stest$p.value<0.05 & resp %in% c("cumulative_priming", "priming", "cumulative_priming_relative", "priming_relative")){
      mod1<-aov(formula =  as.formula(paste("(", resp, ")^(1/3) ~ Plastic * Nitrogen")),
                data = df[[t]])
      stest <- shapiro.test(mod1$residuals)
      mod.anova$shapiro.W[t+(4*(i-1))] <- stest$statistic
      mod.anova$shapiro.p[t+(4*(i-1))] <- stest$p.value
    }
    
    ## Export anova results
    res <- anova(mod1)
    mod.anova[t+(4*(i-1)), c("Plastic", "Nitrogen", "Plastic_Nitrogen")] <- paste0("F=", round(res$`F value`, 2), ", p=", round(res$`Pr(>F)`, 3))[1:3]
    ps <- res$`Pr(>F)`[1:3]
    ss <- rep("N.S.", 3)
      for(j in 1:3){
        if(ps[j] < 0.1){ss[j] <- "#"}
        if(ps[j] < 0.05){ss[j] <- "*"}
        if(ps[j] < 0.01){ss[j] <- "**"}
        if(ps[j] < 0.001){ss[j] <- "***"}
      }
    mod.anova[t+(4*(i-1)), c("Plastic.sig", "Nitrogen.sig", "Plastic_Nitrogen.sig")] <- ss
    
    ### tukey letters for 2-way interaction: 
    test1 <- emmeans(mod1, ~ Plastic*Nitrogen)
    testlet <- cld(test1, type = "response", Letters = "ABCDEFGHIJ", reversed = TRUE)
    testlet <- testlet[order(testlet$Plastic, testlet$Nitrogen),]
    testlet$.group <- gsub(" ", "", testlet$.group)
    testlet$resp <- rep(resp, dim(testlet)[1])
    testlet$timepoint <- rep(times[t], dim(testlet)[1])
    
    testlet_all <- rbind(testlet_all, testlet)

    ### tukey letters for Plastic effect: 
    test1 <- emmeans(mod1, ~ Plastic)
    testlet <- cld(test1, type = "response", Letters = "ABCDEFG", reversed = TRUE)
    testlet <- testlet[order(testlet$Plastic),]
    testlet$.group <- gsub(" ", "", testlet$.group)
    testlet$resp <- rep(resp, dim(testlet)[1])
    testlet$timepoint <- rep(times[t], dim(testlet)[1])

    testlet_plastic <- rbind(testlet_plastic, testlet)
    
    ### tukey letters for Nitrogen effect: 
    test1 <- emmeans(mod1, ~ Nitrogen)
    testlet <- cld(test1, type = "response", Letters = "ABCD", reversed = TRUE)
    testlet <- testlet[order(testlet$Nitrogen),]
    testlet$.group <- gsub(" ", "", testlet$.group)
    testlet$resp <- rep(resp, dim(testlet)[1])
    testlet$timepoint <- rep(times[t], dim(testlet)[1])
    
    testlet_nitrogen <- rbind(testlet_nitrogen, testlet)
    
    }
}


write.csv(testlet_all, paste0("Model-output/tukey/plastic_nitrogen.csv"))
write.csv(testlet_plastic, paste0("Model-output/tukey/plastic.csv"))
write.csv(testlet_nitrogen, paste0("Model-output/tukey/nitrogen.csv"))
write.csv(mod.anova, paste0("Model-output/anova/plastic_nitrogen.csv"))




