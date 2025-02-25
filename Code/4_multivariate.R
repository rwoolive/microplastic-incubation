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

# colors for differnt levels 
colplas <- c("goldenrod2", "indianred1", "steelblue", "lightseagreen", "slateblue1")
colnit <- c("lightblue","salmon")
alphanit <- c(0.1,0.7)
colderive <- c("seashell2","goldenrod4")




# labels for response variables
a1 <-  expression(paste("Daily priming"))
a2 <- expression(paste("Daily plastic-derived CO"[2]))
a3 <- expression(paste("DOC"))
a4 <-expression(paste("MBC"))
a5 <- expression(paste("NO"[3]^"-", "-N"))
a6 <- expression(paste("NH"[4]^"+", "-N"))
a7 <- expression(paste("pH"))
a8 <- expression(paste("C-acquiring enzymes"))
a9 <- expression(paste("LAP activity"))
a10 <- expression(paste("SOC"))
a11 <- expression(paste("TN"))

dat_names <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)







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

write.csv(dat_CO2, "Processed-data/dat_CO2.csv")

# subset data by nitrogen treatment and day
dat_CO2_5 <- subset(dat_CO2, day==5)
dat_CO2_15 <- subset(dat_CO2, day==15)
dat_CO2_30 <- subset(dat_CO2, day==30)
dat_CO2_193 <- subset(dat_CO2, day==193)

dat_CO2_list <- list("dat_CO2_5"=dat_CO2_5, "dat_CO2_15"=dat_CO2_15,
                     "dat_CO2_30"=dat_CO2_30, "dat_CO2_193"=dat_CO2_193)
resp_CO2 <- c("daily_CO2", "daily_CO2_plastic", "daily_CO2_native", "priming")


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

write.csv(dat_soil, "Processed-data/dat_soil.csv")

# subset data by nitrogen treatment and day
dat_soil_5 <- subset(dat_soil, day==5)
dat_soil_15 <- subset(dat_soil, day==15)
dat_soil_30 <- subset(dat_soil, day==30)
dat_soil_193 <- subset(dat_soil, day==193)

dat_soil_list <- list("dat_soil_5"=dat_soil_5, "dat_soil_15"=dat_soil_15,
                      "dat_soil_30"=dat_soil_30, "dat_soil_193"=dat_soil_193)
resp_soil <- c("MBC", "DOC", "cacq", "LAP", "Ammonium", "Nitrate", "pH")








### join soil and CO2 datasets

dat_CO2_5 <- dat_CO2_5[order(dat_CO2_5$Day, dat_CO2_5$Plastic, dat_CO2_5$Nitrogen, dat_CO2_5$Rep),]
dat_CO2_15 <- dat_CO2_15[order(dat_CO2_15$Day, dat_CO2_15$Plastic, dat_CO2_15$Nitrogen, dat_CO2_15$Rep),]
dat_CO2_30 <- dat_CO2_30[order(dat_CO2_30$Day, dat_CO2_30$Plastic, dat_CO2_30$Nitrogen, dat_CO2_30$Rep),]
dat_CO2_193 <- dat_CO2_193[order(dat_CO2_193$Day, dat_CO2_193$Plastic, dat_CO2_193$Nitrogen, dat_CO2_193$Rep),]

dat_soil_5 <- dat_soil_5[order(dat_soil_5$day, dat_soil_5$Plastic, dat_soil_5$Nitrogen, dat_soil_5$Rep),]
dat_soil_15 <- dat_soil_15[order(dat_soil_15$day, dat_soil_15$Plastic, dat_soil_15$Nitrogen, dat_soil_15$Rep),]
dat_soil_30 <- dat_soil_30[order(dat_soil_30$day, dat_soil_30$Plastic, dat_soil_30$Nitrogen, dat_soil_30$Rep),]
dat_soil_193 <- dat_soil_193[order(dat_soil_193$day, dat_soil_193$Plastic, dat_soil_193$Nitrogen, dat_soil_193$Rep),]

dat <- list(dat_5 = cbind(dat_CO2_5, dat_soil_5),
            dat_15 = cbind(dat_CO2_15, dat_soil_15),
            dat_30 = cbind(dat_CO2_30, dat_soil_30),
            dat_193 = cbind(dat_CO2_193, dat_soil_193))
str(dat[[1]])

# reduce dataset to response and predictors 
resp <- "priming"
preds <- c("daily_CO2_plastic",  "DOC", "MBC", "Nitrate", "Ammonium", 
           "pH", "cacq", "LAP",  "SOC", "TN") #  "initial_plastic_c",


########################################
### correlations ###
#######################################
# doing correlations instead of Step-wise regressions, because many of the 
# soil properties are highly correlated and give VIF much greater than 5
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("psych")
library(psych)
#install.packages("ggfortify")
library(ggfortify)


'%!in%' <- function(x,y)!('%in%'(x,y))

# list of model output
mod_output <- list(mod_5 = NA,
                   mod_15 = NA,
                   mod_30 = NA,
                   mod_193 = NA)
mod_output_full <- list(mod_5 = NA,
                   mod_15 = NA,
                   mod_30 = NA,
                   mod_193 = NA)
times <- c(5,15,30,193)
mod_data <- list(dat_5 = NA,
                 dat_15 = NA,
                 dat_30 = NA,
                 dat_193 = NA)
colplas25 <- colplas[2:5]
lets <- c("A", "B", "C", "D")
days <- c("Day 5", "Day 15", "Day 30", "Day 193")
# mars <- data.frame(a=c(1,0,1,0), b=c(0,0,0,0),
#                    c=c(1,0,1,0), d=c(0,0,0,0))


for(i in 1:4){
  dat_mod <- dat[[i]]
  dat_mod <- dat_mod[-which(dat_mod$Plastic=="NONE"),] 
  #dat_mod[,c(resp,preds)] <- scale(dat_mod[,c(resp,preds)])
  dat_mod <- dat_mod[,which(!duplicated(names(dat_mod)))]
  # dat_mod[,c(resp, preds)]
  new_names <- c("Priming", "Plastic-CO2", "DOC", "MBC", "NO3", "NH4", "pH", "C-acq", "LAP", "SOC", "TN")
  dat_mod0 <- dat_mod[,c(resp,preds)] %>%
    rename_with(~new_names)
  dat_mod <- cbind(dat_mod[,c("Plastic", "Nitrogen")], dat_mod0)
  
  
  # correlation plot
  corr <- round(cor(dat_mod[,c(new_names)]), 2)
  p.mat <- cor_pmat(dat_mod[,c(new_names)])  
  pl <- ggcorrplot(corr, show.diag = F, p.mat = p.mat,lab=T, type="lower", insig="blank", lab_size = 2, lab_col="white") +
    theme_minimal()+
    labs(x="", y="", title=paste0(lets[i], ") ", days[i])) +
    scale_x_discrete(labels=dat_names[-1]) +
    scale_y_discrete(labels=dat_names) +
    theme(axis.text.x = element_text(angle = 305, hjust=0), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  
    scale_fill_gradient2(low = "red", high =  "blue", mid = "white", midpoint = 0)
  ggsave(pl, file=paste0("Figures/correlations_", days[i], ".png"),width = 4.5, height = 4.5, dpi = 300)

  # another correlation plot
  png(file=paste0("Figures/correlations_", days[i], "_2.png"),width = 5.5, height = 4.5, units="in",res = 300)
  pairs.panels(dat_mod[,c(new_names)],smooth = F,ellipses = F,lm = T,
               gap=0, las=1,
               bg = colplas[dat_mod$Plastic],
               pch= 21)
  dev.off()
  
  # PCA
  df <- dat_mod[,c(new_names)]
  pca_res <- prcomp(df, scale. = TRUE)
  mod_output[[i]] <- print(pca_res)  
  mod_output_full[[i]] <- pca_res$x
  
  g <- autoplot(pca_res, data=dat_mod, 
           colour="Plastic",
           loadings=T, loadings.color="blue",
           loadings.label=T, loadings.label.color="blue",labelsize = 10,
           frame = TRUE, frame.type = 'norm') +
    scale_color_manual(values=colplas25) +
    theme_bw() + theme(legend.position="bottom") + 
    labs(title=paste0(lets[i], ") ", days[i])) 
  g
  
  
  
  # Extract PC axes for plotting
  PCAvalues <- data.frame(Plastic = dat_mod$Plastic, Nitrogen = dat_mod$Nitrogen, pca_res$x)
  
  # Extract loadings of the variables
  PCAloadings <- data.frame(Variables = rownames(pca_res$rotation), pca_res$rotation)
  
  # Extract PC axes for plotting
  PCAaxes <- (summary(pca_res)$importance)
  
  # Plot
  g <- ggplot(PCAvalues, aes(x = PC1, y = PC2, colour = Plastic)) +
    geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*5),
                                         yend = (PC2*5)), arrow = arrow(length = unit(1/2, "picas")),
                 color = "black") +
    geom_point(size = 3, aes(shape=Nitrogen)) +
    # for labels on the positive x side
    annotate("text", x = (PCAloadings$PC1[which(PCAloadings$PC1>0)]*5)*1.1, 
             y = (PCAloadings$PC2[which(PCAloadings$PC1>0)]*5)*1.1, 
             #angle=(PCAloadings$PC1[which(PCAloadings$PC1>0)] + PCAloadings$PC2[which(PCAloadings$PC1>0)])*50, 
             hjust=0,
             label = rownames(PCAloadings)[which(PCAloadings$PC1>0)], size=3) +
    # for labels on the negative x side
    annotate("text", x = (PCAloadings$PC1[which(PCAloadings$PC1<0)]*5)*1.1, 
             y = (PCAloadings$PC2[which(PCAloadings$PC1<0)]*5)*1.1, 
             #angle=(PCAloadings$PC1[which(PCAloadings$PC1<0)] + PCAloadings$PC2[which(PCAloadings$PC1<0)])*30, 
             hjust=1,
             label = rownames(PCAloadings)[which(PCAloadings$PC1<0)], size=3) +
    scale_color_manual(values=colplas25) +
    scale_shape_manual(values=c(21, 19)) +
    theme_bw() + theme(legend.position="bottom") + 
    labs(title=paste0(lets[i], ") ", days[i]))  +
    guides(color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
    labs(x=paste0("PC1 (", round(PCAaxes[2,1]*100, 1), "%)"), 
         y=paste0("PC2 (", round(PCAaxes[2,2]*100, 1), "%)"))
  g
  ggsave(g, file=paste0("Figures/pca_", days[i], ".png"),width = 4, height = 4.5, dpi = 300)
}
 



capture.output(mod_output, file = "Model-output/pca.csv") 
#capture.output(mod_output_full, file = "Model-output/pca_full.csv") 





