#####################################################
###AREI - Artificial reef efficienty index 
#####################################################
# 01 Load libraries--------
library(tidyverse)
library(forcats)

# 02 Read biomass data--------
arei<-read.table("arei.txt",dec=".",sep="\t",header=T, strip.white=T)
summary(arei)
data<-as.data.frame(arei)
data

# 03 Set a number of 'empty bar' to add at the end of each group--------
empty_bar <- 2
nObsType=nlevels(as.factor(data$observation))
data$id <- rep(seq(1, nrow(data)/nObsType) , each=nObsType)
data

# 04 Get the name and the y position of each label--------
label_data= data %>% group_by(id, fac) %>% summarize(tot=sum(value))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# 05 Prepare a data frame for base lines--------
base_data=data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# 06 Prepare a data frame for grid (scales)--------
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

# 07 Basic plot--------
p = ggplot(data) +      
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5)+ 
  scale_fill_manual(values = c('#0073C2FF','#E69F00'), breaks = c("AR","CA"), 
                    labels = c("Artificial reef", "Control area"))+
  
# 08 Add val=150/100/50/0 lines--------
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
# 09 Add text showing the value of each 100/75/50/25 lines--------
  annotate("text", x = rep(max(data$id),4), y = c(0, 50, 100, 150), 
           label = c("0", "50", "100", "150") , color="grey", 
           size=3 , angle=0, fontface="bold", hjust=0.5) +  
  ylim(-80,220)+
  #ylim(-100,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position=c(0.3,0.92),
    legend.justification=c(1,1),
    legend.title = element_blank(),
    #legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar()+
  
# 10 Add labels on top of each bar--------
  geom_text(data=label_data, aes(x=id, y=tot+2, label=fac, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )+
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.5 , inherit.aes = FALSE)  +
  geom_text(data=base_data, aes(x = title, y = -14, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=3.0, fontface="bold", inherit.aes = FALSE)
p

# 11 Save plot
ggsave(last_plot(),
       filename = 'my_plot.tiff',
       device = 'tiff',
       compression = 'lzw',
       dpi = 300,
       units = 'cm',
       width = 12,
       heigh = 12)
