##############################################
###ARRI - Artificial reef ranking index 
##############################################
read.table("./Circular plots/arri.txt",dec=".",sep="\t",header=T, strip.white=T)
arri<-read.table("./Circular plots/arri.txt",dec=".",sep="\t",header=T, strip.white=T)
summary(arri)

library(tidyverse)

fac<-arri[,1]
group<-arri[,2]
AR<-arri[,3]
data <- data.frame(fac, group, AR)
data

#Crie grupos vazios para separar as barras
empty_bar=2
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)))
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(group)
data$id=seq(1, nrow(data))
data

#Adiquira o nome e a posição de cada grupo
label_data <- data
number_of_bar <- nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5)/number_of_bar     
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# Prepare a data frame para base lines
base_data <- 
  data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# Prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

#Gráfico
p <- ggplot(data, aes(x=as.factor(id), y=AR, fill=group))+
  geom_bar(aes(x=as.factor(id), y=AR, fill=group), 
           stat="identity", alpha=0.5)+

  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

# Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(max(data$id),6), y = c(0, 20, 40, 60, 80, 100), 
               label = c("0","20","40","60","80","100"), color="grey", 
               size=3 , angle=0, fontface="bold", hjust=0.5)+
geom_bar(aes(x=as.factor(id), y=AR, fill=group), stat="identity", alpha=0.5)+
  ylim(-70,155)+ 
  theme_minimal()+
  #scale_fill_manual(values = c('#0073C2FF',"#A6D854","#FF9933","#CC0000"))
  scale_fill_manual(values = c('#0073C2FF',"#FF9933","#A6D854","#CC0000"))+
  #scale_fill_brewer(palette = "Spectral")
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm"))+
  coord_polar()+
  geom_text(data=label_data, aes(x=id, y=AR+2, label=fac, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.3, angle= label_data$angle, inherit.aes = FALSE)+
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -14, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=3.0, fontface="bold", inherit.aes = FALSE)
p    

ggsave(last_plot(),
       filename = 'my_plot.tiff',
       device = 'tiff',
       #compression = 'lzw',
       dpi = 300,
       units = 'cm',
       width = 10,
       heigh = 10)



