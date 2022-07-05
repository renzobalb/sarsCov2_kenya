#!/usr/bin/env Rscript

library(tidyverse)
library(readxl)
library(ggmap)
library(RColorBrewer)

dat <- read_excel("data/220705_Working_Metadata.xlsx")

df <- dat %>%
  dplyr::select(Lineage,`Clinical status`, `CT Value`, Region) %>%
  mutate(lat=ifelse(`Region`=="St. Francis", -1.2253578573970736,0), lon=ifelse(`Region`=="St. Francis", 36.91624715602447,0), 
         lat=ifelse(`Region`=="Gatundu L5", -1.0150745177991116,lat), lon=ifelse(`Region`=="Gatundu L5", 36.90616228485947,lon),
         lat=ifelse(`Region`=="Kiambbu L5",  -1.1746925043998608,lat), lon=ifelse(`Region`=="Kiambbu L5", 36.8304316459525,lon),
         lat=ifelse(`Region`=="Mbagadhi", -1.3088396386768522,lat), lon=ifelse(`Region`=="Mbagadhi", 36.803445456024846,lon),
         lat=ifelse(`Region`=="Uhai Neema", -1.2171681631941191,lat), lon=ifelse(`Region`=="Uhai Neema", 36.884195030104847,lon)
         )

positions <- df %>%
  select(lat,lon)

map <- get_map(location=c(lon=36.85793665680343,
                          lat=-1.1765372095565402), zoom=11, maptype='roadmap', color='bw')

# Bubble plot of sampling

sampleDf <- df %>%
  group_by(Region, lon, lat) %>%
  summarise(count=n()) %>%
  drop_na()

ggmap(map, extent = "device") +
  geom_jitter(data=sampleDf, aes(x=lon, y=lat, color = "blue", size = count), alpha = 0.5)+
  geom_text(data=sampleDf,aes(label = count))+
  #geom_text(data=sampleDf,aes(label = Region))+
  scale_size_continuous(range  = c(3, 40), 
                        limits = c(0, 24), 
                        breaks = c(1, 3, 5, 12, 24),
                        name="Number of samples")+
  theme(strip.text = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) + 
  guides(color = "none")

# Bubble plot of number of samples by colour and bubble size

#Prepare data
bubbleDf <- df %>%
  group_by(Lineage,Region, lon, lat) %>%
  summarise(count=n()) %>%
  drop_na() %>%
  mutate(labels=as.character(count))

#Graph per lineage, no labels
ggmap(map, extent = "device") +
  geom_jitter(data=bubbleDf, aes(x=lon, y=lat, color = Lineage, size = count), alpha = 0.5)+
  scale_size_continuous(range  = c(1, 27), 
                        limits = c(0, 24), 
                        breaks = c(1, 3, 5, 9, 13),
                        name="Number of samples")+
  scale_color_brewer(palette ="Paired")+
  facet_wrap( ~Lineage, nrow = 3)+
  theme(strip.text = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        plot.margin = margin(1,1,1.5,1.2, "cm")) + 
  guides(color = "none")


