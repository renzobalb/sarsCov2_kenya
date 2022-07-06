#!/usr/bin/env Rscript

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(stringr)

dat <- read_excel("data/220705_Working_Metadata.xlsx")

sample <- read_csv("data/ct_values.csv", col_names = TRUE) %>%
  mutate(name=Lineage) %>%
  rename(L=Lineage)

sample$Lineage <- str_replace(sample$name," \\s*\\([^\\)]+\\)", "")
sample$Lineage <- str_remove_all(sample$Lineage, " ")

#Plot CT Values
dat %>%
  select(Lineage,`CT Value`) %>%
  drop_na() %>%
  group_by(Lineage) %>%
  summarise(CtValue = mean(`CT Value`, na.rm =T),
            sd = sd(`CT Value`, na.rm = T),
            count=n()) %>%
  left_join(.,sample) %>%
  mutate(name=ifelse(Lineage=="B.1", "B.1 (n = 11)", name )) %>%
  ggplot(aes(x=name,y=CtValue, fill=name)) +
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=CtValue-sd, ymax=CtValue+sd), width=.2) +
  theme_bw()+
  theme(strip.text = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = "none",
        axis.title = element_text(size=20),
        axis.text.x = element_text(size=20, angle=30, hjust=1),
        axis.text.y = element_text(size=20),
        plot.margin = margin(1,1,1.5,1.2, "cm"))+
  xlab("Lineages")+
  ylab("Ct value")+
scale_fill_brewer(palette ="Paired")
