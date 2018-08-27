library(tidyverse)
conidia<- read_csv("data/conidia.csv") %>%
  select(length, width) %>%
  mutate(lw_ratio=length/width) %>%
  mutate(type="conidia")
foot_cell<- read_csv("data/foot_cell.csv")%>%
  mutate(lw_ratio=length/width)%>%
  mutate(type="foot cell")
conidiophore <- read_csv("data/conidiophore.csv")
conidia_foot<- conidia %>%
  full_join(foot_cell)


ggplot(conidia, aes(lw_ratio))+
  geom_density(size=2) +
  geom_vline(xintercept = mean(conidia$lw_ratio), 
             linetype="dashed", color="red")+
  xlab("Conidial Length to Width Ratio")+
  ylab("Density")+
  theme_minimal() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20))

ggsave("img/length_width_ratio.png", width = 8, height = 5, dpi = 300)

ggplot(conidiophore, aes(conidiophore))+
  geom_density(size=2) +
  geom_vline(xintercept = mean(conidiophore$conidiophore), 
             linetype="dashed", color="red")+
  xlab("Conidiophore Length (um)")+
  ylab("Density")+
  theme_minimal() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20))

ggsave("img/conidiophore_length.png", width = 8, height = 5, dpi = 300)

ggplot(foot_cell, aes(lw_ratio))+
  geom_density(size=2) +
  geom_vline(xintercept = mean(foot_cell$lw_ratio), 
             linetype="dashed", color="red")+
  xlab("Foot Cell Length to Width Ratio")+
  ylab("Density")+
  theme_minimal() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20))

ggsave("img/length_width_ratio_foot.png", width = 8, height = 5, dpi = 300)

ggplot(conidia_foot, aes(lw_ratio, fill=type))+
  geom_histogram(size=2) +
  geom_vline(xintercept = mean(foot_cell$lw_ratio), 
             linetype="dashed")+
  geom_vline(xintercept = mean(conidia$lw_ratio), 
             linetype="dotted")+
  xlab("Length to Width Ratio")+
  ylab("Density")+
  theme_minimal() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        legend.position = c(0.5,0.5))

ggsave("img/hist_conidia_foot.png", width = 8, height = 5, dpi = 300)
