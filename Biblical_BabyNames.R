library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)
library(geomtextpath)

font_add_google(name = "League Gothic", family = "League Gothic")
font_add_google(name = "Nanum Gothic Coding", family = "Nanum Gothic Coding")

showtext_auto()

tidytuesdayR::last_tuesday()

# top100 <- cran_top_downloads(count = 100)
# write_csv(top100, "top100.csv")

top100 <- read_csv("top100.csv")

tt_data <- tt_load("2022-03-22")
bb_name <- tt_data$babynames


df <- read_csv("person.csv") %>% 
  distinct(person_name) %>% 
  inner_join(bb_name, by=c('person_name'='name')) %>% 
  mutate(sex=case_when(sex=='F'~"Female",
                       sex=='M'~'Male'),
         name_sex=paste(person_name, sex, sep = " - "))




filt_list <- df %>% 
  group_by(person_name, sex) %>% 
  summarise(n=sum(n)) %>% 
  arrange(sex, -n) %>% 
  group_by(sex) %>% 
  slice(1:5) %>% 
  mutate(name_sex2=paste(person_name, sex, sep = " - "))%>% 
  select(name_sex2) %>% 
  pull()

df %>% 
  filter(name_sex %in% filt_list) %>% 
  ggplot()+
  # geom_point(aes(x=year, y=n, color=person_name), alpha=0.1)+
  geom_textline(aes(x=year, y=n, color=person_name,label=person_name),
                vjust = -1, straight = TRUE,show.legend = F)+
  facet_wrap(~sex, scales = "free_y", nrow = 2)+
  theme_minimal(base_size = 14)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none",
        plot.title = element_text(family = "League Gothic", colour = 'Black', 
                                  lineheight = 1.2, size = 22),
        plot.subtitle = element_text(lineheight = 1.1, size = 12, face = "italic"),
        strip.text = element_text(family = "Nanum Gothic Coding",size = 12),
        axis.title.y = element_text(family = "Nanum Gothic Coding",size = 12),
        axis.title.x = element_text(family = "Nanum Gothic Coding",size = 12),
        plot.caption = element_text(family = "Nanum Gothic Coding", colour = '#8d99ae', size = 10)
  )+
  labs(x='Year', y='Number of new-borns',
       title="The rise and fall of Biblical names" ,
       subtitle = "Top 5 biblical names between 1870-2018, top 5 for each gender",
       caption = "Data: #TidyTuesday 2022-03-22 \n Data on biblical name taken from: https://data.world/bradys/bibledata-person/activity
license CC BY-NC-SA")
    
  
  
  


