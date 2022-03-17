# library(tidyverse)
# library(tidytuesdayR)
# library(ggtext)
# library(showtext)
# library(cranlogs)
# font_add_google(name = "Oswald", family = "Oswald")
# showtext_auto()
# 
# tidytuesdayR::last_tuesday()
# 
# top100 <- cran_top_downloads(count = 100)
# 
# tt_data <- tt_load("2022-03-15")
# df <- tt_data$cran
# 
# skimr::skim(df)
# df2 <- df %>% 
#   mutate(date_p=as.Date(date),
#            vig=rmd+rnw) %>% 
#   filter(lubridate::year(date_p)>=2010) %>% 
#   arrange(package, date_p) %>% 
#   group_by(package) %>% 
#   mutate(time_between_updats=difftime(time1 = date_p, time2 = lag(date_p), units = "days"),
#          update_number=seq(n())) %>% 
#   add_count(package) %>% 
#   mutate(major_version=str_sub(version,1,1)) %>% 
#   separate(col = version,sep = "\\.", into = c("major", 'minor','patch', 'dev'),extra = "merge", remove = F)
# 
# 
# 
# quantile(df2$n, c(0.05,0.1, 0.25,0.5,0.75,0.9,0.95))
# 
# df2 %>% 
#   separate(col = version,sep = "\\.", into = c("major", 'minor','patch', 'dev'),extra = "merge") %>% 
#   janitor::tabyl(a)
#     
# 
# df3 <- df2 %>% 
#   filter(n>10) %>% 
#   group_by(package) %>% 
#   summarise(number_of_updates=n(), 
#             avg_days_between_update=mean(time_between_updats, na.rm=T),
#             rmd=max(vig, na.rm=T)) %>% 
#   mutate(rmd_group = case_when(rmd==0~"0",
#                                rmd==1~"1",
#                                TRUE~"2+"))
# 
# df3 %>% 
#   arrange(-avg_days_between_update)
# 
# min(df2$date_p, na.rm=T)
# max(df2$date_p, na.rm=T)
# 
# ggplot(data = df3) +
#   geom_jitter(aes(x=number_of_updates, y=avg_days_between_update, color=rmd_group))+
#   # geom_smooth(aes(x=number_of_updates, y=avg_days_between_update))+
#   theme_minimal()+
#   scale_color_manual(name=NULL, 
#                      values = c("0"="#73009e", "1"="#009e73", '2+'='#9e7300'))+
#   labs(y="Average Number of Days Between updates",
#        x='Number of Updates',
#        title = "Does many and frequent updates associated with more vignettes?",
#        subtitle = "Packages with more then 10 updates between 2010-2021.<br>By number of vignettes in Rmarkdown/Sweave formats -  <span style='color:#73009e;'> 0 </span>, 
#                   <span style='color:#009e73;'> 1 </span> or <span style='color:#9e7300;'> 2+ </span> vignetts.")+
#   theme(legend.position = "none",
#         plot.title = element_text(family = "Oswald", colour = 'dodgerblue'),
#         plot.subtitle = element_markdown(lineheight = 1.1),
#         # strip.text = element_text(family = "Oswald",size = font_size_facet),
#         axis.title.y = element_text(family = "Oswald",size = 16),
#         axis.text.x = element_text(family = "Oswald",size = 12),
#       # legend.text = element_text(family = "Oswald",size = font_size)
#       )+
#   geom_smooth(aes(x=number_of_updates, y=avg_days_between_update), color='#F28482', se = F)
# 
# 
#   





# final code --------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)
library(cranlogs)
font_add_google(name = "Oswald", family = "Oswald")
showtext_auto()

tidytuesdayR::last_tuesday()

top100 <- cran_top_downloads(count = 100)

tt_data <- tt_load("2022-03-15")
df <- tt_data$cran
df2 <- df %>% 
  mutate(date_p=as.Date(date),
         vig=rmd+rnw) %>% 
  # filter(lubridate::year(date_p)>=2010) %>% 
  arrange(package, date_p) %>% 
  group_by(package) %>% 
  mutate(time_between_updats=difftime(time1 = date_p, time2 = lag(date_p), units = "days"),
         update_number=seq(n())) %>% 
  add_count(package) %>% 
  mutate(major_version=str_sub(version,1,1)) %>% 
  separate(col = version,sep = "\\.", into = c("major", 'minor','patch', 'dev'),extra = "merge", remove = F)


df3 <- top100 %>% 
  left_join(df2, by = c("package")) %>% 
  filter(package!='RColorBrewer') %>% 
  group_by(package) %>% 
  summarise(number_of_updates=n(), 
            avg_days_between_update=mean(time_between_updats, na.rm=T),
            downloads=max(count),
            rmd=max(vig, na.rm=T),
            package_age=difftime(time1 = max(date_p), time2 = min(date_p), units = "days")/365.24) %>% 
  mutate(rmd_group = case_when(rmd==0~"0",
                               rmd==1~"1",
                               TRUE~"2+"),
         updates_per_year=number_of_updates/as.numeric(package_age),
         label2=if_else((downloads>65000 & updates_per_year<10) |
                          (downloads<50000 & updates_per_year>10)  , 
                        package, 
                        ''))      
         


g <- ggplot(data = df3) +
  geom_jitter(aes(x=updates_per_year, y=downloads, color=rmd_group))+
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(name=NULL, 
                     values = c("0"="#73009e", "1"="#009e73", '2+'='#9e7300'))+
  labs(y="Number of downloads as of 2021-03-15",
       x='Number of updates per package age (in years)',
       title = "Updating frequency, number of vignettes and number of downloads - top 100 downloaded packages as of 2021-03-15",
       subtitle = "Number of downloads on 2021-03-15, number of updates per year of each package <br> (defined as the years between the first release and the last), <br>By number of vignettes -  <span style='color:#73009e;'> 0 </span>, 
                  <span style='color:#009e73;'> 1 </span> or <span style='color:#9e7300;'> 2+ </span> vignetts.",
       caption = "Data source: #TidyTuesday (2022-03-15), by Robert Flight (https://github.com/rmflight/vignette_analysis) ")+
  theme(legend.position = "none",
        plot.title = element_text(family = "Oswald", colour = 'dodgerblue', lineheight = 1.2, size = 14),
        plot.subtitle = element_markdown(lineheight = 1.1, size = 12, face = "italic"),
        # strip.text = element_text(family = "Oswald",size = font_size_facet),
        axis.title.y = element_text(family = "Oswald",size = 12),
        axis.title.x = element_text(family = "Oswald",size = 12),
        plot.caption = element_text(family = "Oswald", colour = '#8d99ae', size = 10)
        # legend.text = element_text(family = "Oswald",size = font_size)
  )+
  ggrepel::geom_label_repel(aes(x=updates_per_year, y=downloads,label=label2, color=rmd_group))

# +
#   geom_smooth(aes(x=updates_per_year, y=downloads), color='#F28482', se = F)
g

ggsave("tidytuesday_2021_03_15.png",plot = g, bg="white", dpi = 300, scale = 0.85)


ggtext::element_textbox_simple(  face = "plain",
  size = 24,
  margin = margin(t = 2, r = 0, b = 2, l = 0, unit = "line")








