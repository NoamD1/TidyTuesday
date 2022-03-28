library(tidyverse)
library(tidytuesdayR)
library(geofacet)
library(ggtext)
library(showtext)
font_add_google(name = "Oswald", family = "Oswald")
showtext_auto()

tt_datt <- tidytuesdayR::tt_load("2022-03-08")

iso <- read_csv("wikipedia-iso-country-codes.csv") |> 
    janitor::clean_names() |> 
    select(1:2)


df <- tt_datt$erasmus

df0 <- df |>  filter(receiving_country_code!=sending_country_code)
df1 <- df0 |> 
    group_by(receiving_country_code, academic_year) |> 
    summarise(incoming_participants=sum(participants)) |> 
    ungroup() 
    
df2 <- df0 |> 
    group_by(sending_country_code, academic_year) |> 
    summarise(outgoing_participants=sum(participants)) |> 
    ungroup() |> 
    mutate(year=parse_number(substr(x = academic_year, start = 6, stop = 9)))

df3 <- full_join(df1, df2, by=c("receiving_country_code"="sending_country_code",
                             "academic_year"="academic_year")) |> 
    left_join(iso, by=c("receiving_country_code"="alpha_2_code")) |> 
    mutate(country=if_else(receiving_country_code=='UK', 'GB', 
                           if_else(receiving_country_code=='EL', 'GR',receiving_country_code)),
           year=parse_number(substr(x = academic_year, start = 6, stop = 9)))


ggplot(data = df3)+
    geom_line(aes(x=year, y=incoming_participants, color="Incoming"))+
    geom_line(aes(x=year, y=outgoing_participants, color="Outgoing"))+
    facet_geo(~country,grid = "europe_countries_grid1", label = "name")+
    scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
    theme_minimal()+
    labs(title = "Which countries host most of Erasmus exchange students?",
         subtitle = "Number of students <span style='color:#73009e;'> Incoming to </span> and
                     <span style='color:#9e7300;'> outgoing from </span>
                    each European country. 2015-2020.",
         x="",
         y="",
         caption = "#TidyTuesday 2022-03-08")+
    scale_color_manual(name=NULL, 
                       values = c("Incoming"="#73009e", 'Outgoing'='#9e7300'))+
    theme(
          plot.title = element_text(family = "Oswald", colour = '#1369CB', lineheight = 1.2, size = 14),
          plot.subtitle = element_markdown(lineheight = 1.1, size = 12, face = "italic", ),
          strip.text = element_text(family = "Oswald",size = 10,hjust = 0),
          axis.title.y = element_text(family = "Oswald",size = 12),
          axis.title.x = element_text(family = "Oswald",size = 12),
          plot.caption = element_text(family = "Oswald", colour = '#8d99ae', size = 10)
          # legend.text = element_text(family = "Oswald",size = font_size)
    )
    

