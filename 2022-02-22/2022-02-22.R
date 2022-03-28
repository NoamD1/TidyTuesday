library(tidyverse)
library(tidytuesdayR)
library(sf)
library(ggspatial)
library(maps)

tt_data <- tt_load("2022-02-22")
df <- tt_data$freedom



w_map <- st_as_sf(map("world", plot = FALSE, fill = TRUE)) |> 
    mutate(country=case_when(ID=='Bolivia' ~'Bolivia (Plurinational State of)',
                             ID=='USA'~'United States of America',
                             ID=='UK'~'United Kingdom of Great Britain and Northern Ireland',
                             ID=='Congo'~'Democratic Republic of the Congo',
                             ID=='Republic of Congo'~'Congo',
                             ID=='Brunei'~'Brunei Darussalam',
                             ID=='Cape Verde'~'Cabo Verde',
                             ID=='Barbuda'~'Antigua and Barbuda',
                             ID=='Antigua '~'Antigua and Barbuda',
                             ID=='Czech Republic'~'Czechia',
                             ID=='Iran'~'Iran (Islamic Republic of)',
                             ID=='Laos'~"Lao People's Democratic Republic",
                             ID=='Micronesia'~'Micronesia (Federated States of)',
                             ID=='Moldova'~'Republic of Moldova',
                             ID=='North Korea'~"Democratic People's Republic of Korea",
                             ID=='South Korea'~'Republic of Korea',
                             ID=='Russia'~'Russian Federation',
                             ID=='Saint Kitts'~'Saint Kitts and Nevis',
                             ID=='Nevis'~'Saint Kitts and Nevis',
                             ID=='Syria'~'Syrian Arab Republic',
                             ID=='Tanzania'~'United Republic of Tanzania',
                             ID %in% c('Trinidad', 'Tobago')~'Trinidad and Tobago',
                             ID=='Venezuela'~'Venezuela (Bolivarian Republic of)',
                             ID=='Vietnam'~'Viet Nam',
                             ID=='Ivory Coast'~'CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire',
                             TRUE~ID))

# merging geom data and Freedom house Data, re scaling the measurements such that high score= high freedom
# see: https://freedomhouse.org/reports/freedom-world/faq-freedom-world#A10

df_country <- df |> 
    select(country, year, CL, PR) |> 
    filter(year %in% c(1995, 2020)) |> 
    pivot_wider(id_cols = country, values_from = CL, names_from = year, names_prefix = "cl_") |> 
    mutate(cl_1995_raw=cl_1995,
           cl_2020_raw=cl_2020,
           cl_1995=abs(cl_1995-8),
           cl_2020=abs(cl_2020-8),
           cl_chagne=cl_2020-cl_1995) |> 
    full_join(w_map, by=c("country"="country"))

# Create interactive map

# TODO - fix country specific plots - throws error, looks like there is a problem in the creation of ggplot list-col
# very helpful resource: https://stackoverflow.com/questions/61686111/how-to-fix-distorted-interactive-popup-of-ggplot-figure-in-leaflet-map

# country_plots <- df |>
#     nest(-country) |> 
#     full_join(w_map, by=c("country"="country"))    |> 
#     select(country, data) |> 
#     mutate(ggp = purrr::map2(.x = data, .y=country,
#                             .f = ~ggplot(data = .x, aes(x=.x$year, y=.x$CL)) +
#                                 geom_line( color="steelblue")+
#                                 geom_point( color="steelblue")+
#                                 labs(x=NULL,
#                                      y="Civil Liberties",
#                                      title = glue::glue("Civil Liberties Score {.y}")) + 
#                                 theme_bw()))
#     
    

CivilLibertiesChange <- df_country |>
    # full_join(country_plots, by=c("country"="country")) |> 
    
    mutate(rank_2020=ntile(cl_2020,n = 10)) |> 
    select(-ID, -ends_with("raw")) |> 
    rename('Civil_Liberties_Change_1995_2020'=cl_chagne,
           Civil_Liberties_1995=cl_1995,
           Civil_Liberties_2020=cl_2020) |> 
    st_as_sf()

# CivilLibertiesChange |> as_tibble() |> filter(is.na(rank_2020)) |> view()
mapview::mapview(CivilLibertiesChange,
                 at = seq(from=min(df_country$cl_chagne, na.rm = T), 
                          to=max(df_country$cl_chagne, na.rm = T), 
                          by=1),
                 layer.name ='Civil Liberties Change between 1995-2020, percent', 
                 zcol='Civil_Liberties_Change_1995_2020')

# TODO - after fixing the country charts, figure out which one to put in the map
# ,
#                  leafpop::popupTable(CivilLibertiesChange,
#                                      feature.id = F,
#                                      row.numbers = F, 
#                                      zcol=c('country', 
#                                             'Civil_Liberties_1995', 
#                                             'Civil_Liberties_2020', 
#                                             'Civil_Liberties_Change_1995_2020')))

                 
                 
                 # popup = leafpop::popupGraph(CivilLibertiesChange$ggp))
                     
                     
                     
                     
                     
                     

# Manual check countries that didn't merge in the first round
# df_check <- anti_join(df_country,w_map, by=c("country"="country"))
    