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

# mergeing geom data and Freedom house Data, rescaling the meaurments such that high score= high freedom
df_country <- df |> 
    select(country, year, CL, PR) |> 
    filter(year %in% c(1995, 2020)) |> 
    pivot_wider(id_cols = country, values_from = CL, names_from = year, names_prefix = "cl_") |> 
    mutate(cl_1995_raw=cl_1995,
           cl_2020_raw=cl_2020,
           cl_1995=abs(cl_1995-8),
           cl_2020=abs(cl_2020-8),
           cl_chagne=cl_2020/cl_1995-1) |> 
    full_join(w_map, by=c("country"="country"))

# ggplot(data = df_country)+
#     geom_sf(aes(geometry=geom, color=cl_chagne, fill=cl_chagne))+
#     coord_sf()+
#     ggthemes::theme_map(base_size = 14) 
    

# Create interactive map

CivilLibertiesChange <- df_country |>
    mutate(cl_chagne=cl_chagne*100,
           rank_2020=ntile(cl_2020,n = 10)) |> 
    select(-ID) |> 
    rename('Civil_Liberties_Change_1995_2020'=cl_chagne,
           Civil_Liberties_1995=cl_1995,
           Civil_Liberties_2020=cl_2020) |> 
    st_as_sf()

mapview::mapview(CivilLibertiesChange,
                 at = seq(from=-100, to=100, by=50),
                 layer.name ='Civil Liberties Change between 1995-2020, percent', 
                 zcol='Civil_Liberties_Change_1995_2020', 
                 popup = leafpop::popupTable(CivilLibertiesChange,
                                             feature.id = F,
                                             row.numbers = F, 
                                             zcol=c('country', 
                                                    'Civil_Liberties_1995', 
                                                    'Civil_Liberties_2020', 
                                                    'Civil_Liberties_Change_1995_2020')))

# Manualy check countries that didn't merge in the first round
# df_check <- anti_join(df_country,w_map, by=c("country"="country"))
    