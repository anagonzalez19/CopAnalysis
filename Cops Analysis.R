library(ggrepel)
library(dplyr)
library(sf)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(viridis)
library(scales)
library(lubridate)
library(sp)

# loading in base dfs

df_cops <- read_csv('/Users/anagonzalez/Documents/Cop Analysis/cop_analysis_data.csv')
df_acs <- read_csv('/Users/anagonzalez/Documents/Cop Analysis/acs_community_chi.csv')
df_complaints <- read_csv('/Users/anagonzalez/Documents/Cop Analysis/Complaint Specifics.csv')

url <- 'https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON'
tmp_filepath <- paste0(tempdir(), '/', basename(url))
download.file(url = paste0(url), destfile = tmp_filepath)
sf_areas <- sf::st_read(tmp_filepath) %>% st_as_sf()

# expand and format race and age data

df_cops_sum <- df_cops %>%
  select(community,race_coalesced,race_subethnicity,age_coalesced,cpd_role,
         number_of_cops,total_annual_salary,predictwise_racial_resentment_score,predictwise_guns_score) %>%
  mutate(total = 'Total') %>%
  mutate(race_subethnicity = case_when(is.na(race_subethnicity) & race_coalesced == 'White' ~ 'White', 
                                       is.na(race_subethnicity) & race_coalesced == 'Black' ~ 'African American', 
                                       is.na(race_subethnicity) & race_coalesced == 'Asian' ~ 'Other Asian', 
                                       is.na(race_subethnicity) & race_coalesced == 'Latino' ~ 'Other Latino', 
                                       is.na(race_subethnicity) & race_coalesced == 'Other' ~ 'Other', 
                                       TRUE ~ as.character(str_to_title(gsub("_", " ",race_subethnicity)))),
         age_coalesced = case_when(is.na(age_coalesced) ~ '40-49',
                                   age_coalesced <= 29 ~ '18-29',
                                   age_coalesced >= 30 & age_coalesced <= 39  ~ '30-39',
                                   age_coalesced >= 40 & age_coalesced <= 49  ~ '40-49',
                                   age_coalesced >= 50 & age_coalesced <= 59  ~ '50-59',
                                   age_coalesced >= 60 ~ '60+',
                                   TRUE ~ as.character(mean(age_coalesced, na.rm=TRUE)))) %>%
  melt(id.vars = c('community','number_of_cops','total_annual_salary','predictwise_racial_resentment_score','predictwise_guns_score')) %>%
  group_by(community, variable, value) %>% 
  summarize_at(vars(number_of_cops,total_annual_salary,predictwise_racial_resentment_score,predictwise_guns_score), list(sum), na.rm=TRUE)  %>%
  ungroup() %>%
  mutate(predictwise_racial_resentment_score = predictwise_racial_resentment_score /number_of_cops,
         predictwise_guns_score =  predictwise_guns_score/number_of_cops) %>%
  group_by(community, variable) %>%
  mutate(number_of_cops_agg = sum(number_of_cops),
         total_annual_salary_agg =  sum(total_annual_salary)) %>%
  ungroup()

# clean up base cop data

df_cops_wide <- df_cops_sum %>% 
  filter(variable %in% c('total','race_coalesced', 'age_coalesced')) %>%
  select(community, variable, value, number_of_cops, total_annual_salary) %>%
  pivot_wider(id_cols = community,
              names_from = c(variable,value), 
              values_from = c(number_of_cops, total_annual_salary), 
              values_fill=list(number_of_cops=0,total_annual_salary=0)) %>%
  rename_all(tolower) %>%
  mutate(community_label = str_to_title(community))

# clean complaint data

df_complaints <- df_complaints %>%
  rename_all(tolower) %>%
  select(incidentdate, latitude, longitude, officefirst, officerlast) %>%
  filter(incidentdate >= as.Date("2010-01-01") & incidentdate <= as.Date("2018-06-26"))
df_sf = st_as_sf(df_complaints %>% filter(!is.na(longitude) | !is.na(latitude)), coords = c("longitude", "latitude"),
                 crs = 4326, agr = "constant")

# create base analysis data

df_analysis <- left_join(df_cops_wide, df_acs, by = c('community'='community'))
df_analysis <- left_join(sf_areas, df_analysis, by = c('community'='community')) 

df_analysis <- df_analysis %>%
  st_transform(crs = st_crs(4326)) %>% 
  st_as_sf()

# create long analysis data

df_analysis_long <- left_join(df_cops_sum, df_acs, by = c('community'='community'))
df_analysis_long <- left_join(sf_areas, df_analysis_long, by = c('community'='community')) 

df_analysis_long <- df_analysis_long %>%
  st_transform(crs = st_crs(4326)) %>% 
  st_as_sf() %>%
  mutate(salary_per_cop = total_annual_salary / number_of_cops)

#join complaint data with geometry

df_joined <-  st_join(df_sf, sf_areas, left= TRUE) %>%
  na.omit(df_joined)

df_joined  <- df_joined %>% 
  mutate(number_of_incidents = 1) %>%
  st_set_geometry(NULL)%>% 
  as.data.frame() %>% 
  group_by(community) %>%
  summarise(number_of_incidents = sum(number_of_incidents, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(share_of_incidents = number_of_incidents/6284)

df_done <- left_join(df_joined, sf_areas, by = c('community'='community')) %>%
  select(community, number_of_incidents, share_of_incidents)
df_analysis <- left_join(df_analysis, df_done, by = c('community'='community'))

# calculate additional data points for analysis

df_analysis <- df_analysis %>%
  mutate(cops_per_capita = number_of_cops_total_total / (tot_population_acs_14_18/1000) ,
         cops_income_per_capital = total_annual_salary_total_total / (tot_population_acs_14_18 / 1000)) %>%
  mutate(share_of_cops = number_of_cops_total_total / 13851,
         share_of_pop = tot_population_acs_14_18 / sum(tot_population_acs_14_18),
         ratio_cop_share_to_pop_share = share_of_cops /  share_of_pop) %>%
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>%
  mutate(incidents_to_pop_ratio = share_of_incidents / share_of_pop) %>%
  mutate(Incidents_to_cops_ratio = share_of_incidents / share_of_cops) %>%
  mutate(share_of_pop_black = nh_blk_alone_acs_14_18 / tot_population_acs_14_18) %>%
  mutate(share_of_pop_hispanic = hispanic_acs_14_18 / tot_population_acs_14_18) %>%
  mutate(share_of_black_or_hisp_pop = share_of_pop_black + share_of_pop_hispanic)

# pull out geometry [not sure if this is effective to join back in later]

df_analysis_geo <- df_analysis %>%
  select (community, geometry, shape_area, shape_len, area_num_1, area_numbe)

# balance age data

df_cops_age <- df_analysis_long %>%
  filter(variable == 'age_coalesced')

df_cops_age_b <- data.frame(community = rep(unique(df_cops_age$community), length(unique(df_cops_age$value))),
                            value = rep(unique(df_cops_age$value), each = length(unique(df_cops_age$community))))
df_cops_age_balanced <- left_join(df_cops_age_b, df_cops_age) %>%
  select (community, value, number_of_cops, total_annual_salary)
df_cops_age_balanced[is.na(df_cops_age_balanced)] = 0
df_cops_age_balanced <- left_join(df_cops_age_balanced, df_analysis_geo)

# balance race data

df_cops_race <- df_analysis_long %>%
  filter(variable == 'race_coalesced')
#names(df_analysis_long)

df_cops_race_b <- data.frame(community = rep(unique(df_cops_race$community), length(unique(df_cops_race$value))),
                   value = rep(unique(df_cops_race$value), each = length(unique(df_cops_race$community))))
df_cops_race_balanced <- left_join(df_cops_race_b, df_cops_race) %>%
  select (community, value, number_of_cops, total_annual_salary)
df_cops_race_balanced[is.na(df_cops_race_balanced)] = 0
df_cops_race_balanced <- left_join(df_cops_race_balanced, df_analysis_geo)

# create aesthetics

theme_plot <- theme(axis.line = element_blank(),
                    axis.text = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    panel.spacing.x=unit(0, "lines"),
                    panel.spacing.y=unit(0, "lines"),
                    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
                    panel.grid.minor = element_blank(),
                    plot.background = element_rect(fill = "#f5f5f2", color = NA),
                    plot.title = element_text(face = 'bold',size=10),
                    plot.subtitle=element_text(size=9),
                    plot.caption=element_text(size=9, hjust = 0, face = 'italic'),
                    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
                    legend.background = element_rect(fill = "#f5f5f2", color = NA),
                    panel.border = element_blank())  

# cop distribution by neighborhood
(p <- ggplot(df_analysis) +
    geom_sf( aes(fill = cops_per_capita), color = 'black', size = .1) +
    #geom_sf_text(aes(label =str_wrap( community_label, width = 4)), size = 2, check_overlap = TRUE) +
    geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("", option = "magma", direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Chicago Community Areas",
         subtitle = "Cops per 1,000 people",
         caption = 'City of Chicago Office of Budget and Management') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot)

# White cop neighborhoods have several times more share of cops than share of population
(p <- ggplot(df_analysis) +
    geom_sf( aes(fill = ratio_cop_share_to_pop_share), color = 'black' , size = .1) +
    #geom_sf_text(aes(label =str_wrap( community_label, width = 4)), size = 2, check_overlap = TRUE) +
    geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("Times\nmore cops", option = "viridis", breaks = c(1,2,3,4,5,6,7), direction = -1) +
    labs(title = str_wrap("White cop neighborhoods have 4 to 6 times more cops than other neighborhoods", width = 75),
         subtitle = "Proportion of Cops in Community Area to General Population",
         caption = 'City of Chicago Office of Budget and Management') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot)

# White cop neighborhoods recieve millions each year
(p <- ggplot(df_analysis) +
    geom_sf( aes(fill =total_annual_salary_total_total/1000000), color = 'black' , size = .1) +
    geom_sf_text(aes(label =str_wrap( community_label, width = 4)), size = 2, check_overlap = TRUE) +
    #geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("Millions ($)", option = "magma", labels = comma, direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "White cop neighborhoods recieve millions each year",
         subtitle = "City Spending on Cop Salaries (millions)",
         caption = 'City of Chicago Office of Budget and Management') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot)

# White cop neighborhoods make more on average
(p <- ggplot(df_analysis) +
    geom_sf( aes(fill =total_annual_salary_total_total/number_of_cops_total_total), color =  'black', size = .1) +
    #geom_sf_text(aes(label =str_wrap( community_label, width = 4)), size = 2.2, check_overlap = TRUE) +
    geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("Average\nIncome ($)", option = "magma",  labels = comma, direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "White cop neighborhoods make more on average",
         subtitle = "Average Cop Income",
         caption = 'City of Chicago Office of Budget and Management') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot)

# cop distribution by share
(p <- ggplot(df_analysis) +
    geom_sf( aes(fill = share_of_cops), color = 'black', size = .1) +
    #geom_sf_text(aes(label =str_wrap( community_label, width = 4)), size = 2, check_overlap = TRUE) +
    geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("", option = "magma", labels =percent, direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Chicago Community Areas",
         subtitle = "Share of all Cops",
         caption = 'City of Chicago Office of Budget and Management') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot)

# incidents to population ratio
(p <- ggplot(df_analysis) +
    geom_sf( aes(fill = incidents_to_pop_ratio), color = 'black' , size = .1) +
    #geom_sf_text(aes(x = lon, y = lat, label =str_wrap( community, width = 4)), size = 2, check_overlap = TRUE) +
    geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("Ratio of Incidents Share\nto Population Share", option = "magma", labels = comma, direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Distribution of Incidents Compared to Population Distribution",
         subtitle = "Recorded between 2010-2018",
         caption = 'Data Courtesy of the Invisible Institute') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot)

# incidents to cops ratio, controlled for population
(p <- ggplot(df_analysis) +
    geom_sf( aes(fill = Incidents_to_cops_ratio), color = 'black' , size = .1) +
    #geom_sf_text(aes(x = lon, y = lat, label =str_wrap( community, width = 4)), size = 2, check_overlap = TRUE) +
    geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("Ratio of Incidents Share\nto Cop Share", option = "magma", labels = comma, direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Distribution of Incidents Compared to Cop Distribution",
         subtitle = "Recorded between 2010-2018",
         caption = 'Data Courtesy of the Invisible Institute') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot)

# show race map of blacks
(p <- ggplot(df_analysis) +
    geom_sf( aes(fill = share_of_pop_black), color = 'black' , size = .1) +
    #geom_sf_text(aes(x = lon, y = lat, label =str_wrap( community, width = 4)), size = 2, check_overlap = TRUE) +
    #geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("Ratio of Black Population\nto Total Population", option = "magma", labels = comma, direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Share of Population that is Black by Neighborhood",
         subtitle = "2014-2018",
         caption = 'Data Courtesy of the American Census Bureau') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot)

# show race map of hispanics
(p <- ggplot(df_analysis) +
    geom_sf( aes(fill = share_of_pop_hispanic), color = 'black' , size = .1) +
    #geom_sf_text(aes(x = lon, y = lat, label =str_wrap( community, width = 4)), size = 2, check_overlap = TRUE) +
    geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("Ratio of Hispanic Population\nto Total Population", option = "magma", labels = comma, direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Share of Population that is Hispanic by Neighborhood",
         subtitle = "2014-2018",
         caption = 'Data Courtesy of the American Census Bureau') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot)

# show race map of blacks and hispanics
(p <- ggplot(df_analysis) +
    geom_sf( aes(fill = share_of_black_or_hisp_pop), color = 'black' , size = .1) +
    #geom_sf_text(aes(x = lon, y = lat, label =str_wrap( community, width = 4)), size = 2, check_overlap = TRUE) +
    geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("Ratio of Black or Hispanic\nPopulation to Total Population", option = "magma", labels = comma, direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Share of Population that is Black or Hispanic by Neighborhood",
         subtitle = "2014-2018",
         caption = 'Data Courtesy of the American Census Bureau') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot)

# create ranked bar charts by neighborhoods
# show that these neighrboohds are differnet more like suburbs (singled detached, owner occupied)

# show where use of force complaint allegations take place
(p <- ggplot(df_analysis) +
    geom_sf( aes(fill = number_of_incidents), color = 'black' , size = .1) +
    #geom_sf_text(aes(x = lon, y = lat, label =str_wrap( community, width = 4)), size = 2, check_overlap = TRUE) +
    geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("Number of Use of\nForce Incidents", option = "magma", labels = comma, direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Distribution of Incidents",
         subtitle = "Recorded between 2010-2018",
         caption = 'Data Courtesy of the Invisible Institute') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot)

#   ggrepel::geom_text_repel(
#     aes(label = str_wrap( community_label, width = 4), geometry = geometry),
#     stat = "sf_coordinates",
#     min.segment.length = 0, 
#     size = 2, 
#     check_overlap = TRUE
    
# faceted by age, neighborhood distribution of cops
(p <- ggplot(df_cops_age) +
    geom_sf( aes(fill = number_of_cops), color = 'black', size = .1) +
    #geom_sf_text(aes(label =str_wrap( community_label, width = 4)), size = 2, check_overlap = TRUE) +
    #geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("", option = "magma", direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Chicago Community Areas",
         subtitle = "Age Distribution of Cops",
         caption = 'City of Chicago Office of Budget and Management') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot) +
  facet_grid ( ~ value)

# faceted by race, neighborhood distribution of cops
(p <- ggplot(df_cops_race_balanced) +
    geom_sf( aes(fill = number_of_cops), color = 'black', size = .1) +
    #geom_sf_text(aes(label =str_wrap( community_label, width = 4)), size = 2, check_overlap = TRUE) +
    #geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("", option = "magma", direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Chicago Community Areas",
         subtitle = "Race Distribution of Cops",
         caption = 'City of Chicago Office of Budget and Management') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot) +
  facet_grid ( ~ value)
rlang::last_error()

# faceted by race, salary per cop in different community areas
(p <- ggplot(df_cops_race) +
    geom_sf( aes(fill = salary_per_cop), color = 'black', size = .1) +
    #geom_sf_text(aes(label =str_wrap( community_label, width = 4)), size = 2, check_overlap = TRUE) +
    #geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("", option = "magma", direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Chicago Community Areas",
         subtitle = "Salary Per Cop, Separated by Race",
         caption = 'City of Chicago Office of Budget and Management') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot) +
  facet_grid ( ~ value)

# faceted by age, salary per cop in different community areas
(p <- ggplot(df_cops_age) +
    geom_sf( aes(fill = salary_per_cop), color = 'black', size = .1) +
    #geom_sf_text(aes(label =str_wrap( community_label, width = 4)), size = 2, check_overlap = TRUE) +
    #geom_text_repel(mapping = aes(x = lon,y =lat,label = str_wrap( community_label, width = 3)), force = .01,size = 1.5,max.iter = 3000, min.segment.length = 1,point.padding = NA,segment.color = "grey50")+
    scale_fill_viridis("", option = "magma", direction = -1) + #, breaks = c(0, .25, .5, .75, 1)
    labs(title = "Chicago Community Areas",
         subtitle = "Salary Per Cop, Separated by Age",
         caption = 'City of Chicago Office of Budget and Management') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme_plot) +
  facet_grid ( ~ value)

# faceted by race, incident distribution

# race matching with incidents?
    