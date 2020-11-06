
library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(sf)
library(plotly)
library(reactable)

theme_set(theme_light())

tuesdata <- tidytuesdayR::tt_load(2020, week = 43)

raw_beer <- tuesdata$beer_awards
raw_beer %>% glimpse()

raw_beer %>% count(beer_name,sort = FALSE)
raw_beer %>% count(category) %>% arrange(-n)
raw_beer %>% View()

prepped_beer <- raw_beer %>% 
  mutate(medal=medal %>% as_factor()) 
levels(prepped_beer$medal)



top_10_brewery <- function(data=prepped_beer){
  
  data%>% 
    # Shape the data
    dplyr::count(brewery,sort = TRUE,name="tot_awards") %>%
    dplyr::slice_max(tot_awards,n=10) %>% 
    dplyr::left_join(prepped_beer, by = "brewery")%>% 
    dplyr::count(brewery,medal,tot_awards,name="n_awards") %>% 
    dplyr::mutate(brewery=brewery %>% forcats::fct_reorder(tot_awards)) %>% 
    
    #Visualize the data
    ggplot2::ggplot(aes(n_awards,brewery))+
    ggplot2::geom_col(aes(fill=medal))
}
  


Beers_US_state <- function(data=prepped_beer){
  
  # Prep the total medal
  tot_medal_state <- data %>% 
    dplyr::group_by(state) %>% 
    dplyr::summarise(tot_medal=n()) %>% 
    dplyr::ungroup() 
  
  # Prep labels - Number of medal by category
  map_tbl <- prepped_beer %>% 
    dplyr::count(state,medal) %>% 
    tidyr::pivot_wider(names_from=medal,
                       values_from=n,
                       values_fill=0) %>% 
    dplyr::mutate(label_map=str_glue("Gold : {Gold}
                            Silver: {Silver}
                            Bronze: {Bronze}")) %>% 
    dplyr::left_join(tot_medal_state, by = "state")
  
  # Map it all
  map_tbl %>% 
    plotly::plot_geo(locationmode="USA-states") %>% 
    plotly::add_trace(z=~tot_medal,
                      locations=~state,
                      color=~tot_medal,
                      text=~label_map,
                      colors="Blues") %>% 
    plotly::layout(
      geo=list(
        scope="usa",
        projection = list(type="albers usa")
      )
    )
  
}

Beers_table <- function(data=prepped_beer){
  
  data %>% 
  mutate(location=str_glue("{state} - {city}"),
         Gold=if_else(medal=="Gold",1,0),
         Silver=if_else(medal=="Silver",1,0),
         Bronze=if_else(medal=="Bronze",1,0),
         Total_medal=Gold+Silver+Bronze) %>% 
  select(location, brewery,beer_name,category,year,Gold,Silver,Bronze) %>% 
  reactable(groupBy = c("location","brewery")) 
}

