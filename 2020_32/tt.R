
library(tidytuesdayR)
library(tidyverse)
library(gghighlight)
library(ggrepel)
library(glue)
library(scales)
library(grid)




#https://stackoverflow.com/questions/30179442/plotting-minor-breaks-on-a-log-scale-with-ggplot
log10_minor_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks = 
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}

#https://stackoverflow.com/questions/50413812/log-axis-labels-in-ggplot2-show-only-necessary-digits
plain <- function(x,...) {
  format(x, ..., scientific = FALSE, drop0trailing = TRUE)
}



tuesdata <- tidytuesdayR::tt_load(2020, week = 32)

et <- tuesdata$energy_types
head(et)

et %>% distinct(country_name)

et %>% filter(type %>% str_detect("ydro"))

sum(is.na(et))

et %>% filter(is.na(country_name))
et <- et %>% mutate(country_name = if_else(is.na(country_name),"UK",country_name))

et
# "Hydro" consists of "pumped hydro power" and "other hydro"



#lets just look at 2016
df <- et  %>% filter(level != "Level 2") %>% select(country:type, `2016`) %>% rename(GWh = `2016`)

#look at percentage generation
df <- 
df %>% 
  pivot_wider(names_from=type, values_from=GWh) %>% 
  rowwise() %>% mutate(Total = sum(across(`Conventional thermal`:Other))) %>% 
  mutate(across(`Conventional thermal`:Other, ~ . / Total, .names="{.col}_frac"))%>% 
  rename_with(~paste0(., "_GWh"),`Conventional thermal`:Total)

df %>% 
  pivot_longer(cols = `Conventional thermal_GWh`:Other_frac, names_to=c("type"),names_pattern = "(.*)_.*", values_to=c("GWh", "frac"))
  
  
  mutate(across(`Conventional thermal`:Other), . / !!Total)
  

  
  
  
  
  #---
  # test
  library(reprex)
  
  
  library(tidyverse)
  
  Building <- c("A","B","C")
  RoomA <- c(5,1,7)
  RoomB <- c(20,1,10)
  RoomC <- c(7,3,5)
  
  #number of each type of room in each building
  room_types <- tibble(Building, RoomA, RoomB, RoomC)
  
  
  #how many rooms in each building?
  room_types <- 
    room_types %>% 
    rowwise() %>% mutate(Total = sum(across(RoomA:RoomC)))
  
  #I want to create three new columns containing the percentage of each room in each building
  #  some sort of mutate across??
  
  
  reprex()  
  room_types %>% mutate(RoomA_frac = RoomA / Total,
                        RoomB_frac = RoomB / Total,
                        RoomC_frac = RoomC / Total)
  
  
  room_types %>%
    mutate(across(RoomA:RoomC, ~ . / Total, .names="{.col}_frac"))
  
  
  
  
  
  library(tidyverse)
  
  d <- seq(0, 100, 0.5) 
  Fe <- runif(201, min = 0, max = 1000) 
  Ca <- runif(201, min = 0, max = 1000) 
  Zr <- runif(201, min = 0, max = 1000) 
  Ti <- runif(201, min = 0, max = 1000) 
  Al <- runif(201, min = 0, max = 1000) 
  example <- tibble(d, Fe, Ca, Zr, Ti, Al)
  Ratio_Elements <- c("Fe", "Ti", "Zr", "d")
  
  Detrital_Divisor <- "Zr"
  
  example
  
  example %>%
    mutate(across(-Detrital_Divisor, ~ . / Zr))
  
#  %>%
    select(all_of(Ratio_Elements))
  
  #---
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

#Got the lump to work!
artwork %>% 
  mutate(artist = fct_lump_n(artist,50,other_level = "Everyone else"))%>% 
  count(artist) %>%   
                  #reorder the factors by how many occurances there are, and
                  #  then make the "everyone else" category go to the end of the list
  mutate(artist = fct_reorder(artist,n) %>% fct_relevel("Everyone else", after = 0)) %>% 
ggplot(aes(x = artist,y = n))+
  geom_col(fill = "#bbd1f0")+
  scale_y_log10(minor_break = log10_minor_break(), labels = plain)+
  coord_flip(ylim=c(100,40000))+
  labs(y = "Number of pieces by artist", x= "",
       title = "Top 50 artists in the Tate collection", 
       subtitle = "Joseph Turner et al.", 
       caption = "Data from the Tate Art Museum. Visualisation by Matthew Rowles | @xray_matt") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "grey15"),
        panel.background = element_rect(fill = "grey15"), 
        legend.position = c(0.12, 0.95),
        legend.background = element_rect(fill = "grey15"),
        panel.grid = element_blank(),
        text = element_text(family = "Candara", colour = "#9dc6e0"),
        panel.grid.major.x = element_line(color = "grey20"),
        panel.grid.minor.x = element_line(color = "grey20"),
        axis.line = element_line(color = "#ced8f2"),
        axis.ticks = element_line(color = "#ced8f2"),
        axis.text.y = element_text(size = 10, colour = "#9dc6e0"),
        axis.text.x = element_text(size = 13, colour = "#9dc6e0"), 
        axis.title = element_text(size = 13),        
        plot.title = element_text(size = 21),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(size = 11)       
        ) 

ggsave("tate_artists.png", type = "cairo-png", dpi = 400)



artwork %>% filter(artist != "Turner, Joseph Mallord William") %>% 
  count(artist, medium) %>% 
  arrange(desc(n))


