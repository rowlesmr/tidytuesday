
library(tidytuesdayR)
library(tidyverse)
library(gghighlight)
library(ggrepel)
library(glue)
library(scales)
library(grid)

tuesdata <- tidytuesdayR::tt_load(2021, week = 3)

artwork <- tuesdata$artwork
head(artwork)



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


