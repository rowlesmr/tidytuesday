library(tidytuesdayR)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2020, week = 44)

df <- tuesdata[[1]]

#inspect data
head(df)
glimpse(df)


#convert to numeric - if there are NAs, make them == 0
df <- df %>% 
  mutate(commissioning_date = replace_na(parse_number(commissioning_date), 0))
         

#sort by province and project
df <- df %>% arrange(province_territory, project_name)

#get the turbine num per project and total turbines in a project and make them numeric
df <- df %>% 
  separate(turbine_number_in_project, c("turbine_number_in_project", "total_turbines_in_project"), sep="/") %>% 
  mutate(across(turbine_number_in_project:total_turbines_in_project, as.numeric))

#correct the NAs by inspection
df[is.na(df$turbine_rated_capacity_k_w),]
df %>% filter(project_name == "Armow Wind Project")
df <- df %>% mutate(turbine_rated_capacity_k_w = if_else(project_name == "Armow Wind Project", total_project_capacity_mw * 1000 / total_turbines_in_project, turbine_rated_capacity_k_w))


df[is.na(df$project_name),]
test <- df %>% filter(str_detect(province_territory, "^Newfoundland"))
view(test)

#by inspection
df[is.na(df$project_name),]$turbine_rated_capacity_k_w = 3000
df[is.na(df$project_name),]$project_name = "St. Lawrence"




#summarise by individual projects, total capacity, and commissioning date
dfp <- df %>% 
  group_by(project_name) %>% 
  summarise(capacity = mean(total_project_capacity_mw),
            turbines = max(turbine_number_in_project),
            latitude = mean(latitude),
            longitude= mean(longitude),
            commissioning_date = max(commissioning_date))


#summarise everything by year, 
dfy <- dfp %>% 
  group_by(commissioning_date) %>% 
  summarise(capacity = sum(capacity),
            turbines = sum(turbines)) %>% 
  arrange(commissioning_date) %>% 
  mutate(cumcap = cumsum(capacity),
         cumtur = cumsum(turbines))



#make the data longer to facilite ease of plotting
#need to rearrange to make the plotting order correct (!)
dfy_long <- dfy %>% pivot_longer(capacity:cumtur, names_to="type", values_to="count") %>% arrange(desc(type))


#plot data showing growth in installations by year
dfy_long %>% filter(type == "capacity" | type == "cumcap", commissioning_date >=1995) %>% 
  ggplot(aes(x = commissioning_date, y = count/1000, fill = type)) +
  geom_col(width = 1.8, position = position_dodge(width = 0))+
  scale_x_continuous(breaks=seq(1995,2020,by=5))+
  scale_y_continuous(breaks=seq(0,15,by=3), expand=expansion(mult = 0, add = c(0,1)))+
  scale_fill_manual(values=c("#385ee8","#bbd1f0"),
                      labels = c("Generation capacity added per year", "Cumulative generation capacity"),
                      guide=guide_legend(reverse=TRUE)) +
  
  labs(x = "Year of commissioning", 
       y = "Electrical capacity in gigawatts", 
       title = "Canadian wind turbines", 
       subtitle = "How Canada has taken up wind power", 
       caption = "Data from the Government of Canada. Visualisation by Matthew Rowles | @xray_matt.\nTheme by Martín Pons | @MartinPonsM.",
       fill = NULL) +
  theme_classic() +
  theme(plot.background = element_rect(fill = "grey15"),
        panel.background = element_rect(fill = "grey15"), 
        legend.position = c(0.12, 0.95),
        legend.background = element_rect(fill = "grey15"),
        panel.grid = element_blank(),
        text = element_text(family = "Candara", colour = "#9dc6e0"),
        axis.line = element_line(color = "#ced8f2"),
        axis.ticks = element_line(color = "#ced8f2"),
        axis.text = element_text(size = 13, colour = "#9dc6e0"), 
        axis.title = element_text(size = 13),        
        plot.title = element_text(size = 21),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(size = 11)       
        
        )
  
ggsave("wind_turbines.png", type = "cairo-png", dpi = 400)




#Let's do some animation!

library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(viridis)
library(gifski)
library(gganimate)
library(extrafont)
loadfonts(device="win")

#get the entire world into this
world <- ne_countries(scale = "medium", returnclass = "sf")


#resummarise the data to do the plotting on a map
dfm <- df %>% 
  group_by(project_name) %>% 
  summarise(capacity = mean(total_project_capacity_mw),
            turbines = max(turbine_number_in_project),
            latitude = mean(latitude),
            longitude= mean(longitude),
            commissioning_date = max(commissioning_date))%>% 
  arrange(commissioning_date) %>% 
  mutate(cumcap = cumsum(capacity),
         cumtur = cumsum(turbines))


#put the plot into an object so we can add snimation things to it separately
p <- 
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-145, -50), ylim = c(40, 75), expand = FALSE,) +
  geom_point(data=dfm, aes(y=latitude,
                           x=longitude,
                           size = turbines,
                           colour = capacity,
                           group = commissioning_date),
             alpha = 0.7) +
  scale_colour_viridis(end=0.9) +
  labs(title = "Canadian wind turbines", 
       subtitle = "Where has Canada installed wind power?", 
       caption = "Data from the Government of Canada. Visualisation by Matthew Rowles | @xray_matt.",
       colour = "Electrical generation\ncapacity in megawatts",
       size = "Number of turbines\nin installation") +
  
  theme_classic() + 
  theme(axis.line  = element_blank(),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank(),

        plot.background = element_rect(fill = "#9dc6e0"),
        panel.background = element_rect(fill = "#9dc6e0"), 
        legend.position = c(0.065, 0.2),
        legend.background = element_rect(fill = "#9dc6e0"),
        panel.grid = element_blank(),
        text = element_text(family = "Candara", colour = "grey15"),
        plot.title = element_text(size = 21),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(size = 11)       
       )+
  guides(size=guide_legend(override.aes=list(colour="white")))
        
#to view the static plot
plot(p)


#animate it
#https://stackoverflow.com/questions/56447125/gganimate-not-showing-all-frames
anim <- p +
  transition_manual(frames = commissioning_date, cumulative = TRUE)+
  labs(subtitle = "The installations to {current_frame}.")

animate(
  plot = anim, 
  nframes = 2*length(unique(dfm$commissioning_date)), 
  detail = 10,
  fps = 2, 
  end_pause = 8,
  height = 207, width = 270, units = "mm", res = 100 #quite squirrelly on the dimensions re excess whitespace
)
#saves the last animation by default
anim_save("tubine_animation.gif")











