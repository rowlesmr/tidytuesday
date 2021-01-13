install.packages("ggrepel")



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


#I want to plot the surface area of the 2D works of art as a function of time
df <- artwork %>% 
  filter(is.na(depth)) %>% select(id, accession_number, artist,title, medium,year,acquisitionYear,width,height,units) %>% 
  mutate(year = replace_na(year, "Unspecified"))

#how many types of units are there?
unique(df$units)

#mm and NA! I will ignore rows with NAs
df <- df[complete.cases(df), ]


df <- df %>% mutate(area = width * height / (1000*1000)) #sq m
df <- df %>% arrange(acquisitionYear)

df <- df %>% mutate(largest = 0)


#is the work the largest up until now?
val <- 0
for (i in 1:nrow(df)){
  if(df[i,]$area > val){
    val = df[i,]$area
    df[i,]$largest = 1
  }
}


#how about we just look at the minmax?
maxarea <- max(df$area)
minarea <- min(df$area)

#the largest pieces with time
#dfl <- df %>% filter(largest == 1)


#dfl <- dfl %>% 
#  mutate(text = glue('Artist: {artist}\nTitle: {title}\nMedium: {medium}\nCreated: {year}\nArea: {comma(area, accuracy = 0.1, suffix = " sq. m")}'),
#         x = c(1833,1837,1838,1840,1857,1872,1885,1925,   1965,1975,1990),
#         y = c( 142, 122, 102,  82,  62,  41,  22,36.2,     75, 100, 132),
#         hjust = c("left","left","left","left","left","left","left","left","right","right","right"))


#the minmax pieces
df <- df %>% mutate(minmax = case_when(area == maxarea ~ 1, area == minarea ~ -1, TRUE ~ 0))


dfmm <- df %>% filter(minmax != 0) %>% 
  mutate(text = glue('Artist: {artist}\nTitle: {title}\nMedium: {medium}\nCreated: {year}\nSize: {width} x {height} mm'),
         x = c(1950,1990),
         y = c(50, 132),
         hjust = c("right","right"))
         




df %>% #filter(area > 1) %>%
  ggplot(aes(x=acquisitionYear, y=area))+
  geom_point(colour="#385ee8", size = 3)+  #unhighlighted "#bbd1f0"    highlighted: "#385ee8"
  gghighlight(minmax != 0,unhighlighted_params = list(colour="#bbd1f0", alpha = 0.3, size = 2)) +

 
#  geom_text(data = dfmm, 
#            aes(x = ifelse(hjust == "right",x - 0.5, x + 0.5), y = y, 
#                label = text,
#                hjust = hjust), 
#            color = "white", 
#            size = 3) +  
  
   
  scale_x_continuous(breaks=c(seq(1825,2020,by=25),2014))+
  scale_y_continuous(breaks=seq(0,150,by=25), expand=expansion(mult = 0, add = c(0,1)))+
  coord_cartesian(ylim=c(0,150)) +
  scale_fill_manual(values=c("#385ee8","#bbd1f0"),
                    labels = c("Generation capacity added per year", "Cumulative generation capacity"),
                    guide=guide_legend(reverse=TRUE)) +
  
  labs(x = "Acquisition year", 
       y = "Art surface area per piece in square metres", 
       title = "Art in the Tate", 
       subtitle = "How big are the pieces?", 
       caption = "Data from the Tate Art Museum. Visualisation by Matthew Rowles | @xray_matt\nTheme by Martín Pons | @MartinPonsM",
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

ggsave("theTate.png", type = "cairo-png", dpi = 400)




















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











