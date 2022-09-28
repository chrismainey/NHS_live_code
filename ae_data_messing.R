# Looking for A24 - A26 from AmbSYS, NHS England
# 
library(ragg)
library(tidyverse)
library(readr)
library(scales)

theme_set(
  theme_minimal()+
  theme(text = element_text(family="Open Sans")
        , legend.position = "bottom"
        , legend.text = element_text(size = 8))
)

ae <- read_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/09/AmbSYS-for-Aug-2022.csv")

# Replace . with NA
ae[ae == "."] <- NA

# Select organisation level, based on org codes starting 'R'
ae_sub <- 
  ae %>% 
  filter(str_starts(string = `Org Code`, pattern = 'R'))

# subset
ae_sub2 <-
  ae_sub %>% 
  mutate(dt = as.Date(paste0(Year, "-", Month, "-01"))) %>% 
  select(dt
         , `Org Code`
         , `Org Name`
         , num_range("A", 24:26))
  
# pivot for plotting
ae_piv1 <- 
  ae_sub2 %>% 
  pivot_longer(cols = starts_with("A")) %>% 
  mutate(value = as.numeric(value))


ae_piv1 %>% 
  filter(name=="A24") %>% 
  ggplot(aes(y=value, x = dt, col = `Org Name`))+
  geom_point()+
  geom_line()+
  scale_color_viridis_d()+
  scale_y_continuous(labels = comma)+
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%y")+
  theme(axis.text.x = element_text(angle = 45),
        legend.text = element_text(size=6))
  #facet_grid(col = vars(name))
