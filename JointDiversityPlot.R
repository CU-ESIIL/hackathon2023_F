
library(tidyverse)
library(sf)
library(vegan)

# Set working direction
setwd("/home/jovyan/data-store/hackathon2023_F")

# FOR RIM FIRE
# rs <- read.csv("./Data/RS_diversity_rim_fire.csv")
# occ <- read.csv("./Data/GBIF_DivByYear_Rim Fire.csv")
# fire_name <- "Rim Fire"

# FOR CAMERON PEAK FIRE
rs <- read.csv("./Data/RS_Shannon_diversity_Cameron_peak.csv")
occ <- read.csv("./Data/GBIF_DivByYear_Cameron Peak Fire.csv")
fire_name <- "Cameron Peak Fire"

df <- left_join(rs, occ, by="year") %>% select(year, RS_shannon_mean, shannon_div) %>% gather("source", "div", -year)

df$source[df$source == "RS_shannon_mean"] <- "Remote Sensing"
df$source[df$source == "shannon_div"] <- "Citizen Science"
df$year <- as.integer(df$year)

# Diversity index vs. number of obs 
plt1 <- ggplot(df, aes(x = year, y = div, col=source)) +
  geom_line() + 
  xlab("Number of Observations") +
  ylab("Shannon Diversity Index Value") +
  labs(col = "Data Type") + 
  theme_bw()
  
ggsave(plt1, filename = paste0("./Figures/ShannonIndexValuesThroughTime_", fire_name, ".png"))
