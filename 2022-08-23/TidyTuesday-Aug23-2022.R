# TidyTuesday-Aug23-2022
# Author: Michelle Asbury

# load the packages
library(tidytuesdayR)
library(stringr)
library(ggplot2)

# Load the data
TTdata <- tidytuesdayR::tt_load(2022, week = 34) # this is only a summary of the data
chip <- read.csv("~/Downloads/chip_dataset.csv", header = TRUE, sep=",", check.names = FALSE) # Full dataset: https://chip-dataset.vercel.app/

# Create ReleaseYear variable
chip$ReleaseYear <- substr(chip$`Release Date`, 0, 4)
chip <- chip[chip$ReleaseYear != "NaT",]
chip$ReleaseYear <- as.numeric(chip$ReleaseYear)

# Plot
chipPlot <- ggplot(chip, aes(ReleaseYear, log(`Transistors (million)`), colour=Vendor))+
  geom_point(data= ~subset(., select=-c(Vendor)), colour="grey85")+
  geom_point(alpha=0.6)+
  geom_smooth(se=FALSE, span=1.5)+
  facet_grid(Vendor~Type)+
  labs(x="Year",
       y="Number of Transistors \n(log million)",
       title="Moore's Law", 
       subtitle="Moore's law states that the number of transistors within an integrated circuit \ndoubles approximately every 2 years. The number of transitors (in millions on \na log scale) over time are shown below by processor type and vendor.", 
       caption="Data from the CHIP dataset")+
  scale_colour_manual(values=c("#4ba6c9", "#5b8fc6", "#7b75b4", "#935891", "#9b3e63"))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title = element_text(face="bold", size=12),
        strip.text = element_text(size=9), #face="bold", 
        strip.background = element_blank(),
        plot.background = element_rect(fill="grey95"),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=10))

ggsave("TT-ChipPlot-Aug23-2022.png", width=5.83, height=8.27, units="in")