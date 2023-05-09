library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(tigris)
library(plotly)
library(scales)
library(DT)
library(lubridate)

# import data
salaries = read.csv("data/levels.fyi.csv")
teleport = read.csv("data/teleport.csv") %>% rename(
  `Business Freedom` = Business.Freedom,
  `Cost of Living` =  Cost.of.Living,
  `Environmental Quality` = Environmental.Quality,
  `Internet Access` = Internet.Access,
  `Leisure & Culture` = Leisure...Culture,
  `Travel Connectivity` = Travel.Connectivity,
  `Venture Capital` = Venture.Capital
)
states = states(cb = TRUE)

# process data
dat = salaries %>%
  mutate(date = as.Date(date)) %>%
  left_join(teleport, by = c("city", "state"))

abreviate_num = function(x) {
  case_when(
    x < 1e3 ~ as.character(round(x)),
    x < 1e6 ~ paste0(as.character(round(x/1e3)), "K"),
    x < 1e9 ~ paste0(as.character(round(x/1e6)), "M"),
  )
}
