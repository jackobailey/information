
# Create {information} package hex sticker

# Housekeeping ------------------------------------------------------------

# Load packages

library(tidyverse)
library(hexSticker)
library(showtext)
library(here)


# Load Cabin font

font_add_google("Cabin")


# Create ggplot object

p <-
  ggplot() +
  annotate(
    geom = "text",
    x = 0,
    y = 0,
    label = "01",
    size = 100,
    colour = "#111111"
  ) +
  theme_void() +
  theme_transparent()


# Create hex sticker

sticker(
  subplot = p,
  package = "information",
  h_fill = "#DDDDDD",
  h_color = "#AAAAAA",
  p_size = 40,
  p_color = "#111111",
  p_y = 1.3,
  s_x = 1,
  s_y = .75,
  s_width = 1.3,
  s_height = 1,
  p_family = "Cabin",
  dpi = 600,
  filename =
    here(
      "inst",
      "figures",
      "information_hex.png"
    )
)
