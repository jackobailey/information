
# Create {information} package hex sticker

# Housekeeping ------------------------------------------------------------

# Load packages

library(tidyverse)
library(hexSticker)
library(showtext)
library(here)


# Load Cabin font

font_add_google("Cabin")


# Load image of Claude Shannon

shannon <-
  here(
    "inst",
    "extdata",
    "shannon.jpg"
  )


# Create hex sticker

hex_sticker <-
  sticker(
    shannon,
    s_x = 0.95,
    s_y = 1.1,
    s_width = 0.9,
    package = "",
    h_fill = "#DDDDDD",
    h_color = "#AAAAAA",
    h_size = 2,
    p_family = "Cabin",
    dpi = 600,
    white_around_sticker = T,
    spotlight = T,
    l_width = 40,
    filename =
      here(
        "inst",
        "figures",
        "information_hex.png"
      )
  ) +
  annotate(
    geom = "text",
    x = 1.005,
    y = 0.475,
    label = "information",
    colour = "#222222",
    family = "Cabin",
    size = 38
  ) +
  annotate(
    geom = "text",
    x = 1,
    y = 0.48,
    label = "information",
    colour = "#DDDDDD",
    family = "Cabin",
    size = 38
  )


# Save hex sticker

ggsave(
  filename = "information_hex.png",
  plot = hex_sticker,
  device = "png",
  path = here("inst", "figures"),
  width = 6.64/2,
  height = 4.75/2,
  dpi = 600,
)
