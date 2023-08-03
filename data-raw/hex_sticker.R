
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

# p <-
#   ggplot() +
#   annotate(
#     geom = "text",
#     x = 0,
#     y = 0,
#     label = "01",
#     size = 100,
#     colour = "#111111"
#   ) +
#   theme_void() +
#   theme_transparent()


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
    s_x = 1,
    s_y = 0.99,
    s_width = 0.83,
    package = "",
    h_fill = "#DDDDDD",
    h_color = "#AAAAAA",
    h_size = 2,
    p_family = "Cabin",
    dpi = 600,
    white_around_sticker = T,
    spotlight = T,
    l_width = 40
  ) +
  annotate(
    geom = "text",
    x = 1.0075,
    y = 0.475,
    label = "information",
    colour = "#222222",
    family = "Cabin",
    size = 30
  ) +
  annotate(
    geom = "text",
    x = 1,
    y = 0.48,
    label = "information",
    colour = "#DDDDDD",
    family = "Cabin",
    size = 30
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
