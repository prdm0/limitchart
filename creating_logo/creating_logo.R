# remotes::install_github("GuangchuangYu/hexSticker", force = TRUE)

library(hexSticker)
library(ggplot2)
library(glue)
library(magick)
library(fs)

img <- image_read(path(getwd(), "creating_logo", "img_logo.png"))
logo <- image_ggplot(img, interpolate = TRUE)

sticker(
  logo,
  package = "limitchart",
  p_size = 20,
  s_width = 0.9,
  s_height = 0.9,
  s_x = 1,
  s_y = 0.75,
  h_fill =  "#00c1d5",
  h_color = "#2c3e50",
  p_color = "#2c3e50",
  p_family = "Aller_Rg",
  h_size = 1.4,
  white_around_sticker = F,
  filename = path(getwd(), "creating_logo", "logo.png"),
  url = "https://github.com/prdm0/limitchart/",
  u_size = 4.1,
  spotlight = F,
  l_alpha = 0.6,
  dpi = 300,
  u_color = "#0F2536"
)

# fs::file_delete(fs::path(getwd(), "logo.png"))
#
# fs::file_copy(
#   path = path(getwd(), "img", "logo", "logo.png"),
#   new_path = path(getwd(), "logo.png")
# )
