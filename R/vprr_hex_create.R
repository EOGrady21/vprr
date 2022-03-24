# hexSticker for vprr

library(hexSticker)
library(magick)
library(showtext)

imgpath <- "~/../Desktop/March22/CAL_hex.png"
img <- image_read(imgpath)

# font_add_google('Source Code Pro')

sticker(img,
        package="vprr",
        p_color = 'light grey',
        p_family = 'Source Code Pro',
        p_size=20,
        p_x = 1.3,
        p_y = 0.6,
        h_size = 2,
        h_fill = 'black',
        h_color = 'grey',
        spotlight = TRUE,
        l_x = 1,
        l_y = 1,
        l_width = 7,
        l_height = 7,
        s_x=1,
        s_y=1,
        s_width=1.5,
        s_height = 1.5,
        white_around_sticker = TRUE,
        filename="vprr_hex.png")
