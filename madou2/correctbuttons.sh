
#convert rsrc/button01.png -dither None -remap rsrc_raw/button_text_palette.png PNG32:button01.png

for num in `seq -w 1 15`; do
  convert rsrc/button${num}.png -dither None -remap rsrc_raw/button_text_palette.png PNG32:rsrc/button${num}.png
done

