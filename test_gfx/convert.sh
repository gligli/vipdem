mogrify -ordered-dither 1x1 -monochrome -format mono part_greets.png
mogrify -ordered-dither 1x1 -monochrome -format mono check_tex.png
mogrify -ordered-dither 1x1 -monochrome -format mono road_tex.png
mogrify -ordered-dither 1x1 -monochrome -format mono road2_tex.png
../Bits2Byte/Bits2Byte check_tex.bin check_tex.mono
../Bits2Byte/Bits2Byte road_tex.bin road_tex.mono
../Bits2Byte/Bits2Byte road2_tex.bin road2_tex.mono