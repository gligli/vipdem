mogrify -ordered-dither 2x1 -depth 1 -format mono *.bmp
mogrify -ordered-dither 2x1 -depth 1 -format png *.bmp

../Bits2Byte/Bits2Byte anim1.bin 0000.mono 0001.mono 0002.mono 0003.mono 0004.mono 0005.mono 0006.mono 0007.mono
../Bits2Byte/Bits2Byte anim2.bin 0008.mono 0009.mono 0010.mono 0011.mono 0012.mono 0013.mono 0014.mono 0015.mono
../Bits2Byte/Bits2Byte anim3.bin 0016.mono 0017.mono 0018.mono 0019.mono 0020.mono 0021.mono 0022.mono 0023.mono
../Bits2Byte/Bits2Byte anim4.bin 0024.mono 0025.mono 0026.mono 0027.mono 0028.mono 0029.mono 0030.mono 0031.mono
