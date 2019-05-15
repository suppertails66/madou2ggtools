mkdir -p rsrc
mkdir -p rsrc/orig
mkdir -p rsrc_raw
mkdir -p rsrc_raw/grp

make libsms && make rawdmp
make libsms && make grpdmp_gg

./grpdmp_gg madou2.gg rsrc_raw/grp/button01.png $((0x1EA00+(0x20*10))) 6 -p rsrc_raw/main.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/button02.png $((0x1EA00+(0x20*16))) 6 -p rsrc_raw/main.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/button03.png $((0x1EA00+(0x20*22))) 6 -p rsrc_raw/main.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/button04.png $((0x1EA00+(0x20*28))) 6 -p rsrc_raw/main.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/button05.png $((0x1EA00+(0x20*34))) 6 -p rsrc_raw/main.pal

./grpdmp_gg madou2.gg rsrc_raw/grp/button06.png $((0x1EA00+(0x20*40))) 2 -p rsrc_raw/main.pal

./grpdmp_gg madou2.gg rsrc_raw/grp/button07.png $((0x1EA00+(0x20*42))) 6 -p rsrc_raw/main.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/button08.png $((0x1EA00+(0x20*48))) 2 -p rsrc_raw/main.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/button09.png $((0x1EA00+(0x20*50))) 2 -p rsrc_raw/main.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/button10.png $((0x1EA00+(0x20*52))) 2 -p rsrc_raw/main.pal

./grpdmp_gg madou2.gg rsrc_raw/grp/button11.png $((0x1EA00+(0x20*54))) 6 -p rsrc_raw/main.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/button12.png $((0x1EA00+(0x20*60))) 6 -p rsrc_raw/main.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/button13.png $((0x1EA00+(0x20*66))) 6 -p rsrc_raw/main.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/button14.png $((0x1EA00+(0x20*72))) 6 -p rsrc_raw/main.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/button15.png $((0x1EA00+(0x20*78))) 4 -p rsrc_raw/main.pal

./grpdmp_gg madou2.gg rsrc_raw/grp/compass.png $((0x22020+(0x20*0))) 28 -p rsrc_raw/main_sprite_distinct.pal
./grpdmp_gg madou2.gg rsrc_raw/grp/buttons_title.png $((0x376E0+(0x20*0))) 12 -p rsrc_raw/main.pal

./tilemapdmp_gg madou2.gg 0x37AED full 20 9 rsrc_raw/title_vram.bin 0 rsrc/orig/title_logo.png -p rsrc_raw/title.pal
./tilemapdmp_gg rsrc_raw/title_subtitle_map.bin 0x0 full 8 1 rsrc_raw/title_vram.bin 0 rsrc/orig/title_subtitle.png -p rsrc_raw/title.pal

