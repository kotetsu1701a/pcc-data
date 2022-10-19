# pcc-data
アップロードされているFortranコードは「天体の位置計算 増補版」（地人書館 1998年第4刷 長沢 工著）に掲載されている計算方法をコード化したものです。
計算方法についての解説については同書を参照してください。

The uploaded Fortran code is a coded version of the calculation method described in "Celestial Object Position Calculation, Expanded Edition" (Chijin Shokan, 1998, 4th printing, written by K. Nagasawa).<br>
Please refer to the same book for an explanation of the calculation method.<br>
Sorry, Japanese only.

## Compile
gfortran -c pcc_li.f90<br>
gfortran -c <ファイル名>.f90<br>
gfortran pcc_lib.o <ファイル名>.o

## Execute
./a.out (Linux)<br>
a.exe (Windows)
