NB. int, int
3 % 4
4 % 3
NB. int, [int]
3 % 1 2 3 4 5 6 7 8 9 10
NB. [int], int
_1 0 1 2 3 4 5 6 7 8 9 10 % 7
NB. [int], [int]
 _1 382 _4 329 99 8 _3 _5 % 293 _32 444 _22 _1 43 _2 4129

NB. double, int
3.1415 % 4
NB. int, double
4 % 3.1415
NB. double, [int]
3.1415 % 1 2 3 4 5 6 7 8 9 10
NB. [int], [double]
1 2 3 4 5 6 7 8 9 10 % 3.1415
NB. int, [double]
8 % _4.0 13.3 _91.202 3293.55555
NB. [double], int
_4.0 13.3 _91.202 3293.55555 % 8
NB. [int], [double]
9 8 7 6 5 4 3 2 1 _1 _2 % 12.33 _219. _4.0 _99.81 12.4 4.3 9.00001 5.5 6. _81.33 _8.
NB. [double], [int]
12.33 _219. _4.0 _99.81 12.4 4.3 9.00001 5.5 6. _81.33 _8. % 9 8 7 6 5 4 3 2 1 _1 _2

NB. double, double
7.23 % 41.32
39.24 % _12.23
_139. % _943.8
NB. [double], [double]
_1 382 _4 329 99 8 _3 _5 % 293 _32 444 _22 _1 43 _2 4129
293 _32 444 _22 _1 43 _2 4129  % _1 382 _4 329 99 8 _3 _5

x =: _8.2 _2.3 1 2.3 4 5.9 8 10.000001
NB. int, [double,int]
5 % x
_8 % x
0 % x
NB. [double,int], int
x % 5
x % _8
NB. double, [double,int]
8.324 % x
_9.123 % x
NB. [double,int], double
x % 8.324
x % _9.123
NB. [double,int] [double,int]
y =: 6.3 _8 _9. 12 2 49 _54.23 8.999912
x % y
y % x
