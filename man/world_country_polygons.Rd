\name{world_country_polygons}
\alias{world_country_polygons}
\docType{data}
\title{
world_country_polygons
}
\description{
world_country_polygons shaoefile
}
\usage{data("world_country_polygons")}
\format{
  The format is:
Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
  ..@ data       :'data.frame':	246 obs. of  11 variables:
  .. ..$ FIPS     : Factor w/ 244 levels "","AC","AE","AF",..: 2 5 6 7 8 10 11 12 13 17 ...
  .. ..$ ISO2     : Factor w/ 246 levels "AD","AE","AF",..: 4 61 17 6 7 9 12 11 14 24 ...
  .. ..$ ISO3     : Factor w/ 246 levels "ABW","AFG","AGO",..: 15 64 18 6 11 3 12 10 16 25 ...
  .. ..$ UN       : int [1:246] 28 12 31 8 51 24 16 32 36 48 ...
  .. ..$ NAME     : Factor w/ 246 levels "Aaland Islands",..: 10 4 16 3 12 7 5 11 14 18 ...
  .. ..$ AREA     : int [1:246] 44 238174 8260 2740 2820 124670 20 273669 768230 71 ...
  .. ..$ POP2005  : num [1:246] 83039 32854159 8352021 3153731 3017661 ...
  .. ..$ REGION   : int [1:246] 19 2 142 150 142 2 9 19 9 142 ...
  .. ..$ SUBREGION: int [1:246] 29 15 145 39 145 17 61 5 53 145 ...
  .. ..$ LON      : num [1:246] -61.78 2.63 47.4 20.07 44.56 ...
  .. ..$ LAT      : num [1:246] 17.1 28.2 40.4 41.1 40.5 ...
  ..@ polygons   :List of 246
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 2
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.8 17.1
  .. .. .. .. .. .. ..@ area   : num 0.00957
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -61.7 -61.9 -61.8 -61.7 17 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.8 17.6
  .. .. .. .. .. .. ..@ area   : num 0.00774
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -61.7 -61.9 -61.9 -61.7 17.6 ...
  .. .. .. ..@ plotOrder: int [1:2] 1 2
  .. .. .. ..@ labpt    : num [1:2] -61.8 17.1
  .. .. .. ..@ ID       : chr "ATG"
  .. .. .. ..@ area     : num 0.0173
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 2.63 28.17
  .. .. .. .. .. .. ..@ area   : num 214
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:45, 1:2] 2.96 4.79 5.33 6.4 8.62 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 2.63 28.17
  .. .. .. ..@ ID       : chr "DZA"
  .. .. .. ..@ area     : num 214
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 5
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 47.7 40.3
  .. .. .. .. .. .. ..@ area   : num 8.49
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:22, 1:2] 46.6 47.8 48.6 49.5 50.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 45.5 39.4
  .. .. .. .. .. .. ..@ area   : num 0.506
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] 45.1 45.8 46.2 45 44.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 45.5 40.6
  .. .. .. .. .. .. ..@ area   : num 0.00169
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 45.5 45.5 45.6 45.5 40.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 45 41.1
  .. .. .. .. .. .. ..@ area   : num 0.000995
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 45 45 45 45 41 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 45.2 41
  .. .. .. .. .. .. ..@ area   : num 0.00036
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 45.2 45.2 45.2 45.2 41 ...
  .. .. .. ..@ plotOrder: int [1:5] 1 2 3 4 5
  .. .. .. ..@ labpt    : num [1:2] 47.7 40.3
  .. .. .. ..@ ID       : chr "AZE"
  .. .. .. ..@ area     : num 9
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 20.1 41.1
  .. .. .. .. .. .. ..@ area   : num 2.89
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] 19.4 19.6 19.4 19.6 20.1 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 20.1 41.1
  .. .. .. ..@ ID       : chr "ALB"
  .. .. .. ..@ area     : num 2.89
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 4
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 45 40.3
  .. .. .. .. .. .. ..@ area   : num 3.21
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] 45.2 46 45.6 46.5 46.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 45.5 40.6
  .. .. .. .. .. .. ..@ area   : num 0.00169
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 45.6 45.5 45.5 45.6 40.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 45 41.1
  .. .. .. .. .. .. ..@ area   : num 0.000995
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 45 45 45 45 41 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 45.2 41
  .. .. .. .. .. .. ..@ area   : num 0.00036
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 45.2 45.2 45.2 45.2 41 ...
  .. .. .. ..@ plotOrder: int [1:4] 1 2 3 4
  .. .. .. ..@ labpt    : num [1:2] 45 40.3
  .. .. .. ..@ ID       : chr "ARM"
  .. .. .. ..@ area     : num 3.21
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 3
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 17.6 -12.4
  .. .. .. .. .. .. ..@ area   : num 104
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:35, 1:2] 14 16.6 16.9 17.6 19.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.47 -5.06
  .. .. .. .. .. .. ..@ area   : num 0.616
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] 13.1 12.6 12.5 12.2 12 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 11.7 -16.7
  .. .. .. .. .. .. ..@ area   : num 0.00124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 11.8 11.7 11.8 11.8 -16.8 ...
  .. .. .. ..@ plotOrder: int [1:3] 1 2 3
  .. .. .. ..@ labpt    : num [1:2] 17.6 -12.4
  .. .. .. ..@ ID       : chr "AGO"
  .. .. .. ..@ area     : num 104
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 5
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -169.5 -14.3
  .. .. .. .. .. .. ..@ area   : num 0.00168
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -169.4 -169.5 -169.5 -169.4 -14.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -170.7 -14.3
  .. .. .. .. .. .. ..@ area   : num 0.000856
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -170.6 -170.8 -170.6 -170.6 -14.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -169.7 -14.2
  .. .. .. .. .. .. ..@ area   : num 0.00033
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -169.7 -169.7 -169.7 -169.7 -14.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -169.6 -14.2
  .. .. .. .. .. .. ..@ area   : num 0.000227
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -169.6 -169.6 -169.6 -169.6 -14.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -170.5 -14.3
  .. .. .. .. .. .. ..@ area   : num 0.000137
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -170.5 -170.6 -170.5 -170.5 -14.3 ...
  .. .. .. ..@ plotOrder: int [1:5] 1 2 3 4 5
  .. .. .. ..@ labpt    : num [1:2] -169.5 -14.3
  .. .. .. ..@ ID       : chr "ASM"
  .. .. .. ..@ area     : num 0.00323
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 6
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -65.2 -35.2
  .. .. .. .. .. .. ..@ area   : num 275
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:119, 1:2] -65.7 -65.2 -64.6 -64.3 -63.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -67.4 -54.4
  .. .. .. .. .. .. ..@ area   : num 3.13
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -68.3 -68.5 -67.4 -65.1 -65.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.4 -54.8
  .. .. .. .. .. .. ..@ area   : num 0.038
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -63.8 -64.7 -64.8 -63.8 -54.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -62 -39.2
  .. .. .. .. .. .. ..@ area   : num 0.0103
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -61.9 -62.1 -61.9 -61.9 -39.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -62 -39
  .. .. .. .. .. .. ..@ area   : num 0.00309
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -62 -62.1 -62 -62 -39.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -68.6 -54.9
  .. .. .. .. .. .. ..@ area   : num 0.00146
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -68.6 -68.6 -68.6 -68.6 -54.9 ...
  .. .. .. ..@ plotOrder: int [1:6] 1 2 3 4 5 6
  .. .. .. ..@ labpt    : num [1:2] -65.2 -35.2
  .. .. .. ..@ ID       : chr "ARG"
  .. .. .. ..@ area     : num 278
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 97
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 134.4 -25.6
  .. .. .. .. .. .. ..@ area   : num 688
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:209, 1:2] 143 143 144 145 145 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 147 -42
  .. .. .. .. .. .. ..@ area   : num 6.66
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] 145 147 148 148 148 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 131 -11.5
  .. .. .. .. .. .. ..@ area   : num 0.403
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 131 131 132 131 130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 137.4 -35.8
  .. .. .. .. .. .. ..@ area   : num 0.184
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 137.6 138.1 136.5 137.6 -35.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.7 -14.1
  .. .. .. .. .. .. ..@ area   : num 0.11
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 137 137 137 137 136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 153.1 -25.4
  .. .. .. .. .. .. ..@ area   : num 0.0983
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 153.1 152.9 153.3 153.1 -25.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 130.3 -11.6
  .. .. .. .. .. .. ..@ area   : num 0.0934
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 130.5 130 130.3 130.5 -11.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 148 -40
  .. .. .. .. .. .. ..@ area   : num 0.0687
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 148.2 147.9 148.3 148.2 -40.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 139.4 -16.6
  .. .. .. .. .. .. ..@ area   : num 0.0657
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 139.7 139.1 139.3 139.7 -16.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 144 -39.9
  .. .. .. .. .. .. ..@ area   : num 0.0495
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 144 144 144.1 144 -40.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 113 -25.8
  .. .. .. .. .. .. ..@ area   : num 0.0378
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 113.2 113 113 113.2 -26.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 148.3 -40.4
  .. .. .. .. .. .. ..@ area   : num 0.0295
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 148.4 148.5 148 148.4 -40.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 151.2 -23.6
  .. .. .. .. .. .. ..@ area   : num 0.0206
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 151.3 151 151.2 151.3 -23.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 147.3 -43.4
  .. .. .. .. .. .. ..@ area   : num 0.0172
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 147.4 147.1 147.3 147.4 -43.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 153.5 -27.5
  .. .. .. .. .. .. ..@ area   : num 0.0164
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 153.5 153.4 153.5 153.5 -27.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 146.2 -18.4
  .. .. .. .. .. .. ..@ area   : num 0.0161
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 146.3 146.1 146.3 146.3 -18.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 124.5 -15.4
  .. .. .. .. .. .. ..@ area   : num 0.0153
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 125 125 124 125 125 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.7 -11.2
  .. .. .. .. .. .. ..@ area   : num 0.0119
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.5 136.8 136.7 136.5 -11.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 137 -15.7
  .. .. .. .. .. .. ..@ area   : num 0.0116
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 137.1 136.9 137 137.1 -15.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 145.3 -38.5
  .. .. .. .. .. .. ..@ area   : num 0.0116
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 145.3 145.4 145.1 145.3 -38.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 132.6 -11.1
  .. .. .. .. .. .. ..@ area   : num 0.0112
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 132.6 132.5 132.6 132.6 -11.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 148.1 -42.7
  .. .. .. .. .. .. ..@ area   : num 0.0102
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 148.2 148 148 148.2 -42.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.2 -13.7
  .. .. .. .. .. .. ..@ area   : num 0.0101
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.3 136.1 136.2 136.3 -13.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 115.4 -20.8
  .. .. .. .. .. .. ..@ area   : num 0.0096
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 115.5 115.3 115.4 115.5 -20.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 147.4 -43.2
  .. .. .. .. .. .. ..@ area   : num 0.00929
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 147.4 147.3 147.4 147.4 -43.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 139.5 -17.1
  .. .. .. .. .. .. ..@ area   : num 0.00862
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 139.6 139.4 139.5 139.6 -17.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 145.4 -38.3
  .. .. .. .. .. .. ..@ area   : num 0.00755
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 145.5 145.3 145.3 145.5 -38.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.2 -10.7
  .. .. .. .. .. .. ..@ area   : num 0.00748
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 142.3 142.2 142.1 142.3 -10.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.4 -11.6
  .. .. .. .. .. .. ..@ area   : num 0.00705
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.5 136.2 136.5 136.5 -11.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 125.1 -14.6
  .. .. .. .. .. .. ..@ area   : num 0.00597
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 125.1 125.1 125.2 125.1 -14.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.3 -10.2
  .. .. .. .. .. .. ..@ area   : num 0.00544
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 142.3 142.2 142.3 142.3 -10.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 148.2 -40.5
  .. .. .. .. .. .. ..@ area   : num 0.00522
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 148.2 148.1 148.2 148.2 -40.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 132.6 -11.7
  .. .. .. .. .. .. ..@ area   : num 0.00511
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 132.6 132.5 132.6 132.6 -11.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 150.5 -22.3
  .. .. .. .. .. .. ..@ area   : num 0.0049
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 150.5 150.5 150.6 150.5 -22.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 145 -40.7
  .. .. .. .. .. .. ..@ area   : num 0.00484
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 144.9 144.9 145 144.9 -40.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 144.8 -40.5
  .. .. .. .. .. .. ..@ area   : num 0.00469
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 144.8 144.7 144.8 144.8 -40.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 158.9 -54.6
  .. .. .. .. .. .. ..@ area   : num 0.00434
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 158.9 158.8 159 158.9 -54.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.1 -10.1
  .. .. .. .. .. .. ..@ area   : num 0.0043
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 142.2 142.1 142.1 142.2 -10.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 148.9 -20.1
  .. .. .. .. .. .. ..@ area   : num 0.00404
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 148.9 148.9 149 148.9 -20.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 144.9 -40.4
  .. .. .. .. .. .. ..@ area   : num 0.00402
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 144.9 144.8 144.9 144.9 -40.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.8 -15.7
  .. .. .. .. .. .. ..@ area   : num 0.00391
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.7 136.7 136.8 136.7 -15.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.6 -15.6
  .. .. .. .. .. .. ..@ area   : num 0.00383
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.5 136.6 136.6 136.5 -15.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 149 -20.3
  .. .. .. .. .. .. ..@ area   : num 0.00374
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 149 149 149 149 -20.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 128 -15
  .. .. .. .. .. .. ..@ area   : num 0.00373
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 128 128 128 128 -15 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 128.1 -15.1
  .. .. .. .. .. .. ..@ area   : num 0.0036
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 128.2 128.1 128.1 128.2 -15.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.7 -15.7
  .. .. .. .. .. .. ..@ area   : num 0.00357
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.7 136.6 136.7 136.7 -15.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 146.6 -38.8
  .. .. .. .. .. .. ..@ area   : num 0.00352
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 146.7 146.5 146.5 146.7 -38.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 153.4 -27.2
  .. .. .. .. .. .. ..@ area   : num 0.00343
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 153.4 153.5 153.4 153.4 -27.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 149.9 -22.1
  .. .. .. .. .. .. ..@ area   : num 0.00338
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 149.9 149.9 149.9 149.9 -22.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 133.4 -11.6
  .. .. .. .. .. .. ..@ area   : num 0.00328
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 133.4 133.4 133.5 133.4 -11.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136 -12
  .. .. .. .. .. .. ..@ area   : num 0.00318
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136 136 136 136 -12 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 133.5 -11.5
  .. .. .. .. .. .. ..@ area   : num 0.00318
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 133.5 133.4 133.5 133.5 -11.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 124.4 -15.3
  .. .. .. .. .. .. ..@ area   : num 0.00305
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 124.4 124.3 124.4 124.4 -15.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 146.8 -19.1
  .. .. .. .. .. .. ..@ area   : num 0.00271
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 146.9 146.8 146.9 146.9 -19.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136 -35
  .. .. .. .. .. .. ..@ area   : num 0.00248
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.2 136.1 136.2 136.2 -35.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.6 -14.9
  .. .. .. .. .. .. ..@ area   : num 0.00239
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.6 129.6 129.5 129.6 -14.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 133.6 -32.3
  .. .. .. .. .. .. ..@ area   : num 0.00229
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 133.6 133.5 133.7 133.6 -32.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136 -11.7
  .. .. .. .. .. .. ..@ area   : num 0.00228
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136 136 136.1 136 -11.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 135.7 -14.9
  .. .. .. .. .. .. ..@ area   : num 0.00228
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 135.8 135.7 135.7 135.8 -14.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 125 -15
  .. .. .. .. .. .. ..@ area   : num 0.00218
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 125 125 125 125 -15 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 132.4 -12.1
  .. .. .. .. .. .. ..@ area   : num 0.00215
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 132.4 132.3 132.4 132.4 -12.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 134.5 -33.7
  .. .. .. .. .. .. ..@ area   : num 0.00203
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 134.5 134.5 134.5 134.5 -33.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 148.3 -42.3
  .. .. .. .. .. .. ..@ area   : num 0.00195
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 148.3 148.2 148.3 148.3 -42.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.3 -10.6
  .. .. .. .. .. .. ..@ area   : num 0.00186
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 142.3 142.2 142.3 142.3 -10.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 113.1 -24.9
  .. .. .. .. .. .. ..@ area   : num 0.0018
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 113.1 113.1 113.1 113.1 -24.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.9 -15.6
  .. .. .. .. .. .. ..@ area   : num 0.00177
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.9 136.8 136.9 136.9 -15.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126.5 -13.9
  .. .. .. .. .. .. ..@ area   : num 0.00156
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126.6 126.5 126.5 126.6 -13.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 134.9 -12.1
  .. .. .. .. .. .. ..@ area   : num 0.00152
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 134.9 134.9 134.9 134.9 -12.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 151.3 -23.8
  .. .. .. .. .. .. ..@ area   : num 0.00143
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 151.4 151.3 151.3 151.4 -23.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 135 -12
  .. .. .. .. .. .. ..@ area   : num 0.00126
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 135 135 135 135 -12 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 150.3 -21.8
  .. .. .. .. .. .. ..@ area   : num 0.00124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 150.3 150.3 150.4 150.3 -21.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 159.1 -31.6
  .. .. .. .. .. .. ..@ area   : num 0.00119
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 159.1 159.1 159.1 159.1 -31.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.6 -11.8
  .. .. .. .. .. .. ..@ area   : num 0.00116
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.6 136.5 136.6 136.6 -11.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 147.3 -39.5
  .. .. .. .. .. .. ..@ area   : num 0.00113
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 147.4 147.3 147.3 147.4 -39.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 149.7 -22.4
  .. .. .. .. .. .. ..@ area   : num 0.0011
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 149.7 149.7 149.8 149.7 -22.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 139.1 -16.8
  .. .. .. .. .. .. ..@ area   : num 0.00105
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 139.1 139.1 139.1 139.1 -16.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 124.8 -15.3
  .. .. .. .. .. .. ..@ area   : num 0.00102
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 124.8 124.8 124.8 124.8 -15.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 135.1 -11.9
  .. .. .. .. .. .. ..@ area   : num 0.000989
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 135.1 135 135.1 135.1 -11.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.5 -11.9
  .. .. .. .. .. .. ..@ area   : num 0.000979
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.5 136.5 136.5 136.5 -11.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 150.3 -21.7
  .. .. .. .. .. .. ..@ area   : num 0.000907
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 150.3 150.3 150.3 150.3 -21.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.5 -35.2
  .. .. .. .. .. .. ..@ area   : num 0.000873
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.5 136.4 136.5 136.5 -35.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.2 -10.6
  .. .. .. .. .. .. ..@ area   : num 0.000854
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 142.2 142.2 142.2 142.2 -10.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 148 -20
  .. .. .. .. .. .. ..@ area   : num 0.000845
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 148 148 148 148 -20 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 137.4 -34.5
  .. .. .. .. .. .. ..@ area   : num 0.000833
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 137.4 137.3 137.4 137.4 -34.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 115.7 -32.2
  .. .. .. .. .. .. ..@ area   : num 0.000823
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 115.7 115.7 115.6 115.7 -32.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.8 -12.1
  .. .. .. .. .. .. ..@ area   : num 0.000812
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.8 136.8 136.8 136.8 -12.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 151 -23.2
  .. .. .. .. .. .. ..@ area   : num 0.000787
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 151 150.9 151 151 -23.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.6 -14.8
  .. .. .. .. .. .. ..@ area   : num 0.00074
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.6 129.6 129.6 129.6 -14.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 149.1 -20.5
  .. .. .. .. .. .. ..@ area   : num 0.000695
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 149.1 149 149.1 149.1 -20.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 123.2 -34.1
  .. .. .. .. .. .. ..@ area   : num 0.000586
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 123.2 123.2 123.2 123.2 -34.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 118.5 -34.9
  .. .. .. .. .. .. ..@ area   : num 0.000547
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 118.5 118.4 118.5 118.5 -34.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 153 -25.3
  .. .. .. .. .. .. ..@ area   : num 0.000506
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 153 152.9 153 153 -25.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 136.1 -13.5
  .. .. .. .. .. .. ..@ area   : num 0.000501
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 136.2 136.2 136.1 136.2 -13.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.3 -10.5
  .. .. .. .. .. .. ..@ area   : num 0.000479
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 142.3 142.3 142.3 142.3 -10.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 147.3 -39.5
  .. .. .. .. .. .. ..@ area   : num 0.000435
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 147.3 147.3 147.3 147.3 -39.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 122.2 -34.1
  .. .. .. .. .. .. ..@ area   : num 0.000401
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 122.2 122.2 122.3 122.2 -34.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 113.1 -25.2
  .. .. .. .. .. .. ..@ area   : num 0.000184
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 113.1 113.1 113.1 113.1 -25.2 ...
  .. .. .. ..@ plotOrder: int [1:97] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 134.4 -25.6
  .. .. .. ..@ ID       : chr "AUS"
  .. .. .. ..@ area     : num 696
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 6
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 50.5 26
  .. .. .. .. .. .. ..@ area   : num 0.0205
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 50.5 50.6 50.5 50.5 26.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 50.8 25.7
  .. .. .. .. .. .. ..@ area   : num 0.00287
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 50.8 50.7 50.8 50.8 25.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 50.6 26.3
  .. .. .. .. .. .. ..@ area   : num 0.00075
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 50.7 50.6 50.6 50.7 26.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 50.8 25.7
  .. .. .. .. .. .. ..@ area   : num 0.000224
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 50.8 50.8 50.8 50.8 25.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 50.8 25.7
  .. .. .. .. .. .. ..@ area   : num 4.56e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 50.8 50.8 50.8 50.8 25.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 50.8 25.8
  .. .. .. .. .. .. ..@ area   : num 2.73e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 50.8 50.8 50.8 50.8 25.7 ...
  .. .. .. ..@ plotOrder: int [1:6] 1 2 3 4 5 6
  .. .. .. ..@ labpt    : num [1:2] 50.5 26
  .. .. .. ..@ ID       : chr "BHR"
  .. .. .. ..@ area     : num 0.0244
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -59.5 13.2
  .. .. .. .. .. .. ..@ area   : num 0.0208
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -59.5 -59.6 -59.4 -59.5 13.1 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -59.5 13.2
  .. .. .. ..@ ID       : chr "BRB"
  .. .. .. ..@ area     : num 0.0208
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 3
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.8 32.3
  .. .. .. .. .. .. ..@ area   : num 0.00389
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -64.8 -64.9 -64.7 -64.8 32.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.9 32.3
  .. .. .. .. .. .. ..@ area   : num 0.000459
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -64.9 -64.9 -64.8 -64.9 32.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.7 32.4
  .. .. .. .. .. .. ..@ area   : num 0.000332
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -64.6 -64.7 -64.7 -64.6 32.4 ...
  .. .. .. ..@ plotOrder: int [1:3] 1 2 3
  .. .. .. ..@ labpt    : num [1:2] -64.8 32.3
  .. .. .. ..@ ID       : chr "BMU"
  .. .. .. ..@ area     : num 0.00468
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 28
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78 24.6
  .. .. .. .. .. .. ..@ area   : num 0.253
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -78.2 -77.7 -78 -78.4 -78.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.3 26.4
  .. .. .. .. .. .. ..@ area   : num 0.157
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -77.7 -77 -77.2 -77.4 -77.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.5 26.6
  .. .. .. .. .. .. ..@ area   : num 0.116
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.9 -78.7 -79 -77.9 26.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.7 24
  .. .. .. .. .. .. ..@ area   : num 0.0711
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -77.7 -77.6 -77.6 -77.9 -77.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.5 24.3
  .. .. .. .. .. .. ..@ area   : num 0.0639
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.3 -75.5 -75.7 -75.3 24.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.2 21.1
  .. .. .. .. .. .. ..@ area   : num 0.0613
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73 -73.7 -73 -73 21.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74 22.5
  .. .. .. .. .. .. ..@ area   : num 0.0496
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.8 -73.9 -74.3 -73.8 22.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.4 25.2
  .. .. .. .. .. .. ..@ area   : num 0.0354
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -76.5 -76.3 -76.1 -76.2 -76.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.1 23.2
  .. .. .. .. .. .. ..@ area   : num 0.0184
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75 -75.3 -74.8 -75 23.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.9 22.3
  .. .. .. .. .. .. ..@ area   : num 0.0167
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -72.8 -72.8 -73.2 -72.8 22.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.1 22.7
  .. .. .. .. .. .. ..@ area   : num 0.012
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.1 -74.3 -74 -74.1 22.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.9 23.6
  .. .. .. .. .. .. ..@ area   : num 0.00614
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.8 -76 -76 -75.8 23.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.5 24.1
  .. .. .. .. .. .. ..@ area   : num 0.00561
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.5 -74.5 -74.5 -74.5 24 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73 21.5
  .. .. .. .. .. .. ..@ area   : num 0.00484
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -72.9 -73 -73.1 -72.9 21.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.9 24.2
  .. .. .. .. .. .. ..@ area   : num 0.00448
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.9 -78 -77.8 -77.9 24.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.4 25
  .. .. .. .. .. .. ..@ area   : num 0.00432
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.4 -77.6 -77.3 -77.4 25 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.9 23.7
  .. .. .. .. .. .. ..@ area   : num 0.00373
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.8 -74.8 -74.9 -74.8 23.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78 24.3
  .. .. .. .. .. .. ..@ area   : num 0.00229
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.9 -78 -77.9 -77.9 24.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.7 24.3
  .. .. .. .. .. .. ..@ area   : num 0.00209
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.7 -77.8 -77.7 -77.7 24.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.3 22.6
  .. .. .. .. .. .. ..@ area   : num 0.00194
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.4 -74.4 -74.3 -74.4 22.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.7 23.1
  .. .. .. .. .. .. ..@ area   : num 0.00162
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.7 -73.8 -73.7 -73.7 23.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.5 24.2
  .. .. .. .. .. .. ..@ area   : num 0.0011
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -76.5 -76.5 -76.5 -76.5 24.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.6 26.3
  .. .. .. .. .. .. ..@ area   : num 0.00105
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.6 -77.6 -77.5 -77.6 26.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.6 23.4
  .. .. .. .. .. .. ..@ area   : num 0.00102
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.6 -75.5 -75.7 -75.6 23.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.9 25.8
  .. .. .. .. .. .. ..@ area   : num 0.000989
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.8 -77.9 -77.9 -77.8 25.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.4 24.1
  .. .. .. .. .. .. ..@ area   : num 0.000845
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -76.4 -76.3 -76.4 -76.4 24.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.5 22.6
  .. .. .. .. .. .. ..@ area   : num 0.000571
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.5 -73.6 -73.5 -73.5 22.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.6 22.6
  .. .. .. .. .. .. ..@ area   : num 0.000299
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.6 -73.6 -73.6 -73.6 22.6 ...
  .. .. .. ..@ plotOrder: int [1:28] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] -78 24.6
  .. .. .. ..@ ID       : chr "BHS"
  .. .. .. ..@ area     : num 0.897
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 35
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.3 23.9
  .. .. .. .. .. .. ..@ area   : num 11.5
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:45, 1:2] 88.4 89.3 89.7 89.8 92.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.7 22.4
  .. .. .. .. .. .. ..@ area   : num 0.11
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] 90.8 90.6 90.7 90.6 90.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 91.1 22.3
  .. .. .. .. .. .. ..@ area   : num 0.0278
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 91 91.1 91.2 91 22.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 91.9 21.6
  .. .. .. .. .. .. ..@ area   : num 0.013
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 91.9 91.9 92 91.9 21.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.6 22.8
  .. .. .. .. .. .. ..@ area   : num 0.0107
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 90.5 90.5 90.7 90.5 90.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 91.5 22.5
  .. .. .. .. .. .. ..@ area   : num 0.0107
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 91.5 91.4 91.4 91.5 22.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.6 23
  .. .. .. .. .. .. ..@ area   : num 0.0094
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 90.7 90.6 90.7 90.5 90.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.5 22.1
  .. .. .. .. .. .. ..@ area   : num 0.00706
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.5 90.5 90.6 90.5 22 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.2 22.1
  .. .. .. .. .. .. ..@ area   : num 0.00473
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.2 90.1 90.2 90.2 22.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.4 22
  .. .. .. .. .. .. ..@ area   : num 0.00466
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.4 90.4 90.5 90.4 21.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 91.9 21.8
  .. .. .. .. .. .. ..@ area   : num 0.00369
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 91.8 91.9 91.9 91.8 21.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 89.1 21.8
  .. .. .. .. .. .. ..@ area   : num 0.0033
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 89.1 89.1 89.1 89.1 21.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.5 21.9
  .. .. .. .. .. .. ..@ area   : num 0.0028
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.5 90.5 90.6 90.5 21.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.6 22.4
  .. .. .. .. .. .. ..@ area   : num 0.00216
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.7 90.6 90.6 90.7 22.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 91 22.3
  .. .. .. .. .. .. ..@ area   : num 0.00179
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.9 91 91 90.9 22.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.5 23.3
  .. .. .. .. .. .. ..@ area   : num 0.0015
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.5 90.6 90.4 90.5 23.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.9 22.2
  .. .. .. .. .. .. ..@ area   : num 0.00124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.9 90.9 91 90.9 22.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.5 23
  .. .. .. .. .. .. ..@ area   : num 0.00124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.5 90.5 90.5 90.5 23 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 91.2 22.4
  .. .. .. .. .. .. ..@ area   : num 0.00103
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 91.2 91.2 91.2 91.2 22.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 91 22.2
  .. .. .. .. .. .. ..@ area   : num 0.000695
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 91 91 91 91 22.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.6 21.9
  .. .. .. .. .. .. ..@ area   : num 0.000526
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.6 90.6 90.7 90.6 21.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.4 22
  .. .. .. .. .. .. ..@ area   : num 0.000525
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.4 90.3 90.4 90.4 22 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 91 22.4
  .. .. .. .. .. .. ..@ area   : num 0.00052
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 91 91 91 91 22.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 91 22.4
  .. .. .. .. .. .. ..@ area   : num 0.00046
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 91 91 91 91 22.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.7 22.4
  .. .. .. .. .. .. ..@ area   : num 0.000366
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.7 90.6 90.7 90.7 22.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 91.3 22.6
  .. .. .. .. .. .. ..@ area   : num 0.000349
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 91.3 91.3 91.3 91.3 22.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 91 22.4
  .. .. .. .. .. .. ..@ area   : num 0.000338
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 91 91 91 91 22.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.4 21.8
  .. .. .. .. .. .. ..@ area   : num 0.000335
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.4 90.4 90.5 90.4 21.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.7 23.5
  .. .. .. .. .. .. ..@ area   : num 0.000325
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.7 90.7 90.7 90.7 23.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.7 22
  .. .. .. .. .. .. ..@ area   : num 0.000236
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.7 90.7 90.7 90.7 22 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.8 21.9
  .. .. .. .. .. .. ..@ area   : num 0.000202
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.8 90.7 90.8 90.8 21.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.6 23.6
  .. .. .. .. .. .. ..@ area   : num 0.000152
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.6 90.6 90.6 90.6 23.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.5 21.8
  .. .. .. .. .. .. ..@ area   : num 0.000144
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.5 90.5 90.5 90.5 21.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.5 23.1
  .. .. .. .. .. .. ..@ area   : num 0.000128
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.5 90.5 90.5 90.5 23 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.5 22.2
  .. .. .. .. .. .. ..@ area   : num 8.89e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 90.5 90.5 90.5 90.5 22.2 ...
  .. .. .. ..@ plotOrder: int [1:35] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 90.3 23.9
  .. .. .. ..@ ID       : chr "BGD"
  .. .. .. ..@ area     : num 11.8
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 6
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -88.7 17.2
  .. .. .. .. .. .. ..@ area   : num 1.86
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -88.4 -88.3 -88.1 -88.3 -88.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -87.9 17.4
  .. .. .. .. .. .. ..@ area   : num 0.00645
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -87.8 -87.9 -87.8 -87.8 17.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -87.9 18
  .. .. .. .. .. .. ..@ area   : num 0.00421
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -87.9 -88 -87.9 -87.9 18 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -88.1 17.7
  .. .. .. .. .. .. ..@ area   : num 0.00124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -88.1 -88.2 -88.1 -88.1 17.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -87.8 17.3
  .. .. .. .. .. .. ..@ area   : num 0.00118
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -87.8 -87.8 -87.8 -87.8 17.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -88.1 17.6
  .. .. .. .. .. .. ..@ area   : num 0.000393
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -88 -88.1 -88.1 -88 17.6 ...
  .. .. .. ..@ plotOrder: int [1:6] 1 2 3 4 5 6
  .. .. .. ..@ labpt    : num [1:2] -88.7 17.2
  .. .. .. ..@ ID       : chr "BLZ"
  .. .. .. ..@ area     : num 1.87
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 17.8 44.1
  .. .. .. .. .. .. ..@ area   : num 5.99
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] 17.6 17.6 16.1 15.8 18.3 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 17.8 44.1
  .. .. .. ..@ ID       : chr "BIH"
  .. .. .. ..@ area     : num 5.99
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.6 -16.7
  .. .. .. .. .. .. ..@ area   : num 92.2
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:50, 1:2] -65.2 -65.7 -66.2 -67.2 -67.9 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -64.6 -16.7
  .. .. .. ..@ ID       : chr "BOL"
  .. .. .. ..@ area     : num 92.2
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 44
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 96.5 21.2
  .. .. .. .. .. .. ..@ area   : num 57.3
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:89, 1:2] 97.8 98.3 98.7 98.8 98.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.7 19.2
  .. .. .. .. .. .. ..@ area   : num 0.0426
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.8 93.9 93.5 93.8 19.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 94.5 16
  .. .. .. .. .. .. ..@ area   : num 0.0355
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 94.6 94.4 94.4 94.6 94.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.8 19.4
  .. .. .. .. .. .. ..@ area   : num 0.0334
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.8 93.7 94 93.8 19.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.4 12.5
  .. .. .. .. .. .. ..@ area   : num 0.0243
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.3 98.3 98.5 98.3 12.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.5 12
  .. .. .. .. .. .. ..@ area   : num 0.0205
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.5 98.4 98.7 98.5 11.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.5 11.7
  .. .. .. .. .. .. ..@ area   : num 0.0197
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.5 98.4 98.5 98.5 11.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.2 10.8
  .. .. .. .. .. .. ..@ area   : num 0.0192
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.2 98.3 98.1 98.2 10.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.6 18.8
  .. .. .. .. .. .. ..@ area   : num 0.0186
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.7 93.7 93.5 93.7 18.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 97.6 16.4
  .. .. .. .. .. .. ..@ area   : num 0.0127
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 97.6 97.5 97.6 97.6 16.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.3 11.6
  .. .. .. .. .. .. ..@ area   : num 0.0115
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.2 98.3 98.3 98.2 11.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.2 9.96
  .. .. .. .. .. .. ..@ area   : num 0.00972
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.3 98.1 98.2 98.3 10 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.5 19.9
  .. .. .. .. .. .. ..@ area   : num 0.00645
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.5 93.4 93.5 93.5 19.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 94.8 15.9
  .. .. .. .. .. .. ..@ area   : num 0.00572
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 94.8 94.7 94.8 94.8 15.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98 12.3
  .. .. .. .. .. .. ..@ area   : num 0.00507
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.1 98 97.9 98.1 12.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.1 12.2
  .. .. .. .. .. .. ..@ area   : num 0.0047
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.1 98.1 98.1 98.1 12.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.6 12.4
  .. .. .. .. .. .. ..@ area   : num 0.00434
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 98.6 98.7 98.6 98.5 98.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98 11.7
  .. .. .. .. .. .. ..@ area   : num 0.00432
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.1 98 98 98.1 11.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.5 11
  .. .. .. .. .. .. ..@ area   : num 0.00418
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.5 98.5 98.5 98.5 11 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93 20
  .. .. .. .. .. .. ..@ area   : num 0.00414
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93 92.9 93 93 19.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.3 13.1
  .. .. .. .. .. .. ..@ area   : num 0.00377
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.3 98.3 98.2 98.3 13 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.2 10.1
  .. .. .. .. .. .. ..@ area   : num 0.00316
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.2 98.2 98.2 98.2 10 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.5 10.8
  .. .. .. .. .. .. ..@ area   : num 0.00264
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.5 98.5 98.6 98.5 10.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 97.9 10.4
  .. .. .. .. .. .. ..@ area   : num 0.00174
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 97.9 97.9 97.9 97.9 10.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.5 12.4
  .. .. .. .. .. .. ..@ area   : num 0.00161
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.5 98.5 98.5 98.5 12.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.3 11.3
  .. .. .. .. .. .. ..@ area   : num 0.00134
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.3 98.2 98.3 98.3 11.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.3 12.5
  .. .. .. .. .. .. ..@ area   : num 0.00126
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.3 98.2 98.2 98.3 12.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 97.9 10.9
  .. .. .. .. .. .. ..@ area   : num 0.00123
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 97.9 97.9 97.9 97.9 10.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.2 10.5
  .. .. .. .. .. .. ..@ area   : num 0.00112
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.2 98.2 98.2 98.2 10.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 97.7 11.9
  .. .. .. .. .. .. ..@ area   : num 0.00103
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 97.7 97.6 97.7 97.7 11.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 97.9 12.8
  .. .. .. .. .. .. ..@ area   : num 0.00085
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 97.9 97.8 97.8 97.9 12.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.2 12
  .. .. .. .. .. .. ..@ area   : num 0.00082
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.3 98.2 98.3 98.3 11.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.4 14.1
  .. .. .. .. .. .. ..@ area   : num 0.000794
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.4 93.4 93.4 93.4 14.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.04 9.81
  .. .. .. .. .. .. ..@ area   : num 0.000782
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.04 98.02 98.06 98.04 9.79 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.3 11.9
  .. .. .. .. .. .. ..@ area   : num 0.000692
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.3 98.3 98.3 98.3 11.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 97.8 12.6
  .. .. .. .. .. .. ..@ area   : num 0.000691
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 97.9 97.8 97.8 97.9 12.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.7 14.9
  .. .. .. .. .. .. ..@ area   : num 0.000555
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.7 93.7 93.7 93.7 14.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 97.9 13.8
  .. .. .. .. .. .. ..@ area   : num 0.000507
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 97.9 97.9 97.9 97.9 13.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 97.8 14.2
  .. .. .. .. .. .. ..@ area   : num 0.000473
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 97.8 97.8 97.8 97.8 14.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.4 11
  .. .. .. .. .. .. ..@ area   : num 0.000429
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.4 98.4 98.5 98.4 11 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 97.7 15.5
  .. .. .. .. .. .. ..@ area   : num 0.000378
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 97.7 97.7 97.7 97.7 15.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 97.5 11.8
  .. .. .. .. .. .. ..@ area   : num 0.000338
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 97.5 97.4 97.5 97.5 11.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 98.6 12.4
  .. .. .. .. .. .. ..@ area   : num 8.66e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 98.6 98.6 98.6 98.6 12.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 97.9 13.9
  .. .. .. .. .. .. ..@ area   : num 5.77e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 97.9 97.9 97.9 97.9 13.9 ...
  .. .. .. ..@ plotOrder: int [1:44] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 96.5 21.2
  .. .. .. ..@ ID       : chr "MMR"
  .. .. .. ..@ area     : num 57.6
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 2.33 9.6
  .. .. .. .. .. .. ..@ area   : num 10.1
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] 2.484 1.635 1.399 1.355 0.777 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 2.33 9.6
  .. .. .. ..@ ID       : chr "BEN"
  .. .. .. ..@ area     : num 10.1
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 50
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 160.17 -9.63
  .. .. .. .. .. .. ..@ area   : num 0.352
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 160 160 161 160 160 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 159.41 -8.15
  .. .. .. .. .. .. ..@ area   : num 0.145
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 159.85 159.89 158.49 159.85 -8.33 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 157.51 -8.28
  .. .. .. .. .. .. ..@ area   : num 0.138
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 158 158 157 157 158 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 156.93 -7.06
  .. .. .. .. .. .. ..@ area   : num 0.116
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 157.43 156.94 156.44 157.43 -7.32 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 161.7 -10.5
  .. .. .. .. .. .. ..@ area   : num 0.0974
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 161.5 162.4 161.3 161.5 -10.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 160.98 -8.94
  .. .. .. .. .. .. ..@ area   : num 0.0508
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 160.97 161.38 160.58 160.97 -8.85 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 156.69 -7.73
  .. .. .. .. .. .. ..@ area   : num 0.0292
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 156.7 156.6 156.8 156.7 -7.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 161.44 -9.58
  .. .. .. .. .. .. ..@ area   : num 0.0271
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 161.57 161.4 161.35 161.57 -9.72 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 158 -8.64
  .. .. .. .. .. .. ..@ area   : num 0.0243
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 158 157.88 158.12 158 -8.77 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 160.2 -11.6
  .. .. .. .. .. .. ..@ area   : num 0.0239
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 160.1 160.5 160 160.1 -11.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 157.3 -8.6
  .. .. .. .. .. .. ..@ area   : num 0.0237
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 157.4 157.4 157.2 157.4 -8.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 165.9 -10.7
  .. .. .. .. .. .. ..@ area   : num 0.021
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 166.1 165.8 165.9 166.1 -10.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 157.05 -8.02
  .. .. .. .. .. .. ..@ area   : num 0.0201
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 157.16 156.97 157.03 157.16 -8.15 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 155.76 -7.05
  .. .. .. .. .. .. ..@ area   : num 0.0118
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 155.9 155.7 155.7 155.9 -7.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 160.31 -9.12
  .. .. .. .. .. .. ..@ area   : num 0.00915
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 160.41 160.21 160.32 160.41 -9.14 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 159.6 -8.46
  .. .. .. .. .. .. ..@ area   : num 0.00776
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 159.68 159.54 159.57 159.68 -8.55 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 159.11 -9.05
  .. .. .. .. .. .. ..@ area   : num 0.00633
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 159.15 159.03 159.14 159.15 -9.11 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 166.8 -11.6
  .. .. .. .. .. .. ..@ area   : num 0.00611
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 166.9 166.8 166.9 166.9 -11.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 156.56 -8.08
  .. .. .. .. .. .. ..@ area   : num 0.0055
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 156.6 156.5 156.5 156.6 -8.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 157.57 -8.75
  .. .. .. .. .. .. ..@ area   : num 0.00524
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 157.63 157.62 157.44 157.63 -8.75 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 158.4 -7.63
  .. .. .. .. .. .. ..@ area   : num 0.0048
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 158.53 158.36 158.31 158.53 -7.65 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 158.19 -8.77
  .. .. .. .. .. .. ..@ area   : num 0.00444
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 158.21 158.15 158.21 158.21 -8.83 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 157.2 -8.2
  .. .. .. .. .. .. ..@ area   : num 0.00434
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 157.21 157.09 157.18 157.21 -8.27 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 157.1 -8.26
  .. .. .. .. .. .. ..@ area   : num 0.00359
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 157.12 157.15 157.02 157.12 -8.26 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 158.35 -7.52
  .. .. .. .. .. .. ..@ area   : num 0.00346
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 158.43 158.37 158.26 158.43 -7.53 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 157.74 -7.43
  .. .. .. .. .. .. ..@ area   : num 0.00305
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 157.78 157.68 157.75 157.78 -7.48 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 166.5 -11.3
  .. .. .. .. .. .. ..@ area   : num 0.00244
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 166.5 166.5 166.6 166.5 -11.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 161.96 -9.74
  .. .. .. .. .. .. ..@ area   : num 0.0024
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 161.97 161.94 161.97 161.97 -9.85 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 156.09 -6.92
  .. .. .. .. .. .. ..@ area   : num 0.0024
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 156.11 156.08 156.07 156.11 -6.94 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 159.2 -9.09
  .. .. .. .. .. .. ..@ area   : num 0.00233
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 159.2 159.17 159.23 159.2 -9.13 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 155.56 -7.36
  .. .. .. .. .. .. ..@ area   : num 0.00219
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 155.59 155.51 155.58 155.59 -7.39 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 160.08 -8.99
  .. .. .. .. .. .. ..@ area   : num 0.00154
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 160 160 160 160 -9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 161.7 -10.3
  .. .. .. .. .. .. ..@ area   : num 0.00145
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 161.8 161.7 161.8 161.8 -10.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 156.8 -8.09
  .. .. .. .. .. .. ..@ area   : num 0.00131
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 156.84 156.78 156.78 156.84 -8.12 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 159.83 -9.14
  .. .. .. .. .. .. ..@ area   : num 0.000827
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 159.83 159.81 159.85 159.83 -9.17 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 158.55 -7.69
  .. .. .. .. .. .. ..@ area   : num 0.000649
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 158.6 158.5 158.5 158.6 -7.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 156.02 -6.81
  .. .. .. .. .. .. ..@ area   : num 0.000628
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 156.02 156 156.05 156.02 -6.82 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 159.8 -11.3
  .. .. .. .. .. .. ..@ area   : num 0.000588
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 159.9 159.8 159.8 159.9 -11.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 161.04 -8.76
  .. .. .. .. .. .. ..@ area   : num 0.000578
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 161.06 161.04 161.01 161.06 -8.75 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 162.5 -10.8
  .. .. .. .. .. .. ..@ area   : num 0.000565
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 162.5 162.5 162.5 162.5 -10.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 160.03 -8.89
  .. .. .. .. .. .. ..@ area   : num 0.000409
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 160.05 160.01 160.03 160.05 -8.92 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 159.37 -5.43
  .. .. .. .. .. .. ..@ area   : num 0.000318
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 159.4 159.37 159.35 159.4 -5.45 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 159.7 -5.5
  .. .. .. .. .. .. ..@ area   : num 0.000163
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 159.71 159.71 159.72 159.71 -5.52 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 159.51 -5.48
  .. .. .. .. .. .. ..@ area   : num 0.000155
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 159.5 159.5 159.5 159.5 -5.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 167.2 -9.95
  .. .. .. .. .. .. ..@ area   : num 0.000144
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 167.2 167.19 167.21 167.2 -9.96 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 167.12 -9.87
  .. .. .. .. .. .. ..@ area   : num 0.000126
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 167.13 167.11 167.13 167.13 -9.88 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 162.74 -8.38
  .. .. .. .. .. .. ..@ area   : num 0.00011
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 162.74 162.76 162.73 162.74 -8.37 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 167.1 -9.85
  .. .. .. .. .. .. ..@ area   : num 8.62e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 167.1 167.09 167.11 167.1 -9.85 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 161.38 -9.46
  .. .. .. .. .. .. ..@ area   : num 4.43e-05
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 161.39 161.39 161.38 161.39 -9.47 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 159.25 -5.31
  .. .. .. .. .. .. ..@ area   : num 3.81e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 159.25 159.25 159.25 159.25 -5.31 ...
  .. .. .. ..@ plotOrder: int [1:50] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 160.17 -9.63
  .. .. .. ..@ ID       : chr "SLB"
  .. .. .. ..@ area     : num 1.17
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 62
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.1 -10.9
  .. .. .. .. .. .. ..@ area   : num 701
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:237, 1:2] -60.1 -60.1 -59.7 -59.6 -60 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -49.71 -0.936
  .. .. .. .. .. .. ..@ area   : num 2.96
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -49.7 -48.4 -48.6 -49.2 -50.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.511 -0.977
  .. .. .. .. .. .. ..@ area   : num 0.323
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -51.9 -52 -51.9 -51.7 -51.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.0044 0.0882
  .. .. .. .. .. .. ..@ area   : num 0.0928
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -49.636 -49.985 -50.393 -49.636 0.228 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -49.6266 -0.0428
  .. .. .. .. .. .. ..@ area   : num 0.0343
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -49.526 -49.853 -49.502 -49.526 -0.134 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.006 -0.742
  .. .. .. .. .. .. ..@ area   : num 0.0299
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -51.132 -51.07 -50.816 -51.132 -0.959 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.81 -0.19
  .. .. .. .. .. .. ..@ area   : num 0.0275
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.5569 -50.8578 -51.0272 -50.5569 -0.0625 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.442 0.353
  .. .. .. .. .. .. ..@ area   : num 0.025
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.416 -50.536 -50.374 -50.416 0.211 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.203 0.501
  .. .. .. .. .. .. ..@ area   : num 0.0239
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.238 -50.308 -50.064 -50.238 0.351 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -52.05 -1.54
  .. .. .. .. .. .. ..@ area   : num 0.0204
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -52 -52.2 -52 -51.9 -52 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -45.3 -23.9
  .. .. .. .. .. .. ..@ area   : num 0.0196
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -45.2 -45.2 -45.4 -45.2 -23.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -48.5 -27.5
  .. .. .. .. .. .. ..@ area   : num 0.0186
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -48.6 -48.5 -48.4 -48.6 -27.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.3 -1.1
  .. .. .. .. .. .. ..@ area   : num 0.0174
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -51.4 -51.4 -51.4 -51.2 -51.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51 -1.91
  .. .. .. .. .. .. ..@ area   : num 0.0169
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.86 -50.99 -51.15 -50.86 -1.91 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.207 -0.297
  .. .. .. .. .. .. ..@ area   : num 0.0165
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -51.381 -51.103 -51.138 -51.381 -0.485 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -48.6 -26.3
  .. .. .. .. .. .. ..@ area   : num 0.016
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -48.6 -48.7 -48.5 -48.6 -26.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.42 2.01
  .. .. .. .. .. .. ..@ area   : num 0.0157
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.4 -50.51 -50.36 -50.4 1.88 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.5213 0.0983
  .. .. .. .. .. .. ..@ area   : num 0.015
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.4556 -50.6433 -50.465 -50.4556 -0.0228 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -39 -13.5
  .. .. .. .. .. .. ..@ area   : num 0.0144
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -38.9 -39 -39 -39 -39 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -44.2 -23.1
  .. .. .. .. .. .. ..@ area   : num 0.0143
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -44.1 -44.4 -44.2 -44.1 -23.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.03 -0.392
  .. .. .. .. .. .. ..@ area   : num 0.0139
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -51.11 -51.03 -50.95 -51.11 -0.54 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -48.38 -1.14
  .. .. .. .. .. .. ..@ area   : num 0.0129
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -48.3 -48.5 -48.4 -48.3 -48.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.159 0.838
  .. .. .. .. .. .. ..@ area   : num 0.0122
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.007 -50.268 -50.201 -50.007 0.882 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -48.97 -1.69
  .. .. .. .. .. .. ..@ area   : num 0.0119
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -49 -49 -48.9 -49 -1.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -44.55 -2.85
  .. .. .. .. .. .. ..@ area   : num 0.0113
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -44.59 -44.58 -44.48 -44.59 -3.05 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -38.7 -13
  .. .. .. .. .. .. ..@ area   : num 0.011
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -38.8 -38.6 -38.6 -38.8 -13.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.055 -0.977
  .. .. .. .. .. .. ..@ area   : num 0.0109
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -51 -50.9 -51 -51 -51.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -47.8 -24.9
  .. .. .. .. .. .. ..@ area   : num 0.00977
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -47.9 -47.8 -47.6 -47.9 -25 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -44.72 -1.63
  .. .. .. .. .. .. ..@ area   : num 0.00933
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -44.7 -44.7 -44.8 -44.8 -44.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.871 -0.446
  .. .. .. .. .. .. ..@ area   : num 0.00918
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.959 -50.878 -50.777 -50.959 -0.567 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -46.489 -0.969
  .. .. .. .. .. .. ..@ area   : num 0.00861
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -46.5 -46.5 -46.5 -46.5 -46.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.8096 0.0124
  .. .. .. .. .. .. ..@ area   : num 0.00849
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.9017 -50.8297 -50.6975 -50.9017 -0.0475 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -52.34 -1.47
  .. .. .. .. .. .. ..@ area   : num 0.00839
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -52.42 -52.43 -52.17 -52.42 -1.53 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -49.1 -1.8
  .. .. .. .. .. .. ..@ area   : num 0.00763
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -49.02 -49.16 -49.06 -49.02 -1.83 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.82 -0.315
  .. .. .. .. .. .. ..@ area   : num 0.00694
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.803 -50.908 -50.749 -50.803 -0.359 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -44.95 -1.32
  .. .. .. .. .. .. ..@ area   : num 0.00675
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -45 -45 -44.9 -45 -1.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -47.678 -0.647
  .. .. .. .. .. .. ..@ area   : num 0.00663
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -47.7 -47.7 -47.7 -47.7 -47.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -45.65 -1.28
  .. .. .. .. .. .. ..@ area   : num 0.00601
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -45.63 -45.69 -45.63 -45.63 -1.35 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.01 0.99
  .. .. .. .. .. .. ..@ area   : num 0.00529
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.009 -50.074 -49.948 -50.009 0.935 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.17 0.291
  .. .. .. .. .. .. ..@ area   : num 0.00489
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.243 -50.184 -50.084 -50.243 0.229 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -47.93 -0.683
  .. .. .. .. .. .. ..@ area   : num 0.00476
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -47.9 -48 -48 -47.9 -47.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -48.3 -25.4
  .. .. .. .. .. .. ..@ area   : num 0.00467
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -48.3 -48.3 -48.3 -48.3 -25.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.895 -0.767
  .. .. .. .. .. .. ..@ area   : num 0.00464
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.92 -50.923 -50.843 -50.92 -0.845 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.47 2.17
  .. .. .. .. .. .. ..@ area   : num 0.00445
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.48 -50.52 -50.41 -50.48 2.12 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -42.1 -2.71
  .. .. .. .. .. .. ..@ area   : num 0.00428
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -42.05 -42.18 -42.07 -42.05 -2.76 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -48.56 -1.44
  .. .. .. .. .. .. ..@ area   : num 0.00416
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -48.53 -48.59 -48.55 -48.53 -1.52 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.351 -0.498
  .. .. .. .. .. .. ..@ area   : num 0.00372
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -51.403 -51.411 -51.239 -51.403 -0.541 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -38.9 -13.6
  .. .. .. .. .. .. ..@ area   : num 0.00348
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -38.9 -39 -38.9 -38.9 -13.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.94 -0.292
  .. .. .. .. .. .. ..@ area   : num 0.00343
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.907 -51.025 -50.889 -50.907 -0.336 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -46.3 -24
  .. .. .. .. .. .. ..@ area   : num 0.00316
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -46.3 -46.4 -46.3 -46.3 -24 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -48.66 -1.44
  .. .. .. .. .. .. ..@ area   : num 0.00301
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -48.63 -48.7 -48.66 -48.63 -1.48 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -44.63 -2.96
  .. .. .. .. .. .. ..@ area   : num 0.00274
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -44.61 -44.66 -44.61 -44.61 -3.01 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.058 -0.618
  .. .. .. .. .. .. ..@ area   : num 0.00265
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -51.051 -51.098 -51.025 -51.051 -0.659 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -37.1 -11.1
  .. .. .. .. .. .. ..@ area   : num 0.00222
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -37.1 -37.1 -37 -37.1 -11.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -50.058 0.908
  .. .. .. .. .. .. ..@ area   : num 0.000862
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -50.038 -50.094 -50.042 -50.038 0.884 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -32.42 -3.86
  .. .. .. .. .. .. ..@ area   : num 0.000634
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -32.45 -32.42 -32.39 -32.45 -3.88 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -43.68 -2.31
  .. .. .. .. .. .. ..@ area   : num 0.000566
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -43.69 -43.74 -43.62 -43.69 -2.33 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -30.3 -20.5
  .. .. .. .. .. .. ..@ area   : num 0.000561
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -30.3 -30.3 -30.3 -30.3 -20.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -45.1 -23.8
  .. .. .. .. .. .. ..@ area   : num 0.000328
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -45.1 -45.1 -45.1 -45.1 -23.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -34.88 -7.06
  .. .. .. .. .. .. ..@ area   : num 0.00019
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -34.87 -34.89 -34.87 -34.87 -7.07 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -34.9 -7
  .. .. .. .. .. .. ..@ area   : num 7.21e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -34.85 -34.86 -34.86 -34.85 -7.01 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -29.8 -20.5
  .. .. .. .. .. .. ..@ area   : num 5.06e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -29.8 -29.8 -29.9 -29.8 -20.5 ...
  .. .. .. ..@ plotOrder: int [1:62] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] -53.1 -10.9
  .. .. .. ..@ ID       : chr "BRA"
  .. .. .. ..@ area     : num 705
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.3 42.7
  .. .. .. .. .. .. ..@ area   : num 12.1
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:24, 1:2] 27.9 27.4 28 27.4 26.4 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 25.3 42.7
  .. .. .. ..@ ID       : chr "BGR"
  .. .. .. ..@ area     : num 12.1
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 2
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 114.59 4.51
  .. .. .. .. .. .. ..@ area   : num 0.351
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 115 115 115 114 115 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 115.19 4.68
  .. .. .. .. .. .. ..@ area   : num 0.0558
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 115 115 115 115 115 ...
  .. .. .. ..@ plotOrder: int [1:2] 1 2
  .. .. .. ..@ labpt    : num [1:2] 114.59 4.51
  .. .. .. ..@ ID       : chr "BRN"
  .. .. .. ..@ area     : num 0.407
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 475
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -101.7 57.7
  .. .. .. .. .. .. ..@ area   : num 1260
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:593, 1:2] -134 -134 -134 -134 -135 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.7 68.6
  .. .. .. .. .. .. ..@ area   : num 110
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:332, 1:2] -73.4 -73.2 -73.3 -73.4 -73.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.1 80.3
  .. .. .. .. .. .. ..@ area   : num 89.7
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:162, 1:2] -70.1 -66.3 -68.6 -64.7 -64.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -110.3 70.9
  .. .. .. .. .. .. ..@ area   : num 53.1
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:64, 1:2] -114 -115 -114 -113 -111 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -121 73
  .. .. .. .. .. .. ..@ area   : num 19.4
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:22, 1:2] -120 -120 -120 -119 -119 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -91.1 79.6
  .. .. .. .. .. .. ..@ area   : num 17.6
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:53, 1:2] -92.7 -90.8 -89.2 -88.8 -88.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -87.5 75.4
  .. .. .. .. .. .. ..@ area   : num 16.8
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:44, 1:2] -95.7 -93.2 -93.5 -91.4 -90.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56.1 48.7
  .. .. .. .. .. .. ..@ area   : num 12.9
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:69, 1:2] -53.8 -54.1 -54.1 -53.8 -53 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -111.5 75.5
  .. .. .. .. .. .. ..@ area   : num 12.2
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:38, 1:2] -109 -109 -108 -108 -108 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -99 72.7
  .. .. .. .. .. .. ..@ area   : num 8.54
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:35, 1:2] -99.8 -99.2 -97.8 -97 -97.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -84.2 64.4
  .. .. .. .. .. .. ..@ area   : num 7.88
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -85.5 -85.2 -85.3 -84.9 -84.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -93.5 73.3
  .. .. .. .. .. .. ..@ area   : num 6.84
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] -92.6 -90.2 -92.1 -94.3 -93.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -118.9 76.7
  .. .. .. .. .. .. ..@ area   : num 5.59
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:27, 1:2] -116 -115 -116 -116 -116 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -99.4 75.8
  .. .. .. .. .. .. ..@ area   : num 4.35
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:27, 1:2] -98.4 -97.5 -97.9 -97.4 -97.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -102 78.6
  .. .. .. .. .. .. ..@ area   : num 4.12
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -103.6 -102.6 -101.6 -100 -99.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -125.7 49.7
  .. .. .. .. .. .. ..@ area   : num 3.55
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:25, 1:2] -127 -125 -125 -124 -124 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.7 73.2
  .. .. .. .. .. .. ..@ area   : num 3.1
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -80.1 -77.4 -76.1 -79.4 -80.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -97.3 69.1
  .. .. .. .. .. .. ..@ area   : num 2.84
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -97.4 -95.2 -96.5 -99.6 -98.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -96.7 78.3
  .. .. .. .. .. .. ..@ area   : num 1.88
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -96.8 -94.9 -95.4 -94.9 -97.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.1 67.8
  .. .. .. .. .. .. ..@ area   : num 1.86
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -75.6 -75 -75.2 -76.7 -77.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -94.8 75.2
  .. .. .. .. .. .. ..@ area   : num 1.84
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -94.4 -93.5 -93.5 -96.6 -95.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -111.6 77.7
  .. .. .. .. .. .. ..@ area   : num 1.46
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -110 -111 -110 -110 -112 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -105.6 73.4
  .. .. .. .. .. .. ..@ area   : num 1.13
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -105 -104 -105 -107 -105 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.8 46.2
  .. .. .. .. .. .. ..@ area   : num 1.01
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -60.4 -60.6 -60.4 -61.1 -60.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -83.1 62.6
  .. .. .. .. .. .. ..@ area   : num 0.922
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -81.9 -83.1 -83.7 -83.9 -83.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -132.4 53.7
  .. .. .. .. .. .. ..@ area   : num 0.777
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] -133 -132 -133 -132 -132 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -134.9 69.2
  .. .. .. .. .. .. ..@ area   : num 0.77
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:24, 1:2] -134 -134 -135 -135 -135 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -111 78.5
  .. .. .. .. .. .. ..@ area   : num 0.715
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -110 -109 -113 -111 -110 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -63 49.4
  .. .. .. .. .. .. ..@ area   : num 0.585
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -62.1 -61.7 -63.1 -64.5 -62.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -94.6 77.6
  .. .. .. .. .. .. ..@ area   : num 0.579
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -95.4 -93.1 -93.6 -96.3 -95.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -63.3 46.4
  .. .. .. .. .. .. ..@ area   : num 0.576
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -64 -63.7 -63.6 -63.2 -62 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.8 62
  .. .. .. .. .. .. ..@ area   : num 0.494
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -79.5 -79.3 -79.7 -80.3 -80 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.4 56.2
  .. .. .. .. .. .. ..@ area   : num 0.345
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] -79 -78.9 -78.9 -79.2 -79 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -118.4 75.8
  .. .. .. .. .. .. ..@ area   : num 0.293
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -118.3 -119.4 -117.5 -118.3 75.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -81.3 53
  .. .. .. .. .. .. ..@ area   : num 0.276
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -81.1 -80.7 -82.1 -81.1 53.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -132 52.9
  .. .. .. .. .. .. ..@ area   : num 0.244
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -132 -132 -132 -132 -132 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -105.2 77.4
  .. .. .. .. .. .. ..@ area   : num 0.244
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -105 -104 -105 -106 -105 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -104.2 75.2
  .. .. .. .. .. .. ..@ area   : num 0.242
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -104 -105 -104 -104 -104 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.8 52.9
  .. .. .. .. .. .. ..@ area   : num 0.241
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -104 76.5
  .. .. .. .. .. .. ..@ area   : num 0.22
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -104 -103 -104 -105 -104 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -99.4 79.9
  .. .. .. .. .. .. ..@ area   : num 0.194
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -99.5 -98.6 -100.2 -99.5 80.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78 63.3
  .. .. .. .. .. .. ..@ area   : num 0.192
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -78.1 -77.5 -77.9 -78.6 -78.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.5 77.5
  .. .. .. .. .. .. ..@ area   : num 0.191
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -90.6 -89.6 -91.2 -90.6 77.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.7 69.1
  .. .. .. .. .. .. ..@ area   : num 0.189
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -78.4 -78.2 -78.8 -79.4 -78.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -93.3 63.9
  .. .. .. .. .. .. ..@ area   : num 0.174
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -93.5 -92.5 -93.8 -93.5 63.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.1 69.7
  .. .. .. .. .. .. ..@ area   : num 0.173
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79.4 -80 -80.8 -79.4 69.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -83.9 65.9
  .. .. .. .. .. .. ..@ area   : num 0.167
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -83.9 -83.7 -83.7 -83.2 -83.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -86.7 68
  .. .. .. .. .. .. ..@ area   : num 0.165
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -86.4 -86.6 -87 -86.7 -86.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -103.5 76.1
  .. .. .. .. .. .. ..@ area   : num 0.149
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -102.5 -103.3 -104.5 -102.5 76.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.1 68
  .. .. .. .. .. .. ..@ area   : num 0.144
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.2 -73.3 -74.8 -74.2 68.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -98.7 73.9
  .. .. .. .. .. .. ..@ area   : num 0.128
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -98.9 -99.4 -97.6 -98.9 73.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.1 76.7
  .. .. .. .. .. .. ..@ area   : num 0.122
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -89.9 -90.6 -89.7 -89.9 76.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.8 53.6
  .. .. .. .. .. .. ..@ area   : num 0.0955
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -101.7 77.8
  .. .. .. .. .. .. ..@ area   : num 0.0938
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -101.7 -100.9 -102.5 -101.7 77.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -114.3 77.9
  .. .. .. .. .. .. ..@ area   : num 0.0932
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -114 -114 -115 -114 78 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -96.6 75.5
  .. .. .. .. .. .. ..@ area   : num 0.088
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -95.9 -96.9 -97.1 -95.9 75.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -65 61.5
  .. .. .. .. .. .. ..@ area   : num 0.0878
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -64.7 -64.9 -65.5 -64.7 61.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -114.1 76.8
  .. .. .. .. .. .. ..@ area   : num 0.0874
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -113.5 -114.9 -113.9 -113.5 76.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.1 68.5
  .. .. .. .. .. .. ..@ area   : num 0.0756
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.8 -75.4 -75 -74.8 68.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -127.6 52.2
  .. .. .. .. .. .. ..@ area   : num 0.0742
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -128 -128 -127 -128 52 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.3 69.6
  .. .. .. .. .. .. ..@ area   : num 0.0691
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.9 -78.9 -78.2 -77.9 69.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -95.7 69.5
  .. .. .. .. .. .. ..@ area   : num 0.0688
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -95.5 -95.5 -95.7 -95.7 -95.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -68.1 60.4
  .. .. .. .. .. .. ..@ area   : num 0.0634
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -68.3 -68.1 -67.8 -68.3 60.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -84.8 65.7
  .. .. .. .. .. .. ..@ area   : num 0.0634
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -84.7 -85.2 -84.6 -84.7 65.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.1 53.4
  .. .. .. .. .. .. ..@ area   : num 0.0614
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -129.9 -129.8 -130.5 -129.9 53.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.3 75.9
  .. .. .. .. .. .. ..@ area   : num 0.0581
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -78.9 -79.8 -78.8 -79.2 -78.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -101.9 68.7
  .. .. .. .. .. .. ..@ area   : num 0.056
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -101.8 -102.3 -101.7 -101.8 68.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.9 70.9
  .. .. .. .. .. .. ..@ area   : num 0.0535
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -71.5 -72 -72.2 -71.5 71 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -100.5 68.9
  .. .. .. .. .. .. ..@ area   : num 0.053
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -100.2 -100.6 -100.6 -100.2 68.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -103 76
  .. .. .. .. .. .. ..@ area   : num 0.0517
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -102.4 -102.3 -104 -102.4 76.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -94.6 75.9
  .. .. .. .. .. .. ..@ area   : num 0.0487
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -94.4 -94.9 -94.5 -94.4 75.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -101.1 76.6
  .. .. .. .. .. .. ..@ area   : num 0.0464
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -101.4 -101.7 -100.2 -101.4 76.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -126.8 49.8
  .. .. .. .. .. .. ..@ area   : num 0.0462
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -127 -127 -127 -127 -127 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -70.7 62.8
  .. .. .. .. .. .. ..@ area   : num 0.0458
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -70.7 -70.2 -71.2 -70.7 62.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -96.8 73
  .. .. .. .. .. .. ..@ area   : num 0.0433
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -96.8 -97.1 -96.6 -96.8 72.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.8 68.6
  .. .. .. .. .. .. ..@ area   : num 0.0411
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -78.5 -79 -78.9 -78.5 68.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.8 47.4
  .. .. .. .. .. .. ..@ area   : num 0.0406
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -61.4 -62 -61.9 -61.4 47.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.9 63.6
  .. .. .. .. .. .. ..@ area   : num 0.0391
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -76.8 -76.5 -77.5 -76.8 63.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -102.8 75.8
  .. .. .. .. .. .. ..@ area   : num 0.038
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -103.1 -103.4 -102 -103.1 75.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -96.3 69.5
  .. .. .. .. .. .. ..@ area   : num 0.0377
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -96.7 -96.1 -96.2 -96.7 69.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -87 70.1
  .. .. .. .. .. .. ..@ area   : num 0.0377
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -87.1 -86.5 -87.4 -87.1 70.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -104.8 68.5
  .. .. .. .. .. .. ..@ area   : num 0.0374
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -104.5 -105.1 -104.7 -104.5 68.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.5 56.8
  .. .. .. .. .. .. ..@ area   : num 0.0358
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -61.4 -61.4 -61.6 -61.4 56.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77 69.3
  .. .. .. .. .. .. ..@ area   : num 0.035
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77 -76.6 -77.4 -77 69.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.1 51.9
  .. .. .. .. .. .. ..@ area   : num 0.0349
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -128.1 -128.3 -128 -128.1 51.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130 54
  .. .. .. .. .. .. ..@ area   : num 0.0327
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -130 -131 -130 -130 54 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.1 52.6
  .. .. .. .. .. .. ..@ area   : num 0.0327
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -129 -129.3 -128.9 -129 52.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -68 69.6
  .. .. .. .. .. .. ..@ area   : num 0.0318
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -67.9 -68.2 -67.9 -67.9 69.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.4 52.6
  .. .. .. .. .. .. ..@ area   : num 0.0305
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -128.4 -128.4 -128.3 -128.4 52.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -126.4 50.7
  .. .. .. .. .. .. ..@ area   : num 0.0302
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -126.3 -126.6 -126.3 -126.3 50.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.3 63.5
  .. .. .. .. .. .. ..@ area   : num 0.0291
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -64.1 -64.5 -64.3 -64.1 63.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -95.6 74.6
  .. .. .. .. .. .. ..@ area   : num 0.0289
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -95.3 -95.9 -95.5 -95.3 74.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.2 53.2
  .. .. .. .. .. .. ..@ area   : num 0.0283
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129 53.4
  .. .. .. .. .. .. ..@ area   : num 0.0279
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.8 56.4
  .. .. .. .. .. .. ..@ area   : num 0.0277
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79.6 -80.1 -79.5 -79.6 56.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.3 69.3
  .. .. .. .. .. .. ..@ area   : num 0.0275
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -90.3 -90.5 -90.2 -90.3 69.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129 53.6
  .. .. .. .. .. .. ..@ area   : num 0.0268
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79 68.3
  .. .. .. .. .. .. ..@ area   : num 0.0258
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79 -79.2 -78.8 -79 68.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.3 53.7
  .. .. .. .. .. .. ..@ area   : num 0.0258
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -130.1 -130.4 -130.3 -130.1 53.6 ...
  .. .. .. .. .. [list output truncated]
  .. .. .. ..@ plotOrder: int [1:475] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] -101.7 57.7
  .. .. .. ..@ ID       : chr "CAN"
  .. .. .. ..@ area     : num 1662
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 5
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 104.9 12.7
  .. .. .. .. .. .. ..@ area   : num 15.2
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] 107 108 106 106 106 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 103.3 10.7
  .. .. .. .. .. .. ..@ area   : num 0.00396
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 103.3 103.3 103.2 103.3 10.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 103 11.3
  .. .. .. .. .. .. ..@ area   : num 0.00391
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 103 103 103 103 11.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 103.8 10.5
  .. .. .. .. .. .. ..@ area   : num 0.00197
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 104 104 104 104 104 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 103 11.5
  .. .. .. .. .. .. ..@ area   : num 0.00103
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 103 103 103 103 11.4 ...
  .. .. .. ..@ plotOrder: int [1:5] 1 2 3 4 5
  .. .. .. ..@ labpt    : num [1:2] 104.9 12.7
  .. .. .. ..@ ID       : chr "KHM"
  .. .. .. ..@ area     : num 15.2
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 6
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 80.68 7.65
  .. .. .. .. .. .. ..@ area   : num 5.27
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] 80.3 80.8 80.9 81.2 81.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 79.9 9.67
  .. .. .. .. .. .. ..@ area   : num 0.0058
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 79.97 79.87 79.85 79.97 9.62 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 81.78 7.61
  .. .. .. .. .. .. ..@ area   : num 0.00477
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 81.81 81.72 81.8 81.81 7.48 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 79.82 9.04
  .. .. .. .. .. .. ..@ area   : num 0.00417
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 79.91 79.85 79.69 79.91 9.02 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 79.68 9.51
  .. .. .. .. .. .. ..@ area   : num 0.00165
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 79.72 79.66 79.66 79.72 9.48 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 81.71 7.69
  .. .. .. .. .. .. ..@ area   : num 0.000205
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 81.7 81.7 81.7 81.7 81.7 ...
  .. .. .. ..@ plotOrder: int [1:6] 1 2 3 4 5 6
  .. .. .. ..@ labpt    : num [1:2] 80.68 7.65
  .. .. .. ..@ ID       : chr "LKA"
  .. .. .. ..@ area     : num 5.29
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 15.218 -0.873
  .. .. .. .. .. .. ..@ area   : num 27.8
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:37, 1:2] 12.8 12 11.1 11.5 11.9 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 15.218 -0.873
  .. .. .. ..@ ID       : chr "COG"
  .. .. .. ..@ area     : num 27.8
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 2
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.65 -2.87
  .. .. .. .. .. .. ..@ area   : num 190
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:74, 1:2] 25.9 27.5 28.4 29.6 30.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.84 -5.89
  .. .. .. .. .. .. ..@ area   : num 0.00643
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 12.95 12.84 12.74 12.95 -5.88 ...
  .. .. .. ..@ plotOrder: int [1:2] 1 2
  .. .. .. ..@ labpt    : num [1:2] 23.65 -2.87
  .. .. .. ..@ ID       : chr "COD"
  .. .. .. ..@ area     : num 190
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 29.9 -3.35
  .. .. .. .. .. .. ..@ area   : num 2.33
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] 29.2 29 29.9 30 30.6 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 29.9 -3.35
  .. .. .. ..@ ID       : chr "BDI"
  .. .. .. ..@ area     : num 2.33
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 34
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 103.8 36.6
  .. .. .. .. .. .. ..@ area   : num 946
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:285, 1:2] 123 126 128 128 131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 109.7 19.2
  .. .. .. .. .. .. ..@ area   : num 3
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] 111 111 110 110 109 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 122.1 30.1
  .. .. .. .. .. .. ..@ area   : num 0.0217
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 122 122 122 122 30 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 121.5 31.7
  .. .. .. .. .. .. ..@ area   : num 0.0167
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 121.5 121.9 121.2 121.5 31.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 110 21
  .. .. .. .. .. .. ..@ area   : num 0.0134
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 110.5 110.5 110.2 110.5 21.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 117.4 23.7
  .. .. .. .. .. .. ..@ area   : num 0.0119
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 117.4 117.5 117.3 117.4 23.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 119.7 25.5
  .. .. .. .. .. .. ..@ area   : num 0.0117
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 119.8 119.7 119.7 119.8 25.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 119.8 32.2
  .. .. .. .. .. .. ..@ area   : num 0.0105
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 119.9 119.7 119.8 119.9 32.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 118.1 24.5
  .. .. .. .. .. .. ..@ area   : num 0.0085
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 118 118 118 118 118 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 117.1 23.4
  .. .. .. .. .. .. ..@ area   : num 0.00752
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 117.1 116.9 117.1 117.1 23.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 121.4 39.4
  .. .. .. .. .. .. ..@ area   : num 0.0075
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 121.4 121.3 121.4 121.4 39.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 113.5 22.8
  .. .. .. .. .. .. ..@ area   : num 0.00744
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 113.6 113.4 113.5 113.6 22.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 122.1 29.7
  .. .. .. .. .. .. ..@ area   : num 0.00743
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 122.1 122.2 122 122.1 29.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 121.2 28.1
  .. .. .. .. .. .. ..@ area   : num 0.00706
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 121 121 121 121 28 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 112.8 21.7
  .. .. .. .. .. .. ..@ area   : num 0.00688
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 112.8 112.7 112.9 112.8 21.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 122.2 30.3
  .. .. .. .. .. .. ..@ area   : num 0.00671
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 122.2 122.1 122.2 122.2 30.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 111.9 21.6
  .. .. .. .. .. .. ..@ area   : num 0.00628
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 111.8 111.8 112 111.8 21.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 110.6 21.2
  .. .. .. .. .. .. ..@ area   : num 0.00553
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 110.5 110.6 110.5 110.5 21.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 122.4 29.9
  .. .. .. .. .. .. ..@ area   : num 0.00489
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 122.4 122.4 122.3 122.4 29.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 121.9 29.1
  .. .. .. .. .. .. ..@ area   : num 0.0038
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 121.9 121.9 121.9 121.9 29.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 112.6 21.6
  .. .. .. .. .. .. ..@ area   : num 0.0036
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 112.5 112.6 112.6 112.5 21.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 121.8 31.3
  .. .. .. .. .. .. ..@ area   : num 0.00321
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 121.8 121.8 121.9 121.8 31.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 119.3 25.5
  .. .. .. .. .. .. ..@ area   : num 0.00317
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 119.3 119.3 119.3 119.3 25.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 122.3 30.4
  .. .. .. .. .. .. ..@ area   : num 0.00307
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 122.4 122.3 122.3 122.4 30.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 113.3 22.1
  .. .. .. .. .. .. ..@ area   : num 0.00288
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 113.3 113.3 113.4 113.3 22.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 113 22
  .. .. .. .. .. .. ..@ area   : num 0.00285
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 113 113 113 113 22 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 113.5 22.7
  .. .. .. .. .. .. ..@ area   : num 0.00283
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 113.6 113.5 113.5 113.6 22.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 122 30
  .. .. .. .. .. .. ..@ area   : num 0.00243
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 122 122 122 122 30 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 110.6 20.9
  .. .. .. .. .. .. ..@ area   : num 0.00193
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 110.6 110.5 110.6 110.6 20.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 109 21
  .. .. .. .. .. .. ..@ area   : num 0.001
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 109.1 109.1 109.1 109.1 21.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 119 26
  .. .. .. .. .. .. ..@ area   : num 0.000676
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 119 119 119 119 26 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 108.3 21.7
  .. .. .. .. .. .. ..@ area   : num 0.000104
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 108.3 108.3 108.3 108.3 21.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 116.7 20.7
  .. .. .. .. .. .. ..@ area   : num 5.53e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 116.7 116.7 116.7 116.7 20.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 110.5 19.1
  .. .. .. .. .. .. ..@ area   : num 3.32e-05
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 110.5 110.5 110.5 110.5 19.1 ...
  .. .. .. ..@ plotOrder: int [1:34] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 103.8 36.6
  .. .. .. ..@ ID       : chr "CHN"
  .. .. .. ..@ area     : num 950
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 66 33.9
  .. .. .. .. .. .. ..@ area   : num 62.4
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:54, 1:2] 74.9 74.4 74.6 72.6 71.2 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 66 33.9
  .. .. .. ..@ ID       : chr "AFG"
  .. .. .. ..@ area     : num 62.4
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 90.5 27.4
  .. .. .. .. .. .. ..@ area   : num 3.66
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] 90.5 91.3 91.7 92.1 92.1 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 90.5 27.4
  .. .. .. ..@ ID       : chr "BTN"
  .. .. .. ..@ area     : num 3.66
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 148
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.2 -35.1
  .. .. .. .. .. .. ..@ area   : num 64.4
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:204, 1:2] -69.5 -69.1 -68.9 -68.4 -68.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -69.5 -53.7
  .. .. .. .. .. .. ..@ area   : num 3.31
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] -70.9 -70.8 -70.9 -70.9 -72 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.9 -42.7
  .. .. .. .. .. .. ..@ area   : num 0.886
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -73.9 -73.5 -73.4 -73.7 -73.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.8 -49.3
  .. .. .. .. .. .. ..@ area   : num 0.698
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] -74.6 -74.4 -74.8 -74.9 -74.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -68.9 -55.3
  .. .. .. .. .. .. ..@ area   : num 0.448
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -68.4 -69.1 -68.2 -68.7 -68.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.8 -53.8
  .. .. .. .. .. .. ..@ area   : num 0.408
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -72.9 -72.9 -72.8 -72.1 -72.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -67.8 -55.1
  .. .. .. .. .. .. ..@ area   : num 0.238
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -67.8 -67.1 -68.1 -68.4 -67.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.1 -44.6
  .. .. .. .. .. .. ..@ area   : num 0.213
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -72.7 -72.8 -73 -72.8 -73.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.3 -54.2
  .. .. .. .. .. .. ..@ area   : num 0.111
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -71.3 -71 -71.1 -71.7 -71.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -70.6 -53.9
  .. .. .. .. .. .. ..@ area   : num 0.107
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -70.4 -70.7 -70.5 -70.9 -70.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.4 -48.4
  .. .. .. .. .. .. ..@ area   : num 0.0752
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.2 -75.2 -75.6 -75.2 -48.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.6 -51
  .. .. .. .. .. .. ..@ area   : num 0.075
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -74.4 -74.4 -74.5 -74.7 -75 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72 -54.1
  .. .. .. .. .. .. ..@ area   : num 0.067
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -71.7 -72 -71.8 -72.3 -71.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.9 -45.1
  .. .. .. .. .. .. ..@ area   : num 0.0639
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.8 -73.7 -74.2 -73.8 -45 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.4 -50.7
  .. .. .. .. .. .. ..@ area   : num 0.0576
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -74.4 -74.2 -74.5 -74.4 -74.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.1 -48.2
  .. .. .. .. .. .. ..@ area   : num 0.0527
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.9 -75 -75.3 -74.9 -48.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -69.6 -55
  .. .. .. .. .. .. ..@ area   : num 0.0484
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -69.8 -69.9 -69.2 -69.8 -55 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.1 -53
  .. .. .. .. .. .. ..@ area   : num 0.0441
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.4 -74.7 -73.1 -74.4 -52.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.9 -52.6
  .. .. .. .. .. .. ..@ area   : num 0.0428
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.8 -74.1 -73.7 -73.8 -52.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.3 -50.7
  .. .. .. .. .. .. ..@ area   : num 0.0423
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.1 -75.3 -75.5 -75.1 -50.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75 -45.9
  .. .. .. .. .. .. ..@ area   : num 0.0416
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.7 -75.1 -75.1 -74.7 -45.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.8 -51.3
  .. .. .. .. .. .. ..@ area   : num 0.0413
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.5 -75 -74.8 -74.5 -51.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.4 -49.8
  .. .. .. .. .. .. ..@ area   : num 0.0404
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.3 -75.2 -75.6 -75.3 -49.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.2 -51.4
  .. .. .. .. .. .. ..@ area   : num 0.0394
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75 -75.3 -75.2 -75 -51.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.9 -51.9
  .. .. .. .. .. .. ..@ area   : num 0.0392
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.8 -75 -74.9 -74.8 -51.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75 -49.2
  .. .. .. .. .. .. ..@ area   : num 0.0384
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.9 -75.2 -75 -74.9 -49.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.4 -45.6
  .. .. .. .. .. .. ..@ area   : num 0.0368
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.5 -74.4 -74.2 -74.5 -45.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75 -48
  .. .. .. .. .. .. ..@ area   : num 0.0352
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.8 -74.9 -75.3 -74.8 -47.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.1 -50.2
  .. .. .. .. .. .. ..@ area   : num 0.0352
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -75 -74.8 -75.5 -75.1 -75.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.2 -44.7
  .. .. .. .. .. .. ..@ area   : num 0.035
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.2 -74.4 -73.9 -74.2 -44.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.4 -49
  .. .. .. .. .. .. ..@ area   : num 0.0325
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -75.3 -75.3 -75.3 -75.5 -75.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -70.7 -55
  .. .. .. .. .. .. ..@ area   : num 0.0315
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -70.5 -70.7 -70.3 -71 -70.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.5 -48.6
  .. .. .. .. .. .. ..@ area   : num 0.0314
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.3 -75.6 -75.7 -75.3 -48.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.1 -51.7
  .. .. .. .. .. .. ..@ area   : num 0.0298
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.1 -73.9 -74.2 -74.1 -51.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.3 -54.2
  .. .. .. .. .. .. ..@ area   : num 0.0278
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -72.3 -72.5 -72.3 -72.2 -72.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.4 -49.1
  .. .. .. .. .. .. ..@ area   : num 0.0255
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.3 -75.7 -75.4 -75.3 -49.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.9 -43.8
  .. .. .. .. .. .. ..@ area   : num 0.0253
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.9 -73.8 -74.2 -73.9 -43.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.2 -45
  .. .. .. .. .. .. ..@ area   : num 0.0252
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74 -74.4 -74.2 -74 -45 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.9 -52.1
  .. .. .. .. .. .. ..@ area   : num 0.023
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.7 -74.1 -73.8 -73.7 -52.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.7 -51.9
  .. .. .. .. .. .. ..@ area   : num 0.0229
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.9 -74.7 -74.6 -74.9 -52.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.2 -52.2
  .. .. .. .. .. .. ..@ area   : num 0.0229
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.2 -73.9 -74.4 -74.2 -52.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.9 -52.3
  .. .. .. .. .. .. ..@ area   : num 0.0212
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.8 -74.1 -73.8 -73.8 -52.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -67.4 -55.7
  .. .. .. .. .. .. ..@ area   : num 0.0207
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -67.3 -67.6 -67.4 -67.3 -55.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.8 -50.8
  .. .. .. .. .. .. ..@ area   : num 0.0203
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.7 -75 -74.7 -74.7 -50.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.2 -51.8
  .. .. .. .. .. .. ..@ area   : num 0.02
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.1 -74.5 -74.1 -74.1 -52 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74 -45.6
  .. .. .. .. .. .. ..@ area   : num 0.0197
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -74 -74.1 -74 -73.9 -74 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.7 -45.6
  .. .. .. .. .. .. ..@ area   : num 0.0191
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.6 -73.8 -73.7 -73.6 -45.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.4 -48.4
  .. .. .. .. .. .. ..@ area   : num 0.0181
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.3 -74.5 -74.3 -74.3 -48.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.8 -46.1
  .. .. .. .. .. .. ..@ area   : num 0.0177
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.8 -73.9 -73.7 -73.8 -46.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.6 -53.5
  .. .. .. .. .. .. ..@ area   : num 0.0175
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.5 -73.5 -73.8 -73.5 -53.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74 -45.5
  .. .. .. .. .. .. ..@ area   : num 0.0172
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74 -73.8 -74.1 -74 -45.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.2 -48.9
  .. .. .. .. .. .. ..@ area   : num 0.0169
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.2 -75.3 -75.1 -75.2 -49 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.4 -48.7
  .. .. .. .. .. .. ..@ area   : num 0.0163
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.3 -75.7 -75.3 -75.3 -48.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.1 -47.1
  .. .. .. .. .. .. ..@ area   : num 0.015
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.2 -74.2 -74 -74.2 -47.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.7 -44.7
  .. .. .. .. .. .. ..@ area   : num 0.0146
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.7 -73.7 -73.6 -73.7 -44.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.1 -48.7
  .. .. .. .. .. .. ..@ area   : num 0.0143
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.1 -75.2 -75.1 -75.1 -48.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.5 -51.1
  .. .. .. .. .. .. ..@ area   : num 0.0136
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.4 -74.6 -74.5 -74.4 -51.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.3 -51.1
  .. .. .. .. .. .. ..@ area   : num 0.0133
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.3 -74.3 -74.2 -74.3 -51.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.4 -49.5
  .. .. .. .. .. .. ..@ area   : num 0.0131
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.3 -74.4 -74.4 -74.3 -49.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.5 -52.9
  .. .. .. .. .. .. ..@ area   : num 0.0129
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.4 -73.4 -73.8 -73.4 -52.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.2 -47.8
  .. .. .. .. .. .. ..@ area   : num 0.0128
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.1 -75.3 -75.1 -75.1 -47.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -67.7 -55.9
  .. .. .. .. .. .. ..@ area   : num 0.0121
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -67.5 -67.6 -67.9 -67.5 -55.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.7 -50.4
  .. .. .. .. .. .. ..@ area   : num 0.0117
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.7 -74.8 -74.5 -74.7 -50.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.5 -48.3
  .. .. .. .. .. .. ..@ area   : num 0.0117
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.4 -74.5 -74.6 -74.4 -48.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.3 -54.1
  .. .. .. .. .. .. ..@ area   : num 0.0114
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.2 -73.5 -73.2 -73.2 -54.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -109.4 -27.1
  .. .. .. .. .. .. ..@ area   : num 0.0114
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -109.2 -109.4 -109.4 -109.2 -27.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.8 -53.5
  .. .. .. .. .. .. ..@ area   : num 0.0108
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.8 -73.9 -73.7 -73.8 -53.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.2 -54.9
  .. .. .. .. .. .. ..@ area   : num 0.0105
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -71.1 -70.9 -71.5 -71.1 -54.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.5 -42.4
  .. .. .. .. .. .. ..@ area   : num 0.0103
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.4 -73.7 -73.5 -73.4 -42.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.8 -45.9
  .. .. .. .. .. .. ..@ area   : num 0.0102
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.7 -73.9 -73.9 -73.7 -45.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -66.9 -55.3
  .. .. .. .. .. .. ..@ area   : num 0.0102
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -66.9 -67.1 -66.9 -66.9 -55.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.6 -45.7
  .. .. .. .. .. .. ..@ area   : num 0.00988
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.6 -74.7 -74.5 -74.6 -45.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.7 -52.3
  .. .. .. .. .. .. ..@ area   : num 0.00965
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.7 -74.8 -74.6 -74.7 -52.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.4 -48.5
  .. .. .. .. .. .. ..@ area   : num 0.0095
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.4 -74.5 -74.3 -74.4 -48.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74 -51.3
  .. .. .. .. .. .. ..@ area   : num 0.00943
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74 -74.1 -74 -74 -51.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.4 -54.4
  .. .. .. .. .. .. ..@ area   : num 0.00922
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -72.3 -72.5 -72.5 -72.3 -54.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -70.3 -54.2
  .. .. .. .. .. .. ..@ area   : num 0.0092
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -70.3 -70.5 -70.2 -70.3 -54.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -66.9 -55
  .. .. .. .. .. .. ..@ area   : num 0.00915
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -66.8 -67.1 -66.9 -66.8 -55.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -66.5 -55.2
  .. .. .. .. .. .. ..@ area   : num 0.00887
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -66.6 -66.6 -66.4 -66.6 -55.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.4 -45.3
  .. .. .. .. .. .. ..@ area   : num 0.00869
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.3 -74.5 -74.4 -74.3 -45.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.4 -47.1
  .. .. .. .. .. .. ..@ area   : num 0.00831
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.5 -74.5 -74.3 -74.5 -47.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74 -44.4
  .. .. .. .. .. .. ..@ area   : num 0.00806
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.9 -74.1 -73.9 -73.9 -44.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.2 -43.9
  .. .. .. .. .. .. ..@ area   : num 0.00805
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.2 -73.3 -73.1 -73.2 -44 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.2 -51.3
  .. .. .. .. .. .. ..@ area   : num 0.008
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.1 -74.3 -74.2 -74.1 -51.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75 -51.8
  .. .. .. .. .. .. ..@ area   : num 0.00778
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.1 -75.1 -75 -75.1 -51.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.9 -48.7
  .. .. .. .. .. .. ..@ area   : num 0.00769
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75 -75 -74.8 -75 -48.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.6 -52
  .. .. .. .. .. .. ..@ area   : num 0.00758
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.7 -74.6 -74.5 -74.7 -52.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.8 -43.6
  .. .. .. .. .. .. ..@ area   : num 0.00751
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.6 -74.8 -74.9 -74.6 -43.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.9 -44.9
  .. .. .. .. .. .. ..@ area   : num 0.00745
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.8 -73.9 -73.9 -73.8 -45 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.1 -44.8
  .. .. .. .. .. .. ..@ area   : num 0.00741
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74 -74.2 -74 -74 -44.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.4 -44.8
  .. .. .. .. .. .. ..@ area   : num 0.00738
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.4 -74.5 -74.4 -74.4 -44.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.4 -44.5
  .. .. .. .. .. .. ..@ area   : num 0.00737
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.4 -74.5 -74.2 -74.4 -44.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74 -44.5
  .. .. .. .. .. .. ..@ area   : num 0.00732
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74 -74.1 -74 -74 -44.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.9 -33.6
  .. .. .. .. .. .. ..@ area   : num 0.00729
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -78.8 -79 -78.9 -78.8 -33.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.8 -46
  .. .. .. .. .. .. ..@ area   : num 0.0071
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.7 -73.8 -73.8 -73.7 -46 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.8 -51.3
  .. .. .. .. .. .. ..@ area   : num 0.00698
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.8 -73.9 -73.8 -73.8 -51.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.7 -44.8
  .. .. .. .. .. .. ..@ area   : num 0.00694
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.7 -73.7 -73.6 -73.7 -44.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75 -50.7
  .. .. .. .. .. .. ..@ area   : num 0.00689
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75 -75.1 -75 -75 -50.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.7 -52.1
  .. .. .. .. .. .. ..@ area   : num 0.00688
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -74.8 -74.8 -74.6 -74.8 -52.2 ...
  .. .. .. .. .. [list output truncated]
  .. .. .. ..@ plotOrder: int [1:148] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] -71.2 -35.1
  .. .. .. ..@ ID       : chr "CHL"
  .. .. .. ..@ area     : num 72.9
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 3
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -81.3 19.3
  .. .. .. .. .. .. ..@ area   : num 0.00845
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -81.1 -81.4 -81.3 -81.1 19.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80 19.7
  .. .. .. .. .. .. ..@ area   : num 0.00226
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -80.1 -80.1 -80 -80.1 19.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.8 19.7
  .. .. .. .. .. .. ..@ area   : num 0.00126
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79.9 -79.9 -79.7 -79.9 19.7 ...
  .. .. .. ..@ plotOrder: int [1:3] 1 2 3
  .. .. .. ..@ labpt    : num [1:2] -81.3 19.3
  .. .. .. ..@ ID       : chr "CYM"
  .. .. .. ..@ area     : num 0.012
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 2
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.73 5.64
  .. .. .. .. .. .. ..@ area   : num 38.3
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:38, 1:2] 14.6 15 15.1 15.7 14.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 9.65 3.57
  .. .. .. .. .. .. ..@ area   : num 0.000647
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 9.69 9.63 9.64 9.69 3.57 ...
  .. .. .. ..@ plotOrder: int [1:2] 1 2
  .. .. .. ..@ labpt    : num [1:2] 12.73 5.64
  .. .. .. ..@ ID       : chr "CMR"
  .. .. .. ..@ area     : num 38.3
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 18.7 15.3
  .. .. .. .. .. .. ..@ area   : num 107
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:35, 1:2] 14.5 14.1 13.6 13.5 15.5 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 18.7 15.3
  .. .. .. ..@ ID       : chr "TCD"
  .. .. .. ..@ area     : num 107
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 3
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 43.3 -11.7
  .. .. .. .. .. .. ..@ area   : num 0.0513
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 43.5 43.2 43.3 43.5 -11.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 44.4 -12.2
  .. .. .. .. .. .. ..@ area   : num 0.0412
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 44.5 44.5 44.2 44.5 -12.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 43.7 -12.3
  .. .. .. .. .. .. ..@ area   : num 0.00955
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 43.9 43.7 43.6 43.9 -12.4 ...
  .. .. .. ..@ plotOrder: int [1:3] 1 2 3
  .. .. .. ..@ labpt    : num [1:2] 43.3 -11.7
  .. .. .. ..@ ID       : chr "COM"
  .. .. .. ..@ area     : num 0.102
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 11
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.05 3.89
  .. .. .. .. .. .. ..@ area   : num 92.5
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:84, 1:2] -71.6 -71.1 -71.3 -72 -72.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.43 4.26
  .. .. .. .. .. .. ..@ area   : num 0.0124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.54 -77.43 -77.32 -77.54 4.19 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.15 2.58
  .. .. .. .. .. .. ..@ area   : num 0.0106
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -78.1 -78.2 -78.2 -78.1 -78.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.59 3.12
  .. .. .. .. .. .. ..@ area   : num 0.00671
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.57 -77.66 -77.53 -77.57 3.07 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.82 2.66
  .. .. .. .. .. .. ..@ area   : num 0.00622
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -77.8 -77.8 -77.9 -77.8 -77.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.99 2.59
  .. .. .. .. .. .. ..@ area   : num 0.00596
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -78 -78 -78 -77.9 -78 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.86 2.62
  .. .. .. .. .. .. ..@ area   : num 0.00487
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.86 -77.92 -77.79 -77.86 2.57 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.49 4.15
  .. .. .. .. .. .. ..@ area   : num 0.00354
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.45 -77.54 -77.49 -77.45 4.07 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -81.7 12.5
  .. .. .. .. .. .. ..@ area   : num 0.00112
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -81.7 -81.7 -81.7 -81.7 12.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -81.4 13.3
  .. .. .. .. .. .. ..@ area   : num 0.000733
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -81.4 -81.4 -81.4 -81.4 13.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.55 2.42
  .. .. .. .. .. .. ..@ area   : num 0.000146
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -78.5 -78.5 -78.6 -78.6 -78.5 ...
  .. .. .. ..@ plotOrder: int [1:11] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] -73.05 3.89
  .. .. .. ..@ ID       : chr "COL"
  .. .. .. ..@ area     : num 92.6
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 2
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -84.1 9.91
  .. .. .. .. .. .. ..@ area   : num 4.55
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -85.1 -83.9 -83.6 -82.6 -82.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -85.2 10.1
  .. .. .. .. .. .. ..@ area   : num 0.00302
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -85.1 -85.2 -85.2 -85.2 -85.1 ...
  .. .. .. ..@ plotOrder: int [1:2] 1 2
  .. .. .. ..@ labpt    : num [1:2] -84.1 9.91
  .. .. .. ..@ ID       : chr "CRI"
  .. .. .. ..@ area     : num 4.55
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 20.47 6.56
  .. .. .. .. .. .. ..@ area   : num 50.4
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:36, 1:2] 19.1 19 20.4 21.7 21.7 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 20.47 6.56
  .. .. .. ..@ ID       : chr "CAF"
  .. .. .. ..@ area     : num 50.4
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 26
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.9 21.6
  .. .. .. .. .. .. ..@ area   : num 8.69
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:37, 1:2] -82 -81.6 -81.5 -81.2 -81.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -82.9 21.7
  .. .. .. .. .. .. ..@ area   : num 0.141
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -82.5 -82.9 -83.2 -82.9 -83.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.5 22.5
  .. .. .. .. .. .. ..@ area   : num 0.0208
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -78.3 -78.4 -78.7 -78.3 22.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.1 22.4
  .. .. .. .. .. .. ..@ area   : num 0.0151
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -78.1 -78 -78.3 -78.1 22.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.7 22
  .. .. .. .. .. .. ..@ area   : num 0.0151
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.9 -77.7 -77.6 -77.9 22.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78 22.2
  .. .. .. .. .. .. ..@ area   : num 0.0116
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.8 -78 -78 -77.8 22.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.5 22.7
  .. .. .. .. .. .. ..@ area   : num 0.00677
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79.3 -79.6 -79.6 -79.3 22.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -81.5 21.6
  .. .. .. .. .. .. ..@ area   : num 0.0033
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -81.5 -81.6 -81.4 -81.5 21.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.2 21
  .. .. .. .. .. .. ..@ area   : num 0.00307
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79.1 -79.2 -79.2 -79.1 20.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.4 20.6
  .. .. .. .. .. .. ..@ area   : num 0.00281
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -78.3 -78.5 -78.3 -78.3 20.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.3 23
  .. .. .. .. .. .. ..@ area   : num 0.00224
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -80.2 -80.2 -80.3 -80.2 23 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.5 20.8
  .. .. .. .. .. .. ..@ area   : num 0.00214
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -75.5 -75.6 -75.5 -75.5 20.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.9 20.8
  .. .. .. .. .. .. ..@ area   : num 0.00208
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -78.8 -79 -78.9 -78.8 20.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79 20.9
  .. .. .. .. .. .. ..@ area   : num 0.00195
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79 -79.1 -79 -79 20.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79 22.7
  .. .. .. .. .. .. ..@ area   : num 0.00134
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79 -79.1 -79 -79 22.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.9 22.9
  .. .. .. .. .. .. ..@ area   : num 0.00132
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79.9 -80 -79.9 -79.9 22.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.3 21.1
  .. .. .. .. .. .. ..@ area   : num 0.00124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79.3 -79.4 -79.3 -79.3 21.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.2 23.1
  .. .. .. .. .. .. ..@ area   : num 0.00124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -80.1 -80.2 -80.2 -80.1 23.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.5 20.7
  .. .. .. .. .. .. ..@ area   : num 0.00118
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -78.4 -78.5 -78.5 -78.4 20.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.5 23.2
  .. .. .. .. .. .. ..@ area   : num 0.00107
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -80.5 -80.6 -80.6 -80.5 23.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.8 20.7
  .. .. .. .. .. .. ..@ area   : num 0.000764
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -78.8 -78.8 -78.8 -78.8 20.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.8 22.2
  .. .. .. .. .. .. ..@ area   : num 0.000703
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -77.8 -77.8 -77.9 -77.8 22.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.9 23.1
  .. .. .. .. .. .. ..@ area   : num 6e-04
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -80.9 -80.8 -81 -80.9 23.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.4 20.7
  .. .. .. .. .. .. ..@ area   : num 0.000478
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -78.4 -78.4 -78.4 -78.4 20.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.1 23
  .. .. .. .. .. .. ..@ area   : num 0.000456
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -80 -80.1 -80.1 -80 23 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.4 21.1
  .. .. .. .. .. .. ..@ area   : num 0.000402
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79.4 -79.5 -79.4 -79.4 21.1 ...
  .. .. .. ..@ plotOrder: int [1:26] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] -78.9 21.6
  .. .. .. ..@ ID       : chr "CUB"
  .. .. .. ..@ area     : num 8.93
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 6
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -23.6 15.1
  .. .. .. .. .. .. ..@ area   : num 0.0393
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -23.4 -23.7 -23.8 -23.4 15 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -25.2 17
  .. .. .. .. .. .. ..@ area   : num 0.033
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -25.3 -25.3 -25 -25.3 16.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -24.3 16.6
  .. .. .. .. .. .. ..@ area   : num 0.0293
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -24 -24.3 -24.4 -24 16.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -22.8 16.1
  .. .. .. .. .. .. ..@ area   : num 0.0226
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -22.7 -23 -22.8 -22.7 16 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -24.4 14.9
  .. .. .. .. .. .. ..@ area   : num 0.0177
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -24.4 -24.5 -24.4 -24.4 14.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -25 16.9
  .. .. .. .. .. .. ..@ area   : num 0.01
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -24.9 -25.1 -24.9 -24.9 16.8 ...
  .. .. .. ..@ plotOrder: int [1:6] 1 2 3 4 5 6
  .. .. .. ..@ labpt    : num [1:2] -23.6 15.1
  .. .. .. ..@ ID       : chr "CPV"
  .. .. .. ..@ area     : num 0.152
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 15
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -159.8 -21.2
  .. .. .. .. .. .. ..@ area   : num 0.0025
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -159.7 -159.8 -159.8 -159.7 -21.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -157.9 -21.9
  .. .. .. .. .. .. ..@ area   : num 0.00155
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -157.9 -158 -157.9 -157.9 -21.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -157.7 -19.8
  .. .. .. .. .. .. ..@ area   : num 0.00125
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -157.7 -157.7 -157.7 -157.7 -19.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -158 -20
  .. .. .. .. .. .. ..@ area   : num 0.000648
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -158 -158 -158 -158 -20 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -157.3 -20.2
  .. .. .. .. .. .. ..@ area   : num 0.000517
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -157.3 -157.3 -157.3 -157.3 -20.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -157.97 -8.97
  .. .. .. .. .. .. ..@ area   : num 0.000457
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -157.94 -157.97 -158.01 -157.94 -8.98 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -159.8 -18.9
  .. .. .. .. .. .. ..@ area   : num 0.000429
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -159.8 -159.8 -159.8 -159.8 -18.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -161 -10.4
  .. .. .. .. .. .. ..@ area   : num 0.000366
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -161 -161 -161 -161 -10.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -161 -10
  .. .. .. .. .. .. ..@ area   : num 0.000248
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -161 -161 -161 -161 -10 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -161 -10.4
  .. .. .. .. .. .. ..@ area   : num 0.000246
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -161 -161 -161 -161 -10.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -158.3 -19.8
  .. .. .. .. .. .. ..@ area   : num 0.000244
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -158.3 -158.3 -158.3 -158.3 -19.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -158.9 -19.3
  .. .. .. .. .. .. ..@ area   : num 0.000231
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -158.9 -158.9 -159 -158.9 -19.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -163.2 -18.1
  .. .. .. .. .. .. ..@ area   : num 0.000158
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -163.2 -163.2 -163.2 -163.2 -18.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -165.8 -10.9
  .. .. .. .. .. .. ..@ area   : num 0.000125
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -165.8 -165.9 -165.8 -165.8 -10.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -165.4 -11.5
  .. .. .. .. .. .. ..@ area   : num 0.000111
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -165.4 -165.4 -165.4 -165.4 -11.5 ...
  .. .. .. ..@ plotOrder: int [1:15] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] -159.8 -21.2
  .. .. .. ..@ ID       : chr "COK"
  .. .. .. ..@ area     : num 0.00909
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 33.3 35
  .. .. .. .. .. .. ..@ area   : num 0.861
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] 33.7 34.6 33.9 34.1 33 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 33.3 35
  .. .. .. ..@ ID       : chr "CYP"
  .. .. .. ..@ area     : num 0.861
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 18
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 9.26 56.02
  .. .. .. .. .. .. ..@ area   : num 3.38
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:28, 1:2] 9.97 10.31 10.31 9.87 10.96 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 11.8 55.5
  .. .. .. .. .. .. ..@ area   : num 1.06
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] 12.6 12.2 12.5 12.1 11.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 9.56 57.2
  .. .. .. .. .. .. ..@ area   : num 0.737
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] 10.43 10.34 10.01 9.24 9.12 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 10.3 55.3
  .. .. .. .. .. .. ..@ area   : num 0.406
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] 10.7 10.8 10.5 10.2 10.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 11.9 54.8
  .. .. .. .. .. .. ..@ area   : num 0.0561
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 12 12 11.7 12 54.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 11.5 54.8
  .. .. .. .. .. .. ..@ area   : num 0.045
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 11.5 11.9 11 11.5 54.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14.8 55.1
  .. .. .. .. .. .. ..@ area   : num 0.0399
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 15.1 14.7 14.7 15.1 55 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.73 56.78
  .. .. .. .. .. .. ..@ area   : num 0.033
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.92 8.77 8.51 8.92 56.92 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 10.8 54.9
  .. .. .. .. .. .. ..@ area   : num 0.027
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 10.8 10.7 10.9 10.8 54.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.3 55
  .. .. .. .. .. .. ..@ area   : num 0.0231
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 12.6 12.1 12.3 12.6 55 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 9.83 55
  .. .. .. .. .. .. ..@ area   : num 0.0198
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 9.79 10.07 9.63 9.79 55.07 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 10.6 55.9
  .. .. .. .. .. .. ..@ area   : num 0.0113
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 10.6 10.5 10.5 10.6 55.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 11 57.3
  .. .. .. .. .. .. ..@ area   : num 0.0102
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 11.2 11 10.9 11.2 57.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.6 55.6
  .. .. .. .. .. .. ..@ area   : num 0.00665
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 12.6 12.6 12.7 12.6 55.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 10.6 55
  .. .. .. .. .. .. ..@ area   : num 0.00589
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 10.6 10.5 10.6 10.6 55 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.43 55.41
  .. .. .. .. .. .. ..@ area   : num 0.00444
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.46 8.36 8.46 8.46 55.33 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 10.3 54.9
  .. .. .. .. .. .. ..@ area   : num 0.0044
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 10.4 10.2 10.2 10.4 54.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 11.6 56.7
  .. .. .. .. .. .. ..@ area   : num 0.00316
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 11.6 11.5 11.6 11.6 56.7 ...
  .. .. .. ..@ plotOrder: int [1:18] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 9.26 56.02
  .. .. .. ..@ ID       : chr "DNK"
  .. .. .. ..@ area     : num 5.87
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 42.6 11.8
  .. .. .. .. .. .. ..@ area   : num 1.79
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] 42.9 43.2 42.9 41.8 41.8 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 42.6 11.8
  .. .. .. ..@ ID       : chr "DJI"
  .. .. .. ..@ area     : num 1.79
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.4 15.4
  .. .. .. .. .. .. ..@ area   : num 0.0356
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -61.4 -61.5 -61.3 -61.4 15.2 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -61.4 15.4
  .. .. .. ..@ ID       : chr "DMA"
  .. .. .. ..@ area     : num 0.0356
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 3
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -70.5 18.9
  .. .. .. .. .. .. ..@ area   : num 4.02
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] -70.8 -69.9 -69.8 -69.2 -69.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -68.7 18.1
  .. .. .. .. .. .. ..@ area   : num 0.00615
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -68.6 -68.7 -68.8 -68.6 18.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.5 17.6
  .. .. .. .. .. .. ..@ area   : num 0.00239
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -71.5 -71.5 -71.5 -71.5 17.5 ...
  .. .. .. ..@ plotOrder: int [1:3] 1 2 3
  .. .. .. ..@ labpt    : num [1:2] -70.5 18.9
  .. .. .. ..@ ID       : chr "DOM"
  .. .. .. ..@ area     : num 4.03
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 14
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.4 -1.45
  .. .. .. .. .. .. ..@ area   : num 20.3
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] -78.6 -77.4 -76.2 -75.3 -75.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -91.167 -0.445
  .. .. .. .. .. .. ..@ area   : num 0.259
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -91.2 -90.8 -91.4 -91.1 -91.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.1 -2.83
  .. .. .. .. .. .. ..@ area   : num 0.0472
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -80.19 -80.21 -79.9 -80.19 -3.03 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.329 -0.626
  .. .. .. .. .. .. ..@ area   : num 0.0345
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -90.261 -90.535 -90.191 -90.261 -0.751 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -89.44 -0.85
  .. .. .. .. .. .. ..@ area   : num 0.024
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -89.444 -89.629 -89.259 -89.444 -0.937 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -91.545 -0.353
  .. .. .. .. .. .. ..@ area   : num 0.023
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -91.499 -91.664 -91.471 -91.499 -0.496 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.738 -0.241
  .. .. .. .. .. .. ..@ area   : num 0.0211
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -90.548 -90.874 -90.793 -90.548 -0.305 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.43 -1.28
  .. .. .. .. .. .. ..@ area   : num 0.0067
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -90.43 -90.48 -90.37 -90.43 -1.36 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.94 1.29
  .. .. .. .. .. .. ..@ area   : num 0.00573
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -78.91 -78.9 -79 -78.91 1.24 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.467 0.313
  .. .. .. .. .. .. ..@ area   : num 0.00426
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -90.459 -90.533 -90.408 -90.459 0.266 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.86 -2.58
  .. .. .. .. .. .. ..@ area   : num 0.0041
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -79.84 -79.89 -79.86 -79.84 -2.64 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -89.67 -1.37
  .. .. .. .. .. .. ..@ area   : num 0.00348
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -89.62 -89.75 -89.65 -89.62 -1.41 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.781 0.588
  .. .. .. .. .. .. ..@ area   : num 0.00221
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -90.751 -90.8 -90.792 -90.751 0.548 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.055 -0.816
  .. .. .. .. .. .. ..@ area   : num 0.000641
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -90.043 -90.082 -90.04 -90.043 -0.839 ...
  .. .. .. ..@ plotOrder: int [1:14] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] -78.4 -1.45
  .. .. .. ..@ ID       : chr "ECU"
  .. .. .. ..@ area     : num 20.8
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 6
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 29.9 26.5
  .. .. .. .. .. .. ..@ area   : num 90.8
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:38, 1:2] 25.3 27.3 29.1 31 31.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 34 27.5
  .. .. .. .. .. .. ..@ area   : num 0.00284
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 34 34 33.9 34 27.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 33.1 31.2
  .. .. .. .. .. .. ..@ area   : num 0.00177
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 33.2 33.3 33.1 33 33.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 33.4 31.2
  .. .. .. .. .. .. ..@ area   : num 0.00168
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 33.5 33.3 33.4 33.5 31.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 34 26.7
  .. .. .. .. .. .. ..@ area   : num 0.000609
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 34 34 34 34 26.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 32 31.2
  .. .. .. .. .. .. ..@ area   : num 4.27e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 32 32 32 32 31.1 ...
  .. .. .. ..@ plotOrder: int [1:6] 1 2 3 4 5 6
  .. .. .. ..@ labpt    : num [1:2] 29.9 26.5
  .. .. .. ..@ ID       : chr "EGY"
  .. .. .. ..@ area     : num 90.8
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 5
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -8.08 53.21
  .. .. .. .. .. .. ..@ area   : num 9.2
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:37, 1:2] -7.41 -8.16 -7.56 -7.03 -6.27 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -10.1 54
  .. .. .. .. .. .. ..@ area   : num 0.0218
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -9.96 -9.96 -10.27 -9.96 54.02 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -9.68 53.26
  .. .. .. .. .. .. ..@ area   : num 0.00187
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -9.66 -9.72 -9.66 -9.66 53.22 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -8.52 54.99
  .. .. .. .. .. .. ..@ area   : num 0.0011
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -8.52 -8.55 -8.49 -8.52 54.96 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -8.44 54.95
  .. .. .. .. .. .. ..@ area   : num 8.3e-05
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -8.44 -8.44 -8.45 -8.44 54.94 ...
  .. .. .. ..@ plotOrder: int [1:5] 1 2 3 4 5
  .. .. .. ..@ labpt    : num [1:2] -8.08 53.21
  .. .. .. ..@ ID       : chr "IRL"
  .. .. .. ..@ area     : num 9.22
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 3
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 10.44 1.58
  .. .. .. .. .. .. ..@ area   : num 2.11
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] 10.03 11.34 11.35 9.8 9.36 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.71 3.55
  .. .. .. .. .. .. ..@ area   : num 0.0987
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 8.86 8.45 8.69 8.96 8.86 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 5.63 -1.45
  .. .. .. .. .. .. ..@ area   : num 0.000686
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 5.64 5.62 5.63 5.64 -1.47 ...
  .. .. .. ..@ plotOrder: int [1:3] 1 2 3
  .. .. .. ..@ labpt    : num [1:2] 10.44 1.58
  .. .. .. ..@ ID       : chr "GNQ"
  .. .. .. ..@ area     : num 2.21
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 6
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.9 58.7
  .. .. .. .. .. .. ..@ area   : num 6.67
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] 25.8 28 28.2 27.4 27.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22.5 58.4
  .. .. .. .. .. .. ..@ area   : num 0.46
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 23 23.3 22 22.2 21.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22.6 58.9
  .. .. .. .. .. .. ..@ area   : num 0.0667
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 22.7 23 22 22.7 59 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.2 58.6
  .. .. .. .. .. .. ..@ area   : num 0.0171
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.4 23.1 23.2 23.4 58.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.3 59
  .. .. .. .. .. .. ..@ area   : num 0.00644
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.3 23.1 23.4 23.3 59 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24 58.1
  .. .. .. .. .. .. ..@ area   : num 0.00166
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24 23.9 24 24 58.1 ...
  .. .. .. ..@ plotOrder: int [1:6] 1 2 3 4 5 6
  .. .. .. ..@ labpt    : num [1:2] 25.9 58.7
  .. .. .. ..@ ID       : chr "EST"
  .. .. .. ..@ area     : num 7.23
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 3
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 38.9 15.3
  .. .. .. .. .. .. ..@ area   : num 10.4
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:20, 1:2] 38.8 39.7 39.9 40.2 41.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 40.2 15.7
  .. .. .. .. .. .. ..@ area   : num 0.0562
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 40.1 40.4 40 40.1 15.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 40 16.1
  .. .. .. .. .. .. ..@ area   : num 0.00445
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 40.1 40 40 40.1 16.1 ...
  .. .. .. ..@ plotOrder: int [1:3] 1 2 3
  .. .. .. ..@ labpt    : num [1:2] 38.9 15.3
  .. .. .. ..@ ID       : chr "ERI"
  .. .. .. ..@ area     : num 10.4
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 2
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -88.9 13.7
  .. .. .. .. .. .. ..@ area   : num 1.52
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -89.3 -88.5 -87.8 -87.8 -87.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -87.7 13.2
  .. .. .. .. .. .. ..@ area   : num 0.000823
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -87.7 -87.7 -87.7 -87.7 13.2 ...
  .. .. .. ..@ plotOrder: int [1:2] 1 2
  .. .. .. ..@ labpt    : num [1:2] -88.9 13.7
  .. .. .. ..@ ID       : chr "SLV"
  .. .. .. ..@ area     : num 1.52
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 39.63 8.61
  .. .. .. .. .. .. ..@ area   : num 92.8
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:39, 1:2] 36.5 37.3 37.6 37.9 38.4 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 39.63 8.61
  .. .. .. ..@ ID       : chr "ETH"
  .. .. .. ..@ area     : num 92.8
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14.2 47.6
  .. .. .. .. .. .. ..@ area   : num 10.2
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:25, 1:2] 13.8 14.7 15 16.9 17.2 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 14.2 47.6
  .. .. .. ..@ ID       : chr "AUT"
  .. .. .. ..@ area     : num 10.2
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 15.3 49.8
  .. .. .. .. .. .. ..@ area   : num 10.2
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] 14.7 13.8 12.7 12.1 14.3 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 15.3 49.8
  .. .. .. ..@ ID       : chr "CZE"
  .. .. .. ..@ area     : num 10.2
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.25 3.89
  .. .. .. .. .. .. ..@ area   : num 6.55
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -53.5 -52.3 -52 -51.9 -51.7 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -53.25 3.89
  .. .. .. ..@ ID       : chr "GUF"
  .. .. .. ..@ area     : num 6.55
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 26
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 26.2 64.5
  .. .. .. .. .. .. ..@ area   : num 61.3
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:60, 1:2] 28.2 29.2 28.8 29 28.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22.6 60.1
  .. .. .. .. .. .. ..@ area   : num 0.0374
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 22.8 22.5 22.4 22.6 22.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.8 65
  .. .. .. .. .. .. ..@ area   : num 0.0205
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.7 24.6 25 24.7 64.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.3 63.2
  .. .. .. .. .. .. ..@ area   : num 0.0167
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 21.1 21.4 21.2 21.1 63.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.9 60.4
  .. .. .. .. .. .. ..@ area   : num 0.00939
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 22 21.8 21.8 22 60.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22.8 63.8
  .. .. .. .. .. .. ..@ area   : num 0.00797
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 22.9 22.7 22.8 22.9 63.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.5 59.9
  .. .. .. .. .. .. ..@ area   : num 0.00688
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.7 23.5 23.4 23.7 59.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22.9 59.9
  .. .. .. .. .. .. ..@ area   : num 0.00671
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 22.8 23.1 22.9 22.8 60 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.8 60.2
  .. .. .. .. .. .. ..@ area   : num 0.00598
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 21.7 21.7 21.9 21.7 60.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22.2 60.3
  .. .. .. .. .. .. ..@ area   : num 0.00516
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 22.3 22.1 22.1 22.3 60.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.4 60.5
  .. .. .. .. .. .. ..@ area   : num 0.00466
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 21.5 21.4 21.3 21.5 60.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22.1 63.3
  .. .. .. .. .. .. ..@ area   : num 0.00421
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 22.2 22 22.2 22.2 63.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.6 60.1
  .. .. .. .. .. .. ..@ area   : num 0.0037
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 21.6 21.5 21.7 21.6 60.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22 60.2
  .. .. .. .. .. .. ..@ area   : num 0.00357
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 22 21.9 22.1 22 60.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.3 63.3
  .. .. .. .. .. .. ..@ area   : num 0.00343
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 21.3 21.2 21.4 21.3 63.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22.4 60
  .. .. .. .. .. .. ..@ area   : num 0.00339
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 22.4 22.4 22.4 22.4 60 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22.4 60.3
  .. .. .. .. .. .. ..@ area   : num 0.0032
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 22.4 22.3 22.5 22.4 60.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.3 60.9
  .. .. .. .. .. .. ..@ area   : num 0.00285
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 21.3 21.3 21.4 21.3 60.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.6 60.2
  .. .. .. .. .. .. ..@ area   : num 0.0028
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.7 25.6 25.6 25.7 60.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.3 60.5
  .. .. .. .. .. .. ..@ area   : num 0.00205
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 21.3 21.3 21.2 21.3 60.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.3 60.6
  .. .. .. .. .. .. ..@ area   : num 0.00194
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 21.3 21.3 21.2 21.3 60.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22.3 60.2
  .. .. .. .. .. .. ..@ area   : num 0.0018
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 22.2 22.2 22.3 22.2 60.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.7 60.5
  .. .. .. .. .. .. ..@ area   : num 0.00162
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 21.8 21.7 21.7 21.8 21.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.4 60.2
  .. .. .. .. .. .. ..@ area   : num 0.00146
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 21.4 21.4 21.3 21.4 60.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.8 60.2
  .. .. .. .. .. .. ..@ area   : num 0.00041
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.8 25.8 25.9 25.8 60.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.5 65.8
  .. .. .. .. .. .. ..@ area   : num 0.00033
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.5 24.4 24.5 24.5 65.8 ...
  .. .. .. ..@ plotOrder: int [1:26] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 26.2 64.5
  .. .. .. ..@ ID       : chr "FIN"
  .. .. .. ..@ area     : num 61.5
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 37
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 178 -17.8
  .. .. .. .. .. .. ..@ area   : num 0.674
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 178 179 177 178 178 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 179.3 -16.6
  .. .. .. .. .. .. ..@ area   : num 0.393
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] 180 179 180 180 180 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 178 -19
  .. .. .. .. .. .. ..@ area   : num 0.0284
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 178 178 178 178 -19 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -179.9 -16.8
  .. .. .. .. .. .. ..@ area   : num 0.0122
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -180 -180 -180 -180 -17 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 179.9 -16.9
  .. .. .. .. .. .. ..@ area   : num 0.00802
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 180 180 180 180 -17 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 179 -18
  .. .. .. .. .. .. ..@ area   : num 0.0062
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 179.4 179.2 179.3 179.4 -18.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -179 -17.2
  .. .. .. .. .. .. ..@ area   : num 0.00604
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -178.9 -179 -179 -178.9 -17.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 179.9 -18.6
  .. .. .. .. .. .. ..@ area   : num 0.00549
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 179.9 179.8 180 179.9 -18.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 179.4 -17.3
  .. .. .. .. .. .. ..@ area   : num 0.00548
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 179.4 179.4 179.4 179.4 -17.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 178.8 -17.7
  .. .. .. .. .. .. ..@ area   : num 0.00408
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 178.8 178.7 178.8 178.8 -17.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 177.5 -16.7
  .. .. .. .. .. .. ..@ area   : num 0.00325
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 177.4 177.6 177.6 177.4 -16.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -178.3 -17.9
  .. .. .. .. .. .. ..@ area   : num 0.003
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -178 -178 -178 -178 -18 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -180 -16.5
  .. .. .. .. .. .. ..@ area   : num 0.00297
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -180 -180 -179.9 -180 -16.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -180 -19
  .. .. .. .. .. .. ..@ area   : num 0.00283
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -179.8 -179.9 -179.8 -179.8 -18.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 177.2 -17.1
  .. .. .. .. .. .. ..@ area   : num 0.00275
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 177.3 177.2 177.3 177.3 -17.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -178.8 -18.2
  .. .. .. .. .. .. ..@ area   : num 0.00214
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -178.8 -178.8 -178.7 -178.8 -18.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -179 -18
  .. .. .. .. .. .. ..@ area   : num 0.00191
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -179 -179 -179 -179 -18 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 179.9 -16.6
  .. .. .. .. .. .. ..@ area   : num 0.00141
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 180 180 180 180 180 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 179.8 -19.2
  .. .. .. .. .. .. ..@ area   : num 0.0014
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 179.8 179.7 179.8 179.8 -19.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -178.6 -19.2
  .. .. .. .. .. .. ..@ area   : num 0.00138
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -178.5 -178.6 -178.6 -178.5 -19.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 177.1 -12.5
  .. .. .. .. .. .. ..@ area   : num 0.00137
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 177.1 177 177.1 177.1 -12.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 177.6 -18.5
  .. .. .. .. .. .. ..@ area   : num 0.00133
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 177.7 177.6 177.6 177.7 -18.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 178.5 -18.9
  .. .. .. .. .. .. ..@ area   : num 0.00123
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 178.5 178.5 178.5 178.5 -18.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 178.3 -16.8
  .. .. .. .. .. .. ..@ area   : num 0.00112
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 178.3 178.3 178.3 178.3 -16.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 177.1 -17.3
  .. .. .. .. .. .. ..@ area   : num 0.00105
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 177.1 177.1 177.1 177.1 -17.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -179.3 -17.8
  .. .. .. .. .. .. ..@ area   : num 0.00093
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -179.3 -179.3 -179.3 -179.3 -17.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -179 -19
  .. .. .. .. .. .. ..@ area   : num 9e-04
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -179 -179 -179 -179 -19 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -179.2 -17.4
  .. .. .. .. .. .. ..@ area   : num 0.000816
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -179.1 -179.2 -179.1 -179.1 -17.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 178.1 -18.4
  .. .. .. .. .. .. ..@ area   : num 0.000799
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 178.1 178.1 178.1 178.1 -18.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -180 -16.2
  .. .. .. .. .. .. ..@ area   : num 0.00079
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -180 -180 -179.9 -180 -16.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -178.4 -19.2
  .. .. .. .. .. .. ..@ area   : num 0.000784
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -178.4 -178.4 -178.4 -178.4 -19.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 179.4 -17.8
  .. .. .. .. .. .. ..@ area   : num 0.000719
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 179.4 179.4 179.4 179.4 -17.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -178.5 -18.7
  .. .. .. .. .. .. ..@ area   : num 0.000602
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -178.5 -178.5 -178.5 -178.5 -18.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -179.1 -17.3
  .. .. .. .. .. .. ..@ area   : num 0.000545
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -179.1 -179.2 -179.1 -179.1 -17.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 180 -16.5
  .. .. .. .. .. .. ..@ area   : num 0.000413
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 180 180 180 180 -16.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -178.7 -20.7
  .. .. .. .. .. .. ..@ area   : num 0.000322
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -178.7 -178.7 -178.7 -178.7 -20.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -178.2 -19.8
  .. .. .. .. .. .. ..@ area   : num 0.00016
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -178.2 -178.2 -178.2 -178.2 -19.9 ...
  .. .. .. ..@ plotOrder: int [1:37] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 178 -17.8
  .. .. .. ..@ ID       : chr "FJI"
  .. .. .. ..@ area     : num 1.18
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 10
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.8 -51.8
  .. .. .. .. .. .. ..@ area   : num 0.613
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] -58.7 -58.2 -58.3 -57.9 -57.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.1 -51.7
  .. .. .. .. .. .. ..@ area   : num 0.528
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -59.2 -60.4 -61 -60.2 -60.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.2 -51.3
  .. .. .. .. .. .. ..@ area   : num 0.0121
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -60.1 -60.3 -60.1 -60.1 -51.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -59.6 -51.3
  .. .. .. .. .. .. ..@ area   : num 0.012
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -59.5 -59.8 -59.5 -59.5 -51.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -59.7 -52.2
  .. .. .. .. .. .. ..@ area   : num 0.00592
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -59.7 -59.8 -59.8 -59.7 -59.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.5 -52
  .. .. .. .. .. .. ..@ area   : num 0.00521
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -58.4 -58.5 -58.4 -58.4 -52.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61 -51.8
  .. .. .. .. .. .. ..@ area   : num 0.00518
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -61 -60.9 -61.1 -60.9 -61 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.3 -51.8
  .. .. .. .. .. .. ..@ area   : num 0.00321
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -61.2 -61.3 -61.2 -61.2 -51.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60 -51.3
  .. .. .. .. .. .. ..@ area   : num 0.00292
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -59.9 -60 -59.9 -59.9 -51.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.3 -51.7
  .. .. .. .. .. .. ..@ area   : num 0.00225
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -61.2 -61.3 -61.3 -61.2 -51.7 ...
  .. .. .. ..@ plotOrder: int [1:10] 1 2 3 4 5 6 7 8 9 10
  .. .. .. ..@ labpt    : num [1:2] -58.8 -51.8
  .. .. .. ..@ ID       : chr "FLK"
  .. .. .. ..@ area     : num 1.19
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 19
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 158.25 6.88
  .. .. .. .. .. .. ..@ area   : num 0.0143
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 158.3 158.12 158.32 158.3 6.79 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 138.1 9.5
  .. .. .. .. .. .. ..@ area   : num 0.00768
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 138.21 138.06 138.13 138.21 9.52 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 162.98 5.32
  .. .. .. .. .. .. ..@ area   : num 0.00615
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 163.01 162.9 163.02 163.01 5.26 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 151.6 7.35
  .. .. .. .. .. .. ..@ area   : num 0.00225
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 151.64 151.57 151.61 151.64 7.33 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 151.88 7.44
  .. .. .. .. .. .. ..@ area   : num 0.00101
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 151.91 151.85 151.87 151.91 7.44 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 151.86 7.35
  .. .. .. .. .. .. ..@ area   : num 0.000569
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 151.86 151.84 151.87 151.86 7.32 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 149.67 8.59
  .. .. .. .. .. .. ..@ area   : num 0.000429
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 149.67 149.66 149.69 149.67 8.58 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 153.7 5.3
  .. .. .. .. .. .. ..@ area   : num 0.000324
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 153.67 153.66 153.71 153.67 5.29 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 158.25 6.99
  .. .. .. .. .. .. ..@ area   : num 0.000133
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 158.25 158.24 158.26 158.25 6.98 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 157.94 6.72
  .. .. .. .. .. .. ..@ area   : num 9.85e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 157.94 157.93 157.95 157.94 6.72 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 149.32 6.71
  .. .. .. .. .. .. ..@ area   : num 9.2e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 149.3 149.3 149.3 149.3 6.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 157.98 6.74
  .. .. .. .. .. .. ..@ area   : num 9.08e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 157.97 157.96 157.99 157.97 6.74 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 150.13 8.98
  .. .. .. .. .. .. ..@ area   : num 8.69e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 150.13 150.12 150.13 150.13 8.98 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 146.19 7.51
  .. .. .. .. .. .. ..@ area   : num 5.72e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 146.2 146.2 146.2 146.2 7.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 143.06 6.71
  .. .. .. .. .. .. ..@ area   : num 5.45e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 143.06 143.05 143.06 143.06 6.71 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 149.19 7.38
  .. .. .. .. .. .. ..@ area   : num 4.88e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 149.19 149.18 149.19 149.19 7.38 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 150.37 8.62
  .. .. .. .. .. .. ..@ area   : num 4.79e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 150.38 150.37 150.37 150.38 8.62 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 149.2 7.37
  .. .. .. .. .. .. ..@ area   : num 4.39e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 149.2 149.19 149.2 149.2 7.36 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 143.92 7.38
  .. .. .. .. .. .. ..@ area   : num 4.01e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 143.92 143.91 143.91 143.92 7.38 ...
  .. .. .. ..@ plotOrder: int [1:19] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 158.25 6.88
  .. .. .. ..@ ID       : chr "FSM"
  .. .. .. ..@ area     : num 0.0335
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 32
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -149.3 -17.7
  .. .. .. .. .. .. ..@ area   : num 0.0259
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -149.2 -149.2 -149.6 -149.2 -17.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -140.16 -8.89
  .. .. .. .. .. .. ..@ area   : num 0.0139
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -140 -140.2 -140.2 -140 -8.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -149.9 -17.5
  .. .. .. .. .. .. ..@ area   : num 0.00742
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -149.8 -149.9 -149.8 -149.8 -17.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -151.5 -16.8
  .. .. .. .. .. .. ..@ area   : num 0.00591
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -151.4 -151.5 -151.5 -151.4 -16.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -140.1 -9.39
  .. .. .. .. .. .. ..@ area   : num 0.00413
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -140.08 -140.15 -140.09 -140.08 -9.45 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -138.7 -10.5
  .. .. .. .. .. .. ..@ area   : num 0.00391
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -138.7 -138.7 -138.6 -138.7 -10.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -139.57 -8.92
  .. .. .. .. .. .. ..@ area   : num 0.00388
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -139.58 -139.5 -139.61 -139.58 -8.87 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -151.5 -16.6
  .. .. .. .. .. .. ..@ area   : num 0.00346
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -151.5 -151.5 -151.4 -151.5 -16.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -138.98 -9.76
  .. .. .. .. .. .. ..@ area   : num 0.00292
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -138.96 -138.81 -139.17 -138.96 -9.74 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -140.68 -7.99
  .. .. .. .. .. .. ..@ area   : num 0.00284
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -140.7 -140.71 -140.64 -140.7 -8.04 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -139.09 -9.93
  .. .. .. .. .. .. ..@ area   : num 0.00273
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -139.11 -139.13 -139.05 -139.11 -9.98 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -138.8 -22.2
  .. .. .. .. .. .. ..@ area   : num 0.00201
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -138.7 -138.8 -138.7 -138.7 -22.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -140.6 -21.7
  .. .. .. .. .. .. ..@ area   : num 0.00156
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -140.6 -140.7 -140.7 -140.6 -21.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -144.3 -27.6
  .. .. .. .. .. .. ..@ area   : num 0.00154
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -144.3 -144.3 -144.3 -144.3 -27.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -151 -16.8
  .. .. .. .. .. .. ..@ area   : num 0.00136
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -151 -151 -151 -151 -16.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -151.4 -22.5
  .. .. .. .. .. .. ..@ area   : num 0.00131
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -151.4 -151.4 -151.4 -151.4 -22.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -146.4 -16.1
  .. .. .. .. .. .. ..@ area   : num 0.00128
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -146.3 -146.4 -146.3 -146.3 -16.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -149.5 -23.4
  .. .. .. .. .. .. ..@ area   : num 0.00121
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -149.5 -149.5 -149.5 -149.5 -23.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135 -23.1
  .. .. .. .. .. .. ..@ area   : num 0.0012
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -135 -135 -134.9 -135 -23.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -151 -16.7
  .. .. .. .. .. .. ..@ area   : num 0.00118
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -151 -151 -151 -151 -16.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -151.8 -16.5
  .. .. .. .. .. .. ..@ area   : num 0.000789
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -151.7 -151.8 -151.7 -151.7 -16.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -147.7 -23.9
  .. .. .. .. .. .. ..@ area   : num 0.00077
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -147.7 -147.7 -147.6 -147.7 -23.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -152.8 -22.6
  .. .. .. .. .. .. ..@ area   : num 0.000703
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -152.9 -152.8 -152.8 -152.9 -22.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -148.3 -15.8
  .. .. .. .. .. .. ..@ area   : num 0.000678
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -148.2 -148.3 -148.3 -148.2 -15.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -140.58 -7.91
  .. .. .. .. .. .. ..@ area   : num 0.000489
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -140.58 -140.61 -140.56 -140.58 -7.92 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -144.6 -15.7
  .. .. .. .. .. .. ..@ area   : num 0.000439
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -144.6 -144.6 -144.6 -144.6 -15.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -150.7 -17.7
  .. .. .. .. .. .. ..@ area   : num 0.000238
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -150.6 -150.7 -150.6 -150.6 -17.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -150 -17
  .. .. .. .. .. .. ..@ area   : num 0.000205
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -150 -150 -150 -150 -17 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135 -23.2
  .. .. .. .. .. .. ..@ area   : num 0.000193
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -135.1 -135.1 -135 -135.1 -23.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -143.5 -27.9
  .. .. .. .. .. .. ..@ area   : num 0.00017
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -143.5 -143.5 -143.5 -143.5 -27.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -145 -19.9
  .. .. .. .. .. .. ..@ area   : num 0.000131
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -145 -145 -145 -145 -19.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -150 -17
  .. .. .. .. .. .. ..@ area   : num 5.73e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -150 -150 -150 -150 -17 ...
  .. .. .. ..@ plotOrder: int [1:32] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] -149.3 -17.7
  .. .. .. ..@ ID       : chr "PYF"
  .. .. .. ..@ area     : num 0.0946
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 10
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 2.46 46.65
  .. .. .. .. .. .. ..@ area   : num 63.7
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:69, 1:2] 2.54 4.17 4.15 4.83 4.87 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 9.1 42.1
  .. .. .. .. .. .. ..@ area   : num 0.865
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] 9.45 9.55 9.18 8.79 8.58 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -1.28 45.95
  .. .. .. .. .. .. ..@ area   : num 0.0103
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -1.19 -1.4 -1.23 -1.19 45.82 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -3.18 47.33
  .. .. .. .. .. .. ..@ area   : num 0.00558
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -3.07 -3.22 -3.26 -3.07 47.31 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -1.43 46.22
  .. .. .. .. .. .. ..@ area   : num 0.00507
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -1.26 -1.55 -1.47 -1.26 46.16 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -2.21 46.98
  .. .. .. .. .. .. ..@ area   : num 0.0048
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -2.2 -2.14 -2.28 -2.2 47.02 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -2.34 46.71
  .. .. .. .. .. .. ..@ area   : num 0.00123
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -2.27 -2.36 -2.38 -2.27 46.69 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -5.08 48.47
  .. .. .. .. .. .. ..@ area   : num 0.00113
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -5.05 -5.13 -5.06 -5.05 48.45 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -3.57 48.81
  .. .. .. .. .. .. ..@ area   : num 5.47e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -3.58 -3.58 -3.56 -3.58 48.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 9.48 42.62
  .. .. .. .. .. .. ..@ area   : num 3.9e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 9.49 9.49 9.46 9.49 42.62 ...
  .. .. .. ..@ plotOrder: int [1:10] 1 2 3 4 5 6 7 8 9 10
  .. .. .. ..@ labpt    : num [1:2] 2.46 46.65
  .. .. .. ..@ ID       : chr "FRA"
  .. .. .. ..@ area     : num 64.6
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -15.5 13.5
  .. .. .. .. .. .. ..@ area   : num 0.995
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] -16.7 -16.2 -16.2 -15.3 -16.1 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -15.5 13.5
  .. .. .. ..@ ID       : chr "GMB"
  .. .. .. ..@ area     : num 0.995
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 3
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 11.811 -0.615
  .. .. .. .. .. .. ..@ area   : num 21.2
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:35, 1:2] 12.5 13.3 13.2 14.2 14.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.987 -0.675
  .. .. .. .. .. .. ..@ area   : num 0.0051
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 9.008 8.947 9.007 9.008 -0.768 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 9.546 0.277
  .. .. .. .. .. .. ..@ area   : num 0.000105
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 9.554 9.544 9.538 9.554 0.274 ...
  .. .. .. ..@ plotOrder: int [1:3] 1 2 3
  .. .. .. ..@ labpt    : num [1:2] 11.811 -0.615
  .. .. .. ..@ ID       : chr "GAB"
  .. .. .. ..@ area     : num 21.2
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 43.5 42.2
  .. .. .. .. .. .. ..@ area   : num 7.65
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] 41.5 41.4 40 40.3 42.8 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 43.5 42.2
  .. .. .. ..@ ID       : chr "GEO"
  .. .. .. ..@ area     : num 7.65
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -1.18 7.93
  .. .. .. .. .. .. ..@ area   : num 19.3
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] 0.64 0.662 0.256 -0.796 -1.613 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -1.18 7.93
  .. .. .. ..@ ID       : chr "GHA"
  .. .. .. ..@ area     : num 19.3
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 3
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.7 12.1
  .. .. .. .. .. .. ..@ area   : num 0.0104
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -61.7 -61.6 -61.6 -61.7 12 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.5 12.5
  .. .. .. .. .. .. ..@ area   : num 0.00262
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -61.4 -61.5 -61.4 -61.4 12.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.6 12.3
  .. .. .. .. .. .. ..@ area   : num 0.000127
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -61.6 -61.6 -61.6 -61.6 12.3 ...
  .. .. .. ..@ plotOrder: int [1:3] 1 2 3
  .. .. .. ..@ labpt    : num [1:2] -61.7 12.1
  .. .. .. ..@ ID       : chr "GRD"
  .. .. .. ..@ area     : num 0.0132
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 194
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -41.5 74.7
  .. .. .. .. .. .. ..@ area   : num 638
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:576, 1:2] -32.3 -25.7 -35.6 -25.1 -24.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.5 69.8
  .. .. .. .. .. .. ..@ area   : num 1.54
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -52.7 -51.8 -53.6 -54.3 -53.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -26.6 70.7
  .. .. .. .. .. .. ..@ area   : num 0.84
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -25.4 -25.3 -26 -28.1 -27.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -45.9 82.4
  .. .. .. .. .. .. ..@ area   : num 0.836
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -46 -44.4 -45.1 -44.7 -45.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -23.2 72.5
  .. .. .. .. .. .. ..@ area   : num 0.622
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -23.6 -21.9 -22.8 -22.1 -22.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -24.5 73.3
  .. .. .. .. .. .. ..@ area   : num 0.384
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -24.4 -23.2 -25 -22.9 -25.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -23 72.9
  .. .. .. .. .. .. ..@ area   : num 0.316
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -22.5 -21.9 -24.6 -22.5 73 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -18.4 75.2
  .. .. .. .. .. .. ..@ area   : num 0.283
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -17.8 -18.2 -17.3 -18.9 -18.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -21 74.3
  .. .. .. .. .. .. ..@ area   : num 0.194
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -20.9 -20.1 -22 -20.9 74.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -18.8 76.4
  .. .. .. .. .. .. ..@ area   : num 0.186
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -18.7 -18.6 -19.1 -18.8 -19 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -20.3 74.9
  .. .. .. .. .. .. ..@ area   : num 0.173
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -20.2 -19.7 -20.1 -20.7 -20.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51 69.7
  .. .. .. .. .. .. ..@ area   : num 0.123
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -50.9 -50.7 -51 -51.4 -50.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.7 71.2
  .. .. .. .. .. .. ..@ area   : num 0.0892
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -53.6 -54 -53.6 -53.4 -53.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -19.6 80.2
  .. .. .. .. .. .. ..@ area   : num 0.0856
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -19.9 -20 -19.8 -19 -19.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -19.9 77.9
  .. .. .. .. .. .. ..@ area   : num 0.0756
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -19.9 -19.2 -19.8 -20.5 -19.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -39.7 83.2
  .. .. .. .. .. .. ..@ area   : num 0.0661
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -39.6 -38.6 -40.7 -39.6 83.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -39.8 83.1
  .. .. .. .. .. .. ..@ area   : num 0.0643
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -39.6 -40.5 -39.3 -39.6 83 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -20.3 82
  .. .. .. .. .. .. ..@ area   : num 0.061
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -19.8 -20.8 -20.3 -19.8 81.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -52 82.1
  .. .. .. .. .. .. ..@ area   : num 0.0594
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -51.5 -53.4 -51.2 -51.5 82 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.1 64.4
  .. .. .. .. .. .. ..@ area   : num 0.0565
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -51.2 -51.3 -51 -50.8 -51.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -37.6 65.7
  .. .. .. .. .. .. ..@ area   : num 0.0562
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -37.4 -37.5 -38 -37.4 65.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72 77.4
  .. .. .. .. .. .. ..@ area   : num 0.0552
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -71.9 -72.6 -71.3 -71.9 77.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -40.5 64.7
  .. .. .. .. .. .. ..@ area   : num 0.0548
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -40.2 -40.9 -40.5 -40.2 64.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -40.7 83.1
  .. .. .. .. .. .. ..@ area   : num 0.0525
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -39.9 -41.5 -40.8 -39.9 83 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.4 72.2
  .. .. .. .. .. .. ..@ area   : num 0.0412
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -55.4 -55.7 -55 -55.4 72.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -52.6 68.7
  .. .. .. .. .. .. ..@ area   : num 0.0387
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -52.1 -53.1 -52.6 -52.1 68.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -17.8 79.1
  .. .. .. .. .. .. ..@ area   : num 0.0375
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -18 -17.8 -17.6 -18 79 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -41.5 63.3
  .. .. .. .. .. .. ..@ area   : num 0.0371
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -41.1 -41.4 -41.9 -41.1 63.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -48.4 82.5
  .. .. .. .. .. .. ..@ area   : num 0.0351
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -48.2 -48.9 -48 -48.2 82.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -27.6 70.8
  .. .. .. .. .. .. ..@ area   : num 0.034
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -27.7 -27.7 -27.3 -27.7 70.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.9 68.2
  .. .. .. .. .. .. ..@ area   : num 0.034
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -52.1 -52.3 -51.5 -52.1 68.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -43.9 59.9
  .. .. .. .. .. .. ..@ area   : num 0.0337
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -43.6 -44.1 -44 -43.6 59.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -39.2 83.3
  .. .. .. .. .. .. ..@ area   : num 0.0308
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -38.9 -39.6 -39.2 -38.9 83.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -48 60.8
  .. .. .. .. .. .. ..@ area   : num 0.0304
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -47.8 -47.9 -48.2 -47.8 60.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.9 70.9
  .. .. .. .. .. .. ..@ area   : num 0.0291
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -51.6 -52.2 -52 -51.6 70.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -44.4 60
  .. .. .. .. .. .. ..@ area   : num 0.0291
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -44.4 -44.5 -44.4 -44.2 -44.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56.4 73.8
  .. .. .. .. .. .. ..@ area   : num 0.0286
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -56 -56.3 -56.8 -56 73.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -19.4 78.9
  .. .. .. .. .. .. ..@ area   : num 0.0278
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -19.3 -19.8 -19.2 -19.3 78.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.4 72.7
  .. .. .. .. .. .. ..@ area   : num 0.0255
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -55.3 -55.8 -55 -55.3 72.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -25 72.8
  .. .. .. .. .. .. ..@ area   : num 0.0255
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -24.9 -25.2 -24.8 -24.9 72.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53 68.4
  .. .. .. .. .. .. ..@ area   : num 0.0249
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -53 -53.2 -53 -52.9 -53 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -18.9 74.6
  .. .. .. .. .. .. ..@ area   : num 0.0243
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -18.8 -19.2 -18.8 -18.8 74.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.7 73.5
  .. .. .. .. .. .. ..@ area   : num 0.0238
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -55.6 -56.1 -55.5 -55.6 73.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -20.9 76.5
  .. .. .. .. .. .. ..@ area   : num 0.0236
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -20.8 -21.1 -20.7 -20.8 76.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -18.7 81.7
  .. .. .. .. .. .. ..@ area   : num 0.0236
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -18.6 -19.2 -18.3 -18.6 81.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -36.6 65.9
  .. .. .. .. .. .. ..@ area   : num 0.0231
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -36.6 -36.7 -36.7 -36.5 -36.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -70.7 77.4
  .. .. .. .. .. .. ..@ area   : num 0.0216
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -70.7 -70.1 -71.3 -70.7 77.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -52.7 71.3
  .. .. .. .. .. .. ..@ area   : num 0.0209
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -52.6 -52.3 -53.2 -52.6 71.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -18 77.7
  .. .. .. .. .. .. ..@ area   : num 0.0207
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -18 -18.2 -17.6 -18 77.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -20.4 76.3
  .. .. .. .. .. .. ..@ area   : num 0.02
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -20.3 -20.7 -20.2 -20.3 76.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -45.3 60.3
  .. .. .. .. .. .. ..@ area   : num 0.0199
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -45.4 -45.4 -45.2 -45.4 60.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.4 73
  .. .. .. .. .. .. ..@ area   : num 0.0184
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -55.1 -55.7 -55.5 -55.1 73 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -44.2 60
  .. .. .. .. .. .. ..@ area   : num 0.0177
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -44.3 -44.2 -44 -44.3 59.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -21.2 76.6
  .. .. .. .. .. .. ..@ area   : num 0.0167
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -21.1 -21.5 -21 -21.1 76.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.9 72.7
  .. .. .. .. .. .. ..@ area   : num 0.0161
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -55.7 -56.2 -55.6 -55.7 72.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -40.8 64.2
  .. .. .. .. .. .. ..@ area   : num 0.0155
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -40.7 -40.7 -41.1 -40.7 64.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56.7 73.6
  .. .. .. .. .. .. ..@ area   : num 0.0155
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -56.3 -56.8 -57 -56.3 73.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56.9 74.5
  .. .. .. .. .. .. ..@ area   : num 0.015
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -56.5 -56.5 -57.6 -56.5 74.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53 65.6
  .. .. .. .. .. .. ..@ area   : num 0.0149
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -53 -53.2 -52.8 -53 65.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -23.4 69.7
  .. .. .. .. .. .. ..@ area   : num 0.0148
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -23.3 -23.6 -23.3 -23.3 69.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -54.9 70.4
  .. .. .. .. .. .. ..@ area   : num 0.0146
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -54.7 -54.9 -55 -54.7 70.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56 73.2
  .. .. .. .. .. .. ..@ area   : num 0.0145
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -56.1 -56 -55.8 -56.1 73.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.3 64.5
  .. .. .. .. .. .. ..@ area   : num 0.014
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -51.3 -51.4 -51.1 -51.3 64.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.6 71.9
  .. .. .. .. .. .. ..@ area   : num 0.0132
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -55.6 -55.8 -55.4 -55.6 71.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -47.8 82.8
  .. .. .. .. .. .. ..@ area   : num 0.0132
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -47.6 -48.4 -47.4 -47.6 82.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -37.1 65.7
  .. .. .. .. .. .. ..@ area   : num 0.0128
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -37 -37.2 -37 -37 65.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -19 82.1
  .. .. .. .. .. .. ..@ area   : num 0.0125
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -18.9 -19.4 -18.8 -18.9 82 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -69.7 76.6
  .. .. .. .. .. .. ..@ area   : num 0.0123
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -69.7 -70 -69.5 -69.7 76.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -19 78.1
  .. .. .. .. .. .. ..@ area   : num 0.0117
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -19 -19.2 -18.8 -19 78.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -18.5 78.6
  .. .. .. .. .. .. ..@ area   : num 0.0116
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -18.6 -18.7 -18.3 -18.6 78.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -21.3 77.9
  .. .. .. .. .. .. ..@ area   : num 0.0112
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -21.3 -21.4 -21.1 -21.3 77.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -19.1 78.3
  .. .. .. .. .. .. ..@ area   : num 0.0112
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -19.2 -19.4 -18.9 -19.2 78.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -19.3 78.4
  .. .. .. .. .. .. ..@ area   : num 0.0111
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -19.3 -19.4 -19.1 -19.3 78.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -46.6 60.8
  .. .. .. .. .. .. ..@ area   : num 0.0109
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -46.8 -46.8 -46.2 -46.8 60.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -20.6 81.7
  .. .. .. .. .. .. ..@ area   : num 0.0108
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -20.6 -21 -20.2 -20.6 81.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -42.1 62.8
  .. .. .. .. .. .. ..@ area   : num 0.0107
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -42 -41.9 -42.4 -42 62.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -29.8 68.2
  .. .. .. .. .. .. ..@ area   : num 0.0107
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -29.8 -30 -29.7 -29.8 68.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -18.2 78.8
  .. .. .. .. .. .. ..@ area   : num 0.0106
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -18.2 -18.3 -18.1 -18.2 78.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.2 72.6
  .. .. .. .. .. .. ..@ area   : num 0.0105
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -55.1 -55.4 -55 -55.1 72.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -18.4 74.7
  .. .. .. .. .. .. ..@ area   : num 0.01
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -18.5 -18.3 -18.4 -18.5 74.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -20.5 78.2
  .. .. .. .. .. .. ..@ area   : num 0.01
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -20.5 -20.8 -20.3 -20.5 78.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -46.4 60.7
  .. .. .. .. .. .. ..@ area   : num 0.00908
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -46.3 -46.5 -46.3 -46.3 60.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -19.6 78.3
  .. .. .. .. .. .. ..@ area   : num 0.00902
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -19.6 -19.7 -19.5 -19.6 78.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53 65.5
  .. .. .. .. .. .. ..@ area   : num 0.00894
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -52.9 -53.1 -52.9 -52.9 65.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -36.8 65.8
  .. .. .. .. .. .. ..@ area   : num 0.00892
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -36.8 -36.9 -36.8 -36.8 65.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -39.6 65.3
  .. .. .. .. .. .. ..@ area   : num 0.00888
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -39.5 -39.6 -39.8 -39.5 65.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -20.2 77.6
  .. .. .. .. .. .. ..@ area   : num 0.00882
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -20.1 -20.4 -20 -20.1 77.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.4 64.3
  .. .. .. .. .. .. ..@ area   : num 0.0086
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -51.6 -51.4 -51.3 -51.6 64.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -41.5 63
  .. .. .. .. .. .. ..@ area   : num 0.00858
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -41.6 -41.6 -41.5 -41.6 62.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -19.4 79.2
  .. .. .. .. .. .. ..@ area   : num 0.00849
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -19.4 -19.5 -19.3 -19.4 79.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -38.3 83.2
  .. .. .. .. .. .. ..@ area   : num 0.00803
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -38.3 -38.7 -38 -38.3 83.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56.4 73.6
  .. .. .. .. .. .. ..@ area   : num 0.00794
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -56.4 -56.6 -56.2 -56.4 73.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -45.9 60.7
  .. .. .. .. .. .. ..@ area   : num 0.00783
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -46 -46 -46 -46 -45.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56 73.3
  .. .. .. .. .. .. ..@ area   : num 0.0077
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -56 -56.2 -55.8 -56 73.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -46.1 60.7
  .. .. .. .. .. .. ..@ area   : num 0.00767
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -45.9 -46 -46.1 -46.2 -46 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.7 73.3
  .. .. .. .. .. .. ..@ area   : num 0.00754
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -55.7 -55.9 -55.7 -55.7 73.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -42.2 61.8
  .. .. .. .. .. .. ..@ area   : num 0.00716
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -42.3 -42.1 -42.1 -42.3 61.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.5 72.9
  .. .. .. .. .. .. ..@ area   : num 0.00698
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -55.6 -55.4 -55.5 -55.6 72.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -51.7 70.7
  .. .. .. .. .. .. ..@ area   : num 0.00675
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -51.5 -51.9 -51.7 -51.5 70.7 ...
  .. .. .. .. .. [list output truncated]
  .. .. .. ..@ plotOrder: int [1:194] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] -41.5 74.7
  .. .. .. ..@ ID       : chr "GRL"
  .. .. .. ..@ area     : num 645
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 23
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 10.4 51.1
  .. .. .. .. .. .. ..@ area   : num 45
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:63, 1:2] 11 10.8 12.5 12.9 12.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 13.4 54.4
  .. .. .. .. .. .. ..@ area   : num 0.102
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] 13.4 13.7 13.1 13.3 13.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14 53.9
  .. .. .. .. .. .. ..@ area   : num 0.0418
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] 14 14.2 14.2 13.8 14.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 11.2 54.5
  .. .. .. .. .. .. ..@ area   : num 0.0138
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 11.3 11 11.2 11.3 54.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.51 54.72
  .. .. .. .. .. .. ..@ area   : num 0.00562
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.57 8.4 8.55 8.57 54.69 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.89 54.49
  .. .. .. .. .. .. ..@ area   : num 0.00355
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.89 8.82 8.96 8.89 54.46 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.34 54.66
  .. .. .. .. .. .. ..@ area   : num 0.00316
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.36 8.29 8.35 8.36 54.61 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 11.4 54
  .. .. .. .. .. .. ..@ area   : num 0.00305
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 11.5 11.5 11.4 11.5 54 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.65 54.52
  .. .. .. .. .. .. ..@ area   : num 0.00287
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.66 8.59 8.71 8.66 54.49 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 13.1 54.6
  .. .. .. .. .. .. ..@ area   : num 0.00265
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 13.1 13.1 13.2 13.1 54.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 6.74 53.58
  .. .. .. .. .. .. ..@ area   : num 0.00263
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 6.81 6.75 6.66 6.81 53.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 7.53 53.75
  .. .. .. .. .. .. ..@ area   : num 0.00169
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 7.62 7.47 7.49 7.62 53.75 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 7.24 53.71
  .. .. .. .. .. .. ..@ area   : num 0.000976
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 7.24 7.14 7.35 7.24 53.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 7.75 53.77
  .. .. .. .. .. .. ..@ area   : num 0.000738
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 7.76 7.66 7.81 7.76 53.76 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.43 53.95
  .. .. .. .. .. .. ..@ area   : num 0.000647
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.43 8.41 8.45 8.43 53.93 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.15 53.72
  .. .. .. .. .. .. ..@ area   : num 0.000598
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.19 8.12 8.14 8.19 53.72 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 6.97 53.67
  .. .. .. .. .. .. ..@ area   : num 0.000526
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 6.94 6.88 7.09 6.94 53.67 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.69 54.07
  .. .. .. .. .. .. ..@ area   : num 0.000459
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.7 8.67 8.69 8.7 54.04 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.69 47.7
  .. .. .. .. .. .. ..@ area   : num 0.000295
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.71 8.68 8.67 8.71 47.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 11.6 54.1
  .. .. .. .. .. .. ..@ area   : num 0.000278
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 11.5 11.6 11.5 11.5 54.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.7 54.4
  .. .. .. .. .. .. ..@ area   : num 0.000232
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 12.7 12.7 12.7 12.7 54.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 11 54.4
  .. .. .. .. .. .. ..@ area   : num 6.6e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 11 11 11 11 54.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 13.9 54
  .. .. .. .. .. .. ..@ area   : num 3.95e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 13.9 13.9 13.9 13.9 54 ...
  .. .. .. ..@ plotOrder: int [1:23] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 10.4 51.1
  .. .. .. ..@ ID       : chr "DEU"
  .. .. .. ..@ area     : num 45.2
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 144.7 13.4
  .. .. .. .. .. .. ..@ area   : num 0.0273
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 144.7 144.7 144.9 144.7 13.2 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 144.7 13.4
  .. .. .. ..@ ID       : chr "GUM"
  .. .. .. ..@ area     : num 0.0273
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 68
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 22.6 39.5
  .. .. .. .. .. .. ..@ area   : num 11.2
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:49, 1:2] 26.3 26.4 26.6 26 25.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.7 35.2
  .. .. .. .. .. .. ..@ area   : num 0.777
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 23.9 26.3 24.8 23.5 23.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.8 38.5
  .. .. .. .. .. .. ..@ area   : num 0.473
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 23.5 24.2 24.6 22.8 23.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 26.3 39.2
  .. .. .. .. .. .. ..@ area   : num 0.0818
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 26.4 26.6 25.8 26.4 39.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 27.9 36.2
  .. .. .. .. .. .. ..@ area   : num 0.0793
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 27.8 27.7 28.2 27.8 35.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 20.6 38.2
  .. .. .. .. .. .. ..@ area   : num 0.0648
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 20.6 20.8 20.3 20.6 38.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 26 38.4
  .. .. .. .. .. .. ..@ area   : num 0.0573
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 26 25.9 26.2 26 38.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.3 39.9
  .. .. .. .. .. .. ..@ area   : num 0.0361
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.4 25.4 25.1 25.4 40 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 19.8 39.7
  .. .. .. .. .. .. ..@ area   : num 0.0354
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 19.9 19.8 20.1 19.6 19.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 20.8 37.8
  .. .. .. .. .. .. ..@ area   : num 0.0238
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 20.9 20.8 20.6 20.9 37.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.4 37.1
  .. .. .. .. .. .. ..@ area   : num 0.0232
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.5 25.3 25.5 25.5 36.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 27.2 35.6
  .. .. .. .. .. .. ..@ area   : num 0.023
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 27.2 27.1 27.2 27.2 35.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.8 37.9
  .. .. .. .. .. .. ..@ area   : num 0.0228
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.8 25 24.7 24.8 38 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 26.8 37.8
  .. .. .. .. .. .. ..@ area   : num 0.0218
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 27.1 26.6 26.7 27.1 37.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 27.1 36.8
  .. .. .. .. .. .. ..@ area   : num 0.021
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 27 27.1 27.4 27 36.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.6 40.7
  .. .. .. .. .. .. ..@ area   : num 0.0202
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.8 24.5 24.6 24.8 40.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 26.1 37.6
  .. .. .. .. .. .. ..@ area   : num 0.0159
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 26 26.1 26.4 26 37.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.6 38.8
  .. .. .. .. .. .. ..@ area   : num 0.014
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.7 24.6 24.5 24.7 38.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 20.6 38.7
  .. .. .. .. .. .. ..@ area   : num 0.0139
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 20.6 20.6 20.7 20.6 38.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23 36.3
  .. .. .. .. .. .. ..@ area   : num 0.0134
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23 22.9 23.1 23 36.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.1 37.6
  .. .. .. .. .. .. ..@ area   : num 0.0118
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.2 25 25.2 25.2 37.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.2 37.1
  .. .. .. .. .. .. ..@ area   : num 0.0101
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.3 25.1 25.3 25.3 37 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.6 40.4
  .. .. .. .. .. .. ..@ area   : num 0.00899
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.6 25.4 25.7 25.6 40.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.4 36.7
  .. .. .. .. .. .. ..@ area   : num 0.00897
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.4 24.3 24.5 24.4 36.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 26.4 36.6
  .. .. .. .. .. .. ..@ area   : num 0.0087
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 26.3 26.3 26.5 26.3 36.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 27 37
  .. .. .. .. .. .. ..@ area   : num 0.00846
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 27 26.9 27 27 36.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.7 39.1
  .. .. .. .. .. .. ..@ area   : num 0.00685
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.7 23.6 23.8 23.7 39.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.4 37.4
  .. .. .. .. .. .. ..@ area   : num 0.00621
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.4 24.4 24.5 24.4 37.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.3 37.6
  .. .. .. .. .. .. ..@ area   : num 0.00617
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.3 24.3 24.4 24.3 37.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.7 37
  .. .. .. .. .. .. ..@ area   : num 0.00567
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.7 24.6 24.8 24.7 36.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 27.8 36.6
  .. .. .. .. .. .. ..@ area   : num 0.00564
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 27.9 27.8 27.9 27.9 36.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.4 37.5
  .. .. .. .. .. .. ..@ area   : num 0.00559
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.4 25.3 25.5 25.4 37.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.5 38
  .. .. .. .. .. .. ..@ area   : num 0.00544
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.5 23.4 23.5 23.5 37.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.4 36.4
  .. .. .. .. .. .. ..@ area   : num 0.00518
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.5 25.4 25.5 25.5 36.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.4 36.7
  .. .. .. .. .. .. ..@ area   : num 0.00503
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.4 25.3 25.4 25.4 36.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.9 37.4
  .. .. .. .. .. .. ..@ area   : num 0.00501
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25 24.9 24.9 25 37.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.5 37.7
  .. .. .. .. .. .. ..@ area   : num 0.00491
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.5 23.4 23.6 23.5 37.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 20.7 38.4
  .. .. .. .. .. .. ..@ area   : num 0.00483
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 20.7 20.6 20.6 20.7 38.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.8 36.8
  .. .. .. .. .. .. ..@ area   : num 0.00459
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.8 25.7 26 25.8 36.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 27.4 36.4
  .. .. .. .. .. .. ..@ area   : num 0.00394
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 27.4 27.3 27.4 27.4 36.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 26.8 37.2
  .. .. .. .. .. .. ..@ area   : num 0.00383
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 26.9 26.8 26.9 26.9 37.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 26.9 35.4
  .. .. .. .. .. .. ..@ area   : num 0.00374
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 26.9 26.9 27 26.9 35.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.5 37.1
  .. .. .. .. .. .. ..@ area   : num 0.00348
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.5 24.5 24.4 24.5 37.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.9 39.2
  .. .. .. .. .. .. ..@ area   : num 0.00321
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.9 24 24 23.9 39.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.5 39.2
  .. .. .. .. .. .. ..@ area   : num 0.003
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.5 23.4 23.5 23.5 39.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.1 37
  .. .. .. .. .. .. ..@ area   : num 0.00257
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.1 25 25.1 25.1 37 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25 39.5
  .. .. .. .. .. .. ..@ area   : num 0.00251
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25 25 25.1 25 39.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.6 38.6
  .. .. .. .. .. .. ..@ area   : num 0.00251
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.6 25.5 25.6 25.6 38.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.1 36.7
  .. .. .. .. .. .. ..@ area   : num 0.0024
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.1 25.1 25.2 25.1 36.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.8 36.4
  .. .. .. .. .. .. ..@ area   : num 0.00219
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.8 25.7 25.8 25.8 36.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.1 34.8
  .. .. .. .. .. .. ..@ area   : num 0.002
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.1 24.1 24 24.1 34.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.9 36.6
  .. .. .. .. .. .. ..@ area   : num 0.0019
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25 24.8 24.9 25 36.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.5 36.8
  .. .. .. .. .. .. ..@ area   : num 0.00146
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.6 24.5 24.6 24.6 36.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.8 36.7
  .. .. .. .. .. .. ..@ area   : num 0.00119
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 21.8 21.8 21.8 21.8 21.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.3 35.9
  .. .. .. .. .. .. ..@ area   : num 0.00115
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.3 23.3 23.3 23.3 35.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.5 37.3
  .. .. .. .. .. .. ..@ area   : num 0.00107
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.4 23.4 23.6 23.4 37.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.5 36.8
  .. .. .. .. .. .. ..@ area   : num 0.000988
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.5 25.4 25.5 25.5 36.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 20.2 39.2
  .. .. .. .. .. .. ..@ area   : num 0.000934
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 20.2 20.1 20.1 20.2 39.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.5 37.5
  .. .. .. .. .. .. ..@ area   : num 0.000903
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.5 23.4 23.5 23.5 37.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.6 36.9
  .. .. .. .. .. .. ..@ area   : num 0.000881
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.7 25.6 25.7 25.7 36.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 23.1 37.3
  .. .. .. .. .. .. ..@ area   : num 0.000857
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 23.2 23.1 23.2 23.2 37.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 24.6 36.8
  .. .. .. .. .. .. ..@ area   : num 0.000805
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 24.7 24.6 24.7 24.7 36.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.8 37.1
  .. .. .. .. .. .. ..@ area   : num 0.000724
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.8 25.8 25.8 25.8 37.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 21.7 36.8
  .. .. .. .. .. .. ..@ area   : num 0.000698
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 21.7 21.7 21.7 21.7 36.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 19.4 39.9
  .. .. .. .. .. .. ..@ area   : num 0.000632
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 19.4 19.4 19.4 19.4 39.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 20.9 38.6
  .. .. .. .. .. .. ..@ area   : num 0.000489
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 20.9 20.9 20.9 20.9 38.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 26.8 37.2
  .. .. .. .. .. .. ..@ area   : num 0.00015
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 26.8 26.8 26.8 26.8 37.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 25.4 36.4
  .. .. .. .. .. .. ..@ area   : num 0.000114
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 25.4 25.4 25.4 25.4 36.4 ...
  .. .. .. ..@ plotOrder: int [1:68] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 22.6 39.5
  .. .. .. ..@ ID       : chr "GRC"
  .. .. .. ..@ area     : num 13.2
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.3 15.7
  .. .. .. .. .. .. ..@ area   : num 9.02
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] -90.6 -91.4 -92.2 -91.7 -90.4 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -90.3 15.7
  .. .. .. ..@ ID       : chr "GTM"
  .. .. .. ..@ area     : num 9.02
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -10.9 10.4
  .. .. .. .. .. .. ..@ area   : num 20.3
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:39, 1:2] -13.6 -14.7 -14.7 -15 -14.7 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -10.9 10.4
  .. .. .. ..@ ID       : chr "GIN"
  .. .. .. ..@ area     : num 20.3
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 2
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.97 4.75
  .. .. .. .. .. .. ..@ area   : num 17.1
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:28, 1:2] -59.8 -58.5 -58.6 -58.3 -57.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.4 6.94
  .. .. .. .. .. .. ..@ area   : num 0.00331
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -58.5 -58.4 -58.3 -58.5 6.9 ...
  .. .. .. ..@ plotOrder: int [1:2] 1 2
  .. .. .. ..@ labpt    : num [1:2] -58.97 4.75
  .. .. .. ..@ ID       : chr "GUY"
  .. .. .. ..@ area     : num 17.1
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 5
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.7 18.9
  .. .. .. .. .. .. ..@ area   : num 2.42
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -72.7 -71.8 -71.7 -72 -71.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73 18.8
  .. .. .. .. .. .. ..@ area   : num 0.0282
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -72.8 -73.3 -72.9 -72.8 18.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.8 20
  .. .. .. .. .. .. ..@ area   : num 0.0109
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -72.6 -73 -72.8 -72.6 20 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.7 18.1
  .. .. .. .. .. .. ..@ area   : num 0.00182
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.6 -73.7 -73.7 -73.6 18.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.8 18.6
  .. .. .. .. .. .. ..@ area   : num 0.00167
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -73.7 -73.7 -73.8 -73.7 18.6 ...
  .. .. .. ..@ plotOrder: int [1:5] 1 2 3 4 5
  .. .. .. ..@ labpt    : num [1:2] -72.7 18.9
  .. .. .. ..@ ID       : chr "HTI"
  .. .. .. ..@ area     : num 2.46
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 8
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -86.7 14.8
  .. .. .. .. .. .. ..@ area   : num 9.54
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -85.8 -84.3 -83.9 -84.2 -84.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -86.4 16.4
  .. .. .. .. .. .. ..@ area   : num 0.00907
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -86.6 -86.4 -86.3 -86.6 16.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -85.9 16.5
  .. .. .. .. .. .. ..@ area   : num 0.00266
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -85.9 -85.9 -85.8 -85.9 16.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -87.6 13.3
  .. .. .. .. .. .. ..@ area   : num 0.00265
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -87.6 -87.6 -87.7 -87.6 13.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -86.9 16.1
  .. .. .. .. .. .. ..@ area   : num 0.00189
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -86.9 -87 -86.9 -86.9 16.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -87.6 13.3
  .. .. .. .. .. .. ..@ area   : num 0.00106
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -87.6 -87.7 -87.6 -87.6 13.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -83.9 17.4
  .. .. .. .. .. .. ..@ area   : num 7.23e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -83.9 -83.9 -83.9 -83.9 17.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -83.9 17.4
  .. .. .. .. .. .. ..@ area   : num 5.21e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -83.9 -84 -83.9 -83.9 17.4 ...
  .. .. .. ..@ plotOrder: int [1:8] 1 2 3 4 5 6 7 8
  .. .. .. ..@ labpt    : num [1:2] -86.7 14.8
  .. .. .. ..@ ID       : chr "HND"
  .. .. .. ..@ area     : num 9.56
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 19
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 16.4 45.2
  .. .. .. .. .. .. ..@ area   : num 5.81
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:22, 1:2] 16.6 16.6 17.7 18.8 19 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 17.8 42.8
  .. .. .. .. .. .. ..@ area   : num 0.124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 17.3 17.6 18.5 18.5 17 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14.6 45.1
  .. .. .. .. .. .. ..@ area   : num 0.0339
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 14.8 14.4 14.5 14.8 44.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14.4 44.9
  .. .. .. .. .. .. ..@ area   : num 0.0318
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 14.5 14.5 14.3 14.4 14.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 16.7 43.2
  .. .. .. .. .. .. ..@ area   : num 0.0214
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 16.6 17.2 16.4 16.6 43.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 16.7 43.3
  .. .. .. .. .. .. ..@ area   : num 0.0179
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 16.9 16.4 16.8 16.9 43.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 15.1 44
  .. .. .. .. .. .. ..@ area   : num 0.0131
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 14.9 15.2 15.1 14.9 44.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14.4 44.6
  .. .. .. .. .. .. ..@ area   : num 0.00753
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 14.5 14.4 14.3 14.5 44.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 17.5 42.8
  .. .. .. .. .. .. ..@ area   : num 0.0058
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 17.7 17.3 17.4 17.7 42.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14.8 44.8
  .. .. .. .. .. .. ..@ area   : num 0.00525
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 14.8 14.9 14.7 14.8 44.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 16.2 43.1
  .. .. .. .. .. .. ..@ area   : num 0.00486
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 16.2 16.1 16.3 16.2 43 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 16.3 43.4
  .. .. .. .. .. .. ..@ area   : num 0.00337
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 16.4 16.2 16.3 16.4 43.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 15.4 44
  .. .. .. .. .. .. ..@ area   : num 0.00335
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 15.4 15.3 15.4 15.4 43.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14.9 44.5
  .. .. .. .. .. .. ..@ area   : num 0.00325
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 14.8 14.7 15.2 14.8 44.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 15.2 44.1
  .. .. .. .. .. .. ..@ area   : num 0.00289
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 15.3 15.1 15.2 15.3 44 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 16.9 42.8
  .. .. .. .. .. .. ..@ area   : num 0.00265
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 16.9 16.8 16.9 16.9 42.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 15.3 43.8
  .. .. .. .. .. .. ..@ area   : num 0.00103
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 15.3 15.4 15.2 15.3 43.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 15.7 43.7
  .. .. .. .. .. .. ..@ area   : num 0.000856
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 15.7 15.6 15.7 15.7 43.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 16.9 43
  .. .. .. .. .. .. ..@ area   : num 4.81e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 16.8 17.2 16.6 16.8 43 ...
  .. .. .. ..@ plotOrder: int [1:19] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 16.4 45.2
  .. .. .. ..@ ID       : chr "HRV"
  .. .. .. ..@ area     : num 6.09
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 19.4 47.2
  .. .. .. .. .. .. ..@ area   : num 11.1
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] 18.8 17.7 16.6 16.1 16.5 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 19.4 47.2
  .. .. .. ..@ ID       : chr "HUN"
  .. .. .. ..@ area     : num 11.1
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -18.6 65
  .. .. .. .. .. .. ..@ area   : num 18.4
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:60, 1:2] -15.1 -14.6 -14.8 -14.3 -14.6 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -18.6 65
  .. .. .. ..@ ID       : chr "ISL"
  .. .. .. ..@ area     : num 18.4
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 46
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 79.6 22.9
  .. .. .. .. .. .. ..@ area   : num 277
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:163, 1:2] 78.1 78.3 79 78.8 79.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 92.7 11.8
  .. .. .. .. .. .. ..@ area   : num 0.0669
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 92.7 92.7 92.5 92.7 12.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93 13.2
  .. .. .. .. .. .. ..@ area   : num 0.0521
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.1 92.8 93 93.1 13.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 92.9 12.6
  .. .. .. .. .. .. ..@ area   : num 0.0518
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 92.9 93 92.8 92.9 12.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.8 6.99
  .. .. .. .. .. .. ..@ area   : num 0.041
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.91 93.82 93.67 93.91 7.03 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 92.4 10.7
  .. .. .. .. .. .. ..@ area   : num 0.0282
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 92.5 92.3 92.5 92.5 10.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 88.1 21.7
  .. .. .. .. .. .. ..@ area   : num 0.0114
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 88.1 88 88.1 88.1 21.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 92.8 12.2
  .. .. .. .. .. .. ..@ area   : num 0.0105
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 92.8 92.9 92.8 92.8 12.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.48 8.12
  .. .. .. .. .. .. ..@ area   : num 0.00743
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.49 93.44 93.51 93.49 7.99 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.69 7.34
  .. .. .. .. .. .. ..@ area   : num 0.00642
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.64 93.69 93.73 93.64 7.25 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.37 7.92
  .. .. .. .. .. .. ..@ area   : num 0.00601
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.45 93.35 93.32 93.45 7.87 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 92.7 11.4
  .. .. .. .. .. .. ..@ area   : num 0.00546
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 92.6 92.6 92.7 92.6 11.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 88.6 21.8
  .. .. .. .. .. .. ..@ area   : num 0.00526
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 88.6 88.6 88.6 88.6 88.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.1 8.27
  .. .. .. .. .. .. ..@ area   : num 0.00466
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.2 93.1 93.1 93.2 8.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93 12
  .. .. .. .. .. .. ..@ area   : num 0.00458
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.1 92.9 93 93.1 11.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 88.9 21.6
  .. .. .. .. .. .. ..@ area   : num 0.00447
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 88.9 88.8 88.8 88.8 88.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 92.75 9.18
  .. .. .. .. .. .. ..@ area   : num 0.0043
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 92.77 92.71 92.76 92.77 9.12 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 72.7 21.6
  .. .. .. .. .. .. ..@ area   : num 0.0037
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 72.7 72.6 72.8 72.7 21.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 80.9 15.8
  .. .. .. .. .. .. ..@ area   : num 0.00335
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 80.9 80.9 81 80.9 15.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 92.7 12.9
  .. .. .. .. .. .. ..@ area   : num 0.00284
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 92.7 92.7 92.7 92.7 12.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.54 7.97
  .. .. .. .. .. .. ..@ area   : num 0.00229
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.57 93.5 93.54 93.57 7.93 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.1 12.1
  .. .. .. .. .. .. ..@ area   : num 0.00177
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.1 93.1 93.1 93.1 12.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 92.3 11.6
  .. .. .. .. .. .. ..@ area   : num 0.00173
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 92.3 92.2 92.3 92.3 11.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 88.1 21.9
  .. .. .. .. .. .. ..@ area   : num 0.0016
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 88.1 88.1 88.1 88.1 21.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.6 8.5
  .. .. .. .. .. .. ..@ area   : num 0.00142
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.61 93.6 93.63 93.61 8.43 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93 13.7
  .. .. .. .. .. .. ..@ area   : num 0.000998
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93 93 93 93 13.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 73.04 8.28
  .. .. .. .. .. .. ..@ area   : num 0.000874
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 73.03 73.02 73.08 73.03 8.25 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 92.9 12.4
  .. .. .. .. .. .. ..@ area   : num 0.000449
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 92.9 92.9 92.9 92.9 12.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 72.8 11.2
  .. .. .. .. .. .. ..@ area   : num 0.000321
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 72.8 72.8 72.8 72.8 11.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 94.3 13.4
  .. .. .. .. .. .. ..@ area   : num 0.000259
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 94.3 94.3 94.3 94.3 13.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 73.6 10.1
  .. .. .. .. .. .. ..@ area   : num 0.000235
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 73.6 73.6 73.6 73.6 10.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.9 12.3
  .. .. .. .. .. .. ..@ area   : num 0.00022
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.9 93.8 93.9 93.9 12.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 72.2 10.8
  .. .. .. .. .. .. ..@ area   : num 0.000193
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 72.2 72.2 72.2 72.2 10.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 86.3 20
  .. .. .. .. .. .. ..@ area   : num 0.000166
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 86.3 86.3 86.3 86.3 20 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 72.6 10.6
  .. .. .. .. .. .. ..@ area   : num 0.000135
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 72.6 72.6 72.7 72.6 10.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 73 11.5
  .. .. .. .. .. .. ..@ area   : num 0.00013
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 73 73 73 73 11.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.1 12.1
  .. .. .. .. .. .. ..@ area   : num 8.65e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.1 93.1 93.1 93.1 12.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 72.3 10.9
  .. .. .. .. .. .. ..@ area   : num 8.36e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 72.3 72.3 72.3 72.3 10.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 73.7 10.8
  .. .. .. .. .. .. ..@ area   : num 7.88e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 73.7 73.7 73.7 73.7 10.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 72.7 11.1
  .. .. .. .. .. .. ..@ area   : num 7.63e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 72.7 72.7 72.7 72.7 11.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 72.7 11.7
  .. .. .. .. .. .. ..@ area   : num 6.34e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 72.7 72.7 72.7 72.7 11.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 92.9 13.4
  .. .. .. .. .. .. ..@ area   : num 5.85e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 92.9 92.9 92.9 92.9 13.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 72.1 11.2
  .. .. .. .. .. .. ..@ area   : num 4.92e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 72.1 72.1 72.1 72.1 11.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 93.66 7.41
  .. .. .. .. .. .. ..@ area   : num 4.59e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 93.66 93.65 93.66 93.66 7.41 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 92.8 12.3
  .. .. .. .. .. .. ..@ area   : num 2.72e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 92.8 92.8 92.8 92.8 12.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 82.4 16.9
  .. .. .. .. .. .. ..@ area   : num 9.52e-06
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 82.4 82.4 82.4 82.4 16.9 ...
  .. .. .. ..@ plotOrder: int [1:46] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 79.6 22.9
  .. .. .. ..@ ID       : chr "IND"
  .. .. .. ..@ area     : num 278
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 8
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 54.3 32.6
  .. .. .. .. .. .. ..@ area   : num 156
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:71, 1:2] 45 46.2 46.5 48 48.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 55.9 26.8
  .. .. .. .. .. .. ..@ area   : num 0.0619
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 56.2 55.3 55.8 55.8 56.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 55.7 26.9
  .. .. .. .. .. .. ..@ area   : num 0.00566
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 55.7 55.6 55.7 55.7 26.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 54 26.5
  .. .. .. .. .. .. ..@ area   : num 0.00414
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 54 53.9 54 54 26.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 56.5 27.1
  .. .. .. .. .. .. ..@ area   : num 0.00263
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 56.5 56.4 56.5 56.5 56.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 53.2 26.8
  .. .. .. .. .. .. ..@ area   : num 0.0024
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 53.3 53.2 53.2 53.3 26.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 56.4 26.9
  .. .. .. .. .. .. ..@ area   : num 0.00201
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 56.3 56.3 56.4 56.3 26.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 50.3 29.3
  .. .. .. .. .. .. ..@ area   : num 0.00147
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 50.3 50.3 50.3 50.3 29.2 ...
  .. .. .. ..@ plotOrder: int [1:8] 1 2 3 4 5 6 7 8
  .. .. .. ..@ labpt    : num [1:2] 54.3 32.6
  .. .. .. ..@ ID       : chr "IRN"
  .. .. .. ..@ area     : num 156
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 2
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 35 31.4
  .. .. .. .. .. .. ..@ area   : num 2.19
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] 35.6 35.6 35.6 35.3 34.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 35.3 31.8
  .. .. .. .. .. .. ..@ area   : num 8.28e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 35.3 35.3 35.2 35.3 31.8 ...
  .. .. .. ..@ plotOrder: int [1:2] 1 2
  .. .. .. ..@ labpt    : num [1:2] 35 31.4
  .. .. .. ..@ ID       : chr "ISR"
  .. .. .. ..@ area     : num 2.19
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 24
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.2 43.5
  .. .. .. .. .. .. ..@ area   : num 27.9
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:52, 1:2] 12.1 12.4 13.7 13.4 13.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 9.05 40.11
  .. .. .. .. .. .. ..@ area   : num 2.51
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] 9.51 9.83 9.57 9.02 8.86 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14.2 37.6
  .. .. .. .. .. .. ..@ area   : num 2.4
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] 15.5 15.1 15.3 15.1 12.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 10.3 42.8
  .. .. .. .. .. .. ..@ area   : num 0.0223
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 10.4 10.4 10.1 10.4 42.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.29 41.06
  .. .. .. .. .. .. ..@ area   : num 0.00428
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.34 8.22 8.32 8.34 41.06 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12 36.8
  .. .. .. .. .. .. ..@ area   : num 0.00398
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 12 11.9 12 12 36.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 8.27 39.15
  .. .. .. .. .. .. ..@ area   : num 0.00367
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 8.29 8.22 8.3 8.29 39.09 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.5 43.9
  .. .. .. .. .. .. ..@ area   : num 0.00354
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 12.5 12.5 12.4 12.5 43.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.4 45.4
  .. .. .. .. .. .. ..@ area   : num 0.00227
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 12.4 12.3 12.4 12.4 45.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 13.9 40.7
  .. .. .. .. .. .. ..@ area   : num 0.00195
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 14 13.9 13.9 14 40.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14.9 38.5
  .. .. .. .. .. .. ..@ area   : num 0.00185
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 15 14.9 15 15 38.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14.8 38.6
  .. .. .. .. .. .. ..@ area   : num 0.00172
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 14.9 14.8 14.9 14.9 38.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 9.82 43.04
  .. .. .. .. .. .. ..@ area   : num 0.00168
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 9.84 9.81 9.8 9.83 9.84 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 15 38.4
  .. .. .. .. .. .. ..@ area   : num 0.00107
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 15 14.9 15 15 38.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.3 37.9
  .. .. .. .. .. .. ..@ area   : num 0.000946
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 12.4 12.3 12.3 12.4 37.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 10.9 42.4
  .. .. .. .. .. .. ..@ area   : num 0.000918
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 10.9 10.9 10.9 10.9 42.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 13 40.9
  .. .. .. .. .. .. ..@ area   : num 0.000735
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 13 13 13 13 40.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 15.2 38.8
  .. .. .. .. .. .. ..@ area   : num 0.00067
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 15.2 15.2 15.2 15.2 38.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 14.2 40.6
  .. .. .. .. .. .. ..@ area   : num 0.000647
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 14.2 14.3 14.2 14.2 40.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.1 38
  .. .. .. .. .. .. ..@ area   : num 0.000535
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 12.1 12 12.1 12.1 38 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 10.3 42.3
  .. .. .. .. .. .. ..@ area   : num 0.000458
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 10.3 10.3 10.3 10.3 42.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 13.2 38.7
  .. .. .. .. .. .. ..@ area   : num 0.000419
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 13.2 13.2 13.2 13.2 38.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 10.1 42.6
  .. .. .. .. .. .. ..@ area   : num 0.000379
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 10.1 10.1 10.1 10.1 42.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 12.5 41.9
  .. .. .. .. .. .. ..@ area   : num 3.37e-05
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 12.4 12.5 12.5 12.4 41.9 ...
  .. .. .. ..@ plotOrder: int [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 12.2 43.5
  .. .. .. ..@ ID       : chr "ITA"
  .. .. .. ..@ area     : num 32.9
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 3
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -5.54 7.6
  .. .. .. .. .. .. ..@ area   : num 26.6
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:32, 1:2] -4.79 -5.32 -5 -7.53 -7.42 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -4.31 5.23
  .. .. .. .. .. .. ..@ area   : num 0.0223
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -4.79 -4.15 -4 -4.79 5.17 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -3.12 5.1
  .. .. .. .. .. .. ..@ area   : num 0.000813
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -3.1 -3.1 -3.17 -3.1 5.11 ...
  .. .. .. ..@ plotOrder: int [1:3] 1 2 3
  .. .. .. ..@ labpt    : num [1:2] -5.54 7.6
  .. .. .. ..@ ID       : chr "CIV"
  .. .. .. ..@ area     : num 26.6
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 43.8 33.1
  .. .. .. .. .. .. ..@ area   : num 42.4
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:34, 1:2] 45.8 45.4 46.2 46.1 47.4 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 43.8 33.1
  .. .. .. ..@ ID       : chr "IRQ"
  .. .. .. ..@ area     : num 42.4
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 67
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 138 36.6
  .. .. .. .. .. .. ..@ area   : num 23.4
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:57, 1:2] 141 142 142 141 141 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.5 43.3
  .. .. .. .. .. .. ..@ area   : num 8.88
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:22, 1:2] 142 144 145 145 145 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 130.9 32.6
  .. .. .. .. .. .. ..@ area   : num 3.91
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:24, 1:2] 131 132 132 132 132 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 133.4 33.6
  .. .. .. .. .. .. ..@ area   : num 1.87
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] 134 135 134 134 133 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 128 26.5
  .. .. .. .. .. .. ..@ area   : num 0.151
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 128 128 128 128 128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.4 28.3
  .. .. .. .. .. .. ..@ area   : num 0.0619
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.7 129.4 129.1 129.7 28.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 134.8 34.4
  .. .. .. .. .. .. ..@ area   : num 0.0364
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 134.8 134.7 135 134.8 34.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.4 34.5
  .. .. .. .. .. .. ..@ area   : num 0.0241
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.3 129.3 129.5 129.3 34.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 128.7 32.7
  .. .. .. .. .. .. ..@ area   : num 0.0237
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 128.9 128.6 128.7 128.9 32.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 130.1 32.4
  .. .. .. .. .. .. ..@ area   : num 0.0222
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 130 130.2 130.2 130 32.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 124.2 24.4
  .. .. .. .. .. .. ..@ area   : num 0.0217
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 124 124 124 124 124 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 130.5 30.3
  .. .. .. .. .. .. ..@ area   : num 0.0213
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 130.6 130.4 130.7 130.6 30.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 138.4 38.1
  .. .. .. .. .. .. ..@ area   : num 0.0196
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 138 138 139 138 38 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 130.9 30.6
  .. .. .. .. .. .. ..@ area   : num 0.0174
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 130.9 130.9 131.1 130.9 30.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.3 34.2
  .. .. .. .. .. .. ..@ area   : num 0.0154
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.2 129.2 129.3 129.2 34.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 130.3 32.4
  .. .. .. .. .. .. ..@ area   : num 0.0131
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 130.4 130.2 130.4 130.4 32.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 123.8 24.3
  .. .. .. .. .. .. ..@ area   : num 0.013
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 123.9 123.7 123.9 123.9 24.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 125.3 24.8
  .. .. .. .. .. .. ..@ area   : num 0.0116
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 125 125 125 125 125 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 133.3 36.2
  .. .. .. .. .. .. ..@ area   : num 0.00993
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 133.3 133.4 133.3 133.3 36.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 134.3 34.5
  .. .. .. .. .. .. ..@ area   : num 0.00983
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 134.4 134.2 134.4 134.4 34.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129 27.8
  .. .. .. .. .. .. ..@ area   : num 0.00932
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129 128.9 129 129 27.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 139.5 42.2
  .. .. .. .. .. .. ..@ area   : num 0.00903
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 139.6 139.4 139.4 139.6 42.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 141.2 45.2
  .. .. .. .. .. .. ..@ area   : num 0.0085
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 141.3 141.1 141.3 141.3 45.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 141 45.4
  .. .. .. .. .. .. ..@ area   : num 0.00832
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 141 141 141.1 141 45.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.5 33.3
  .. .. .. .. .. .. ..@ area   : num 0.00752
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.4 129.5 129.6 129.4 33.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 132.4 34.2
  .. .. .. .. .. .. ..@ area   : num 0.0073
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 132.4 132.4 132.5 132.4 34.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 128.6 27.4
  .. .. .. .. .. .. ..@ area   : num 0.00626
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 128.6 128.5 128.7 128.6 27.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.7 33.8
  .. .. .. .. .. .. ..@ area   : num 0.00575
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.8 129.7 129.7 129.8 33.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 132.5 34.1
  .. .. .. .. .. .. ..@ area   : num 0.00574
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 133 133 132 133 133 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.2 28.1
  .. .. .. .. .. .. ..@ area   : num 0.005
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.3 129.2 129.2 129.3 28.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.7 31.7
  .. .. .. .. .. .. ..@ area   : num 0.00489
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.7 129.7 129.8 129.7 31.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 139.8 33.1
  .. .. .. .. .. .. ..@ area   : num 0.00443
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 140 140 140 140 33 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 137 37.1
  .. .. .. .. .. .. ..@ area   : num 0.00394
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 137.1 137 136.9 137.1 37.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 139.4 34.7
  .. .. .. .. .. .. ..@ area   : num 0.00373
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 139.5 139.4 139.4 139.5 34.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 133 36.1
  .. .. .. .. .. .. ..@ area   : num 0.00364
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 133 133 133 133 36 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 130 28.3
  .. .. .. .. .. .. ..@ area   : num 0.00353
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 130 129.9 130 130 28.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 133 34.2
  .. .. .. .. .. .. ..@ area   : num 0.00348
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 133 133 133.1 133 34.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126.7 26.3
  .. .. .. .. .. .. ..@ area   : num 0.00347
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126.8 126.8 126.7 126.8 26.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 130.2 32.2
  .. .. .. .. .. .. ..@ area   : num 0.00344
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 130.2 130.1 130.2 130.2 32.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 145.3 43.6
  .. .. .. .. .. .. ..@ area   : num 0.00284
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 145.3 145.2 145.4 145.3 43.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 132.3 33.9
  .. .. .. .. .. .. ..@ area   : num 0.00279
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 132.3 132.2 132.5 132.3 33.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129 33
  .. .. .. .. .. .. ..@ area   : num 0.00236
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129 129 129 129 33 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 139.5 34.1
  .. .. .. .. .. .. ..@ area   : num 0.00226
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 140 139 140 140 34 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 139.3 34.4
  .. .. .. .. .. .. ..@ area   : num 0.00175
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 139.3 139.2 139.3 139.3 34.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 133.1 36.1
  .. .. .. .. .. .. ..@ area   : num 0.00167
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 133 133 133 133 36 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.2 27.1
  .. .. .. .. .. .. ..@ area   : num 0.00153
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 142 142 142 142 27 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.9 29.8
  .. .. .. .. .. .. ..@ area   : num 0.00149
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.9 129.9 129.9 129.9 29.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.8 33.4
  .. .. .. .. .. .. ..@ area   : num 0.00113
  .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.8 129.9 129.8 129.8 33.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 141.3 24.8
  .. .. .. .. .. .. ..@ area   : num 0.00111
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 141.3 141.3 141.4 141.3 24.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 132.2 33.8
  .. .. .. .. .. .. ..@ area   : num 0.00108
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 132.3 132.2 132.2 132.3 33.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 131.2 25.8
  .. .. .. .. .. .. ..@ area   : num 0.00105
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 131.2 131.2 131.3 131.2 25.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.1 26.7
  .. .. .. .. .. .. ..@ area   : num 0.001
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 142.2 142.1 142.2 142.2 26.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 123 24.5
  .. .. .. .. .. .. ..@ area   : num 0.000985
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 123 122.9 123 123 24.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 139.6 33.9
  .. .. .. .. .. .. ..@ area   : num 0.00089
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 139.6 139.6 139.6 139.6 33.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 129.7 29.6
  .. .. .. .. .. .. ..@ area   : num 0.000879
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 129.7 129.7 129.7 129.7 29.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 143.9 44.2
  .. .. .. .. .. .. ..@ area   : num 0.000718
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 144 143.9 143.8 144 44.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 133 36
  .. .. .. .. .. .. ..@ area   : num 0.000551
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 133 133 133 133 36 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 130 30
  .. .. .. .. .. .. ..@ area   : num 0.000464
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 130 130 130 130 30 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 141.3 25.4
  .. .. .. .. .. .. ..@ area   : num 0.000418
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 141.3 141.3 141.3 141.3 25.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 131.3 24.5
  .. .. .. .. .. .. ..@ area   : num 0.000359
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 131.3 131.2 131.3 131.3 24.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.2 27.2
  .. .. .. .. .. .. ..@ area   : num 0.000342
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 142.2 142.2 142.2 142.2 27.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 154 24.3
  .. .. .. .. .. .. ..@ area   : num 0.000232
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 154 153.9 154 154 24.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 140.3 30.5
  .. .. .. .. .. .. ..@ area   : num 0.00022
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 140.3 140.3 140.3 140.3 30.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.2 27.6
  .. .. .. .. .. .. ..@ area   : num 0.000209
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 142.2 142.2 142.2 142.2 27.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 140.9 27.2
  .. .. .. .. .. .. ..@ area   : num 0.000138
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 140.9 140.9 140.9 140.9 27.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 142.1 27.7
  .. .. .. .. .. .. ..@ area   : num 0.000137
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 142.1 142.1 142.1 142.1 27.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 140 31.9
  .. .. .. .. .. .. ..@ area   : num 0.000103
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 140 140 140 140 31.9 ...
  .. .. .. ..@ plotOrder: int [1:67] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 138 36.6
  .. .. .. ..@ ID       : chr "JPN"
  .. .. .. ..@ area     : num 38.7
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.3 18.1
  .. .. .. .. .. .. ..@ area   : num 0.753
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -77.1 -77.7 -78.3 -76.9 -76.2 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -77.3 18.1
  .. .. .. ..@ ID       : chr "JAM"
  .. .. .. ..@ area     : num 0.753
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 36.8 31.2
  .. .. .. .. .. .. ..@ area   : num 8.33
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] 35 35 35.5 35.6 35.6 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 36.8 31.2
  .. .. .. ..@ ID       : chr "JOR"
  .. .. .. ..@ area     : num 8.33
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 2
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 37.824 0.539
  .. .. .. .. .. .. ..@ area   : num 47
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:26, 1:2] 35.9 37 38.1 39.5 40.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 41.05 -2.11
  .. .. .. .. .. .. ..@ area   : num 0.00936
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 41.14 40.95 41.06 41.14 -2.12 ...
  .. .. .. ..@ plotOrder: int [1:2] 1 2
  .. .. .. ..@ labpt    : num [1:2] 37.824 0.539
  .. .. .. ..@ ID       : chr "KEN"
  .. .. .. ..@ area     : num 47
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 74.6 41.4
  .. .. .. .. .. .. ..@ area   : num 21
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:32, 1:2] 80.2 78.1 76.9 76.3 75.7 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 74.6 41.4
  .. .. .. ..@ ID       : chr "KGZ"
  .. .. .. ..@ area     : num 21
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 2
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 127.2 40.2
  .. .. .. .. .. .. ..@ area   : num 12.8
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:34, 1:2] 131 131 130 130 128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 124.9 39.6
  .. .. .. .. .. .. ..@ area   : num 0.00238
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 124.8 124.9 124.9 124.8 39.5 ...
  .. .. .. ..@ plotOrder: int [1:2] 1 2
  .. .. .. ..@ labpt    : num [1:2] 127.2 40.2
  .. .. .. ..@ ID       : chr "PRK"
  .. .. .. ..@ area     : num 12.8
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 42
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -157.39 1.86
  .. .. .. .. .. .. ..@ area   : num 0.0419
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -157.43 -157.18 -157.57 -157.43 2.02 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 173.029 0.927
  .. .. .. .. .. .. ..@ area   : num 0.00687
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.02 173.08 172.98 173.02 1.01 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 172.99 1.82
  .. .. .. .. .. .. ..@ area   : num 0.00629
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.01 172.93 173.03 173.01 1.71 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -159.34 3.86
  .. .. .. .. .. .. ..@ area   : num 0.00616
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -159.34 -159.27 -159.4 -159.34 3.92 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 175.03 -1.51
  .. .. .. .. .. .. ..@ area   : num 0.00242
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 175.04 175 175.05 175.04 -1.55 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -154.97 -4.07
  .. .. .. .. .. .. ..@ area   : num 0.00206
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -154.94 -154.96 -155.02 -154.94 -4.05 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 175.97 -1.32
  .. .. .. .. .. .. ..@ area   : num 0.00187
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 175.95 176.02 175.93 175.95 -1.28 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 172.8 3.03
  .. .. .. .. .. .. ..@ area   : num 0.00185
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 172.87 172.77 172.75 172.87 3.06 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -171.66 -2.82
  .. .. .. .. .. .. ..@ area   : num 0.00176
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -171.63 -171.63 -171.73 -171.63 -2.83 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 173.85 0.42
  .. .. .. .. .. .. ..@ area   : num 0.00166
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.831 173.922 173.805 173.831 0.448 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 174.481 -0.747
  .. .. .. .. .. .. ..@ area   : num 0.00123
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 174.49 174.458 174.496 174.49 -0.817 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 174.8 -1.2
  .. .. .. .. .. .. ..@ area   : num 0.00122
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 174.77 174.72 174.77 174.77 -1.26 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 173.618 0.188
  .. .. .. .. .. .. ..@ area   : num 0.00111
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.618 173.604 173.632 173.618 0.134 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 175.58 -1.87
  .. .. .. .. .. .. ..@ area   : num 0.00102
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 175.59 175.55 175.59 175.59 -1.92 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 176.82 -2.64
  .. .. .. .. .. .. ..@ area   : num 0.000856
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 176.85 176.77 176.83 176.85 -2.67 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 176.43 -1.37
  .. .. .. .. .. .. ..@ area   : num 0.000822
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 176.46 176.39 176.42 176.46 -1.44 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -172.21 -4.51
  .. .. .. .. .. .. ..@ area   : num 0.000751
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -172.19 -172.23 -172.2 -172.19 -4.53 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -155.89 -5.63
  .. .. .. .. .. .. ..@ area   : num 0.000745
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -155.88 -155.93 -155.86 -155.88 -5.64 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -151.8 -11.4
  .. .. .. .. .. .. ..@ area   : num 0.000645
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -151.8 -151.8 -151.8 -151.8 -11.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 172.97 1.57
  .. .. .. .. .. .. ..@ area   : num 0.000629
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 172.98 172.96 172.96 172.98 1.53 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 173.33 1.99
  .. .. .. .. .. .. ..@ area   : num 0.000565
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.34 173.31 173.33 173.34 1.98 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 174.41 -0.615
  .. .. .. .. .. .. ..@ area   : num 0.000528
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 174.459 174.377 174.392 174.459 -0.652 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -160.39 4.71
  .. .. .. .. .. .. ..@ area   : num 0.000476
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -160.39 -160.41 -160.38 -160.39 4.69 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 173.33 1.95
  .. .. .. .. .. .. ..@ area   : num 0.00041
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.33 173.31 173.35 173.33 1.94 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 172.97 3.28
  .. .. .. .. .. .. ..@ area   : num 0.000395
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 172.97 172.96 172.98 172.97 3.25 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -171.24 -4.45
  .. .. .. .. .. .. ..@ area   : num 0.000354
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -171.23 -171.26 -171.24 -171.23 -4.46 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 173.11 1.36
  .. .. .. .. .. .. ..@ area   : num 0.000331
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.13 173.1 173.12 173.13 1.35 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 175.51 -1.78
  .. .. .. .. .. .. ..@ area   : num 0.000324
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 175.53 175.49 175.52 175.53 -1.81 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 173.05 1.42
  .. .. .. .. .. .. ..@ area   : num 0.000281
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.06 173.04 173.03 173.06 1.41 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 173.412 0.217
  .. .. .. .. .. .. ..@ area   : num 0.000277
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.41 173.402 173.426 173.41 0.204 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 172.86 1.85
  .. .. .. .. .. .. ..@ area   : num 0.000252
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 172.88 172.83 172.86 172.88 1.84 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -171.08 -3.13
  .. .. .. .. .. .. ..@ area   : num 0.000174
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -171.08 -171.09 -171.09 -171.08 -3.14 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -150.23 -9.94
  .. .. .. .. .. .. ..@ area   : num 0.000163
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -150.22 -150.23 -150.22 -150.22 -9.95 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 172.93 1.34
  .. .. .. .. .. .. ..@ area   : num 0.000144
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 172.94 172.91 172.94 172.94 1.34 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 176 -2.5
  .. .. .. .. .. .. ..@ area   : num 0.000142
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 176 176 176 176 -2.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 173.378 0.254
  .. .. .. .. .. .. ..@ area   : num 0.000124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.385 173.371 173.377 173.385 0.244 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 173.932 0.307
  .. .. .. .. .. .. ..@ area   : num 0.000111
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.932 173.924 173.939 173.932 0.298 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -150.2 -9.9
  .. .. .. .. .. .. ..@ area   : num 0.000102
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -150.23 -150.23 -150.22 -150.23 -9.92 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 174.86 -1.41
  .. .. .. .. .. .. ..@ area   : num 7.59e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 174.87 174.86 174.85 174.87 -1.41 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -152.4 -10.1
  .. .. .. .. .. .. ..@ area   : num 5.53e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -152.4 -152.4 -152.4 -152.4 -10.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 172.89 1.94
  .. .. .. .. .. .. ..@ area   : num 5.36e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 172.89 172.88 172.89 172.89 1.93 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 173.01 1.48
  .. .. .. .. .. .. ..@ area   : num 7.42e-06
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 173.01 173.02 173 173.01 1.47 ...
  .. .. .. ..@ plotOrder: int [1:42] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] -157.39 1.86
  .. .. .. ..@ ID       : chr "KIR"
  .. .. .. ..@ area     : num 0.0873
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 19
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 127.9 36.5
  .. .. .. .. .. .. ..@ area   : num 9.15
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:32, 1:2] 129 129 129 129 128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126.6 33.4
  .. .. .. .. .. .. ..@ area   : num 0.076
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126.6 126.2 126.9 126.6 33.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 128.6 34.8
  .. .. .. .. .. .. ..@ area   : num 0.0222
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 128.8 128.6 128.5 128.8 34.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 127.9 34.8
  .. .. .. .. .. .. ..@ area   : num 0.0182
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 128 128 128 128 128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126.3 34.5
  .. .. .. .. .. .. ..@ area   : num 0.0174
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126.2 126.2 126.4 126.2 34.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126.4 37.7
  .. .. .. .. .. .. ..@ area   : num 0.013
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126.5 126.4 126.4 126.5 37.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126.4 36.5
  .. .. .. .. .. .. ..@ area   : num 0.00727
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126.4 126.3 126.3 126.4 36.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126.7 34.3
  .. .. .. .. .. .. ..@ area   : num 0.00493
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126.8 126.7 126.7 126.8 34.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 130.9 37.5
  .. .. .. .. .. .. ..@ area   : num 0.00472
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 130.9 130.8 130.9 130.9 37.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 128 34.9
  .. .. .. .. .. .. ..@ area   : num 0.0038
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 128.1 128 128 128.1 34.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 127.2 34.5
  .. .. .. .. .. .. ..@ area   : num 0.0035
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 127.2 127.1 127.2 127.2 34.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126.1 35.1
  .. .. .. .. .. .. ..@ area   : num 0.00345
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126 126 126 126 35 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 125 38
  .. .. .. .. .. .. ..@ area   : num 0.00342
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 124.7 124.6 124.7 124.7 37.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126 34.9
  .. .. .. .. .. .. ..@ area   : num 0.00295
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126.1 126 126.1 126.1 34.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 127.8 34.6
  .. .. .. .. .. .. ..@ area   : num 0.0024
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 127.8 127.7 127.8 127.8 34.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126 34.8
  .. .. .. .. .. .. ..@ area   : num 0.00215
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126 126 125.9 126 34.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 127.8 34.5
  .. .. .. .. .. .. ..@ area   : num 0.00194
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 127.8 127.7 127.8 127.8 34.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126 34.7
  .. .. .. .. .. .. ..@ area   : num 0.00175
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126 125.9 126 126 34.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 126.2 36.8
  .. .. .. .. .. .. ..@ area   : num 6.43e-05
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 126.2 126.2 126.2 126.2 36.8 ...
  .. .. .. ..@ plotOrder: int [1:19] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 127.9 36.5
  .. .. .. ..@ ID       : chr "KOR"
  .. .. .. ..@ area     : num 9.34
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 4
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 47.6 29.3
  .. .. .. .. .. .. ..@ area   : num 1.53
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] 47.9 47.9 48.2 47.7 48 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 48.2 29.8
  .. .. .. .. .. .. ..@ area   : num 0.0322
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 48.2 48.2 48.4 48.2 29.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 48.3 29.4
  .. .. .. .. .. .. ..@ area   : num 0.00273
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 48.4 48.4 48.3 48.4 29.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 48.1 30
  .. .. .. .. .. .. ..@ area   : num 0.00208
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 48 48 48 48.1 48 ...
  .. .. .. ..@ plotOrder: int [1:4] 1 2 3 4
  .. .. .. ..@ labpt    : num [1:2] 47.6 29.3
  .. .. .. ..@ ID       : chr "KWT"
  .. .. .. ..@ area     : num 1.57
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 4
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 67.3 48.1
  .. .. .. .. .. .. ..@ area   : num 330
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:129, 1:2] 69.2 70.8 71.3 71 71.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 50.1 45
  .. .. .. .. .. .. ..@ area   : num 0.0162
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 50.1 50 50.1 50.1 44.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 53 46.3
  .. .. .. .. .. .. ..@ area   : num 0.00921
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 53 53 53 53.1 53 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 50.3 45
  .. .. .. .. .. .. ..@ area   : num 0.00395
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 50.3 50.3 50.3 50.3 44.9 ...
  .. .. .. ..@ plotOrder: int [1:4] 1 2 3 4
  .. .. .. ..@ labpt    : num [1:2] 67.3 48.1
  .. .. .. ..@ ID       : chr "KAZ"
  .. .. .. ..@ area     : num 330
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 103.7 18.5
  .. .. .. .. .. .. ..@ area   : num 19.6
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:45, 1:2] 108 107 107 106 106 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] 103.7 18.5
  .. .. .. ..@ ID       : chr "LAO"
  .. .. .. ..@ area     : num 19.6
  .. .. [list output truncated]
  ..@ plotOrder  : int [1:246] 145 175 24 209 30 21 9 71 98 8 ...
  ..@ bbox       : num [1:2, 1:2] -180 -90 180 83.6
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:2] "x" "y"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. ..@ projargs: chr " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
}
\details{
see maptools package
}
\source{
maptools package
}
\references{
maptools package
}
\examples{
\donttest{
data(world_country_polygons)
str(world_country_polygons)
}
}
\keyword{datasets}
