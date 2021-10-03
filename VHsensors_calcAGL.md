---
title: 'vildehaye sensors tutorials: Calculate Altitude by Pressure'
author: "sivan margalit"
date: "2021-03-30"
output:
  html_document:
    toc: true
    toc_float:
      collapsed: true
      smooth_scroll: true
---
## credits
This code is a translation of Sivan Toledo's MATLAB code  
The data, contributed by Dr. Yoni Vortman and Anat Levi, of navigating study done in Hula valey at 2020

## Background
After loading Temperature and pressure measurements from vildehaye sensors, you can calculate altitude by pressure data. 

For this calculation we need information about the environment barometric data on same day. We can download this information from the Israely Meteorological Service (IMS)

Accessing to IMS data requires a Token (access key).  [here a link](https://ims.gov.il/he/ObservationDataAPI) for more information.

### project structure
<ol>
<li>Data Folder:
  <ul> 
  <li>binary data from battery and barometric sensors.</li>
  <li>excel file of meteorology stations, downloaded from IMS</li> </ul>
</li>
  
<li>Function folder:
R file with 3 local functions</li>
<li>VHsensors_calcAltitude.Rproj : R project file, define the folder, where it located, as a working folder for R project</li>
</ol>




## R Code sample 
### loading libraries and local sources
The package "toolsForAtlas" contains the methods for read binary sensors data, and access to remote database by url address.

in functions folder we contains local source files contains functions, which are not in the package


```r
library(toolsForAtlas) # useful functions for atlas users
options("digits"=14)   # show long numbers till 14 digits
source("functions/pressures.R")
```

### loading data from binary files

```r
# loading battery data
bat_df<-read_BAT("data/tag-972001006611-BATMON.bin")
bat_df$TAG<-"972001006611"
head(bat_df, 4)
```

```
##         TIME temp    battery          TAG
## 1 1606841067   13 2.87109375 972001006611
## 2 1606841068   17 2.87109375 972001006611
## 3 1606841069   17 2.87109375 972001006611
## 4 1606841070   17 2.87109375 972001006611
```

```r
# loading barometric pressure data (in mbar units)
pressure_df<-read_BME("data/tag-972001006611-BME280.bin")
pressure_df$TAG<-"972001006611"
head(pressure_df, 4)
```

```
##         TIME  Pressure          TAG
## 1 1606841059 1006.2096 972001006611
## 2 1606841060 1006.1111 972001006611
## 3 1606841061 1006.1199 972001006611
## 4 1606841062 1006.1294 972001006611
```

### plot baromeric data
The file functions/pressures.R contains "plot_BME" function plotting barometric data.
This function has 4 arguments :
1. bme.df - is the sensor's data
2. reference.df - optional . a data frame of environment  barometric data, as a reference. when this data frame is supply the function add it to the BME plot
3. fromTime - optional. if it not supply the fromTime is the minimum time in the dataframe
4. toTime - optional. if it not supply the toTime is the maximum time in the dataframe

```r
bme.plot.1<-plot_BME(pressure_df)
#bme.plot.1
```

### read local meteorologic data
The file functions/pressures.R contains "get.ISM.data" function which download pressure and temperature data, from a close meteorologic station.  
In this example - it uses stattion in Har-Knaan, in Tzfat 

```r
ism.lst<-get.ISM.data(easting=257000,
                      northing=780000,
                      fromTime=min(pressure_df$TIME),
                      toTime=max(pressure_df$TIME),
                      ims.token)
```

```
## [1] 1192
##    [1] "גשם, טמפ', רוח, לחות"           
##    [2] "גשם, טמפ', רוח, לחות"           
##    [3] "גשם, טמפ', רוח, לחות"           
##    [4] "גשם, טמפ', רוח, לחות, קרינה"    
##    [5] "גשם, טמפ', רוח, לחות"           
##    [6] "גשם, טמפ', רוח, לחות"           
##    [7] "גשם, טמפ', לחות"                
##    [8] "גשם, טמפ', רוח, לחות"           
##    [9] "גשם, טמפ', רוח, לחות,לחץ"       
##   [10] "גשם, טמפ',לחות"                 
##   [11] "גשם, טמפ', רוח, לחות"           
##   [12] "גשם, טמפ', רוח, לחות"           
##   [13] "גשם, טמפ', רוח, לחות"           
##   [14] "גשם, טמפ', רוח, לחות,לחץ"       
##   [15] "גשם, טמפ', רוח, לחות"           
##   [16] "גשם, טמפ', רוח, לחות"           
##   [17] "גשם, טמפ', רוח, לחות,לחץ"       
##   [18] "גשם, טמפ', רוח, לחות,לחץ"       
##   [19] "גשם, טמפ', רוח, לחות"           
##   [20] "גשם, טמפ', רוח, לחות"           
##   [21] "גשם, טמפ', רוח, לחות"           
##   [22] "גשם, טמפ', רוח, לחות, קרינה"    
##   [23] "גשם, טמפ', רוח, לחות"           
##   [24] "גשם, טמפ', רוח, לחות"           
##   [25] "גשם, טמפ', רוח, לחות, קרינה"    
##   [26] "גשם, טמפ', רוח, לחות, קרינה"    
##   [27] "גשם, טמפ', רוח, לחות"           
##   [28] "גשם, טמפ', לחות"                
##   [29] "גשם, טמפ', רוח, לחות"           
##   [30] "גשם, טמפ', לחות"                
##   [31] "גשם, טמפ', רוח, לחות, קרינה"    
##   [32] "גשם, טמפ', רוח, לחות"           
##   [33] "גשם, טמפ', רוח, לחות"           
##   [34] "גשם, טמפ', לחות"                
##   [35] "גשם, טמפ', רוח, לחות"           
##   [36] "גשם, טמפ', רוח, לחות"           
##   [37] "גשם, טמפ', רוח, לחות"           
##   [38] "גשם, טמפ', רוח, לחות, קרינה"    
##   [39] "גשם, טמפ', לחות"                
##   [40] "גשם, טמפ', לחות"                
##   [41] "גשם, טמפ', רוח, לחות"           
##   [42] "גשם, טמפ', רוח, לחות"           
##   [43] "גשם, טמפ', לחות"                
##   [44] "גשם, טמפ', רוח, לחות"           
##   [45] "גשם, טמפ', רוח, לחות"           
##   [46] "גשם, טמפ', רוח, לחות"           
##   [47] "גשם, טמפ', רוח, לחות, קרינה,לחץ"
##   [48] "גשם, טמפ', רוח, לחות"           
##   [49] "גשם, טמפ', רוח, לחות"           
##   [50] "גשם, טמפ', רוח, לחות"           
##   [51] "גשם, טמפ', רוח, לחות"           
##   [52] "גשם, טמפ', לחות"                
##   [53] "גשם, טמפ', רוח, לחות"           
##   [54] "גשם, טמפ', רוח, לחות"           
##   [55] "גשם, טמפ', רוח, לחות"           
##   [56] "גשם, טמפ', רוח, לחות,לחץ"       
##   [57] "גשם, טמפ', רוח, לחות"           
##   [58] "גשם, טמפ', רוח, לחות, קרינה"    
##   [59] "גשם, טמפ', רוח, לחות"           
##   [60] "גשם, טמפ', לחות"                
##   [61] "גשם, טמפ', רוח, לחות"           
##   [62] "גשם, טמפ', רוח, לחות"           
##   [63] "גשם, טמפ', רוח, לחות, קרינה"    
##   [64] "גשם, טמפ', רוח, לחות"           
##   [65] "גשם, טמפ', לחות"                
##   [66] "גשם, טמפ', רוח, לחות"           
##   [67] "גשם, טמפ', רוח, לחות, קרינה"    
##   [68] "גשם, טמפ', רוח, לחות"           
##   [69] "גשם, טמפ', לחות"                
##   [70] "גשם, טמפ', רוח, לחות"           
##   [71] "גשם, טמפ', רוח, לחות, קרינה"    
##   [72] "גשם, טמפ', רוח, לחות, קרינה,לחץ"
##   [73] "גשם, טמפ', רוח, לחות, קרינה"    
##   [74] "גשם, טמפ', רוח, לחות"           
##   [75] "גשם, טמפ', רוח, לחות"           
##   [76] "גשם, טמפ', רוח, לחות, קרינה,לחץ"
##   [77] "גשם, טמפ', רוח, לחות, קרינה"    
##   [78] "גשם, טמפ', רוח, לחות"           
##   [79] "גשם, טמפ', רוח, לחות"           
##   [80] "גשם, טמפ', רוח, לחות"           
##   [81] "גשם, טמפ', רוח, לחות"           
##   [82] "גשם, טמפ', רוח, לחות"           
##   [83] "גשם, טמפ', רוח, לחות"           
##   [84] "גשם, טמפ', רוח, לחות, קרינה"    
##   [85] "גשם, טמפ', רוח, לחות, קרינה,לחץ"
##   [86] NA                               
##   [87] NA                               
##   [88] NA                               
##   [89] NA                               
##   [90] NA                               
##   [91] NA                               
##   [92] NA                               
##   [93] NA                               
##   [94] NA                               
##   [95] NA                               
##   [96] NA                               
##   [97] NA                               
##   [98] NA                               
##   [99] NA                               
##  [100] NA                               
##  [101] NA                               
##  [102] NA                               
##  [103] NA                               
##  [104] NA                               
##  [105] NA                               
##  [106] NA                               
##  [107] NA                               
##  [108] NA                               
##  [109] NA                               
##  [110] NA                               
##  [111] NA                               
##  [112] NA                               
##  [113] NA                               
##  [114] NA                               
##  [115] NA                               
##  [116] NA                               
##  [117] NA                               
##  [118] NA                               
##  [119] NA                               
##  [120] NA                               
##  [121] NA                               
##  [122] NA                               
##  [123] NA                               
##  [124] NA                               
##  [125] NA                               
##  [126] NA                               
##  [127] NA                               
##  [128] NA                               
##  [129] NA                               
##  [130] NA                               
##  [131] NA                               
##  [132] NA                               
##  [133] NA                               
##  [134] NA                               
##  [135] NA                               
##  [136] NA                               
##  [137] NA                               
##  [138] NA                               
##  [139] NA                               
##  [140] NA                               
##  [141] NA                               
##  [142] NA                               
##  [143] NA                               
##  [144] NA                               
##  [145] NA                               
##  [146] NA                               
##  [147] NA                               
##  [148] NA                               
##  [149] NA                               
##  [150] NA                               
##  [151] NA                               
##  [152] NA                               
##  [153] NA                               
##  [154] NA                               
##  [155] NA                               
##  [156] NA                               
##  [157] NA                               
##  [158] NA                               
##  [159] NA                               
##  [160] NA                               
##  [161] NA                               
##  [162] NA                               
##  [163] NA                               
##  [164] NA                               
##  [165] NA                               
##  [166] NA                               
##  [167] NA                               
##  [168] NA                               
##  [169] NA                               
##  [170] NA                               
##  [171] NA                               
##  [172] NA                               
##  [173] NA                               
##  [174] NA                               
##  [175] NA                               
##  [176] NA                               
##  [177] NA                               
##  [178] NA                               
##  [179] NA                               
##  [180] NA                               
##  [181] NA                               
##  [182] NA                               
##  [183] NA                               
##  [184] NA                               
##  [185] NA                               
##  [186] NA                               
##  [187] NA                               
##  [188] NA                               
##  [189] NA                               
##  [190] NA                               
##  [191] NA                               
##  [192] NA                               
##  [193] NA                               
##  [194] NA                               
##  [195] NA                               
##  [196] NA                               
##  [197] NA                               
##  [198] NA                               
##  [199] NA                               
##  [200] NA                               
##  [201] NA                               
##  [202] NA                               
##  [203] NA                               
##  [204] NA                               
##  [205] NA                               
##  [206] NA                               
##  [207] NA                               
##  [208] NA                               
##  [209] NA                               
##  [210] NA                               
##  [211] NA                               
##  [212] NA                               
##  [213] NA                               
##  [214] NA                               
##  [215] NA                               
##  [216] NA                               
##  [217] NA                               
##  [218] NA                               
##  [219] NA                               
##  [220] NA                               
##  [221] NA                               
##  [222] NA                               
##  [223] NA                               
##  [224] NA                               
##  [225] NA                               
##  [226] NA                               
##  [227] NA                               
##  [228] NA                               
##  [229] NA                               
##  [230] NA                               
##  [231] NA                               
##  [232] NA                               
##  [233] NA                               
##  [234] NA                               
##  [235] NA                               
##  [236] NA                               
##  [237] NA                               
##  [238] NA                               
##  [239] NA                               
##  [240] NA                               
##  [241] NA                               
##  [242] NA                               
##  [243] NA                               
##  [244] NA                               
##  [245] NA                               
##  [246] NA                               
##  [247] NA                               
##  [248] NA                               
##  [249] NA                               
##  [250] NA                               
##  [251] NA                               
##  [252] NA                               
##  [253] NA                               
##  [254] NA                               
##  [255] NA                               
##  [256] NA                               
##  [257] NA                               
##  [258] NA                               
##  [259] NA                               
##  [260] NA                               
##  [261] NA                               
##  [262] NA                               
##  [263] NA                               
##  [264] NA                               
##  [265] NA                               
##  [266] NA                               
##  [267] NA                               
##  [268] NA                               
##  [269] NA                               
##  [270] NA                               
##  [271] NA                               
##  [272] NA                               
##  [273] NA                               
##  [274] NA                               
##  [275] NA                               
##  [276] NA                               
##  [277] NA                               
##  [278] NA                               
##  [279] NA                               
##  [280] NA                               
##  [281] NA                               
##  [282] NA                               
##  [283] NA                               
##  [284] NA                               
##  [285] NA                               
##  [286] NA                               
##  [287] NA                               
##  [288] NA                               
##  [289] NA                               
##  [290] NA                               
##  [291] NA                               
##  [292] NA                               
##  [293] NA                               
##  [294] NA                               
##  [295] NA                               
##  [296] NA                               
##  [297] NA                               
##  [298] NA                               
##  [299] NA                               
##  [300] NA                               
##  [301] NA                               
##  [302] NA                               
##  [303] NA                               
##  [304] NA                               
##  [305] NA                               
##  [306] NA                               
##  [307] NA                               
##  [308] NA                               
##  [309] NA                               
##  [310] NA                               
##  [311] NA                               
##  [312] NA                               
##  [313] NA                               
##  [314] NA                               
##  [315] NA                               
##  [316] NA                               
##  [317] NA                               
##  [318] NA                               
##  [319] NA                               
##  [320] NA                               
##  [321] NA                               
##  [322] NA                               
##  [323] NA                               
##  [324] NA                               
##  [325] NA                               
##  [326] NA                               
##  [327] NA                               
##  [328] NA                               
##  [329] NA                               
##  [330] NA                               
##  [331] NA                               
##  [332] NA                               
##  [333] NA                               
##  [334] NA                               
##  [335] NA                               
##  [336] NA                               
##  [337] NA                               
##  [338] NA                               
##  [339] NA                               
##  [340] NA                               
##  [341] NA                               
##  [342] NA                               
##  [343] NA                               
##  [344] NA                               
##  [345] NA                               
##  [346] NA                               
##  [347] NA                               
##  [348] NA                               
##  [349] NA                               
##  [350] NA                               
##  [351] NA                               
##  [352] NA                               
##  [353] NA                               
##  [354] NA                               
##  [355] NA                               
##  [356] NA                               
##  [357] NA                               
##  [358] NA                               
##  [359] NA                               
##  [360] NA                               
##  [361] NA                               
##  [362] NA                               
##  [363] NA                               
##  [364] NA                               
##  [365] NA                               
##  [366] NA                               
##  [367] NA                               
##  [368] NA                               
##  [369] NA                               
##  [370] NA                               
##  [371] NA                               
##  [372] NA                               
##  [373] NA                               
##  [374] NA                               
##  [375] NA                               
##  [376] NA                               
##  [377] NA                               
##  [378] NA                               
##  [379] NA                               
##  [380] NA                               
##  [381] NA                               
##  [382] NA                               
##  [383] NA                               
##  [384] NA                               
##  [385] NA                               
##  [386] NA                               
##  [387] NA                               
##  [388] NA                               
##  [389] NA                               
##  [390] NA                               
##  [391] NA                               
##  [392] NA                               
##  [393] NA                               
##  [394] NA                               
##  [395] NA                               
##  [396] NA                               
##  [397] NA                               
##  [398] NA                               
##  [399] NA                               
##  [400] NA                               
##  [401] NA                               
##  [402] NA                               
##  [403] NA                               
##  [404] NA                               
##  [405] NA                               
##  [406] NA                               
##  [407] NA                               
##  [408] NA                               
##  [409] NA                               
##  [410] NA                               
##  [411] NA                               
##  [412] NA                               
##  [413] NA                               
##  [414] NA                               
##  [415] NA                               
##  [416] NA                               
##  [417] NA                               
##  [418] NA                               
##  [419] NA                               
##  [420] NA                               
##  [421] NA                               
##  [422] NA                               
##  [423] NA                               
##  [424] NA                               
##  [425] NA                               
##  [426] NA                               
##  [427] NA                               
##  [428] NA                               
##  [429] NA                               
##  [430] NA                               
##  [431] NA                               
##  [432] NA                               
##  [433] NA                               
##  [434] NA                               
##  [435] NA                               
##  [436] NA                               
##  [437] NA                               
##  [438] NA                               
##  [439] NA                               
##  [440] NA                               
##  [441] NA                               
##  [442] NA                               
##  [443] NA                               
##  [444] NA                               
##  [445] NA                               
##  [446] NA                               
##  [447] NA                               
##  [448] NA                               
##  [449] NA                               
##  [450] NA                               
##  [451] NA                               
##  [452] NA                               
##  [453] NA                               
##  [454] NA                               
##  [455] NA                               
##  [456] NA                               
##  [457] NA                               
##  [458] NA                               
##  [459] NA                               
##  [460] NA                               
##  [461] NA                               
##  [462] NA                               
##  [463] NA                               
##  [464] NA                               
##  [465] NA                               
##  [466] NA                               
##  [467] NA                               
##  [468] NA                               
##  [469] NA                               
##  [470] NA                               
##  [471] NA                               
##  [472] NA                               
##  [473] NA                               
##  [474] NA                               
##  [475] NA                               
##  [476] NA                               
##  [477] NA                               
##  [478] NA                               
##  [479] NA                               
##  [480] NA                               
##  [481] NA                               
##  [482] NA                               
##  [483] NA                               
##  [484] NA                               
##  [485] NA                               
##  [486] NA                               
##  [487] NA                               
##  [488] NA                               
##  [489] NA                               
##  [490] NA                               
##  [491] NA                               
##  [492] NA                               
##  [493] NA                               
##  [494] NA                               
##  [495] NA                               
##  [496] NA                               
##  [497] NA                               
##  [498] NA                               
##  [499] NA                               
##  [500] NA                               
##  [501] NA                               
##  [502] NA                               
##  [503] NA                               
##  [504] NA                               
##  [505] NA                               
##  [506] NA                               
##  [507] NA                               
##  [508] NA                               
##  [509] NA                               
##  [510] NA                               
##  [511] NA                               
##  [512] NA                               
##  [513] NA                               
##  [514] NA                               
##  [515] NA                               
##  [516] NA                               
##  [517] NA                               
##  [518] NA                               
##  [519] NA                               
##  [520] NA                               
##  [521] NA                               
##  [522] NA                               
##  [523] NA                               
##  [524] NA                               
##  [525] NA                               
##  [526] NA                               
##  [527] NA                               
##  [528] NA                               
##  [529] NA                               
##  [530] NA                               
##  [531] NA                               
##  [532] NA                               
##  [533] NA                               
##  [534] NA                               
##  [535] NA                               
##  [536] NA                               
##  [537] NA                               
##  [538] NA                               
##  [539] NA                               
##  [540] NA                               
##  [541] NA                               
##  [542] NA                               
##  [543] NA                               
##  [544] NA                               
##  [545] NA                               
##  [546] NA                               
##  [547] NA                               
##  [548] NA                               
##  [549] NA                               
##  [550] NA                               
##  [551] NA                               
##  [552] NA                               
##  [553] NA                               
##  [554] NA                               
##  [555] NA                               
##  [556] NA                               
##  [557] NA                               
##  [558] NA                               
##  [559] NA                               
##  [560] NA                               
##  [561] NA                               
##  [562] NA                               
##  [563] NA                               
##  [564] NA                               
##  [565] NA                               
##  [566] NA                               
##  [567] NA                               
##  [568] NA                               
##  [569] NA                               
##  [570] NA                               
##  [571] NA                               
##  [572] NA                               
##  [573] NA                               
##  [574] NA                               
##  [575] NA                               
##  [576] NA                               
##  [577] NA                               
##  [578] NA                               
##  [579] NA                               
##  [580] NA                               
##  [581] NA                               
##  [582] NA                               
##  [583] NA                               
##  [584] NA                               
##  [585] NA                               
##  [586] NA                               
##  [587] NA                               
##  [588] NA                               
##  [589] NA                               
##  [590] NA                               
##  [591] NA                               
##  [592] NA                               
##  [593] NA                               
##  [594] NA                               
##  [595] NA                               
##  [596] NA                               
##  [597] NA                               
##  [598] NA                               
##  [599] NA                               
##  [600] NA                               
##  [601] NA                               
##  [602] NA                               
##  [603] NA                               
##  [604] NA                               
##  [605] NA                               
##  [606] NA                               
##  [607] NA                               
##  [608] NA                               
##  [609] NA                               
##  [610] NA                               
##  [611] NA                               
##  [612] NA                               
##  [613] NA                               
##  [614] NA                               
##  [615] NA                               
##  [616] NA                               
##  [617] NA                               
##  [618] NA                               
##  [619] NA                               
##  [620] NA                               
##  [621] NA                               
##  [622] NA                               
##  [623] NA                               
##  [624] NA                               
##  [625] NA                               
##  [626] NA                               
##  [627] NA                               
##  [628] NA                               
##  [629] NA                               
##  [630] NA                               
##  [631] NA                               
##  [632] NA                               
##  [633] NA                               
##  [634] NA                               
##  [635] NA                               
##  [636] NA                               
##  [637] NA                               
##  [638] NA                               
##  [639] NA                               
##  [640] NA                               
##  [641] NA                               
##  [642] NA                               
##  [643] NA                               
##  [644] NA                               
##  [645] NA                               
##  [646] NA                               
##  [647] NA                               
##  [648] NA                               
##  [649] NA                               
##  [650] NA                               
##  [651] NA                               
##  [652] NA                               
##  [653] NA                               
##  [654] NA                               
##  [655] NA                               
##  [656] NA                               
##  [657] NA                               
##  [658] NA                               
##  [659] NA                               
##  [660] NA                               
##  [661] NA                               
##  [662] NA                               
##  [663] NA                               
##  [664] NA                               
##  [665] NA                               
##  [666] NA                               
##  [667] NA                               
##  [668] NA                               
##  [669] NA                               
##  [670] NA                               
##  [671] NA                               
##  [672] NA                               
##  [673] NA                               
##  [674] NA                               
##  [675] NA                               
##  [676] NA                               
##  [677] NA                               
##  [678] NA                               
##  [679] NA                               
##  [680] NA                               
##  [681] NA                               
##  [682] NA                               
##  [683] NA                               
##  [684] NA                               
##  [685] NA                               
##  [686] NA                               
##  [687] NA                               
##  [688] NA                               
##  [689] NA                               
##  [690] NA                               
##  [691] NA                               
##  [692] NA                               
##  [693] NA                               
##  [694] NA                               
##  [695] NA                               
##  [696] NA                               
##  [697] NA                               
##  [698] NA                               
##  [699] NA                               
##  [700] NA                               
##  [701] NA                               
##  [702] NA                               
##  [703] NA                               
##  [704] NA                               
##  [705] NA                               
##  [706] NA                               
##  [707] NA                               
##  [708] NA                               
##  [709] NA                               
##  [710] NA                               
##  [711] NA                               
##  [712] NA                               
##  [713] NA                               
##  [714] NA                               
##  [715] NA                               
##  [716] NA                               
##  [717] NA                               
##  [718] NA                               
##  [719] NA                               
##  [720] NA                               
##  [721] NA                               
##  [722] NA                               
##  [723] NA                               
##  [724] NA                               
##  [725] NA                               
##  [726] NA                               
##  [727] NA                               
##  [728] NA                               
##  [729] NA                               
##  [730] NA                               
##  [731] NA                               
##  [732] NA                               
##  [733] NA                               
##  [734] NA                               
##  [735] NA                               
##  [736] NA                               
##  [737] NA                               
##  [738] NA                               
##  [739] NA                               
##  [740] NA                               
##  [741] NA                               
##  [742] NA                               
##  [743] NA                               
##  [744] NA                               
##  [745] NA                               
##  [746] NA                               
##  [747] NA                               
##  [748] NA                               
##  [749] NA                               
##  [750] NA                               
##  [751] NA                               
##  [752] NA                               
##  [753] NA                               
##  [754] NA                               
##  [755] NA                               
##  [756] NA                               
##  [757] NA                               
##  [758] NA                               
##  [759] NA                               
##  [760] NA                               
##  [761] NA                               
##  [762] NA                               
##  [763] NA                               
##  [764] NA                               
##  [765] NA                               
##  [766] NA                               
##  [767] NA                               
##  [768] NA                               
##  [769] NA                               
##  [770] NA                               
##  [771] NA                               
##  [772] NA                               
##  [773] NA                               
##  [774] NA                               
##  [775] NA                               
##  [776] NA                               
##  [777] NA                               
##  [778] NA                               
##  [779] NA                               
##  [780] NA                               
##  [781] NA                               
##  [782] NA                               
##  [783] NA                               
##  [784] NA                               
##  [785] NA                               
##  [786] NA                               
##  [787] NA                               
##  [788] NA                               
##  [789] NA                               
##  [790] NA                               
##  [791] NA                               
##  [792] NA                               
##  [793] NA                               
##  [794] NA                               
##  [795] NA                               
##  [796] NA                               
##  [797] NA                               
##  [798] NA                               
##  [799] NA                               
##  [800] NA                               
##  [801] NA                               
##  [802] NA                               
##  [803] NA                               
##  [804] NA                               
##  [805] NA                               
##  [806] NA                               
##  [807] NA                               
##  [808] NA                               
##  [809] NA                               
##  [810] NA                               
##  [811] NA                               
##  [812] NA                               
##  [813] NA                               
##  [814] NA                               
##  [815] NA                               
##  [816] NA                               
##  [817] NA                               
##  [818] NA                               
##  [819] NA                               
##  [820] NA                               
##  [821] NA                               
##  [822] NA                               
##  [823] NA                               
##  [824] NA                               
##  [825] NA                               
##  [826] NA                               
##  [827] NA                               
##  [828] NA                               
##  [829] NA                               
##  [830] NA                               
##  [831] NA                               
##  [832] NA                               
##  [833] NA                               
##  [834] NA                               
##  [835] NA                               
##  [836] NA                               
##  [837] NA                               
##  [838] NA                               
##  [839] NA                               
##  [840] NA                               
##  [841] NA                               
##  [842] NA                               
##  [843] NA                               
##  [844] NA                               
##  [845] NA                               
##  [846] NA                               
##  [847] NA                               
##  [848] NA                               
##  [849] NA                               
##  [850] NA                               
##  [851] NA                               
##  [852] NA                               
##  [853] NA                               
##  [854] NA                               
##  [855] NA                               
##  [856] NA                               
##  [857] NA                               
##  [858] NA                               
##  [859] NA                               
##  [860] NA                               
##  [861] NA                               
##  [862] NA                               
##  [863] NA                               
##  [864] NA                               
##  [865] NA                               
##  [866] NA                               
##  [867] NA                               
##  [868] NA                               
##  [869] NA                               
##  [870] NA                               
##  [871] NA                               
##  [872] NA                               
##  [873] NA                               
##  [874] NA                               
##  [875] NA                               
##  [876] NA                               
##  [877] NA                               
##  [878] NA                               
##  [879] NA                               
##  [880] NA                               
##  [881] NA                               
##  [882] NA                               
##  [883] NA                               
##  [884] NA                               
##  [885] NA                               
##  [886] NA                               
##  [887] NA                               
##  [888] NA                               
##  [889] NA                               
##  [890] NA                               
##  [891] NA                               
##  [892] NA                               
##  [893] NA                               
##  [894] NA                               
##  [895] NA                               
##  [896] NA                               
##  [897] NA                               
##  [898] NA                               
##  [899] NA                               
##  [900] NA                               
##  [901] NA                               
##  [902] NA                               
##  [903] NA                               
##  [904] NA                               
##  [905] NA                               
##  [906] NA                               
##  [907] NA                               
##  [908] NA                               
##  [909] NA                               
##  [910] NA                               
##  [911] NA                               
##  [912] NA                               
##  [913] NA                               
##  [914] NA                               
##  [915] NA                               
##  [916] NA                               
##  [917] NA                               
##  [918] NA                               
##  [919] NA                               
##  [920] NA                               
##  [921] NA                               
##  [922] NA                               
##  [923] NA                               
##  [924] NA                               
##  [925] NA                               
##  [926] NA                               
##  [927] NA                               
##  [928] NA                               
##  [929] NA                               
##  [930] NA                               
##  [931] NA                               
##  [932] NA                               
##  [933] NA                               
##  [934] NA                               
##  [935] NA                               
##  [936] NA                               
##  [937] NA                               
##  [938] NA                               
##  [939] NA                               
##  [940] NA                               
##  [941] NA                               
##  [942] NA                               
##  [943] NA                               
##  [944] NA                               
##  [945] NA                               
##  [946] NA                               
##  [947] NA                               
##  [948] NA                               
##  [949] NA                               
##  [950] NA                               
##  [951] NA                               
##  [952] NA                               
##  [953] NA                               
##  [954] NA                               
##  [955] NA                               
##  [956] NA                               
##  [957] NA                               
##  [958] NA                               
##  [959] NA                               
##  [960] NA                               
##  [961] NA                               
##  [962] NA                               
##  [963] NA                               
##  [964] NA                               
##  [965] NA                               
##  [966] NA                               
##  [967] NA                               
##  [968] NA                               
##  [969] NA                               
##  [970] NA                               
##  [971] NA                               
##  [972] NA                               
##  [973] NA                               
##  [974] NA                               
##  [975] NA                               
##  [976] NA                               
##  [977] NA                               
##  [978] NA                               
##  [979] NA                               
##  [980] NA                               
##  [981] NA                               
##  [982] NA                               
##  [983] NA                               
##  [984] NA                               
##  [985] NA                               
##  [986] NA                               
##  [987] NA                               
##  [988] NA                               
##  [989] NA                               
##  [990] NA                               
##  [991] NA                               
##  [992] NA                               
##  [993] NA                               
##  [994] NA                               
##  [995] NA                               
##  [996] NA                               
##  [997] NA                               
##  [998] NA                               
##  [999] NA                               
## [1000] NA                               
##  [ reached getOption("max.print") -- omitted 192 entries ]
## [1] "׳\234׳—׳¥"
## character(0)
```

```r
print(ism.lst[[1]])
```

```
## numeric(0)
```

```r
print(length(ism.lst[[2]]))
```

```
## [1] 0
```

```r
#ism.pressure.df<-ism.lst[["pressure"]]
#hsl<-ism.lst[["hsl"]]
```
