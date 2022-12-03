Dataset Creation
================
Melvin Coleman
12/2/2022

Let’s create the dataset we will utilize for the final project. Data was
pulled from the web via scarping and downloaded from websites. The
“data” folder contains the csv files of data downloaded and intended for
use.

### World Cup Records & Statistics

This data was loaded data from Wikipedia and contains record and
statistics of the overall team records of the FIFA World Cup.

``` r
wiki_list =
records_stats_html = read_html("https://en.wikipedia.org/wiki/FIFA_World_Cup_records_and_statistics") %>% 
  html_table(header = TRUE)

wc_stats =
  wiki_list[[2]]
  
wc_stats = 
  wc_stats %>% 
  janitor::clean_names() %>%
   rename(country = team) %>% 
  ## Delete rank(data ranked in order of WC winners) & team
  select(country, everything(), -rank) %>% 
  
  ## Clean dataset, remove weird characters in names 
  mutate(
     country = str_replace(country, "//[c]", "")
  )
  
  
wc_stats
```

    ## # A tibble: 80 × 10
    ##    country      part   pld     w     d     l    gf    ga gd      pts
    ##    <chr>       <int> <int> <int> <int> <int> <int> <int> <chr> <int>
    ##  1 Brazil         22   112    75    18    19   232   106 +126    243
    ##  2 Germany[c]     20   112    68    21    23   232   130 +102    225
    ##  3 Italy          18    83    45    21    17   128    77 +51     156
    ##  4 Argentina      18    85    46    15    24   144    96 +48     153
    ##  5 France         16    69    36    13    20   126    80 +46     121
    ##  6 England        16    72    31    22    19   100    66 +34     115
    ##  7 Spain          16    66    31    16    19   108    75 +33     109
    ##  8 Netherlands    11    54    30    13    11    94    50 +44     103
    ##  9 Uruguay        14    59    25    13    21    89    76 +13      88
    ## 10 Belgium        14    51    21    10    20    69    74 −5       73
    ## # … with 70 more rows

### Fifa Ranking [Fifa Rankings](https://www.2026worldcupnorthamerica.com/fifa-ranking/)

This dataset was created via web scraping.

``` r
wc_rank_html =
   read_html("https://www.2026worldcupnorthamerica.com/fifa-ranking/") 

rank_text = 
  wc_rank_html %>% 
  html_elements(".grippy-host , td:nth-child(1), td:nth-child(1)") %>% 
  html_text() 

rank_text
```

    ##   [1] "1"   "2"   "3"   "4"   "5"   "6"   "7"   "8"   "9"   "10"  "11"  "12" 
    ##  [13] "13"  "14"  "15"  "16"  "17"  "18"  "19"  "20"  "21"  "22"  "23"  "24" 
    ##  [25] "25"  "26"  "27"  "28"  "29"  "30"  "31"  "32"  "33"  "34"  "35"  "36" 
    ##  [37] "37"  "38"  "39"  "40"  "41"  "42"  "43"  "44"  "45"  "46"  "47"  "48" 
    ##  [49] "49"  "50"  "51"  "52"  "53"  "54"  "55"  "56"  "57"  "58"  "59"  "60" 
    ##  [61] "61"  "62"  "63"  "64"  "65"  "66"  "67"  "68"  "69"  "70"  "71"  "72" 
    ##  [73] "73"  "74"  "75"  "76"  "77"  "78"  "79"  "80"  "81"  "82"  "83"  "84" 
    ##  [85] "85"  "86"  "87"  "88"  "89"  "90"  "91"  "92"  "93"  "94"  "95"  "96" 
    ##  [97] "97"  "98"  "99"  "100" "101" "102" "103" "104" "105" "106" "107" "108"
    ## [109] "109" "110" "111" "112" "113" "114" "115" "116" "117" "118" "119" "120"
    ## [121] "121" "122" "123" "124" "125" "126" "127" "128" "129" "130" "131" "132"
    ## [133] "133" "134" "135" "136" "137" "138" "139" "140" "141" "142" "143" "144"
    ## [145] "145" "146" "147" "148" "149" "150" "151" "152" "153" "154" "155" "156"
    ## [157] "157" "158" "159" "160" "161" "162" "163" "164" "165" "166" "167" "168"
    ## [169] "169" "170" "171" "172" "173" "174" "175" "176" "177" "178" "179" "180"
    ## [181] "181" "182" "183" "184" "185" "186" "187" "188" "189" "190" "191" "192"
    ## [193] "193" "194" "195" "196" "197" "198" "199" "200" "201" "202" "203" "204"
    ## [205] "205" "206" "207" "208" "209" "210" "211"

``` r
country_text =
  wc_rank_html %>% 
  html_elements("td:nth-child(2)") %>% 
  html_text() 
  
country_text
```

    ##   [1] "Brazil"                        "Belgium"                      
    ##   [3] "Argentina"                     "France"                       
    ##   [5] "England"                       "Italy"                        
    ##   [7] "Spain"                         "Netherlands"                  
    ##   [9] "Portugal"                      "Denmark"                      
    ##  [11] "Germany"                       "Croatia"                      
    ##  [13] "Mexico"                        "Uruguay"                      
    ##  [15] "Switzerland"                   "USA"                          
    ##  [17] "Colombia"                      "Senegal"                      
    ##  [19] "Wales"                         "IR Iran"                      
    ##  [21] "Serbia"                        "Morocco"                      
    ##  [23] "Peru"                          "Japan"                        
    ##  [25] "Sweden"                        "Poland"                       
    ##  [27] "Ukraine"                       "Korea Republic"               
    ##  [29] "Chile"                         "Tunisia"                      
    ##  [31] "Costa Rica"                    "Nigeria"                      
    ##  [33] "Russia"                        "Austria"                      
    ##  [35] "Czechia"                       "Hungary"                      
    ##  [37] "Algeria"                       "Australia"                    
    ##  [39] "Egypt"                         "Scotland"                     
    ##  [41] "Canada"                        "Norway"                       
    ##  [43] "Cameroon"                      "Ecuador"                      
    ##  [45] "Türkiye"                       "Mali"                         
    ##  [47] "Paraguay"                      "Côte d’Ivoire"                
    ##  [49] "Republic of Ireland"           "Qatar"                        
    ##  [51] "Saudi Arabia"                  "Greece"                       
    ##  [53] "Romania"                       "Burkina Faso"                 
    ##  [55] "Slovakia"                      "Finland"                      
    ##  [57] "Venezuela"                     "Bosnia and Herzegovina"       
    ##  [59] "Northern Ireland"              "Panama"                       
    ##  [61] "Ghana"                         "Iceland"                      
    ##  [63] "Slovenia"                      "Jamaica"                      
    ##  [65] "North Macedonia"               "Albania"                      
    ##  [67] "South Africa"                  "Iraq"                         
    ##  [69] "Montenegro"                    "United Arab Emirates"         
    ##  [71] "Cabo Verde"                    "Bulgaria"                     
    ##  [73] "Congo DR"                      "El Salvador"                  
    ##  [75] "Oman"                          "Israel"                       
    ##  [77] "Uzbekistan"                    "Georgia"                      
    ##  [79] "China PR"                      "Honduras"                     
    ##  [81] "Gabon"                         "Bolivia"                      
    ##  [83] "Guinea"                        "Jordan"                       
    ##  [85] "Bahrain"                       "Curaçao"                      
    ##  [87] "Haiti"                         "Zambia"                       
    ##  [89] "Uganda"                        "Syria"                        
    ##  [91] "Benin"                         "Luxembourg"                   
    ##  [93] "Armenia"                       "Palestine"                    
    ##  [95] "Kyrgyz Republic"               "Vietnam"                      
    ##  [97] "Belarus"                       "Equatorial Guinea"            
    ##  [99] "Lebanon"                       "Congo"                        
    ## [101] "Kenya"                         "Madagascar"                   
    ## [103] "Mauritania"                    "Trinidad and Tobago"          
    ## [105] "New Zealand"                   "India"                        
    ## [107] "Kosovo"                        "Tajikistan"                   
    ## [109] "Estonia"                       "Cyprus"                       
    ## [111] "Thailand"                      "Korea DPR"                    
    ## [113] "Kazakhstan"                    "Mozambique"                   
    ## [115] "Namibia"                       "Guinea-Bissau"                
    ## [117] "Sierra Leone"                  "Guatemala"                    
    ## [119] "Angola"                        "Libya"                        
    ## [121] "Niger"                         "Faroe Islands"                
    ## [123] "Azerbaijan"                    "Malawi"                       
    ## [125] "Zimbabwe"                      "The Gambia"                   
    ## [127] "Togo"                          "Sudan"                        
    ## [129] "Comoros"                       "Tanzania"                     
    ## [131] "Antigua and Barbuda"           "Central African Republic"     
    ## [133] "Philippines"                   "Latvia"                       
    ## [135] "Turkmenistan"                  "Solomon Islands"              
    ## [137] "Rwanda"                        "Ethiopia"                     
    ## [139] "Suriname"                      "St Kitts and Nevis"           
    ## [141] "Burundi"                       "Nicaragua"                    
    ## [143] "Eswatini"                      "Lithuania"                    
    ## [145] "Hong Kong"                     "Malaysia"                     
    ## [147] "Lesotho"                       "Botswana"                     
    ## [149] "Kuwait"                        "Liberia"                      
    ## [151] "Andorra"                       "Indonesia"                    
    ## [153] "Dominican Republic"            "Maldives"                     
    ## [155] "Yemen"                         "Afghanistan"                  
    ## [157] "Chinese Taipei"                "Myanmar"                      
    ## [159] "Papua New Guinea"              "Singapore"                    
    ## [161] "New Caledonia"                 "Tahiti"                       
    ## [163] "Fiji"                          "Vanuatu"                      
    ## [165] "South Sudan"                   "Barbados"                     
    ## [167] "Cuba"                          "Malta"                        
    ## [169] "Bermuda"                       "Puerto Rico"                  
    ## [171] "Guyana"                        "St Lucia"                     
    ## [173] "Grenada"                       "Moldova"                      
    ## [175] "Nepal"                         "Belize"                       
    ## [177] "Cambodia"                      "St Vincent and the Grenadines"
    ## [179] "Montserrat"                    "Mauritius"                    
    ## [181] "Chad"                          "Macau"                        
    ## [183] "Mongolia"                      "Dominica"                     
    ## [185] "Bhutan"                        "São Tomé and Príncipe"        
    ## [187] "Laos"                          "American Samoa"               
    ## [189] "Cook Islands"                  "Brunei Darussalam"            
    ## [191] "Samoa"                         "Bangladesh"                   
    ## [193] "Djibouti"                      "Pakistan"                     
    ## [195] "Cayman Islands"                "Liechtenstein"                
    ## [197] "Tonga"                         "Timor-Leste"                  
    ## [199] "Seychelles"                    "Eritrea"                      
    ## [201] "Aruba"                         "Bahamas"                      
    ## [203] "Somalia"                       "Gibraltar"                    
    ## [205] "Guam"                          "Turks and Caicos Islands"     
    ## [207] "Sri Lanka"                     "US Virgin Islands"            
    ## [209] "British Virgin Islands"        "Anguilla"                     
    ## [211] "San Marino"

``` r
fifa_rankings = 
tibble(
  rank = rank_text,
  country = country_text
) 

fifa_rankings
```

    ## # A tibble: 211 × 2
    ##    rank  country    
    ##    <chr> <chr>      
    ##  1 1     Brazil     
    ##  2 2     Belgium    
    ##  3 3     Argentina  
    ##  4 4     France     
    ##  5 5     England    
    ##  6 6     Italy      
    ##  7 7     Spain      
    ##  8 8     Netherlands
    ##  9 9     Portugal   
    ## 10 10    Denmark    
    ## # … with 201 more rows

### Confederations Dataset

This dataset was downloaded as a csv file and limited to variables of
interest.

``` r
confederations_data = 
  read_csv(file = "data/fifa_countries_audience.csv", col_names = TRUE) %>% 
  janitor::clean_names() %>% 
  select(country, confederation)## Select variables of interest 
```

    ## Rows: 191 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): country, confederation
    ## dbl (3): population_share, tv_audience_share, gdp_weighted_share
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### Top goal scorers per country

This dataset was pulled from Wikipedia via web scraping.

``` r
goals_text_html =
   read_html("https://en.wikipedia.org/wiki/List_of_top_international_men%27s_football_goal_scorers_by_country")  %>% 
  html_table(header = TRUE)

goals_country_df = 
  goals_text_html[[1]]


goals_country_df =
  goals_country_df %>% 
  janitor::clean_names() %>% 
  select(country, player, goals)  %>% 
  
## There are countries with more than one goal scorer, let's fix this
  aggregate(player ~ country, FUN = paste, collapse = ' & ') %>% 
  arrange(country)
    
## Countries to worry about:
## Anguilla, Bulgaria, Curaçao, Denmark,Eritrea, Eswatini, Faroe Islands, France,
## Gibraltar, Iceland, Lebanon, Lesotho, Mongolia, Namibia, Palestine,Romania,
## Scotland, U.S. Virgin Islands, United States

goals_country_df
```

    ##                              country
    ## 1                        Afghanistan
    ## 2                            Albania
    ## 3                            Algeria
    ## 4                     American Samoa
    ## 5                            Andorra
    ## 6                             Angola
    ## 7                           Anguilla
    ## 8                Antigua and Barbuda
    ## 9                          Argentina
    ## 10                           Armenia
    ## 11                             Aruba
    ## 12                         Australia
    ## 13                           Austria
    ## 14                        Azerbaijan
    ## 15                           Bahamas
    ## 16                           Bahrain
    ## 17                        Bangladesh
    ## 18                          Barbados
    ## 19                           Belarus
    ## 20                           Belgium
    ## 21                            Belize
    ## 22                             Benin
    ## 23                           Bermuda
    ## 24                            Bhutan
    ## 25                           Bolivia
    ## 26            Bosnia and Herzegovina
    ## 27                          Botswana
    ## 28                            Brazil
    ## 29            British Virgin Islands
    ## 30                            Brunei
    ## 31                          Bulgaria
    ## 32                      Burkina Faso
    ## 33                           Burundi
    ## 34                          Cambodia
    ## 35                          Cameroon
    ## 36                            Canada
    ## 37                        Cape Verde
    ## 38                    Cayman Islands
    ## 39          Central African Republic
    ## 40                              Chad
    ## 41                             Chile
    ## 42                             China
    ## 43                    Chinese Taipei
    ## 44                          Colombia
    ## 45                           Comoros
    ## 46                             Congo
    ## 47                      Cook Islands
    ## 48                        Costa Rica
    ## 49                           Croatia
    ## 50                              Cuba
    ## 51                           Curaçao
    ## 52                            Cyprus
    ## 53                    Czech Republic
    ## 54                           Denmark
    ## 55                          Djibouti
    ## 56                          Dominica
    ## 57                Dominican Republic
    ## 58                          DR Congo
    ## 59                           Ecuador
    ## 60                             Egypt
    ## 61                       El Salvador
    ## 62                           England
    ## 63                 Equatorial Guinea
    ## 64                           Eritrea
    ## 65                           Estonia
    ## 66                          Eswatini
    ## 67                          Ethiopia
    ## 68                     Faroe Islands
    ## 69                              Fiji
    ## 70                           Finland
    ## 71                            France
    ## 72                             Gabon
    ## 73                            Gambia
    ## 74                           Georgia
    ## 75                           Germany
    ## 76                             Ghana
    ## 77                         Gibraltar
    ## 78                            Greece
    ## 79                           Grenada
    ## 80                              Guam
    ## 81                         Guatemala
    ## 82                            Guinea
    ## 83                     Guinea-Bissau
    ## 84                            Guyana
    ## 85                             Haiti
    ## 86                          Honduras
    ## 87                         Hong Kong
    ## 88                           Hungary
    ## 89                           Iceland
    ## 90                             India
    ## 91                         Indonesia
    ## 92                              Iran
    ## 93                              Iraq
    ## 94                            Israel
    ## 95                             Italy
    ## 96                       Ivory Coast
    ## 97                           Jamaica
    ## 98                             Japan
    ## 99                            Jordan
    ## 100                       Kazakhstan
    ## 101                            Kenya
    ## 102                           Kosovo
    ## 103                           Kuwait
    ## 104                       Kyrgyzstan
    ## 105                             Laos
    ## 106                           Latvia
    ## 107                          Lebanon
    ## 108                          Lesotho
    ## 109                          Liberia
    ## 110                            Libya
    ## 111                    Liechtenstein
    ## 112                        Lithuania
    ## 113                       Luxembourg
    ## 114                            Macau
    ## 115                       Madagascar
    ## 116                           Malawi
    ## 117                         Malaysia
    ## 118                         Maldives
    ## 119                             Mali
    ## 120                            Malta
    ## 121                       Mauritania
    ## 122                        Mauritius
    ## 123                           Mexico
    ## 124                          Moldova
    ## 125                         Mongolia
    ## 126                       Montenegro
    ## 127                       Montserrat
    ## 128                          Morocco
    ## 129                       Mozambique
    ## 130                          Myanmar
    ## 131                          Namibia
    ## 132                            Nepal
    ## 133                      Netherlands
    ## 134                    New Caledonia
    ## 135                      New Zealand
    ## 136                        Nicaragua
    ## 137                            Niger
    ## 138                          Nigeria
    ## 139                      North Korea
    ## 140                  North Macedonia
    ## 141                 Northern Ireland
    ## 142                           Norway
    ## 143                             Oman
    ## 144                         Pakistan
    ## 145                        Palestine
    ## 146                           Panama
    ## 147                 Papua New Guinea
    ## 148                         Paraguay
    ## 149                             Peru
    ## 150                      Philippines
    ## 151                           Poland
    ## 152                         Portugal
    ## 153                      Puerto Rico
    ## 154                            Qatar
    ## 155              Republic of Ireland
    ## 156                          Romania
    ## 157                           Rwanda
    ## 158            Saint Kitts and Nevis
    ## 159                      Saint Lucia
    ## 160 Saint Vincent and the Grenadines
    ## 161                            Samoa
    ## 162                       San Marino
    ## 163            São Tomé and Príncipe
    ## 164                     Saudi Arabia
    ## 165                         Scotland
    ## 166                          Senegal
    ## 167                           Serbia
    ## 168                       Seychelles
    ## 169                     Sierra Leone
    ## 170                        Singapore
    ## 171                         Slovakia
    ## 172                         Slovenia
    ## 173                  Solomon Islands
    ## 174                          Somalia
    ## 175                     South Africa
    ## 176                      South Korea
    ## 177                      South Sudan
    ## 178                  Soviet Union[b]
    ## 179                            Spain
    ## 180                        Sri Lanka
    ## 181                            Sudan
    ## 182                         Suriname
    ## 183                           Sweden
    ## 184                      Switzerland
    ## 185                            Syria
    ## 186                           Tahiti
    ## 187                       Tajikistan
    ## 188                         Tanzania
    ## 189                         Thailand
    ## 190                      Timor-Leste
    ## 191                             Togo
    ## 192                            Tonga
    ## 193              Trinidad and Tobago
    ## 194                          Tunisia
    ## 195                           Turkey
    ## 196                     Turkmenistan
    ## 197         Turks and Caicos Islands
    ## 198              U.S. Virgin Islands
    ## 199                           Uganda
    ## 200                          Ukraine
    ## 201             United Arab Emirates
    ## 202                    United States
    ## 203                          Uruguay
    ## 204                       Uzbekistan
    ## 205                          Vanuatu
    ## 206                        Venezuela
    ## 207                          Vietnam
    ## 208                            Wales
    ## 209                            Yemen
    ## 210                           Zambia
    ## 211                         Zimbabwe
    ##                                                      player
    ## 1                                          Faysal Shayesteh
    ## 2                                             Erjon Bogdani
    ## 3                                             Islam Slimani
    ## 4                                                 Ramin Ott
    ## 5                                             Ildefons Lima
    ## 6                                                      Akwá
    ## 7        Richard O'Connor & Terrence Rogers & Girdon Connor
    ## 8                                               Peter Byers
    ## 9                                              Lionel Messi
    ## 10                                       Henrikh Mkhitaryan
    ## 11                                             Ronald Gómez
    ## 12                                               Tim Cahill
    ## 13                                             Toni Polster
    ## 14                                          Gurban Gurbanov
    ## 15                                          Lesly St. Fleur
    ## 16                                        Ismail Abdullatif
    ## 17                             Ashraf Uddin Ahmed Chunnu[c]
    ## 18                                       Llewellyn Riley[c]
    ## 19                                       Maksim Romaschenko
    ## 20                                            Romelu Lukaku
    ## 21                                            Deon McCaulay
    ## 22                                       Stéphane Sessègnon
    ## 23                                             Shaun Goater
    ## 24                                        Chencho Gyeltshen
    ## 25                                           Marcelo Moreno
    ## 26                                               Edin Džeko
    ## 27                                     Jerome Ramatlhakwane
    ## 28                                                     Pelé
    ## 29                                        Avondale Williams
    ## 30                                           Shahrazen Said
    ## 31                          Dimitar Berbatov & Hristo Bonev
    ## 32                                          Moumouni Dagano
    ## 33                                       Fiston Abdul Razak
    ## 34                                             Hok Sochetra
    ## 35                                             Samuel Eto'o
    ## 36                                               Cyle Larin
    ## 37                                             Héldon Ramos
    ## 38                                         Lee Ramoon[e][c]
    ## 39                                             Hilaire Momi
    ## 40                                      Ezechiel N'Douassel
    ## 41                                           Alexis Sánchez
    ## 42                                              Hao Haidong
    ## 43                                            Chen Po-liang
    ## 44                                           Radamel Falcao
    ## 45                                  El Fardou Ben Nabouhane
    ## 46                                           Thievy Bifouma
    ## 47                                           Taylor Saghabi
    ## 48                                          Rolando Fonseca
    ## 49                                              Davor Šuker
    ## 50                                              Lester Moré
    ## 51                           Rangelo Janga & Leandro Bacuna
    ## 52                                    Michalis Konstantinou
    ## 53                                               Jan Koller
    ## 54                         Poul Nielsen & Jon Dahl Tomasson
    ## 55                                   Mahdi Houssein Mahabeh
    ## 56                                              Julian Wade
    ## 57                                            Jonathan Faña
    ## 58                                        Dieumerci Mbokani
    ## 59                                           Enner Valencia
    ## 60                                            Hossam Hassan
    ## 61                                           Raúl Díaz Arce
    ## 62                                             Wayne Rooney
    ## 63                                              Emilio Nsue
    ## 64  Berhane Aregai & Yonas Fesehaye & Yidnekachew Shimangus
    ## 65                                              Andres Oper
    ## 66                       Felix Badenhorst & Sabelo Ndzinisa
    ## 67                                           Getaneh Kebede
    ## 68                           Klæmint Olsen & Rógvi Jacobsen
    ## 69                                              Roy Krishna
    ## 70                                              Teemu Pukki
    ## 71                           Olivier Giroud & Thierry Henry
    ## 72                                Pierre-Emerick Aubameyang
    ## 73                                             Assan Ceesay
    ## 74                                          Shota Arveladze
    ## 75                                           Miroslav Klose
    ## 76                                             Asamoah Gyan
    ## 77                        Roy Chipolina[f] & Liam Walker[g]
    ## 78                                       Nikos Anastopoulos
    ## 79                                            Ricky Charles
    ## 80                                           Jason Cunliffe
    ## 81                                              Carlos Ruiz
    ## 82                                   Ibrahima Kandia Diallo
    ## 83                                                 Nando Có
    ## 84                                         Nigel Codrington
    ## 85                                           Emmanuel Sanon
    ## 86                                             Carlos Pavón
    ## 87                                              Chan Siu Ki
    ## 88                                            Ferenc Puskás
    ## 89                   Kolbeinn Sigþórsson & Eiður Guðjohnsen
    ## 90                                            Sunil Chhetri
    ## 91                                              Abdul Kadir
    ## 92                                                 Ali Daei
    ## 93                                            Hussein Saeed
    ## 94                                              Eran Zahavi
    ## 95                                                Gigi Riva
    ## 96                                            Didier Drogba
    ## 97                                            Luton Shelton
    ## 98                                       Kunishige Kamamoto
    ## 99                                         Hamza Al-Dardour
    ## 100                                          Ruslan Baltiev
    ## 101                                            William Ouma
    ## 102                                            Vedat Muriqi
    ## 103                                         Bashar Abdullah
    ## 104                                         Mirlan Murzayev
    ## 105                                      Visay Phaphouvanin
    ## 106                                      Māris Verpakovskis
    ## 107                       Vardan Ghazaryan & Hassan Maatouk
    ## 108                          Sera Motebang & Jane Thabantso
    ## 109                                             George Weah
    ## 110                                         Ali Al-Biski[c]
    ## 111                                             Mario Frick
    ## 112                                      Tomas Danilevičius
    ## 113                                               Leon Mart
    ## 114                                           Chan Kin Seng
    ## 115                                            Paulin Voavy
    ## 116                                            Kinnah Phiri
    ## 117                                          Mokhtar Dahari
    ## 118                                              Ali Ashfaq
    ## 119                                            Seydou Keita
    ## 120                                          Michael Mifsud
    ## 121                                                  Bessam
    ## 122                                           Daniel Imbert
    ## 123                                        Javier Hernández
    ## 124                                       Serghei Cleșcenco
    ## 125             Naranbold Nyam-Osor & Lümbengarav Donorovyn
    ## 126                                          Stevan Jovetić
    ## 127                                             Lyle Taylor
    ## 128                                             Ahmed Faras
    ## 129                                               Tico-Tico
    ## 130                                          Myo Hlaing Win
    ## 131                         Rudolf Bester & Peter Shalulile
    ## 132                         Nirajan Rayamajhi & Hari Khadka
    ## 133                                        Robin van Persie
    ## 134                                            Bertrand Kaï
    ## 135                                              Chris Wood
    ## 136                                            Juan Barrera
    ## 137                                      Victorien Adebayor
    ## 138                                          Rashidi Yekini
    ## 139                                            Jong Il-gwan
    ## 140                                            Goran Pandev
    ## 141                                             David Healy
    ## 142                                             Jørgen Juve
    ## 143                                          Hani Al-Dhabit
    ## 144                                           Muhammad Essa
    ## 145                             Fahed Attal & Ashraf Nu'man
    ## 146                                             Luis Tejada
    ## 147                                           Reggie Davani
    ## 148                                        Roque Santa Cruz
    ## 149                                          Paolo Guerrero
    ## 150                                       Phil Younghusband
    ## 151                                      Robert Lewandowski
    ## 152                                       Cristiano Ronaldo
    ## 153                                            Héctor Ramos
    ## 154                                          Mansour Muftah
    ## 155                                            Robbie Keane
    ## 156                             Adrian Mutu & Gheorghe Hagi
    ## 157                                        Olivier Karekezi
    ## 158                                             Keith Gumbs
    ## 159                                            Earl Jean[c]
    ## 160                                          Shandel Samuel
    ## 161                                       Desmond Fa'aiuaso
    ## 162                                              Andy Selva
    ## 163                                               Luís Leal
    ## 164                                          Majed Abdullah
    ## 165                              Denis Law & Kenny Dalglish
    ## 166                                              Sadio Mané
    ## 167                                     Aleksandar Mitrović
    ## 168                                           Philip Zialor
    ## 169                                          Mohamed Kallon
    ## 170                                             Fandi Ahmad
    ## 171                                            Marek Hamšík
    ## 172                                          Zlatko Zahovič
    ## 173                                          Commins Menapi
    ## 174                                Abdullahi Sheikh Mohamed
    ## 175                                          Benni McCarthy
    ## 176                                             Cha Bum-kun
    ## 177                                              James Moga
    ## 178                                            Oleg Blokhin
    ## 179                                             David Villa
    ## 180                                        Kasun Jayasuriya
    ## 181                                     Nasr Eddin Abbas[c]
    ## 182                                         Stefano Rijssel
    ## 183                                      Zlatan Ibrahimović
    ## 184                                          Alexander Frei
    ## 185                                         Firas Al-Khatib
    ## 186                                           Teaonui Tehau
    ## 187                                    Manuchekhr Dzhalilov
    ## 188                                            Mrisho Ngasa
    ## 189                                      Kiatisuk Senamuang
    ## 190                                             Rufino Gama
    ## 191                                       Emmanuel Adebayor
    ## 192                                            Unaloto Feao
    ## 193                                              Stern John
    ## 194                                             Issam Jemâa
    ## 195                                             Hakan Şükür
    ## 196                                       Wladimir Baýramow
    ## 197                                            Billy Forbes
    ## 198                               Jamie Browne & J. C. Mack
    ## 199                                           Emmanuel Okwi
    ## 200                                       Andriy Shevchenko
    ## 201                                            Ali Mabkhout
    ## 202                          Clint Dempsey & Landon Donovan
    ## 203                                             Luis Suárez
    ## 204                                        Maksim Shatskikh
    ## 205                                            Richard Iwai
    ## 206                                          Salomón Rondón
    ## 207                                            Lê Công Vinh
    ## 208                                             Gareth Bale
    ## 209                                             Ali Al-Nono
    ## 210                                         Godfrey Chitalu
    ## 211                                            Peter Ndlovu

### Population Data 2021 Population

``` r
pop_df = 
read_csv(file = "data/pop.csv", col_names = TRUE) %>% 
  janitor::clean_names() %>% 
  select(country, area, land_area_km)
```

    ## Rows: 205 Columns: 19
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): cca2, country
    ## dbl (17): pop2022, pop2021, pop2020, pop2050, pop2030, pop2015, pop2010, pop...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Now let’s combine all of our datasets to create our final dataset. The
datasets we have currently are: <br> - wc_stats (contains world cup
statistics & records) - fifa_rankings (official Fifa rankings 2022) -
goals_country_df (top goal scorers per country) - pop_df(land area of
countries)

We will combine these datasets by the `country` variable.

``` r
### Put all dataframes into a list & merge by country

dfs = list(wc_stats, fifa_rankings, goals_country_df, pop_df)

worldcup_dataset = 
  dfs
```
