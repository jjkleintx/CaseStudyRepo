# Case Study 1
Josh Klein  
11/1/2016  
This is an analysis of GDP data as supplied by the World Bank. There are 2 data sets involved that will be combined on a common country code.

The first set of data contains GDP (gross domestic product) by country and their ranking compared to the other countries in the dataset. Also included in this file are region totals of GDP.

The second set of data contains education statistics which are indictors describing education access, progression, completion, literacy, teachers, population, and expeditures by country and also region. For the purposes of this analysis the education statistics are not in scope, but there is a non-education indicator in this data set that will be used which is Income Group.

Income Group is a classification from the World Bank based on GNI (gross national income) per capita in US dollars. There are 4 distinct income groups with the highest level split into 2 sub-groups based on OECD (Organization for Economic Cooperation and Development) membership. The groups are:

<ul>
<li><b>Low Income</b> - GNI of $1,025 or less in 2015</li>
<li><b>Lower Middle Income</b> - GNI between $1,026 & $4,035 in 2015</li>
<li><b>Upper Middle Income</b> - GNI between $4,036 & $12,475 in 2015</li>
<li><b>High Income: nonOECD</b> - GNI of $12,476 or more in 2015 and not a member of OECD</li>
<li><b>High Income: OECD</b> - GNI of $12,476 or more in 2015 and also a member of OECD</li>
</ul>

####Libraries needed

```r
library(ggplot2)
library(scales)
```
####Setting the working directory

```r
setwd("/Users/joshuaklein/Documents/SMU/DoingDataScience1/CaseStudy1")
```
####Downloading the first file

```r
gdp <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",skip=4,header=TRUE)
str(gdp)
```

```
## 'data.frame':	326 obs. of  10 variables:
##  $ X  : Factor w/ 229 levels "","ABW","ADO",..: 215 38 102 51 68 72 28 174 99 93 ...
##  $ X.1: Factor w/ 194 levels "",".. Not available.  ",..: 3 104 115 126 137 148 159 170 181 4 ...
##  $ X.2: logi  NA NA NA NA NA NA ...
##  $ X.3: Factor w/ 229 levels "","  East Asia & Pacific",..: 218 52 107 82 77 217 37 172 105 98 ...
##  $ X.4: Factor w/ 205 levels ""," 1,008 "," 1,129 ",..: 40 178 143 100 66 63 61 58 57 16 ...
##  $ X.5: Factor w/ 7 levels "","a","b","c",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ X.6: logi  NA NA NA NA NA NA ...
##  $ X.7: logi  NA NA NA NA NA NA ...
##  $ X.8: logi  NA NA NA NA NA NA ...
##  $ X.9: logi  NA NA NA NA NA NA ...
```
####Cleaning the first file
The data in this file needs quite a bit of cleaning. There are no headers and also several columns containing blank (NA) data. First I will give the data descriptive headers.

```r
names(gdp) <- c("country_code", "ranking", "unused1", "economy","us_dollars","notations","unused2","unused3","unused4","unused5")
head(gdp)
```

```
##   country_code ranking unused1        economy   us_dollars notations
## 1          USA       1      NA  United States  16,244,600           
## 2          CHN       2      NA          China   8,227,103           
## 3          JPN       3      NA          Japan   5,959,718           
## 4          DEU       4      NA        Germany   3,428,131           
## 5          FRA       5      NA         France   2,612,878           
## 6          GBR       6      NA United Kingdom   2,471,784           
##   unused2 unused3 unused4 unused5
## 1      NA      NA      NA      NA
## 2      NA      NA      NA      NA
## 3      NA      NA      NA      NA
## 4      NA      NA      NA      NA
## 5      NA      NA      NA      NA
## 6      NA      NA      NA      NA
```
Since there were some notes at the end of the file that I want to preserve, this next piece of cleanup is creating a new column and populating the text at the bottom based on the letter used in the notations column

```r
gdp$new_notations <- 'NA'
gdp$new_notations[gdp$notations=='a'] <- "Includes former Spanish Sahara"
gdp$new_notations[gdp$notations=='b'] <- "Excludes South Sudan"
gdp$new_notations[gdp$notations=='c'] <- "Covers mainland Tanzania only"
gdp$new_notations[gdp$notations=='d'] <- "Data are for the area controlled by the government of the Republic of Cyprus"
gdp$new_notations[gdp$notations=='e'] <- "Excludes Abkhazia and South Ossetia"
gdp$new_notations[gdp$notations=='f'] <- "Excludes Transnistria"
```
The columns containing the rankings and the GDP data are currently text and will need to be changed to numeric for data analysis.

```r
gdp$ranking_new <- as.numeric(gsub("[^[:digit:]]","", gdp$ranking))
gdp$gdp_us_dollars <- as.numeric(gsub("[^[:digit:]]","", gdp$us_dollars))
```
The final cleanup of the first file will create a new data frame that drops the blank columns from the initial download.

```r
new_gdp <- gdp
new_gdp[["unused1"]] <- NULL
new_gdp[["unused2"]] <- NULL
new_gdp[["unused3"]] <- NULL
new_gdp[["unused4"]] <- NULL
new_gdp[["unused5"]] <- NULL
new_gdp[["ranking"]] <- NULL
new_gdp[["notations"]] <- NULL
new_gdp[["us_dollars"]] <- NULL
```
####Downloading the second file

```r
fed <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv",header=TRUE)
```
####Cleaning the second file
Compared to the first file this one is fairly clean and usable. The only thing needed is to change all the headers to use the same naming convention as the first data frame.

```r
names(fed) <-c("country_code", "country_name","income_group","region","lending_category","other_groups","currency_unit","latest_population_census","latest_household_survey","special_notes","national_accounts_base_yr","national_accounts_reference_yr","SNA","SNA_price_valuation","alt_conversion_factor","ppp_survey_yr","balance_of_pmts_manual_inUse","ext_debt_reporting_status","trade_system","govt_accounting_concept","imf_data_dissemination_std","income_expenditure_recent_source","vital_registration_complete","latest_ag_census","latest_industrial_data","latest_trade_data","latest_water_withdrawal_data","2_alpha_code", "wb2_code","table_name","short_name")
```
####Merging the files

```r
combined_gdp_1 <- merge(new_gdp, fed, by.x="country_code",by.y="country_code", all=TRUE)
attach(combined_gdp_1)
hist(gdp_us_dollars)
```

![](https://github.com/jjkleintx/CaseStudyRepo/blob/master/Images/CaseStudy1Hist.png)<!-- -->

```r
detach(combined_gdp_1)
```
Both sets of data contain GDP totals of region groups (e.g. South Asia, North America, etc) that do not represent individual countries and are therefore outside the scope of the analysis. Removing them will take care of many outliers.

```r
combined_gdp <- combined_gdp_1[ which(combined_gdp_1$income_group!=''), ]
```
####Question 1: How many IDs match in the combined file?
210 countries

There were 24 other matches for IDs that referred to region groups, but were removed since they are subtotals and not individual countries.

```r
sum( !is.na(combined_gdp$country_name) )
```

```
## [1] 210
```
####Question 2: In ascending order by GDP, what is the 13th country on the list?
St. Kitts & Nevis

```r
sorted_gdp <- combined_gdp[order(combined_gdp$gdp_us_dollars),]
sorted_gdp[13, 1:2]
```

```
##     country_code             economy
## 205          KNA St. Kitts and Nevis
```
####Question 3: What are the average GDP rankings for the "High income: OECD" and "High income:nonOECD" groups?

<ul>
<li><b>High income: nonOECD</b> = 91.91304</li>
<li><b>High income: OECD</b> = 32.96667</li>
</ul>

```r
aggregate(combined_gdp$ranking_new, list(income = combined_gdp$income_group), mean, na.rm=TRUE)
```

```
##                 income         x
## 1 High income: nonOECD  91.91304
## 2    High income: OECD  32.96667
## 3           Low income 133.72973
## 4  Lower middle income 107.70370
## 5  Upper middle income  92.13333
```
####Question 4: Plot the GDP for all of the countries. Use ggplot2 to color your plot by Income Group.

```r
ggplot(combined_gdp,na.rm=FALSE, aes(country_name, gdp_us_dollars, color=factor(income_group)))+geom_point() + scale_x_discrete(breaks=NULL)+ ggtitle("GDP by Country")+ theme(axis.title.x = element_blank(),legend.position = "right")+scale_y_continuous(name="GDP (US$)", labels = comma, limits=c(0, 2000000))+ labs(color = "Income Group")
```

```
## Warning: Removed 30 rows containing missing values (geom_point).
```

![](https://github.com/jjkleintx/CaseStudyRepo/blob/master/Images/CaseStudy1Scatter.png)<!-- -->




####Question 5: Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income Group. How many countries are Lower middle income but among the 38 nations with highest GDP?

There are 5 countries
<ul>
<li>People's Republic of China</li>
<li>Arab Republic of Egypt</li>
<li>Republic of Indonesia</li>
<li>Republic of India</li>
<li>Kingdom of Thailand</li>
</ul>

```r
combined_gdp <- within(combined_gdp, quartile <- as.integer(cut(gdp_us_dollars, quantile(gdp_us_dollars, probs=0:5/5, na.rm = TRUE), include.lowest=TRUE)))
quart_gdp <- combined_gdp[which(combined_gdp$quartile > 4 & combined_gdp$income_group == "Lower middle income"), ]
quart_gdp[c("country_name")]
```

```
##                   country_name
## 135 People's Republic of China
## 157     Arab Republic of Egypt
## 188      Republic of Indonesia
## 190          Republic of India
## 298        Kingdom of Thailand
```

####Conclusion
Based on this data there are 2 conclusions that can be made:
<ul>
<li>Average GDP rank increases as GNI increases, although it is not always a perfect match.</li>
<li>Economic system of a country impacts the GNI and therefore the income group assigned, but it doesn't necessarily impact GDP. The 5 countries with high GDP and lower income groups do not have pure capitalistic economies which impacts GNI. However, their GDPs are very high. The difference in the high GDP and lower GNI is because there is an element of a socialistic economy that distributes income more equally across the population.</li>
</ul>
