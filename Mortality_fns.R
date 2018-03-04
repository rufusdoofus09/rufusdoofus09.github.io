library(shiny)
library(dplyr)
library(tidyr)
library(rlang)
require(datasets) ## for state.name and state.abb

DataDir="D:/Data"
socialDataDir="CensusFactFinder_county2015ACS_15_5YR_DP03"              
educationDataDir="educationCensusFactFinder_county2015ACS_15_5YR_DP03"     
hospitalDataDir="hospitalsECN_2012_US_62A1"                               
ICD10_causeCodesFile="ICD10_causeCodes.csv"                                    
CDC_Mortality2015File="Linked Birth  Infant Death Records, 2007-2015.txt"       

healthExpendituresDataDir="WHO_2012healthExpenditures"                              
WHO_Mortality2012File="WHO_mortality2012.csv"                                   

UN_LiveBirths2015File="UNdata_Export_20180301_212744399.csv"
UN_LiveBirths2014File="UNdata_Export_20180301_215359378.csv"
UN_LiveBirths_canada2009File="UNdata_Export_20180301_220127819.csv"
UN_LiveBirths_italy2013File="UNdata_Export_20180301_220347367.csv"
UN_LiveBirths_2003_2015_File="UNdata_Export_20180303_094258156.csv"


UN_InfantDeath_all2009_2015File="UNdata_Export_20180301_220944747.csv"

GHO_infantMortality_allYears_File = "GHO_infantMortality.csv"
GHO_population_allYears_File = "GHO_population.csv"
GHO_infantMortality<-read.csv(file.path(DataDir,GHO_infantMortality_allYears_File),stringsAsFactors = F)
GHO_infantMortality %>% 
    select(Country,Year,Mortality_Rate=Infant.mortality.rate..probability.of.dying.between.birth.and.age.1.per.1000.live.births.) -> 
    GHO_infantMortality_1yr
GHO_infantMortality_1yr_byYear<-spread(GHO_infantMortality_1yr,Year,Mortality_Rate)

GHO_infantMortality_2015 <- select(GHO_infantMortality_1yr_byYear,Country,Mortality_Rate="2015")
GHO_infantMortality_2015 %>% 
    separate(Mortality_Rate,c("Mortality_Rate","error"),sep=" ") %>% 
    as_tibble() %>%
    select(Country,Mortality_Rate) %>%
    mutate(Mortality_Rate=as.numeric(Mortality_Rate)) ->
    GHO_infantMortality_2015

GHO_infantMortality_2015





GHO_population<-read.csv(file.path(DataDir,GHO_population_allYears_File),stringsAsFactors = F)

hdr1<-names(GHO_population)
hdr2<-GHO_population[1,]
names(GHO_population)<-paste(hdr1,hdr2)
GHO_population <- GHO_population[-1,]
GHO_population %>% 
    select(Country="X Country",Population_Thousands="Population..in.thousands..total 2015") ->
    GHO_population_2015

GHO_Mortality_Population <- merge(GHO_infantMortality_2015,GHO_population_2015,by="Country")

weighted_mortality_rate <- GHO_Mortality_Population$Mortality_Rate*GHO_Mortality_Population$Population_Thousands
weighted_mortality_rate_inclUS <- sum(weighted_mortality_rate)/sum(GHO_Mortality_Population$Population_Thousands)
weighted_mortality_rate_noUS <- sum(weighted_mortality_rate[1:19])/sum(GHO_Mortality_Population$Population_Thousands[1:19])
weighted_mortality_rate_US <- GHO_Mortality_Population$Mortality_Rate[20]

# Live birth data country
UN_LiveBirths_2003_2015 <- read.csv(file.path(DataDir,UN_LiveBirths_2003_2015_File),
                                    stringsAsFactors = F)
UN_LiveBirths_2003_2015 %>% filter(Month=="Total") -> UN_LiveBirths_2003_2015_total
UN_LiveBirths_2003_2015_byReliability <-
    spread(UN_LiveBirths_2003_2015_total,Reliability,Value)
UN_LiveBirths_2003_2015_byReliability %>%
    select(Country=Country.or.Area,Year,Final="Final figure, complete",Provisional="Provisional figure")

formatWorldData <- function() {
ICD10_causeCodes <- read.csv(file.path(DataDir,ICD10_causeCodesFile),
                          stringsAsFactors = F,header = F,
                          col.names=c("skip","cause_code","cause_of_death"))
ICD10_causeCodes %>% as_tibble() %>% mutate(skip=NULL) -> ICD10_causeCodes

WHO_Mortality2012<-read.csv(file.path(DataDir,WHO_Mortality2012File),skip=4)

x=1:nrow(WHO_Mortality2012)
pick=sample(x,100)
small_WHO_Mortality2012<-WHO_Mortality2012[pick,]

small_WHO_Mortality2012 <- small_WHO_Mortality2012[,1:10]
small_WHO_Mortality2012 %>% as_tibble() %>% mutate(Year=NULL, List=NULL,Format=NULL,All.ages=NULL,Age_0=X0,Age_1=X1) -> small_WHO_Mortality2012


WHO_Mortality2012 %>% as_tibble() %>% mutate(Year=NULL, List=NULL,Format=NULL,All.ages=NULL,Age_0=X0,Age_1=X1) -> WHO_Mortality2012
WHO_Mortality2012  %>% filter(Age_0+Age_1>0) -> WHO_Mortality2012

UN_InfantDeath_all2009_2015=read.csv(file.path(DataDir,UN_InfantDeath_all2009_2015File),stringsAsFactors = F)

UN_InfantDeath_all2009_2015 %>% group_by(Year,Country.or.Area) -> grouped_UN_InfantDeath
summarize(grouped_UN_InfantDeath) -> summ_UN_InfantDeath
summ_UN_InfantDeath %>% filter(!is.na(as.integer(Year))) -> summ_UN_InfantDeath
summ_UN_InfantDeath %>% ungroup() %>% 
    group_by(Country.or.Area) %>% 
    mutate(latest=max(Year)) -> latest

#table(latest$Country.or.Area,latest$latest)
#                                                     2008 2013 2014 2015
#Australia                                               0    0    9    0
#Austria                                                 0    8    0    0
#Belgium                                                 0    0    0   10
#Canada                                                  3    0    0    0
#Denmark                                                 0    0    9    0
#Finland                                                 0    0    9    0
#France                                                  0    0    9    0
#Germany                                                 0    0    9    0
#Iceland                                                 0    0    0   10
#Ireland                                                 0    0    9    0
#Italy                                                   0    0    9    0
#Japan                                                   0    0    0   10
#Netherlands                                             0    0    9    0
#New Zealand                                             0    0    0   10
#Norway                                                  0    0    9    0
#Spain                                                   0    0    9    0
#Sweden                                                  0    0    9    0
#Switzerland                                             0    0    9    0
#United Kingdom of Great Britain and Northern Ireland    0    0    8    0
#United States of America                                0    0    9    0
}

formatUSAData <- function()
    {
    socialData_cols_of_interestFile="ACS_15_5YR_DP03_metadata_of_interest.csv"
socialDataFile="ACS_15_5YR_DP03_with_ann.csv"
socialData_cols_of_interest<-read.csv(
    file.path(DataDir,socialDataDir,socialData_cols_of_interestFile),
    stringsAsFactors = F,header=F)
socialData_cols_of_interest %>% rename(short=V1,long=V2) -> socialData_cols_of_interest
trans_col <- function(x) (gsub("-",".",x))
#trans_col2 <- function(x) (gsub("(Estimate; |Percent; )","",x))
#socialData_cols_of_interest %>% mutate(short=trans_col(short),long=trans_col2(long)) -> socialData_cols_of_interest
socialData_cols_of_interest %>% mutate(short=trans_col(short)) -> socialData_cols_of_interest


#socialData_USA_2015<-read.csv(
#    file.path(DataDir,socialDataDir,socialDataFile),
#    stringsAsFactors = F,header=T,na.strings = "(X)",strip.white = TRUE)
# Read first header line only
socialData_USA_2015_names <-read.csv(
    file.path(DataDir,socialDataDir,socialDataFile),
    stringsAsFactors = F,header=T,nrows =1)
# Don't read second header line (annotations)
# if we want them they are in socialData_cols_of_interest$long
socialData_USA_2015 <-read.csv(
    file.path(DataDir,socialDataDir,socialDataFile),
    stringsAsFactors = F,header=F,skip=2,na.strings = c("(X)","-"),strip.white = TRUE)
names(socialData_USA_2015) = names(socialData_USA_2015_names)

socialData_USA_2015 %>% as_tibble() %>% 
    select(socialData_cols_of_interest$short) %>% 
    mutate(HC03_VC161=if_else(is.na(HC03_VC161),0.0,HC03_VC161),
           HC03_VC162=if_else(is.na(HC03_VC162),0.0,HC03_VC162),
           HC03_VC163=if_else(is.na(HC03_VC163),0.0,HC03_VC163)) -> 
    socialData_USA_2015


CDC_Mortality2015=read.delim(file.path(DataDir,CDC_Mortality2015File))
CDC_Mortality2015 %>% as_tibble() %>% filter(Notes=="") -> CDC_Mortality2015
getStateAbbr <- function(x) (gsub("^(.*, )([A-Z]{2})$","\\2",x))
CDC_Mortality2015 %>% mutate(state=getStateAbbr(County)) -> CDC_Mortality2015
CDC_Mortality2015 %>% mutate(Notes=NULL) -> CDC_Mortality2015


getState <- function(x) (gsub("^(.*, )([A-Za-z ]{2,30})$","\\2",x))
    
stateAbbr <- c(state.abb,"DC")
names(stateAbbr) <- c(state.name,"District of Columbia")

socialData_USA_2015 %>% mutate(state=stateAbbr[getState(GEO.display.label)],
                               County.Code=as.integer(GEO.id2)) -> 
    socialData_USA_2015 

merge(CDC_Mortality2015,socialData_USA_2015,by=c("County.Code","state"),all=TRUE) -> yyy
yyy %>% as_tibble() %>%
    mutate(county_group = if_else(is.na(County),
                                  paste0("Unidentified Counties, ",state),
                                  as.character(County))) -> zzz

# HC01_VC103: number of Families

#HC03_VC161,Percent; PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL - All families
#HC03_VC162,Percent; PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL - All families - With related children of the householder under 18 years
#HC03_VC163,Percent; PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL - All families - With related children of the householder under 18 years - With related children of the householder under 5 years only

#only have percentages for these so convert to numbers in order to sum for 
# unnamed counties and state totls
#Number of Families BELOW THE POVERTY LEVEL
#  HC03_VC161*HC01_VC103/100
#Number of Families BELOW THE POVERTY LEVEL - With related children of the householder under 18 years
#  HC03_VC162*HC01_VC103/100
#Number of Families BELOW THE POVERTY LEVEL - With related children of the householder under 5 years only
#  HC03_VC163*HC01_VC103/100
# will be NA for unidentified counties
zzz %>% mutate(HC01_VC161_bpl=HC03_VC161*HC01_VC103/100,
               HC01_VC162_bpl_18=HC03_VC162*HC01_VC103/100,
               HC01_VC163_bpl_5=HC03_VC163*HC01_VC103/100) -> zzz




# A tibble: 3,192 x 27
# Groups:   county_group, state [307]
# County.Code state County Deaths Births Death.Rate GEO.id2 GEO.display.label   HC01_VC03
#       <int> <chr> <fct>   <int>  <int> <fct>        <int> <chr>                   <int>
#    1        1001 AL    NA         NA     NA NA            1001 Autauga County, Al~     42801
# 2        1003 AL    NA         NA     NA NA            1003 Baldwin County, Al~    156379
# 3        1005 AL    NA         NA     NA NA            1005 Barbour County, Al~     21785
# 4        1007 AL    NA         NA     NA NA            1007 Bibb County, Alaba~     18339
# 5        1009 AL    NA         NA     NA NA            1009 Blount County, Ala~     45839
# 6        1011 AL    NA         NA     NA NA            1011 Bullock County, Al~      8653
# 7        1013 AL    NA         NA     NA NA            1013 Butler County, Ala~     16215
# 8        1015 AL    NA         NA     NA NA            1015 Calhoun County, Al~     93346
# 9        1017 AL    NA         NA     NA NA            1017 Chambers County, A~     27492
#10        1019 AL    NA         NA     NA NA            1019 Cherokee County, A~     21242
# ... with 3,182 more rows, and 19 more variables: HC01_VC04 <int>, HC01_VC05 <int>,
#   HC01_VC07 <int>, HC01_VC09 <int>, HC03_VC12 <dbl>, HC01_VC74 <int>, HC01_VC85 <int>,
#   HC01_VC86 <int>, HC01_VC103 <int>, HC01_VC118 <int>, HC01_VC130 <int>,
#   HC01_VC131 <int>, HC01_VC132 <int>, HC01_VC133 <int>, HC01_VC134 <int>,
#   HC03_VC161 <dbl>, HC03_VC162 <dbl>, HC03_VC163 <dbl>, county_group <chr>

#HC01_VC03 <int>, HC01_VC04 <int>, HC01_VC05 <int>,
#   HC01_VC07 <int>, HC01_VC09 <int>, HC01_VC74 <int>, HC01_VC85 <int>,
#   HC01_VC86 <int>, HC01_VC103 <int>, HC01_VC118 <int>, HC01_VC130 <int>,
#   HC01_VC131 <int>, HC01_VC132 <int>, HC01_VC133 <int>, HC01_VC134 <int>,

#HC03_VC12 <dbl>, 


# Mortality Rate is given in deaths per 1000 live births (not a percent)
zzz %>% group_by(state) %>% summarise(state_Deaths=sum(Deaths,na.rm = TRUE),
                          state_Births=sum(Births,na.rm = TRUE),
                          state_Mortality_rate = (state_Deaths*1000)/state_Births,
                          state_HC01_VC03=sum(HC01_VC03,na.rm = TRUE),
                          state_HC01_VC04=sum(HC01_VC04,na.rm = TRUE),
                          state_HC01_VC05=sum(HC01_VC05,na.rm = TRUE),
                          state_HC01_VC07=sum(HC01_VC07,na.rm = TRUE),
                          state_HC01_VC09=sum(HC01_VC09,na.rm = TRUE),
                          state_HC03_VC12_unemp_pct = (state_HC01_VC07*100)/state_HC01_VC05,
                          state_HC01_VC74=sum(HC01_VC74,na.rm = TRUE),
                          state_HC01_VC85_inc_med=sum((HC01_VC85/state_HC01_VC74)*HC01_VC74,na.rm = TRUE),
                          state_HC01_VC86_inc_mean=sum((HC01_VC86/state_HC01_VC74)*HC01_VC74,na.rm = TRUE),
                          state_HC01_VC103=sum(HC01_VC103,na.rm = TRUE),
                          state_HC01_VC118=sum(HC01_VC118,na.rm = TRUE),
                          state_HC01_VC130=sum(HC01_VC130,na.rm = TRUE),
                          state_HC01_VC131_hc=sum(HC01_VC131,na.rm = TRUE),
                          state_HC03_VC131_hc_pct=(state_HC01_VC131_hc*100)/state_HC01_VC130,
                          state_HC01_VC132_hc_priv=sum(HC01_VC132,na.rm = TRUE),
                          state_HC03_VC132_hc_priv_pct=(state_HC01_VC132_hc_priv*100)/state_HC01_VC130,
                          state_HC01_VC133_hc_pub=sum(HC01_VC133,na.rm = TRUE),
                          state_HC03_VC133_hc_pub_pct=(state_HC01_VC133_hc_pub*100)/state_HC01_VC130,
                          state_HC01_VC134_nohc=sum(HC01_VC134,na.rm = TRUE),
                          state_HC03_VC134_nohc_pct=(state_HC01_VC134_nohc*100)/state_HC01_VC130,
                          state_HC01_VC161_bpl=sum(HC01_VC161_bpl,na.rm = TRUE), 
                          state_HC03_VC161_bpl_pct=(state_HC01_VC161_bpl*100)/state_HC01_VC103, 
                          state_HC01_VC162_bpl_18=sum(HC01_VC162_bpl_18,na.rm = TRUE), 
                          state_HC03_VC162_bpl_18_pct=(state_HC01_VC162_bpl_18*100)/state_HC01_VC103, 
                          state_HC01_VC163_bpl_5=sum(HC01_VC163_bpl_5,na.rm = TRUE),
                          state_HC03_VC163_bpl_5_pct=(state_HC01_VC163_bpl_5*100)/state_HC01_VC103) -> 
    state_summary
state_summary %>% print

                 
zzz %>% group_by(county_group) %>% 
    summarise(county_Deaths=sum(Deaths,na.rm = TRUE),
              county_Births=sum(Births,na.rm = TRUE),
              county_Mortality_rate = (county_Deaths*1000)/county_Births,
              county_HC01_VC03=sum(HC01_VC03,na.rm = TRUE),
              county_HC01_VC04=sum(HC01_VC04,na.rm = TRUE),
              county_HC01_VC05=sum(HC01_VC05,na.rm = TRUE),
              county_HC01_VC07=sum(HC01_VC07,na.rm = TRUE),
              county_HC01_VC09=sum(HC01_VC09,na.rm = TRUE),
              county_HC03_VC12_unemp_pct = (county_HC01_VC07*100)/county_HC01_VC05,
              county_HC01_VC74=sum(HC01_VC74,na.rm = TRUE),
              county_HC01_VC85_inc_med=sum((HC01_VC85/county_HC01_VC74)*HC01_VC74,na.rm = TRUE),
              county_HC01_VC86_inc_mean=sum((HC01_VC86/county_HC01_VC74)*HC01_VC74,na.rm = TRUE),
              county_HC01_VC103=sum(HC01_VC103,na.rm = TRUE),
              county_HC01_VC118=sum(HC01_VC118,na.rm = TRUE),
              county_HC01_VC130=sum(HC01_VC130,na.rm = TRUE),
              # HC03_VC131 = HC01_VC131/HC01_VC130 * 100
              #Percent of noninstitutionalized With private health insurance
              # HC03_VC132 = HC01_VC132/HC01_VC130 * 100
              #Percent of noninstitutionalized With With public coverage
              # HC03_VC133 = HC01_VC133/HC01_VC130 * 100
              #Percent of noninstitutionalized With No health insurance coverage
              # HC03_VC134 = HC01_VC131/HC01_VC130 * 100
              county_HC01_VC131_hc=sum(HC01_VC131,na.rm = TRUE),
              county_HC03_VC131_hc_pct=(county_HC01_VC131_hc*100)/county_HC01_VC130,
              county_HC01_VC132_hc_priv=sum(HC01_VC132,na.rm = TRUE),
              county_HC03_VC132_hc_priv_pct=(county_HC01_VC132_hc_priv*100)/county_HC01_VC130,
              county_HC01_VC133_hc_pub=sum(HC01_VC133,na.rm = TRUE),
              county_HC03_VC133_hc_pub_pct=(county_HC01_VC133_hc_pub*100)/county_HC01_VC130,
              county_HC01_VC134_nohc=sum(HC01_VC134,na.rm = TRUE),
              county_HC03_VC134_nohc_pct=(county_HC01_VC134_nohc*100)/county_HC01_VC130,
              county_HC01_VC161_bpl=sum(HC01_VC161_bpl,na.rm = TRUE), 
              county_HC03_VC161_bpl_pct=(county_HC01_VC161_bpl*100)/county_HC01_VC103, 
              county_HC01_VC162_bpl_18=sum(HC01_VC162_bpl_18,na.rm = TRUE), 
              county_HC03_VC162_bpl_18_pct=(county_HC01_VC162_bpl_18*100)/county_HC01_VC103, 
              county_HC01_VC163_bpl_5=sum(HC01_VC163_bpl_5,na.rm = TRUE),
              county_HC03_VC163_bpl_5_pct=(county_HC01_VC163_bpl_5*100)/county_HC01_VC103) -> 
    county_summary
county_summary %>% print

    write.csv(state_summary,"./stateData.csv",row.names = F)
    write.csv(county_summary,"./countyData.csv",row.names = F)
    
    assign("county_summary",county_summary,envir = .GlobalEnv)
    assign("state_summary",state_summary,envir = .GlobalEnv)
    
}
                                                           
#Civilian labor force
# HC01_VC05 == HC01_VC11 == HC03_VC11

# unemployment rate = num unemployed/civ labor force * 100
# HC03_VC12 = HC01_VC07 *100/HC01_VC05
# civ labor force = civ labor force num employed + civ labor force unemployed
# HC01_VC05 = HC01_VC06+HC01_VC07
# labor force = civ labor force + Armed Forces
# HC01_VC04 = HC01_VC05   + HC01_VC08

#HC01_VC09
#Estimate; EMPLOYMENT STATUS - Population 16 years and over - Not in labor force

#HC01_VC03
#Estimate; EMPLOYMENT STATUS - Population 16 years and over
# Population 16 years and over = labor force + Not in labor force (student or retired?)
# HC01_VC04+HC01_VC09 = HC01_VC03
#Percent of Population 16 years and over that are In labor force
# HC03_VC04 = HC01_VC04/HC03_VC03 * 100

#HC03_VC09 
#Percent; EMPLOYMENT STATUS - Population 16 years and over - Not in labor force

# HC03_VC04 + HC03_VC09 == 100

# HC01_VC103: number of Families

#HC03_VC161,Percent; PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL - All families
#HC03_VC162,Percent; PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL - All families - With related children of the householder under 18 years
#HC03_VC163,Percent; PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL - All families - With related children of the householder under 18 years - With related children of the householder under 5 years only

#Number of Families BELOW THE POVERTY LEVEL
# HC03_VC161*HC01_VC103*100
#Number of Families BELOW THE POVERTY LEVEL - With related children of the householder under 18 years
# HC03_VC162*HC01_VC103*100
#Number of Families BELOW THE POVERTY LEVEL - With related children of the householder under 5 years only
# HC03_VC163*HC01_VC103*100

#HC01_VC74,Estimate; INCOME AND BENEFITS (IN 2015 INFLATION-ADJUSTED DOLLARS) - Total households
#HC01_VC85,Estimate; INCOME AND BENEFITS (IN 2015 INFLATION-ADJUSTED DOLLARS) - Total households - Median household income (dollars)
#HC01_VC86,Estimate; INCOME AND BENEFITS (IN 2015 INFLATION-ADJUSTED DOLLARS) - Total households - Mean household income (dollars)

# state_HC01_VC85=sum(HC01_VC85*HC01_VC74)/state_HC01_VC74
# state_HC01_VC86=sum(HC01_VC86*HC01_VC74)/state_HC01_VC74

#HC01_VC130,Estimate; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population
#HC01_VC131,Estimate; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population - With health insurance coverage
#HC01_VC132,Estimate; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population - With health insurance coverage - With private health insurance
#HC01_VC133,Estimate; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population - With health insurance coverage - With public coverage
#HC01_VC134,Estimate; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population - No health insurance coverage
#Percent of noninstitutionalized With health insurance coverage
# HC03_VC131 = HC01_VC131/HC01_VC130 * 100
#Percent of noninstitutionalized With private health insurance
# HC03_VC132 = HC01_VC132/HC01_VC130 * 100
#Percent of noninstitutionalized With With public coverage
# HC03_VC133 = HC01_VC133/HC01_VC130 * 100
#Percent of noninstitutionalized With No health insurance coverage
# HC03_VC134 = HC01_VC131/HC01_VC130 * 100
# some people have both private and public insurance, so HC03_VC132 + HC03_VC133 > 100%
# but HC03_VC131 + HC03_VC134 = 100%

columnText_county=c("County","Deaths","Births",               
                    "Mortality_rate","Population (16 years+)","Population ... - In labor force",            
                    "Population ... - In Civilian labor force","Population ... Civilian labor force - Unemployed","Population ... - Not in labor force",             
                    "Unemployment %","Total Number of households","Median household income (US$)",     
                    "Mean household income (US$)","Total Number of Families","Per capita income (dollars)",    
                    "Civilian noninstitutionalized population","Civilian ... population with Health Coverage","Health Coverage %" ,   
                    "Civilian ... population with Private Health insurance","Private Health insurance %","Civilian ... population with Public Coverage",
                    "Public Coverage %","Civilian ... population with No Health Coverage","No Health Coverage %",
                    "Number of Families below Poverty Level","Families below Poverty Level %","Number of Families below Poverty Level & children under 18",
                    "Families below Poverty Level & children under 18 %","Number of Families below Poverty Level & all children under 5","Families below Poverty Level & all children under 5 %")

columnName_county=c("county_group", "county_Deaths","county_Births",
                    "county_Mortality_rate","county_HC01_VC03","county_HC01_VC04",
                    "county_HC01_VC05","county_HC01_VC07","county_HC01_VC09",
                    "county_HC03_VC12_unemp_pct","county_HC01_VC74","county_HC01_VC85_inc_med",
                    "county_HC01_VC86_inc_mean","county_HC01_VC103","county_HC01_VC118",
                    "county_HC01_VC130","county_HC01_VC131_hc","county_HC03_VC131_hc_pct",
                    "county_HC01_VC132_hc_priv","county_HC03_VC132_hc_priv_pct","county_HC01_VC133_hc_pub",
                    "county_HC03_VC133_hc_pub_pct","county_HC01_VC134_nohc","county_HC03_VC134_nohc_pct",
                    "county_HC01_VC161_bpl","county_HC03_VC161_bpl_pct","county_HC01_VC162_bpl_18",
                    "county_HC03_VC162_bpl_18_pct","county_HC01_VC163_bpl_5","county_HC03_VC163_bpl_5_pct")

columnText_state=c("County","State","Deaths","Births",               
                   "Mortality_rate","Population  (16 years+)","Population ... - In labor force",            
                   "Population ... - In Civilian labor force","Population ... Civilian labor force - Unemployed","Population ... - Not in labor force",             
                   "Unemployment %","Total Number of households","Median household income (US$)",     
                   "Mean household income (US$)","Total Number of Families","Per capita income (dollars)",    
                   "Civilian noninstitutionalized population","Civilian ... population with Health Coverage","Health Coverage %" ,   
                   "Civilian ... population with Private Health insurance","Private Health insurance %","Civilian ... population with Public Coverage",
                   "Public Coverage %","Civilian ... population with No Health Coverage","No Health Coverage %",
                   "Number of Families below Poverty Level","Families below Poverty Level %","Number of Families below Poverty Level & children under 18",
                   "Families below Poverty Level & children under 18 %","Number of Families below Poverty Level & all children under 5","Families below Poverty Level & all children under 5 %")
columnName_state=c("county_group", "state", "state_Deaths","state_Births",
                   "state_Mortality_rate","state_HC01_VC03","state_HC01_VC04",
                   "state_HC01_VC05","state_HC01_VC07","state_HC01_VC09",
                   "state_HC03_VC12_unemp_pct","state_HC01_VC74","state_HC01_VC85_inc_med",
                   "state_HC01_VC86_inc_mean","state_HC01_VC103","state_HC01_VC118",
                   "state_HC01_VC130","state_HC01_VC131_hc","state_HC03_VC131_hc_pct",
                   "state_HC01_VC132_hc_priv","state_HC03_VC132_hc_priv_pct","state_HC01_VC133_hc_pub",
                   "state_HC03_VC133_hc_pub_pct","state_HC01_VC134_nohc","state_HC03_VC134_nohc_pct",
                   "state_HC01_VC161_bpl","state_HC03_VC161_bpl_pct","state_HC01_VC162_bpl_18",
                   "state_HC03_VC162_bpl_18_pct","state_HC01_VC163_bpl_5","state_HC03_VC163_bpl_5_pct")

columnText=c(             
             "Unemployment Rate",
             "Median household income (US$)",     
             "Mean household income (US$)",
             "Per capita income (US$)",    
             "% of Population with Health Coverage" ,   
             "% of Population with Private Health insurance",
             "% of Population with Public Coverage",
             "% of Population with No Health Coverage",
             "% of Families below Poverty Level",
             "% of Families with children under 18 below Poverty Level",
             "% of Families with all children under 5 below Poverty Level",
             "Total Births","Total Deaths","Mortality Rate",          
             "Population 16 years and over",
             "Population ... In labor force",            
             "Population ... In Civilian labor force",
             "Population ... In Civilian labor force - Unemployed",
             "Population ... Not in labor force",
             "Total Number of households",
             "Total Number of Families"
)
columnName=c(
             "HC03_VC12_unemp_pct",
             "HC01_VC85_inc_med",
             "HC01_VC86_inc_mean",
             "HC01_VC118",
             "HC03_VC131_hc_pct",
             "HC03_VC132_hc_priv_pct",
             "HC03_VC133_hc_pub_pct",
             "HC03_VC134_nohc_pct",
             "HC03_VC161_bpl_pct",
             "HC03_VC162_bpl_18_pct",
             "HC03_VC163_bpl_5_pct",
             "Births","Deaths","Mortality_rate",
             "HC01_VC03",
             "HC01_VC04",
             "HC01_VC05",
             "HC01_VC07",
             "HC01_VC09",
             "HC01_VC74",
             "HC01_VC103"
)

names(columnName_county) <- columnText_county
names(columnName_state) <- columnText_state

names(columnName) <- columnText

max_US_Mortality_rate_county<-max(county_summary$county_Mortality_rate)
max_US_Mortality_rate_state<-max(state_summary$state_Mortality_rate)
max_US_Mortality_rate<-max(max_US_Mortality_rate_county,max_US_Mortality_rate_state)