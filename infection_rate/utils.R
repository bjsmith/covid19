
library(shiny)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(sf)
library(dplyr)
library(spData)
library(leaflet)
library(ggplot2) # tidyverse vis package
library(shiny)   # for web applications
library(rnaturalearth)
library(rnaturalearthdata)
library(data.table)


show_leaflet <- function(data_to_show,primary_col,rounding_func,legend_title,
                         quant_grades=5,pal_reverse=TRUE){
  
  pal<-colorQuantile(palette="YlOrRd",domain= data_to_show[[primary_col]],n=quant_grades,reverse=pal_reverse)
  
  chloro_labels <- paste0(data_to_show$name_long, ": ", as.character(rounding_func(data_to_show[[primary_col]])))
  
  m <- leaflet(data_to_show) %>% 
    addProviderTiles(providers$OpenStreetMap) %>%
    addPolygons(data=data_to_show,weight=2,fillColor=~pal(data_to_show[[primary_col]]),
                fillOpacity = 0.8,
                highlightOptions = highlightOptions(color='white',weight=1,
                                                    bringToFront = TRUE),
                label=chloro_labels,
                labelOptions=labelOptions(textsize="15px")) %>% 
    addLegend("bottomright", pal = pal, values = ~data_to_show[[primary_col]],
              labFormat=function(type,cuts,p){
                n=length(cuts)
                return(paste0(rounding_func(cuts[-n])," to ", rounding_func(cuts[-1])))
                #return(quantile(world_with_covid_data_inc$TestsPerCase,))
              },
              
              #labFormat = labelFormat(transform=function(x){x}),
              title = legend_title,
              opacity = 1
    ) 
  return(m)
}



######### map visualization

get_world_with_supplements<-function(){
  #create a polygon for Singapore because we can't miss out Singapore!
  
  add_circle_polygon <- function(iso_a2,name_long,lat,long){
    polygon_generic<-rbind(
      c(-0.3,-0.1),
      c(-0.3,0.1),
      c(-0.1,0.3),
      c(0.1,0.3),
      c(0.3,0.1),
      c(0.3,-0.1),
      c(0.1,-0.3),
      c(-0.1,-0.3),
      c(-0.3,-0.1))
    
    sg_poly<-t(t(polygon_generic) + c(long,lat))
    sg_multipolygon_list <- list(list(
      sg_poly
    ))
    
    sg_mp<-st_multipolygon(sg_multipolygon_list)
    
    world_template <- world[world$name_long=="Malaysia",]
    
    world_template[,c("continent",      "area_km2",  "pop",       "lifeExp",   "gdpPercap")]<-NA
    world_template$iso_a2<-iso_a2
    world_template$name_long<-name_long
    world_template$geom[[1]]<-sg_mp
    return(world_template)
  }
  
  worldc<-sf::st_as_sf(data.table::rbindlist(list(
    world,
    add_circle_polygon("SG","Singapore",1.2,103.8),
    add_circle_polygon("WS","Western Samoa",-13.9,-171.9),
    add_circle_polygon("AS","American Samoa",-14.3,-170.7),
    add_circle_polygon("TO","Tonga",-21.2,-175.2),
    add_circle_polygon("PN","Pitcairn Islands",-25.1,-130.1),
    add_circle_polygon("KI","Kiribati",1.9,-157.4),
    add_circle_polygon("NR","Nauru",-0.53,155.9),
    add_circle_polygon("TV","Tuvalu",-8.5,179.1),
    add_circle_polygon("VU","Vanuatu",-15.7,167.1)
    )))
  
  return(worldc)
}



get_geomapped_covid_data <- function(life_exp_thresh=50,run_date=Sys.Date()){
  

  jh_cases_recovered<-readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  jh_cases_recovered$EventType<-"Recoveries"
  jh_cases_confirmed <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  jh_cases_confirmed$EventType<-"CasesConfirmed"
  jh_deaths<-readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  jh_deaths$EventType<-"Deaths"
  jh_data<-rbind(jh_cases_confirmed,jh_cases_recovered,jh_deaths)
  jh_bycountry<-jh_data %>% dplyr::select(-Lat,-Long,-`Province/State`) %>% group_by(`Country/Region`,EventType) %>% summarise_all(sum,na.rm=TRUE)
  
  jh_long<-jh_bycountry %>% tidyr::gather("Date","Count",3:ncol(.))
  jh_long$Date<-as.Date(jh_long$Date,format="%m/%d/%y")
  
  jh_dxc <- jh_long %>% tidyr::spread("EventType","Count")
  
  jh_country_mapping <- readr::read_csv("data/mapping/country_mapping_jh.csv")
  jh_dxc <- left_join(jh_dxc,jh_country_mapping,by=c("Country/Region" = "John_Hopkins_Name"))
  
  jh_dxc<- jh_dxc%>% mutate(ActiveCases = CasesConfirmed-Deaths-Recoveries)
  
  #now get life expectancy
  life_exp<-readr::read_csv("data/mapping/lifeexpectancy-verbose.csv") %>% 
    filter(GhoDisplay=="Life expectancy at birth (years)" & SexCode=="BTSX") %>%
    select(CountryCode,Numeric,YearCode) %>% group_by(CountryCode) %>% filter(YearCode==max(YearCode)) %>%ungroup
  
  colnames(life_exp)[colnames(life_exp)=="Numeric"]<-"LifeExp"
  life_exp<-rbind(life_exp,
                  tibble::as_tibble(data.frame(CountryCode="TWN","LifeExp"=80.4,"YearCode"=2017,stringsAsFactors = FALSE)))
  
  
  #mapping to get iso2 to iso3
  country_codes<-read.csv("data/mapping/country-codes.csv")
  country_iso_2_to_3_map<-country_codes[,c("ISO3166.1.Alpha.2","ISO3166.1.Alpha.3","official_name_en")]
  
  #### arrivals data
  #load New Zealand arrivals data.
  arrivals<-read_csv("data/stats-nz-arrivals-by-country.csv")
  #pick out the month we are interested in
  
  month_code <- paste0("2019M",str_pad(as.character(lubridate::month(run_date)),2,side="left",pad="0"))
  
  arrivals_this_month <- arrivals %>% 
    filter(`Total passenger movements by EVERY country of residence (Monthly)`==month_code) %>% 
    .[,2:ncol(.)] %>%
    tidyr::gather("Country","MonthlyArrivals")
  country_mapping_stats_nz <- read_csv("data/mapping/country_mapping_stats_nz_to_iso.csv")
  statsnz_arr <- left_join(arrivals_this_month,country_mapping_stats_nz,by=c("Country" = "Stats_NZ_Arrivals_Name"))
  
  worldc <- get_world_with_supplements()
  # world_large<-ne_countries(110,returnclass = "sf")
  # world_tiny_countries<-ne_countries(50,type="tiny_countries",returnclass = "sf")
  # world_tiny_countries<-world_tiny_countries[world_tiny_countries$formal_en %in% 
  #                   c("Independent State of Samoa",
  #                    "American Samoa",
  #                    "Kingdom of Tonga",
  #                    "Republic of Vanuatu",
  #                    "Republic of Kiribati",
  #                    "Republic of Nauru",
  #                    "Republic of Singapore",
  #                    "Tuvalu",
  #                    "Hong Kong Special Administrative Region, PRC",
  #                    "Macao Special Administrative Region, PRC",
  #                    "Pitcairn, Henderson, Ducie and Oeno Islands"
  #                    ) #select important pacific countries and other areas that aren't included in the main map.
  #                   ,
  # ]
  # world_large$geometry <-  world_large$geometry %>% st_cast("GEOMETRY") %>% st_cast("GEOMETRYCOLLECTION")
  # world_tiny_countries$geometry <-world_tiny_countries$geometry  %>% st_cast("GEOMETRY") %>% st_cast("GEOMETRYCOLLECTION")
  # common_cols<-intersect(colnames(world_tiny_countries),colnames(world_large))
  # world_full<-sf::st_as_sf(rbindlist(list(world_large[,common_cols],world_tiny_countries[,common_cols]),fill=TRUE))
  # 
  world_health<-worldc %>% 
    left_join(country_iso_2_to_3_map,by=c("iso_a2" = "ISO3166.1.Alpha.2"),name="iso_a2") %>%
    left_join(life_exp,by=c("ISO3166.1.Alpha.3"="CountryCode"),name="ISO3166.1.Alpha.3")
  
  ######load our world in data
  
  
  #load the test rates and prevalence of COVID-19
  owid_fullset<-readr::read_csv("https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv")
  
  # tests <- read.csv("data/mapping/full-list-daily-covid-19-tests-per-thousand.csv")
  # owid_fullset <-read.csv("data/mapping/owid-covid-data.csv",stringsAsFactors = FALSE)
  owid_fullset$date<-as.Date(as.character(owid_fullset$date))
  
  #adjustment for China deaths.
  #this is because a whole lot of deaths were attributed to the wrong date.
  # chn_extra_deaths<-owid_fullset[owid_fullset$iso_code=="CHN" & owid_fullset$date=="2020-04-17","new_deaths"]
  # owid_fullset[owid_fullset$iso_code=="CHN" & owid_fullset$date=="2020-04-17","new_deaths"]<-10
  # chn_base_deaths<-sum(owid_fullset[owid_fullset$iso_code=="CHN" ,"new_deaths"],na.rm = TRUE)
  # chn_prop_increase<-(chn_extra_deaths+chn_base_deaths)/chn_base_deaths
  # owid_fullset[owid_fullset$iso_code=="CHN" & !is.na(owid_fullset$new_deaths) %>% .$new_deaths <-
  #   (
  #     round(owid_fullset[owid_fullset$iso_code=="CHN",] %>% .[!is.na(.$new_deaths),"new_deaths"]*chn_prop_increase[[1]])
  #   )
  #no longer need this code because we only use data from the last three weeks anyway.
  #not sure how this is going to go if China isn't reporting data anymore.
  
  
  
  test_data_availability<-owid_fullset %>% group_by(date) %>% summarise(datacount=sum(!is.na(new_tests)))
  latest_date <- max(owid_fullset$date)
  date_period_begin<- latest_date - days(7)
  
  most_complete_testing_date<-filter(test_data_availability,datacount==max(test_data_availability$datacount))$date
  
  
  
  #let's try 7-day averages
  owid_7_day_average_testing_observable<-owid_fullset %>% 
    filter(date>=date_period_begin) %>% 
    select(-contains("total"))%>%select(-contains("tests_units"))%>%
    select(-continent) %>%
    group_by(iso_code,location) %>%
    summarise_all(mean,na.rm=TRUE)
  
  owid_7_day_average_testing_observable$TestsPerCase <- owid_7_day_average_testing_observable$new_tests/owid_7_day_average_testing_observable$new_cases
  world_with_covid_data<-
    #left_join(world,country_iso_2_to_3_map,by=c("iso_a2" = "ISO3166.1.Alpha.2"),name="iso_a2") %>%
    world_health %>%
    left_join(owid_7_day_average_testing_observable,by=c("ISO3166.1.Alpha.3" = "iso_code"))
  
  vals_to_include <- 
    (is.finite(world_with_covid_data$TestsPerCase) & !is.na(world_with_covid_data$TestsPerCase)
     & world_with_covid_data$LifeExp>=life_exp_thresh
    )
  
  world_with_covid_data_inc<-world_with_covid_data[vals_to_include,]
  
  #we could do the lags another day but right now we're just interested in that one 3 week average figure.
  
  #select the cases 3 weeks ago
  owid_7_day_cases_lagged<-owid_fullset %>% 
    filter(date>=(date_period_begin - days(21)) & date<(date_period_begin - days(14))) %>% 
    select(iso_code,location,contains("cases")) %>% 
    group_by(iso_code,location) %>%
    summarise_all(mean,na.rm=TRUE)
  
  #select the current deaths
  owid_7_day_deaths<-owid_fullset %>% 
    filter(date>=date_period_begin) %>% 
    select(iso_code,location,contains("deaths")) %>% 
    group_by(iso_code,location) %>%
    summarise_all(mean,na.rm=TRUE)
  
  
  
  deaths_with_lagged_cases <- owid_7_day_cases_lagged %>% left_join(owid_7_day_deaths)
  
  assumed_cfr<-0.005
  deaths_with_lagged_cases$InferredDetectionRate <- (assumed_cfr*deaths_with_lagged_cases$new_cases)/(deaths_with_lagged_cases$new_deaths)
  #if there are NO deaths then we infer detection rate is 100%
  deaths_with_lagged_cases[deaths_with_lagged_cases$new_deaths==0,"InferredDetectionRate"]<-1
  
  deaths_with_lagged_cases$CountryPopulation<-deaths_with_lagged_cases$total_cases/deaths_with_lagged_cases$total_cases_per_million*10^6
  #we want the inferred case population rate
  #this has to come from the john hopkins data because we can get active cases from that.
  jh_key_stats<-jh_dxc %>% ungroup %>% 
    filter(Date>=date_period_begin) %>% 
    select(CasesConfirmed,Deaths,Recoveries,ActiveCases, `ISO3166-1-Alpha-3`) %>%
    group_by(`ISO3166-1-Alpha-3`) %>%
    summarise_all(mean)
  deaths_with_lagged_cases <- deaths_with_lagged_cases %>% left_join(jh_key_stats,by=c("iso_code"="ISO3166-1-Alpha-3"))
  
  deaths_with_lagged_cases$InferredActiveCases <- (
    (deaths_with_lagged_cases$ActiveCases/deaths_with_lagged_cases$InferredDetectionRate)
  )
  
  deaths_with_lagged_cases$InferredActiveCasePopRate <- 
    (deaths_with_lagged_cases$InferredActiveCases/
       deaths_with_lagged_cases$CountryPopulation)
  
  deaths_with_lagged_cases$ActiveCasePopRate <- (
    (deaths_with_lagged_cases$ActiveCases/deaths_with_lagged_cases$CountryPopulation)
  )
  
  
  
  world_with_covid_data<-
    #left_join(world,country_iso_2_to_3_map,by=c("iso_a2" = "ISO3166.1.Alpha.2"),name="iso_a2") %>%
    world_health %>%
    left_join(deaths_with_lagged_cases,by=c("ISO3166.1.Alpha.3" = "iso_code")) %>%
    left_join(
      statsnz_arr %>% 
        filter(`ISO3166-1-Alpha-3`!="") %>% 
        select(MonthlyArrivals,`ISO3166-1-Alpha-3`),
      by=c("ISO3166.1.Alpha.3" =  "ISO3166-1-Alpha-3"))
  
  return(world_with_covid_data)
}

get_analysis_covid_data <- function(world_with_covid_data,quarantine_odds_override=NULL,travel_volume_weighting=1){
  print(quarantine_odds_override)
  print(travel_volume_weighting)
  
  world_with_covid_data$InferredDetectionRate[is.infinite(world_with_covid_data$InferredDetectionRate)]<-1
  
  
  ### calculate the probability of at least one COVID-19 case
  
  world_with_covid_data$MonthlyArrivals<-as.numeric(world_with_covid_data$MonthlyArrivals)
  world_with_covid_data$MonthlyArrivalsScaled<-world_with_covid_data$MonthlyArrivals*travel_volume_weighting

  #assume that each new person has an independent chance of carrying COVID-19
  #this is conservative when we are trying to work out the probability of at least one case
  #so the probability is going to be 
  #1-((Population-ProbableActiveCases)/Population)^MonthlyArrivals
  #if 10% of the population of 5000 has covid, and 20 of them come to NZ, that's
  #1-((5000-500)/5000)^20
  #1-(4500/5000)^20
  #0.8784233
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ProbabilityOfMoreThanZeroCases=1-((CountryPopulation-InferredActiveCases)/CountryPopulation)^MonthlyArrivalsScaled)
  
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ExpectedNumberOfCases=InferredActiveCasePopRate*MonthlyArrivalsScaled)
  
  
  
  #odds that a detainee in quarantine with COVID-19 leaves before being COVID-19
  if(is.null(quarantine_odds_override)){
    probability_an_infected_arrival_escapes_quarantine <- 1/50
    probability_an_infected_arrival_leaves_quarantine_undetected <- 0.01
    prob_infected_arr_reaches_community<-probability_an_infected_arrival_escapes_quarantine+probability_an_infected_arrival_leaves_quarantine_undetected
  }else{
    prob_infected_arr_reaches_community<-quarantine_odds_override
  }
  
  
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ProbabilityOfMoreThanZeroCommunityCases=
        1-((CountryPopulation-InferredActiveCases*prob_infected_arr_reaches_community)/CountryPopulation)^MonthlyArrivalsScaled)
  
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ExpectedNumberOfCasesInCommunity=InferredActiveCasePopRate*MonthlyArrivalsScaled*prob_infected_arr_reaches_community)
  
  
  world_with_covid_data$InfActiveCasesPerMillion <- world_with_covid_data$InferredActiveCasePopRate*10^6
  world_with_covid_data$InfActiveCasesPerThousand <- world_with_covid_data$InferredActiveCasePopRate*10^3
  world_with_covid_data$ActiveCasesPerThousand <- world_with_covid_data$ActiveCasePopRate*10^3
  world_with_covid_data$ActiveCasesPerMillion <- world_with_covid_data$ActiveCasePopRate*10^6

  return(world_with_covid_data)
}



