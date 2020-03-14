#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


library(readr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(directlabels)
library(dplyr)
library(plotly)

confirmed_cases<-data.table(read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
confirmed_cases<-confirmed_cases[,
                                 names(which(!apply(confirmed_cases,2,function(ccc){all(is.na(ccc))}))),
                                 with=FALSE]
transform_cols<-colnames(confirmed_cases)[5:ncol(confirmed_cases)]
id_vars <- c("Country/Region","Province/State","Lat","Long")
confirmed_cases_long <- melt(confirmed_cases,
                             id.vars=id_vars,
                             measure.vars = transform_cols,
                             variable.name = "case_date",
                             value.name = "Confirmed Cases"
)

confirmed_cases_long_bycountry <- confirmed_cases_long[,.(`Confirmed Cases`=sum(`Confirmed Cases`)),by=.(`Country/Region`,case_date)]

confirmed_cases_long_bycountry$case_date<-as.Date(confirmed_cases_long_bycountry$case_date,format="%m/%d/%y")

#make it easy to see the US & UK
confirmed_cases_long_bycountry[`Country/Region`=="US",
                               `Country/Region`:="United States",
                               ]
confirmed_cases_long_bycountry[`Country/Region`=="UK",
                               `Country/Region`:="United Kingdom",
                               ]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("COVID-19 progression"),
   
   # Sidebar with a slider input for number of bins 
   flowLayout(
        numericInput("reference_count",
                    "Line up cases at",
                    min = 1,
                    max = 2000,
                    value = 20)
        # selectInput("location","Reference Country or Region",
        #             ),
        
         
      ),
      hr(),
      
        fluidRow(
          column(width = 6,
                 plotlyOutput("distPlot")
          ),
          column(width=6,
                 plotlyOutput("bydate")
          )
          )
   

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
   
   output$distPlot <- renderPlotly({
     
     threshold<-input$reference_count
     
     #Remove China if it was above the threshold on the first day. we don't do this to any other countries
     #but China is further along than all others.
     if(threshold<min(confirmed_cases_long_bycountry[`Country/Region`=="China",`Confirmed Cases`])){
       confirmed_cases_long_bycountry <- 
         confirmed_cases_long_bycountry[`Country/Region`!="China"]
     }

     
     #mark which days have reached the threshold
     confirmed_cases_long_bycountry[,IsAtOrPastThreshold:=(`Confirmed Cases`>=threshold)]
     
     #identify the first day for each country that has reached the threshold
     confirmed_cases_long_bycountry[,
                                    DatePastThreshold:=min(.SD[IsAtOrPastThreshold==TRUE,case_date]),
                                    by=`Country/Region`]
     
     #then calculate the date relative to the threshold
     confirmed_cases_long_bycountry[,DaysPastThreshold:=as.integer(case_date-DatePastThreshold)]
     
     
     case_limit<-40
     days_limit <- 21

     #now we want a list of the lines
     #with an x axis point either at the end of the graph,
     #or at the end of the line, whichever is first,     
     country_labels<-confirmed_cases_long_bycountry[,
                                    .(intercept_day=min(days_limit,max(DaysPastThreshold))),
                                    by=`Country/Region`] %>% .[!is.na(intercept_day)] %>%
       #and then at that x-axis point, we want the y-axis point.  
       merge(confirmed_cases_long_bycountry[,.(`Country/Region`,DaysPastThreshold,`Confirmed Cases`,DatePastThreshold)],
                           by.x=c("Country/Region","intercept_day"),
                           by.y=c("Country/Region","DaysPastThreshold"),
             all.x=TRUE,all.y=FALSE)
 
     x_breaks  <- seq(-7,days_limit,7)
     x_labels <- c(paste(abs(x_breaks[1]),"Days Before"),
                   x_breaks[2:(length(x_breaks)-1)],
                   paste(x_breaks[length(x_breaks)],"Days After"))
     myplot<-ggplot(confirmed_cases_long_bycountry
            # [`Country/Region` %in%
            #                                  c("New Zealand","Australia",
            #                                    "Canada","Denmark","Singapore",
            #                                    "Hong Kong","US")]
            ,
            aes(x=DaysPastThreshold,y=`Confirmed Cases`,
                group=`Country/Region`,color=DatePastThreshold))+
       geom_line(size=1,alpha=0.3)+
       #geom_point(alpha=0.5)+
       scale_x_continuous(limits=c(-7,days_limit),
                          breaks=x_breaks,
                          labels = x_labels)+
       coord_cartesian(xlim=c(-7,days_limit+3))+
       scale_y_continuous(trans = "log10",
                          breaks = c(1,2,5,10,20,50,100,200,500,1000,2000,5000,10000,20000,50000,100000),
                          minor_breaks=NULL,
                          labels = scales::comma,
                          sec.axis = dup_axis())+
       geom_vline(xintercept = 0,linetype="dashed")+
       #geom_hline(yintercept = threshold,,linetype="dashed")+
       labs(title=paste0("Growth in cases by day from the ",threshold," case mark, by country"),
            x=paste0("Days past first day with ",threshold," cases"),
            y="Total confirmed cases",
            color="Date Country Reached Threshold")+
       theme(legend.position = "none")+
       geom_text(data=country_labels,
                 aes(x=intercept_day,y=`Confirmed Cases`,label=`Country/Region`),
                 hjust=0,size=2,fontface="bold")
     
     ggplotly(myplot)
     # %>%layout(legend = list(
     #     orientation = "h"
     #   )
     #   )
     
   })
   
   output$bydate<-renderPlotly({

     confirmed_cases_long_bycountry[order(case_date),
                                    `New Cases`:=(`Confirmed Cases`-shift(`Confirmed Cases`)),
                                    `Country/Region`]
     
     #moving average over 7 days; figure is dated on the last day of the average window.
     confirmed_cases_long_bycountry[order(case_date),
                                    #NewCasesMA:=shift(zoo::rollapply(`New Cases`,7,mean),0),
                                    NewCasesMA_c:=zoo::rollapply(`New Cases`,7,mean),
                                    `Country/Region`]
     
     confirmed_cases_long_bycountry[order(case_date),
                                    #NewCasesMA:=shift(zoo::rollapply(`New Cases`,7,mean),0),
                                    NewCasesMA:=shift(NewCasesMA_c,6),
                                    `Country/Region`]
     
     new_cases<-confirmed_cases_long_bycountry[!is.na(NewCasesMA)]
     
     # if(threshold<min(new_cases[`Country/Region`=="Mainland China",`Confirmed Cases`])){
     #        new_cases <-
     #          new_cases[`Country/Region`!="Mainland China"]
     #      }
     
     case_limit<-40
     days_limit <- 21
     
     #now we want a list of the lines
     #with an x axis point either at the end of the graph,
     #or at the end of the line, whichever is first,     
     country_labels<-new_cases[,
                               .(intercept_date=max(case_date)),
                               by=`Country/Region`] %>% 
       merge(new_cases[,.(`Country/Region`,case_date,NewCasesMA)],
             by.x=c("Country/Region","intercept_date"),
             by.y=c("Country/Region","case_date"),
             all.x=TRUE,all.y=FALSE)
     
     country_labels[,NewCasesGroup:=round(log(NewCasesMA,10),1)]
     country_labels_grouped<- country_labels[,
                                             .(Countries=paste0(`Country/Region`, collapse=", "),
                                               GroupedNewCases=mean(NewCasesMA),
                                               `Country/Region` = .SD[`Country/Region`][[1]]
                                             )
                                             ,by=.(NewCasesGroup,intercept_date)] %>% .[GroupedNewCases>2]
     x_breaks  <- seq(min(new_cases$case_date),max(new_cases$case_date),7)
     
     myplot<-ggplot(new_cases
                    ,
                    aes(x=as.Date(case_date),y=NewCasesMA,
                        group=`Country/Region`))+
       geom_line(size=1,alpha=0.3)+
       theme_classic()+
       #geom_point(alpha=0.5)+
       scale_x_date(#breaks=x_breaks,
         date_breaks = "7 days",
         minor_breaks=NULL,date_labels="%b %d")+
       coord_cartesian(xlim=c(min(new_cases$case_date),max(new_cases$case_date)+7),
                       ylim=c(2,max(new_cases$NewCasesMA)))+
       scale_y_continuous(trans = "log10",
                          breaks = c(5,10,20,50,100,200,500,1000,2000,5000,10000,20000,50000,100000),
                          minor_breaks=NULL,
                          labels = scales::comma,
                          sec.axis = dup_axis())+
       #geom_vline(xintercept = 0,linetype="dashed")+
       #geom_hline(yintercept = threshold,,linetype="dashed")+
       labs(title=paste0("New confirmed cases by day and country"),
            x=paste0("Date"),
            y="New Cases (7 day rolling average)",
            color="Date Country Reached Threshold",
            caption="Source: John Hopkins Dataset")+
       theme(legend.position = "none")+
       geom_text(data=country_labels_grouped,
                 aes(x=intercept_date,y=GroupedNewCases,label=Countries),
                 hjust=0,size=2,fontface="bold")
     
     ggplotly(myplot)
     
     #return(myplot)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

