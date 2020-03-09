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
   titlePanel("Coronavirus growth based on reference date"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("reference_count",
                    "Line up cases at",
                    min = 1,
                    max = 2000,
                    value = 20)
        # selectInput("location","Reference Country or Region",
        #             ),
        
         
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlotly({
     
     threshold<-input$reference_count
     
     #Remove China if it was above the threshold on the first day. we don't do this to any other countries
     #but China is further along than all others.
     if(threshold<min(confirmed_cases_long_bycountry[`Country/Region`=="Mainland China",`Confirmed Cases`])){
       confirmed_cases_long_bycountry <- 
         confirmed_cases_long_bycountry[`Country/Region`!="Mainland China"]
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
       scale_y_continuous(#trans = "log10",
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
}

# Run the application 
shinyApp(ui = ui, server = server)

