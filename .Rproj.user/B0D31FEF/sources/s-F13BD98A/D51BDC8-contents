library(data.table)
library(ggplot2)
library(readr)
library(cowplot)

#load
cases <- readr::read_csv("../data/Coronavirus stats - cases.csv")
fatalities <- read_csv("../data/Coronavirus stats - fatalities.csv")

#preprocess
cases$Day<-as.Date(paste0("2020 ", cases$Day),format = "%Y %b %d")
fatalities$Day<-as.Date(paste0("2020 ", fatalities$Day),format = "%Y %b %d")

#cases - preapre
cases_long <- tidyr::gather(cases[,c(1:2,8)],key="Category",value="Count",2:3)
cases_long$Category<-factor(cases_long$Category
                            ,levels = c("Suspected Change","Confirmed")
                            ,labels = c("Suspected","Confirmed")
                            ,ordered = TRUE)

plot_formatting <- list(
  scale_x_date(name="Day (2020)",
               date_breaks = "1 days",
              limits = c(as.Date("2020-01-24"),min(max(cases$Day),max(fatalities$Day))
                         #+1
                         ),
               date_minor_breaks = "1 day",
               date_labels = "%b %d"),
    scale_y_continuous(name = "Cases",labels=scales::comma),
    theme_classic())
topplots_formatting<- list(
    theme(
      legend.position = c(0.1, 0.9),
      # legend.position = "none",
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0,face = "bold"),
          plot.subtitle = element_text(size=6,hjust = 0),
          panel.grid.major.y = element_line(color="#ddddee"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y = element_text(vjust=0)
      #,axis.text.y=element_text(vjust=-2)
      
    )
)
bottomplot_formatting <- list(
  theme(
    legend.position = c(0.1, 0.9),
    # legend.position = "none",
    legend.title=element_blank(),
    plot.title = element_text(hjust = 0,face = "bold"),
    plot.subtitle = element_text(size=6,hjust = 0),
    panel.grid.major.y = element_line(color="#ddddee")
))
#cases - graph


label_position <- cases_long[cases_long$Day==max(cases_long$Day),]
label_position<-label_position[sort(label_position$Category),]
label_position$cumsum<-cumsum(label_position$Count)
# label_positioning <- data.frame(x=final$Day,
#                                 y=final$cumsum,
#                                 label=final$Category)
cases_plot <- ggplot(cases_long,
                     aes(x=Day,y=Count,group=Category,fill=Category))+
  geom_area(alpha=0.5)+
  #background values
  # geom_label(data=label_position,
  #           aes(x=Day,y=cumsum,label=Category,fill=Category),
  #           show.legend = FALSE,
  #           alpha=0.5,hjust=0,vjust=1)+
  # #text values
  # geom_label(data=label_position,
  #            aes(x=Day,y=cumsum,label=Category),
  #            show.legend = FALSE,
  #            fill=NA,hjust=0,vjust=1)+
  plot_formatting+topplots_formatting+
  labs(title="Novel coronavirus (2019-nCoV) daily net change in cases reported",
      subtitle="Chinese Center for Disease Control (China CDC), http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm",
      caption="Suspected cases drop off as they turn into no virus or into confirmed cases.\nThe graph records new confirmed cases every day and the net change in suspected cases from one day to the next.")

fatalities_plot <- ggplot(fatalities,
                     aes(x=Day,y=`New fatality reports by day`))+
  geom_area(alpha=0.5)+
  plot_formatting+topplots_formatting+
  scale_y_continuous(name = "Fatalities\n",labels=scales::comma)+
  labs(title="Novel coronavirus (2019-nCoV) daily new fatality reports",
       subtitle="Chinese Center for Disease Control (China CDC), http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm")


fatality_rate_graph <- merge(cases, fatalities,by="Day")
fatality_rate_graph$FatalityRate<-fatality_rate_graph$Fatalities/fatality_rate_graph$`Confirmed Total`
fatality_rate <- ggplot(fatality_rate_graph,aes(Day,FatalityRate))+
  geom_line(alpha=0.5)+
  plot_formatting+bottomplot_formatting+
  scale_y_continuous(name = "Percent",labels=scales::percent)+
  labs(title="Novel coronavirus (2019-nCoV) fatality rate of confirmed cases by day",
       subtitle="Fatalities as a percentage of confirmed cases. Does not represent an overall fatality rate for the disease.\nChinese Center for Disease Control (China CDC), http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm")

plot_grid(
  cases_plot, fatalities_plot,fatality_rate,
  labels = "AUTO", ncol = 1
)
