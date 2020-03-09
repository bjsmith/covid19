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
cases_long <- tidyr::gather(cases[,c(1,5:6)],key="Category",value="Count",2:3)
cases_long$Category<-factor(cases_long$Category
                            ,levels = c("Confirmed Total","Suspected Total")
                            ,labels = c("Confirmed","Suspected")
                            ,ordered = TRUE)

plot_formatting <- list(
  scale_x_date(name="Day (2020)",
               date_breaks = "2 days",
              limits = c(as.Date("2020-01-24"),min(max(cases$Day),max(fatalities$Day)+1)
                         #+1
                         ),
               date_minor_breaks = "1 day",
               date_labels = "%b %d"),
    theme_classic(),
    theme(
      legend.position = c(0.8, 0.4),
      # legend.position = "none",
      # legend.title=element_blank(),
      plot.title = element_text(hjust = 0,face = "bold"),
          plot.subtitle = element_text(size=6,hjust = 0),
          panel.grid.major.y = element_line(color="#ddddee"))
)
#cases - graph

label_position <- cases_long[cases_long$Day==max(cases_long$Day),]
label_position<-label_position[sort(label_position$Category),]
label_position$cumsum<-cumsum(label_position$Count)
# label_positioning <- data.frame(x=final$Day,
#                                 y=final$cumsum,
#                                 label=final$Category)
cases_plot <- ggplot(cases_long,
                     aes(x=Day,y=Count,group=Category,color=Category))+
  geom_line()+
  scale_y_log10(name = "Cases",labels=scales::comma
                ,breaks=(rep(c(1,2,5),times=3)*rep(10^(2:4),each=3))[1:8]
                )+
  #background values
  # geom_label(data=label_position,
  #           aes(x=Day,y=cumsum,label=Category,fill=Category),
  #           show.legend = FALSE,
  #           alpha=0.5,hjust=0,vjust=1)+
  #text values
  # geom_label(data=label_position,
  #            aes(x=Day,y=cumsum,label=Category),
  #            show.legend = FALSE,
  #            fill=NA,hjust=0,vjust=1)+
  plot_formatting+
  labs(title="Novel coronavirus (2019-nCoV) total cases reported",
      subtitle="Chinese Center for Disease Control (China CDC), http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm"
      #,caption="Suspected cases drop off as they turn into no virus or into confirmed cases.\nThe graph records new confirmed cases every day and the net change in suspected cases from one day to the next."
      )

fatalities_plot <- ggplot(fatalities,
                     aes(x=Day,y=Fatalities))+
  geom_line()+
  plot_formatting+
  labs(title="Novel coronavirus (2019-nCoV) total fatality reports by day",
       subtitle="Chinese Center for Disease Control (China CDC), http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm")



plot_grid(
  cases_plot, fatalities_plot,
  labels = "AUTO", ncol = 1
)