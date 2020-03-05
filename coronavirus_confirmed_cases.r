library( tidyverse)
library( data.table)
library( scales)
library( ggrepel)

confirmed = as.tbl( fread( "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")) # Load the data
confirmed = confirmed %>% gather( Date, Confirmed_Cases, -(1:4)) # Melting the data
confirmed = confirmed %>% mutate( Date = as.Date( Date, format = "%m/%d/%y")) # Casting strings to dates
confirmed = confirmed %>% group_by( `Country/Region`, Date) %>% summarise( Confirmed_Cases = sum( Confirmed_Cases)) %>% ungroup() # Number of diseased people grouped by region and date

confirmed = confirmed %>% rename( Country = `Country/Region`) %>% filter( Country != 'Others')
temp = confirmed %>% filter( Date == max( Date)) %>% mutate( Position = dense_rank( desc( Confirmed_Cases))) %>% select( Country, Position)
confirmed = confirmed %>% inner_join( temp, "Country") %>% filter( Position <= 5)
confirmed = confirmed %>% mutate( Confirmed_Cases = ifelse( Confirmed_Cases == 0, 0.9, Confirmed_Cases))

q = ggplot( data = confirmed, aes( x = Date, y = Confirmed_Cases, grp = Country, col = Country)) +
  geom_line() + 
  theme_bw() + 
  theme( legend.position = "none") + 
  scale_y_log10( labels = comma) +
  scale_x_date(date_breaks = "1 week") + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text( size = 12),
        plot.title = element_text( hjust = 0.5, face = "bold"), axis.title = element_text( size = 12), 
        legend.text = element_text( size = 12)) +
  ggtitle( "Coronavirus diseased counts") + 
  ylab("Number of diseased people") +
  geom_text_repel(aes (label = Country), data = confirmed %>% filter( Date == max( Date)), hjust=1)

plot( q)