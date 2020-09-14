library(tidyverse)


df <- read_tsv("user-ct-test-collection-01.txt")

glimpse(df_small)


# Making dataset tiny to make it compatible for low config personal computer
df_small <- df[1:10000,]


sessions <- df_small %>% arrange(AnonID, QueryTime) %>% 
  group_by(AnonID) %>% 
  mutate(mins = difftime(QueryTime, lag(QueryTime), units = 'mins'),flag = is.na(lag(AnonID)) | mins > 60, seq = cumsum(flag)) %>% 
  group_by(AnonID, seq) %>%
  summarize(start = first(QueryTime), searches = n(), terms = n_distinct(Query),
duration = as.numeric(difftime(last(QueryTime), first(QueryTime), units = "mins")), clicks = sum(!is.na(ClickURL)))


glimpse(sessions)
sessions$terms <- as.factor(sessions$terms)
sessions$searches <- as.factor(sessions$searches)



ggplot(data = sessions, aes(x = duration))+
  geom_histogram(binwidth = 5, fill = "#00BFC4")+
  xlim(1,150)+
  labs(title = "Distribution of durations", x = "Sessions Duration")+
  theme_bw()



ggplot(data = sessions, aes(x = clicks))+
  geom_histogram(binwidth = 1, fill = 'black')+
  scale_y_log10()+
  xlim(1,40)+
  labs(title = "Distribution of numbers of clicks in Session ", x = "Number of Clicks")+
  theme_bw()

# First 1000 observations 
ggplot(data = sessions[1:1000,], aes(y = searches, x = AnonID)) +
  geom_point()+ 
  labs(title = "User vs no. of Searches ")
  theme_bw()
  

  
ggplot(data = sessions, aes(x = searches)) + 
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) ) +
  coord_cartesian(xlim = c(1, 10))+
  labs(title ="Total no searches by all users",
       x ="Number of Searches")+
  theme_bw()
  


































  


