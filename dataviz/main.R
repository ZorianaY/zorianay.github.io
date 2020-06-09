# Title     : Communication {r}Evolution
# Created by: Zoriana Yuzvin and Viktoria Lavrynenko
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)

mms <- read.csv("csv\\Internet_Usage.csv")
internet_users <- mms %>% filter(!is.na(Percent)) %>% group_by(Time) %>% summarise(avg = mean(Percent))
df_internet <- data.frame(internet_users)
df<-as.data.frame(df_internet)
theme_set(theme_bw())
internet_chart <- ggplot(df_internet, aes(x=Time, y=avg)) +
  geom_point(size=3) +
  geom_segment(aes(x=Time, xend=Time, y=0, yend=avg)) +
  scale_x_continuous(limits=c(min(1990), max(df_internet$Time)), breaks=seq(1990, 2018))+
  theme(axis.title.x = element_blank())+
  scale_y_continuous(name="%", breaks = c(0,20,40,60,80))+
theme(axis.text.x = element_text(angle=55, vjust=0.6))+
  labs(title="Використання інтернету у світі", subtitle="% до населення", size = 2, caption = "Джерело: World Bank")
print(internet_chart)
ggsave("Internet_chart.svg", width = 16, height = 10, units = "cm")

chatbots <- read_csv("csv\\Chatbots.csv")
df_chatbots <- data.frame(chatbots)
chatbot_graph <-ggplot(df_chatbots, aes(ymax=0, ymin=Percantage, xmax=4, xmin=3, fill=Industry)) +
  geom_rect() +
  facet_wrap(Industry ~ ., ncol=3) +
  coord_polar(theta="y", start = 0) +
  ylim(c(0, 100)) +
  xlim(c(2, 4))
print(chatbot_graph)
ggsave("Chatbot.svg", width = 19, height = 20, units = "cm")

email_chart <- read_csv("csv\\Spam.csv")
df_email <- data.frame(email_chart)
email_users_graph <- ggplot(df_email, aes(x=Year,y=Percantage, theme_set(theme_classic()))) +
  geom_col(width=0.5)+
  scale_x_continuous(breaks=seq(2007,2019))+
  theme(axis.title.x = element_blank())+
  scale_y_continuous(name= "%")+
  theme(axis.text.x = element_text(angle=55, vjust=0.6))+
  labs(title="Глобальний обсяг спаму до загального трафіку електронної пошти", subtitle = "за 2007-2019, %", caption="Джерело: Statista")
print(email_users_graph)
ggsave("email_chart.svg", width = 16, height = 10, units = "cm")

sms_chart <- read_csv("csv\\Sms.csv")
df_sms <- data.frame(sms_chart)
sms_graph <- ggplot(df_sms, aes(x=Year, y=Sms, theme_minimal())) +
geom_line(linetype="dotted",size=0.1) +
  geom_point(size=3) +
  scale_y_continuous(name="мільярди")+
  theme(axis.title.x = element_blank())+
labs(title="Кількість відправлених SMS у Німеччині", subtitle="за 2000-2019, мільярди", caption = "Джерело: Statista")
print(sms_graph)
ggsave("sms_chart.svg", width = 16, height = 10, units = "cm")

icq_chart <- read_csv("csv\\Devices.csv")
df_icq <- data.frame(icq_chart)
icq_graph <- ggplot(df_icq) + geom_segment(aes(x=0,xend=150,y=Desktop, yend=Mobile, colour=Year )) +
  scale_y_continuous(name = "Комп'ютер, хвилини", sec.axis = dup_axis(name = "Мобільний телефон, хвилини")) +
  scale_x_continuous(breaks = 0)+
  theme(axis.title.x = element_blank())+
  labs(title="Щоденний час користувача в Інтернеті по всьому світу за пристроєм", subtitle="з 2011 по 2020 рік, хвилини", caption = "Джерело: Statista")
print(icq_graph)
ggsave("icq_chart.svg", width = 16, height = 10, units = "cm")

facebook_chart <- read_csv("csv\\Facebook.csv")
instagram_chart <- read_csv("csv\\Instagram.csv")
twitter_chart <- read_csv("csv\\Twitter.csv")
whatsap_chart <- read_csv("csv\\WhatsAp.csv")
df_facebook <- data.frame(facebook_chart)
df_facebook$Messenger <- "Facebook"
df_instagram <- data.frame(instagram_chart)
df_instagram$Messenger <- "Instagram"
df_twitter <- data.frame(twitter_chart)
df_twitter$Messenger <- "Twitter"
df_whatsap <- data.frame(whatsap_chart)
df_whatsap$Messenger <- "WhatsApp"
df_message_chart <- Reduce(function(x, y) merge(x, y, all=TRUE), list(df_facebook, df_instagram, df_twitter, df_whatsap))
message_chart <- ggplot(df_message_chart, aes(x=Year, y=Users)) +
geom_point(aes(colour = Messenger,size=Users, alpha=1)) +
  scale_size(range = c(1, 20))+
  scale_y_continuous(name="мільйони")+
  theme(axis.title.x = element_blank())+
  labs(title="Кількість щомісячних активних користувачів месенджерів Facebook, Instagram, Twitter, WhatsAp", subtitle = "з 2014 по 2020 рік, мільйони", caption = "Джерело:Statista")
print(message_chart)
ggsave("messanger_chart.svg", width = 19, height = 20, units = "cm")

