library(data.table)
library(dplyr)
library(knitr)
data_df = fread("data.txt")
dest_df = fread("dest.txt")


dest_df_ = dest_df %>%
  select(1:5)
###################################################################################
#process destination name
library(stringr)   
srch_dest_country = matrix("na", nrow = nrow(dest_df_), ncol = 1)
srch_dest_region = matrix("na", nrow = nrow(dest_df_), ncol = 1)
srch_dest_city = matrix("na", nrow = nrow(dest_df_), ncol = 1)
for (i in seq(nrow(dest_df_))) {
  tmp = str_split(dest_df_$srch_destination_name[i], ",")
  srch_dest_country[i] = tmp[[1]][length(tmp[[1]])]
  if (length(tmp[[1]]) >= 2) {
    srch_dest_region[i] = tmp[[1]][length(tmp[[1]]) - 1]
  }
  if (length(tmp[[1]]) >= 3) {
    srch_dest_city[i] = tmp[[1]][length(tmp[[1]]) - 2]
  }
}

dest_df_ = dest_df_ %>%
  cbind(data.frame(apply(srch_dest_country, 1, function(x) str_replace(x, " ", ""))),
        data.frame(apply(srch_dest_region, 1, function(x) str_replace(x, " ", ""))),
        data.frame(apply(srch_dest_city, 1, function(x) str_replace(x, " ", "")))) %>%
  select(-srch_destination_name)
colnames(dest_df_)[5] = "srch_dest_country"
colnames(dest_df_)[6] = "srch_dest_region"
colnames(dest_df_)[7] = "srch_dest_city"
dest_df_ = dest_df_ %>%
  mutate(srch_dest_city = str_replace_all(srch_dest_city, " ", ""),
         srch_dest_region = str_replace_all(srch_dest_region, " ", ""),
         srch_dest_country = str_replace_all(srch_dest_country, " ", ""))

#join data.txt and dest.txt
city_df = inner_join(data_df, dest_df_)

vegas_df = city_df[str_detect(city_df$srch_dest_city, "LasVegas"),]
nyc_df = city_df[str_detect(city_df$srch_dest_city, "NewYork"),]

write.csv(vegas_df, file = "vegas_df.csv")
write.csv(nyc_df, file = "nyc_df.csv")
###################################################################################

#data cleaning and preprocessing
raw_df_cl = raw_df %>%
  sample_frac(0.1) %>%
  select(-user_location_latitude, -user_location_longitude) %>%
  filter(orig_destination_distance != "NULL") 
raw_df_cl = raw_df_cl %>%
  mutate(orig_destination_distance = as.numeric(orig_destination_distance))
raw_df_cl=raw_df_cl %>%
  mutate(site_name = as.factor(site_name),
         user_location_country = as.factor(user_location_country),
         user_location_region = as.factor(user_location_region),
         user_location_city = as.factor(user_location_city),
         is_mobile = as.factor(is_mobile),
         is_package = as.factor(is_package),
         channel = as.factor(channel),
         hotel_country = as.factor(hotel_country),
         is_booking = as.factor(is_booking),
         prop_is_branded = as.factor(prop_is_branded),
         distance_band = as.factor(distance_band),
         hist_price_band = as.factor(hist_price_band),
         popularity_band = as.factor(popularity_band))
date_time =  as.POSIXlt(raw_df_cl$date_time, format = "%Y-%m-%d %H:%M:%S")
srch_ci = as.POSIXlt(raw_df_cl$srch_ci, format = "%Y-%m-%d")
srch_co = as.POSIXlt(raw_df_cl$srch_co, format = "%Y-%m-%d")
raw_df_cl = raw_df_cl %>%
  cbind(date_hour = date_time$hour,
        date_mday = date_time$mday,
        date_wday = date_time$wday,
        date_mon = date_time$mon,
        srch_ci_mday = srch_ci$mday,
        srch_ci_wday = srch_ci$wday,
        srch_ci_mon = srch_ci$mon,
        srch_co_mday = srch_co$mday,
        srch_co_wday = srch_co$wday,
        srch_co_mon = srch_co$mon,
        duration = as.numeric((srch_co-srch_ci), units="days"))
raw_df_cl = raw_df_cl %>%
  select(-srch_co, -srch_ci)
library(stringr)   
srch_dest_country = matrix("na", nrow = nrow(raw_df_cl), ncol = 1)
srch_dest_region = matrix("na", nrow = nrow(raw_df_cl), ncol = 1)
for (i in seq(nrow(raw_df_cl))) {
  tmp = str_split(raw_df_cl$srch_destination_name[i], ",")
  srch_dest_country[i] = tmp[[1]][length(tmp[[1]])]
  if (length(tmp[[1]]) > 1) {
    srch_dest_region[i] = tmp[[1]][length(tmp[[1]]) - 1]
  }
}
raw_df_cl = raw_df_cl %>%
  cbind(data.frame(apply(srch_dest_country, 1, function(x) str_replace(x, " ", ""))),
        data.frame(apply(srch_dest_region, 1, function(x) str_replace(x, " ", "")))) %>%
  select(-srch_destination_name)
colnames(raw_df_cl)[38] = "srch_dest_region"
colnames(raw_df_cl)[37] = "srch_dest_country"
raw_df_cl = raw_df_cl %>%
  mutate(srch_destination_type_id = as.factor(srch_destination_type_id))
save(raw_df_cl, file = "raw_df_cl.Rdata")
###################################################################################
###########           MODELING
#svm
library(e1071)
set.seed(2000)
index = sample(1:nrow(raw_df_cl), round(0.1*nrow(raw_df_cl)))
train = raw_df_cl[index,]
test = raw_df_cl[-index,]
n = names(raw_df_cl)
f = as.formula(paste("is_booking~", paste(n[!n %in% c("is_booking", "date_time","user_location_region",
                                                      "user_location_city","srch_destination_latitude",
                                                      "srch_destination_longitude","srch_dest_country",
                                                      "srch_dest_region", "hotel_country", "user_id")], collapse = "+")))
svm.speed  = svm(data = train, f, epsilon = 0.1)
summary(lm)

#grouping by hotel_id
df2 = raw_df %>%
  group_by(hotel_id) %>%
  summarise(count_booking = sum(is_booking == 1),
            count_click = n() - count_booking,
            prop_is_branded = prop_is_branded[1],
            prop_starrating = mean(prop_starrating),
            distance_band = distance_band[1],
            hist_price_band = hist_price_band[1],
            popularity_band = popularity_band[1],
            count_users = length(unique(user_id)),
            average_room_cnt = mean(srch_rm_cnt),
            average_children_cnt = mean(srch_children_cnt),
            average_adults_cnt = mean(srch_adults_cnt),
            mobile_rate = sum(is_mobile == 1)/n(),
            package_rate = sum(is_package == 1)/n(),
            hotel_country = hotel_country[1]) %>%
  mutate(con_rate = count_booking/count_click)

df2 = df2 %>%
  slice(-which(df2$con_rate == Inf))
df2 = df2 %>%
  mutate(distance_band = as.factor(distance_band),
         hist_price_band = as.factor(hist_price_band),
         popularity_band = as.factor(popularity_band),
         hotel_country = as.factor(hotel_country))


############################################
## distribution of type of travellers
distribution = city_df %>%
  group_by(srch_adults_cnt, srch_children_cnt, srch_rm_cnt) %>%
  summarise(count = n()) %>%
  filter(srch_adults_cnt!=0 & 
         srch_rm_cnt != 0) %>%
  filter((srch_adults_cnt + srch_children_cnt)>=srch_rm_cnt) %>%
  arrange(desc(count))

###########################
###################
library(dplyr)      
ny_df = read.csv("NewYork3.csv")
cl2 = ny_df %>%
  select(user_id, hotel_id, date_time,is_booking, srch_ci, srch_co,
         srch_adults_cnt, srch_children_cnt, srch_rm_cnt,is_mobile,
         is_package,prop_is_branded, prop_starrating, region, distance_band, hist_price_band, popularity_band,
         user_location_country,user_location_region,user_location_city,user_location_latitude,user_location_longitude,
         channel,hotel_country
         
  )
time =  as.POSIXlt(cl2$date_time, format = "%Y-%m-%d %H:%M:%S")
srch_ci = as.POSIXlt(cl2$srch_ci, format = "%Y-%m-%d")
srch_co = as.POSIXlt(cl2$srch_co, format = "%Y-%m-%d")
cl2 = cl2 %>%
  cbind(date_hour = time$hour,
        duration = as.numeric((srch_co-srch_ci), units="days"),
        advance = as.numeric((srch_ci-time), units="days"),
        ci_wday = srch_ci$wday)

cl2_ = cl2 %>%
  group_by(user_id, srch_ci, srch_co, srch_children_cnt, srch_rm_cnt, srch_adults_cnt, region) %>%
  mutate(sign = ifelse(sum(is_booking)>=1&is_booking==1, 1, ifelse(sum(is_booking)>=1, 2, 0)))
cl2_ = cl2_ %>%
  group_by(user_id, srch_ci, srch_co, srch_children_cnt, srch_rm_cnt, srch_adults_cnt, region) %>%
  arrange(user_id)

cl3 = cl2_ %>%
  ungroup() %>%
  select(-user_id, -hotel_id, -date_time, -srch_ci, -srch_co, -region, -is_booking)
cl3 = cl3 %>%
  filter(advance>0)

##random forest
library(randomForest)
set.seed(2000)
index = sample(1:nrow(cl3), round(0.01*nrow(cl3)))
train = cl3[index,]
test = cl3[-index,]

#train the random forest classification model
n = names(cl3)
f = as.formula(paste("sign~", paste(n[!n %in% "sign"], collapse = "+")))
cl3 = cl3 %>%
  mutate(sign = as.factor(sign))
rf = randomForest(data = train, f, importance = TRUE)
rf = predict(rf)
train_table = table(train$sign, rf)
sum(diag(train_table))/nrow(train)
###############
#final data
library(dplyr)      
final_raw = city_df %>%
  select(user_id, hotel_id, date_time,is_booking, srch_ci, srch_co,
         srch_adults_cnt, srch_children_cnt, srch_rm_cnt,is_mobile,
         is_package,prop_is_branded, prop_starrating, srch_dest_region, srch_dest_city, srch_dest_country,
         distance_band, hist_price_band, popularity_band,
         user_location_country,user_location_region,user_location_city,user_location_latitude,user_location_longitude,
         channel,hotel_country) %>%
  filter(user_location_latitude != "NULL",
         user_location_longitude != "NULL") 

time =  as.POSIXlt(final_raw$date_time, format = "%Y-%m-%d %H:%M:%S")
srch_ci = as.POSIXlt(final_raw$srch_ci, format = "%Y-%m-%d")
srch_co = as.POSIXlt(final_raw$srch_co, format = "%Y-%m-%d")

final_raw = final_raw %>%
  cbind(date_hour = time$hour,
        duration = as.numeric((srch_co-srch_ci), units="days"),
        advance = as.numeric((srch_ci-time), units="days"),
        ci_wday = srch_ci$wday) %>%
  group_by(user_id, srch_ci, srch_co, srch_children_cnt, srch_rm_cnt, srch_adults_cnt, srch_dest_region) %>%
  mutate(sign = ifelse(sum(is_booking)>=1&is_booking==1, 1, ifelse(sum(is_booking)>=1, 2, 0))) %>%
  ungroup() %>%
  select(-user_id, -date_time, -srch_ci, -srch_co, -is_booking, -srch_dest_city, -srch_dest_country) %>%
  filter(advance>0)

write.csv(final_raw, "final_raw.csv")

################################
final_raw = read.csv("final_raw.csv")
pie_price = final_raw %>%
  group_by(hist_price_band) %>%
  summarise(fail_rate = sum(sign == 0)/(n() - sum(sign == 1)),
            con_rate = sum(sign == 1)/(n() - sum(sign == 1)))

pie_user_country = final_raw %>%
  group_by(user_location_country) %>%
  summarise(fail_rate = sum(sign == 0)/(n() - sum(sign == 1)),
            con_rate = sum(sign == 1)/(n() - sum(sign == 1)),
            n = n())

library(ggplot2)
ggplot(pie_user_country, aes(x = fail_rate, y= con_rate, label = user_location_country)) +
  geom_point(aes(colour = user_location_country, size = n), alpha = 0.7) +
  scale_colour_hue(h=c(90, 360)) +
  scale_size(range = c(8,30)) +
  theme_grey() +
  geom_text() +
  guides(size=FALSE, colour = F) +
  theme(axis.title=element_text(size=16,face="bold"),
        axis.text = element_text(size=12),
        plot.title = element_text(size=20,face="bold", hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"))+
  geom_hline(yintercept = 0.0918, lty = 2) +
  geom_vline(xintercept = 0.7202, lty = 2) +
  labs(list(title = "Convertion/Churn Rate per User Country", 
            x = "Failure Rate", y = "Conversion Rate"))
  


pie_channel = final_raw %>%
  mutate(channel = as.factor(channel)) %>%
  group_by(channel) %>%
  summarise(fail_rate = sum(sign == 0)/(n() - sum(sign == 1)),
            con_rate = sum(sign == 1)/(n() - sum(sign == 1)),
            n = n())

ggplot(pie_channel, aes(y = con_rate, x = fail_rate, size = n, colour = channel, label = channel)) +
  geom_point(alpha = 0.7) +
scale_size(range = c(5,50)) +
  theme_grey() +
  scale_colour_hue(h=c(0, 180)) +
  labs(list(title = "Convertion/Failure Rate per Channel", 
            x = "Failure Rate", y = "Conversion Rate")) +
  guides(size=FALSE) +
  theme(axis.title=element_text(size=16,face="bold"),
        axis.text = element_text(size=12),
        plot.title = element_text(size=20,face="bold", hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"))
  

final_raw %>% summarise(fail_rate = sum(sign == 0)/(n() - sum(sign == 1)),
                        con_rate = sum(sign == 1)/(n() - sum(sign == 1)))
  
