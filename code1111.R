# 加载包，读入原始数据 ---------------------------------------------------------
library(sp)
library(ggmap)
library(rgdal)
library(dlm)
library(plotrix)
library(lubridate)
library(ggmap)
register_google(key = "AIzaSyBV2z3Mffxz66v1CmsXX25vm1xOcZAoZVg")
"GeoJSON" %in% ogrDrivers()$name
rm(list = ls())
filelist = dir(pattern = ".geojson")
data = lapply(filelist, readOGR)

#清洗数据(时间减去7个小时,将时间调整到当地时间)---------------------------------
time_stamp = list()
longitude = list()
latitude = list()

for (i in seq(length(data))) {
  time_stamp = list(time_stamp,as.character.Date(data[[i]]@data$time))
  longitude = list(longitude,data[[i]]@coords[,1])
  latitude = list(latitude,data[[i]]@coords[,2])
}

time_stamp = unlist(time_stamp)
longitude = unlist(longitude)
latitude = unlist(latitude)

data_raw = cbind.data.frame(ymd_hms(time_stamp) - hms('7:00:00'),
                            as.numeric(longitude),as.numeric(latitude))
names(data_raw) = c("time_stamp","longitude","latitude")

index = duplicated(paste0(data_raw$longitude,data_raw$latitude))
data = data_raw[!index,]
data = data[order(data$time_stamp),]

#转换UTM坐标系-------------------------------------------------------------------
# This following step is similar to transforming time stamps like "2020-01-25" into a time object
spat_df <- SpatialPointsDataFrame(coords=data[, c("longitude", "latitude")],
                                  data=data['time_stamp'],   # This needs to be a data frame
                                  proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df <- spTransform(spat_df, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords <- coordinates(utm_df)
colnames(utm_coords) = paste0("utm_",colnames(utm_coords))

#卡尔曼滤波---------------------------------------------------------------------
gps_variance <- 20^2
v_mat <- matrix(c(gps_variance,0,0,gps_variance),nrow = 2)
time_inverval_median = summary(as.numeric(diff.Date(data$time_stamp)))[3]
dt <- time_inverval_median
g_mat <- matrix(c(1, 0, dt, 0,
                  0, 1, 0, dt,
                  0, 0, 1, 0,
                  0, 0, 0, 1), byrow=TRUE, ncol=4)
avg_walk_speed_m_per_sec <- 1.4  # https://en.wikipedia.org/wiki/Walking
dlm_spec <- dlm(
  FF= matrix(c(1,0,0,0,
               0,1,0,0),byrow = TRUE,ncol = 4),
  GG= g_mat,
  V = v_mat,
  W = diag(c(5, 5, 1, 1)^2),
  m0 = matrix(c(utm_coords[1, ], rep(avg_walk_speed_m_per_sec / dt, 2)),
              ncol=1), # A vector by R defaults is a k by 1 matrix
  C0 = diag(rep(10^2, 4)))
dlm_filter_mod <- dlmFilter(utm_coords, dlm_spec)
dlm_smooth_mod <- dlmSmooth(dlm_filter_mod)

#整理数据表----------------------------------------------------------------------
dlm_filter_coords = as.matrix(dlm_filter_mod$m[,1:2])
colnames(dlm_filter_coords) = paste0("dlm_filter_",names(data)[2:3])

dlm_smooth_coords = as.matrix(dlm_smooth_mod$s[,1:2])
colnames(dlm_smooth_coords) = paste0("dlm_smooth_",names(data)[2:3])

speed = as.matrix(sqrt(dlm_smooth_mod[["s"]][,3]^2+dlm_smooth_mod[["s"]][,4]^2))
speed = speed[-1]
time_inverval = as.numeric(diff.Date(data$time_stamp))
time_inverval =c(time_inverval[1],time_inverval)

data = cbind.data.frame(data,utm_coords,dlm_filter_coords[-1,],
                        dlm_smooth_coords[-1,],speed,time_inverval)
rownames(data) = NULL

#作轨迹图-------------------------------------------------------------------
plot(data[1:100,4:5],pch = 16)
points(data[1:100,6:7],col = "blue",pch = 3)
points(data[1:100,8:9],col = "red",pch = 18)
legend("topright",inset = 0.02,c("utm_coords","dlm_filter_mod","dlm_smooth_mod"),
       pch = c(16,3,18),col = c("black","blue","red"))

#开始行动的数据特性分析---------------------------------------------------------

speed_of_interval_100plus = data[data$time_inverval > 60,"speed"]

walk_speed = mean(speed[speed<3])
walk_prob = sum(speed_of_interval_100plus<3)/length(speed_of_interval_100plus)

trot_speed = mean(speed[speed>3&speed<6])
trot_prob = sum(speed_of_interval_100plus>3&speed_of_interval_100plus<6)/
            length(speed_of_interval_100plus)

ride_speed = mean(speed[speed>6&speed<15])
ride_prob = sum(speed_of_interval_100plus>6&speed_of_interval_100plus<15)/
  length(speed_of_interval_100plus)

drive_speed = mean(speed[speed>15])
drive_prob = sum(speed_of_interval_100plus>15)/length(speed_of_interval_100plus)

p_map = ggmap(get_googlemap(center = c(mean(data$longitude),mean(data$latitude)),zoom = 14,maptype = "roadmap"))
p_map + ggplot(sp_day_data, aes(longitude,latitude)) + geom_point()
#加载温度数据作图---------------------------------------------------------------
weather = readxl::read_xlsx("Weather.xlsx")
time_stamp_weather = ymd_hms(paste(weather$Date,substr(weather$Time,11,19)))
dgree_weather = cbind.data.frame(time_stamp_weather,weather$dgree)
dgree_weather$time_stamp_weather = round_date(dgree_weather$time_stamp_weather,unit = 'hour')
colnames(dgree_weather) = c("time","dgree")

data_speed = cbind.data.frame(round_date(data$time_stamp,unit = "hour"),data$speed)
colnames(data_speed) = c("time","speed")
data_speed_weather = merge(data_speed,dgree_weather)

temp  = data_speed_weather[data_speed_weather$speed<3,]
plot(temp$dgree,temp$speed)

#做每天的轨迹图-----------------------------------------------------------------
sp_day_data = data[day(data$time_stamp)==31,]
plot(sp_day_data[,4:5],pch = 16)
points(sp_day_data[,6:7],col = "blue",pch = 3)
points(sp_day_data[,8:9],col = "red",pch = 18)
legend("topright",inset = 0.02,c("utm_coords","dlm_filter_mod","dlm_smooth_mod"),
       pch = c(16,3,18),col = c("black","blue","red"))
