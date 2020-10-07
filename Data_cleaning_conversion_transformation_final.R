#####Data cleaning, conversion and transformation
setwd("/Users/mitchellaarons1/Desktop/UNI/Honours/Honours GPS work")

#####IMPORT LIBRARIES#####
library(REdaS) #For radian to degrees conversion
library(readxl) #Read + write excel
library(readr) #Read + write excel
library(tidyverse) #Data cleaning tools
library(ggforce) #For plotting ellipse (football ground)
library(splancs) #For area of polygon calculations
library(irr) #For kappa testing


######IMPORT FILES#######

all_rounds <- read_csv("Data/all_rounds_reduced.csv")

KickIns <- read_excel("Data/StoppageDataFull.xlsx", 
                      sheet = "KickIns")

Stoppages <- read_excel("Data/StoppageDataFull.xlsx", 
                        sheet = "Stoppages")

Attendance <- read_excel("Data/StoppageDataFull.xlsx", 
                         sheet = "Attendance")

Matches <- read_excel("Data/StoppageDataFull.xlsx", 
                      sheet = "Matches")

Players <- read_excel("Data/StoppageDataFull.xlsx", 
                      sheet = "Players")

Inside50Chains <- read_csv("Data/Inside50Chains.csv") %>%
  drop_na(MatchID)


I50TO_Entry_zone <- read_csv("Data/I50TOEntryzone.csv") 

####CREATE IMPORTANT VARIABLES#####
#round 14 removed from marvel for now
rounds_marvel <- c(1,2,4,6,8,10,15,18,19,21)
rounds_mcg <- c(5,9,22)
rounds_adelaide <- c(20)
rounds_kardinia <- c(17)
rounds_scg <- c(23)
rounds_townsville <- c(13)
rounds_blundstone <- c(16)
rounds_manuka <- c(7)
rounds_china <- c(11)
rounds_optus <- c(3)

#Ground Dimensions
marvel_length <- 160
marvel_width <- 129
mcg_length <- 161
mcg_width <- 138
adelaide_length <- 167
adelaide_width <- 123
kardinia_length <- 170
kardinia_width <- 115
scg_length <- 155
scg_width <- 136
townsville_length <- 159 ##Need to find dimensions of townsville
townsville_width <- 128 ##Need to find dimensions of townsville
blundstone_length <- 160
blundstone_width <- 124
manuka_length <- 162.5
manuka_width <- 138.4
china_length <- 160
china_width <- 132
optus_length <- 165
optus_width <- 130

#Rounds
rounds<- c(1,2,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23)


#Match ID's
match_ids <- c(199475840, 199483954, 199502899, 199516802, 199535184, 199541379, 199566064, 199574147, 199591970, 199606914, 199623298, 199649507, 199664258, 199680642,
               199697507, 199714369, 199729793, 199737987, 199236099, 199250563, 199269905, 199282849)

#Binded Round numbers with ID's
round_id <- cbind(Number = c(1,2,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23),ID = 
                    c(199475840, 199483954, 199502899, 199516802, 199535184, 199541379, 199566064, 199574147, 199591970, 199606914, 199623298, 199649507, 199664258, 199680642,
                      199697507, 199714369, 199729793, 199737987, 199236099, 199250563, 199269905, 199282849))

#Opposition goal line x. This point is (x,0).
marvel_goal_x <- marvel_length/2
mcg_goal_x <- mcg_length/2
adelaide_goal_x <- adelaide_length/2
kardinia_goal_x <- kardinia_length/2
scg_goal_x <- scg_length/2
townsville_goal_x <- townsville_length/2
blundstone_goal_x <- blundstone_length/2
manuka_goal_x <- manuka_length/2
china_goal_x <- china_length/2
optus_goal_x <- optus_length/2


#####CREATE NEW DATAFRAMES#####

Stoppages_Attendance <- Stoppages %>%
  left_join(Attendance, by = c("StoppageKey" = "StoppageKey", "Round" = "Round", "Period" = "Period", 
                               "PeriodSecs" = "PeriodSecs", "MatchID" = "MatchID"), suffix = c('_Stoppage', '_Attendance'))



I50TO <-Inside50Chains %>%
  filter(EndState == "Turnover") %>%
  mutate(I50toTO_seconds = (EndDeci - In50Deci) / 10, InitY = InitY * -1, EndY = EndY * -1) %>%
  mutate(id = as.numeric(rownames(I50TO))) %>%
  left_join(select(Matches, ID, Round, "StK Parity"), by = c("MatchID" = "ID")) %>%
  rename(StkParity = "StK Parity") %>%
  mutate(Ground_x = case_when(Round %in% c(14) ~ marvel_width,
                              Round %in% rounds_mcg ~ mcg_length,
                              Round %in% rounds_marvel ~ marvel_length,
                              Round %in% rounds_adelaide ~ adelaide_length,
                              Round %in% rounds_optus ~ optus_length,
                              Round %in% rounds_china ~ china_length,
                              Round %in% rounds_kardinia ~ kardinia_length,
                              Round %in% rounds_blundstone ~ blundstone_length,
                              Round %in% rounds_manuka ~ manuka_length,
                              Round %in% rounds_scg ~ scg_length,
                              Round %in% rounds_townsville ~ townsville_length)) %>%
  mutate(Direction = case_when(((Quarter %in% c(1,3) & StkParity == 0) | (Quarter %in% c(2,4) & StkParity == 1)) ~ "Right",
                               ((Quarter %in% c(1,3) & StkParity == 1) | (Quarter %in% c(2,4) & StkParity == 0)) ~ "Left")) %>%
  left_join(select(I50TO_Entry_zone, TOid, Entry_zone), by = c("id" = "TOid"))


#Add goal line values and determine distance from goal on return chain. Code for result of TO chain.
I50TO <- I50TO %>%
  mutate(goal_x = case_when(Direction == "Right" & Round %in% rounds_marvel ~ marvel_goal_x*-1,
                            Direction == "Right" & Round %in% rounds_mcg ~ mcg_goal_x*-1,
                            Direction == "Right" & Round %in% rounds_adelaide ~ adelaide_goal_x*-1,
                            Direction == "Right" & Round %in% rounds_kardinia ~ kardinia_goal_x*-1,
                            Direction == "Right" & Round %in% rounds_scg ~ scg_goal_x*-1,
                            Direction == "Right" & Round %in% rounds_townsville ~ townsville_goal_x*-1,
                            Direction == "Right" & Round %in% rounds_blundstone ~ blundstone_goal_x*-1,
                            Direction == "Right" & Round %in% rounds_manuka ~ manuka_goal_x*-1,
                            Direction == "Right" & Round %in% rounds_china ~ china_goal_x*-1,
                            Direction == "Right" & Round %in% rounds_optus ~ optus_goal_x*-1,
                            Direction == "Right" & Round %in% c(14) ~ marvel_goal_x*-1,
                            Direction == "Left" & Round %in% rounds_marvel ~ marvel_goal_x,
                            Direction == "Left" & Round %in% rounds_mcg ~ mcg_goal_x,
                            Direction == "Left" & Round %in% rounds_adelaide ~ adelaide_goal_x,
                            Direction == "Left" & Round %in% rounds_kardinia ~ kardinia_goal_x,
                            Direction == "Left" & Round %in% rounds_scg ~ scg_goal_x,
                            Direction == "Left" & Round %in% rounds_townsville ~ townsville_goal_x,
                            Direction == "Left" & Round %in% rounds_blundstone ~ blundstone_goal_x,
                            Direction == "Left" & Round %in% rounds_manuka ~ manuka_goal_x,
                            Direction == "Left" & Round %in% rounds_china ~ china_goal_x,
                            Direction == "Left" & Round %in% rounds_optus ~ optus_goal_x,
                            Direction == "Left" & Round %in% c(14) ~ marvel_goal_x,),
         goal_y = 0,
         end_dist_from_opp_goal = sqrt((ReturnEndX - goal_x)^2 + ReturnEndY^2),
         Result_code = case_when(end_dist_from_opp_goal <= 50 ~ 1,
                                 end_dist_from_opp_goal > 50 & (ReturnEndState == "OOB"| ReturnEndState == "BallUp") ~ 2,
                                 end_dist_from_opp_goal > 50 & ReturnEndState == "Turnover" ~ 3,
                                 ReturnEndState == "OppoRushed" ~ 4,
                                 ReturnEndState == "Unresolved" ~ 5)) 



####IMPORTANT FUNCTIONS######
#Used for latitude and longitude calculation and conversion
dlat <- function(latitude) {
  #Formula
  lat_rad <- latitude * pi/180
  degree_of_latitude <- 111.13295 - (0.55982*cos(2*lat_rad)) + (0.00117*cos(4*lat_rad))
  #conversion from degree to hundredth of second in metres
  metres <- degree_of_latitude*1000
  
  return(metres)
}

dlong <- function(latitude) {
  #Formula
  lat_rad <- latitude * pi/180
  degree_of_longitude <- (111.41288*cos(lat_rad)) - (0.09350*(cos(3*lat_rad))) + (0.00012*(cos(5*lat_rad)))
  #conversion from degree to hundredth of second in metres
  metres <- degree_of_longitude * 1000
  
  return(metres)
}

#Convert latitude and longitude to metres from centre (x,y)
#For some reason this function doesn't work well with mutate function
gps_to_xy <- function(lat,long,centre_lat,centre_long) {
  xy <- c(dlat(lat) * (lat - centre_lat), dlong(lat) * (long-centre_long))
  return(xy)
}

gps_to_y <- function(lat, centre_lat) {
  y <- dlat(lat) * (lat - centre_lat)
  return(y)
}

gps_to_x <- function(lat, long, centre_long) {
  x <- dlong(lat) * (long - centre_long)
  return(x)
}


#used for ground centre calculation
find_centre <- function(rounds, offset_lat, offset_long) {
  cb <- Stoppages_Attendance %>%
    filter(Round %in% rounds) %>%
    select(-MatchID, -TrueX, -TrueY, -PeriodSecs) %>%
    left_join(all_rounds, by = c("PlayerID" = "ID", "Round" = "Round", "DeciSecond" = "Deci")) %>%
    filter(Pentagraph_Stoppage == "CEBO", PlayerRole == "Ruck", PlayerID %in% Players$PlayerID) %>%
    na.omit()
  centre_x <- mean(cb$Longitude) - offset_long
  centre_y <- mean(cb$Latitude) + offset_lat
  return(c(centre_x, centre_y))
}


#Used for ground angle calculation
find_ground_angle <- function(ground_x,ground_y, rounds, offset_lat, offset_long) {
  kickins_end1 <- KickIns %>%
    left_join(all_rounds, by = c("Decisecond" = "Deci","PlayerID" = "ID", "Round" = "Round")) %>%
    filter(Round %in% rounds) %>%
    filter((Quarter %in% c(1,3) & StkParity == 0) | (Quarter %in% c(2,4) & StkParity == 1), Velocity < 100) %>%
    mutate(Latitude_adjusted = Latitude + offset_lat, Longitude_adjusted = Longitude - offset_long) %>%
    na.omit()
  kickins_end2 <- KickIns %>%
    left_join(all_rounds, by = c("Decisecond" = "Deci","PlayerID" = "ID", "Round" = "Round")) %>%
    filter(Round %in% rounds) %>%
    filter((Quarter %in% c(1,3) & StkParity == 1) | (Quarter %in% c(2,4) & StkParity == 0), Velocity < 100) %>%
    mutate(Latitude_adjusted = Latitude + offset_lat, Longitude_adjusted = Longitude - offset_long) %>%
    na.omit()
  end1 <- c(mean(kickins_end1$Latitude_adjusted), mean(kickins_end1$Longitude_adjusted))
  end2 <- c(mean(kickins_end2$Latitude_adjusted), mean(kickins_end2$Longitude_adjusted))
  ground_end1_xy <- c(gps_to_x(end1[1],end1[2],ground_x),gps_to_y(end1[1],ground_y))
  ground_end2_xy <- c(gps_to_x(end2[1],end2[2],ground_x),gps_to_y(end2[1],ground_y))
  opposite <- (ground_end1_xy[2]) - (ground_end2_xy[2])
  adjacent <- (ground_end1_xy[1]) - (ground_end2_xy[1])
  radians <- atan(opposite/adjacent)
  degrees <- rad2deg(radians)
  print(end1)
  print(end2)
  print(degrees)
  if(degrees < 0) {return(degrees + 180)}
  else(return(degrees))
}


#Convert x and y coordinates and angle of rotation
gps_to_yrot <- function(lat, long, centre_lat, centre_long, angle) {
  x <- dlong(lat) * (long - centre_long)
  y <- dlat(lat) * (lat - centre_lat)
  yrot <- (y*cos(deg2rad(angle))) - (x*sin(deg2rad(angle)))
  return(yrot)
}

gps_to_xrot <- function(lat, long, centre_lat, centre_long, angle) {
  x <- dlong(lat) * (long - centre_long)
  y <- dlat(lat) * (lat - centre_lat)
  xrot <- (x*cos(deg2rad(angle))) + (y*sin(deg2rad(angle)))
  return(xrot)
}




#GROUND INFORMATION _______________________________________________________________________________________________________

#MCG

mcg_x <- find_centre(rounds_mcg,0,0)[1]
mcg_y <- find_centre(rounds_mcg,0,0)[2]
mcg_angle <- find_ground_angle(mcg_x, mcg_y, rounds_mcg,0,0)

#TOWNSVILLE
townsville_x <- find_centre(rounds_townsville,0,0)[1]
townsville_y <- find_centre(rounds_townsville,0,0)[2]
townsville_angle <- find_ground_angle(townsville_x, townsville_y, rounds_townsville,0,0)

#ADELAIDE
adelaide_x <- find_centre(rounds_adelaide,0,0)[1]
adelaide_y <- find_centre(rounds_adelaide,0,0)[2]
adelaide_angle <- find_ground_angle(adelaide_x, adelaide_y, rounds_adelaide,0,0)

#MARVEL - Not working because of offset problem?
marvel_actual_x <- 144.9476208
marvel_actual_y <- -37.8165203
marvel_predicted_x <- find_centre(rounds_marvel,0,0)[1]
marvel_predicted_y <- find_centre(rounds_marvel,0,0)[2]
marvel_offset_long <- abs(marvel_actual_x - marvel_predicted_x)
marvel_offset_lat <- abs(marvel_actual_y - marvel_predicted_y)
marvel_x <- find_centre(rounds_marvel, marvel_offset_lat, marvel_offset_long)[1] 
marvel_y <- find_centre(rounds_marvel,marvel_offset_lat, marvel_offset_long)[2] 
marvel_angle <- find_ground_angle(marvel_x, marvel_y, rounds_marvel, marvel_offset_lat, marvel_offset_long)

#SCG 
scg_x <- find_centre(rounds_scg,0,0)[1]
scg_y <- find_centre(rounds_scg,0,0)[2]
scg_angle <- find_ground_angle(scg_x, scg_y, rounds_scg,0,0)

#BLUNDSTONE
blundstone_x <- find_centre(rounds_blundstone,0,0)[1]
blundstone_y <- find_centre(rounds_blundstone,0,0)[2]
blundstone_angle <- find_ground_angle(blundstone_x, blundstone_y, rounds_blundstone,0,0)

#KARDINIA
kardinia_x <- find_centre(rounds_kardinia,0,0)[1]
kardinia_y <- find_centre(rounds_kardinia,0,0)[2]
kardinia_angle <- find_ground_angle(kardinia_x, kardinia_y, rounds_kardinia,0,0)

#OPTUS
optus_x <- find_centre(rounds_optus,0,0)[1]
optus_y <- find_centre(rounds_optus,0,0)[2]
optus_angle <- find_ground_angle(optus_x, optus_y, rounds_optus,0,0)

#CHINA - Sort of works? one end slightly off
china_x <- find_centre(rounds_china,0,0)[1]
china_y <- find_centre(rounds_china,0,0)[2]
china_angle <- find_ground_angle(china_x, china_y, rounds_china,0,0)

#MANUKA
manuka_x <- find_centre(rounds_manuka,0,0)[1]
manuka_y <- find_centre(rounds_manuka,0,0)[2]
manuka_angle <- find_ground_angle(manuka_x, manuka_y, rounds_manuka,0,0)


#####Mutate GPS Data -------------------------------------------------------------------------------------------------------------------------------------------------

all_rounds_v2 <- all_rounds %>%
  mutate(Latitude_adjusted = case_when(Round %in% rounds_marvel ~ Latitude + marvel_offset_lat, ! Round %in% rounds_marvel ~ Latitude),
         Longitude_adjusted = case_when(Round %in% rounds_marvel ~ Longitude - marvel_offset_long,! Round %in% rounds_marvel ~ Longitude)) %>%
  mutate(Velocity = Velocity/3.6) %>%
  mutate(X = case_when(all_rounds$Round %in% rounds_marvel ~ gps_to_x(Latitude_adjusted,Longitude_adjusted, marvel_x),
                       all_rounds$Round %in% rounds_mcg ~ gps_to_x(Latitude,Longitude, mcg_x),
                       all_rounds$Round %in% rounds_adelaide ~ gps_to_x(Latitude,Longitude, adelaide_x),
                       all_rounds$Round %in% rounds_kardinia ~ gps_to_x(Latitude,Longitude, kardinia_x),
                       all_rounds$Round %in% rounds_scg ~ gps_to_x(Latitude,Longitude, scg_x),
                       all_rounds$Round %in% rounds_townsville ~ gps_to_x(Latitude,Longitude, townsville_x),
                       all_rounds$Round %in% rounds_blundstone ~ gps_to_x(Latitude,Longitude, blundstone_x),
                       all_rounds$Round %in% rounds_manuka ~ gps_to_x(Latitude,Longitude, manuka_x),
                       all_rounds$Round %in% rounds_china ~ gps_to_x(Latitude,Longitude, china_x),
                       all_rounds$Round %in% rounds_optus ~ gps_to_x(Latitude,Longitude, optus_x),
                       Round == 14 ~ gps_to_x(Latitude_adjusted,Longitude_adjusted, marvel_x)),
         Y = case_when(all_rounds$Round %in% rounds_marvel ~ gps_to_y(Latitude_adjusted, marvel_y),
                       all_rounds$Round %in% rounds_mcg ~ gps_to_y(Latitude, mcg_y),
                       all_rounds$Round %in% rounds_adelaide ~ gps_to_y(Latitude, adelaide_y),
                       all_rounds$Round %in% rounds_kardinia ~ gps_to_y(Latitude, kardinia_y),
                       all_rounds$Round %in% rounds_scg ~ gps_to_y(Latitude, scg_y),
                       all_rounds$Round %in% rounds_townsville ~ gps_to_y(Latitude, townsville_y),
                       all_rounds$Round %in% rounds_blundstone ~ gps_to_y(Latitude, blundstone_y),
                       all_rounds$Round %in% rounds_manuka ~ gps_to_y(Latitude, manuka_y),
                       all_rounds$Round %in% rounds_china ~ gps_to_y(Latitude, china_y),
                       all_rounds$Round %in% rounds_optus ~ gps_to_y(Latitude, optus_y),
                       Round == 14 ~ gps_to_y(Latitude_adjusted, marvel_y))) %>%
  mutate(Xrot = case_when(all_rounds$Round %in% rounds_marvel ~ gps_to_xrot(Latitude_adjusted,Longitude_adjusted, marvel_y, marvel_x, marvel_angle),
                          all_rounds$Round %in% rounds_mcg ~ gps_to_xrot(Latitude,Longitude, mcg_y, mcg_x, mcg_angle),
                          all_rounds$Round %in% rounds_adelaide ~ gps_to_xrot(Latitude,Longitude, adelaide_y, adelaide_x, adelaide_angle),
                          all_rounds$Round %in% rounds_kardinia ~ gps_to_xrot(Latitude,Longitude, kardinia_y, kardinia_x, kardinia_angle),
                          all_rounds$Round %in% rounds_scg ~ gps_to_xrot(Latitude,Longitude, scg_y, scg_x, scg_angle),
                          all_rounds$Round %in% rounds_townsville ~ gps_to_xrot(Latitude,Longitude, townsville_y, townsville_x, townsville_angle),
                          all_rounds$Round %in% rounds_blundstone ~ gps_to_xrot(Latitude,Longitude, blundstone_y, blundstone_x, blundstone_angle),
                          all_rounds$Round %in% rounds_manuka ~ gps_to_xrot(Latitude,Longitude, manuka_y, manuka_x, manuka_angle),
                          all_rounds$Round %in% rounds_china ~ gps_to_xrot(Latitude,Longitude, china_y, china_x, china_angle),
                          all_rounds$Round %in% rounds_optus ~ gps_to_xrot(Latitude,Longitude, optus_y, optus_x, optus_angle),
                          Round == 14 ~ gps_to_xrot(Latitude_adjusted,Longitude_adjusted, marvel_y, marvel_x, marvel_angle)),
         Yrot = case_when(all_rounds$Round %in% rounds_marvel ~ gps_to_yrot(Latitude_adjusted,Longitude_adjusted, marvel_y, marvel_x, marvel_angle),
                          all_rounds$Round %in% rounds_mcg ~ gps_to_yrot(Latitude,Longitude, mcg_y, mcg_x, mcg_angle),
                          all_rounds$Round %in% rounds_adelaide ~ gps_to_yrot(Latitude,Longitude, adelaide_y, adelaide_x, adelaide_angle),
                          all_rounds$Round %in% rounds_kardinia ~ gps_to_yrot(Latitude,Longitude, kardinia_y, kardinia_x, kardinia_angle),
                          all_rounds$Round %in% rounds_scg ~ gps_to_yrot(Latitude,Longitude, scg_y, scg_x, scg_angle),
                          all_rounds$Round %in% rounds_townsville ~ gps_to_yrot(Latitude,Longitude, townsville_y, townsville_x, townsville_angle),
                          all_rounds$Round %in% rounds_blundstone ~ gps_to_yrot(Latitude,Longitude, blundstone_y, blundstone_x, blundstone_angle),
                          all_rounds$Round %in% rounds_manuka ~ gps_to_yrot(Latitude,Longitude, manuka_y, manuka_x, manuka_angle),
                          all_rounds$Round %in% rounds_china ~ gps_to_yrot(Latitude,Longitude, china_y, china_x, china_angle),
                          all_rounds$Round %in% rounds_optus ~ gps_to_yrot(Latitude,Longitude, optus_y, optus_x, optus_angle),
                          Round == 14 ~ gps_to_yrot(Latitude_adjusted,Longitude_adjusted, marvel_y, marvel_x, marvel_angle)))

## Version 3  excludes some implauisble values
all_rounds_v3 <- all_rounds_v2 %>%
  filter(Velocity < 10, Latitude >= -180 & Latitude <= 180, Longitude >= -180 & Longitude <= 180, Xrot < 200 & Xrot > -200, Yrot <200 & Yrot > -200)

###reduced rounds excludes removed rounds. It also adds some contextunal information from I50TO and does a little bit of tidying

reduced_rounds <- all_rounds_v3 %>%
  filter(Round != 20 & Round != 7 & Round != 22 & Round != 23) %>%
  mutate(Deci = case_when(Round == 11 ~ Deci - 350,
                          Round != 11 ~ Deci)) %>%
  mutate(Deci = case_when(Round == 6 ~ Deci - 120,
                          Round != 6 ~ Deci)) %>%
  left_join(select(I50TO,EndDeci, id, Round, Quarter, EndX, EndY), by = c("Deci" = "EndDeci", "Round" = "Round")) %>%
  left_join(select(Matches, "StK Parity", Round), by = c("Round" = "Round")) %>%
  rename(StkParity = "StK Parity") %>%
  na.omit() %>%
  mutate(TOid = as.numeric(id)) %>%
  arrange(TOid) %>%
  select(-id, -Latitude, -Longitude, -X, -Y, -Latitude_adjusted, -Longitude_adjusted) %>%
  #Y axis is flipped for AFL data
  mutate(Yrot = Yrot * -1) %>%
  #Flip x-axis for all grounds required
  mutate(Xrot = case_when(Round == 14 ~ Xrot * -1,
                          Round %in% rounds_mcg ~ Xrot * -1,
                          Round %in% rounds_marvel ~ Xrot,
                          Round %in% rounds_adelaide ~ Xrot,
                          Round %in% rounds_optus ~ Xrot * -1,
                          Round %in% rounds_china ~ Xrot * -1,
                          Round %in% rounds_kardinia ~ Xrot * -1,
                          Round %in% rounds_blundstone ~ Xrot * -1,
                          Round %in% rounds_manuka ~ Xrot,
                          Round %in% rounds_scg ~ Xrot,
                          Round %in% rounds_townsville ~ Xrot * -1)) 


#####Functions for creating a point in time slice of players included in analysis and for plotting this point in time with the turnover point indicated----------------------------
plot_players_byTOID <- function(turnover_id) {
  option1 <- reduced_rounds1 %>%
    filter(TOid == turnover_id)
  plot <-  ggplot() +
    geom_point(aes(x = Xrot, y = Yrot),data = option1) +
    geom_point(aes(x = EndX, y = EndY), data = option1, color = "red") +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = option1$Ground_x/2, b = option1$Ground_y/2, angle = 0)) +
    coord_fixed(ratio = 1)
  return(plot)
}


#This function plots players included in analysis and the TO point
plot_behind_TO <- function(turnover_id, ground_length, ground_width) {
  option1 <- reduced_rounds %>%
    filter(TOid == turnover_id & Yrot < ground_width & Yrot > -(ground_width) & Xrot < I50TO$EndX[turnover_id] + 5  & Velocity > 0.00)
  option2 <- reduced_rounds %>%
    filter(TOid == turnover_id & Yrot < ground_width & Yrot > -(ground_width) & Xrot > I50TO$EndX[turnover_id] - 5  & Velocity > 0.00)
  plot1 <-  ggplot() +
    geom_point(aes(x = Xrot, y = Yrot),data = option1) +
    geom_point(aes(x = EndX, y = EndY), data = option1, color = "red") +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = ground_length, b = ground_width, angle = 0)) +
    coord_fixed(ratio = 1) 
  plot2 <-  ggplot() +
    geom_point(aes(x = Xrot, y = Yrot),data = option2) +
    geom_point(aes(x = EndX, y = EndY), data = option2, color = "red") +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = ground_length, b = ground_width, angle = 0)) +
    coord_fixed(ratio = 1)  
  if((I50TO$Quarter[turnover_id] %in% c(1,3) & I50TO$StkParity[turnover_id] == 0) | (I50TO$Quarter[turnover_id] %in% c(2,4) & I50TO$StkParity[turnover_id] == 1)) {
    return(plot1)
  }
  if((I50TO$Quarter[turnover_id] %in% c(1,3) & I50TO$StkParity[turnover_id] == 1) | (I50TO$Quarter[turnover_id] %in% c(2,4) & I50TO$StkParity[turnover_id] == 0)) {
    return(plot2)
  }
}


# This function returns a datafram with included players
behind_TO <- function(turnover_id, ground_length, ground_width) {
  option1 <- reduced_rounds %>%
    filter(TOid == turnover_id & Yrot < ground_width & Yrot > -(ground_width) & Xrot < I50TO$EndX[turnover_id] + 5)
  option2 <- reduced_rounds %>%
    filter(TOid == turnover_id & Yrot < ground_width & Yrot > -(ground_width) & Xrot > I50TO$EndX[turnover_id] - 5)
  if((I50TO$Quarter[turnover_id] %in% c(1,3) & I50TO$StkParity[turnover_id] == 0) | (I50TO$Quarter[turnover_id] %in% c(2,4) & I50TO$StkParity[turnover_id] == 1)) {
    return(option1)
  }
  if((I50TO$Quarter[turnover_id] %in% c(1,3) & I50TO$StkParity[turnover_id] == 1) | (I50TO$Quarter[turnover_id] %in% c(2,4) & I50TO$StkParity[turnover_id] == 0)) {
    return(option2)
  }
}

#This function plots players with a generic AFL ground generated behind
plot_players <- function(deci,round) {
  point_in_time <- all_rounds_v2 %>%
    filter(Deci == deci & Round == round)
  plot <-  ggplot() +
    geom_point(aes(x = Xrot, y = Yrot),data = point_in_time) +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = 80, b = 60, angle = 0)) +
    geom_curve(aes(x = 50, y = 48, xend = 50, yend = -48)) +
    geom_curve(aes(x = -50, y = 48, xend = -50, yend = -48), curvature = -0.5) +
    geom_line(aes(x = c(-25,25), y = c(25,25))) +
    geom_line(aes(x = c(-25,25), y = c(-25,-25))) +
    geom_line(aes(x = c(-25,-25), y = c(-25,25))) +
    geom_line(aes(x = c(25,25), y = c(-25,25))) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 4)) +
    coord_fixed(ratio = 1) 
  return(plot)
}

####Potential problem instance identification####
#######Inspecting risky instances closer. i.e. those where my initial methods of automatic exclusion and manual exclusion may not have picked up bench players.
risky <-  Analysis_variables %>%
  left_join(select(reduced_rounds1, TOid, Ground_y)) %>%
  unique() %>%
  filter(-(Ground_y/2) - Ycentroid >= -(Ground_y/3))

view(risky)

for(id in c(1:10)) {
  plot <- plot_behind_TO(risky$TOid[id], marvel_length/2, marvel_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

for(id in c(11:13)) {
  plot <- plot_behind_TO(risky$TOid[id], kardinia_length/2, kardinia_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

for(id in c(14:22)) {
  plot <- plot_behind_TO(risky$TOid[id], blundstone_length/2, blundstone_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

for(id in c(23:27)) {
  plot <- plot_behind_TO(risky$TOid[id], marvel_length/2, marvel_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

for(id in c(28:29)) {
  plot <- plot_behind_TO(risky$TOid[id], china_length/2, china_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

for(id in c(30)) {
  plot <- plot_behind_TO(risky$TOid[id], marvel_length/2, marvel_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

for(id in c(31:34)) {
  plot <- plot_behind_TO(risky$TOid[id], mcg_length/2, mcg_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

for(id in c(35:42)) {
  plot <- plot_behind_TO(risky$TOid[id], marvel_length/2, marvel_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

for(id in c(43:44)) {
  plot <- plot_behind_TO(risky$TOid[id], mcg_length/2, mcg_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

for(id in c(45:47)) {
  plot <- plot_behind_TO(risky$TOid[id], marvel_length/2, marvel_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

for(id in c(48)) {
  plot <- plot_behind_TO(risky$TOid[id], optus_length/2, optus_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

for(id in c(48:59)) {
  plot <- plot_behind_TO(risky$TOid[id], marvel_length/2, marvel_width/2) +
    labs(title = risky$TOid[id]) 
  print(plot)
}

###### Creating secondary variables for analysis ---------------------------------------------------------------------------------------------------------------------



###Filtering out bench players and those not included in analysis
reduced_rounds1 <- reduced_rounds %>%
  ###Add ground dimensions
  mutate(Ground_x = case_when(Round %in% c(14) ~ marvel_width,
                              Round %in% rounds_mcg ~ mcg_length,
                              Round %in% rounds_marvel ~ marvel_length,
                              Round %in% rounds_adelaide ~ adelaide_length,
                              Round %in% rounds_optus ~ optus_length,
                              Round %in% rounds_china ~ china_length,
                              Round %in% rounds_kardinia ~ kardinia_length,
                              Round %in% rounds_blundstone ~ blundstone_length,
                              Round %in% rounds_manuka ~ manuka_length,
                              Round %in% rounds_scg ~ scg_length,
                              Round %in% rounds_townsville ~ townsville_length),
         Ground_y = case_when(Round %in% c(14) ~ marvel_width,
                              Round %in% rounds_mcg ~ mcg_width,
                              Round %in% rounds_marvel ~ marvel_width,
                              Round %in% rounds_adelaide ~ adelaide_width,
                              Round %in% rounds_optus ~ optus_width,
                              Round %in% rounds_china ~ china_width,
                              Round %in% rounds_kardinia ~ kardinia_width,
                              Round %in% rounds_blundstone ~ blundstone_width,
                              Round %in% rounds_manuka ~ manuka_width,
                              Round %in% rounds_scg ~ scg_width,
                              Round %in% rounds_townsville ~ townsville_width))  %>%
  filter((((Quarter %in% c(1,3) & StkParity == 0) | (Quarter %in% c(2,4) & StkParity == 1))&(Yrot < Ground_y/2 & Yrot > -(Ground_y/2) & Xrot < EndX + 5) & Velocity > 0.00) |
           ((Quarter %in% c(1,3) & StkParity == 1) | (Quarter %in% c(2,4) & StkParity == 0))&(Yrot < Ground_y/2 & Yrot > -(Ground_y/2) & Xrot > EndX - 5) & Velocity > 0.00) %>%
  #Manually handling bench players who haven't been automatically excluded
  filter(TOid != 281 | ID != 1003520, TOid != 281 | ID != 1002213, TOid != 281 | ID != 295015,
         TOid != 282 | ID != 295015, TOid != 282 | ID != 260113,
         TOid != 309 | ID != 291773, TOid != 309 | ID != 990548,
         TOid != 410 | ID != 296205, TOid != 410 | ID != 992468,
         TOid != 304 | ID != 1002213,
         TOid != 307 | ID != 296294,
         TOid != 398 | ID != 992468,
         TOid != 415 | ID != 260113,
         TOid != 484 | ID != 296294,
         TOid != 319 | ID != 294570, TOid != 319 | ID != 291492, TOid != 319 | ID != 295015,
         TOid != 272 | ID != 998134,
         TOid != 305 | ID != 294429,
         TOid != 156 | ID != 992468,
         TOid != 504 | ID != 1003520,
         TOid != 409 | ID != 992468,
         TOid != 304 | ID != 1002213,
         TOid != 401 | ID != 993917,
         TOid != 415 | ID != 260113,
         TOid != 320 | ID != 298421,
         TOid != 335 | ID != 298421,
         TOid != 268 | ID != 280858,
         TOid != 276 | ID != 1005717,
         TOid != 269 | ID != 296422, TOid != 269 | ID != 291492,
         TOid != 257 | ID != 250340,
         TOid != 79 | ID != 280858,
         TOid != 279 | ID != 260113,
         TOid != 408 | ID != 296205,
         TOid != 431 | ID != 296422,
         TOid != 250 | ID != 993821,
         TOid != 308 | ID != 280858,
         TOid != 277 | ID != 293846,
         TOid != 192 | ID != 296294
  ) %>%
  mutate(Direction = case_when(((Quarter %in% c(1,3) & StkParity == 0) | (Quarter %in% c(2,4) & StkParity == 1)) ~ "Right",
                               ((Quarter %in% c(1,3) & StkParity == 1) | (Quarter %in% c(2,4) & StkParity == 0)) ~ "Left"))


#Write data files
write.csv(Stoppages_Attendance, "Data/Stoppages_Attendance.csv")
write.csv(I50TO, "Data/I50TO.csv")
write.csv(reduced_rounds1,"Data/reduced_rounds1.csv")


###entry zone data collection and kappa testing####
library(irr)

I50TO_Entry_zone <- read_csv("Data/I50TOEntryzone.csv") %>%
  select(TOid, Entry_zone) %>%
  filter(TOid %in% c(115:143) | TOid %in% c(423:445))

I50TO_Entry_zone_reliability <- read_csv("Data/I50TOreliability.csv") %>%
  select(TOid, Entry_zone)

#Check dataframes are the same length 
nrow(I50TO_Entry_zone)
nrow(I50TO_Entry_zone_reliability)
#Both are 51 instances long


#Calculate kappa statistic
Kappa_test <- kappa2(cbind(I50TO_Entry_zone$TOid,I50TO_Entry_zone_reliability$TOid))
view(Kappa_test)


##Convex hull area function
cha<-function(x,y){
  chull(x,y)->i
  return(areapl(cbind(x[i],y[i])))
}

Area_calculations <- reduced_rounds1 %>%
  group_by(TOid) %>%
  mutate(Hull_area = list(cha(Xrot,Yrot)))

#
Analysis_variables1 <- reduced_rounds1 %>%
  ### Add summary statitic variables by TOid in summarise
  group_by(TOid) %>%
  summarise(Xcentroid = mean(Xrot),
            Ycentroid = mean(Yrot),
            n_players = n(),
            Length = max(Xrot) - min(Xrot),
            Width = max(Yrot) - min(Yrot),
            Mean_dist_from_cent = mean(sqrt(((Xrot-Xcentroid)^2) + ((Yrot-Ycentroid)^2))),
            Mean_dist_from_TO = mean(sqrt(((Xrot-EndX)^2) + ((Yrot-EndY)^2))),
            SD_dist_from_cent = sd(sqrt(((Xrot-Xcentroid)^2) + ((Yrot-Ycentroid)^2)))) %>%
  ###Add anything from I50TO1 in select() to add to new table
  left_join(dplyr::select(I50TO1, Round, id, EndX, EndY, Result_code, Direction, ReturnGoalDist, Entry_zone), by = c("TOid" = "id")) %>%
  rename(TOx = EndX, TOy = EndY) %>%
  ###Add new variables derived from others already in the new table
  mutate(Xcent_dist_from_TO = abs(TOx - Xcentroid),
         Ycent_dist_from_TO = abs(TOy - Ycentroid),
         Rcent_dist_from_TO = sqrt((Xcent_dist_from_TO^2) + (Ycent_dist_from_TO^2)),
         Result_code = factor(Result_code, labels = c("Negative", "Positive", "OppoRushed", "Unresolved"))) %>%
  ###Add new variables derived from other tables
  left_join(distinct(Area_calculations, as.numeric(Hull_area))) %>%
  rename(Hull_area = "as.numeric(Hull_area)") %>%
  mutate(m2_per_player = Hull_area/n_players) %>%
  #Filter for only positive, neutral and negative instances
  filter(Result_code %in% c("Negative", "Neutral", "Positive"))  %>%
  #Create normalised values for X,Y Centroid and TO X,Y
  mutate(XcentroidNorm = case_when(Direction == "Left" ~ Xcentroid*-1,
                                   Direction == "Right" ~ Xcentroid),
         YcentroidNorm = case_when(Direction == "Left" ~ Ycentroid*-1,
                                   Direction == "Right" ~ Ycentroid),
         TOxNorm = case_when(Direction == "Left" ~ TOx*-1,
                             Direction == "Right" ~ TOx),
         TOyNorm = case_when(Direction == "Left" ~ TOy*-1,
                             Direction == "Right" ~ TOy))


##Adding categorical variables
Analysis_variables_cat <- Analysis_variables1 %>%
  ##This is calculated with all players heading towards the right. 1 = Left, 2 = Corridor, 3 = Right
  mutate(Ycentroid_cat = case_when(YcentroidNorm > 25 ~ "Left",
                                   YcentroidNorm <= 25 & YcentroidNorm >= -25 ~ "Right",
                                   YcentroidNorm < -25 ~ "Corridor"),
         TOy_cat = case_when(TOyNorm > 25 ~ "Left",
                             TOyNorm <= 25 & TOyNorm >= -25 ~ "Corridor",
                             TOyNorm < -25 ~ "Right"),
         # 0 = No, 1 = Yes
         Same_path = case_when(Ycentroid_cat != TOy_cat ~ "No",
                               Ycentroid_cat == TOy_cat ~ "Yes"),
         # 0 = Wider than long, 1 = Longer than wide
         Wide_or_Long = ifelse(Width > Length, "Wide", "Long"),
         Entry_zone2 = case_when(Entry_zone == "Left" | Entry_zone == "Right" ~ "Wide",
                                 Entry_zone == "Corridor" ~ "Corridor"),
         Ycentroid_cat2 = case_when(Ycentroid_cat %in% c("Left","Right") ~ "Wide",
                                    Ycentroid_cat == "Corridor" ~ "Corridor"))

write.csv(Analysis_variables_cat, "Data/Analysis_variables_cat.csv")