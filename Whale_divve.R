#load Libraries
library(dplyr)
library(readr)
library(sf)
library(base)

#load clsfheaders#load clean data
dives <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/whales-dives.csv')

head(dives)

#load messy data
messy_dives <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/whales-dives-messy.csv')

head(messy_dives)

#messy_dives <- messy_dives %>%mutate(YEAR = str_pad(YEAR,width=3,side="left",pad="0")) %>%mutate(YEAR = str_pad(YEAR,width=4,side="left",pad="2")) %>%mutate(Day = str_pad(Day,width=2,side="left",pad="0")) %>%mutate(Month = str_pad(Month,width=2,side="left",pad="0"))%>%mutate(sit= substr(sit, 10,12))

#messy_dives$id <- paste0(messy_dives$YEAR,messy_dives$Month,messy_dives$Day)
#messy_dives<- messy_dives%>%arrange(messy_dives, desc(id))

messy_dives1 <- messy_dives %>%
  mutate(YEAR = str_pad(YEAR,width=3,side="left",pad="0")) %>%
  mutate(YEAR = str_pad(YEAR,width=4,side="left",pad="2")) %>%
  mutate(Day = str_pad(Day,width=2,side="left",pad="0")) %>%
  mutate(Month = str_pad(Month,width=2,side="left",pad="0"))%>%
  mutate(sit= substr(sit, 10,12))


messy_dives1$id <- paste0(messy_dives1$YEAR,messy_dives1$Month,messy_dives1$Day,messy_dives1$sit)

messy_dives1 <- subset(messy_dives1, select = -c(YEAR,Month,Day,sit) )

#reorder columns
messy_dives1 <- messy_dives1 %>% select(id, Species.ID, bhvr, PreyVolume, PreyDepth, Dive_Time, Surfacetime, Blow.Interval, Blow_number_count)
messy_dives1

#rename columns
names(messy_dives1) <- c(
  'id', #'id',
  'species',#'Species.ID',
  'behavior',#'bhvr',
  'prey.volume', #'PreyVolume',
  'prey.depth',#'PreyDepth',
  'dive.time',#'Dive_Time',
  'surface.time',#'Surfacetime',
  'blow.interval',#'Blow.Interval',
  'blow.number'#'Blow_number_count
)
#complete.cases(messy_dives)
#messy_dives <- messy_dives[complete.cases(messy_dives),]

messy_dives1 <-messy_dives1%>%
  na.omit(TRUE)

messy_dives1 <-messy_dives1%>% distinct()

table(messy_dives$Species.ID)
messy_dives1<-messy_dives1 %>%
  mutate(species=case_when(species%in%c('fin','finderbender','FinW','FinWhale','fw') ~'FW',
                           !species %in% c('fin','finderbender','FinW','FinWhale','fw') ~ species)) %>%
  mutate(species=case_when(species%in%c('humperdink','hw','Hw') ~'HW',
                           !species %in% c('humperdink','hw','Hw') ~ species))
#messy_dives1<-messy_dives1 %>%filter(behavior%in%c('FEED','OTHER')) %>%filter(species%in%c('FW','HW'))

messy_dives1<- messy_dives1%>%
  arrange(messy_dives1, desc(id))



