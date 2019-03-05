#
#
#  This file contains supplementary project code
#
#

library(dplyr) # 
library(opencage) # geocoding
library(stringr) # regex & string processing
library(NLP) # text tokenizer


# verify Open Cage API key
Sys.getenv("OPENCAGE_KEY")

# This function uses OpenCage API to resolve address to latitude and longitude
# It returns a vector of  (lat, lng)
getCoordinates <- function(address, city,  state, countryCode) {
  coord = rep(NA,2)
  vec = c()
  if (!missing(address) && !is.na(address)) {
      vec = c(trimws(address))
  }  
  if (!missing(city)){
      vec = c(vec,trimws(city))
  }
  if (!missing(state)) {
      vec = c(vec, trimws(state))
  }
  address = paste0(vec, collapse = ", ")
  if (missing(countryCode)){
     coord <- opencage_forward(placename = address, limit = 1, min_confidence = 1)
  }
  else {
      coord <- opencage_forward(placename = address, countrycode = trimws(countryCode), limit = 1, min_confidence = 6)
  }
  c(coord$results$geometry.lat, coord$results$geometry.lng)
  
}

imputeCoordinates <- function() {
  # load data
  data = read.csv("../data/gun-violence-data_01-2013_03-2018.csv", header = T, na.strings = c("NA","","#NA"),sep=",")

  # the dataset is prohibitatively large. Take the last year : 2017/2018 
  # convert date to Date type
  data$date = as.Date(as.character(data$date),"%Y-%m-%d")
  # sort by date
  data = data[order(data$date),] 
  # take the last year observations from data set
  #data = data %>% filter(date >= as.Date('2017-03-01'))

  missingCoord <- data %>% filter(is.na(latitude) )

  for (idx in 1:nrow(missingCoord)) {
    tryCatch({
      coord = getCoordinates(address = as.character(missingCoord[idx,"address"]),
                           city = as.character(missingCoord[idx,"city_or_county"]),
                           state = (missingCoord[idx,"state"]))
      missingCoord[idx,"latitude"] = as.double(coord[1])
      missingCoord[idx,"longitude"] = as.double(coord[2])
      # give it a 1 sec pause (free account requirement)
      Sys.sleep(1)
    
    }, error = function(e) {
      print(e)
    })
  }

  # now let's populate missing lng, lat with the data from missingcoord dataframe
  tmp = merge(data, missingCoord,by =  "incident_id", all.x = T, suffixes = c("",".appended")) 
  tmp = tmp %>% mutate(latitude = ifelse(is.na(latitude.appended), latitude, latitude.appended),
                     longitude = ifelse(is.na(longitude.appended), longitude, longitude.appended) ) 

  # drop *.appended columns
  tmp = tmp[ -c(30:57) ]
  # write.csv(tmp,'../data/gun-violence-with-all-coord.csv', row.names = F)
}

# @description: This fanction parases a string of the following format [idx1::value1||idx2::value2]
# @return: a named vector, where name is an index (idx1,idx2,..., idxN) , and value is an alphanumeric value (value1, value2,..., valueN)
# Example of the participant type:
#        0                 1                 2                 3                 4 
# "Victim"          "Victim" "Subject-Suspect" "Subject-Suspect"          "Victim" 
parseFeature <- function(val) {
  if(missing(val)){
    return(NA)
  }
  splitted = str_split(val, fixed("||"), simplify = T)
  v = c()
  for(el in splitted){
    entry = str_split(el, fixed("::"), simplify = T)
    if (length(entry) == 2){
      # we create a named vecotor, where name is an entry index and value is the antry vlaue
      v = c(v,setNames(c(entry[1,2]), c(entry[1,1])))
    }  
  }
  return(v)
}

BigramTokenizer <-  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = F)

TrigramTokenizer <-  function(x)
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = F)

isNaOrNull <- function(x)
  is.na(x) || is.null(x)

ggplotColours <- function(n = 15, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}  