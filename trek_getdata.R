#collect data
library(rvest)
library(googlesheets)
library(stringr)
library(ggplot2)
library(stringr)


#function to pull the script from a given URL:
beam_me_in <- function(url) {
  url_string <- url
  html_readin <- read_html(url_string) #Return an XML doc of the episode's site
  xml_find_all(x=html_readin, xpath = "//tr")  
  td_element <- html_nodes(html_readin, css = "td") #extract contents w/ css selector
  script_text <- html_text(td_element)  #extract text from the body
  return(script_text)  #return the script for that URL's episode.  Unformatted.
}

#Read in a table of every episode listing you'll need.
my_sheets <- gs_ls()
#you may need to auth in and allow tidyverse permissions.
trek_key <- "1aPUPBR-QxOJmxLCGyoe7s6i0odI3aeC2mw393LiiHPs" #find this in your my_sheets table
episode_gsheet <- gs_key(trek_key) #pulls in specific google sheet data
gsheet_names <- gs_ws_ls(episode_gsheet) #find the list of tab names
#read in the first 
episodes.all <- episode_gsheet %>% #Read in the episode google sheet
                gs_read(gsheet_names[1]) %>%  #specifically the first tab ("Sheet1")
                as.data.frame()  #coerce it to be a data.frame for better data handling later


#Initialize blank variable where script will go for each episode
episodes.all[, "script"] <- "blank"

#populate the "script" var in episodes with the actual script!
for (i in 1:nrow(episodes.all)) {
  #Pull out that episode's script
  url <- episodes.all[i, "Link"]
  episode_script <- beam_me_in(url)
  episodes.all[i, "script"] <- episode_script
}

#Manually separate out the scripts in the "to be continued" episodes.
#Update the To Be Continued syntax so your next regex catches all 4 split episodes
episodes.all[517, "script"] <- str_replace(episodes.all[517, "script"], 
            pattern = "To be continued", 
            replacement = "To Be Continued")

#Verify there are no duplicate Link elements; 
episodes.all[duplicated(episodes.all$Link), "Link"]

# For your set of 4 shared url episodes, replace script var with the correct 
# half of the script (before/after "to be continued").
  # keep only second half of script in the continuation episode
  for (i in c(493, 518, 567, 593)) {
    episodes.all[i, "script"] <- gsub(pattern = "(.*)(To Be Continued)(.*)",
                    replacement = "\\3", 
                    x = episodes.all[i, "script"])
  }
  #keep only the first half of script (including to be continued) in the initial
  for (i in c(492, 517, 566, 592)) {
    episodes.all[i, "script"] <- gsub(pattern = "(.*To Be Continued)(.*)",
                                      replacement = "\\1", 
                                      x = episodes.all[i, "script"])
  }


#function to extract stardate from a given URL:
get_stardate <- function(url) {
  url_string <- url
  html_readin <- read_html(url_string) #Return an XML doc of the episode's site
  xml_find_all(x=html_readin, xpath = "//tr")  
  p_element <- html_nodes(html_readin, css = "p") #extract contents of p nodes
  header_text <- html_text(p_element)  #extract text
  header_text_raw <- header_text[1]  #header information
  #Replace newlines and returns with space characters.
  header_text_raw <- gsub(pattern = "\\r",
                          replacement = " ", 
                          x = header_text_raw)
  header_text_raw <- gsub(pattern = "\\n",
                          replacement = " ", 
                          x = header_text_raw)
  stardate <- gsub(pattern = "(.*)(Stardate:)( *)(\\d*.\\d*)(.*)",
                             replacement = "\\4",
                             x = header_text_raw)
  return(stardate)
  
}

#Enterprise doesn't use stardate (THANKS) so we have to have a 
#second variable and find a way to convert between them.
episodes.all$stardate <- ""
episodes.all$missiondate <- ""
for (i in 1:615) {
  #Pull out that episode's stardate
  url <- episodes.all[i, "Link"]
  episode_stardate <- get_stardate(url)
  episodes.all[i, "stardate"] <- episode_stardate
  if (str_detect(episode_stardate, "Stardate:  Unknown")){
    episodes.all[i, "stardate"] <- NA
  } else if (str_detect(episode_stardate, "Stardate: Unknown")) {
    episodes.all[i, "stardate"] <- NA
  }
}



#Set missiondate + stardate to NA for episodes where it either isn't mentioned
#or is explicitly said to be "unknown".
for (i in 616:712) {
  if (str_detect(episodes.all[i, "stardate"], "Mission date:") == FALSE) {
    episodes.all[i, "missiondate"] <- NA
    episodes.all[i, "stardate"] <- NA
    
  } else if (str_detect(episodes.all[i, "stardate"], "Mission date: Unknown") == TRUE) {
    episodes.all[i, "missiondate"] <- NA
    episodes.all[i, "stardate"] <- NA
    
  }
}

#identify the list of episodes which contain some text indicating mission date
realmissiondates <- c(which(!is.na(episodes.all[616:712, "stardate"]))) + 615
#for each of those episodes, regex out a mission date; put it in the missiondate
#var.  And then set stardate to NA since you aren't sure what it is yet.
for (i in realmissiondates) {
  episodes.all[i, "missiondate"] <- gsub(pattern = "(.*)(Mission date: )(.*)( Original)(.*)",
                                                       replacement = "\\3",
                                                       x = episodes.all[i, "stardate"])
  episodes.all[i, "stardate"] <- NA
}

#Additional manual tweaks upon inspection:
episodes.all[642, "missiondate"] <- "Apr 1, 2152" #format
episodes.all[563, "missiondate"] <- "Apr 22, 2375" #format + initially in stardate.
episodes.all[630, "missiondate"] <- "Nov 9, 2151" #format
episodes.all[464, "stardate"] <- "48734.2" #someone fat fingered a digit.  Confirmed in script.
episodes.all[529, "stardate"] <- "51471.3" #someone fat fingered a digit.  Confirmed in script.
episodes.all[607, "stardate"] <- "54584.3" #someone fat fingered a digit.  Confirmed in script.
episodes.all[1, "stardate"] <- NA #unaired, no date.
#Manually fix several stardates that escaped our regex's attention.
  stardate_manualid <- c(525, 607, 502, 563)
  stardate_manualdate <- c("51268.4", "545484.3", "50384.2", NA)
  episodes.all[stardate_manualid, "stardate"] <- stardate_manualdate

#Convert stardate to a number
episodes.all$stardate <- as.numeric(episodes.all$stardate)


#Write out episodes.all to csv so you don't have to keep scraping a ton of urls
#to reform the data frame.
write.csv(episodes.all, file = "all_startrek_episode_scripts.csv")

#Plot hist to see if you have any undetected outliers
#episodes.all <- all_startrek_episode_scripts #Start here if you don't want to reimport everything.
ggplot(episodes.all, aes(x=stardate, fill = Series)) + 
  geom_histogram() + 
  ggtitle("Stardate histogram")

#percent of episodes with a given stardate.  Missing "Enterprise" due to Mission Date.
episodes.all$stardate %>% 
  is.na() %>%
  mean()
script_string <- episodes.all[1,"script"]
#function to clean one script, given it's string blob:
clean_episode_string <- function(script_string) {
  testdf <- as.data.frame(script_string)
  names(testdf) <- "script"

  #Replace curly braces with brackets for cleaner regex down the line.
  testdf$script <- gsub("\\{", "\\[", testdf$script)
  testdf$script <- gsub("\\}", "\\]", testdf$script)
  
  #Before you remove newline metachars, evaluate each open bracket to delete
  #all content either ending with a close parens or the newline.
  testdf$script <- gsub("\\[[^]\r\n]*(?:]|\\R)", "", testdf$script, perl=TRUE)
    # \\[         an open bracket
    # [^]\r\n]*  0+ chars other than ], CR and LF
    # (?:]|\\R)   either a ] (]) or (|) line break sequence (\R)
    #Future Jessie - answered at https://stackoverflow.com/questions/48489825/gsub-bracketed-content-occasionally-bound-by-newline-instead-of-closing-bracket?noredirect=1#comment83973951_48489825
  
  #Remove the (.*) lines with similar approach.
  testdf$script <- gsub("\\([^)\r\n]*(?:\\)|\\R)", " ", testdf$script, perl=TRUE)
    # \\(         an open parens
    # [^)\r\n]*  0+ chars other than ), CR and LF
    # (?:\\)|\\R)   either a ')'  or (|) line break sequence (\R)
  
  #Take out all the \r\n  metachars
  testdf$script <- gsub("\\r", " ", testdf$script)
  testdf$script <- gsub("\\n", " ", testdf$script)
  
  #Add a '~' in front of each speaker's name, for each line.
  testdf$script <- gsub("([A-Z]* *[A-Z]* *:)","~\\1", testdf$script)
  
  # Split string to new rows based on that ~ char.
  testdf.split <- strsplit(testdf$script, "~" )
  testdf.lines3 <- testdf.split %>% data.frame() 
  names(testdf.lines3) <- "lines"
  
  testdf.lines4 <- testdf.lines3
  
  #Now split each line into Char | Line
  testdf.lines4$char <- testdf.lines3$lines
  testdf.lines4$line <- testdf.lines3$lines
  
  #Populate each col with their appropriate portion of the entire line.
  testdf.lines4$char <- gsub("([A-Z]* *[A-Z]*)( *:.*)","\\1", testdf.lines4$char)
  testdf.lines4$line <- gsub("(.*:)(.*)","\\2", testdf.lines4$line)
  
  return(testdf.lines4[, c("char", "line")])  #return the script for that URL's episode.  Unformatted.
}


#This generates a MASSIVE file of all script lines by character for all episodes.
#It skips some episodes that my regex doesn't play well with... 9, 14 for example and I'm
#sure there are many more.  
#TODO - tryCatch() to figure out which ones you're skipping.
ScriptLines_append <- c("char","line") #initialize blank vector
for (i in c(1:712)) {
  try({tempscript_byline <- clean_episode_string(episodes.all[i, "script"])
  ScriptLines_append <- rbind(ScriptLines_append, tempscript_byline)}, silent = TRUE)
}

#write that to file, so you don't need to run it each time. Takes about 60s on my laptop.
write.csv(ScriptLines_append, file = "most_startrek_script_lines.csv")

# #TODO - Figure this out so you can tag each script line with metadata that may be
# #fun for future visualization.
# 
# #Initialize an empty for all episodes, with each row being a line in the script.
# #This will be used to rbind all the individual episode scripts.
# all_episodes_by_line <- NULL
# all_episodes_by_line <- data.frame(EpisodeName=character(),
#                                    Production=character(),
#                                    Airdate=character(),
#                                    Series=character(),
#                                    Season=character(),
#                                    URL_string=character(),
#                                    char=character(),
#                                    script_line=character(),
#                                    stardate=numeric(),
#                                    missiondate=character(),
#                                    stringsAsFactors=FALSE)
# 
# #Should this just be a different function?  Take One Episode Script
# for (i in c(1)) {
#   #pull out an individual script
#   try({
#     episodedf <- data.frame(episodes.all[i, "script"])
#     episode_data <- data.frame(episodes.all[i, 2:10])
#     #run your clean_episode_string() program
#     clean_script <- clean_episode_string(episodedf)
#     
#     #define temp_single_episode with nrow(cleanscript) rows and 10 cols.
#     temp_single_episode <- data.frame(matrix(NA, nrow = nrow(clean_script), ncol = 10))
#     colnames(temp_single_episode) <- c("EpisodeName", "Production", "Airdate", "Series",
#                   "Season", "URL_string", "char", "script_line", 
#                   "stardate", "missiondate")
#     
#     #Fill it with that episodes metadata and cleaned script lines
#     temp_single_episode[,1:6] <- episode_data[1,1:6]
#     temp_single_episode[,7:8] <- clean_script[,]
#     temp_single_episode[,9:10] <- episode_data[1,8:9]
#   })  
#   #rbind each episode w/ metadata to the running large df of all episodes.
#   all_episodes_by_line <- rbind(all_episodes_by_line, temp_single_episode)
# 
# }

