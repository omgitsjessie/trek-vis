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
ggplot(episodes.all, aes(x=stardate, fill = Series)) + 
  geom_histogram() + 
  ggtitle("Stardate histogram")

#percent of episodes with a given stardate
episodes.all$stardate %>% 
  is.na() %>%
  mean()


#Create a smaller data.frame from the FIRST episode, containing a row for each
#line of dialogue.  
testdf <- data.frame(episodes.all[1, "script"])
names(testdf) <- "script"
testdf$script <- gsub("\\{", "\\[", testdf$script) #replace curly braces with braces
testdf$script <- gsub("\\}", "\\]", testdf$script) #replace curly braces with braces
testdf$script <- gsub("\\[OC\\]", "", testdf$script) #clean out com mentions
#testdf$script <- gsub("\\[.*\\]", "", testdf$script) #clean out blocking notes altogether


#Backreference each character name (full words separated by a space, then :)
#Then add a ~ around each one.  Then split my string to new rows based on that ~ char.
testdf.split <- strsplit( gsub("([A-Z]* *[A-Z]* *:)","~\\1", testdf$script), "~" )
testdf.lines3 <- testdf.split %>% data.frame() 
names(testdf.lines3) <- "lines"

testdf.lines4 <- testdf.lines3
#Now split each line into Char | Line
testdf.lines4$char <- testdf.lines3$lines
testdf.lines4$line <- testdf.lines3$lines

testdf.lines4$char <- gsub("([A-Z]* *[A-Z]*)( *:.*)","\\1", testdf.lines4$char)
testdf.lines4$line <- gsub("(.*:)(.*)","\\2", testdf.lines4$line)

#Does not capture:
# lines with 'AAAA [blocking]:' as the speaker..
            # some lines beginning with 'AAAAA [OC]:' presumably over intercom? - FIXED
            # character names that are two words 'OLD MAN:' - Fixed
            # character names with an extra space between the character and the :   'PIKE :' - FIXED
#Preserves but probably should not:
# bracket-notated location text: [Bridge], [Transporter room] etc
# parenthetical notes for background activity or visuals: (Boyce enters with bag) etc
  

