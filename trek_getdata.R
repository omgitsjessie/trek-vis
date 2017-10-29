#collect data
library(rvest)
library(googlesheets)

#function to pull the script from a given URL:
beam_me_in <- function(url) {
  url_string <- url
  html_readin <- read_html(url_string) #Return an XML doc of the episode's site
  xml_find_all(x=html_readin, xpath = "//tr")  
  td_element <- html_nodes(html_readin, css = "td") #extract contents w/ css selector
  script_text <- html_text(td_element)  #extract text from the body
  return(script_text)  #return the script for that URL's episode.  Unformatted.
}

#test site
#beam_me_in("http://www.chakoteya.net/NextGen/101.htm")

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


#Run your function on every episode URL in that list, appended to a new table.
#beam_me_in(episodes.all[593, "Link"]) %>% nchar()

#Initialize blank variable where script will go
episodes.all[, "script"] <- "blank"

#populate the "script" var in episodes with the actual script!
for (i in 1:nrow(episodes.all)) {
  #Pull out that episode's script
  url <- episodes.all[i, "Link"]
  episode_script <- beam_me_in(url)
  episodes.all[i, "script"] <- episode_script
}
#Same episodes (split in two "to be continued" halfway down):
#492 and 493
#517 and 518
#566 and 567
#592 and 594

#Write out episodes.all to csv so you don't have to keep scraping a ton of urls
#to reform the data frame.
write.csv(episodes.all, file = "all_startrek_episode_scripts.csv")


#Manually separate out the "to be continued" episodes?

