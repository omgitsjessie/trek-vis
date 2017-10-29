#collect data
library(rvest)

#ex webage: http://www.chakoteya.net/NextGen/101.htm

#Return an XML document of this episode's site
url_101 <- "http://www.chakoteya.net/NextGen/101.htm"
html_101 <- read_html(url_101)

xml_find_all(x = html_101, xpath = "//tr")

#extract contents with css selector
td_element <- html_nodes(html_101, css = "td")

#Extract text from the body
bodytext <- html_text(td_element)

#Print the script
bodytext
