# trekvis
Looking into scraping web data to make totally unnecessary visualizations surrounding the best things in life.

## Data Collection /  Cleaning
A very thorough stranger has a collection of every single Star Trek script, for all series, hosted on her website [Example URL](http://www.chakoteya.net/StarTrek/1.htm). We manually created a table of each individual episode, detailing the URL where its' script was hosted, series, episode name, airdate, and other metadata.  

TODO - TABLE from gsheets.  Not in my sheets, maybe Katherine has it?

Then we looped through that table, and scraped each episode's script text into a large text blob.  This was cleaned (imperfectly) through the magic of regex (Never use regex on HTML.  Never use regex on HTML.  Never use regex on HTML) to smooth out some bumps.  Next, each script text blob was further sanitized and forced into a new format with each line's speaker, and the text from that line on a new row.  This should let any interested parties do some fun data visualization and text mining discoveries based around the speaking character.

