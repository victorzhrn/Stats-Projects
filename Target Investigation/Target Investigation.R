library(readr)
library(rvest)

url="http://gam.target.com/store-locator/state-listing?lnk=findstore_viewall_storesbystate"

location_page <- read_html(url)

income_df = read.csv()