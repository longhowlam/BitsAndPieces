library(rvest)
library(purrr)
library(stringr)
library(ggplot2)

### Linkedin update https://www.linkedin.com/feed/update/urn:li:activity:6328957185808232448/

cleanstring <- function(x){
  str_replace_all(x,"\r","") %>%
    str_replace_all("\t","") %>%
    str_replace_all("\n","")
}

### Scrape Media Markt laptops

scraper = function(i)
{
  MM = paste0(
    "http://www.mediamarkt.nl/nl/category/_laptops-482723.html?searchParams=%2FSearch.ff%3FfilterTabbedCategory%3Donlineshop%26filterCategoriesROOT%3DComputer%25C2%25A7MediaNLnlc482710%26filterCategoriesROOT%252FComputer%25C2%25A7MediaNLnlc482710%3DLaptops%25C2%25A7MediaNLnlc482723%26filteravailability%3D1%26filterLevertijd%3D___Direct%26channel%3Dmmnlnl%26followSearch%3D9800%26disableTabbedCategory%3Dtrue%26navigation%3Dtrue&sort=&view=PRODUCTLIST&page=",
    i
  )

  out = read_html(MM)

  price = html_nodes(out, ".product-wrapper") %>%
    html_nodes(".price-box") %>%
    html_text() %>%
    cleanstring() %>%
    str_extract("\\d+") %>%
    as.numeric()

  brand = html_nodes(out, ".product-wrapper") %>%
    html_nodes("h2") %>%
    html_text() %>%
    cleanstring() %>%
    str_extract('\\w*')

  data.frame(price,brand)
}

index = as.character(1:12)
laptops = purrr::map_df(index, scraper)

ggplot(laptops, aes(x=brand, y = price)) + 
    geom_boxplot(notch = TRUE) +
    ggtitle("Price distributions of laptops on www.mediamarkt.nl")



