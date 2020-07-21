library(pdftools)
library(stringr)
library(dplyr)
library(ggplot2)

## download the data here:
# https://www.uwv.nl/overuwv/Images/register-now-100720.pdf

### import PDF met NOW uitbetalingen
text = pdf_text("register-now-100720.pdf")

### elke pagina van de pdf is een element in de vector text
### de data begint op pagina 3 t/m 2047

### het is wat gepunnik, maar elke pagina bevat strings die we kunnen
### splitsen op meerdere spaties om zo naam, plaats en bedrag te krijgen

for( j in 3:2047){
  
  if (j%% 25 == 0) {print(j)}
  
  zz = text[j]
  pp = str_split(zz, "\n")[[1]]
  n = length(pp)
  xx = pp[3:(n-2)]
  yy = str_split(xx, "  ")

  nl = length(yy)
  for( i in 1:nl){
    ### haal per regel de teveel gesplitste lege strings weg
    tmp = yy[[i]][yy[[i]] != ""]
    if (length(tmp) > 2){
      out = rbind(out, tmp)
    }
  }
}

### maak er een tibble van
now_data = tibble::as_tibble(out)

### en fatsoeneer variablen
now_data = now_data %>% 
  filter(V1 != "") %>% 
  mutate(
    bedrijf = str_trim(V1),
    plaats = str_trim(V2),
    bb = str_remove_all(V3, "\\."),
    bedrag = as.double(bb)
  ) %>% 
  select(bedrijf, plaats, bedrag)

### bewaar als RDS
saveRDS(now_data, "now_data.RDS")

##### wat plaatjes

ggplot(now_data, aes(x=bedrag)) + 
  geom_histogram(col="black") +
  scale_x_log10() +
  ggtitle("NOW regeling betaald bedrag")
