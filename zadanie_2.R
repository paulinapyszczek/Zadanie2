rm(list = ls())

library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(xml2)
library(rvest)

remDr <- remoteDr(remoteServerAddr = 'http://localhost',
                  port = 4444,
                  browserName = "chrome",
                  newSession = TRUE)

remDr %>% go('http://otodom.pl')

remDr <- remoteDr(remoteServerAddr = 'http://localhost',
                  port = 4444,
                  browserName = "chrome",
                  newSession = TRUE)

remDr %>% go('https://www.otodom.pl/wynajem/mieszkanie/warszawa/?search%5Bfilter_enum_rooms_num%5D%5B0%5D=2&search%5Bregion_id%5D=7&search%5Bsubregion_id%5D=197&search%5Bcity_id%5D=26&page=1')

remDr <- remoteDr(remoteServerAddr = 'http://localhost',
                  port = 4444,
                  browserName = "chrome",
                  newSession = TRUE)


wektorLinkow <- c()

# pobranie ogloszen z 10 stron - 2-pokojowe mieszkania na wynajem w Warszawie:

for (i in 1:10) {
  
  newUrl <- paste0('https://www.otodom.pl/wynajem/mieszkanie/warszawa/?search%5Bfilter_enum_rooms_num%5D%5B0%5D=2&search%5Bregion_id%5D=7&search%5Bsubregion_id%5D=197&search%5Bcity_id%5D=26&page=', i)
  remDr %>% go(newUrl)
  
  elems <- remDr %>% findElements(using = "tag name", 'h3')
  
  for(j in 1:length(elems)) {
    
    e <- findElementsFromElement(elems[[j]],
                                 using = 'tag name', 'a')
    if(length(e) > 0) {
      link <- e[[1]] %>% getElementAttribute('href')
      wektorLinkow <- c(wektorLinkow, link)
    }
  }
  
}

wektorLinkowU <- wektorLinkow %>% unique()
length(wektorLinkowU)


zrobWiersz <- function(w, wektorLinkow, remDr) {
  
  # w = 1 
  
  remDr %>% go(wektorLinkowU[w])
  
  opis <- NA
  opis <- remDr %>% findElement("class name", "css-46s0sq") %>% getElementText()
  
  cena <- NA
  cena <- remDr %>% findElement("class name", "css-srd1q3") %>% getElementText()
  
  
  szczegoly <- remDr %>% findElements("class name", 'css-1d9dws4')
  
  listaSzczegolowOpis <- c()
  listaSzczegolowWartosci <- c()
  
  for (i in 1:length(szczegoly)) {
    
    listaSzczegolowOpis <- c(listaSzczegolowOpis,szczegoly[[i]] %>% findElementsFromElement("class name", 'css-o4i8bk'))
    listaSzczegolowWartosci <- c(listaSzczegolowWartosci,szczegoly[[i]] %>% findElementsFromElement("class name", 'css-1ytkscc'))
    
  }
  
  nazwyKolumn <- lapply(listaSzczegolowOpis, getElementText) %>% str_replace_all(':', '') %>% unlist()
  wartosci <- lapply(listaSzczegolowWartosci, getElementText) %>% unlist()
  df1 <- data.frame(matrix(wartosci, nrow = 1, ncol = length(wartosci)))
  names(df1) <- nazwyKolumn
  df1 <- cbind(opis, cena, df1)
  
}

mieszkania <- NULL

# pobranie danych:

for (w in 1:length(wektorLinkowU)) {
  
  skip <- FALSE
  
  tryCatch(
    df1 <- zrobWiersz(w, wektorLinkowU, remDr = remDr), 
    error = function(e) {skip <<- TRUE}
  )
  
  if(skip){next}
  
  if(is.null(mieszkania)){
    
    mieszkania <- df1
    
  } else {
    
    
    mieszkania <- smartbind(mieszkania, df1)
    
  }
}

head(mieszkania)



