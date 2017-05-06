library(haven)
library(dplyr)


# variable-weight: http://diagnoza.com/bazy_czytaj.html

load("./Diagnoza_dane-master/osoby.RData")
load("./Diagnoza_dane-master/osobyDict.RData")

load("data/ksztalt_wojewodztw_data_frame.Rdata")
wojewodztwa_nazwy_kody <- mutate(wojewodztwa_nazwy_kody,
                                 woj = iconv(woj))

wagi <- osoby[, 29L:36]
colnames(wagi) <- paste0(letters[1L:8], "_", colnames(wagi))
wagi <- cbind(id = 1L:nrow(osoby), wagi)

internet_pytanie <- grep(pattern = ".*godzin w ostatnim tygodniu korzyst.*", x = tolower(osobyDict$description)) + 1
samotnosc_pytanie <- grep(pattern = "osamotnion", x = tolower(osobyDict$description)) + 1
internet_zawodowo <- setdiff(grep(pattern = "w celach zawodowych", x = tolower(osobyDict$description)),
                                                   grep(pattern = "kiedykolwiek", x = tolower(osobyDict$description))) + 1




get_data <- function(column_ids, column_name){
  partial_dat <- data.frame(osoby[, column_ids])

  res <- do.call(rbind, lapply(colnames(partial_dat), function(ith_colname) {
    data.frame(id = 1L:nrow(osoby),
               value = partial_dat[, ith_colname], 
               rok = substr(ith_colname, 0, 1),
               plec = osoby[[6]], 
               weight = wagi[, grep(substr(ith_colname, 0, 1), letters) + 1],
               woj =  osoby[[74]],
               region66 = osoby[[77]])
  })) %>% 
    filter(!is.na(value)) %>% 
    filter(value > 0) %>% # aby uniknac wartosci typu -9 itp
    mutate(rok = factor(rok, levels = letters[1L:8], 
                         labels = c(2000, 2003, 2005, 2007, 2009, 2011, 2013, 2015))) %>% 
    group_by(rok, woj, plec) %>% 
    summarise(value = sum(weight*value)/sum(weight))
  
  colnames(res)[4] <- column_name
  res
}



internet_samotnosc <- inner_join(get_data(internet_pytanie, "internet_godz"),
           get_data(samotnosc_pytanie, "samotnosc")) %>% 
  mutate(samotnosc = (samotnosc - 2)*-1) %>% 
  inner_join(get_data(internet_zawodowo, "internet_zawodowo")) %>% 
  mutate(internet_zawodowo = (internet_zawodowo - 2)* -1) %>% 
  droplevels

# internet_godz: liczba godzin w internecie w ciagu ostatniego tygodnia
# samotnosc: frakcja osob odczuwajacych samotnosc
# internet_zawodowo: frakcja osob, ktore wykorzystuja internet w celach zawodowych