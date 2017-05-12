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

    # value_vec <- if(class(partial_dat[, ith_colname]) == "labelled") {
    #   as_factor(partial_dat[, ith_colname])
    # } else {
    #   partial_dat[, ith_colname]
    # }
     
    data.frame(id = 1L:nrow(osoby),
               value = partial_dat[, ith_colname], 
               rok = substr(ith_colname, 0, 1),
               plec = as_factor(osoby[[6]]), 
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


tmp <- get_data(internet_zawodowo, "internet_zawodowo")

internet_samotnosc <- inner_join(get_data(internet_pytanie, "internet_godz"),
           get_data(samotnosc_pytanie, "samotnosc")) %>% 
  mutate(samotnosc = (samotnosc - 2)*-1) %>% 
  inner_join(get_data(internet_zawodowo, "internet_zawodowo")) %>% 
  mutate(internet_zawodowo = (internet_zawodowo - 2)* -1) %>% 
  droplevels %>% 
  write.csv(file = "internet_samotnosc.csv", row.names = FALSE)

# internet_godz: liczba godzin w internecie w ciagu ostatniego tygodnia
# samotnosc: frakcja osob odczuwajacych samotnosc
# internet_zawodowo: frakcja osob, ktore wykorzystuja internet w celach zawodowych

partial_dat <- data.frame(osoby[, c(internet_zawodowo, samotnosc_pytanie, internet_pytanie)])

full_survs <- (3 == substr(colnames(partial_dat), 0, 1) %>% 
  table %>% 
  as.list %>% 
  unlist) %>% 
  which %>% 
  names 

lapply(full_survs, function(ith_surv) {
  cbind(plec = osoby[6], 
        waga = wagi[, grep(substr(ith_surv, 0, 1), letters) + 1],
        woj =  osoby[[74]],
        region66 = osoby[[77]],
        partial_dat[c(grep(paste0("^", ith_surv), colnames(partial_dat)))]) %>% 
    filter(apply(., 1, function(i) all(!is.na(i)))) %>%
    filter(apply(., 1, function(i) all(i > 0))) %>%
    mutate(plec = as_factor(.[[1]]),
           waga = .[[2]],
           woj = as_factor(.[[3]]),
           region66 = .[[4]],
           internet_zawodowo = as_factor(.[[5]]),
           samotnosc = as_factor(.[[6]]),
           internet_h = as.numeric(as.character(.[[7]]))) %>% 
    filter(internet_zawodowo %in% c("TAK", "NIE", "TAK zaznaczone", "NIE zaznaczone", 
                                    "nigdy", "kiedykolwiek", "w ostatnim tygodniu")) %>% 
    droplevels %>% 
    select(plec, waga, woj, region66, internet_zawodowo, samotnosc, internet_h) %>%
    mutate(rok = factor(ith_surv, levels = letters[1L:8], 
                        labels = c(2000, 2003, 2005, 2007, 2009, 2011, 2013, 2015)),
           internet_zawodowo = forcats::fct_collapse(internet_zawodowo, 
                                                     TAK = c("TAK", "TAK zaznaczone", "kiedykolwiek", 
                                                             "w ostatnim tygodniu"),
                                                     NIE = c("NIE", "NIE zaznaczone")))
}) %>% do.call(rbind, .) %>% 
  group_by(plec, internet_zawodowo, samotnosc) %>% 
  summarise(internet_h = sum(waga*internet_h)/sum(waga), n = length(waga)) %>% 
  ungroup %>% 
  write.csv(file = "internet_samotnosc_aggregated.csv", row.names = FALSE)
  




ggplot(dat, aes(x = plec, y = internet_h, fill = samotnosc)) +
  geom_col(position = "dodge") +
  facet_wrap( ~ internet_zawodowo, labeller = label_both) +
  scale_x_discrete("Płeć") +
  scale_y_continuous("Użycie internetu w ostatnim tygodniu [h]") +
  scale_fill_discrete("Uczucie osamotnienia") +
  theme_bw()
