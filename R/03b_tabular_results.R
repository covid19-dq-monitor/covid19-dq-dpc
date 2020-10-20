fails_today <- filter(summary(results_delta), fails > 0) %>%
  select(name) %>% 
  mutate(date = as.Date(today))

vec_len <- rep(21, times=length(fails_today$name))
vec_key <- rep(fails_today$name, times=vec_len)
x <- values(results_delta)[[1]][,fails_today$name] %>% 
  as.data.frame() %>% 
  gather() %>% 
  mutate(date = as.Date(today),
         region = rep(delta_today$denominazione_regione,
                      times=length(fails_today$name)),
         key = vec_key
         ) %>% 
  filter(value == FALSE) %>% 
  select(date, failed_check=key, region) %>%
  readr::write.csv("output/failed_checks.csv")
