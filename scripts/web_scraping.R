source('libraries.R')

#leitura do top 100 de apenas uma semana ----
# url <- 'https://www.billboard.com/charts/hot-100/2019-08-10'
# 
# billboard <- read_html(url)
# 
# songs <- billboard %>% 
#   html_nodes('.text--truncate') %>% 
#   html_text() %>% 
#   .[seq(13,212,2)]
# 
# artist <- billboard %>% 
#   html_nodes('.text--truncate') %>% 
#   html_text() %>% 
#   .[seq(14,212,2)]


#leitura de diversas semanas ----

semanas <- seq(as.Date('1958-08-04'), as.Date('2019-08-10'), by = 'weeks')
urls <- paste('https://www.billboard.com/charts/hot-100/', semanas, sep = '')

final <- NULL

for (i in 1:length(urls)) { 
  
  data <- NULL
  
  billboard <- read_html(urls[i])
  
  songs <- billboard %>% 
    html_nodes('.text--truncate') %>% 
    html_text() %>% 
    .[seq(13,212,2)]
  
  artist <- billboard %>% 
    html_nodes('.text--truncate') %>% 
    html_text() %>% 
    .[seq(14,212,2)]
  
  data <- data.frame(songs, artist)
  
  final <- rbind(final, data)
  
  if(i %% 20 == 0) Sys.sleep(60)

  }

rep(year(semanas), each = 100)  %>% length()

billboard <- data.frame(billboard, ano = rep(year(semanas), each = 100))
