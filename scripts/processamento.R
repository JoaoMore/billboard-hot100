source('libraries.R')

load('billboard.songfeatures.RData') # caracteristicas de cada musica de cada artista
load('billboard.RData') # lista de todos os artistas que apareceram no hot 100
load('popularidade.RData') # popularidade e numero de seguidores de cada artista
load('art50.RData') # quantidade de vezes que cada artista aparece, independente da musica
load('billboard_sf.RData') # dataset com as variaveis de interesse

# criando um novo dataset com a semana que cada artista apareceu ----------

date <- rep(seq(as.Date('1958-08-04'), as.Date('2019-08-10'), by = 'weeks'), each = 100)

billboard.date <- billboard %>% 
  select(artist_name = artist, track_name = songs) %>% 
  mutate(date = date) %>% #data da semana em questao
  mutate(mes = month(date), top100 = 1) #mes, ano e se ficou ou nao no top 100


# novo dataset de caracteristicas com as variaveis de interesse ------------------

(billboard.sf <- billboard.songfeatures %>% 
   select(artist_name,album_name, track_name,album_release_date,
          album_release_year, time_signature, duration_ms, explicit,
          danceability, energy, key,key_name, key_mode, loudness,
          mode, mode_name, speechiness,
          acousticness, instrumentalness, liveness, valence, tempo) %>% 
   mutate(explicit = ifelse(explicit, 1, 0),
          album_release_date = as.Date.character(album_release_date)) %>% 
   mutate(month = month(album_release_date)) %>% 
   mutate(month = factor(month, labels = month.abb)))


billboard.sf <- left_join(billboard.sf, popularidade)



left_join(billboard.sf, art50, by = c('artist_name' = "artist"))

billboard$top50 <- 'nao'
billboard$top50[billboard$artist %in% art50$artist] <- 'sim'

billboard.sf <- billboard %>% 
  filter(top50 == 'sim') %>% 
  left_join(billboard.sf, .) %>% 
  distinct()

billboard %>% 
  group_by(artist, songs) %>% 
  summarise(vezes=n()) %>% 
  drop_na() %>% 
  filter(artist == 'Elton John')

billboard.sf <- billboard %>% 
  group_by(artist, songs) %>% 
  summarise(vezes=n()) %>% 
  drop_na() %>% 
  left_join(billboard.sf, ., by = c('artist_name' = "artist", 'track_name' = 'songs'))

billboard.sf <- 
  billboard.sf %>% 
  mutate(top50 = ifelse(is.na(top50), 'nao', top50),
         vezes = ifelse(is.na(vezes), 0, vezes))

billboard.sf$top50 <- NULL
billboard.sf$ano <- NULL

# transformando variáveis -------------------------------------------------

#billboard.sf$album_release_year <- as.factor(billboard.sf$album_release_year)
billboard.sf$time_signature <- as.factor(billboard.sf$time_signature)
billboard.sf$explicit <- as.factor(billboard.sf$explicit)
billboard.sf$key <- as.factor(billboard.sf$key)
billboard.sf$mode <- as.factor(billboard.sf$mode)
billboard.sf$minutos <- billboard.sf$duration_ms/60000
billboard.sf$duration_ms <- NULL

# criando uma nova coluna com o menor ano que uma música apareceu  --------

billboard.sf <- billboard %>% 
  group_by(artist, songs) %>% 
  summarise(ano_minimo = min(ano)) %>% 
  ungroup() %>% 
  right_join(., billboard.sf, by = c('artist' = 'artist_name', 'songs' = 'track_name'))
