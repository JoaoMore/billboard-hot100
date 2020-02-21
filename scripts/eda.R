source('libraries.R')

load('billboard_sf.RData')
load('originais.RData')

summary(billboard.sf)

#matriz de correlacao 
originais  %>% 
  select_if(is.numeric) %>% 
  select(-c(minimo, minutos)) %>% 
  cor(use = 'complete.obs') %>% 
  corrplot(tl.cex = .7, 
           order = 'hclust',
           hclust.method = 'ward.D2',
           method = 'color',
           tl.col = 'black')

ggplot(originais, aes(popularity, (followers))) +
  geom_point(alpha = 0.5, color = 'dodgerblue4') +
  labs(x = 'Popularidade', y = 'Seguidores', title = 'Popularidade x N° de Seguidores')

ggplot(originais, aes(popularity, log(followers))) +
  geom_point(alpha = 0.5, color = 'dodgerblue4') +
  labs(x = 'Popularidade', y = 'Seguidores', title = 'Popularidade x N° de Seguidores (log)')

ggplot(originais, aes(acousticness, energy)) +
  geom_point(alpha = 0.5, color = 'dodgerblue4') +
  labs(x = 'Acousticness', y = 'Energy', title = 'Acousticness x Energy')

ggplot(originais, aes(acousticness, loudness)) +
  geom_point(alpha = 0.5, color = 'dodgerblue4') +
  labs(x = 'Acouscticness', y = 'Loudness', title = 'Acousticness x Loudness')

# esse vai
ggplot(originais, aes(energy, loudness)) +
  geom_point(alpha = 0.5, color = 'dodgerblue4') +
  labs(x = 'Energy', y = 'Loudness', title = 'Energy x Loudness')

originais %>% 
  filter(top100 == 'sim') %>% 
  ggplot(., aes(danceability, valence)) +
  geom_point(alpha = 0.5, color = 'dodgerblue4') +
  labs(x = 'Danceability', y = 'Valence', title = 'Danceability x Valence')


originais %>% 
  group_by(key_mode) %>% 
  count() %>% 
  head(10) %>% 
  ggplot(., aes(reorder(key_mode, -n), n)) +
  geom_col(fill = 'dodgerblue4', na.rm = TRUE) +
  labs(x = NULL, y = NULL, title = '10 tonalidades mais frequentes')


ggplot(originais, aes(fct_infreq(key_mode))) +
  geom_bar(fill = 'dodgerblue4', na.rm = TRUE) +
  labs(x = NULL, y = NULL, title = 'Tonalidade mais frequente')

# a tonalidade está associada as características numéricas?

originais %>% 
  group_by(key_mode) %>% 
  summarise_if(is.numeric, mean) %>%
  select(-c(minutos, vezes,followers, data, popularity, minimo)) %>% 
  drop_na()


#número de na's
sapply(billboard.sf, function(x) sum(is.na(x)))

# comparação das músicas que chegaram no top 100 e não chegaram -----------

# criando novo dataset somente com musicas originais (dispensando remakes, versões mais longas, etc)

originais <- billboard.sf %>% 
  arrange(data) %>% 
  distinct_at(.vars = vars(artist, track), .keep_all = TRUE)

#tempo de duracao
ggplot(billboard.sf, aes(top100, minutos)) +
  geom_boxplot(outlier.shape = 21)

#acousticness

ggplot(billboard.sf, aes(top100, acousticness)) +
  geom_boxplot(outlier.shape = 21)

#energy

ggplot(billboard.sf, aes(top100, energy)) +
  geom_boxplot(outlier.shape = 21)

#loudness

ggplot(billboard.sf, aes(top100, loudness)) +
  geom_boxplot(outlier.shape = 21)



#musica que chegaram no hot 100 com o maior tempo de duração
originais %>% 
  filter(top100 == 'sim') %>% 
  select(track,artist,album_name, minutos) %>% 
  arrange(-minutos) %>% 
  head(11)


#artistas com média de tempo maior
billboard.sf %>% 
  filter(top100 == 'sim') %>% 
  group_by(artist) %>% 
  summarise(media_tempo = mean(minutos)) %>% 
  arrange(-media_tempo) %>% 
  headTail()

# música mais "acústica"
originais %>% 
  select(track, artist, acousticness) %>% 
  arrange(-acousticness) %>% 
  headTail()

#música mais dançante

originais %>% 
  select(track, artist, danceability) %>% 
  arrange(-danceability) %>% 
  headTail()

# maior valencia
originais %>% 
  select(track, artist, valence) %>% 
  arrange(-valence) %>% 
  headTail()

# número de vezes que uma musica ficou no top 100
originais %>% 
  select(track, artist, vezes) %>% 
  arrange(-vezes) %>% 
  head(10)


# distribuiçao do numero de vezes

originais %>% 
  filter(top100 == 'sim') %>% 
  ggplot(., aes(vezes)) +
  geom_bar(fill = 'dodgerblue4') +
  labs(y = NULL, title = 'Distribuição do número de aparições no Hot 100',
       x = 'Número de vezes')

# matriz de correlação das músicas originais

originais %>% 
  select_if(is.numeric) %>% 
  select(-c(minimo,data,vezes)) %>% 
  cor(use = 'complete.obs') %>% 
  corrplot(tl.cex = .7, 
           order = 'hclust',
           hclust.method = 'ward.D2',
           method = 'color',
           tl.col = 'black')


# musicas explícitas

originais %>%
  select(explicit) %>% 
  na.omit() %>% 
  group_by(explicit) %>% 
  count() %>% 
  ggplot(., aes(explicit, n)) +
  geom_col(fill = 'dodgerblue4')


# comportamento das médias das características ao longo dos anos ----------

#danceability
originais %>%
  filter(top100 == 'sim') %>% 
  group_by(data) %>%
  summarise_if(is.numeric, mean) %>% 
  select(-c(minutos, vezes,followers, popularity, minimo)) %>% 
  filter(data >= 1900) %>% 
  ggplot(., aes(data, danceability)) +
  geom_point()


#valence
originais %>%
  filter(top100 == 'sim') %>% 
  group_by(data) %>%
  summarise_if(is.numeric, mean) %>% 
  select(-c(minutos, vezes,followers, popularity, minimo)) %>% 
  filter(data >= 1900) %>% 
  ggplot(., aes(data, valence)) +
  geom_point()

#loudness
originais %>%
  filter(top100 == 'sim') %>% 
  group_by(data) %>%
  summarise_if(is.numeric, mean) %>% 
  select(-c(vezes,followers, popularity, minimo)) %>% 
  filter(data >= 1900) %>% 
  ggplot(., aes(data, loudness)) +
  geom_point()

#tempo médio
originais %>%
  filter(top100 == 'sim') %>% 
  group_by(data) %>%
  summarise_if(is.numeric, mean) %>% 
  select(-c(vezes,followers, popularity, minimo)) %>% 
  filter(data >= 1900) %>% 
  ggplot(., aes(data, minutos)) +
  geom_point()

#energy
originais %>%
  filter(top100 == 'sim') %>% 
  group_by(data) %>%
  summarise_if(is.numeric, mean) %>% 
  select(-c(vezes,followers, popularity, minimo)) %>% 
  filter(data >= 1900) %>% 
  ggplot(., aes(data, energy)) +
  geom_point()

#tempo
originais %>%
  filter(top100 == 'sim') %>% 
  group_by(data) %>%
  summarise_if(is.numeric, mean) %>% 
  select(-c(vezes,followers, popularity, minimo)) %>% 
  filter(data >= 1900) %>% 
  ggplot(., aes(data, tempo)) +
  geom_point()

#speechiness
originais %>%
  filter(top100 == 'sim') %>% 
  group_by(data) %>%
  summarise_if(is.numeric, mean) %>% 
  select(-c(vezes,followers, popularity, minimo)) %>% 
  filter(data >= 1900) %>% 
  ggplot(., aes(data, speechiness)) +
  geom_point()


originais %>% 
  group_by(data, top100) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select(-c(vezes,followers, popularity, minimo)) %>% 
  filter(data >= 1900) %>% 
  ggplot(., aes(data, speechiness, color = top100)) +
  geom_line(alpha = .5, size = 2) +
  geom_point(size = 1) +
  scale_color_manual(values = c('firebrick2','dodgerblue4')) +
  labs(x=NULL) +
  xlim(1960, 2020)
