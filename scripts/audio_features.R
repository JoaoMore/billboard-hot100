source('libraries.R')

# carregando datasets -----------------------------------------------------

load('billboard.songfeatures.RData') # caracteristicas de cada musica de cada artista
load('billboard.RData') # lista de todos os artistas que apareceram no hot 100
load('popularidade.RData') # popularidade e numero de seguidores de cada artista
load('art50.RData') # quantidade de vezes que cada artista aparece, independente da musica

# credenciais para usar a api do spotify -----


# artistas que aparecem ao menos 50 vezes ---------------------------------

art50 <- billboard %>% 
  drop_na() %>% 
  group_by(artist) %>% 
  count() %>% 
  arrange(-n) %>% 
  filter(n>49)


# buscando as musicas dos artistas que aparecem pelo menos 50 vezes --------

songlist <- list()

start.time <- Sys.time()

for (i in 1:dim(art50[1])) {
  
  print(paste(as.character(art50$artist[i]), i))
  
  not.available <- tryCatch(get_artist_audio_features(as.character(art50$artist[i])),
                            error = function(e) e)
  #pular indices com erros
  
  if(inherits(not.available, 'error')) next
  
  songlist[[i]] <- get_artist_audio_features(as.character(art50$artist[i]))
  
  save(songlist, file = 'songlist.RData')
  
}

songlist <- songlist[!sapply(songlist,is.null)]

billboard.songfeatures <- bind_rows(songlist)

final.time <- Sys.time()

(total.time <- final.time - start.time) #Time difference of 8.834967 hours


# id's dos artistas (usaremos para pegar o numero de seguidores e  --------

id.artist <- billboard.songfeatures %>% 
  group_by(artist_id) %>% 
  count() %>% 
  arrange(-n)

ids <- list()

for (i in 1:length(id.artist$artist_id)) {
  print(i)
  ids[[i]] <- get_artist(id.artist$artist_id[i])
  save(ids, file = 'ids.RData')
}

#save(ids, file = 'artist.Info.RData')

# popularidade e número de seguidores de cada artista ---------------------

popularidade <- list()

for (i in 1:length(ids)) {
  popularidade[[i]] <- data.frame(artist_name = ids[[i]]$name, 
                                  popularity = ids[[i]]$popularity, 
                                  followers = ids[[i]]$followers$total) 
}

popularidade <- bind_rows(popularidade)
