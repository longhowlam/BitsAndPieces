###############################################################################
##
##  visualise songs of  different playlists with t-sne

#### libraries needed ####
library(httr)
library(purrr)
library(furrr)
library(dplyr)
library(Rtsne)
library(reticulate)
library(plotly)

plan(multiprocess)
#### python environment with librosa module installed
use_python(python = "/usr/bin/python3")

#### spotify credientials
#clientID = "123456789"
#secret = "ABCDEFGHIJ"
clientID = readRDS("clientID.RDs")
secret = readRDS("secret.RDs")

###### Spotify api helper functions ###########################################

## given a clientID and a secret we can retrieve a token that is 
## needed for further Spotify API calls

GetSpotifyToken = function(clientID, secret){
  
  response <- POST(
    'https://accounts.spotify.com/api/token',
    accept_json(),
    authenticate(clientID, secret),
    body=list(grant_type='client_credentials'),
    encode='form',
    verbose()
  )
  
  if (status_code(response) == 200){
    return(content(response)$access_token)
  }
  else{
    return("")
  }
}

#### given an userid and a playlistID we extract the tracks from this specific playlist
#### and put some of the track info in a tibble

ExtractTracksFromPlaylist = function(offset = 0, ownerID, playlistID, clientID, secret, mylabel = ""){ 
  ### get the playlist itself
  URI = paste0(
    "https://api.spotify.com/v1/users/", 
    ownerID,
    "/playlists/", 
    playlistID,
    "/tracks",
    "?offset=",
    offset
  )
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  r2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  tracks = content(r2)
  
  ## put track info in a data set, we need to extract it from nested lists
  tibble::tibble(
    label       = mylabel,
    artist      = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["artists"]][[1]][["name"]]),
    song        = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["name"]]),
    preview_url = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["preview_url"]] %||% ""),
    image       = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["album"]][["images"]][[1]][["url"]]),
    duration    = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["duration_ms"]]),
    trackid     = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["id"]])
  )
}

######## get different of songs from playlists ############################################

########  Bach songs #########################
ownerID = "spotify"
playlistID = "37i9dQZF1DWZnzwzLBft6A"

BACH_tracks = ExtractTracksFromPlaylist(
  offset = 0, 
  ownerID = ownerID,  
  playlistID = playlistID,
  clientID, 
  secret, 
  mylabel = "BACH"
)

## ignore the songs without preview URL
BACH_tracks = BACH_tracks %>% filter(preview_url != "")

######### Heavy metal songs ##################
ownerID = "spotify"
playlistID = "37i9dQZF1DX9qNs32fujYe"

HEAVYMETAL_tracks = ExtractTracksFromPlaylist(
  offset = 0, 
  ownerID = ownerID,  
  playlistID = playlistID,
  clientID, 
  secret, 
  mylabel = "HEAVY METAL"
)

## ignore the songs without preview URL
HEAVYMETAL_tracks = HEAVYMETAL_tracks %>% filter(preview_url != "")

######### Michael Jackson songs ############
ownerID = "spotify"
playlistID = "37i9dQZF1DXaTIN6XNquoW"

spotify:user:wim_spaargaren:playlist:60S81N6h2E4A4mWzLGoM39

MJ_tracks = ExtractTracksFromPlaylist(
  offset = 0, 
  ownerID = ownerID,  
  playlistID = playlistID,
  clientID, 
  secret, 
  mylabel = "JACKSON"
)

## ignore the songs without preview URL
MJ_tracks = MJ_tracks %>% filter(preview_url != "")


########## bach violin songs ##################
ownerID = "adrientsuzuki"
playlistID = "6sGxlZaDfoTTQ4FCimYgl7"

VIOLIN_tracks = ExtractTracksFromPlaylist(
  offset = 0, 
  ownerID = ownerID,  
  playlistID = playlistID,
  clientID, 
  secret, 
  mylabel = "VIOLIN"
)

## ignore the songs without preview URL
VIOLIN_tracks = VIOLIN_tracks %>% filter(preview_url != "")


##### stack all songs in one data frame
##### and download the mp3's into a directory called mp3songs 

AllSongs = bind_rows(MJ_tracks, HEAVYMETAL_tracks, BACH_tracks, VIOLIN_tracks)

for(i in seq_along(AllSongs$preview_url))
{
  download.file(
    AllSongs$preview_url[i], 
    destfile = paste0("mp3songs/", AllSongs$trackid[i]),
    mode="wb" 
  )
}

########  Calculate mel spectogram #################################################
## using python librosa pacakge (via the reticulate package)
## all downloaded mp3's are put trhough librosa
use_condaenv("my_py36")
librosa = import("librosa")
ff = librosa$feature

## import one mp3 file
#mp3s = list.files("mp3songs/")
#onemp3 = librosa$load( paste0("mp3songs/", mp3s[1]))

#length(onemp3[[1]])
#length(onemp3[[1]])/onemp3[[2]]  # ~30 seconds sound

## 5 seconds plot
#pp = 5*onemp3[[2]]
#plot(onemp3[[1]][1:pp], type="l")


#### helper function around librosa call ####
mfcc = function(file, dir, .pb = NULL)
{
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  pathfile = paste0(dir, "/", file)
  mp3 = librosa$load(pathfile)
  
  # calc  mel to file
  ff$melspectrogram(
      mp3[[1]], 
      sr = mp3[[2]],
      n_mels=96)
}

#mp3 = librosa$load("mp3songs/031j0imoJX53yvsTnhghpl")

mp3s = list.files("mp3songs/")
pb = progress_estimated(length(mp3s))

### now using purrr::map we can calculate mfcc for each mp3 in the folder mp3songs
#t0 = proc.time()
#AllSongsMFCC = purrr::map(mp3s, mfcc, dir = "mp3songs", .pb = pb)
#t1 = proc.time()
#t1-t0

t0 = proc.time()
AllSongsMFCC = furrr::future_map(mp3s, mfcc, dir = "mp3songs", .pb = pb)
t1 = proc.time()
t1-t0

## create a plot of the mfcc matrix
rotate <- function(x) t(apply(x, 2, rev))
image(rotate(rotate(rotate(AllSongsMFCC[[17]]))))

## create a feature matrix. Simply flatten the matrix
## each song is now a row of 13*1292 values

nsongs = dim(AllSongs)[1]
AllSongsMFCCMatrix = matrix(NA , nrow = nsongs, ncol=96*1292)
for(i in 1:nsongs){
  AllSongsMFCCMatrix[i,] = as.numeric(AllSongsMFCC[[i]])
}


#### UMAP and plotly ####################################################

umap = import("umap")

embedding = umap$UMAP(
  n_neighbors = 5L,
  n_components = 3L,
  min_dist = 0.1,
  metric='euclidean'
)

## compute UMAP with 3 components
embedding_out = embedding$fit_transform(AllSongsMFCCMatrix)



##### Plot the embeddings with plotly #########################################

plotdata = data.frame(embedding_out)
plotdata$trackid = mp3s
plotdata = plotdata %>% left_join(AllSongs)
plot_ly(
  plotdata, 
  x = ~X1,
  y = ~X2, 
  z = ~X3,
  color=~label,
  text = ~paste(artist, "<br>", song),
  size = 1, sizes = c(1,4) 
) %>% 
  layout(title = '3D umap of artists')

#### tsne and plotly ###################################################

### apply T-sne on the songs to reduce to 3 dimensions
tsne_out = Rtsne(AllSongsMFCCMatrix, dims=3) 

### transform result to a data frame and match original songs data frame
reduced = tsne_out$Y %>%
  as_data_frame() 
reduced$trackid = mp3s
  
reduced = reduced %>% left_join(AllSongs)

#######  Create 3D plotly plot of reduced t-sne dimensions 
plot_ly(
  reduced,
  x = ~V1, y = ~V2, z = ~V3,
  color=~label,
  text = ~paste(artist, "<br>", song)
) %>%
  add_markers() %>%
  layout(title="3D t-sne on spotify mp3 samples")













ownerID = "wim_spaargaren"
playlistID = "60S81N6h2E4A4mWzLGoM39"

spotify:user:wim_spaargaren:playlist:60S81N6h2E4A4mWzLGoM39

t2000_tracks2 = ExtractTracksFromPlaylist(
  offset = 100, 
  ownerID = ownerID,  
  playlistID = playlistID,
  clientID, 
  secret, 
  mylabel = "JACKSON"
)
