library(tidyverse)
library(rvest)
library(stringr)

baseurl <- 'https://www.coachella.com/lineup/'
base <- read_html(baseurl)

node <- base %>% 
    xml_node('#main') %>% 
    xml_node('lineup-listing') %>% 
    as.character

coachella <- regmatches(node, gregexpr("(?=\\{).*?(?<=\\})", node, perl=T))[[1]]
str(coachella)

##### weekend data not used in OG post, but i kept here for those interested
weekend_1 <- map_df(1:nrow(coachella_df), function(x) {
    if (length(coachella_df$events[x][[1]]) == 1) {
        data.frame()
    } else {
        coachella_df$events[x][[1]]
    }
})

weekend_2 <- map_df(seq(1, length(coachella)), function(x) {
    print(x)
    if (nchar(coachella[x]) > 400) {
        return(data.frame())
    }
    json <- paste0('[', coachella[x], ']')
    fromJSON(json)
})

events <- rbind(weekend_1, weekend_2)

##################

library(jsonlite)

coachella_df <- map_df(seq(1, length(coachella)), function(x) {
    if (nchar(coachella[x]) <= 400) {
        return(data.frame()) ## skip elements with small amounts of text (they're event info for weekend 2)
    }
    json <- paste0('[', coachella[x], '}}]') ## add brackets to complete json format
    fromJSON(json)
})

str(select(coachella_df, -events))

length(coachella_df$events)
str(coachella_df$events[1])

###################
artists <- select(coachella_df, name, id) %>% mutate(name = str_trim(name))
artists$name[artists$name == 'R&amp;ouml;yksopp'] <- 'Röyksopp'
artists$name[artists$name == 'R&amp;oacute;is&amp;iacute;n Murphy'] <- 'Róisín Murphy'
artists$name[artists$name == 'Richie Hawtin Close'] <- 'Richie Hawtin'
artists$name[artists$name == 'Josh Billings and Nonfiction'] <- 'Josh Billings'
artists$name[artists$name == 'Porter Robinson &amp; Madeon'] <- 'Porter Robinson'
artists$name[artists$name == 'Gaslamp Killer'] <- 'The Gaslamp Killer'
artists$name[artists$name == 'HAANA'] <- 'HÄANA'
artists$name[artists$name == 'Toots and the Maytals'] <- 'Toots & The Maytals'
artists$name[grepl('&amp;', artists$name)] <- gsub('&amp;', '&', artists$name[grepl('&amp;', artists$name)])

artists <- rbind(artists, tibble(name = c('Nonfiction', 'Madeon'), id = c(NA, NA)))

head(artists)

#################
get_spotify_artist_uri <- function(artist_name) {
    
    # Search Spotify API for artist name
    res <- GET('https://api.spotify.com/v1/search', query = list(q = artist_name, type = 'artist')) %>%
        content %>% .$artists %>% .$items
    
    # Clean response and combine all returned artists into a dataframe
    artists <- map_df(seq_len(length(res)), function(x) {
        list(
            artist_name = res[[x]]$name,
            artist_uri = gsub('spotify:artist:', '', res[[x]]$uri) # remove meta info from the uri string
        )
    })
    
    return(artists)
}

library(httr)
artists$artist_uri <- map_chr(artists$name, function(x) {
    res <- get_spotify_artist_uri(x) %>% 
        filter(tolower(artist_name) == tolower(x))
    if (nrow(res) == 0) {
        artist_uri <- NA # not all artists are on Spotify (or at least can't easily be found by name through the API)
    } else {
        artist_uri <- res$artist_uri[1]
    }
    Sys.sleep(.05) # rate limiting
    return(artist_uri)
})

artists$artist_uri[artists$name == 'The Atomics'] <- '7ABVgbel3fSszt6HZN6hew' # Manually correct since it pulled down the wrong "The Atomics"

spotify_artists <- artists %>% 
    filter(!is.na(artist_uri))

head(spotify_artists)

###########################
get_artist_top_tracks <- function(artist_uri) {
    url <- paste0('https://api.spotify.com/v1/artists/', artist_uri, '/top-tracks?country=US')
    res <- GET(url) %>% content %>% .$tracks
    
    if (length(res) == 0) {
        return(data.frame())
    } else {
        
        map_df(1:length(res), function(x) {
            list(
                track_name = res[[x]]$name,
                album_name = res[[x]]$album$name,
                track_uri = res[[x]]$uri   
            )
        }) %>% mutate(track_uri = gsub('spotify:track:', '', track_uri))
    }
}

spotify_tracks <- map_df(1:nrow(spotify_artists), function(x) {
    df <- get_artist_top_tracks(spotify_artists$artist_uri[x]) %>% 
        mutate(artist_name = spotify_artists$name[x])
    Sys.sleep(.05)
    return(df)
})

head(spotify_tracks)

#################3
dudu_tracks <- GET(paste0('https://api.spotify.com/v1/albums/5mG2tE79ApgzOkycjWjrhi/tracks')) %>% 
    content %>% 
    .$items
dudu_track_df <- map_df(1:length(dudu_tracks), function(x) {
    list(
        track_name = dudu_tracks[[x]]$name,
        track_uri = dudu_tracks[[x]]$id
    )
}) %>% 
    mutate(album_name = 'Dudu Tassa & the Kuwaitis', 
           artist_name = 'Dudu Tassa & the Kuwaitis')

spotify_tracks <- rbind(spotify_tracks, dudu_track_df) %>% 
    group_by(artist_name) %>% 
    filter(n() >= 10) %>% 
    ungroup

str(spotify_tracks)

###################
get_track_audio_features <- function(tracks) {
    map_df(1:ceiling(nrow(filter(tracks, !duplicated(track_uri))) / 100), function(x) {
        uris <- tracks %>%
            filter(!duplicated(track_uri)) %>%
            slice(((x * 100) - 99):(x*100)) %>%
            select(track_uri) %>%
            .[[1]] %>%
            paste0(collapse = ',')
        
        res <- GET(paste0('https://api.spotify.com/v1/audio-features/?ids=', uris),
                   query = list(access_token = access_token)) %>% content %>% .$audio_features
        
        df <- unlist(res) %>%
            matrix(nrow = length(res), byrow = T) %>%
            as.data.frame(stringsAsFactors = F)
        names(df) <- names(res[[1]])
        df
    }) %>% select(-c(type, uri, track_href, analysis_url)) %>%
        rename(track_uri = id)
}

## Need credentials
client_id <- 'c857dcec62a74825985e4749ef531abe'
client_secret <- '54af922e8c7a44f28eb339adb0f23656'
access_token <- POST('https://accounts.spotify.com/api/token',
                     accept_json(), authenticate(client_id, client_secret),
                     body = list(grant_type='client_credentials'),
                     encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token

feature_vars <- c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness',
                  'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature')

features <- get_track_audio_features(spotify_tracks) %>% 
    mutate_at(feature_vars, as.numeric)

##################
tracks <- spotify_tracks %>% 
    left_join(features, by = 'track_uri') %>% 
    mutate(artist_name = ifelse(artist_name %in% c('Porter Robinson', 'Madeon'), 'Porter Robinson & Madeon', artist_name),
           artist_name = ifelse(artist_name %in% c('Josh Billings', 'Nonfiction'), 'Josh Billings and Nonfiction', artist_name))

str(tracks)

##############
dist_vars <- c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 
               'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms')

artist_features <- tracks %>%
    group_by(artist_name) %>%
    summarise_at(dist_vars, mean) %>% 
    ungroup 

library(scales)

artists_scaled <- artist_features %>% 
    select(match(dist_vars, names(.))) %>%
    mutate_all(scale)

str(artists_scaled)

###############
library(NbClust)

nbc <- NbClust(artists_scaled, distance = 'manhattan',
               min.nc = 2, max.nc = 10, 
               method = 'ward.D', index ='all') 

clust <- nbc$Best.partition

#####################

pca <- prcomp(artist_features[, dist_vars], scale = T)

summary(pca)


#####################
library(highcharter)
library(RColorBrewer)
library(shiny)

lam <- pca$sdev[1:2]
lam <- lam * sqrt(nrow(pca$x))

hplot <- (pca$x[, 1:2] / lam) %>% 
    as.data.frame %>% 
    mutate(name = artist_features$artist_name,
           cluster = paste0('Cluster #', clust),
           tooltip = name)

dfobs <- (pca$x[, 1:2] / lam) %>% 
    as.data.frame

dfcomp <- pca$rotation[, 1:2] * lam
mx <- max(abs(dfobs[, 1:2]))
mc <- max(abs(dfcomp)) 
dfcomp <- dfcomp %>% 
{ . / mc * mx } %>% 
    as.data.frame() %>% 
    setNames(c("x", "y")) %>% 
    rownames_to_column("name") %>%  
    as_data_frame() %>% 
    group_by_("name") %>%
    do(data = list(c(0, 0), c(.$x, .$y))) %>%
    list_parse

pal <- brewer.pal(3, 'Dark2')

library(htmlwidgets)
hchart(hplot, hcaes(x = PC1, y = PC2, group = cluster), type = 'scatter') %>%
    hc_add_series_list(dfcomp) %>%
    hc_colors(color = c(pal, rep('lightblue', nrow(pca$rotation)))) %>%
    hc_tooltip(shared = F, formatter = JS(paste0("function() {var text = ''; if ([", paste0("'", unique(hplot$cluster), "'", collapse = ', '),"].indexOf(this.series.name) >= 0) {text = this.point.tooltip;} else {text = this.series.name;}return text;}"))) %>%
    hc_title(text = 'Clustering Coachella') %>%
    hc_subtitle(text = HTML('<em>PCA and clusters of artist tracks on Spotify</em>')) %>% 
    hc_xAxis(title = list(text = 'Principle Component 1')) %>% 
    hc_yAxis(title = list(text = 'Principle Component 2')) %>% 
    hc_add_theme(hc_theme_smpl())

############
library(threejs)

scatterplot3js(pca$x[, 1:3], color = pal[clust], labels = hplot$name, renderer = 'canvas')

##################
cluster_labels <- c('Hip-hop/Rock', 'EDM/Experimental', 'Alternative/Acoustic')

artist_plot_df <- artist_features %>% 
    mutate(cluster = paste0('Cluster #', clust, ' - ', cluster_labels[clust]),
           tooltip = artist_name,
           xvar = rescale(instrumentalness + danceability, to = c(0, 1)),
           yvar = rescale(acousticness - energy, to = c(0, 1)))

hchart(artist_plot_df, hcaes(x = xvar, y = yvar, group = cluster), type = 'scatter') %>%
    hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>%
    hc_colors(color = pal) %>%
    hc_xAxis(max = 1, title = list(text = 'Instrumentalness + Danceability')) %>%
    hc_yAxis(max = 1, title = list(text = 'Acousticness - Valence')) %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_yAxis(plotLines = list(list(
        value = .5,
        color = 'black',
        width = 2,
        zIndex = 2))) %>% 
    hc_xAxis(plotLines = list(list(
        value = .5,
        color = 'black',
        width = 2,
        zIndex = 2))) %>% 
    hc_title(text = 'The Three Types of Artists at Coachella 2017') %>% 
    hc_subtitle(text = HTML('<em>Clustering of their top tracks on Spotify</em>'))