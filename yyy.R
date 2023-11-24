library(httr)
library(jsonlite)
library(dplyr)

spotify_token <- function() {
  # Spotify API endpoint ve client bilgileri
  endpoint <- "https://accounts.spotify.com/api/token"
  client_id <- Sys.getenv("SPOTIFY_ID")
  client_secret <- Sys.getenv("SECRET_KEY")
  # Token alma isteği için gerekli veriler
  body <- list(
    grant_type = "client_credentials"
  )
  
  # POST isteği yapma
  response <- POST(
    url = endpoint,
    authenticate(client_id, client_secret),
    body = body,
    encode = "form",
    verbose()
  )
  
  # HTTP status kodunu al
  status_code <- status_code(response)
  
  # Token alma başarılıysa token değerini al
  if (status_code == 200) {
    token <- paste("Bearer", content(response)$access_token)
  } else {
    token <- NULL
  }
  
  # Sonuçları liste olarak döndür
  result <- list(
    status_code = status_code,
    token = token
  )
  
  return(result)
}

# Fonksiyonu kullanarak Spotify token'ını al



spotify_search_artist <- function(artist_name) {
  # Spotify API endpoint URL
  endpoint <- "https://api.spotify.com/v1/search"
  
  # Token al
  token_output <- spotify_token()
  if (token_output$status_code != 200) {
    stop("Token alınamadı.")
  }
  
  # Token string
  token <- token_output$token
  
  # Query parameters
  params <- list(q = artist_name, type = "artist")
  
  # HTTP GET isteği
  response <- GET(url = endpoint, query = params, add_headers(Authorization = token))
  
  # HTTP status code
  status_code <- status_code(response)
  
  # Çıktı listesi
  output <- list(status_code = status_code)
  
  # HTTP status code 200 ise, JSON verisini işle ve search_results oluştur
  if (status_code == 200) {
    json_data <- content(response, as = "text")
    artist_results <- fromJSON(json_data)$artists$items %>%
      select(artist = name, id)
    output$search_results <- artist_results
  }
  
  return(output)
}

# Fonksiyonları çağırma
search_result <- spotify_search_artist("Lady Gaga")
token_result <- spotify_token()

# Çıktıları görüntülemee

print(search_result)
print(token_result)