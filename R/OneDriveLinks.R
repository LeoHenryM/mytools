#' OneDriveLinks
#'
#' Gets the OneDrive links for the file in the folder in folder_path
#' @param folder_path the local path to the folder I want the link from
#' @return A DataFrame with file names and their links
#' @examples
#' OneDriveLinks("/Users/leohenry/Library/CloudStorage/OneDrive-Personnel/Documents/Cours/Projets/CrimeUS_Fogglesong-Levy/2024_02_LA_DeclinationRates/output/240613-DRandCrime/3.240627-Changes/1-CityAnalysis")
#' @export

OneDriveLinks <- function(folder_path){

  library(httr)
  library(jsonlite)
  library(tidyverse)
  library(openssl)
  library(getPass)


  folder_path <- folder_path %>%
    str_remove("/Users/leohenry/Library/CloudStorage/OneDrive-Personnel/")

  # Decrypt a credential
  decrypt_credential <- function(encrypted, password) {
    iv <- attr(encrypted, "iv")
    # Decrypt the raw vector using AES-CBC with the given IV
    decrypted_raw <- aes_cbc_decrypt(encrypted, key = sha256(charToRaw(password)), iv = iv)
    # Convert the raw vector back to a character string
    decrypted_text <- rawToChar(decrypted_raw)
    return(decrypted_text)
  }

  password <- getPass::getPass("Password to Unlock Ids: ")


  # Azure AD App Ecrypted credentials - Decryption
  ecid <- structure(as.raw(c(0x0b, 0x2d, 0x1c, 0xcc, 0xc8, 0xc9, 0x78,
                             0x3e, 0x44, 0x41, 0xa6, 0xf0, 0xfd, 0x22, 0x18, 0xbe, 0xec, 0x0f,
                             0xd4, 0x17, 0xf8, 0x34, 0xb4, 0x50, 0xf8, 0xfe, 0xa5, 0xfc, 0xc7,
                             0x2b, 0x86, 0x83, 0xf7, 0x9f, 0x7a, 0xf4, 0x26, 0xc6, 0xc0, 0x0b,
                             0xdf, 0x73, 0xc9, 0xc3, 0x01, 0xb5, 0xd9, 0xc1)),
                    iv = as.raw(c(0xc6,
                                  0x0d, 0x72, 0x8a, 0x31, 0x88, 0x15, 0xbd, 0x30, 0x5e, 0xa5, 0x73,
                                  0xbc, 0xf3, 0x07, 0xba)))
  ecse <- structure(as.raw(c(0x58, 0x0d, 0xd2, 0x75, 0x39, 0x49, 0x74,
                             0x8c, 0x4a, 0x68, 0xb5, 0x34, 0xf3, 0x50, 0xb4, 0xe7, 0xd7, 0x10,
                             0xb3, 0x80, 0xf4, 0x21, 0x1b, 0x1f, 0x0a, 0xb6, 0x70, 0x56, 0x37,
                             0x23, 0x9c, 0xc4, 0xc8, 0x92, 0x8d, 0xe2, 0x39, 0x3c, 0x60, 0xea,
                             0xeb, 0xfc, 0x69, 0x24, 0x98, 0xea, 0x01, 0x7e)), iv = as.raw(c(0x4b,
                                                                                             0xc0, 0x07, 0x89, 0xa7, 0xa5, 0x80, 0xba, 0xdf, 0xdf, 0xeb, 0xa6,
                                                                                             0xa3, 0x22, 0xbc, 0x26)))
  etid <- structure(as.raw(c(0xef, 0x1b, 0x18, 0x0a, 0xdb, 0xca, 0x16,
                             0xd8, 0x61, 0x6f, 0x9a, 0x47, 0x79, 0x95, 0x7e, 0x06, 0xdc, 0xc3,
                             0xbe, 0x50, 0xf1, 0x89, 0x5c, 0x96, 0xa4, 0xc8, 0x9c, 0x92, 0xa1,
                             0x46, 0x5e, 0x27, 0x8c, 0x54, 0xb3, 0xac, 0xff, 0xe5, 0x16, 0x32,
                             0xf5, 0x84, 0x46, 0x17, 0xd1, 0x16, 0xda, 0xab)), iv = as.raw(c(0xe7,
                                                                                             0xe5, 0xdf, 0x5f, 0x50, 0xcf, 0x0f, 0x69, 0xc0, 0x1f, 0x77, 0x1d,
                                                                                             0x76, 0xbb, 0x95, 0x3c)))


  # Azure AD App credentials - Decryption
  client_id <- decrypt_credential(ecid, password)
  client_secret <- decrypt_credential(ecse, password)
  tenant_id <- decrypt_credential(etid, password)
  redirect_uri <- "http://localhost:1410/"  # Local redirect URI for testing

  # OAuth2 endpoints
  authorize_url <- "https://login.microsoftonline.com/consumers/oauth2/v2.0/authorize"
  token_url <- "https://login.microsoftonline.com/consumers/oauth2/v2.0/token"

  # Authorization endpoint parameters
  authorize_params <- list(
    client_id = client_id,
    response_type = "code",
    redirect_uri = redirect_uri,
    response_mode = "query",
    scope = "https://graph.microsoft.com/Files.Read https://graph.microsoft.com/Files.ReadWrite https://graph.microsoft.com/User.Read",
    state = "12345"
  )

  # Redirect user to authorize URL
  authorize_url_full <- modify_url(authorize_url, query = authorize_params)
  browseURL(authorize_url_full)

  # Capture the 'code' parameter from the URL manually
  auth_code <- readline(prompt = "Capture the 'code' parameter from the URL manually: ")

  # Exchange authorization code for access token
  token_response <- POST(token_url,
                         body = list(
                           client_id = client_id,
                           client_secret = client_secret,
                           code = auth_code,
                           redirect_uri = redirect_uri,
                           grant_type = "authorization_code"
                         ),
                         encode = "form")

  if (http_status(token_response)$category != "Success") {
    print(content(token_response, "text"))
    stop("Failed to get access token: ", http_status(token_response)$reason)
  }

  token <- content(token_response)$access_token

  # Function to list all files in a OneDrive folder
  list_files_in_folder <- function(folder_path) {
    folder_url <- paste0("https://graph.microsoft.com/v1.0/me/drive/root:/", folder_path, ":/children")

    folder_response <- GET(folder_url, add_headers(Authorization = paste("Bearer", token)))

    if (http_status(folder_response)$category == "Success") {
      return(content(folder_response)$value)
    } else {
      print(content(folder_response, "text"))
      return(NULL)
    }
  }

  # Function to get shareable link for a file
  get_shareable_link <- function(file_id) {
    link_url <- paste0("https://graph.microsoft.com/v1.0/me/drive/items/", file_id, "/createLink")

    # Specify the link type and scope settings
    link_settings <- list(
      type = "view",
      scope = "anonymous"  # Ensuring the link is publicly accessible
    )

    link_response <- POST(link_url,
                          add_headers(Authorization = paste("Bearer", token),
                                      `Content-Type` = "application/json"),
                          body = toJSON(link_settings, auto_unbox = TRUE), # Convert list to JSON, ensuring single string values
                          encode = "json")

    if (http_status(link_response)$category == "Success") {
      link_content <- content(link_response)
      return(link_content$link$webUrl)
    } else {
      print(content(link_response, "text"))
      return(NA)
    }
  }

  # List all files in the specified folder
  files <- list_files_in_folder(folder_path)

  # Generate shareable links for all files
  file_links <- list()
  if (!is.null(files)) {
    for (file in files) {
      file_id <- file$id
      shareable_link <- get_shareable_link(file_id)
      file_links[[file$name]] <- shareable_link
    }
  }


  links <- data.frame(file_name = names(file_links), link = unlist(file_links))

  links$link <- paste0(links$link, "?e=2ZeChh")

  return(links)


}
