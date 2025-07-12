#' NotionApercuUpdater
#'
#' Gets the OneDrive links for the file in the folder in folder_path
#' @param folder_path the local path to the folder I want the link from
#' @return A DataFrame with file names and their links
#' @examples
#' OneDriveLinks("/Users/leohenry/Library/CloudStorage/OneDrive-Personnel/Documents/Cours/Projets/CrimeUS_Fogglesong-Levy/2024_02_LA_DeclinationRates/output/240613-DRandCrime/3.240627-Changes/1-CityAnalysis")
#' @export

NotionApercuUpdater <- function(folder_path){

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

  nat <- structure(as.raw(c(0x46, 0x78, 0x2f, 0x18, 0x7e, 0xc5, 0xb3,
                            0x2d, 0x3a, 0xd0, 0x98, 0xc3, 0x03, 0xef, 0x6a, 0xd9, 0x59, 0x3b,
                            0x01, 0x99, 0x59, 0x76, 0x9d, 0x92, 0x54, 0x75, 0xd9, 0x09, 0xf8,
                            0x86, 0x16, 0xcd, 0x03, 0xf0, 0xf2, 0x30, 0xb2, 0x65, 0xb7, 0x60,
                            0x9f, 0x81, 0xfb, 0x06, 0x45, 0x3f, 0x70, 0xee, 0x97, 0x30, 0x2d,
                            0x65, 0xd8, 0x94, 0x99, 0x03, 0xb3, 0x9b, 0xfb, 0x37, 0xf8, 0xc7,
                            0x7c, 0x27)), iv = as.raw(c(0x91, 0x9f, 0x90, 0x03, 0x5a, 0x11,
                                                        0x17, 0x5c, 0xd5, 0x98, 0x47, 0x8a, 0x09, 0x9d, 0x3b, 0xc0)))

  notion_api_token <- decrypt_credential(nat, password)

  notion_api_token <- "secret_uFCqTOiSx2pEvKQYPjYk3DgMzF3djmOGzgAgG7pk634"

  page_url <- "https://www.notion.so/progressiveprosecution/Declination-And-Crime-Rate-in-LA-28a85270ff0e459f9dcd2955f98ad2e0?pvs=4"

  headers <- c(
    "Authorization" = paste("Bearer", notion_api_token),
    "Content-Type" = "application/json",
    "Notion-Version" = "2022-06-28"
  )

  # Function to extract page ID from URL and format it
  extract_page_id <- function(url) {
    pattern <- "(?<=-)[0-9a-f]{32}(?=\\?)"
    raw_id <- str_extract(url, pattern)
    formatted_id <- paste0(substr(raw_id, 1, 8), "-", substr(raw_id, 9, 12), "-", substr(raw_id, 13, 16), "-", substr(raw_id, 17, 20), "-", substr(raw_id, 21, 32))
    return(formatted_id)
  }

  # Extract the page ID from the provided URL
  page_id <- extract_page_id(page_url)
  print(paste("Extracted Page ID:", page_id))

  # Function to get page content
  get_page_content <- function(page_id) {
    url <- paste0("https://api.notion.com/v1/blocks/", page_id, "/children")
    response <- GET(url, add_headers(.headers = headers))

    if (status_code(response) == 200) {
      return(content(response, "parsed", simplifyVector = TRUE))
    } else {
      print(paste("Failed to retrieve page content, Status code:", status_code(response)))
      print(content(response, "parsed", simplifyVector = TRUE))
      return(NULL)
    }
  }

  # Function to update a block
  update_block <- function(block_id, data) {
    url <- paste0("https://api.notion.com/v1/blocks/", block_id)
    response <- PATCH(url,
                      add_headers(.headers = headers),
                      body = toJSON(data, auto_unbox = F),
                      encode = "json")

    if (status_code(response) == 200) {
      print(paste("Successfully updated block:", block_id))
    } else {
      print(paste("Failed to update block:", block_id, ", Status code:", status_code(response)))
      print(content(response, "parsed", simplifyVector = TRUE))
    }
  }



  retrieve_block <- function(block_id) {
    url <- paste0("https://api.notion.com/v1/blocks/", block_id)
    response <- GET(url, add_headers(.headers = headers))
    content(response, "parsed", simplifyVector = TRUE)
  }

  block_info <- retrieve_block(block_id)
  parent_id <- block_info$parent$page_id

  delete_block <- function(block_id) {
    url <- paste0("https://api.notion.com/v1/blocks/", block_id)
    response <- DELETE(url, add_headers(.headers = headers))

    if (status_code(response) == 200) {
      print(paste("Successfully deleted block:", block_id))
    } else {
      print(paste("Failed to delete block:", block_id, ", Status code:", status_code(response)))
      print(content(response, "parsed", simplifyVector = TRUE))
    }
  }

  delete_block(block_id)


  create_block <- function(parent_id, embed_url) {
    url <- paste0("https://api.notion.com/v1/blocks/", parent_id, "/children")

    data <- toJSON(list(
      children = list(
        list(
          object = "block",
          type = "embed",
          embed = list(
            url = embed_url
          )
        )
      )
    ), auto_unbox = TRUE, pretty = TRUE)

    response <- POST(url,
                     add_headers(.headers = headers),
                     body = data,
                     encode = "json")

    if (status_code(response) == 200) {
      print("Successfully created new block")
    } else {
      print(paste("Failed to create new block, Status code:", status_code(response)))
      print(content(response, "parsed", simplifyVector = TRUE))
    }
  }



  create_block(parent_id, embed_url)



  update_block <- function(block_id, embed_url) {
    url <- paste0("https://api.notion.com/v1/blocks/", block_id)

    data <- list(
      embed = list(
        url = embed_url
      )
    )

    json_data <- toJSON(data, auto_unbox = TRUE, pretty = TRUE)
    print(json_data)  # Print the JSON data for debugging

    response <- PATCH(url,
                      add_headers(.headers = headers),
                      body = json_data,
                      encode = "json")

    if (status_code(response) == 200) {
      print(paste("Successfully updated block:", block_id))
    } else {
      print(paste("Failed to update block:", block_id, ", Status code:", status_code(response)))
      print(content(response, "parsed", simplifyVector = TRUE))
    }
  }



  # Main function
  main <- function() {
    page_content <- get_page_content(page_id)

    if (!is.null(page_content)) {
      # Debug: Print the structure of page_content
      # print(str(page_content))

      blocks <- page_content$results

      if (!is.null(blocks) && nrow(blocks) > 0) {
        for (i in 1:nrow(blocks)) {
          block <- blocks[i,]
          if (is.list(block) && !is.null(block$type) && block$type == "link_preview") {
            block_id <- block$id
            embed_url <- block$link_preview$url

            # Convert link_preview to text mention

            update_block(block_id, data)

            update_block(block_id, embed_url)


            # Convert mention back to embed (link preview)
            Sys.sleep(1)  # Adding delay to ensure updates are processed sequentially
            embed_data <- list(
              type = "link_preview",
              embed = list(
                url = embed_url
              )
            )
            update_block(block_id, embed_data)
          }
        }
      } else {
        print("No blocks found in the page content.")
      }
    }
  }

  main()


}
