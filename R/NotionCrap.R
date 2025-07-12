#' NotionCrap
#'
#' I forgot what this is for
#' @param folder_path the local path to the folder I want the link from
#' @return A DataFrame with file names and their links
#' @examples
#' OneDriveLinks("/Users/leohenry/Library/CloudStorage/OneDrive-Personnel/Documents/Cours/Projets/CrimeUS_Fogglesong-Levy/2024_02_LA_DeclinationRates/output/240613-DRandCrime/3.240627-Changes/1-CityAnalysis")
#' @export

NotionCrap <- function(){
  # Function to retrieve block or page details
  retrieve_block_or_page <- function(block_id) {
    url <- paste0("https://api.notion.com/v1/blocks/", block_id)

    response <- GET(url, add_headers(.headers = headers))

    if (status_code(response) == 200) {
      print("Successfully retrieved block or page")
      block_info <- content(response, "parsed", simplifyVector = TRUE)
      print(block_info)  # Print the block info for debugging
      return(block_info)
    } else {
      print(paste("Failed to retrieve block or page, Status code:", status_code(response)))
      print(content(response, "parsed", simplifyVector = TRUE))
      return(NULL)
    }
  }

  # Function to create a new block with an embed link as text
  create_block <- function(parent_id, embed_url) {
    url <- paste0("https://api.notion.com/v1/blocks/", parent_id, "/children")

    data <- list(
      children = list(
        list(
          object = "block",
          type = "paragraph",
          paragraph = list(
            rich_text = list(
              list(
                type = "text",
                text = list(
                  content = embed_url
                )
              )
            ),
            color = "default"
          )
        )
      )
    )

    json_data <- toJSON(data, auto_unbox = TRUE, pretty = TRUE)
    print(json_data)  # Print the JSON data for debugging

    response <- POST(url,
                     add_headers(.headers = headers),
                     body = json_data,
                     encode = "json")

    if (status_code(response) == 200) {
      print("Successfully created new block")
      content(response, "parsed", simplifyVector = TRUE)
    } else {
      print(paste("Failed to create new block, Status code:", status_code(response)))
      print(content(response, "parsed", simplifyVector = TRUE))
    }
  }

  # Example usage
  embed_url <- "https://1drv.ms/b/s!AmhC7ecomAjOivNI5HFcn51vtf-BjA?e=mmuxaY"

  # Retrieve the parent ID of the block
  block_info <- retrieve_block_or_page(page_id)
  if (!is.null(block_info)) {
    parent_id <- block_info$parent[[block_info$parent$type]]
    parent_type <- block_info$parent$type
    print(paste("Parent ID:", parent_id))
    print(paste("Parent Type:", parent_type))

    # Create a new block with the embed link as text
    create_block(parent_id, embed_url)
  } else {
    print("Failed to retrieve parent ID")
  }
}
