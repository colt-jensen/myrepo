library(rvest)
library(httr)
library(stringr)

#Be sure that you know the file path for the downloaded PDFs

# Base URL for website
base_url <- "https://dca.georgia.gov"

# List of county pages to scan
#county_urls <- c(
#  "https://dca.georgia.gov/bryan-county-service-delivery-strategy",
 # "https://dca.georgia.gov/ben-hill-county-service-delivery-strategy",
  # "https://dca.georgia.gov/bleckley-county-service-delivery-strategy"
  # Add more counties as needed
#)

georgia_counties <- c(
  "appling", "atkinson", "bacon", "baker", "baldwin", "banks", "barrow", "bartow", "ben-hill",
  "berrien", "bibb", "bleckley", "brantley", "brooks", "bryan", "bulloch", "burke", "butts",
  "calhoun", "camden", "candler", "carroll", "catoosa", "charlton", "chatham", "chattooga", "cusseta-chattahoochee",
  "cherokee", "athens-clarke", "clay", "clayton", "clinch", "cobb", "coffee", "colquitt", "columbia",
  "cook", "coweta", "crawford", "cris", "dade", "dawson", "decatur", "dekalb", "dodge",
  "dooly", "dougherty", "douglas", "early", "echols", "effingham", "elbert", "emanuel",
  "evans", "fannin", "fayette", "floyd", "forsyth", "franklin", "fulton", "gilmer", "glascock",
  "glynn", "gordon", "grady", "greene", "gwinnett", "habersham", "hall", "hancock", "haralson",
  "harris", "hart", "heard", "henry", "houston", "irwin", "jackson", "jasper", "jeff-davis",
  "jefferson", "jenkins", "johnson", "jones", "lamar", "lanier", "laurens", "lee", "liberty",
  "lincoln", "long", "lowndes", "lumpkin", "mcduffie", "mcintosh", "macon", "madison", "marion",
  "meriwether", "miller", "mitchell", "monroe", "montgomery", "morgan", "murray", "columbus-muscogee",
  "newton", "oconee", "oglethorpe", "paulding", "peach", "pickens", "pierce", "pike", "polk",
  "pulaski", "putnam", "georgetown-quitman-unified-government", "rabun", "randolph", "richmond", "rockdale", "schley",
  "screven", "seminole", "spalding", "stephens", "stewart", "sumter", "talbot", "taliaferro",
  "tattnall", "taylor", "telfair", "terrell", "thomas", "tift", "toombs", "towns", "treutlen",
  "troup", "turner", "twiggs", "union", "upton", "walker", "walton", "ware", "warren",
  "washington", "wayne", "webster", "wheeler", "white", "whitfield", "wilcox", "wilkes",
  "wilkinson", "worth"
)

# Generate county URLs using the standard URL pattern
county_urls <- paste0("https://dca.georgia.gov/", georgia_counties, "-county-service-delivery-strategy")

# Print the generated URLs
print(county_urls)


# Function to extract county name from URL
get_county_name <- function(url) {
  url %>%
    str_extract("https://dca.georgia.gov/([^/]+)-county-service-delivery-strategy") %>%
    str_replace("https://dca.georgia.gov/", "")
}

# Function to extract PDF links
get_pdf_links <- function(url) {
  webpage <- tryCatch(read_html(url), error = function(e) NULL)
  
  if (is.null(webpage)) {
    message("Failed to fetch page: ", url)
    return(NULL)
  }
  
  # Extract PDF links
  pdf_links <- webpage %>%
    html_nodes(".link-teaser-list__item a") %>%
    html_attr("href") %>%
    na.omit() %>%
    str_subset("/document/plans/.*?/download$")
  
  # Convert relative links to absolute URLs
  pdf_links <- paste0(base_url, pdf_links)
  
  return(pdf_links)
}

# Function to download PDFs and save in county-specific folder
download_pdfs <- function(pdf_links, county_folder) {
  for (pdf_url in pdf_links) {
    # Extract meaningful filename
    pdf_filename <- pdf_url %>%
      str_extract("/document/plans/([^/]+)/download$") %>%
      str_replace_all("/document/plans/|/download", "") %>%
      paste0(".pdf")
    
    # Full file path
    pdf_name <- file.path(county_folder, pdf_filename)
    
    if (!file.exists(pdf_name)) {  # Avoid re-downloading
      response <- tryCatch(GET(pdf_url, write_disk(pdf_name, overwrite = TRUE)), error = function(e) NULL)
      
      if (!is.null(response) && response$status_code == 200) {
        message("Downloaded: ", pdf_name)
      } else {
        message("Failed to download: ", pdf_url)
      }
    } else {
      message("Already exists: ", pdf_name)
    }
  }
}

# Loop through each county and scrape/download PDFs
for (county_url in county_urls) {
  county_name <- get_county_name(county_url)  # Extract county name
  county_folder <- file.path("downloaded_pdfs", county_name)  # Create county folder path
  
  if (!dir.exists(county_folder)) dir.create(county_folder, recursive = TRUE)  # Ensure folder exists
  
  message("\nScanning: ", county_name)
  pdf_links <- get_pdf_links(county_url)
  
  if (length(pdf_links) > 0) {
    download_pdfs(pdf_links, county_folder)
  } else {
    message("No PDFs found for ", county_name)
  }
}
