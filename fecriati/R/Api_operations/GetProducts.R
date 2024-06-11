library(httr)
library(jsonlite)

# WooCommerce doğrulaması yaptıran kod
woocommerce_auth <- function() {
  consumer_key <- Sys.getenv("")
  consumer_secret <- Sys.getenv("")

  auth <- authenticate(user = consumer_key, password = consumer_secret, type = "basic")
  return(auth)
}

# Function to GET products from WooCommerce
get_products <- function(auth) {
  url <- "http://193.38.34.56:8000/wp-json/wc/v3/products"

  response <- GET(url, auth)
  status <- status_code(response)

  if (status == 200) {
    # Convert JSON response to a list
    products_list <-fromJSON(content(response, as = "text"))

    # Listeyi dataframe'e donustur
    product_df <- as.data.frame(products_list)
    return(product_df)

  } else {
    error <- content(response)
    return(error)
  }
}

# Gelen dataları CSV formatına kaydeden fonksiyon
save_to_csv <- function(data, file_name) {
  write.csv(data, file = file_name, row.names = FALSE)
}

# Get products
products <- get_products(auth)
save_to_csv(products, "products.csv")
