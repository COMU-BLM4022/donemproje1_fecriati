library(httr)
library(jsonlite)

# WooCommerce doğrulaması yaptıran kod
woocommerce_auth <- function() {
  consumer_key <- Sys.getenv("")
  consumer_secret <- Sys.getenv("")

  auth <- authenticate(user = consumer_key, password = consumer_secret, type = "basic")
  return(auth)
}

# Function to GET orders from WooCommerce
get_orders <- function(auth) {
  url <- "http://193.38.34.56:8000/wp-json/wc/v3/orders"

  response <- GET(url, auth)
  status <- status_code(response)

  if (status == 200) {
    # Convert JSON response to a list
    order_list <- fromJSON(content(response, as = "text"))

    # Listeyi dataframe'e donustur
    order_df <- as.data.frame(order_list)
    return(order_df)
  } else {
    error <- content(response)
    return(error)
  }

}

# Gelen dataları CSV formatına kaydeden fonksiyon
save_to_csv <- function(data, file_name) {
  write.csv(data, file = file_name, row.names = FALSE)
}

# Get orders
orders <- get_orders(auth)
save_to_csv(orders, "orders.csv")


