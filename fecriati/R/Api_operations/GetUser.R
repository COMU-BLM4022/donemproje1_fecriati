library(httr)
library(jsonlite)

# WooCommerce doğrulaması yaptıran kod
woocommerce_auth <- function() {
  consumer_key <- Sys.getenv("")
  consumer_secret <- Sys.getenv("")

  auth <- authenticate(user = consumer_key, password = consumer_secret, type = "basic")
  return(auth)
}

# Kullanıcıları çeken kod
get_customers <- function(auth) {
  url <- "https://your-woocommerce-site.com/wp-json/wc/v3/customers"

  response <- GET(url, auth)
  status <- status_code(response)

  if (status == 200) {
    return(fromJSON(content(response, as = "text")))
  } else {
    error <- content(response)
    return(error)
  }
}

# Gelen dataları CSV formatına kaydeden fonksiyon
save_to_csv <- function(data, file_name) {
  write.csv(data, file = file_name, row.names = FALSE)
}

# Get user
customers <- get_customers(auth)
save_to_csv(customers, "customers.csv")
