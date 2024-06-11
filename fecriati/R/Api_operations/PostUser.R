library(httr)
library(jsonlite)

# WooCommerce doğrulaması yaptıran kod
woocommerce_auth <- function() {
  consumer_key <- Sys.getenv("")
  consumer_secret <- Sys.getenv("")

  auth <- authenticate(user = consumer_key, password = consumer_secret, type = "basic")
  return(auth)
}

# CSV dosyalarını okuduğumuz fonksiyon
read_data <- function(csv_file) {
  customer_data <- read.csv(csv_file, stringsAsFactors = FALSE)
  return(customer_data)
}

# WooCommerce veri yapısını belirlediğimiz fonksiyon
add_customer_to_woocommerce <- function(customer, auth) {
  url <- "https://your-woocommerce-site.com/wp-json/wc/v3/customers"

  # Construct customer data for WooCommerce API
  customer_data <- list(
    email = customer$email,
    first_name = customer$first_name,
    last_name = customer$last_name,
    billing = list(
      first_name = customer$first_name,
      last_name = customer$last_name,
      address_1 = customer$street_address,
      address_2 = "",
      city = customer$city,
      state = customer$state,
      postcode = customer$postal_code,
      country = customer$country,
      email = customer$email,
      phone = ""
    ),
    role = "customer",
    meta_data = list(
      list(key = "age", value = customer$age),
      list(key = "gender", value = customer$gender),
      list(key = "traffic_source", value = customer$traffic_source),
      list(key = "created_at", value = customer$created_at)
    )
  )

  response <- POST(
    url = url,
    body = toJSON(customer_data, auto_unbox = TRUE),
    add_headers("Content-Type" = "application/json"),
    auth
  )

  status <- status_code(response)

  if(status == 201){
    return(content(response))
  }else
  {
    error <- content(response)
    return(error)
  }

}

# CSV'yi okuyup user'ları ekleyen fonksiyon
process_customers <- function(csv_file) {
  customers <- read_data(csv_file)
  auth <- woocommerce_auth()

  for (i in 1:nrow(customers)) {
    customer <- customers[i, ]
    result <- add_customer_to_woocommerce(customer, auth)
    print(result)
  }
}

# CSV dosyasının path'ini belirtme
csv_file_path <- file.path("..", "..","inst", "exdata", "usersyeni.csv")

# Fonksiyon çağrısı
process_customers(csv_file_path)
