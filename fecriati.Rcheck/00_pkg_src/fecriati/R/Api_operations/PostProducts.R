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
add_product_to_woocommerce <- function(product, auth) {
  url <- "http://193.38.34.56:8000/wp-json/wc/v3/products"

  csv_file_path <- file.path("..", "inst", "exdata", "product_yeni.csv")
  products <- read_data(csv_file_path)
  categories = unique(product$category)
  # Kategorik veriyi faktör veri tipine dönüştürme
  factor_categories <- factor(categories)
  # Faktör veriyi sınıf dizisi olarak almak
  numeric_categories <- as.numeric(factor_categories)

  # Construct product data for WooCommerce API
  product_data <- list(
    name = product$name,
    regular_price = as.character(product$retail_price),
    description = product$brand,
    short_description = "",
    sku = product$sku,
    categories = list(
      list(id = numeric_categories)
    ),
    images = list(
      list(src = "")
    ),
    attributes = list(
      list(
        id = product$id,
        visible = TRUE,
        variation = FALSE,
        options = c("Mavi", "Siyah")
      )
    ),
    meta_data = list(
      list(key = "department", value = product$department),
      list(key = "distribution_center_id", value = product$distribution_center_id)
    )
  )

  response <- POST(
    url = url,
    body = toJSON(product_data, auto_unbox = TRUE),
    add_headers("Content-Type" = "application/json"),
    auth
  )

  status <- status_code(response)

  if(status == 201){
    return(content(response))
  } else {
    error <- content(response)
    return(error)
  }
}

# CSV'yi okuyup user'ları ekleyen fonksiyon
process_products <- function(csv_file) {
  products <- read_product_data(csv_file)
  auth <- woocommerce_auth()

  for (i in 1:nrow(products)) {
    product <- products[i, ]
    result <- add_product_to_woocommerce(product, auth)
    print(result)
  }
}

# Set your CSV file path
csv_file_path <- file.path("..", "..","inst", "exdata", "product_yeni.csv")

# Fonksiyon çağrısı
process_products(csv_file_path)
