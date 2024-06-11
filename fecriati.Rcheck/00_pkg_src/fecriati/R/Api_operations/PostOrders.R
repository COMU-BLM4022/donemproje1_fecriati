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
add_order_to_woocommerce <- function(order, order_items, auth) {
  url <- "http://193.38.34.56:8000/wp-json/wc/v3/orders"

  # orderitems$order_id ile order$order_id'yi eşleştirdiğimiz kod
  items <- subset(order_items, order_id == order$order_id)

  # Bir siparişte birden fazla ürün varsa bunları bulduğumuz ve line_items olarak oluşturduğumuz kod
  line_items <- lapply(1:nrow(items), function(i) {
    list(
      product_id = items$inventory_item_id[i],
      quantity = order$num_of_item,
      status = items$status[i]
    )
  })

  # WooCommerce API için data yapısını oluşturduğumuz kod
  order_data <- list(
    parent_id = order$order_id,
    status = order$status,
    currency = "TRY",
    customer_id = items$user_id[1],
    customer_note = "",
    date_created = order$created_at,
    payment_method = "",
    payment_method_title = "",
    line_items = line_items,
    set_paid = TRUE,
    meta_data = list(
      list(key = "gender", value = order$gender)
    )
  )

  response <- POST(
    url = url,
    body = toJSON(order_data, auto_unbox = TRUE),
    add_headers("Content-Type" = "application/json"),
    auth
  )

  status <- status_code(response)

  if (status == 201) {
    return(content(response))
  } else {
    error <- content(response)
    return(error)
  }
}

# CSV'yi okuyup Order'ları ekleyen fonksiyon
process_orders <- function(orders_csv, order_items_csv) {
  orders <- read_data(orders_csv)
  order_items <- read_data(order_items_csv)
  auth <- woocommerce_auth()

  for (i in 1:nrow(orders)) {
    order <- orders[i, ]
    result <- add_order_to_woocommerce(order, order_items, auth)
    print(result)
  }
}

csv_file_path_orders <- file.path("..", "..","inst", "exdata", "ordersyeni.csv")
csv_file_path_order_items <- file.path("..", "..","inst", "exdata", "order_itemsyeni.csv")

# Fonksiyon çağrısı
process_orders(csv_file_path_orders, csv_file_path_order_items)
