

#' Veritabanı bağlantısını kurar ve bağlantı nesnesini döndürür
#'
#' @param db_path Veritabanı dosyasının yolunu belirtir.
#' @return Veritabanı bağlantısı nesnesi.
#' @import RSQLite
#' @export
connect_to_database <- function(db_path) {
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    library(RSQLite)
    con = RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_path)
  }else{
    stop("RSQLite package is required. Please install it using install.packages('RSQLite').")
  }

}

