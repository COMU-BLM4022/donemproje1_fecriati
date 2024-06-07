#' CSV dosyasını okur ve veriyi işler
#'
#' @param file_path Dosya yolunu belirtir.
#' @param sep CSV dosyasındaki ayırıcı karakter (varsayılan ";").
#' @import utils
#' @return Veri çerçevesi.
#' @export
read_data <- function(file_path, sep = ";") {
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("utils package is not available.")
  }
    # utils kütüphanesini yükle
    # CSV dosyasını oku
    data <- utils::read.csv(file_path, sep = sep, stringsAsFactors = FALSE)
    return(data)
}

#' Eksik verileri doldurma veya çıkarma işlemlerini yapar
#'
#' @param data Veri çerçevesi.
#' @return Düzenlenmiş veri çerçevesi.
#' @import dplyr
#' @export
handle_missing_data <- function(data) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required but not installed.")
  }

  data <- data %>%
    dplyr::mutate(across(where(is.character), ~ ifelse(. == "", NA, .))) %>%
    dplyr::mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

  return(data)
}


#' Veriye genel bakış ve eksik verileri kontrol eder
#'
#' @param data Veri çerçevesi.
#' @export
summarize_data <- function(data) {
  # Veri çerçevesinin boyutunu yazdır
  cat("Veri çerçevesinin boyutu:", nrow(data), "satır,", ncol(data), "sütun\n")



  # Veri çerçevesindeki sütun isimlerini yazdır
  cat("\nSütun isimleri:", paste(colnames(data), collapse = ", "), "\n")

  # Veri çerçevesindeki her sütundaki eksik değerlerin sayısını yazdır
  missing_values <- sapply(data, function(x) sum(is.na(x)))
  cat("\nHer sütundaki eksik değer sayısı:\n")
  print(missing_values)
}


#' Gereksiz kolonları kaldırır
#'
#' @param data Veri çerçevesi.
#' @param columns Kaldırılacak kolonların isimleri.
#' @return Düzenlenmiş veri çerçevesi.
#' @import dplyr
#' @export
remove_unnecessary_columns <- function(data, columns) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required but not installed.")
  }

  data <- dplyr::select(data, -dplyr::all_of(columns))
  return(data)
}




#' Veri çerçevesini CSV dosyasına kaydeder
#'
#' @param data Veri çerçevesi.
#' @param file_path Kaydedilecek dosyanın yolu.
#' @import utils
#' @export
save_data <- function(data, file_path) {
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("utils package is required but not installed.")
  }

  utils::write.csv(data, file_path, row.names = FALSE)
  return(head(data))
}



# Ana işlem fonksiyonu
#' Veriyi yükler, eksik verileri işler, gereksiz kolonları kaldırır ve sonuçları kaydeder
#'
#' @param input_path Giriş CSV dosyasının yolu.
#' @param output_path Çıkış CSV dosyasının yolu.
#' @param sep CSV dosyasındaki ayırıcı karakter (varsayılan ";").
#' @param remove_cols Kaldırılacak kolonların isimleri.
#' @export
process_data <- function(data, output_path, sep = ";", remove_cols) {

  summarize_data(data)
  data <- handle_missing_data(data)

  if (!is.null(remove_cols) && length(remove_cols) > 0) {
    data <- remove_unnecessary_columns(data, remove_cols)
  }
  cat("\nVeri ön işleme yaptıktan sonra veriler:\n\n")
  summarize_data(data)

  save_data(data, output_path)
  return(as.data.frame(data))
}



