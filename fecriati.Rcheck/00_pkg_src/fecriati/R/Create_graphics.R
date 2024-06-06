#' Grafik oluşturma ve istatistiksel analizler yapma
#'
#' @param data Veri çerçevesi.
#' @export
create_graphics <- function(data) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required but not installed.")
  }

  library(ggplot2)

  # Yaş dağılımını gösteren histogram
  p <- ggplot2::ggplot(data, ggplot2::aes(x = age)) +
    ggplot2::geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
    ggplot2::labs(title = "Kullanıcıların Yaş Dağılımı", x = "Yaş", y = "Kullanıcı Sayısı") +
    ggplot2::theme_minimal()

  print(p)
}
