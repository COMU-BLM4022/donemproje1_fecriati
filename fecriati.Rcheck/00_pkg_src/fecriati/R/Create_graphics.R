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


#' Final analizimiz için bar grafik oluşturma
#'
#' Verilen veri çerçevesine dayalı olarak bir bar plot oluşturur.
#'
#' @param data Veri çerçevesi.
#' @param x X ekseni için kullanılacak değişken adı.
#' @param y Y ekseni için kullanılacak değişken adı.
#' @param fill Barların dolgu estetiği için kullanılacak değişken adı.
#' @param title Grafiğin başlığı.
#' @param x_label X ekseninin etiketi.
#' @param y_label Y ekseninin etiketi.
#' @param fill_label Dolgu estetiğinin etiketi.
#' @param angle X ekseni metinlerinin açısı (varsayılan 30 derece).
#' @return ggplot2 nesnesi olarak döndürür.
#' @import ggplot2
#' @export
create_final_analysis_graph <- function(data, x, y, fill, title, x_label, y_label, fill_label, angle = 30) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required but not installed.")
  }
  library(ggplot2)
  ggplot(data, aes_string(x = x, y = y, fill = fill)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    labs(title = title,
         x = x_label,
         y = y_label,
         fill = fill_label) +
    theme(axis.text.x = element_text(angle = angle, hjust = 1))
}











