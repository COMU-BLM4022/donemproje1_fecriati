#' Kullanıcıları yaş ve cinsiyete göre gruplandırma
#'
#' @param users Kullanıcı veri çerçevesi
#' @return Gruplandırılmış kullanıcı veri çerçevesi
#' @import dplyr
#' @export
group_by_age_gender <- function(users) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required but not installed.")
  }

    users %>%
    mutate(
      age_group = case_when(
        age >= 12 & age < 18 ~ "12-18",
        age >= 18 & age < 30 ~ "18-30",
        age >= 30 & age < 50 ~ "30-50",
        age >= 50 & age <= 70 ~ "50-70",
        TRUE ~ "other"
      ),
      gender_group = case_when(
        gender == "F" ~ "Kadin",
        gender == "M" ~ "Erkek",
        TRUE ~ "other"
      )
    )
}


#' Verilen sütunları seçer ve veri çerçevesi döndürür
#'
#' @param data Veri çerçevesi.
#' @param columns Seçilecek sütunların isimleri.
#' @return Seçilen sütunları içeren veri çerçevesi.
#' @export
select_cols <- function(data, columns) {
  secilenDataframe <- data[, columns]
  return(secilenDataframe)
}


#' Inner join islemi yapar, veri çerçevesi döndürür
#'
#' @param data Veri çerçevesi.
#' @param columns Seçilecek sütunların isimleri.
#' @return Seçilen sütunları içeren veri çerçevesi.
#' @import dplyr
#' @export
inner_joinle <- function(order_items, orders, users, products) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required but not installed.")
  }

    joined_data <- order_items %>%
    inner_join(orders, by = c("order_id", "user_id")) %>%
    inner_join(users, by = c("user_id" = "id")) %>%
    inner_join(products, by = c("product_id" = "id"))

    return(joined_data)
}


#' Aynı kombinasyona sahip satırların fiyatlarını toplar
#'
#' @param data Dataframe içindeki veri
#' @param group_cols Gruplamak istediğiniz kolonlar (vektör olarak belirtilmelidir)
#' @param sum_col Toplanacak kolon adı
#' @return Toplam harcama ile gruplandırılmış veri çerçevesi
#' @import dplyr
#' @export
group_by_and_send_fun <- function(data, group_cols, sum_col, sending_fun) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required but not installed.")
  }

  result <- data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(total_spending = sending_fun(.data[[sum_col]], na.rm = TRUE))

  return(result)
}



#' Filter matched categories based on total spending
#'
#' This function filters the matched categories based on the total spending
#' being in the maximum total spending.
#'
#' @param result Eslestirme yapilacak dataframe1
#' @param maxResult Eslestirme yapilacak dataframe1
#' @param group_cols Eslestirme ypinca hangi columnlar alınsın ve geriye dondurulsun
#' @return Eslesen veri cercevelerini dondurur
#' @import dplyr
#' @export
filter_matched_categories <- function(data1, data2, group_cols) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required but not installed.")
  }
  matched_categories <- data1 %>%
    filter(total_spending %in% data2$total_spending) %>%
    select(all_of(group_cols))

  return(matched_categories)
}


#' Verileri birleştirir ve analiz yapar
#'
#' @param order_items Sipariş öğeleri veri çerçevesi
#' @param orders Siparişler veri çerçevesi
#' @param users Kullanıcılar veri çerçevesi
#' @param products Ürünler veri çerçevesi
#' @return Yaş ve cinsiyete göre en çok harcama yapılan kategorilerin veri çerçevesi
#' @export
merge_and_analyze <- function(order_items, orders, users, products, selectedCols = NULL, number_of_analysis=TRUE) {
  # number of analysis TRUE ise ilk analiz için geldim
  # FALSE ise country analizi için geldim

  if(number_of_analysis){
    users = group_by_age_gender(users = users)
  }

  yeni_user_tablo <- select_cols(users, selectedCols)
  joined_data = inner_joinle(order_items, orders, yeni_user_tablo, products)

  if (number_of_analysis) {
    temiz_joined_tablo <- joined_data[, c("id", "status.x", "age_group","gender_group","category","retail_price","num_of_item")]
  }else{
    temiz_joined_tablo <- joined_data[, c("id", "status.x", "category","retail_price","num_of_item","country","traffic_source")]
  }

  temiz_joined_tablo <- temiz_joined_tablo %>%
    filter(status.x != "Returned" & status.x != "Cancelled")
  # returned ve canceled olan siparisleri almadigimiza emin oluyoruz

  temiz_joined_tablo$total_spending <- temiz_joined_tablo$retail_price * temiz_joined_tablo$num_of_item
  # yapilan toplam harcamayı yeni satir halinde olusturuyorum


  if(number_of_analysis){
    analiz_datasi <- temiz_joined_tablo[, c("id", "age_group","gender_group","category","total_spending")]
    result = group_by_and_send_fun(data = analiz_datasi, group_cols = c("age_group", "gender_group", "category"), "total_spending",sum)
    maxResult <- group_by_and_send_fun(data = result, group_cols = c("age_group", "gender_group"),"total_spending", max)
    matched_categories = filter_matched_categories(result, maxResult, group_cols = c("age_group", "gender_group", "category"))
    matched_categories$total_spending <- maxResult$total_spending
  }else{
    analiz_datasi <- temiz_joined_tablo[, c("id", "category","total_spending","country","traffic_source")]
    result = group_by_and_send_fun(data = analiz_datasi, group_cols = c("country", "traffic_source"), "total_spending",sum)
    minResult = group_by_and_send_fun(data = result, group_cols = c("country"),"total_spending", min)
    matched_categories = filter_matched_categories(result, minResult, group_cols = c("country", "traffic_source"))
    matched_categories$total_spending <- minResult$total_spending
  }


  return(matched_categories)
}



