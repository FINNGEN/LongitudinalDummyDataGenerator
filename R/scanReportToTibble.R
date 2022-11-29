



scanReportToTibble <- function(scan_report, n_rows) {

  res_tibble <- tibble::tibble(FINNGENID = stringr::str_c("FG", stringr::str_pad((1:n_rows), width = 8, pad = "0")))

  for (col in 1:(ncol(scan_report)/2)) {

    variable <- scan_report |>
      select({col*2-1}, {col*2})

    variable <-  variable |> dplyr::filter(!is.na(!!rlang::sym(names(variable)[2])))

    variable_name <- names(variable)[1]

    if(nrow(variable)==0){
      rand_values <- rep(as.character(NA), n_rows)
    }else{
      rand_values <- sample(variable[[1]], n_rows, replace = TRUE, prob = variable[[2]] )
    }

    res_tibble <- dplyr::bind_cols(
      res_tibble,
      tibble::tibble(!!rlang::sym(variable_name):= rand_values )
    )

  }

  return(res_tibble)

}
