
#' @title generate_dummy_hla_imputed
#' @description FUNCTION_DESCRIPTION
#' @param vision_level_data_version PARAM_DESCRIPTION, Default: 'R12v1'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: HLA imputed dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate group_by ungroup rowwise select arrange
#' @importFrom tidyr unnest
#' @importFrom stringr str_c
generate_dummy_hla_imputed_data <- function(
    hla_imputed_level_data_version="R12v1",
    n_patients_minimum = 100,
    seed=13
) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  # process HLA imputed data
  hla_imputed_tables <- summary_data_versions_list[[hla_imputed_level_data_version]]
  hla_imputed_data <- hla_imputed_tables$hla_imputed$ScanReport_hla_imputed

  # Idea is to use the n_subjects column to generate HLAcode alleles for each patient
  # The allele will be randomly assigned to the patients

  # Genrate 100 FINNGENIDs
  FINNGENID <- stringr::str_c("FG", stringr::str_pad((1:n_patients_minimum), width = 8, pad = "0"))

  # Assign random alleles for each HLAcode based on the n_subjects
  hla_imputed_data <- hla_imputed_data |>
    dplyr::mutate(n_subjects = as.double(n_subjects)) |>
    dplyr::group_by(HLAcode) |>
    dplyr::mutate(
        total = max(n_subjects),
        prob = n_subjects / total
    ) |>
    dplyr::ungroup() |>
    dplyr::rowwise() |>
    dplyr::mutate(
        ids = list(sample(FINNGENID, size = round(prob * 100), replace = FALSE))
    ) |>
    tidyr::unnest(ids) |>
    dplyr::select(FINNGENID = ids, Allele, HLAcode) |>
    dplyr::arrange(FINNGENID, HLAcode, Allele)

  return(hla_imputed_data)

}
