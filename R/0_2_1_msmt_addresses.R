#' Download and prepare data with school addresses

#' This function downloads data from the address book of all school and educational facilities in Czech Republic (Rejstříku škol a školských zařízení)
#'
#' @param NUTS3 A character vector, containing either selected NUTS3 region identifiers, or a single character 'all' (default) for the entire country.
#'
#' @return A tibble, containing addresses and other information about schools and educational facilities, on the level of p_izo
#'
#' @examples
#' data_addresses_PRAGUE <- msmt_addresses(NUTS3 = "CZ010")
#'
#' @importFrom xml2 read_xml as_list
#' @importFrom dplyr as_tibble select all_of bind_rows mutate mutate_all rename everything
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom tibble as_tibble_col
#'
#' @export

msmt_addresses <- function(NUTS3 = "all"){

  set_utf8()

  NUTS3_all <- c("CZ010", "CZ020", "CZ031",
                 "CZ032", "CZ041", "CZ042",
                 "CZ051", "CZ052", "CZ053",
                 "CZ063", "CZ064", "CZ071",
                 "CZ072", "CZ080")

  if(!((all(NUTS3 %in% NUTS3_all)) | (length(NUTS3) == 1) & NUTS3[1] == "all")){
    stop(paste0("All elements of argument 'NUTS3' must be one of the following: ", paste0("'", NUTS3_all, "'", collapse = ", "),", or the argument must be 'all'"))
  }

  if((length(NUTS3) == 1) & NUTS3[1] == "all"){
    url_append <- "vrejcelk.xml"
  }else{
    url_append <- paste0("VREJ", NUTS3[NUTS3 %in% NUTS3_all], ".xml")
  }

  rej_url <- paste0(MSMT:::rej, url_append)

  cat("\n\nReading addresses, this might take a while...\n\n")

  data_rej <- lapply(seq_along(rej_url), function(x){
    temp_nuts <- NUTS3[x]

    cat(paste0("\nDownloading '", temp_nuts, "'\n"))

    step_1 <- read_xml(rej_url[x]) %>%
      as_list() %>%
      as_tibble()

    cat(paste0("\n\nUnnesting '", temp_nuts, "' [1/3]"))

    suppressMessages({step_2 <- step_1 %>%
      unnest_wider(all_of("ExportDat")) %>%
      .[-1,-1] %>%
      mutate(RedIzo = unlist(.data$RedIzo),
             ICO = unlist(.data$ICO)) %>%
      unnest_wider(all_of("Reditelstvi"))})

    cat(paste0("\nUnnesting '", temp_nuts, "' [2/3]"))

    step_3 <- step_2 %>%
      mutate(RedPlnyNazev = unlist(all_of("RedPlnyNazev"))) %>%
      unnest_wider(all_of("Reditel")) %>%
      unnest_longer(all_of("Zrizovatele")) %>%
      unnest_wider(all_of("Zrizovatele"))

    cat(paste0("\nUnnesting '", temp_nuts, "' [3/3]\n"))

    step_4 <- step_3 %>%
      unnest_longer(all_of("SkolyZarizeni")) %>%
      unnest_wider(all_of("SkolyZarizeni")) %>%
      unnest_longer(all_of("SkolaMistaVykonuCinnosti")) %>%
      unnest_wider(all_of("SkolaMistaVykonuCinnosti")) %>%
      select(-all_of(c("StatutarniOrgany", "SkolaOboryVzdelani", "SkolaOboryVzdelaniZUS"))) %>%
      mutate_all(function(x){ifelse(lengths(x) == 0, NA, x)}) %>%
      mutate_all(unlist) %>%
      rename(red_izo = .data$RedIzo,
             ico = .data$ICO,
             izo = .data$IZO,
             p_izo = .data$IDMista) %>%
      select(all_of(c("red_izo", "izo", "p_izo", "ico")), everything())

    return(step_4)
  }) %>%
    bind_rows()

  outputs <- data_rej

  return(outputs)
}
