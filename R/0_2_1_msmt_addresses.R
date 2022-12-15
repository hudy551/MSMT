#' Get data with school addresses

#' This function downloads data from the address book of all school and educational facilities in Czech Republic (Rejstříku škol a školských zařízení)
#' 
#' @return A tibble, containing addresses and other information about schools and educational facilities, on the level of p_izo
#' 
#' @examples 
#' data_addresses <- msmt_addresses()

msmt_addresses <- function(){
  set_utf8()
  
  require(xml2)
  require(tidyr)
  require(dplyr)
  require(tibble)
  
  
  rej_url <- "https://rejstriky.msmt.cz/opendata/vrejcelk.xml"
  
  cat("\n\nReading addresses, this might take a while...\n\n")
  
  data_rej <- read_xml(rej_url) %>%
    as_list() %>%
    as_tibble() %>%
    unnest_wider(ExportDat) %>%
    .[-1,-1] %>%
    mutate(RedIzo = unlist(RedIzo),
           ICO = unlist(ICO)) %>%
    unnest_wider(Reditelstvi) %>%
    mutate(RedPlnyNazev = unlist(RedPlnyNazev)) %>%
    unnest_wider(Reditel) %>%
    unnest_longer(Zrizovatele) %>%
    unnest_wider(Zrizovatele) %>%
    unnest_longer(SkolyZarizeni) %>%
    unnest_wider(SkolyZarizeni) %>%
    unnest_longer(SkolaMistaVykonuCinnosti) %>%
    unnest_wider(SkolaMistaVykonuCinnosti) %>%
    select(-StatutarniOrgany, -SkolaOboryVzdelani, -SkolaOboryVzdelaniZUS) %>%
    mutate_all(function(x){ifelse(lengths(x) == 0, NA, x)}) %>%
    mutate_all(unlist) %>%
    rename(red_izo = RedIzo,
           ico = ICO,
           izo = IZO,
           p_izo = IDMista) %>%
    select(red_izo, izo, p_izo, ico, everything())
  
  outputs <- data_rej
  
  return(outputs)
}
