#' @title Prepare gear list to convert Norwegian gear codes to MFDB
#' @description Prepares a gear check list using the IMR database API and an Excel file from the Directorate of Fisheries (FDIR).
#' @param fdirPath Character string defining the path to the FDIR Excel file containing the gear codes on your computer. Download the FDIR Excel file from \href{https://www.fiskeridir.no/Yrkesfiske/Rapportering-ved-landing/Kodeliste}{here}.
#' @param imrPath Character string defining the API URL containing IMR gear codes.
#' @details This function compiles all IMR Biotic and FDIR Landings gear codes and allocates them to general categories. The categorization is written for Greenland halibut and may have to be modified for other species.
#'
#' The function has been written for \href{https://www.fiskeridir.no/Yrkesfiske/Rapportering-ved-landing/Kodeliste}{the code list Excel sheet} published on 2021-03-26. You may have to adjust the function depending on changes in newer versions of the file.
#' @author Mikko Vihtakari, Ibrahim Umar

# fdirPath = "/data/in/Kodeliste_landing_20210326.xlsx"; imrPath = "http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/equipment?version=2.0"
gears2MFDB <- function(fdirPath, imrPath = "http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/equipment?version=2.0") {

  # IMR codes ####

  ## Read gear list reference
  doc <- xml2::read_xml(imrPath)

  tmp <- lapply(xml2::xml_find_all(doc, "//d1:row"), function(x) {
    ch <- xml2::xml_children(x)
    y <- xml2::xml_text(ch)
    z <- xml2::xml_name(ch)
    names(y) <- z
    return(data.frame(t(y)))
  })

  ## Bind to data table
  gearList <- tibble::as_tibble(dplyr::bind_rows(tmp))

  ## Gear categories
  tmp <- ifelse(nchar(gearList$code) != 4, NA, gearList$code)
  tmp <- substr(tmp, 1,2)
  tmp[tmp %in% 10:11] <- "WaterSamplers"
  tmp[tmp %in% 20:21] <- "PlanktonNets"
  tmp[tmp %in% 22] <- "Pumps"
  tmp[tmp %in% 23] <- "TowNets"
  tmp[tmp %in% c(24, 44)] <- "Scrapes"
  tmp[tmp %in% 25] <- "Grabs"
  tmp[tmp %in% 30:31] <- "BottomTrawls"
  tmp[tmp %in% 30:32] <- "ShrimpTrawls"
  tmp[tmp %in% 34] <- "OtherTrawls"
  tmp[tmp %in% 35] <- "PelagicTrawls"
  tmp[tmp %in% 36] <- "DanishSeines"
  tmp[tmp %in% 37] <- "Seines"
  tmp[tmp %in% 40:41] <- "Gillnets"
  tmp[tmp %in% c(42, 43, 53)] <- "Traps"
  tmp[tmp %in% 50] <- "OtherLines"
  tmp[tmp %in% 51] <- "Longlines"
  tmp[tmp %in% 52] <- "Handlines"
  tmp[tmp %in% 60:90] <- "Other"
  tmp[is.na(tmp)] <- "Other"

  gearList$gearcategory <- tmp

  ## Column order and clean-up

  gearList <- gearList %>%
    dplyr::rename(gearname = name) %>%
    dplyr::mutate(source = "IMR", code = as.integer(code)) %>%
    dplyr::select(-area) %>%
    dplyr::relocate(code, gearname, gearcategory, source, description)

  # FDIR codes

  dt <- suppressMessages(readxl::read_xlsx(path = fdirPath, sheet = "A7-Redskap", skip = 8, col_names = FALSE))
  dt <- dt[,1:2]
  colnames(dt) <- c("code", "gearname")
  dt <- dt[!is.na(dt$code),]

  ## Gear categories
  tmp <- dt$code

  tmp[grepl("^1", tmp)] <- "Seines"
  tmp[grepl("^2", tmp)] <- "Gillnets"
  tmp[tmp %in% 30] <- "OtherLines"
  tmp[tmp %in% 31] <- "Longlines"
  tmp[tmp %in% 32] <- "OtherLines"
  tmp[tmp %in% 33:34] <- "Handlines"
  tmp[tmp %in% 35] <- "Longlines"
  tmp[grepl("^4", tmp)] <- "Traps"
  tmp[tmp %in% 50] <- "OtherTrawls"
  tmp[tmp %in% 51:52] <- "BottomTrawls"
  tmp[tmp %in% 53:54] <- "PelagicTrawls"
  tmp[tmp %in% 55] <- "ShrimpTrawls"
  tmp[grepl("^5", tmp)] <- "OtherTrawls"
  tmp[tmp %in% 61] <- "DanishSeines"
  tmp[grepl("\\d\\d", tmp)] <- "Other"

  dt$gearcategory <- tmp
  dt$source <- "FDIR"
  dt$code <- as.integer(dt$code)

  # Combine ####

  gearList <- dplyr::bind_rows(gearList, dt) %>% dplyr::arrange(code)

  # Add major category

  tmp <- gearList$gearcategory

  tmp[grepl("trawls", tmp, ignore.case = TRUE)] <- "Trawls"
  tmp[grepl("lines", tmp, ignore.case = TRUE)] <- "Lines"
  tmp[grepl("seines", tmp, ignore.case = TRUE)] <- "Seines"
  tmp[tmp %in% c("WaterSamplers", "PlanktonNets", "Pumps", "TowNets", "Scrapes", "Grabs")] <- "Scientific"

  ord <- c("Lines", "Trawls", "DanishSeines", "Gillnets", "Seines", "Other", "Traps", "Scientific")
  rest <- unique(tmp)[!unique(tmp) %in% ord]

  tmp <- factor(tmp, levels = c(ord, rest))

  gearList$majorcategory <- tmp

  # Return

  return(gearList)
}
