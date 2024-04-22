rp.wrangle <- function(name) {
   
   datasets <- matrix(c(
      "cofe_2019",     "collated data on giving in the Church of England in 2019",
      "UN_demography", "collated data from UN web sites"
      ), ncol = 2, byrow = TRUE)
   datasets <- as.data.frame(datasets)
   names(datasets) <- c("name", "description")
   
   if(missing(name)) {
      print(datasets)
      return(invisible(datasets))
   }

   if (!requireNamespace("readxl", quietly = TRUE))
      stop("the readxl package is not available.")
   if (!requireNamespace("readr", quietly = TRUE))
      stop("the readr package is not available.")
   if (!requireNamespace("dplyr",  quietly = TRUE))
      stop("the dplyr package is not available.")
   if (length(name) != 1)
      stop("name should be of length 1.")
   if (!is.character(name))
      stop("name should be a character string.")
   
   if (!(name %in% datasets[ , 1]))
      stop("name is not recognised.")
   
   if (name == "cofe_2019") {
      # Locate the paths to the datasets
      path.attend <- rp.datalink('cofe_attendance_2019')
      path.giving <- rp.datalink('cofe_giving_2019')
      path.depriv <- rp.datalink('cofe_deprivation_2019')
   
      # Read the Excel spreadsheets, select variables and summarise as required
      d.elect <- readxl::read_excel(path.attend, sheet = 5, range = 'B4:I47')  |>
         dplyr::select(Diocese = 1, Elect = 3, Worship = 6)
      d.attend <- readxl::read_excel(path.attend, sheet = 6, range = 'B4:D47') |>
         dplyr::select(Diocese = 1, Attend = 3)
      d.giving <- readxl::read_excel(path.giving, sheet = 3, range = 'B8:BS49') |>
         dplyr::select(Diocese = 1, Giving = 19, Givers = 59)
      d.depriv <- readxl::read_excel(path.depriv, sheet = 2, range = 'A1:AK12408') |>
         dplyr::select(Diocese = 11, population = 13, IMD = 37) |>
         dplyr::group_by(Diocese) |>
         dplyr::summarise(IMD = sum(IMD * population, na.rm = TRUE) /
                         sum(population, na.rm = TRUE),
                   population = sum(population, na.rm = TRUE))
   
      # make the diocese names consistent
      d.attend <- dplyr::mutate(d.attend, Diocese = dplyr::recode(Diocese,
                         'St. Albans' = 'St.Albans',
                         'St. Edms & Ipswich' = 'St.Edmundsbury & Ipswich'))
      d.elect  <- dplyr::mutate(d.elect, Diocese = dplyr::recode(Diocese,
                         'St. Albans' = 'St.Albans',
                         'St. Edms & Ipswich' = 'St.Edmundsbury & Ipswich'))
      d.giving <- dplyr::mutate(d.giving, Diocese = dplyr::recode(Diocese,
                         'Sodor and Man' = 'Sodor & Man'))
   
      # merge the datasets
      d <- d.attend |>
         dplyr::full_join(d.elect,  by = 'Diocese') |>
         dplyr::full_join(d.depriv, by = 'Diocese') |>
         dplyr::full_join(d.giving, by = 'Diocese') |>
         dplyr::mutate(Attachment = Attend / population,
                       Giving_per_member = Giving / Elect)
   }
   
   if (name == "UN_demography") {
      path <- rp.datalink("UN_demography")
      path <- unz(path, "WPP2022_Demographic_Indicators_Medium.csv")
      demog <- readr::read_csv(path) |>
         dplyr::filter(LocTypeName == "Country/Area") |>
         dplyr::select("LocID", "Location",
                "Year" = "Time", "Population" = "TPopulation1Jan",
                "Fertility" = "TFR", "Life_Expectancy" = "LEx") |>
         dplyr::mutate(Population = Population / 1000)
      
      path <- rp.datalink("UN_demography_metadata")
      nms <- readxl::read_excel(path, sheet = "Overall", skip = 16) |>
         dplyr::select("LocID", "Continent" = "GeoRegName") |>
         dplyr::distinct() |>
         dplyr::mutate(Continent = dplyr::case_match(Continent,
                                       "Latin America and the Caribbean" ~ "LA and Car.",
                                       "Northern America" ~ "N. America",
                                       .default = Continent))
      
      # anti_join(demog, nms, "LocID")
      d <- dplyr::left_join(demog, nms, "LocID")
   }
   
   invisible(d)
}
