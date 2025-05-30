#' Import special licence EHS household data - derived variables only
#' @param folder A folder containing EHS downloaded from UKDS and unzipped
#' @param years A list of years - the fewer and the more recent the less likely this is to return inconsistent data
#' @export

hh_derived <- function(folder, years){
  files <- list.files(folder, recursive = T)
  hh <- purrr::map_dfr(years, function(year){
    key <- dplyr::filter(sl_key, ehsyear == year & dataset == "household") # import key lookup table
    message("Starting to import data for year(s) selected")

    # import EHS datasets
    message("Importing survey files for ", year, "...")
    gen <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$general), sep = ""))
    int <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$interview), sep = ""))

    hh <- dplyr::left_join(gen, int, by = key$serial_number[1])

    hh <- labelled::to_factor(hh, strict = TRUE, sort_levels = "none") # convert text variables to factors

    # add or standardise variables
    hh$ehsyear <- year # create EHS year variable
    hh <- dplyr::rename(hh, region = key$region[key$ehsyear == year],
             hweight = key$h_weight[key$ehsyear == year],
             ethhrp2x = key$ethhrp2x[key$ehsyear == year],
             ethhrp4x = key$ethhrp4x[key$ehsyear == year],
             agehrp4x = key$agehrp4x[key$ehsyear == year],
             agehrp6x = key$agehrp6x[key$ehsyear == year],
             ndepchild = key$ndepchild[key$ehsyear == year],
             serial_number = key$serial_number[key$ehsyear == year])
    hh$serial_number <- as.character(hh$serial_number)
    hh <- hh %>% dplyr::mutate(london = dplyr::case_when(region == "London" ~ "London", TRUE ~ "Rest of England"))
  })
}

#' Import special licence EHS households data - including selected variables from detailed datasets
#' @param folder A folder containing EHS downloaded from UKDS and unzipped
#' @param years A list of years - the fewer and the more recent the less likely this is to return inconsistent data
#' @export

hh_detailed <- function(folder, years){
  files <- list.files(folder, recursive = T)
  hh <- purrr::map_dfr(years, function(year){
    key <- dplyr::filter(sl_key, ehsyear == year & dataset == "household") # import key lookup table

    # Import EHS datasets
    message("Importing survey files for ", year, "...")
    gen <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$general), sep = ""))
    int <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$interview), sep = ""))

    att <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$attitudes), sep=""))
    dwelling <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$dwelling), sep=""))
    renter <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$renter), sep=""))
    owner <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$owner), sep=""))
    hhldtype <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$hhldtype), sep=""))
    identity <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$identity), sep=""))
    if(file.exists(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$firstimp), sep=""))){firstimp <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$firstimp), sep=""))}
    if(file.exists(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$fire), sep=""))){fire <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$fire), sep=""))}
    if(file.exists(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$rooms), sep=""))){rooms <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$rooms), sep=""))}

    identity <- identity %>% dplyr::rename_with(tolower) %>% dplyr::filter(persno == hrp) # filter identity dataset to HRPs

    hh <- dplyr::left_join(gen, int, by = key$serial_number[1]) %>% # combine datasets
      dplyr::left_join(att %>% dplyr::select(-any_of(c("GorEHS"))), by = key$serial_number[1]) %>%
      dplyr::left_join(renter %>% dplyr::select(-any_of(c("GorEHS"))), by = key$serial_number[1]) %>%
      dplyr::left_join(owner %>% dplyr::select(-any_of(c("GorEHS"))), by = key$serial_number[1]) %>%
      dplyr::left_join(dwelling %>% dplyr::select(-any_of(c("GorEHS"))), by = key$serial_number[1]) %>%
      dplyr::left_join(hhldtype %>% dplyr::select(-any_of(c("GorEHS"))), by = key$serial_number[1]) %>%
      dplyr::left_join(identity %>% dplyr::select(-any_of(c("GorEHS"))), by = key$serial_number[1])

    if(exists("firstimp")){hh <- hh %>% dplyr::left_join(firstimp %>% dplyr::select(-any_of(c("GorEHS"))), by = key$serial_number[1])}
    if(exists("fire")){hh <- hh %>% dplyr::left_join(fire %>% dplyr::select(-any_of(c("GorEHS"))), by = key$serial_number[1])}
    if(exists("rooms")){hh <- hh %>% dplyr::left_join(rooms %>% dplyr::select(-any_of(c("GorEHS"))), by = key$serial_number[1])}

    hh <- hh %>% dplyr::select(-contains(c("quarter"))) # get rid of a rogue duplicated variable, if present

    hh <- labelled::to_factor(hh, strict = TRUE, sort_levels = "none") # convert text variables to factors

    # add or standardise variables
    hh$ehsyear <- year # create EHS year variable

    hh <- dplyr::rename(hh, region = key$region[key$ehsyear == year],
                        hweight = key$h_weight[key$ehsyear == year],
                        ethhrp2x = key$ethhrp2x[key$ehsyear == year],
                        ethhrp4x = key$ethhrp4x[key$ehsyear == year],
                        agehrp4x = key$agehrp4x[key$ehsyear == year],
                        agehrp6x = key$agehrp6x[key$ehsyear == year],
                        satten2 = key$satisfied_tenure[key$ehsyear == year],
                        ndepchild = key$ndepchild[key$ehsyear == year],
                        serial_number = key$serial_number[key$ehsyear == year])

    hh$serial_number <- as.character(hh$serial_number)

    hh <- dplyr::mutate(hh, london = dplyr::case_when(region == "London" ~ "London", TRUE ~ "Rest of England"))

    # ethnicity
    hh <- hh %>%
      dplyr::mutate(ethhrp2x = dplyr::case_when(
        ethhrp2x == "white" ~ "White",
        TRUE ~ "Non-White"
      ) %>% forcats::fct_relevel("Non-White", after = 1))

    hh$ethhrp4x <- hh$ethhrp4x %>% forcats::fct_relabel(~ gsub("Asian", "asian", .x))

    # age
    hh <- hh %>%
      dplyr::mutate(agehrp4x = dplyr::case_when(
        agehrp4x == "16 thru 29" ~ " 16 - 29",
        agehrp4x == "30 thru 44" ~ " 30 - 44",
        agehrp4x == "45 thru 64" ~ " 45 - 64",
        agehrp4x == "65 or over" ~ " 65 or over",
        TRUE ~ as.character(agehrp4x)
      ))

    # satisfaction with tenure
    hh <- hh %>%
      dplyr::mutate(satten2 = stringr::str_to_sentence(satten2)) %>%
      dplyr::mutate(satten2 = dplyr::case_when(
        satten2 == "Strongly agree" ~ "Very satisfied",
        satten2 == "Tend to agree" ~ "Fairly satisfied",
        satten2 == "Neither agree nor disagree" ~ "Neither satisfied nor dissatisfied",
        satten2 == "Tend to disagree" ~ "Slightly dissatisfied",
        satten2 == "Strongly disagree" ~ "Very dissatisfied",
        satten2 == "does not apply" ~ "Does not apply",
        satten2 == "no answer" ~ "No answer",
        satten2 == "No opinion (Spontaneous only)" ~ "No answer",
        TRUE ~ as.character(satten2)
      )) %>% dplyr::mutate(satten2 = as.factor(satten2)) %>%
      dplyr::mutate(satten2 = forcats::fct_relevel(satten2, "Very satisfied", "Fairly satisfied",
                                   "Neither satisfied nor dissatisfied", "Slightly dissatisfied"))

    # convert child variable to numeric
    hh$ndepchild <- as.double(hh$ndepchild)

    # household type
    hh$hhtype6 <- hh$hhtype6 %>% forcats::fct_relabel(~ gsub("households", "household", .x))

    # summary overcrowding variable
    hh <- hh %>% dplyr::mutate(crowd = forcats::fct_collapse(bedstdx,
                                             overcrowded = c("two or more below standard", "one below standard"),
                                             fine = c("at standard", "one above standard"),
                                             underocc = "two or more above standard"))
    # household income
    # note, this is unequivalised income, so the lowest-income group
    # is disproportionately composed of pensioners
    hh <- hh %>%
      dplyr::mutate(hhinc5x = dplyr::case_when(
        hhinc5x == "1" ~ "lowest 20%",
        hhinc5x == "2" ~ "quintile 2",
        hhinc5x == "3" ~ "quintile 3",
        hhinc5x == "4" ~ "quintile 4",
        hhinc5x == "5" ~ "highest 20%",
        TRUE ~ as.character(hhinc5x)
      ) %>% forcats::fct_relevel("highest 20%", after = 4))

    hh <- hh %>% dplyr::rename_with(tolower) # all variable names to lower case

    # tenure
    hh$tenex <- hh$tenex %>% forcats::fct_relabel(~ gsub("LA", "local authority", .x))

    # convert variable case if required
    hh <- hh |> dplyr::mutate(accom = stringr::str_to_sentence(accom))

    # create a summary of the previous tenure variable
    hh <- hh %>%
      dplyr::mutate(prevten_summary = dplyr::case_when(
        prevten == "council" ~ "social sector",
        prevten == "local authority" ~ "social sector",
        prevten == "housing association" ~ "social sector",
        TRUE ~ as.character(prevten)))

    # rename variables which are identical but have different names in some years
    # this code works whether or not the 'wrong' variables are actually present
    # the format used is old_name = "new_name"
    hh <- hh %>%
      dplyr::rename_with(dplyr::recode, bhcinc_1 = "bhcinceqv5")

    # For now let's only select the variables we want.
    hh <- hh %>% dplyr::select(any_of(c("ehsyear", "serial_number",
                                 "hweight", "region", "tenure3", "tenure1",
                                 "london", "tenure4x", "freeleas", "owntype",
                                 "hhsizex", "ndepchild", "nxdepch",
                                 "hhtype6", "hhtype11", "hhcompx", "hhcomp1",
                                 "famnumx", "nounits1", "otherfam", "dvocc",
                                 "famhless", # q about sofa surfers who would otherwise have been homeless in last year (added 2017/18)
                                 "tempemacc", # q about whether living in temporary or emergency accommodation (added 2016/17)
                                 "dvhidhh", #derived number of hidden households
                                 "accom", "hsetype", "flttyp",
                                 "yrbult1", "nbedsx", "dwage5x", "dwellnew",
                                 "bedrqx", "bedstdx", "crowd",
                                 "nstud", "hhempx", "emphrpx", "studhrp",
                                 "sexhrp", "sexprt",
                                 "ethhrp2x", "ethhrp4x", "ethhrp8x", "ethprt8x",
                                 "ethhrp4y", "ethhrp8y", # these replace their x counterparts from 2021 onwards, and differ only in that Chinese ethnicity is now included in 'Asian' rather than 'Other'
                                 "ntnlty", "cry01", "cryspec", "cryo", "camey2",
                                 "nshare", "nkita", "nbatha",
                                 "nrms2a", "shrms2a", "nrms5", "shbthwc", "hidany",
                                 "ftbuyer", "accomhh1", "prevtenlenb",
                                 "lenresb", "prevten", "prevten_summary",
                                 "prevnew", "prevlet", "miles", "prevac", "prevacn",
                                 "hhinc5x", "hyeargrx", "bhcinceqv5", "jointincx",
                                 "rentwkx", "rentexs", "mortwkx",
                                 "equityr5", "prptval1", "prptval2", "prpchng2",
                                 "housbenx", "amthbenx", "ahcinceqv60h", "Ldhsbam3",
                                 "mrgarr", "mrgar21", "morgaff", "mrgarn4", # mortgage affordability
                                 "pha2292", "arrpr1", "arrpr2", # rental affordability
                                 "agehrpx", "agehrp4x", "agehrp6x", "pyngx",
                                 "hhltsick", "hhwhch", # anyone in household long-term sick or disabled, or needing a wheelchair
                                 "hhinc5x", "tenure4", "tenex",
                                 "hsatis", "satten2", "has44",
                                 "qsatis", "qworth", "qhappy", "qanxious", "lonely",
                                 "srcmpcon", "prcmpcon",
                                 "lldsat", "lldsatre", "lldsatreas", # satisfaction with landlord and reason for
                                 "pha2292", "mrgar21", # easy/difficult to afford rent/mortgage
                                 "dischala", "discprs", # discrimination questions (2017/18 on)
                                 "everhless", # ever homeless (added 2016/17)
                                 "nhhmsf1", "nhsfnte",
                                 "floors", "frstimpn", "frstimpb",
                                 "anyfire", # an outbreak of fire of any sort in last 12 months (only asked in some years)
                                 "numala3", # number of smoke alarms (2017/18 on)
                                 "safetyhp2", # whether feels unsafe at home due to fear a fire will break out
                                 "tenemov3", "teneask3", "tenejob3", # why did tenancy end
                                 "tenemtag3", "tenefix3", "tenerel3",
                                 "teneinc3", "imd1004", "imd1010", "imd1510",
                                 "whyerent", "whyehb", "whyellor", # why evicted
                                 "whyeslus", "whyeprob", "whyeothr",
                                 "whymarea", "whymjob", "whymlge",
                                 "whymsml", "whymchp", "whymmrg",
                                 "whymdiv", "whymmar", "whymfmps",
                                 "whymbuy", "whymown", "whymnot",
                                 "whympoor", "whymschl", "whymusui",
                                 "whymllor", "whymothr", "mainr1",
                                 "wlist", "srbuy", "mov6mos", "wymov6mos1b", #mov6mos added in 2016/17, wymov6mos1b etc added in 2019/20
                                 "wymov6mos2b", "wymov6mos3b", "wymov6mos4b", "wymov6mos5b",
                                 "wymov6mos6b", "wymov6mos7b", "wymov6mos8b", "wymov6mos9b",
                                 "wymov6mos10b", "wymov6mos11b", "wymov6mos12b")))
  })
}

#' Import special licence EHS households data on concealed households - to be then matched against other imported data
#' @param folder A folder containing EHS downloaded from UKDS and unzipped
#' @param years A list of years - the fewer and the more recent the less likely this is to return inconsistent data
#' @min_age The minimum age of adults to be included
#' @income_filter Whether to exclude (by turning filter 'on') those for whom there is no income recorded
#' @definition If set to 'wide', includes those who are 'living here temporarily while looking for work' or who 'will soon be moving into own accommodation' and 'will soon be moving into own accommodation'
#' @export

hh_concealed <- function(folder, years, min_age, income_filter = "Off", definition = "narrow"){
  files <- list.files(folder, recursive = T)
  hh <- purrr::map_dfr(years, function(year){
    key <- dplyr::filter(sl_key, ehsyear == year & dataset == "household") # import key lookup table

    # Import EHS datasets
    message("Importing survey files for ", year, "...")
    gen <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$general), sep = ""))
    int <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$interview), sep = ""))
    people <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$people), sep=""))

    # generate data on concealed households
    # in the people dataset, identify those who would like their own housing
    # Filtered to individuals aged over 24, including non-dependent children,
    # other relatives and other individuals, whose reason for living in
    # the household is given, when `definition` = 'narrow', as 'Would like to buy or rent but can't afford
    # it at the moment' or 'Looking to buy/rent and expect(s) to find something affordable shortly'
    # When `definition` = 'wide', it also includes 'living here temporarily while looking for work',
    # 'will soon be moving into own accommodation' and 'will soon be moving into own accommodation'
    people <- people %>% dplyr::rename_with(tolower)

    people <- people %>% dplyr::rename_with(recode, whinform2 = "whinform")

    if(income_filter == "Off" & definition == "wide"){
      people <- people %>%
        mutate(leaver = dplyr::case_when(
          ((whinform %in% c(2:5)) & age >= min_age) ~ 1,
          TRUE ~ 0
        ))}

    if(income_filter == "On" & definition == "wide"){
      people <- people %>%
        mutate(leaver = dplyr::case_when(
          ((whinform %in% c(2:5)) & age >= min_age & teldv != -9) ~ 1,
          TRUE ~ 0
        ))}

    if(income_filter == "Off" & definition == "narrow"){
      people <- people %>%
        mutate(leaver = dplyr::case_when(
          ((whinform %in% c(3:4)) & age >= min_age) ~ 1,
          TRUE ~ 0
        ))}

    if(income_filter == "On" & definition == "narrow"){
      people <- people %>%
        mutate(leaver = dplyr::case_when(
          ((whinform %in% c(3:4)) & age >= min_age & teldv != -9) ~ 1,
          TRUE ~ 0
        ))}

    # find family number of 'leavers'
    people <- people %>%
      dplyr::mutate(leaver_fam = afam * leaver)

    # find sum of leaver individuals in household
    hhleave <- people %>%
      dplyr::group_by(dplyr::pick(key$serial_number[1])) %>%
      dplyr::tally(leaver)

    # find number of separate leaver family units in household
    # this took a while to work out!
    hhleavefam <- people %>%
      filter(leaver_fam > 0) %>%
      dplyr::group_by(dplyr::pick(c(key$serial_number[1], "leaver_fam"))) %>%
      dplyr::count() %>%
      dplyr::group_by(dplyr::pick(key$serial_number[1])) %>%
      dplyr::tally()

    # We now have the number of 'leaver' individuals in 'hhleave',
    # the number of family units containing a leaver in 'hhleavefam',
    # and can join this info onto the main household dataset

    hh <- left_join(gen, int, by = key$serial_number[1]) %>% # combine datasets
     left_join(hhleave, by = key$serial_number[1]) %>%
      rename(leaver_individuals = n) %>%
      left_join(hhleavefam, by = key$serial_number[1]) %>%
      rename(leaver_famunits = n) %>%
      tidyr::replace_na(list(leaver_famunits = 0))

    hh <- hh %>% select(-contains(c("quarter"))) # get rid of a rogue duplicated variable, if present

    hh <- labelled::to_factor(hh, strict = TRUE, sort_levels = "none") # convert text variables to factors

    hh$ehsyear <- year # create EHS year variable

    hh <- hh %>%
      rename(serial_number = key$serial_number[key$ehsyear == year])

    hh$serial_number <- as.character(hh$serial_number)

    # Select the variables of interest
    hh <- hh %>% select(any_of(c("ehsyear", "serial_number",
                                 "leaver_individuals", "leaver_famunits")))

  })
}

#' Import data on whether household members have impairments and have difficulties using wheelchairs, and join onto special licence data
#' @param folder A folder containing EHS downloaded from UKDS and unzipped
#' @param years A list of years - the fewer and the more recent the less likely this is to return inconsistent data
#' @export

hh_disabled <- function(folder, years){
  files <- list.files(folder, recursive = T)
  hh <- purrr::map_dfr(years, function(year){
    key <- dplyr::filter(sl_key, ehsyear == year & dataset == "household") # import key lookup table
    message("Starting to import data for year(s) selected")

    # import special licence household data using the existing function
    d <- hh_detailed(folder, year)

    # import person-level disability dataset
    disab <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$disability), sep=""))
    disab <- labelled::to_factor(disab, strict = TRUE, sort_levels = "none") # convert text variables to factors
    disab <- disab %>% dplyr::rename(serial_number = key$serial_number[key$ehsyear == year])

    # identify households where someone says they have a particular impairment
    check_mentioned <- function(var) {
      disab |>
        select(serial_number, persno, all_of(var)) |>
        tidyr::pivot_wider(names_from = persno, names_prefix = paste0(var),
                    values_from = var, id_cols = serial_number) |>
        mutate(mentioned = if_else(rowSums(across(starts_with(paste0(var))) == "Mentioned", na.rm = T) > 0,
                                   "Mentioned",
                                   if_else(rowSums(across(starts_with(paste0(var))) == "yes", na.rm = T) > 0,
                                           "Mentioned",
                                           if_else(rowSums(across(starts_with(paste0(var))) == "Not mentioned", na.rm = T) > 0,
                                                   "Not mentioned",
                                                   if_else(rowSums(across(starts_with(paste0(var))) == "no", na.rm = T) > 0,
                                                           "Not mentioned", NA))))) %>%
        select(-c(starts_with(paste0(var)))) |>
        rename(!! paste0(paste0(var), "_mentioned") := mentioned)
    }

    impairments_mentioned <- purrr::map_dfc(c( "DsVision2", "DsHearing2", "DsMoblty2", "DsDexterity2", "DsLrnDf2",
                                        "DsMemory2", "DsMental2", "DsStamina2", "DsSocial2", "DsOther2", "DsNo2"),
                                     check_mentioned) |>
      select("serial_number" = 1, ends_with("mentioned")) %>%
      mutate(serial_number = as.character(serial_number))

    # Whether people in the household who use a wheelchair have somewhere to put it
    if("WhChrSt" %in% colnames(disab)){
      wheelchair_place <-  disab |>
        select(serial_number, persno, WhChrSt) |>
        tidyr::pivot_wider(names_from = persno, names_prefix = "WhChrSt",
                    values_from = WhChrSt, id_cols = serial_number) %>%
        mutate(wheelchair_place = if_else(rowSums(across(starts_with("WhChrSt")) == "Yes", na.rm = T) > 0,
                                          "Yes",
                                          if_else(rowSums(across(starts_with("WhChrSt")) == "No", na.rm = T) > 0,
                                                  "No", NA))) %>%
        select(-c(starts_with("WhChrSt"))) %>%
        mutate(serial_number = as.character(serial_number))
    }

    # Whether anyone in the household finds wheelchair use in the home difficult
    wheelchair_difficult <- disab |>
      select(serial_number, persno, WHInside) |>
      tidyr::pivot_wider(names_from = persno, names_prefix = "WHInside",
                  values_from = WHInside, id_cols = serial_number) %>%
      mutate(wheelchair_difficult = if_else(rowSums(across(starts_with("WHInside")) == "Fairly difficult", na.rm = T) > 0,
                                            "Fairly difficult",
                                            if_else(rowSums(across(starts_with("WHInside")) == "Very difficult", na.rm = T) > 0,
                                                    "Very difficult",
                                                    if_else(rowSums(across(starts_with("WHInside")) == "Neither easy nor difficult", na.rm = T) > 0,
                                                            "Neither easy nor difficult",
                                                            if_else(rowSums(across(starts_with("WHInside")) == "Fairly easy", na.rm = T) > 0,
                                                                    "Fairly easy",
                                                                    if_else(rowSums(across(starts_with("WHInside")) == "Very easy", na.rm = T) > 0,
                                                                            "Very easy", NA)))))) %>%
      select(-c(starts_with("WHInside"))) %>%
      mutate(serial_number = as.character(serial_number))

    # join these onto the household dataset
    if(exists("wheelchair_place")){d <- d %>% left_join(wheelchair_place, by = c("serial_number"))}

    d <- d %>%
      left_join(impairments_mentioned, by = c("serial_number")) %>%
      left_join(wheelchair_difficult, by = c("serial_number"))

  })}


#' Import special licence EHS housing stock data - including selected variables from detailed datasets
#' @param folder A folder containing EHS downloaded from UKDS and unzipped
#' @param years A list of years - the fewer and the more recent the less likely this is to return inconsistent data
#' @export

hstock_detailed <- function(folder, years){
  files <- list.files(folder, recursive = T)
  hh <- purrr::map_dfr(years, function(year){
    key <- dplyr::filter(sl_key, ehsyear == year & dataset == "stock") # import key lookup table

    message("Importing survey files for ", year, "...")
    # Import stock datasets
    gen <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$general), sep = ""))
    int <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$interview), sep = ""))
    phy <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$physical), sep = ""))
    att <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$attitudes), sep=""))
    ins <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$interior), sep=""))
    aro <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$around), sep=""))
    if(file.exists(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$firstimp_int), sep=""))){firstimp_int <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$firstimp_int), sep=""))}
    if(file.exists(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$vacant), sep=""))){vac <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$vacant), sep=""))}
    if(file.exists(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$firstimp_phy), sep=""))){firstimp_phy <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$firstimp_phy), sep=""))}
    if(file.exists(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$shared), sep=""))){shared <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$shared), sep=""))}
    if(file.exists(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$shape), sep=""))){shape <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$shape), sep=""))}
    if(file.exists(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$numflats), sep=""))){numflats <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$numflats), sep=""))}
    if(file.exists(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$modelled_physical), sep=""))){modelled_physical <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$modelled_physical), sep=""))}

    stock <- left_join(gen, int, by = key$serial_number[1]) %>%
      left_join(phy %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1]) %>%
      left_join(att %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1]) %>%
      left_join(ins %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1]) %>%
      left_join(aro %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1]) # combine datasets

    if(exists("vac")){stock <- stock %>% left_join(vac %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1])}
    if(exists("mod_phy")){stock <- stock %>% left_join(mod_phy %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1])}
    if(exists("firstimp_phy")){stock <- stock %>% left_join(firstimp_phy %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1])}
    if(exists("firstimp_int")){stock <- stock %>% left_join(firstimp_int %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1])}
    if(exists("shared")){stock <- stock %>% left_join(shared %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1])}
    if(exists("shape")){stock <- stock %>% left_join(shape %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1])}
    if(exists("numflats")){stock <- stock %>% left_join(numflats %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1])}

    stock <- labelled::to_factor(stock, strict = TRUE) # convert text variables to factors

    stock$ehsyear <- year # create EHS year variable

    # standardise variable names
    stock <- stock %>%
      rename(region = key$region[key$ehsyear == year],
             hweight = key$h_weight[key$ehsyear == year],
             dweight = key$d_weight[key$ehsyear == year],
             ethhrp2x = key$ethhrp2x[key$ehsyear == year],
             agehrp4x = key$agehrp4x[key$ehsyear == year],
             agehrp6x = key$agehrp6x[key$ehsyear == year],
             ndepchild = key$ndepchild[key$ehsyear == year],
             serialanon = key$serial_number[key$ehsyear == year])

    # ethnicity
    stock <- stock %>%
      mutate(ethhrp2x = case_when(
        ethhrp2x == "white" ~ "White",
        TRUE ~ "Non-White"
      ) %>% forcats::fct_relevel("Non-White", after = 1))

    # hrp age
    stock <- stock %>%
      mutate(agehrp4x = case_when(
        agehrp4x == "16 thru 29" ~ " 16 - 29",
        agehrp4x == "30 thru 44" ~ " 30 - 44",
        agehrp4x == "45 thru 64" ~ " 45 - 64",
        agehrp4x == "65 or over" ~ " 65 or over",
        TRUE ~ as.character(agehrp4x)
      ))

    # household type
    stock$hhtype6 <- stock$hhtype6 %>% forcats::fct_relabel(~ gsub("households", "household", .x))

    # household income
    # note, this is unequivalised income, so the lowest-income group
    # is disproportionately composed of pensioners
    stock <- stock %>%
      mutate(hhinc5x = case_when(
        hhinc5x == "1" ~ "lowest 20%",
        hhinc5x == "2" ~ "quintile 2",
        hhinc5x == "3" ~ "quintile 3",
        hhinc5x == "4" ~ "quintile 4",
        hhinc5x == "5" ~ "highest 20%",
        TRUE ~ as.character(hhinc5x)
      ) %>% forcats::fct_relevel("highest 20%", after = 4))

    # levels of bedroom standard
    stock <- stock %>%
      mutate(bedstdx = forcats::fct_relevel(bedstdx, "two or more above standard",
                                   "one above standard",
                                   "at standard",
                                   "one below standard"))

    # number of adults - calculated as household size minus number of dependent children
    stock <- stock %>%
      mutate(nadults = hhsizex - ndepchild)

    # summary crowding variable
    stock <- stock %>% mutate(crowd = forcats::fct_collapse(bedstdx,
                                                   overcrowded = c("two or more below standard", "one below standard"),
                                                   fine = c("at standard", "one above standard"),
                                                   underocc = "two or more above standard"))

    # create three-way tenure variable
    stock <- stock %>%
      mutate(tenure3 = case_when(
        tenure4x == "owner occupied" ~ "Owner occupied",
        tenure4x == "private rented" ~ "Private rented",
        TRUE ~ "Social housing"
      ))


    # create London/non-London variable
    stock <- stock %>% mutate(london = case_when(
      region == "London" ~ "London",
      TRUE ~ "Rest of England"
    ))

    stock <- stock %>% dplyr::rename_with(tolower) # all variable names to lower case

    # rent variable
    stock$rentwkx <- as.double(stock$rentwkx)
    stock$rentwkx[stock$rentwkx == 88888888] <- NA # deal with missing labels
    stock$rentwkx[stock$rentwkx == 99999999] <- NA # deal with missing labels

    # actual construction year variable
    stock <- stock %>% mutate(across(matches("fodconac"), as.double))

    stock$serialanon <- as.character(stock$serialanon)

    # For now let's only select the variables we want.
    stock <- stock %>% select(any_of(c("serialanon", "ehsyear", "region", "london",
                                       "hweight", "dweight", "hsatis",
                                       "dhomesy", "dhhhsrsx", "dhhhsrsy",
                                       "dhthermy", "dhdisrx", "dhmodx", "dhcosty",
                                       "dampalf", "vacantx", "vaclngth", "ru11combin",
                                       "tenure4x", "floorx", "storeyx",
                                       "hhsizex", "nadults", "ndepchild", "hhltsick",
                                       "hhtype6", "bedstdx", "ethhrp2x", "ethhrp4x", "ethhrp8x",
                                       "agehrp4x", "tenure3", "hhinc5x", "bhcinceq", "bhcinceqv5",
                                       "dwage5x", "dwage6x", "dwage7x", "dwage10x",
                                       "fodconst", "fodconac",
                                       "dwtype3x", "dwtype7x", "dwtype8x", "constx", "typerstr",
                                       "fexplotf", "fexplotr", "fexwidth", "fexp1fdp", "fexp2fdp",
                                       "fdhmdep1", "fdhmwid1", "fnoflats",
                                       "cstactbx", "cststdbx", # basic repair costs (total and per m2)
                                       "cstactcx", "cststdcx", # comprehensive repair costs
                                       "cstactux", "cststdux", # urgent repair costs
                                       "hhwhch", "imd1510",
                                       "epceeb12e", "sap12", "rentwkx", "amthbenx",
                                       "heat7x",
                                       "frstimpn", "frstimpb",
                                       "fexdstep", "finflush", "finbeden", "finbaten", "finwcen",
                                       "finwawen", "fintrpen", "fincircu", "finlands", "ffcastep", "ffcshare",
                                       "finramps", "fingrabr", "finlifts", "finhoist", "finelecm",
                                       "vnstdrdm", "vncrnton", "vnstrnmd", "vnstsdno", "vnstawnw", "fodoccup")))

})}
