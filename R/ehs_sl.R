#' Import special licence EHS data - derived variables only
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

#' Import special licence EHS data - including selected variables from detailed datasets
#' @param folder A folder containing EHS downloaded from UKDS and unzipped
#' @param years A list of years - the fewer and the more recent the less likely this is to return inconsistent data
#' @export

hh_detailed <- function(folder, years){
  files <- list.files(folder, recursive = T)
  hh <- purrr::map_dfr(years, function(year){
    key <- dplyr::filter(sl_key, ehsyear == year & dataset == "household") # import key lookup table

    # Import EHS datasets
    gen <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$general), sep = ""))
    int <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$interview), sep = ""))

    att <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$attitudes), sep=""))
    dwelling <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$dwelling), sep=""))
    renter <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$renter), sep=""))
    owner <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$owner), sep=""))
    hhldtype <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$hhldtype), sep=""))
    identity <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$identity), sep=""))
    if(file.exists(paste(folder, key$firstimp_int,sep=""))){firstimp <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$firstimp), sep=""))}
    if(file.exists(paste(folder, key$fire,sep=""))){fire <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$fire), sep=""))}
    if(file.exists(paste(folder, key$rooms,sep=""))){rooms <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$rooms), sep=""))}

    identity <- identity %>% dplyr::rename_with(tolower) %>% dplyr::filter(persno == hrp) # filter identity dataset to HRPs

    hh <- dplyr::left_join(gen, int, by = key$serial_number[1]) %>% # combine datasets
      dplyr::left_join(att %>% dplyr::select(-any_of(c("GorEHS"))), by = key$serial_number[1]) %>%
      #     left_join(fire %>% select(-any_of(c("GorEHS"))), by = key$serial_number[1]) %>%
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
                                 "ntnlty", "cry01", "cryspec", "cryo", "cameyr",
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
