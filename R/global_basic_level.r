# The "basic_level" column in the dataset collapses version of the same word:
# "doggie", "dogs", and "dog" get mapped to "dog". This is done at the recording
# level. Here, we map objects to a *global* basic level version at the corpus
# level.
#
# There are three sources used for this mapping:
# 1. object_dict that maps `object` -> `global_bl`. Additionally, it maps
#    (`object`, `disambiguate`) -> `global_bl` when the object is ambiguous.
#    When it is not, the `disambiguate` column is NA.
# 2. annotid_disambiguation that maps `annotid` -> `disambiguate`. To avoid the
#    situation where `object` was updated, `object` is mathed too.
# 3. Some of the "-> `disambiguate`" mapping can be done on the basis of
#    `object`-`basic_level` combination, this is done directly in the code.
#
# The mapping is done in two steps:
# - The `disambiguate` column is filled using sources 2 and 3.
# - object_dict is used to map both ambigous and unambigous (`disambiguate` is
#   NA)



read_object_dict <- function(object_dict_path) {
  # Loads and checks the dictionary that maps objects (possibly disambiguated)
  # to their global basic level.

  object_dict <- object_dict_path %>%
    readr::read_csv(
      col_types = readr::cols(
        object = readr::col_character(),
        disambiguate = readr::col_character(),
        global_bl = readr::col_character()
      ))

  # Any object with multiple rows must have no NAs in the `disambiguate` column.
  # One NA would act as "else" condition, multiple NAs would result in duplicate
  # rows matched to the same token (annotid).
  object_dict %>%
    dplyr::group_by(object) %>%
    dplyr::mutate(count = dplyr::n(),
                  count_NA = sum(is.na(disambiguate))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(count > 1 & count_NA > 0) %>%
    nrow %>%
    assertthat::are_equal(0)

  # There should be no `object`-`disambiguate` duplicates
  object_dict %>%
    count(object, disambiguate, name = 'count') %>%
    dplyr::ungroup() %>%
    dplyr::filter(count > 1) %>%
    nrow %>%
    assertthat::are_equal(0)

  object_dict
}


read_annotid_disambiguation <- function(annotid_disambiguation_path) {
  # Loads and checks the dictionary mapping annotids to their disambiguation
  # variant.
  annotid_disambiguation <- annotid_disambiguation_path %>%
    readr::read_csv(
      col_select = c(annotid, object, disambiguate),
      col_types = readr::cols(
        object = readr::col_character(),
        annotid = readr::col_character(),
        disambiguate = readr::col_character()
      ))

  annotid_disambiguation %>%
    count(annotid, name = 'count') %>%
    dplyr::ungroup() %>%
    dplyr::filter(count > 1) %>%
    nrow %>%
    assertthat::are_equal(0)

  annotid_disambiguation
}


make_global_basic_level <- function(all_basiclevel_na,
                                    object_dict,
                                    annotid_disambiguation) {
  # Check that there are rows with NA in the basic_level column
  assertthat::assert_that(sum(is.na(all_basiclevel_na$basic_level)) > 0)

  # Check that the inputs have expected columns (all_basiclevel_na is checked
  # implicitly via dplyr::select later)
  assertthat::assert_that(identical(
    sort(colnames(object_dict)),
    c("disambiguate", "global_bl", "object")
  ))
  assertthat::assert_that(identical(
    sort(colnames(annotid_disambiguation)),
    c("annotid", "disambiguate", "object")
  ))

  # Disambiguate first based on annotid and then based on
  with_global_bl <- all_basiclevel_na %>%
    dplyr::select(annotid, object, basic_level) %>%
    dplyr::left_join(annotid_disambiguation, by = c("annotid", "object")) %>%
    dplyr::mutate(disambiguate = dplyr::case_when(
      object == "balls" & basic_level == "ball" & is.na(disambiguate) ~ "toy",
      object == "Momo" & basic_level == "Momo" & is.na(disambiguate) ~ "dog",
      object == "glasses" & basic_level == "glasses" & is.na(disambiguate) ~ "eye",
      TRUE ~ disambiguate))

  # Match to the global basic value
  with_global_bl <- with_global_bl %>%
    dplyr::left_join(object_dict, by = c("object", "disambiguate"))
  # Check that we didn't lose any tokens, didn't create any duplicates,
  assertthat::are_equal(all_basiclevel_na$annotid, with_global_bl$annotid)

  # and found global_basic_level for all tokens
  # without_global_bl_count <- sum(is.na(with_global_bl$global_bl))
  # warning(glue::glue('There were {without_global_bl_count} tokens for which we
  #                     couldn\'t assign global basic level.'))

  return(list(with_global_bl = with_global_bl))
}
