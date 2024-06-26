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

# Placeholder for values that require manual editing
FIXME <- '***FIX ME***'


#' Checks object_dict for correct columns and no duplicates
#'
#' In case it was not read with `read_object_dict`
#' @noRd
check_object_dict <- function(object_dict) {
  assertthat::assert_that(identical(
    sort(colnames(object_dict)),
    c("disambiguate", "global_bl", "object")
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
    {assertthat::assert_that(nrow(.) == 0)}

  # There should be no `object`-`disambiguate` duplicates
  object_dict %>%
    dplyr::count(object, disambiguate, name = 'count') %>%
    dplyr::ungroup() %>%
    dplyr::filter(count > 1) %>%
    {assertthat::assert_that(nrow(.) == 0)}
}


#' Check annotid_disambiguation for correct columns and unique annotids
#'
#' In case it was not read with `read_annotid_disambiguation`
#' @noRd
check_annotid_disambiguation <- function(annotid_disambiguation) {
  assertthat::assert_that(identical(
    sort(colnames(annotid_disambiguation)),
    c("annotid", "disambiguate", "object")
  ))

  annotid_disambiguation %>%
    dplyr::count(annotid, name = 'count') %>%
    dplyr::ungroup() %>%
    dplyr::filter(count > 1) %>%
    {assertthat::assert_that(nrow(.) == 0)}
}


#' Checks if annot_disambiguation is up-to-date
#'
#' 1. Are all (annotid, object) still present in all_basiclevel_na?
#' 2. Are all tokens that need to be disambiguated - are?
#'
#' Returns a list with three keys:
#'
#' - n_need_disambiguation - number of non-disambiguated tokens,
#' - n_non_matched - number of (annotid, object) that no longer exist
#' - annotid_disambiguation:
#'   - NULL if both of the above numbers are zero,
#'   - annotid_disambiguation with n_non_matched rows removed and
#'     n_need_disambiguation rows added with a placeholder (constant FIXME)
#'     in the "disambiguate" column.
#'
#' @noRd
update_annotid_disambiguation <- function(all_basiclevel_na,
                                         object_dict,
                                         annotid_disambiguation) {
  # Remove annotids that no longer match any tokens
  annotid_disambiguation_matched <- annotid_disambiguation %>%
    dplyr::semi_join(all_basiclevel_na, by = c('annotid', 'object'))
  n_non_matched <- (nrow(annotid_disambiguation)
                    - nrow(annotid_disambiguation_matched))
  # Keep the ones where only the object changed - there is a good chance the
  # global basic level didn't. The user can then consult the list of the deleted
  # ones.
  objects_changed <- annotid_disambiguation %>%
    dplyr::semi_join(all_basiclevel_na, by = c('annotid')) %>%
    dplyr::anti_join(all_basiclevel_na, by = c('annotid', 'object'))

  # Remove tokens for objects that don't need to be disambiguated
  n_matched <- nrow(annotid_disambiguation_matched)
  annotid_disambiguation_matched <- annotid_disambiguation_matched %>%
    dplyr::anti_join(object_dict %>%
                       dplyr::count(object) %>%
                       dplyr::filter(n == 1),
                     by = c('object'))
  n_non_ambiguous <- n_matched - nrow(annotid_disambiguation_matched)

  # Find tokens those that need to be disambiguated but aren't
  # List all objects that need to be disambiguated
  ambiguous_objects <- object_dict %>%
    dplyr::count(object) %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(object)
  # A token needs to be disambiguated if:
  need_disambiguation <- all_basiclevel_na %>%
    # 1. The object is ambiguous
    dplyr::semi_join(ambiguous_objects, by = 'object') %>%
    # It is not already disambiguated
    dplyr::anti_join(annotid_disambiguation_matched,
                     by = c('annotid', 'object'))
  n_need_disambiguation <- nrow(need_disambiguation)

  # Combine
  annotid_disambiguation_for_update <-
    if (n_need_disambiguation > 0 | n_non_matched > 0 | n_non_ambiguous > 0) {
      dplyr::bind_rows(
        annotid_disambiguation_matched,
        need_disambiguation %>%
          dplyr::select(object, annotid) %>%
          dplyr::mutate(disambiguate = FIXME))
    }

  list(
    n_need_disambiguation = n_need_disambiguation,
    n_non_matched = n_non_matched,
    n_non_ambiguous = n_non_ambiguous,
    annotid_disambiguation = annotid_disambiguation_for_update,
    objects_changed = objects_changed
  )
}

#' Checks if object_dict is up-to-date
#'
#' - Are all objects in object_dict still present in all_basiclevel_na?
#' - Vice versa.
#'
#' Returns a list with three keys:
#' - n_new_objects - number of objects that need to be mapped to global_bl,
#' - n_objects_to_delete - number of objects that need to be deleted,
#' - object_dict:
#'   - NULL, if both of the above numbers are zero,
#'   - object_dict with n_new_objects rows added and n_deleted_objects rows
#'     removed.
#'
#' @noRd
update_object_dict <- function(all_basiclevel_na,
                              object_dict) {
  # Objects in all_basiclevel_na but not in object_dict
  new_objects <- all_basiclevel_na %>%
    dplyr::select(object) %>%
    dplyr::anti_join(object_dict, by = 'object') %>%
    dplyr::distinct(object)
  n_new_objects <- nrow(new_objects)

  # Objects in object_dict but not in all_basiclevel_na
  objects_to_delete <- object_dict %>%
    dplyr::select(object) %>%
    dplyr::anti_join(all_basiclevel_na, by = 'object') %>%
    dplyr::distinct(object)
  n_objects_to_delete <- nrow(objects_to_delete)

  # Keep the deleted objects. They can be referenced for cases when only the
  # spelling changed. Spelling changes shouldn't affect the global basic level.
  deleted_objects <- object_dict %>%
    dplyr::semi_join(objects_to_delete, by = c('object'))

  # Combine
  object_dict_for_update <-
    if (n_new_objects > 0 | n_objects_to_delete > 0) {
      dplyr::bind_rows(
        object_dict %>%
          dplyr::anti_join(objects_to_delete, by = c('object')),
        new_objects %>%
          dplyr::mutate(disambiguate = FIXME,
                        global_bl = FIXME))
    } else {
      object_dict
    }

  # Set `disambiguate` to NA for non-ambiguous words
  n_nonambiguous_disambiguated <- object_dict_for_update %>%
    dplyr::add_count(object) %>%
    dplyr::filter(n == 1 & !is.na(disambiguate)) %>%
    nrow
  if (n_nonambiguous_disambiguated > 0) {
    object_dict_for_update <- object_dict_for_update %>%
      dplyr::add_count(object) %>%
      dplyr::mutate(disambiguate = dplyr::case_when(
        n == 1 ~ NA_character_,
        TRUE ~ disambiguate)) %>%
      dplyr::select(-n)
  }

  if(n_new_objects == 0 & n_objects_to_delete == 0
     & n_nonambiguous_disambiguated == 0) {
    object_dict_for_update <- NULL
  }

  list(n_new_objects = n_new_objects,
       n_objects_to_delete = n_objects_to_delete,
       n_nonambiguous_disambiguated = n_nonambiguous_disambiguated,
       object_dict = object_dict_for_update,
       deleted_objects = deleted_objects)
}


#' Check both mappings for up-to-dateness
#'
#' If either of them have to be updated:
#' - put the files that need to be manually updated into a temporary folder,
#' - return a message with the path to the folder and the instructions.
#'
#' If the mappings are up-to-date, returns NULL
#' @noRd
update_mappings <- function(all_basiclevel_na,
                            object_dict,
                            annotid_disambiguation){
  object_dict_update <- update_object_dict(
    all_basiclevel_na = all_basiclevel_na,
    object_dict = object_dict)
  annotid_disambiguation_update <-  update_annotid_disambiguation(
    all_basiclevel_na = all_basiclevel_na,
    object_dict = object_dict,
    annotid_disambiguation = annotid_disambiguation)

  # If there is nothing to update, return NULL
  object_dict_ok <- is.null(object_dict_update$object_dict)
  annotid_disambiguation_ok <- is.null(
    annotid_disambiguation_update$annotid_disambiguation)
  if (object_dict_ok & annotid_disambiguation_ok) {
    return(NULL)
  }

  # We'll put the files to be updated into a temporary folder and output
  # instructions as needed
  temp_dir <- tempfile('global_bl_mappings_update')
  dir.create(temp_dir, showWarnings = FALSE)
  instructions <- glue::glue(
    "Files used to match tokens to their global basic level will have to be ",
    "updated. See files inside the temporary directory \n{temp_dir}\n",
    "Beware that the folder will be deleted when the R session ends."
    )


  # For each mapping, save the tibbles required/useful for updates

  # We'll need the dictionary both when it itself needs to be updated and when
  # there are tokens that need to be disambiguated - to look up or add
  # disambiguations
  dict_filename <- 'global_bl_dictionary.csv'
  if (!object_dict_ok
      | annotid_disambiguation_update$n_need_disambiguation > 0) {
    # If object_dict does not have to be updated, then we'll write the original
    # object_dict
    object_dict_to_write <- if (is.null(object_dict_update$object_dict)) {
      object_dict}
    else {
      object_dict_update$object_dict}

    object_dict_to_write %>%
      readr::write_csv(file.path(temp_dir, dict_filename))
  }

  if (!object_dict_ok) {

    # Save deleted objects in case it was just the spelling change
    deleted_objects <- object_dict_update$deleted_objects
    deleted_objects_filename <- glue::glue('deleted_objects_{dict_filename}')
    if (nrow(deleted_objects) > 0) {
      deleted_objects %>%
        readr::write_csv(file.path(temp_dir, deleted_objects_filename))
      instructions <- glue::glue(
        instructions, '\n',
        'Some of the objects might have been deleted because the spelling ',
        'changed. Consult {deleted_objects_filename} if you think that might be ',
        'the case.')
    }


    n_new_objects <- object_dict_update$n_new_objects
    n_objects_to_delete <- object_dict_update$n_objects_to_delete
    n_nonambiguous_disambiguated <-
      object_dict_update$n_nonambiguous_disambiguated
    if (n_new_objects > 0) {
      # Add instructions to add global basic levels to new objects
      instructions <- glue::glue(
        instructions, '\n',
        "Update {dict_filename}. ",
        "Look for the cells with \"{FIXME}\" in them and fill in the ",
        "\"global_bl\" column. ",
        'If the object is ambigious, fill in the "disambiguate" column as well,',
        'otherwise just delete "{FIXME}" from it.\n')
    } else if (n_objects_to_delete > 0 | n_nonambiguous_disambiguated > 0) {
      # Tell the user to copy the reduced file to the repository
      instructions <- glue::glue(
        instructions, '\n',
        "File {dict_filename} has been updated in the temporary directory. ",
        "Update it in the repository as well.\n"
      )
    }
  }

  if (!annotid_disambiguation_ok) {
    # Save file to a temporary folder
    filename <- 'disambiguated_rows.csv'
    annotid_disambiguation_update$annotid_disambiguation %>%
      write_csv(file.path(temp_dir, filename))

    # Save tokens whose object changed, if there are any
    objects_changed <- annotid_disambiguation_update$objects_changed
    if (nrow(objects_changed) > 0) {
      objects_changed_filename <- glue::glue('objects_changed_{filename}')
      objects_changed %>%
        write_csv(file.path(temp_dir, objects_changed_filename))
      instructions <- glue::glue(
        instructions, '\n',
        'Some tokens might have simply changed their spelling. Consult ',
        '{objects_changed_filename} if you think that might be the case.')
    }

    n_need_disambiguation <- annotid_disambiguation_update$n_need_disambiguation
    n_non_matched <- annotid_disambiguation_update$n_non_matched
    n_non_ambiguous <- annotid_disambiguation_update$n_non_ambiguous
    if (n_need_disambiguation > 0) {
      # Tell the user to disambiguate tokens
      instructions <- glue::glue(
        instructions, '\n',
        "Update {filename}. ",
        "Look for the cells with \"{FIXME}\" in them and fill in the ",
        '"disambiguate" column.\n'
      )
    } else if (n_non_matched > 0 | n_non_ambiguous > 0) {
      # Tell the user to copy the reduced file to the repository
      instructions <- glue::glue(
        instructions, '\n',
        "File {filename} has fewer rows now, the repository needs to be ",
        "updated.\n"
      )
    }
  }

  instructions <- glue::glue(
    instructions, '\n',
    'Once done, update the correspoding files in the all_basiclevel ',
    'repostitory in the "global_basic_level_dicts" folder.\n')

  instructions
}

#' Map every row in all_basiclevel_na to its global basic level
#'
#' - disambiguate what we can, based on annotid_disambiguation and several
#'   (object, basic_level) combinations,
#' - map (object, disambiguate) to global_bl using object_dict
#'
#' It is assumed that the all_basiclevel_na and global_bl mappings have already
#' been checked and so there is enough information to map every token to a
#' global basic level. No checks are run.
#'
#' @noRd
assign_global_basic_level <- function(all_basiclevel_na,
                                      object_dict,
                                      annotid_disambiguation) {
  # Disambiguate first based on annotid and then based on
  assignment <- all_basiclevel_na %>%
    dplyr::select(annotid, object, basic_level) %>%
    dplyr::left_join(annotid_disambiguation, by = c("annotid", "object")) %>%
    dplyr::mutate(disambiguate = dplyr::case_when(
      !is.na(disambiguate) ~ disambiguate,
      object == "balls" & basic_level == "ball" ~ "toy",
      object == "Momo" & basic_level == "Momo" ~ "dog",
      object == "glasses" & basic_level == "glasses" ~ "eye",
      TRUE ~ disambiguate))

  # Match to the global basic value
  assignment %>%
    dplyr::left_join(object_dict, by = c("object", "disambiguate")) %>%
    dplyr::select(-disambiguate)
}


#' Updates global basic levels in all_basiclevel_na
#'
#' @param all_basic_level_na_path Path to the all_basiclevel_na.csv file created
#'  using `blabpy.seedlings.pipeline.make_updated_all_basic_level_here`
#'
#' Loads
#' - the temporary all_basiclevel_na.csv
#' - object_dict and annotid_disambiguation used for mapping objects/tokens to
#'   their global basic level - from the all_basiclevel repo.
#'
#' Then adds a global_bl column to all_basiclevel_na if it can.
#'
#' If it can, it will return all_basiclevel_na with the new global_bl column.
#'
#' If it can't, it will throw an error and provide instructions on how to update
#' the global basic level mappings.
#'
#' @export
update_global_basic_levels <- function(
  all_basic_level_na_path
) {
  # Load the data and the mappings
  all_basiclevel_na <- read_all_basiclevel_na(all_basic_level_na_path,
                                              has_global_bl = FALSE)
  assertthat::assert_that(sum(is.na(all_basiclevel_na$basic_level)) > 0)

  suppressWarnings({
    latest_version <- get_latest_version('all_basiclevel')
    global_bl_mappings <- get_global_bl_mappings(
      version = latest_version)
    object_dict <- global_bl_mappings$object_dict
    annotid_disambiguation <- global_bl_mappings$annotid_disambiguation
  })

  # Run a few basic checks on the mappings
  check_object_dict(object_dict)
  check_annotid_disambiguation(annotid_disambiguation)

  # Check that the mapping are complete and up-to-date with respect to what is
  # in all_basiclevel_na.
  update_instructions <-
    update_mappings(
      all_basiclevel_na = all_basiclevel_na,
      object_dict = object_dict,
      annotid_disambiguation = annotid_disambiguation
    )
  if (!is.null(update_instructions)) {
    stop(update_instructions)
  }

  # Assign global basic level
  assignment <- assign_global_basic_level(all_basiclevel_na,
                                          object_dict,
                                          annotid_disambiguation)
  assertthat::assert_that(identical(
    colnames(assignment),
    c("annotid", "object", "basic_level", "global_bl")))

  # Add "global_bl" column
  with_global_bl <- all_basiclevel_na %>%
    # Older versions didn't have this column, hence `any_of`
    dplyr::select(-any_of('global_bl')) %>%
    dplyr::inner_join(assignment,
                      by = c("annotid", "object", "basic_level"))

  # Check that we didn't lose any tokens, didn't create any duplicates,
  assertthat::assert_that(assertthat::are_equal(
    all_basiclevel_na$annotid, with_global_bl$annotid))

  # Check if all the tokens have global basic level now.
  assertthat::assert_that(sum(is.na(with_global_bl$global_bl)) == 0)

  with_global_bl
}
