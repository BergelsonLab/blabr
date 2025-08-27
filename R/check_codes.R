subj_nums <- c("01", "02", "03", "04", "05", "06",
               "07", "08", "09", "10", "11", "12",
               "13", "14", "15", "16", "17", "18",
               "19", "20", "21", "22", "23", "24",
               "25", "26", "27", "28", "29", "29",
               "30", "31", "32", "33", "34", "35",
               "36", "37", "38", "39", "40", "41",
               "42", "43", "44", "45", "46")

subj_mos <- c("06", "07", "08", "09", "10", "11",
              "12", "13", "14", "15", "16", "17")

utt_type <- c("d", "r", "q", "n", "s", "i", "u", "o")
obj_pres <- c("y", "n", "u", "o")

check_annot_codes <- function(df, keep_na=FALSE) {
  if (keep_na) {
    utt_type <- c(utt_type, NA)
    obj_pres <- c(obj_pres, NA)
  }
  x <- subset(df, !(utterance_type %in% utt_type) |
                  !(object_present %in% obj_pres) |
                  !(is.numeric(onset) & is.numeric(offset)) |
                  !(subj %in% subj_nums) |
                  !(month %in% subj_mos))
  x
}
