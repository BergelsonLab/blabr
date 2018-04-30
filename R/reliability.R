reliability <- function(df, var) {
df <- df %>%
  filter(new_utt_type!="o")

k <- tibble::tibble(
  "agree"=c(agree(df[,c("orig_utt_type","new_utt_type")])$value,
            agree(df[,c("orig_present","new_present")])$value),
  "kappa"=c(kappa2(df[,c("orig_utt_type","new_utt_type")])$value,
            kappa2(df[,c("orig_present","new_present")])$value)
  )

if (var == "utt_type") {
  return(k[1,])
}
k
}


#
#
# # audiorel_6 <- get_reliability("audio", "06")
# # audiorel_7 <- get_reliability("audio", "07")
#
# audiorel_6_processed <- reliability(audiorel_6)
# audiorel_7_processed <- reliability(audiorel_7)
#
# audiorel_7_processed.UT
#
# mutate(utrel = ifelse(orig_utt_type==new_utt_type, T, F),
#        oprel = ifelse(orig_present==new_present, T, F),
#        orig_present = factor(orig_present),
#        new_present = factor(new_present),
#        orig_utt_type = factor(orig_utt_type),
#        new_utt_type = factor(new_utt_type))
#
# kappavidUT<- kappa2(audiorel_6[,c("orig_utt_type","new_utt_type")])
#
