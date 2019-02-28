library(tidyverse)
library(feather)
all_bl <- read_feather("Z:/Seedlings/Compiled_Data/BLAB_DATA/all_basiclevel/all_basiclevel.feather")
all_data <- all_bl

## Weeding out Multiples ----
# some object words have more than one basic_level. We narrowed them down using the following code
obj_bl <- all_bl %>%
  distinct(object, basic_level)

multiples <- obj_bl %>%
  count(object) %>%
  filter(n > 1)

multiples <- left_join(multiples, obj_bl)

#write_csv(multiples, "multiples.csv") # wrote out, decided on best global basic level, removed any object words with multiple viable basic_levels, e.g. "baba"
multiples_no_dupes <- read_csv("multiples_no_dupes.csv") # read back in the edited version

## Double-checking the ones that map one-to-one to see if we can consolidate them ----
singles <- obj_bl %>%
  count(object) %>%
  filter(n == 1)

singles <- left_join(singles, obj_bl)
#write_csv(singles, "singles.csv")

# Adding the cleaned multiples to the singles before we consolidate, and then writing it out to work on it -----
all_global_bl <- bind_rows(multiples_no_dupes, singles)
#write_csv(all_global_bl, "all_global_bl.csv")

# identifying object words that need better basic_levels
obj_probs <- c("woof\\+woof", 'nupboards', 'hots', 'harmy', 'hammies', 'hammy', 'ghair', 'bumbley', 'bumbo', 'bumba', 'bumbelly', 'bobo', 'buh', 'bacca')
# identifying words that reasonably have more than one basic level and need clarification
mult_dupes <- c("army", "ba", 'baba', 'bababa', 'babas', 'beh\\+beh', 'boo', 'boo\\+boo', 'cords', 'glasses', 'hams', 'manna', 'Momo')

# getting all the rest of the info we need about those so an RA can go disambiguate them
prob_objs <- all_data %>%
  filter(object %in% obj_probs)

dupe_mults <- all_data %>%
  filter(object %in% mult_dupes)

prob_objs <- rbind(prob_objs, dupe_mults)
rm(dupe_mults)

prob_objs <- left_join(prob_objs, baba)
#write_csv(prob_objs, "prob_objs.csv")

#creating global basic_levels for everything that is usually NA ----
NAs <- read_csv("NA_basiclevels.csv")

NAs <- NAs %>%
  distinct(object)
#write_csv(NAs, "NAs.csv")

# Reading everything back in and combining it into a new situation ----
# Gladys made me a new all_basic_level but that includes the rows that usually get filtered out for not having NAs!
all_bl_NA <- read_csv("Z:/Seedlings/Code/blab/output/all_basiclevel_with_na.csv") %>% # 330017 x 14
  select(-tier)

NAs <- read_csv("NAs_bl.csv") # 1686 x 2

NAs %>%
  filter(is.na(basic_level)) # none

all_global_bl <- read_csv("all_global_bl.csv") %>%   #13913 x 2
  distinct(object)

all_global_bl_with_NAs <- full_join(all_global_bl, NAs) #15174 lines... 425 fewer lines than expected. Looks like some things were inconsistently NA'd.
#Basic_leveling is not an exact science.
anti_join(all_global_bl, NAs) # doing the math the other way. There are 13,488 unmatched records in all_global_bl, which is 425 fewer than the total. checks out

#write_csv(all_global_bl_with_NAs, "all_global_bl_with_NAs.csv")

prob_obj <- read_csv("G:/BergelsonLab/Alternations_list/prob_objs_annot.csv") %>% # this is the sheet that the RAs went through to determine what was actually being discussed
  select(-X17, -X18, -19) %>%
  mutate(subj = factor(subj),
         subj = fct_recode(subj,
                           "01" = "1",
                           "02" = "2",
                           "03" = "3",
                           "04" = "4",
                           "06" = "6",
                           "07" = "7",
                           "08" = "8",
                           "09" = "9"),
         month = factor(month),
         month = fct_recode(month,
                            "06" = "6",
                            "07" = "7",
                            "08" = "8",
                            "09" = "9")) %>%
  select(-`FIXES/ISSUES`)


prob_obj_distinct <- prob_obj %>%
distinct(object, basic_level, disambiguate)

all_bl_disamb <- left_join(all_global_bl_with_NAs, prob_obj_distinct)

all_bl_disamb <- all_bl_disamb %>%
  mutate(global_bl = basic_level) %>%
  select(-basic_level)

all_bl_NA2 <- left_join(all_bl_NA, prob_obj) #why are there 90 extra lines!!!!
summary(all_bl_NA)
summary(prob_obj)
everything <- left_join(all_bl_NA, all_bl_disamb)

left_join(all_bl_NA, all_bl_NA)



