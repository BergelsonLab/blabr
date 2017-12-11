library(blabr)

all_bl <- get_all_basiclevel()

missing <- c("babies", "babydoll", "ball ", "blankie","ba+ba", "babas", "diapee", "hands", "ball")

missing %in% all_bl$basic_level
