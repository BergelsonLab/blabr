library(blabr)

# get the latest
all_bl <- get_all_basiclevel()

# get the version at a specific commit
all_bl <- get_all_basiclevel(commit = '833356d604fa2ea020d7984a7f6be612ffea862c')

all_bl <- get_all_basiclevel(branch='no_header')

all_bl <- get_all_basiclevel(commit = '5259867974bfa73ba7c1aa83d187c507d50f832c')

cdi <- get_cdi_spreadsheet()
motor <- get_motor_spreadsheet()

feather::write_feather(all_bl, "all_basiclevel.feather")
feather::write_feather(cdi, file.path(blab_data, "cdi_spreadsheet", "cdi.feather"))
feather::write_feather(motor, file.path(blab_data, "motor_spreadsheet", "motor.feather"))

motor <- read.csv("/Users/andrei/BLAB_DATA/motor_spreadsheet/motor.csv")
