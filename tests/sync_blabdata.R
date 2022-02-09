library(blabr)

# get the latest
all_bl <- get_all_basiclevel(version = '0.1.0')

cdi <- get_cdi_spreadsheet()
motor <- get_motor_spreadsheet()
relia <- get_reliability("audio", "06")

