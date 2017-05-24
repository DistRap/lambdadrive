set confirm off
set history save on
set mem inaccessible-by-default off

# drv tests
#display /x last_value.rx_buf[0]
#display /x last_value.rx_buf[1]
#display /u rounds

# encoder tests
#display /u count
#display /u dir

# calib tests
#display encState
#display calibState
#display sumFwd
#display sumBack

display calibState
display lastSample
display control
