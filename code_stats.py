import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import datetime as dt




# Stats about line counts in hello project, without sh and perl, for almost
# entire project (src, dpid, dpic dirs etc.)

# Haskell numbers don't include code in these dirs:
# src/haskell/tests
# src/haskell/tests_tools
# src/haskell/ffi
# src/Hello/Ffi

# C/C++ numbers don't include code in these dirs/files:
# src/haskell/hello.h




# Line counts generated using David A. Wheeler's 'SLOCCount'.




# The numbers will be stored in the array as strings. We will have to convert
# them to integers before displaying them.
#
#         date           cpp          c    haskell
arr = np.array((
    ["2021-01-05",     33629,     19428,         0],
    ["2021-01-06",     33629,     19423,        21],
    ["2021-01-31",     33629,     19283,       153],
    ["2021-02-06",     33629,     19283,       168],
    ["2021-02-07",     33629,     19304,       168],
    ["2021-02-12",     33629,     19464,       168],
    ["2021-02-14",     33629,     19347,       333],
    ["2021-03-03",     33632,     19048,       587],
    ["2021-03-07",     33502,     19050,       733],
    ["2021-03-14",     33426,     19051,       760],
    ["2021-04-07",     33414,     19060,       957],
    ["2021-04-10",     33117,     19062,      1210],
    ["2021-04-19",     33243,     19072,      1403],
    ["2021-04-24",     33192,     19082,      1568],
    ["2021-05-07",     33090,     19082,      1622],
    ["2021-05-15",     32750,     19091,      1869],
    ["2021-05-23",     32596,     19019,      1982],
    ["2021-06-15",     32564,     19019,      1920],
    ["2021-06-26",     32543,     19019,      1980],
    ["2021-07-06",     32558,     19125,      2054],
))




dates    = [dt.datetime.strptime(d, '%Y-%m-%d').date() for d in arr[ :, 0]]
lang_cpp = [int(numeric_string) for numeric_string in arr[ :, 1]]
lang_c   = [int(numeric_string) for numeric_string in arr[ :, 2]]
lang_hs  = [int(numeric_string) for numeric_string in arr[ :, 3]]
total    = np.add(np.add(lang_cpp, lang_c), lang_hs)




plt.plot(dates, lang_cpp, color='red',      label='C++')
plt.plot(dates, lang_c,   color='green',    label='C')
plt.plot(dates, lang_hs,  color='blue',     label='Haskell')
plt.plot(dates, total,    color='silver',   label='Total')




plt.draw()

# Get rid of margins. plt.tight_layout() doesn't work.
# -1 accesses last element in numpy array. Cool!
axes = plt.gca()
axes.set_ylim(bottom=0)
axes.set_xlim(left = dt.datetime.strptime(arr[0, 0], '%Y-%m-%d').date(),
              right = dt.datetime.strptime(arr[-1, 0], '%Y-%m-%d').date())

plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
plt.gca().xaxis.set_major_locator(mdates.MonthLocator())
plt.gcf().autofmt_xdate()

plt.xlabel("Date")
plt.ylabel("LOC")
plt.title("hello's LOC over time")
plt.legend()

plt.show()
