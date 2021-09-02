import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import datetime as dt




# Stats about line counts in hello project, without sh and perl, for almost
# entire project (src, dpid, dpic dirs etc.)

# Haskell numbers don't include code in these dirs:
# src/Hello/Tests
# src/Hello/Ffi

# C/C++ numbers don't include code in these dirs/files:
# src/Hello/hello.h
# test/




# Line counts generated using David A. Wheeler's 'SLOCCount'.




# The numbers will be stored in the array as strings. We will have to convert
# them to integers before displaying them.
#
#         date           cpp          c    haskell
arr = np.array((
    ["2021-01-05",     31552,     18654,         0],
    ["2021-01-06",     31552,     18649,        21],
    ["2021-01-31",     31552,     18509,       153],
    ["2021-02-06",     31552,     18509,       168],
    ["2021-02-07",     31552,     18530,       168],
    ["2021-02-12",     31552,     18690,       168],
    ["2021-02-14",     31552,     18573,       333],
    ["2021-03-03",     31555,     18274,       587],
    ["2021-03-07",     31425,     18276,       733],
    ["2021-03-14",     31349,     18277,       760],
    ["2021-04-07",     31337,     18286,       957],
    ["2021-04-10",     31040,     18288,      1210],
    ["2021-04-19",     31166,     18298,      1403],
    ["2021-04-24",     31115,     18308,      1568],
    ["2021-05-07",     31013,     18308,      1622],
    ["2021-05-15",     30673,     18317,      1869],
    ["2021-05-23",     30519,     18245,      1982],
    ["2021-06-15",     30487,     18245,      1920],
    ["2021-06-26",     30466,     18245,      1980],
    ["2021-07-06",     30481,     18351,      2054],
    ["2021-07-24",     30210,     18429,      2157],
    ["2021-07-31",     30090,     18425,      2260],
    ["2021-08-21",     30090,     18449,      2357],
    ["2021-09-02",     30092,     18449,      2393],
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

# Major ticks every 20, minor ticks every 5
major_ticks = np.arange(0, 60000, 5000)
minor_ticks = np.arange(0, 60000, 1000)

axes.set_yticks(major_ticks)
axes.set_yticks(minor_ticks, minor=True)

# Or if you want different settings for the grids:
plt.grid(which='minor', alpha=0.2)
plt.grid(which='major', alpha=0.5)
plt.subplots_adjust(left=0.05, right=0.99, top=0.97, bottom=0.1)

plt.show()
