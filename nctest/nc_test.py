from netCDF4 import Dataset

# code from tutorial.

# create a file (Dataset object, also the root group).
rootgrp = Dataset('test.nc', 'w', format='NETCDF4')
print(rootgrp.file_format)
rootgrp.close()

# create some groups.
rootgrp = Dataset('test.nc', 'a')
fcstgrp = rootgrp.createGroup('forecasts')
analgrp = rootgrp.createGroup('analyses')
fcstgrp1 = rootgrp.createGroup('/forecasts/model1')
fcstgrp2 = rootgrp.createGroup('/forecasts/model2')

# walk the group tree using a Python generator.
def walktree(top):
    yield top.groups.values()
    for value in top.groups.values():
        yield from walktree(value)

print(rootgrp)
for children in walktree(rootgrp):
    for child in children:
        print(child)

# dimensions.
level = rootgrp.createDimension('level', None)
time = rootgrp.createDimension('time', None)
lat = rootgrp.createDimension('lat', 73)
lon = rootgrp.createDimension('lon', 144)

print(rootgrp.dimensions)

print(len(lon))
print(lon.isunlimited())
print(time.isunlimited())

for dimobj in rootgrp.dimensions.values():
    print(dimobj)

print(time)

# variables.
times = rootgrp.createVariable('time','f8',('time',))
levels = rootgrp.createVariable('level','i4',('level',))
latitudes = rootgrp.createVariable('lat','f4',('lat',))
longitudes = rootgrp.createVariable('lon','f4',('lon',))
# 2 unlimited dimensions.
#temp = rootgrp.createVariable('temp','f4',('time','level','lat','lon',))
# this makes the compression 'lossy' (preserving a precision of 1/1000)
# try it and see how much smaller the file gets.
temp = rootgrp.createVariable('temp','f4',('time','level','lat','lon',),least_significant_digit=3)
print(temp)
# create variable in a group using a path.
temp = rootgrp.createVariable('/forecasts/model1/temp','f4',('time','level','lat','lon',))
print(rootgrp['/forecasts/model1']) # print the Group instance
print(rootgrp['/forecasts/model1/temp']) # print the Variable instance

# attributes.
import time
rootgrp.description = 'bogus example script'
rootgrp.history = 'Created ' + time.ctime(time.time())
rootgrp.source = 'netCDF4 python module tutorial'
latitudes.units = 'degrees north'
longitudes.units = 'degrees east'
levels.units = 'hPa'
temp.units = 'K'
times.units = 'hours since 0001-01-01 00:00:00.0'
times.calendar = 'gregorian'

for name in rootgrp.ncattrs():
    print('Global attr', name, '=', getattr(rootgrp,name))

print(rootgrp)

print(rootgrp.__dict__)

print(rootgrp.variables)

import numpy as np
# no unlimited dimension, just assign to slice.
lats =  np.arange(-90,91,2.5)
lons =  np.arange(-180,180,2.5)
latitudes[:] = lats
longitudes[:] = lons
print('latitudes =\n',latitudes[:])
print('longitudes =\n',longitudes[:])

# append along two unlimited dimensions by assigning to slice.
nlats = len(rootgrp.dimensions['lat'])
nlons = len(rootgrp.dimensions['lon'])
print('temp shape before adding data = ',temp.shape)
from numpy.random.mtrand import uniform # random number generator.
temp[0:5,0:10,:,:] = uniform(size=(5,10,nlats,nlons))
print('temp shape after adding data = ',temp.shape)
# levels have grown, but no values yet assigned.
print('levels shape after adding pressure data = ',levels.shape)

# assign values to levels dimension variable.
levels[:] =  [1000.,850.,700.,500.,300.,250.,200.,150.,100.,50.]
# fancy slicing
tempdat = temp[::2, [1,3,6], lats>0, lons>0]
print('shape of fancy temp slice = ',tempdat.shape)
print(temp[0, 0, [0,1,2,3], [0,1,2,3]].shape)

# fill in times.
from datetime import datetime, timedelta
from netCDF4 import num2date, date2num, date2index
dates = [datetime(2001,3,1)+n*timedelta(hours=12) for n in range(temp.shape[0])]
times[:] = date2num(dates,units=times.units,calendar=times.calendar)
print("time values (in units {}):\n{}".format(times.units, times[:]))
dates = num2date(times[:],units=times.units,calendar=times.calendar)
print("dates corresponding to time values:\n{}".format(dates))

rootgrp.close()
