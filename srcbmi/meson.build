bmi_sources = files(
    'bmi.f90',
    'mf6bmi.f90',
    'mf6bmiError.f90',
    'mf6bmiGrid.f90',
    'mf6bmiUtil.f90',
    'mf6xmi.F90',
)

message('MODFLOW 6 shared library name:', 'lib' + buildname)

if build_machine.system() == 'darwin' and with_netcdf
  library(buildname, bmi_sources, link_with: mf6core, link_args : '/opt/local/lib/libnetcdff.dylib', name_prefix: 'lib', install: true)
else
  library(buildname, bmi_sources, link_with: mf6core, name_prefix: 'lib', install: true)
endif
