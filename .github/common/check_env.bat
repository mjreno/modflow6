cd "%GITHUB_WORKSPACE%\modflow6"
pixi add pkg-config
pixi add netcdf-fortran
pixi run which meson
pixi run which pkg-config
pixi run pkg-config --list-all
pixi run pkg-config --libs netcdf
pixi run pkg-config --libs netcdf-fortran
pixi run pkg-config --cflags netcdf
pixi run pkg-config --cflags netcdf-fortran
