echo "hi"
set FC=ifort
cd "%GITHUB_WORKSPACE%\modflow6"
pixi run setup -Dnetcdf=true builddir
pixi run build builddir
pixi run pytest -v --durations=0 --netcdf -k "test_netcdf"
