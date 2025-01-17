cd "%GITHUB_WORKSPACE%\modflow6\autotest"
pixi run autotest -k test_gwf_sto01.py -s
pixi run autotest -k test_netcdf_gwf_sto01.py --netcdf -s
pixi run autotest -m "%MARKERS%" --netcdf --parallel
