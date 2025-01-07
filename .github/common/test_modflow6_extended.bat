cd "%GITHUB_WORKSPACE%\modflow6\autotest"
pytest -v test_gwf_sto01.py -s
pytest -v test_netcdf_gwf_sto01.py --netcdf -s
pixi run autotest -m "%MARKERS%" --netcdf --parallel
