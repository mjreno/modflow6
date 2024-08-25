echo "COMPILE ZLIB"
ls -l "$GITHUB_WORKSPACE"
cd "$GITHUB_WORKSPACE/zlib"
export CC=icc
export CXX=icpc
export F77=ifort
export FC=ifort
export F90=ifort
export CPP='icc -E'
export CXXCPP='icpc -E'
#export CFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'
#export CXXFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'
#export FFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'
export CFLAGS='-O3 -xHost -ip -no-prec-div'
export CXXFLAGS='-O3 -xHost -ip -no-prec-div'
export FFLAGS='-O3 -xHost -ip -no-prec-div'
./configure --prefix=$(pwd) --enable-shared
make check
make install
