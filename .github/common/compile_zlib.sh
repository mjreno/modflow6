cd "$GITHUB_WORKSPACE/zlib"
echo "COMPILE ZLIB"
export CC=icc
export CXX=icpc
#export CFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'
#export CXXFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'
export F77=ifort
export FC=ifort
export F90=ifort
#export FFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'
export CPP='icc -E'
export CXXCPP='icpc -E'
export CFLAGS='-O3 -xHost -ip -no-prec-div'
export CXXFLAGS='-O3 -xHost -ip -no-prec-div'
export FFLAGS='-O3 -xHost -ip -no-prec-div'
./configure --prefix=$(pwd) --enable-shared
make check
make install
