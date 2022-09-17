git submodule update --init --recursive

# build truct first
git clone https://github.com/RealNeGate/Truct
cd Truct
git submodule update --init
git pull
sh ./build.sh
cd ..

# actually build Cuik
export PATH=/home/runner/work/Cuik/Truct/bin:$PATH

mkdir -p bin
./Truct/bin/truct
