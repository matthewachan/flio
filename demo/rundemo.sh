# demo.f
# Author: Matthew Chan
# Build the project
cd ../src
make

cd -

# Compile and run the demo script
../src/flio.native < demo.f > demo.ll
llc demo.ll
clang demo.s ../src/stdlib.c -o demo
./demo

rm demo.ll demo.s demo
