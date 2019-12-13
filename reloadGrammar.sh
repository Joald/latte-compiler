set -e

mv src/BNFC src/bnfc_copy
cp Latte.cf src
cd src
bnfc -m -p BNFC --haskell Latte.cf && make
make clean
rm Makefile Latte.cf
cd BNFC
rm *.y *.x *.txt TestLatte TestLatte.hs *.info
cd ../
rm -r bnfc_copy
