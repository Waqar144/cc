set -e

TESTDIR=test
mkdir -p testout
rm -rf testout/*

TESTFILES="cast sizeof typedef union struct arith control function variable string pointer decl usualconv"

for filename in $TESTFILES
do
#     filename=$(basename -s .c $cfile)
    # preprocess using gcc
    gcc -o- -E -P -C $TESTDIR/$filename.c > testout/$filename.c
    # run our compiler
    cargo run -- testout/$filename.c -o testout/$filename.s
    # compile output asm with gcc
    gcc -o testout/$filename testout/$filename.s -xc $TESTDIR/test.c
    # run the output
    ./testout/$filename
done
