#!/bin/bash

tmp=$(mktemp)
trap "rm -f ${tmp}" EXIT

dune build || exit 5 ;
echo ""
echo "Start testing!"
echo ""

for prog in tests/*.exp ; do
    mips=${prog%%exp}s

    if !  ./calc.exe $prog
    then
        echo -n "Compilation failed on ${prog}..."  ;
        if test -e ${prog%%exp}err ;
        then
            echo " as expected!"
        else
            echo " error!"
            exit 3 ;
        fi ;
    else
        in=${prog%%exp}in
        out=${prog%%exp}out
        if ! test -e $in
        then
            echo "Missing ${in}!" ;
            exit 1  ;
        fi 
        if ! test -e $out
        then
            echo "Missing ${out}!" ;
            exit 2  ;
        fi ;
        if ! test -e $mips ;
        then
            echo "Compilation failed ${out}?"  ;
            exit 3 ;
        fi ;
        spim -file $mips < $in | grep '^[0-9]*$'> $tmp
        if ! cmp --silent $tmp $out ;
        then
            echo "Test ${prog%%.exp} failed: output is not what was expected!"
            diff $tmp $out ;
            exit 4
        fi ;
        echo "Test ${prog%%.exp} OK!"
    fi;
done ;

echo "All tests are OK!"
