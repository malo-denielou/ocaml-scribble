#/bin/sh

DIR=../..
TOOL=$DIR/oscribble.native

TESTDIR=`ls -d */`


for d in $TESTDIR
do
    cd $d
    TESTS=`ls *.sh`
    for t in $TESTS
    do
        source $t
        $TOOL $OPTIONS $PROTOCOL
        if [ "$?" = $EXPECTED ]
        then 
            echo "[ OK ] $NAME"
        else 
            echo "[FAIL] $NAME"
            exit 1
        fi
    done
    cd ..
done
