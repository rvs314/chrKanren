#!/bin/sh

TEST_FILES=$(find ./tests -type f -name *.scm) 

if [ -z "$SCHEME_CMD" ]; then
    SCHEME_CMD="chezscheme --libdirs .. --script"
fi

echo "Running tests with" $(echo $SCHEME_CMD | cut -d' ' -f1)

START=`date "+%s%3N"`
FAILURES=0

for f in $TEST_FILES; do
    echo "> $SCHEME_CMD $f $@"
    DEBUG=OFF $SCHEME_CMD $f $@
    RETCODE=$?
    if [ $RETCODE -ne 0 ]; then
        echo "Test failed! Rerunning in debug mode"
        $SCHEME_CMD $f $@
    fi
    FAILURES=$(($FAILURES + $RETCODE))
done

END=`date "+%s%3N"`

if [ $FAILURES -eq 0 ]; then
    DURATION="$(($END-$START))"
    echo "Tests passed in $DURATION milliseconds"
    if [ "$DEBUG" = "OFF" ]; then
        echo "$(git show | head -n1) $DURATION" >> perf.txt
    fi
else
    echo "Some Tests Failed!"
fi
