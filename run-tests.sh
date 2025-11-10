#!/bin/sh

TEST_FILES=$(find ./tests -type f -name *.scm) 

if [ -z "$SCHEME_CMD" ]; then
    SCHEME_CMD="chezscheme --libdirs .. --script"
fi

echo "Running tests with" $(echo $SCHEME_CMD | cut -d' ' -f1)

for f in $TEST_FILES; do
    echo "> $SCHEME_CMD $f $@"
    $SCHEME_CMD $f $@
done

