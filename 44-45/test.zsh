#!/bin/zsh

TEST="test1.txt"
OUTPUT="test1.out"
TESTS=('45.sml' '45b.sml' '45c.sml')
SEP=" "

touch $TEST
touch $OUTPUT

setopt no_multios

function runTest()
{
  t=$( { time ( sml $1 < /dev/null ) } 2>&1 >/dev/null )
  echo $t | awk '{ printf $1 }' >> $OUTPUT
  echo -n $SEP >> $OUTPUT
}

function increaseSize()
{
  head -c 25K < /dev/urandom >> $TEST
}

function logSize()
{
  du -s $TEST | awk '{ printf $1 }' >> $OUTPUT
  echo -n $SEP >> $OUTPUT
}

for var ({0..100}); do {
  echo -n '.'
  increaseSize;
  logSize;
  for var in $TESTS; do {
    runTest $var;
  } done;
  echo $SEP >> $OUTPUT;
} done

