#!/bin/bash

OOZE=../build/ooze

echo "-------- ./ooze functions --------"
$OOZE functions

echo "-------- ./ooze types --------"
$OOZE types

echo "-------- ./ooze fn create_point 4 5 -o p1 --------"
$OOZE fn create_point 4 5 -o p1

echo "-------- ./ooze dump p1 --------"
$OOZE dump p1

echo "-------- ./ooze run script.oz @p1 1 -v --------"
$OOZE run script.oz @p1 1 -v

rm p1 result_0 result_1
