#!/bin/bash

OOZE=../build/ooze

echo "./ooze functions"
$OOZE functions

echo "./ooze types"
$OOZE types

echo "./ooze run \"create_point(4, 5)\" -o point"
$OOZE run "create_point(4, 5)" -o point

echo "./ooze run \"@point\""
$OOZE run "@point"

echo "./ooze run -s script.oz \"main(@point, 1)\""
$OOZE run -s script.oz "main(@point, 1)"

rm point
