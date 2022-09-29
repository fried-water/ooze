#!/bin/bash

OOZE=../build/ooze

echo "./ooze functions"
$OOZE functions

echo "./ooze types"
$OOZE types

echo "./ooze run \"write('point', serialize(create_point(4, 5)))\""
$OOZE run "write('point', serialize(create_point(4, 5)))"

echo "./ooze run -s script.oz \"main(deserialize(read('point')), 1)\""
$OOZE run -s script.oz "main(deserialize(read('point')), 1)"

rm point
