#!/bin/bash

c=$1
while [ $c -gt 0 ]
do
   echo abc | sha256 > /dev/null
   c=$(( $c - 1 ))
done
