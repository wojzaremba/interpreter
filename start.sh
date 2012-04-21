#!/bin/bash
 
function run {
  out="$(echo $1 | sed s/jl/output/g)";
  res="$(echo $out | sed s/examples/out/g)";
  echo -n "execution $1 "
  cat $1 | runhaskell Interpreter.hs > ${res}
  awk "/output/, /end/" ${res} | tail -n +2 > ${res}_new
  new=${res}_new
  if [ -f "$out" ]; then
    dif="$(diff ${new} $out)"
    if [ "$dif" == "" ]; then
      echo "PASSED"
    else
      echo "FAILED"
    fi
  else 
    output="$(cat ${new})"
    if [ "$output" == "" ]; then
      echo "PASSED"
    else
      echo "FAILED"
    fi
  fi  
}

if [ "$1" == "" ]; then
  rm -rf out/*
  mkdir out/mixed
  mkdir out/assignexp
  find examples -iname *.jl -print | 
  while read i; do  
  run "$i"
  done
else
  #run "examples/$1.jl"
  cat examples/$1.jl | runhaskell Interpreter.hs
fi
