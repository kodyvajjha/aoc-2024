#!/bin/bash 

mkdir ./bin/$1 
cd ./bin/$1
touch $1.ml
touch dune 
touch input.txt 

echo "let test = {||} module Solving = struct end module Printing = struct end let () = ()" > $1.ml

echo "(executable
 (public_name $1)
 (preprocess (pps ppx_deriving.show))
 (name $1)
 (libraries aoclib containers))" > dune 

 dune build