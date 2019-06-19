#!/usr/bin/env bash

for filename in *.asciidoc; do
  #[ "$filename" -e ] || continue
  echo "$filename"
  asciidoctor "$filename"
done
