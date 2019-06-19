#!/usr/bin/env bash

### Book
rm -rf /Users/joe/Documents/programmingbitcoinscala/*html
rm -rf /Users/joe/Documents/programmingbitcoinscala/*ascii
find /Users/joe/Documents/programmingbitcoin/ -name \*.asciidoc -exec asciidoctor {} \;
find /Users/joe/Documents/programmingbitcoin/ -name \*.html -exec cp {} /Users/joe/Documents/programmingbitcoinscala/public \;

### Images
rm -rf /Users/joe/Documents/programmingbitcoinscala/public/images/*
find /Users/joe/Documents/programmingbitcoin/ -name \*.png -exec cp {} /Users/joe/Documents/programmingbitcoinscala/public/images \;
