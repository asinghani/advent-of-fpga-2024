#!/bin/bash

# Embed all of the inputs into one text file to allow embedding it into the
# OCaml input parser binaries

for file in *.txt; do
    b64=$(base64 -w 0 "$file")
    echo "$file $b64"
done
