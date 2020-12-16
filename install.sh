#!/bin/bash
for f in $(find -type f | grep -v git); do cp $f ~/; done
