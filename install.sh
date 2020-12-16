#!/bin/bash
for f in $(find . -type f | grep -v git); do cp $f $HOME/$(echo $f | sed 's/\.\///g'); done
