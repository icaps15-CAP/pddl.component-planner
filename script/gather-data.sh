#! /bin/bash

DATE=$(date +%Y-%m-%d-%H-%M-%S)
tar cvzf lisp-output.$DATE.tar.gz ~/Dropbox/lisp-output/
rm -r ~/Dropbox/lisp-output/
