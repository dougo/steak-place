#! /bin/sh

TEXINPUTS=:/usr/share/racket/collects/tex2page latex dougo-resume && dvipdfm dougo-resume && tex2page dougo-resume
