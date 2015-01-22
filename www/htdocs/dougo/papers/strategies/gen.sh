#! /bin/sh

latex strategies
bibtex strategies
latex strategies
latex strategies
dvips -t letter strategies -o strategies.ps
dvipdfm strategies
