#!/bin/bash

latexmk -e '$pdflatex=q/pdflatex %O -shell-escape %S/' -pdf

