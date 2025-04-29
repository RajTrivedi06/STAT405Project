#!/bin/bash
PART=$1
Rscript process_chunk.R $PART output_$PART.csv

