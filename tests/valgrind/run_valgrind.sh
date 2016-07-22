#!/bin/bash
R -d "valgrind --tool=memcheck --leak-check=full --track-origins=yes" --vanilla < src_tests.R
