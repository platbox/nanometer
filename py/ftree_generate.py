#!/usr/bin/env python3
import itertools


def print_header(x, y, z = None):
    print("join_digits(", seq2digit(x), ", ", seq2some(y), ", ", seq2digit(z), ") ->", sep="")


def produce(seq):
    while seq:
        if len(seq) == 4:
            yield seq2node(seq[:2])
            yield seq2node(seq[2:])
            break
        yield seq2node(seq[:3])
        seq = seq[3:]


def print_body(seq):
    print("    ", seq2some(produce(seq)), ";", sep="")


def seq2digit(seq):
    return "{{{}}}".format(", ".join(itertools.chain("_", seq)))


def seq2some(seq):
    return "{{{}}}".format(", ".join(seq))


def seq2node(seq):
    return "node({})".format(", ".join(seq))

print("-spec join_digits(digit(X), some(X), digit(X)) -> some(node(X)) when X :: desc().")
var_x = 'ABCD'
var_some = 'EFGH'
var_z = 'IJKL'
MAX = 12
MAXONE = 4
for size_some in range(0, MAXONE + 1):
    for size_x in range(1, MAXONE + 1):
        for size_z in range(1, MAXONE + 1):
            x, some, z = var_x[:size_x], var_some[:size_some], var_z[:size_z]
            print_header(x, some, z)
            print_body(x + some + z)
print("join_digits(Left, Some, Right) -> error(function_clause, [Left, Some, Right]).")
