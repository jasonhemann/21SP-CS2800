from z3 import *

# https://0hh1.com/
# https://news.ycombinator.com/item?id=14622073

# we use '-1' for empty
instance  = ((-1,-1,-1,-1,-1, 1,-1,-1,-1, 1),
             ( 1,-1,-1,-1,-1,-1,-1, 0,-1,-1),
             (-1,-1, 0,-1,-1,-1,-1, 0,-1,-1),
             (-1, 0, 0,-1,-1,-1, 0,-1,-1, 1),
             ( 1,-1,-1,-1,-1,-1,-1,-1,-1, 1),
             (-1,-1,-1, 0,-1,-1, 1,-1,-1,-1),
             ( 0,-1,-1,-1,-1, 1,-1,-1,-1,-1),
             (-1,-1,-1,-1,-1,-1,-1, 0,-1, 0),
             ( 0,-1,-1,-1,-1,-1,-1,-1,-1, 0),
             (-1, 0,-1, 0,-1, 1,-1,-1,-1,-1))

size = len(instance)

# we could use bitvecs here too
X = [ [ Int("x_%s_%s" % (i+1, j+1)) for j in range(size) ] for i in range(size) ]

# each cell is a 1 or a 0
cells_c  = [ And(0 <= X[i][j], X[i][j] <= 1) for i in range(size) for j in range(size) ]

# each row contains as many 1s as 0s
rows_c   = [ Sum(X[i]) == size / 2 for i in range(size) ]

# each column contains as many 1s as 0s
cols_c   = [ (Sum([ X[i][j] for i in range(size) ]) == size / 2)
             for j in range(size) ]

# each cell can only have 2 neighbors sharing it's number.
rows_not_alike = [ If(X[i][j] == X[i][j-1], X[i][j+1] != X[i][j-1], True) for i in range(size)
                   for j in range(size-1) ]

cols_not_alike = [ If(X[i][j] == X[i-1][j], X[i-1][j] != X[i+1][j], True) for i in range(size-1)
                   for j in range(size) ]

puzzle_c = cells_c + rows_c + cols_c + rows_not_alike + cols_not_alike

instance_c = [ If(instance[i][j] == -1, True, X[i][j] == instance[i][j]) for i in range(size) for j in range(size) ]

s = Solver()
s.add(puzzle_c + instance_c)
# print(s.to_smt2())
if s.check() == sat:
    m = s.model()
    r = [ [ m.evaluate(X[i][j]) for j in range(size) ] for i in range(size) ]
    print_matrix(r)
else:
    print("failed to solve")

# Generates:
#   [[0, 1, 0, 1, 0, 1, 0, 1, 0, 1],
#    [1, 0, 1, 0, 1, 0, 1, 0, 1, 0],
#    [0, 1, 0, 1, 1, 0, 1, 0, 1, 0],
#    [1, 0, 0, 1, 0, 1, 0, 1, 0, 1],
#    [1, 0, 1, 0, 1, 0, 0, 1, 0, 1],
#    [0, 1, 1, 0, 1, 0, 1, 0, 1, 0],
#    [0, 1, 0, 1, 0, 1, 0, 1, 0, 1],
#    [1, 0, 1, 0, 0, 1, 1, 0, 1, 0],
#    [0, 1, 0, 1, 1, 0, 0, 1, 1, 0],
#    [1, 0, 1, 0, 0, 1, 1, 0, 0, 1]]
