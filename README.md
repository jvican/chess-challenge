# Chess Challenge
The work presented in this repo is my solution to the Chess challenge. The chess
challenge is a generalization of the n-queen problem, in which any group of chess
pieces has to be arranged in a board. The arrangement must be done so that any
piece cannot attack another one. 
  
This is a well-know combinatorial problem and I have strived to give an efficient
functional solution, with a minimum of penalties in the performance. 
I still consider that this solution could be further improved and there are
some areas in which other alternative solutions should be tried (and profiled). I haven't
finished my solution due to the deadline of one week and my personal shortage of time,
as I am in my 4th year of CS Bachelors.
  
## Details of the solution
This solution is slitted up into two solvers: the **asynchronous** (using `Futures`) 
and the **synchronous**. The first one is meant to be used to problems of large size
and the second one to problems of small size. The overhead of the asynchronous
solver is noticeable for small problems. However, it outperforms the synchronous
one in large problems, despite being mostly cpu-intensive operations. A good thing
about the synchronous one is that it is lazy, one can obtain the results just
asking them, as it is implemented using `Iterator`s.
  
I have used extensively *value classes*, as they avoid object allocation. As this
problem needs to do a huge amount of little computations, the overhead of creating
a new object for each one is high.

In some cases, I use *implicit value classes* as a way to give a more idiomatic
and clear programming style, as they allow me to use `defs` to call fields
of the tuples, improving the readibility.

I also use *inlining* to allow the compiler to inline operations and avoid function
invocations. I haven't checked that the compiler is indeed doing this transformation,
as sometimes it cannot, but in most of the cases it is probably performed.
  
One important step of my solution is the cut of a big part of the search space.
I wanted to avoid take `Decision`s (`List` of `(Piece, Cell)`) which have already been
tried before, so I use a `mutable.Set` in which I store every path that has been
taken. It is mutable because it is the only way to share state between different
`Iterators`. 

Once we have that set, we have to store decisions and our set should be able to
identify `List((Queen, (1,3)), (King, (2,3)))` as equal to `List((King, (2,3)), (Queen, (1,3)))`. 
The problem is that the hashing of a list is ordered, thus for the `Set` both 
of them are distinct. Only `Set` has an unordered hash. 
In order to avoid this, one can define a `Decision` as a `Set` of
moves, but this definition affects the performance of the solution, as it is not
using structural sharing and getting profit from the O(1) prepend cost of the `List`.
Another approach would be to sort each `Decision` before checking the `Set` but
this affects performance as well. So, finally, I decided to create an implicit class of `List` (named `RichList`)
which is indeed allocating a new object but allows me to override `equals` and `hashCode`,
defining them so that it doesn't take into account the ordering of the elements
in that `List`. Hence, defining `Decision` as `RichList[Move]`. This solution does 
not almost affect performance. 
  
Finally, I will explain my implementation of the asynchronous version. This version
could be improved by doing a better distribution of the workload.
My naive approach has been to split up the search space, creating a `Future` for each
row of the matrix. This distribution is only done for the first decision. Hence,
if we try to solve the 8-queen problem, my algorithm will have 8 Futures, one of
each will explore the decisions that have in the first positions all the columns
of that row. Here, another problem came up. I needed a lock-free way to synchronize
the access to the set that marks a decision as visited. Then, I used a `TrieMap`,
the only concurrent collection in Scala, and have defined an implicit wrapper
that gives me from that `TrieMap` a dumb `TrieSet`, hence solving the problem.
  
## Test suite of the solvers
The test suite of the solver has been implemented with `scalatest` and `scalacheck`.
In fact, I generate possible problems and my tests check that the solutions given
are correct. They don't check that this solver is giving all the possible solutions.
I could have done a specific check for the examples presented in the challenge but
I hadn't time, so I check them out manually.

## Notes
As the values classes cannot be defined in a trait or class, I couldn't organized
the code in a better way. I wanted to create a `shared` package with all the
type definitions and the model, but I couldn't do it because of that value
class limitation. Consequently, the model is replicated in both packages with
minor changes between them.
  
**jvican** @ `jorgevc@fastmail.es` - *Nov 14 2015*
