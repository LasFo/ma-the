--this can be decomposed into an dependency graph with two connected components.
--the first component includes the 'readTVar t1' plus the if-then-else and the 
--second is the shift between t5 and t4.
transaction = do
  a <- readTVar t1
  if a < 5
     then writeTVar t2 19
     else writeTVar t3 17
  readTVar t5 **> writeTVar t4

--we assume that rollbacks are performed conservatively, which means 
--every read TVar is a critical TVar.
--connectivity is not the correct term rather than reachability.
transaction = do
  a <- readTVar t1                      --OP1
  if a < 5
     then readTVar t3 **> writeTVar t2  --OP2
     else readTVar t2 **> writeTVar t3  --OP3
  readTVar t5 **> writeTVar t4          --OP4


-------Graph----------
-- t1     t3       t5
-- ||     ||       ||
-- ||     \/       ||
-- \/  -> OP2      \/
--OP1              OP4
--     -> OP3
--        /\
--        ||
--        t2
--This graph indicates if t1 is modified the operation OP1 and the operation OP2 or OP3 need 
--to be reexecuted. WRONG no reexecution for OP2 and OP3 is needed. If the condition is
--unchanged nothing has to be done. If the condition changed the other operation needs 
--to be executed(for its first time).
--If on the other hand t3 is modified just OP2 (if it were executed) needs to be reexecuted.
--Thats why reachability and not connectivity is the desired property

--Another example:
transaction = do 
  a <- readTVar t1
  OP1 a
  b <- readTVar t2
  OP2 a b

-------Graph----------
-- t1     t2
-- ||     ||
-- ||     ||
-- \/     \/
-- OP1    OP2
--        /\
--        ||
--        t1
--This shows that if t1 is modified OP1 and OP2 need to be reexecuted.
--If t2 is modified it is enough to reexecute OP2.

--This shows we need a graph and a collection of elements that point to
--specific nodes of the graph. The overlaying collection contains one 
--element for each accessed TVar. This element points to the nodes
--that depend on this TVars value. Since the order in which the operation
--appear is important, the graph may no be the best data structure.
--An ordered data structure may be more appropriated.

--Theory:
--What are unnecessary recomputation?
--Some would say computations which result is the same.
--I would restrict it to computations which inputs are the same.
--Since Haskell in a referential transparent language same input always lead to same results.
--So computations with same input are a subset of computations with same results.
--What does this mean with regards to STM?
--Only readTVars can be invalidated, since they heavily depend on the actual state of
--the TVars. writeTVar on the other hand works independetly from the actual TVar 
--vlaues. But these operation may depend on readTVar operations, thus they are 
--transitively depend on the actual TVar values. In the following example R1 
--depends on t1, R2 on t2, R3 on t4, R4 on t4, R5 on t5, R6 on t5, and R7 on t2 or t4.
--There are more cases. The last two lines below show, that the 'readTVar t4' depends
--on the value of t5, but not on the value of t4.
--In more complex examples it can lead to the fact, that a single
--readTVar depends on multiple TVars rather a single one. 
--The scope of a value generated by readTVar is with a pure Applicative implementation
--tangible, but with Monad this value may be extracted and distributed to all following
--operations. Thus potentially all operations need to be reexecuted. Whereas with a 
--readTVar without bind the value effects at most all operation until the
--next '>>' or '*>'. In the following I will denote inseparable action as chunks.
--The user may define separable sequences as inseparable 
--(e.g. writeTVar t1 5 >>= (\_ -> readTVar t2). A chunk may contain subchunks.
--This is the case for branch mechanisms, namely if-then-else, case, 
--function (whose result are STM actions that are executed). To reexecute a chunk
--that contains subchunks mean not necessarily to recompute all subchunks. In the case
--of an if-then-else expression we need to compute one subchunk. In the example below 
--the chunk1.5 has the subchunks 2, 3, and 4. Chunks can depend either on TVar or on 
--other chunks. In this case chunk1 depends on TVar t1, chunk2 on TVar t2, chunk3
--on TVar t4, chunk4 on TVar t4, chunk5 und TVar t5, and chunk6 on chunk5 and chunk2. 
--Note that no transitive dependencies are resolved here. Thus chunk6 depends not on 
--TVar t1, even though the value of t2 depends on the if-then-else condition which on
--the other hand depends on the value of t1. They may also depend on nothing, like
--chunk8. Usually those operations do not make much sense and therefore occur rarely.
--retry depends on the implementation. In the case of direct notification the rexecution
--is unnecessary, because it just returns the same stuff. In the case of validation, 
--retry also enters its retry MVar into the read TVars which means this has to be 
--reexecuted.

transaction = do 
  a <- readTVar t1                         --R1 chunk1
  if p a                                   --chunk1.5
    then do readTVar t2 **> writeTVar t3   --R2 chunk2
            readTVar t4 **> writeTVar t2   --R3 chunk3
    else readTVar t4 **> writeTVar ts3     --R4 chunk4
  readTVar t5 **> writeTVar t4             --R5 chunk5
  writeTVar t8 "hello"                     --chunk8
  fmap (,) (readTVar t4) <*> readTVar t2   --R6,R7 chunk6

--Dependencies:
--Chunkname | depends on TVar | depends on chunk | has subchunk
--chunk1    | t1              |                  |
--chunk2    |                 | chunk1           | chunk3, chunk4, chunk7, chunk9, chunk10
--chunk3    | t2              |                  |
--chunk4    |                 | chunk3           | chunk5, chunk6
--chunk5    |                 | chunk3           | 
--chunk6    |                 | chunk3           |
--chunk7    | t3              |                  |
--chunk9    | t4              |                  |
--chunk10   |                 | chunk9           | chunk11, chunk12
--chunk11   |                 | chunk2, chunk9   | 
--chunk12   |                 |                  |
--chunk13   | (t1)            | chunk 11, chunk7,| 
--          |                 | chunk6, chunk5   |
transaction2 = do 
  a <- readTVar t1                              --chunk1
  case a of                                     --chunk2
    Bla   -> do b <- readTVar t2                --chunk3
                if p b                          --chunk4, p is a predicate
                  then writeTVar t1 b           --chunk5
                  else wirteTVar t1 (h b)       --chunk6
    Blub  -> readTVar t3 **> writeTVar t1       --chunk7
    Bob x -> do b <- readTVar t4                --chunk9
                if q b                          --chunk10, q is a predicate
                  then writeTVar t1 (x b)       --chunk11
                  else retry                    --chunk12
  readTVar t1                                   --chunk13

--When transactions commit their changes they invalidate parts of other transactions, by 
--modifying certain TVars. If a TVar is modified, all chunks which depend on this TVar 
--need to be reexecuted. Since chunks may depend on other chunks a reexecution of
--indirectly effected chunks is needed. This must be continued recursively until
--no more chunks are invalidated. 
--If a chunk is reexecuted, which has subchunks, it is necessary to study these subexpression.
--It is possible that these subexpression were allready executed and none of the values it 
--depends on were changed. Thus no reexecution is needed, but if another subexpression is
--demanded an execution is enforced. In theory some could think of caching all the 
--execution for the case that the transaction is rolled back multiple times. This could
--be possible for chunks that depend on a single chunk/TVar even if it is expensive, but 
--when the chunk depends on multiple chunk/TVar all the input combination would have to
--be stored and matched in the case an execution is requested by a superchunk. This overhead
--is most likely not worth the saved computation time.
--So I suggest that just the last execution is saved. This computation can be invalidated
--at any time, so if the execution is requested, the system can check if the result is still 
--valid and if so use it, otherwise it could evoke the reexecution.
--Another approach could be to recursively invalidate the chunks, when a TVar is changed.
--Another important issue is the order of reexecution which needs to be kept in mind.

problem = do 
  writeTVar t1 "hallo"          --chunk1
  a <- readTVar t2              --chunk2
  if p a                        --chunk3
    then writeTVar t1 "welt"    --chunk4
    else return ()
  readTVar t1

--This is a problem with the current implementation. Imagine a run where this transaction
--executes all of its operation and (p a) == True holds. Thus the state of the transaction
--holds in its writeSet the information that "hello" was written to t1. This means the
--transaction has no information that chunk1 was executed. Thus if t2 is modified and 
--with it p a the transaction has to reexecute certain chunks. With the aforemention 
--technique this would mean reexecuting chunk2 (since p a == False) and not chunk4.
--Before reexecution, the state has to be modified in order to undo the changes by 
--the invalid chunks. The current implementation is not able to handle this.
--The reason is that chunks can overwrite each other. For example chunk4 overwrites
--the changes of chunk1.

--Chunk may also depend on parameters. This is important to maintain one of
--the most important features of STM, composability. 

fun t1 val =
  a <- readTVar t1            --chunk1
  if p a                      --chunk2
    then writeTVar t1 val     --chunk3
    else return ()            --chunk4

--In this example chunk3 depends on val. val is an input parameter of fun.


------------------------------------------
----------Implementation Idea-------------
------------------------------------------
--Since the ordered execution and partial reexecution contradict each other, I will 
--present an idea which reexecutes all, but still save some time. 
--In the current implementation execution of chunks means to modify the state.
--Thus generally a chunk is a function of type (StmState -> StmState). The first time 
--a chunk is executed a function of type (StmState -> StmState) is generated and 
--applied to the current state. In addition this function is stored. If the 
--transaction rolls back, the the unchanged chunks can use the allready calculated 
--functions instead of constructing this function again. So in the reexecution only 
--the manipulation of the state is done again, while the interpretation of the STM 
--operations is cached.

-- action1 >> action2
--Since >> is the seperator of chunks, this operator needs to log the information.
--The problem is, that this operator has no information about the actual actions 
--and thus aobut their dependencies which are the most important information
--writeTVar and readTVar could enter their dependencies in a field like
--"current chunk". When ever a chunk reaches its end (i.e. reaching >>)
--this chunk and its dependencies are entered into the STMState.
--Is it possible for writeTVar and readTVar to determine their dependencies?

--The problem is similar to the 'unnecessary rollback' problem in terms that the 
--bind operator is the core of this problem. If we use it to extract an value 
--from the STM context, we lose track of the dependencies of this value. The
--library cannot determine the origin of the value, since it is a unboxed 
--and pure value, thus no context information are available. 

transaction t1 t2 = do
  a <- readTVar t1
  if p t1
    then do wirteTVar t1 (f a)
            transaction t1 t2
    else writeTVar t2 (g a)

--A dynamic analysis won't work because of the stated problems with bind. 
--A staic analysis gets problems with recursion. You cannot generate
--code that covers (unlimited) recursion. One specific problem is
--the fact that chunks may depend on input parameters. These may 
--depend on something the caller binded or something the callee binded 
--before it evoked a recursive call. If there is more than one way
--the recursive call can be evoked, it is not clear at compile time, 
--which of these are acually used at run time, becuase it depends 
--on the state of the transactional system. Without recursion an easy
--approach would be to inline all code functions and then use a
--static analysis.


