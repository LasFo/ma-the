--Here I will assume the dependecies are correctly resolved. I will 
--now explore in which form these dependencies need to be stored and
--how they are used to induce the partial recalculation.

--I need a data structure with cross refrences. A graph-like structure works.
--The structure contains a node for each chunk. The directed edges are the 
--dependecies between these chunks. Furthermore we can include nodes for 
--each TVat that was read during the process, because these TVars are needed
--to initialize an partial rollback. TVar are the only objects that may be
--modified by another thread, thus invalidate the current transaction.
--Additionally this structure needs to hold an order on the chunk in order 
--to be available to reexecute the chunk in the correct order.
--(I need to make some thoughts to find out if it is possible to 
--mark an reexecute in the same traversale or if I have to do multiple
--traversales to keep the correct order of execution. This would lead
--to the problem that multiple execution are still an issue)

--If a TVar is modified I need to perform a reachability check for the 
--changed TVAr. I will not return a subgraph of the inital graph. I will
--mark the chunks that need to be reexecuted.

transaction = do
  a <- readTVar t1
  if p a 
    then writeTVar t2 b         --chunk3
    else writeTVar t3 c
  readTVar t2 **> writeTVar t1  --chunk5
--assume in the firstrun p a does not hold then chunk5 depends on nothing,
--but if t1 is modified and thus t2 is changed, chunk5 suddently depends on
--chunk3. Statically it depended on chunk3 from the very beginning, but static
--analysis is not suitable.



changing dependencies
branches (subchunks). How to represent? 
undoing chunks
interaction with atomically
