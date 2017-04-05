a1 <- readTVar acc1
a2 <- readTVar acc2
writeTVar acc1 (am - 50)
writeTVar acc2 (am + 50)
