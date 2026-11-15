knapsack' :: forall name weight value . (Ix weight, Num weight, Ord value, Num value) =>
            [(name, weight, value)] -> weight -> value
knapsack' wvs c = table ! c
where
table :: Array weight value
table = tabulate (0,c) mknapsack
mknapsack :: weight -> value
mknapsack c = maximumElse 0 [ v + table ! (c - w) | (_,w,v) <- wvs , w <= c ]