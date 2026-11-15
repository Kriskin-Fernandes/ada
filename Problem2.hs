knapsack'':: forall name weight value .(Ix weight, Num weight, Ord value, Num value) =>
[(name, weight, value)] -> weight -> (value, [name])
knapsack'' wvs c = table ! c
where
table :: Array weight (value, [name])
table = tabulate (0,c) mknapsack
mknapsack :: weight -> (value, [name])
    mknapsack c = maximumBy (compare `on` fst) ((0, []) :
                    [ (v + v', n : ns) | (n,w,v) <- wvs, w <= c, let (v', ns) = table ! (c - w) ])