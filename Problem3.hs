bknapsack :: forall name weight value . (Ord weight, Num weight, Ord value, Num value) =>
[(name, weight, value)] -> weight -> (value, [name])
bknapsack wvs c = maximumElse 0 [ v + knapsack (a | a <- wvs, a != (n,w,v)) (c - w) | (n,w,v) <- wvs , w <= c ]