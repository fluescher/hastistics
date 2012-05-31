module Hastistics.Fields where

import Hastistics.Types

{- | Static HSField implementation which holds a static value which cannot be updated. -}
data HSStaticField  = HSStaticField HSValue
instance HSField HSStaticField where
    val (HSStaticField v)   = v
    update f _              = f
instance Show HSStaticField where
    show = showField

{- | HSField where the last update call's value is stored. -}
data HSValueOfField = HSValueOfField Key HSValue
instance HSField HSValueOfField where
    meta    (HSValueOfField k _)       = "Value of " ++ k
    val     (HSValueOfField _ v)       = v 
    update  (HSValueOfField h None) r  = HSValueOfField h (fieldValueOf h r)
    update  f _                        = f
instance Show HSValueOfField where
    show = showField

{- | HSField which calculates the average of a column. -}
data HSAvgField = HSAvgField Key HSValue Int
instance HSField HSAvgField where
    meta    (HSAvgField k _  _  )   = "Average of " ++ k
    val     (HSAvgField _ su cnt)   = su Hastistics.Types./ HSDouble (fromIntegral cnt)
    update  (HSAvgField h su cnt) r = HSAvgField h sm newCnt
                                    where sm     = (Hastistics.Types.+) su (fieldValueOf h r)
                                          newCnt = (Prelude.+) 1 cnt
instance Show HSAvgField where
    show = showField

{- | HSField which count the amount of rows in this result. -}
data HSCountField = HSCountField HSValue
instance HSField HSCountField where
    meta    _                  = "Count"
    val     (HSCountField v)   = v
    update  (HSCountField v) _ = HSCountField (v Hastistics.Types.+ HSInteger 1)

instance Show HSCountField where
    show = showField

{- | HSField which calculates the sum of a column -}
data HSSumField = HSSumField Key HSValue
instance HSField HSSumField where
    meta    (HSSumField k _)   = "Sum of " ++ k
    val     (HSSumField _ v)   = v
    update  (HSSumField h v) r = HSSumField h ((Hastistics.Types.+) v (fieldValueOf h r))

instance Show HSSumField where
    show = showField

{- | HSField which stores the minimum value of a column. -}
data HSMinField = HSMinField Key HSValue
instance HSField HSMinField where
    meta    (HSMinField k _)   = "Min of " ++ k
    val     (HSMinField _ v)   = v
    update  (HSMinField h v) r = HSMinField h (min v (fieldValueOf h r))

instance Show HSMinField where
    show = showField

{- | HSField which stores the maximum value of a column. -}
data HSMaxField = HSMaxField Key HSValue
instance HSField HSMaxField where
    meta    (HSMaxField k _)   = "Max of " ++ k
    val     (HSMaxField _ v)   = v
    update  (HSMaxField h v) r = HSMaxField h (max v (fieldValueOf h r))

instance Show HSMaxField where
    show = showField
