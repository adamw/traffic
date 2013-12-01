module Util where

mergeSort: (a -> a -> Bool) -> [ a ] -> [ a ]
mergeSort lt l = 
  case l of
    [] -> []
    [ x ] -> [ x ]
    _ -> 
      let (l1, l2) = mergeSortPartition l [] []
          l1s = mergeSort lt l1
          l2s = mergeSort lt l2
      in  mergeSortMerge lt l1s l2s []

mergeSortPartition l p1 p2 = 
  case l of
    [] -> (p1, p2)
    hd :: tl ->  mergeSortPartition tl p2 (hd :: p1)

mergeSortMerge lt l1 l2 acc = 
  case (l1, l2) of
    ([], _) -> (reverse acc) ++ l2
    (_, []) -> (reverse acc) ++ l1
    (hd1 :: tl1, hd2 :: tl2) ->
      if (lt hd1 hd2) 
        then mergeSortMerge lt tl1 l2 (hd1 :: acc)
        else mergeSortMerge lt l1 tl2 (hd2 :: acc)

getOrElse: a -> Maybe a -> a
getOrElse def ma = case ma of
  Just x -> x
  Nothing -> def