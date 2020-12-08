type btree = 
  | Leaf of int
  | Left of btree
  | Right of btree
  | LeftRight of btree * btree

let rec mirror : btree -> btree
= fun tree -> match tree with
  |Leaf n -> Leaf n
  |Left btree' -> Right (mirror btree')
  |Right btree' -> Left btree'
  |LeftRight (btree1, btree2) -> LeftRight (mirror btree2, mirror btree1);;
  
  
(*mirror (Left (LeftRight (Leaf 1, Left (Leaf 3))));;*)
    

