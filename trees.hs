-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- data List' a = Empty | Cons {listHead :: a, listTail :: List' a} deriving (Show, Read, Eq, Ord)

-- binary tree:
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

singleton x = Node x EmptyTree EmptyTree

treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
  | x==y = Node y left right -- no duplicate keys
  | x<y = Node y (treeInsert x left) right
  | x>y = Node y left (treeInsert x right)

treeElem x EmptyTree = False
treeElem x (Node y left right)
  | x==y = True
  | x<y  = treeElem x left
  | x>y  = treeElem x right


