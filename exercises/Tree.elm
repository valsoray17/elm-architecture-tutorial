{- OVERVIEW ------------------------------------------------------

A "Tree" represents a binary tree. A "Node" in a binary tree
always has two children. A tree can also be "Empty". Below I have
defined "Tree" and a number of useful functions.

This example also includes some challenge problems!

-----------------------------------------------------------------}


import Html exposing (Html, div, text)
import Html.Attributes exposing (style)



-- TREES


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


empty : Tree a
empty =
    Empty


singleton : a -> Tree a
singleton v =
    Node v Empty Empty


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
      Empty ->
          singleton x

      Node y left right ->
          if x > y then
              Node y left (insert x right)

          else if x < y then
              Node y (insert x left) right

          else
              tree


fromList : List comparable -> Tree comparable
fromList xs =
    List.foldl insert empty xs


depth : Tree a -> Int
depth tree =
    case tree of
      Empty -> 0
      Node v left right ->
          1 + max (depth left) (depth right)


map : (a -> b) -> Tree a -> Tree b
map f tree =
    case tree of
      Empty -> Empty
      Node v left right ->
          Node (f v) (map f left) (map f right)


sum : Tree number -> number
sum tree =
  case tree of
    Empty ->
      0
    Node v left right ->
      v + sum left + sum right


flatten : Tree a -> List a
flatten tree =
  case tree of
    Empty ->
      []
    Node v left right ->
      v :: flatten left ++ flatten right


isElement : a -> Tree a -> Bool
isElement a tree =
  case tree of
    Empty ->
      False
    Node v left right ->
      v == a || isElement a left || isElement a right


fold : (a -> b -> b) -> b -> Tree a -> b
fold fn b tree =
  case tree of
    Empty ->
      b
    Node v left right ->
      let
        rightFold = fold fn b right
      in
        fold fn (fn v rightFold) left


-- PLAYGROUND


deepTree =
  fromList [1,2,3,4]


niceTree =
  fromList [4,2,1,3]


main =
  div [ style [ ("font-family", "monospace") ] ]
    ([ display "depth deepTree" (depth deepTree)
    , display "depth niceTree" (depth niceTree)
    , display "incremented" (map (\n -> n + 1) niceTree)
    , display "sum niceTree" (sum niceTree)
    , display "flatten niceTree" (flatten niceTree)
    , display "flatten deepTree" (flatten deepTree)
    , display "isElement 3 deepTree" (isElement 3 deepTree)
    , display "isElement 4 deepTree" (isElement 4 deepTree)
    , display "fold toString deepTree" (fold (\n r -> toString n ++ r) "" deepTree)
    , display "fold toString niceTree" (fold (\n r -> toString n ++ r) "" niceTree)
    , display "fold sum niceTree" (fold (+) 0 niceTree)
    , display "fold toList niceTree" (fold (::) [] niceTree)
    , display "fold toList deepTree" (fold (::) [] deepTree)
    ] ++ summary1)


display : String -> a -> Html msg
display name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]


summary1 : List (Html msg)
summary1 =
  let sum         = fold (+) 0
      flatten     = fold (::) []
      isElement e = fold ((==) e >> (||)) False
  in  [ display "fold: sum t1"         (sum niceTree)
      , display "fold: flatten t1"     (flatten niceTree)
      , display "fold: isElement 1 t1" (isElement 1 niceTree)
      , display "fold: isElement 2 t1" (isElement 2 niceTree)
      , display "fold: isElement 3 t1" (isElement 3 niceTree)
      , display "fold: isElement 4 t1" (isElement 4 niceTree)
      ]



{-----------------------------------------------------------------

Exercises:

(1) Sum all of the elements of a tree.

       sum : Tree number -> number

(2) Flatten a tree into a list.

       flatten : Tree a -> List a

(3) Check to see if an element is in a given tree.

       isElement : a -> Tree a -> Bool

(4) Write a general fold function that acts on trees. The fold
    function does not need to guarantee a particular order of
    traversal.

       fold : (a -> b -> b) -> b -> Tree a -> b

(5) Use "fold" to do exercises 1-3 in one line each. The best
    readable versions I have come up have the following length
    in characters including spaces and function name:
      sum: 16
      flatten: 21
      isElement: 46
    See if you can match or beat me! Don't forget about currying
    and partial application!

(6) Can "fold" be used to implement "map" or "depth"?

(7) Try experimenting with different ways to traverse a
    tree: pre-order, in-order, post-order, depth-first, etc.
    More info at: http://en.wikipedia.org/wiki/Tree_traversal

-----------------------------------------------------------------}
