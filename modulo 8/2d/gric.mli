type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val preorder : 'a tree -> 'a list
val postorder : 'a tree -> 'a list
val inorder : 'a tree -> 'a list
