## Module Data.Tree

This module defines a strict rose tree.

#### `Tree`

``` purescript
data Tree a = Tree a [Tree a]
```

#### `flatten`

``` purescript
flatten :: forall a. Tree a -> NonEmpty Array a
```
