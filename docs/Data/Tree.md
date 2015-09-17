## Module Data.Tree

#### `Tree`

``` purescript
data Tree a
  = Tree a (Array (Tree a))
```

##### Instances
``` purescript
instance showTree :: (Show a) => Show (Tree a)
instance eqTree :: (Eq a) => Eq (Tree a)
instance functorTree :: Functor Tree
instance applyTree :: Apply Tree
instance applicativeTree :: Applicative Tree
instance bindTree :: Bind Tree
instance monadTree :: Monad Tree
instance foldableTree :: Foldable Tree
instance traversableTree :: Traversable Tree
```

#### `flatten`

``` purescript
flatten :: forall a. Tree a -> NonEmpty Array a
```


