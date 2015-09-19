## Module Data.Tree

#### `Tree`

``` purescript
data Tree a
```

An abstract tree type

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

#### `View`

``` purescript
data View f a
  = View a (Array (f a))
```

A view which is used to construct and destruct the tree. This type is a
pattern functor/signature, where `f` represents the recursive structure.

#### `out`

``` purescript
out :: forall a. Tree a -> View Tree a
```

Pattern match on a tree. Example:

````purescript
case out t of
  View a ts -> ...
````

#### `into`

``` purescript
into :: forall a. View Tree a -> Tree a
```

Construct a tree from a tree pattern/view. Example:

````purescript
tree :: Tree Int
tree = into $ View 0 []
````

#### `transView`

``` purescript
transView :: forall f g a. (forall a. f a -> g a) -> View f a -> View g a
```

Map a tree across a natural transformation.

#### `flatten`

``` purescript
flatten :: forall a. Tree a -> NonEmpty Array a
```


