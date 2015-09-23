## Module Data.Tree.Fan

#### `Fan`

``` purescript
data Fan a
```

A fan is a finitarily-branching spread (i.e. a non-wellfounded tree where
each node has only finitely many immediate children).

#### `View`

``` purescript
data View f a
  = View a (Array (f a))
```

A view which is used to construct and destruct the fan. This type is a
pattern functor/signature, where `f` represents the recursive structure.

#### `into`

``` purescript
into :: forall a. View Fan a -> Fan a
```

#### `out`

``` purescript
out :: forall a. Fan a -> View Fan a
```

#### `intoLazy`

``` purescript
intoLazy :: forall a. View (Compose Lazy Fan) a -> Fan a
```

#### `outLazy`

``` purescript
outLazy :: forall a. Fan a -> View (Compose Lazy Fan) a
```

#### `transView`

``` purescript
transView :: forall f g a. (forall b. f b -> g b) -> View f a -> View g a
```
