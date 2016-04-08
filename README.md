# unfoldable-restricted

A pretty simple alternative for Unfoldable, such that
we can add additional constraints to the contained elements.

```haskell
import qualified Data.Set as Set

instance UnfoldableR Ord Set.Set where
  -- ...
```
