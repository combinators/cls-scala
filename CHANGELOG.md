### Version 2.0.1
This version contains minor patches.
- Method `removeEntriesWithArgument` is now non-final to allow logging in a debugger
  inheriting from `FiniteCombinatoryLogic`
### Version 2.0.0
This is the first public release of cls-scala.
The most notable changes to prior (internal development) versions are:
- The namespace is now `org.combinators`.
- `BoundedCombinatoryLogic` accepts any finite space of substitutions instead of just Kindings for single variables.
- The newsticker example is now just a console instead of a play application (this minimizes dependencies).
- The project directory layout has been flattened (`core/src` is now `src`).
