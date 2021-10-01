### Version 3.0.1
- Enumeration of results on single core computers fixed
- Updated dependencies and compiled with newest Scala versions
- Tested to work with Java 17
- CI-Pipeline maintenance
### Version 3.0.0
This version replaces the algorithms by the version published in [A Type-Theoretic Framework for Software Component Synthesis](http://dx.doi.org/10.17877/DE290R-20320).
- Constructors are now unary with products for arity > 1
- Domain restrictions on substitution spaces allow to avoid duplication in types
- The algorithm is based on relational semantics of multiple specialized machine models
- Results are now real regular tree grammars without arity violations
- A long standing bug concerning the inhabitation of type omega is fixed
- Several dependencies have been updated
- Compatibility with Scala 2.13 is added
- Symbol syntax for constructors is deprecated
### Version 2.1.0
This version contains minor patches.
- Method `removeEntriesWithArgument` is now non-final to allow logging in a debugger
  inheriting from `FiniteCombinatoryLogic`
- Various performance improvements
- Method `addOption` of FiniteSubstitutionSpace` allows to add an allowed substitution,
  where previously the only way to allow substitutions was via per-variable kinding.
- Some sbt/version updates, including migration of [shapeless-feat](https://github.com/combinators/shapeless-feat) to `combinators.org`
- Better pretty printing of constructors `C(x, y)` instead of `C(x,y)`
### Version 2.0.0
This is the first public release of cls-scala.
The most notable changes to prior (internal development) versions are:
- The namespace is now `org.combinators`.
- `BoundedCombinatoryLogic` accepts any finite space of substitutions instead of just Kindings for single variables.
- The newsticker example is now just a console instead of a play application (this minimizes dependencies).
- The project directory layout has been flattened (`core/src` is now `src`).
