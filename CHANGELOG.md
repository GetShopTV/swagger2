0.4.1
---
* Fixes:
    * Use `PackageImports` for `Data.HashSet` to avoid test failure on stackage (see [#15](https://github.com/GetShopTV/swagger2/issues/15));
    * Add an upper version bound for `aeson` due to `aeson-0.10.0.0` bug (see [bos/aeson#293](https://github.com/bos/aeson/issues/293));
    * Switch to Cabal-based multi GHC Travis config.

0.4
---
* Remove `Swagger`/`swagger` prefixes;
* Add `ToSchema` type class with default generic implementation;
* Add configurable generic `ToSchema` helpers;
* Add `doctest` test suite;
* Fixes:
    * Fix `HasSchemaCommon` instance for `Schema`;
    * Change `minimum`, `maximum` and `multipleOf` properties to be any number,
      not necessarily an integer;
    * Fix all warnings

0.3
---
* Fixes:
    * Fix `SwaggerMonoid Text` instance;
    * Wrap `Bool` in `Maybe` everywhere;
    * These changes make all `Data.Swagger` `Monoid` instances obey monoid laws
      (previously right identity law was broken by some instances).

0.2
---
* Add `Data.Swagger.Lens`
* Support references
* Fixes:
    * Fix `FromJSON SwaggerHost` instance
    * Add missing `Maybe`s for field types
    * Decode petstore swagger.json successfully
