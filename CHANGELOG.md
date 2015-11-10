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
