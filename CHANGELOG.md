1.0
---
* Major changes:
    * Add `Data` and `Typeable` instances for `Data.Swagger` types;
    * Merge `ParamType`/`ItemsType`/`SchemaType` into `SwaggerType` GADT;
    * Merge collection format types into `CollectionFormat` GADT;
    * Introduce `SwaggerItems` GADT, replacing `Items` and `SchemaItems` in `ParamSchema` (see [#24](https://github.com/GetShopTV/swagger2/pull/24));
    * Move type, format and items fields to `ParamSchema` (former `SchemaComon`);
    * Prepend reference path automatically (see [commit 49d1fad](https://github.com/GetShopTV/swagger2/commit/49d1fadd2100644e70c442667180d0d73e107a5f))
      and thus remove `"#/definitions/"` from user code, leaving much clearer `Reference "Name"`;
    * Change `Data.Swagger.Schema` (see [#19](https://github.com/GetShopTV/swagger2/pull/19)):
        * Change the only method of `ToSchema` to `declareNamedSchema` which should produce a `NamedSchema`
          along with a list of schema definitions used to produce it;
        * Add `declareSchema`, `declareSchemaRef`;
        * Replace `genericTo*` helpers with `genericDeclare*` helpers;
        * Add `paramSchemaTo[Named]Schema` helpers to facilitate code reuse for primitive schemas;
        * Add helpers for inlining `Schema` references dynamically (see [#23](https://github.com/GetShopTV/swagger2/pull/23)).
    * Add `ToParamSchema` class (see [#17](https://github.com/GetShopTV/swagger2/pull/17)) with
        * generic default implementation and
        * instances for some base types compliant with `http-api-data` instances;
    * Add `Data.Swagger.Declare` module with
        * `DeclareT` monad transformer;
        * `MonadDeclare` type class;
        * various helpers;
    * Rename parameter-related types:
        * `Parameter` -> `Param`;
        * `ParameterSchema` -> `ParamAnySchema`;
        * `ParameterOtherSchema` -> `ParamOtherSchema`;
        * `ParameterLocation` -> `ParamLocation`;
        * `SchemaCommon` -> `ParamSchema`;
        * `parameter*` fields renamed to `param*` fields;
        * `schemaCommon*` fields renamed to `paramSchema*` fields;
        * `HasSchemaCommon` -> `HasParamSchema`.

* Minor changes:
    * Replace TH-generated JSON instances with `Generic`-based (see [#25](https://github.com/GetShopTV/swagger2/pull/25));
    * Drop `template-haskell` dependency;
    * Omit empty array/object properties from `toJSON` output ([#22](https://github.com/GetShopTV/swagger2/pull/22));
    * Remove `minLength` property from schemas for `time` types;
    * Move `SchemaOptions` to `Data.Swagger.SchemaOptions`;
    * Remove `useReferences` from `SchemaOptions` (see [#23](https://github.com/GetShopTV/swagger2/pull/23));
    * Place all internal submodules under `Data.Swagger.Internal`.
    * Better documentation (see [#26](https://github.com/GetShopTV/swagger2/pull/26));

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
