2.8.2
---

- `text-2.0` support (see [#234](https://github.com/GetShopTV/swagger2/pull/234));

2.8.1
---

- Better error message in `validateSchemaType` (see [#221](https://github.com/GetShopTV/swagger2/pull/221));

2.8
---

- Support GHC 9.2 (see [#231](https://github.com/GetShopTV/swagger2/pull/231), [#232](https://github.com/GetShopTV/swagger2/pull/232), [#233](https://github.com/GetShopTV/swagger2/pull/233));
- Aeson `2.0.3.0` compatibility (see [#230]( https://github.com/GetShopTV/swagger2/pull/230 ));

2.7
---

- Upgrade `aeson` to `2.0.1.0` (see [#228]( https://github.com/GetShopTV/swagger2/pull/228 ));
- Switch from Travis CI to GitHub Actions (see [#228]( https://github.com/GetShopTV/swagger2/pull/228 ));
- GHC 9 support (see [#228]( https://github.com/GetShopTV/swagger2/pull/228 ));
- More GHC-8.10 related cleanup, tighten lower bound on some dependencies.
  (see [#216]( https://github.com/GetShopTV/swagger2/pull/216 ));
- Add "format" field for uint32 and uint64
  (see [#215]( https://github.com/GetShopTV/swagger2/pull/215 ));

2.6
---

- GHC 8.10 support (see [#210]( https://github.com/GetShopTV/swagger2/pull/210 ));
- Move `aeson-pretty` to other dependencies (see [#206]( https://github.com/GetShopTV/swagger2/pull/206 ));
- Merge OAuth2 scopes (see [#151]( https://github.com/GetShopTV/swagger2/pull/151 ));

2.5
---

Highlights:
- Add instance for `TimeOfDay` (see [#196]( https://github.com/GetShopTV/swagger2/pull/196 ));
- Add support for [`optics`](https://hackage.haskell.org/package/optics) (see [#200]( https://github.com/GetShopTV/swagger2/pull/200 ));
- Slightly better validation errors (see [#195]( https://github.com/GetShopTV/swagger2/pull/195 ));

Fixes and minor changes:
- Minor cleanup (see [#194]( https://github.com/GetShopTV/swagger2/pull/194 ));
- Allow base-4.13 and other GHC-8.8 related stuff (see [#198]( https://github.com/GetShopTV/swagger2/pull/198 ));
- Fix `stack.yaml` (see [#201]( https://github.com/GetShopTV/swagger2/pull/201 ));

2.4
---

- Allow hashable-1.3, semigroups-0.19, network-3.1, time-1.9, generics-sop-0.5
- Tags aren't sorted
  (see [#165](https://github.com/GetShopTV/swagger2/issues/165))
- Schema type is optional
  (see [#138](https://github.com/GetShopTV/swagger2/issues/138), [#164](https://github.com/GetShopTV/swagger2/pull/164))
- Take concrete 'Proxy' as argument
  (see [#180](https://github.com/GetShopTV/swagger2/pull/180))
- GHC-8.8 preparations

2.3.1.1
-------

- Allow `network-3.0`

2.3.1
-----

- Add a quickcheck generator for aeson Values that match a swagger schema
  (see [#162](https://github.com/GetShopTV/swagger2/pull/162))
- Add `ToParamSchema` instance for `SetCookie`
  (see [#173](https://github.com/GetShopTV/swagger2/pull/173))
- Make nullary schemas valid
  (see [#168](https://github.com/GetShopTV/swagger2/pull/168))

2.3.0.1
-------

- Support GHC-8.6

2.3
---

Major changes:

- Add support for `additionalProperties` with `Bool` value (see [#159](https://github.com/GetShopTV/swagger2/pull/159));
- Add `ToSchema` `Object` instance (for aeson's `Object`) (see [`d72466a`](https://github.com/GetShopTV/swagger2/commit/d72466a));

Dependencies and CI:

- Aeson 1.4 (see [#158](https://github.com/GetShopTV/swagger2/pull/158));
- Update .travis.yml (see [#160](https://github.com/GetShopTV/swagger2/pull/160));
- Allow network 2.7 (see [#155](https://github.com/GetShopTV/swagger2/pull/155));
- Allow lens-4.17 (see [#161](https://github.com/GetShopTV/swagger2/pull/161));

2.2.2
-----

* Add `ToSchema Version` instance
* Use base-compat-batteries (0.10)
* PVP bounds

2.2.1
-----

* Add `Semigroup` instances
* GHC-8.4 compatibility
* `Schema (NonEmpty a) instance (see [#141](https://github.com/GetShopTV/swagger2/pull/141))
* Fix optional property for unary records (see [#142](https://github.com/GetShopTV/swagger2/issues/142))
* Add `fromAesonOptions` helper (see [#146](https://github.com/GetShopTV/swagger2/issues/139))
* Fix non-termination when using `datatypeNameModifier` with recursive data types (see [#132](https://github.com/GetShopTV/swagger2/issues/132))

2.2
---

* Treat unknown properties as validation errors in `validateToJSON` (see [#126](https://github.com/GetShopTV/swagger2/pull/126));
* Add `validateJSON` and `validateJSONWithPatternChecker` to validate JSON `Value` against `Schema` without classes (see [#126](https://github.com/GetShopTV/swagger2/pull/126));
* Add more `Schema` helpers (see [#126](https://github.com/GetShopTV/swagger2/pull/126)):
    * `genericNameSchema` — to give a custom schema `Generic`-based name;
    * `genericDeclareNamedSchemaNewtype` — to derive `NamedSchema` for `newtype`s;
    * `declareSchemaBoundedEnumKeyMapping` — to derive more specific `Schema` for maps with `Bounded` `Enum` keys;
* Add a few tests with invalid `Schema`.

2.1.6
-----

* Add `ToParamSchema` and `ToSchema` instances for `Fixed` (see [#121](https://github.com/GetShopTV/swagger2/pull/121)) and `Natural` (see [#123](https://github.com/GetShopTV/swagger2/pull/123));
* Fix instance for `NominalDiffTime` (see [#121](https://github.com/GetShopTV/swagger2/pull/121));
* Fix build for `aeson-1.2.2.0` (see [#125](https://github.com/GetShopTV/swagger2/pull/125)).

2.1.5
-----

* Type error by default when deriving Generic-based instances for mixed sum types (see [#118](https://github.com/GetShopTV/swagger2/pull/118));
* Adjust `allOf` to accept referenced schemas (see [#119](https://github.com/GetShopTV/swagger2/issues/119));
* Add instances for `Identity` (see [#116](https://github.com/GetShopTV/swagger2/pull/116));
* Add Gitter chat badge (see [#114](https://github.com/GetShopTV/swagger2/pull/114)).

2.1.4.1
-------

* GHC-8.2 support (see [#95](https://github.com/GetShopTV/swagger2/issues/95), [#106](https://github.com/GetShopTV/swagger2/pull/106) and [#108](https://github.com/GetShopTV/swagger2/pull/108));
* Documentation corrections (see [#105](https://github.com/GetShopTV/swagger2/pull/105));
* Allow `generics-sop-0.3` (see [#102](https://github.com/GetShopTV/swagger2/pull/102));
* Fix `ToSchema` example in docs (see [#104](https://github.com/GetShopTV/swagger2/pull/104)).

2.1.4
-----

* Add `liftDeclare` (see [#100](https://github.com/GetShopTV/swagger2/pull/100));
* Add `MonadDeclare` instances for all monad transformers (see [#100](https://github.com/GetShopTV/swagger2/pull/100)).

2.1.3
-----

* Add [`UUID`](http://hackage.haskell.org/package/uuid-types/docs/Data-UUID-Types.html#t:UUID) instances (see [#81](https://github.com/GetShopTV/swagger2/pull/81)).
* Add [`TypeError`](https://hackage.haskell.org/package/base-4.9.0.0/docs/GHC-TypeLits.html#g:4) `ToSchema` and `ToParamSchema ByteString` instances (see [#78](https://github.com/GetShopTV/swagger2/pull/78))
* Improve documentation for generic sum type instance derivation (see [#75](https://github.com/GetShopTV/swagger2/pull/75))
* Compile warning free (see [#82](https://github.com/GetShopTV/swagger2/pull/82))

2.1.2.1
-------

* Bug fix previous release

2.1.2
---

* Minor changes:
  * Support `aeson-1.0.0.0` (see [#70](https://github.com/GetShopTV/swagger2/pull/70)).

2.1.1
---

* Minor changes:
  * Proper `Schema` examples for `Char`, `Day`, `LocalTime`, `ZonedTime` and `UTCTime`.

2.1
---

* Major changes:
  * Use `InsOrdHashMap` to preserve insertion order for endpoints and definitions (see [#56](https://github.com/GetShopTV/swagger2/pull/56));
  * Add support for GHC 8.0 (see [#65](https://github.com/GetShopTV/swagger2/pull/65)).

2.0.2
---

* Fixes:
  * Fix `additionalProperties` to allow references;
  * Fix `ToSchema` instances for `Map` and `HashMap` (prevent infinite recursion for recursive values).

2.0.1
---

* Fixes:
    * Re-export `Pattern` synonym from `Data.Swagger`;
    * Documentation fixes.

2.0
---

* Major changes:
    * GHC 7.8 support (see [#49](https://github.com/GetShopTV/swagger2/pull/49));
    * Switch to classy field lenses (see [#41](https://github.com/GetShopTV/swagger2/pull/41));
    * Add `Data.Swagger.Schema.Validation` (see [#18](https://github.com/GetShopTV/swagger2/pull/18));
    * Add `Data.Swagger.Operation` with helpers (see [#50](https://github.com/GetShopTV/swagger2/pull/50));
    * Add `IsString` instances for some types (see [#47](https://github.com/GetShopTV/swagger2/pull/47));
    * Add helpers to sketch `Schema` from JSON (see [#48](https://github.com/GetShopTV/swagger2/pull/48)).

* Minor changes:
    * Make `NamedSchema` a `data` rather than `type` (see [#42](https://github.com/GetShopTV/swagger2/pull/42));
    * Change `Definitions` to `Definitions Schema`;
    * Add schema templates for `"binary"`, `"byte"` and `"password"` formats (see [63ed597](https://github.com/GetShopTV/swagger2/commit/63ed59736dc4f942f0e2a7d668d7cee513fa9eaf));
    * Add `Monoid` instance for `Contact`;
    * Change `tags` to be `Set` rather than list.

* Fixes:
    * Fix schema for `()` and nullary constructors (see [ab65c4a](https://github.com/GetShopTV/swagger2/commit/ab65c4a48253c34f8a88221a53dc97bf5e6e8d29));
    * Fix `Operation` `FromJSON` instance to allow missing `tags` and `parameters` properties.

1.2.1
---

* Minor changes:
    * Change `_SwaggerItemsPrimitive` type from a `Prism'` to a more restrictive `Review`-like `Optic'`.

* Fixes:
    * Fix build for GHC 8.0-rc1.

1.2
---
* Minor changes (see [#36](https://github.com/GetShopTV/swagger2/pull/36)):
  * Change default `ToSchema` instance for unit data types (i.e. types with one nullable constructor like `data Unit = Unit`):
    now these types are treated like sum types with only one alternative;
  * Add generic `ToParamSchema` instance for unit data types;
  * Add `items: []` to schema for `()` (making it a valid schema).

* Fixes:
    * Do not omit `items: []` from `Schema` JSON;
    * Do not generate unused definitions for nested `newtype`s (see [#38](https://github.com/GetShopTV/swagger2/pull/38)).

1.1.1
---
* Fixes:
    * `CollectionFormat Param` -> `CollectionFormat ParamOtherSchema`;
      this change was necessary after putting `CollectionFormat` to `SwaggerItems`.

1.1
---
* Major changes:
    * Put `CollectionFormat` in one place (see [`3cc860d`](https://github.com/GetShopTV/swagger2/commit/3cc860dd3f002ab984f4d0e4ce1d1799f985832e)).

* Minor changes:
    * Use Swagger formats for `Int32`, `Int64`, `Float`, `Double`, `Day` and `ZonedTime` (see [#32](https://github.com/GetShopTV/swagger2/pull/32));
    * Export `HeaderName`, `TagName`, `HttpStatusCode` type synonyms;
    * Add `ToParamSchema` instances for `[a]`, `Set a` and `HashSet a`;
    * Add `Monoid` instances for `Header` and `Example`.

* Fixes:
    * Use overwrite strategy for `HashMap` `SwaggerMonoid` instances by default.

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
        * Add helpers for inlining `Schema` references dynamically (see [#23](https://github.com/GetShopTV/swagger2/pull/23));
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
    * Place all internal submodules under `Data.Swagger.Internal`;
    * Better documentation (see [#26](https://github.com/GetShopTV/swagger2/pull/26)).

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
    * Fix all warnings.

0.3
---
* Fixes:
    * Fix `SwaggerMonoid Text` instance;
    * Wrap `Bool` in `Maybe` everywhere;
    * These changes make all `Data.Swagger` `Monoid` instances obey monoid laws
      (previously right identity law was broken by some instances).

0.2
---
* Add `Data.Swagger.Lens`;
* Support references;
* Fixes:
    * Fix `FromJSON SwaggerHost` instance;
    * Add missing `Maybe`s for field types;
    * Decode petstore `swagger.json` successfully.
