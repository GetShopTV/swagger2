{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ <710
{-# LANGUAGE PolyKinds #-}
#endif
#include "overlapping-compat.h"
module Data.Swagger.Internal where

import Prelude ()
import Prelude.Compat

import           Control.Lens             ((&), (.~), (?~))
import           Control.Applicative
import           Data.Aeson
import qualified Data.Aeson.Types         as JSON
import           Data.Data                (Data(..), Typeable, mkConstr, mkDataType, Fixity(..), Constr, DataType, constrIndex)
import qualified Data.HashMap.Strict      as HashMap
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Monoid              (Monoid (..))
import           Data.Semigroup           (Semigroup (..))
import           Data.Scientific          (Scientific)
import           Data.Set                 (Set)
import           Data.String              (IsString(..))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)
import           Network                  (HostName, PortNumber)
import           Network.HTTP.Media       (MediaType)
import           Text.Read                (readMaybe)

import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap

import Generics.SOP.TH                  (deriveGeneric)
import Data.Swagger.Internal.AesonUtils (sopSwaggerGenericToJSON
                                        ,sopSwaggerGenericToJSONWithOpts
                                        ,sopSwaggerGenericParseJSON
                                        ,HasSwaggerAesonOptions(..)
                                        ,AesonDefaultValue(..)
                                        ,mkSwaggerAesonOptions
                                        ,saoAdditionalPairs
                                        ,saoSubObject)
import Data.Swagger.Internal.Utils

#if MIN_VERSION_aeson(0,10,0)
import Data.Swagger.Internal.AesonUtils (sopSwaggerGenericToEncoding)
#define DEFINE_TOENCODING toEncoding = sopSwaggerGenericToEncoding
#else
#define DEFINE_TOENCODING
#endif

-- | A list of definitions that can be used in references.
type Definitions = InsOrdHashMap Text

-- | This is the root document object for the API specification.
data Swagger = Swagger
  { -- | Provides metadata about the API.
    -- The metadata can be used by the clients if needed.
    _swaggerInfo :: Info

    -- | The host (name or ip) serving the API. It MAY include a port.
    -- If the host is not included, the host serving the documentation is to be used (including the port).
  , _swaggerHost :: Maybe Host

    -- | The base path on which the API is served, which is relative to the host.
    -- If it is not included, the API is served directly under the host.
    -- The value MUST start with a leading slash (/).
  , _swaggerBasePath :: Maybe FilePath

    -- | The transfer protocol of the API.
    -- If the schemes is not included, the default scheme to be used is the one used to access the Swagger definition itself.
  , _swaggerSchemes :: Maybe [Scheme]

    -- | A list of MIME types the APIs can consume.
    -- This is global to all APIs but can be overridden on specific API calls.
  , _swaggerConsumes :: MimeList

    -- | A list of MIME types the APIs can produce.
    -- This is global to all APIs but can be overridden on specific API calls.
  , _swaggerProduces :: MimeList

    -- | The available paths and operations for the API.
    -- Holds the relative paths to the individual endpoints.
    -- The path is appended to the @'basePath'@ in order to construct the full URL.
  , _swaggerPaths :: InsOrdHashMap FilePath PathItem

    -- | An object to hold data types produced and consumed by operations.
  , _swaggerDefinitions :: Definitions Schema

    -- | An object to hold parameters that can be used across operations.
    -- This property does not define global parameters for all operations.
  , _swaggerParameters :: Definitions Param

    -- | An object to hold responses that can be used across operations.
    -- This property does not define global responses for all operations.
  , _swaggerResponses :: Definitions Response

    -- | Security scheme definitions that can be used across the specification.
  , _swaggerSecurityDefinitions :: Definitions SecurityScheme

    -- | A declaration of which security schemes are applied for the API as a whole.
    -- The list of values describes alternative security schemes that can be used
    -- (that is, there is a logical OR between the security requirements).
    -- Individual operations can override this definition.
  , _swaggerSecurity :: [SecurityRequirement]

    -- | A list of tags used by the specification with additional metadata.
    -- The order of the tags can be used to reflect on their order by the parsing tools.
    -- Not all tags that are used by the Operation Object must be declared.
    -- The tags that are not declared may be organized randomly or based on the tools' logic.
    -- Each tag name in the list MUST be unique.
  , _swaggerTags :: Set Tag

    -- | Additional external documentation.
  , _swaggerExternalDocs :: Maybe ExternalDocs
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | The object provides metadata about the API.
-- The metadata can be used by the clients if needed,
-- and can be presented in the Swagger-UI for convenience.
data Info = Info
  { -- | The title of the application.
    _infoTitle :: Text

    -- | A short description of the application.
    -- GFM syntax can be used for rich text representation.
  , _infoDescription :: Maybe Text

    -- | The Terms of Service for the API.
  , _infoTermsOfService :: Maybe Text

    -- | The contact information for the exposed API.
  , _infoContact :: Maybe Contact

    -- | The license information for the exposed API.
  , _infoLicense :: Maybe License

    -- | Provides the version of the application API
    -- (not to be confused with the specification version).
  , _infoVersion :: Text
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Contact information for the exposed API.
data Contact = Contact
  { -- | The identifying name of the contact person/organization.
    _contactName  :: Maybe Text

    -- | The URL pointing to the contact information.
  , _contactUrl   :: Maybe URL

    -- | The email address of the contact person/organization.
  , _contactEmail :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | License information for the exposed API.
data License = License
  { -- | The license name used for the API.
    _licenseName :: Text

    -- | A URL to the license used for the API.
  , _licenseUrl :: Maybe URL
  } deriving (Eq, Show, Generic, Data, Typeable)

instance IsString License where
  fromString s = License (fromString s) Nothing

-- | The host (name or ip) serving the API. It MAY include a port.
data Host = Host
  { _hostName :: HostName         -- ^ Host name.
  , _hostPort :: Maybe PortNumber -- ^ Optional port.
  } deriving (Eq, Show, Generic, Typeable)

instance IsString Host where
  fromString s = Host s Nothing

hostConstr :: Constr
hostConstr = mkConstr hostDataType "Host" [] Prefix

hostDataType :: DataType
hostDataType = mkDataType "Data.Swagger.Host" [hostConstr]

instance Data Host where
  gunfold k z c = case constrIndex c of
    1 -> k (k (z (\name mport -> Host name (fromInteger <$> mport))))
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type Host."
  toConstr (Host _ _) = hostConstr
  dataTypeOf _ = hostDataType

-- | The transfer protocol of the API.
data Scheme
  = Http
  | Https
  | Ws
  | Wss
  deriving (Eq, Show, Generic, Data, Typeable)

-- | Describes the operations available on a single path.
-- A @'PathItem'@ may be empty, due to ACL constraints.
-- The path itself is still exposed to the documentation viewer
-- but they will not know which operations and parameters are available.
data PathItem = PathItem
  { -- | A definition of a GET operation on this path.
    _pathItemGet :: Maybe Operation

    -- | A definition of a PUT operation on this path.
  , _pathItemPut :: Maybe Operation

    -- | A definition of a POST operation on this path.
  , _pathItemPost :: Maybe Operation

    -- | A definition of a DELETE operation on this path.
  , _pathItemDelete :: Maybe Operation

    -- | A definition of a OPTIONS operation on this path.
  , _pathItemOptions :: Maybe Operation

    -- | A definition of a HEAD operation on this path.
  , _pathItemHead :: Maybe Operation

    -- | A definition of a PATCH operation on this path.
  , _pathItemPatch :: Maybe Operation

    -- | A list of parameters that are applicable for all the operations described under this path.
    -- These parameters can be overridden at the operation level, but cannot be removed there.
    -- The list MUST NOT include duplicated parameters.
    -- A unique parameter is defined by a combination of a name and location.
  , _pathItemParameters :: [Referenced Param]
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Describes a single API operation on a path.
data Operation = Operation
  { -- | A list of tags for API documentation control.
    -- Tags can be used for logical grouping of operations by resources or any other qualifier.
    _operationTags :: Set TagName

    -- | A short summary of what the operation does.
    -- For maximum readability in the swagger-ui, this field SHOULD be less than 120 characters.
  , _operationSummary :: Maybe Text

    -- | A verbose explanation of the operation behavior.
    -- GFM syntax can be used for rich text representation.
  , _operationDescription :: Maybe Text

    -- | Additional external documentation for this operation.
  , _operationExternalDocs :: Maybe ExternalDocs

    -- | Unique string used to identify the operation.
    -- The id MUST be unique among all operations described in the API.
    -- Tools and libraries MAY use the it to uniquely identify an operation,
    -- therefore, it is recommended to follow common programming naming conventions.
  , _operationOperationId :: Maybe Text

    -- | A list of MIME types the operation can consume.
    -- This overrides the @'consumes'@.
    -- @Just []@ MAY be used to clear the global definition.
  , _operationConsumes :: Maybe MimeList

    -- | A list of MIME types the operation can produce.
    -- This overrides the @'produces'@.
    -- @Just []@ MAY be used to clear the global definition.
  , _operationProduces :: Maybe MimeList

    -- | A list of parameters that are applicable for this operation.
    -- If a parameter is already defined at the @'PathItem'@,
    -- the new definition will override it, but can never remove it.
    -- The list MUST NOT include duplicated parameters.
    -- A unique parameter is defined by a combination of a name and location.
  , _operationParameters :: [Referenced Param]

    -- | The list of possible responses as they are returned from executing this operation.
  , _operationResponses :: Responses

    -- | The transfer protocol for the operation.
    -- The value overrides @'schemes'@.
  , _operationSchemes :: Maybe [Scheme]

    -- | Declares this operation to be deprecated.
    -- Usage of the declared operation should be refrained.
    -- Default value is @False@.
  , _operationDeprecated :: Maybe Bool

    -- | A declaration of which security schemes are applied for this operation.
    -- The list of values describes alternative security schemes that can be used
    -- (that is, there is a logical OR between the security requirements).
    -- This definition overrides any declared top-level security.
    -- To remove a top-level security declaration, @Just []@ can be used.
  , _operationSecurity :: [SecurityRequirement]
  } deriving (Eq, Show, Generic, Data, Typeable)

newtype MimeList = MimeList { getMimeList :: [MediaType] }
  deriving (Eq, Show, Semigroup, Monoid, Typeable)

mimeListConstr :: Constr
mimeListConstr = mkConstr mimeListDataType "MimeList" ["getMimeList"] Prefix

mimeListDataType :: DataType
mimeListDataType = mkDataType "Data.Swagger.MimeList" [mimeListConstr]

instance Data MimeList where
  gunfold k z c = case constrIndex c of
    1 -> k (z (\xs -> MimeList (map fromString xs)))
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type MimeList."
  toConstr (MimeList _) = mimeListConstr
  dataTypeOf _ = mimeListDataType

-- | Describes a single operation parameter.
-- A unique parameter is defined by a combination of a name and location.
data Param = Param
  { -- | The name of the parameter.
    -- Parameter names are case sensitive.
    _paramName :: Text

    -- | A brief description of the parameter.
    -- This could contain examples of use.
    -- GFM syntax can be used for rich text representation.
  , _paramDescription :: Maybe Text

    -- | Determines whether this parameter is mandatory.
    -- If the parameter is in "path", this property is required and its value MUST be true.
    -- Otherwise, the property MAY be included and its default value is @False@.
  , _paramRequired :: Maybe Bool

    -- | Parameter schema.
  , _paramSchema :: ParamAnySchema
  } deriving (Eq, Show, Generic, Data, Typeable)

data ParamAnySchema
  = ParamBody (Referenced Schema)
  | ParamOther ParamOtherSchema
  deriving (Eq, Show, Generic, Data, Typeable)

data ParamOtherSchema = ParamOtherSchema
  { -- | The location of the parameter.
    _paramOtherSchemaIn :: ParamLocation

    -- | Sets the ability to pass empty-valued parameters.
    -- This is valid only for either @'ParamQuery'@ or @'ParamFormData'@
    -- and allows you to send a parameter with a name only or an empty value.
    -- Default value is @False@.
  , _paramOtherSchemaAllowEmptyValue :: Maybe Bool

  , _paramOtherSchemaParamSchema :: ParamSchema 'SwaggerKindParamOtherSchema
  } deriving (Eq, Show, Generic, Typeable, Data)

-- | Items for @'SwaggerArray'@ schemas.
--
-- @'SwaggerItemsPrimitive'@ should be used only for query params, headers and path pieces.
-- The @'CollectionFormat' t@ parameter specifies how elements of an array should be displayed.
-- Note that @fmt@ in @'SwaggerItemsPrimitive' fmt schema@ specifies format for elements of type @schema@.
-- This is different from the original Swagger's <http://swagger.io/specification/#itemsObject Items Object>.
--
-- @'SwaggerItemsObject'@ should be used to specify homogenous array @'Schema'@s.
--
-- @'SwaggerItemsArray'@ should be used to specify tuple @'Schema'@s.
data SwaggerItems t where
  SwaggerItemsPrimitive :: Maybe (CollectionFormat k) -> ParamSchema k-> SwaggerItems k
  SwaggerItemsObject    :: Referenced Schema   -> SwaggerItems 'SwaggerKindSchema
  SwaggerItemsArray     :: [Referenced Schema] -> SwaggerItems 'SwaggerKindSchema
  deriving (Typeable)

deriving instance Eq (SwaggerItems t)
deriving instance Show (SwaggerItems t)
--deriving instance Typeable (SwaggerItems t)

swaggerItemsPrimitiveConstr :: Constr
swaggerItemsPrimitiveConstr = mkConstr swaggerItemsDataType "SwaggerItemsPrimitive" [] Prefix

swaggerItemsObjectConstr :: Constr
swaggerItemsObjectConstr = mkConstr swaggerItemsDataType "SwaggerItemsObject" [] Prefix

swaggerItemsArrayConstr :: Constr
swaggerItemsArrayConstr = mkConstr swaggerItemsDataType "SwaggerItemsArray" [] Prefix

swaggerItemsDataType :: DataType
swaggerItemsDataType = mkDataType "Data.Swagger.SwaggerItems" [swaggerItemsPrimitiveConstr]

-- Note: unfortunately we have to write these Data instances by hand,
-- to get better contexts / avoid duplicate name when using standalone deriving

instance Data t => Data (SwaggerItems ('SwaggerKindNormal t)) where
  -- TODO: define gfoldl
  gunfold k z c = case constrIndex c of
    1 -> k (k (z SwaggerItemsPrimitive))
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type (SwaggerItems t)."
  toConstr _ = swaggerItemsPrimitiveConstr
  dataTypeOf _ = swaggerItemsDataType

-- SwaggerItems SwaggerKindParamOtherSchema can be constructed using SwaggerItemsPrimitive only
instance Data (SwaggerItems 'SwaggerKindParamOtherSchema) where
  -- TODO: define gfoldl
  gunfold k z c = case constrIndex c of
    1 -> k (k (z SwaggerItemsPrimitive))
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type (SwaggerItems SwaggerKindParamOtherSchema)."
  toConstr _ = swaggerItemsPrimitiveConstr
  dataTypeOf _ = swaggerItemsDataType

instance Data (SwaggerItems 'SwaggerKindSchema) where
  gfoldl _ _ (SwaggerItemsPrimitive _ _) = error $ " Data.Data.gfoldl: Constructor SwaggerItemsPrimitive used to construct SwaggerItems SwaggerKindSchema"
  gfoldl k z (SwaggerItemsObject ref)    = z SwaggerItemsObject `k` ref
  gfoldl k z (SwaggerItemsArray ref)     = z SwaggerItemsArray `k` ref

  gunfold k z c = case constrIndex c of
    2 -> k (z SwaggerItemsObject)
    3 -> k (z SwaggerItemsArray)
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type (SwaggerItems SwaggerKindSchema)."

  toConstr (SwaggerItemsPrimitive _ _) = error "Not supported"
  toConstr (SwaggerItemsObject _)      = swaggerItemsObjectConstr
  toConstr (SwaggerItemsArray _)       = swaggerItemsArrayConstr

  dataTypeOf _ = swaggerItemsDataType

-- | Type used as a kind to avoid overlapping instances.
data SwaggerKind t
    = SwaggerKindNormal t
    | SwaggerKindParamOtherSchema
    | SwaggerKindSchema
    deriving (Typeable)

deriving instance Typeable 'SwaggerKindNormal
deriving instance Typeable 'SwaggerKindParamOtherSchema
deriving instance Typeable 'SwaggerKindSchema

type family SwaggerKindType (k :: SwaggerKind *) :: *
type instance SwaggerKindType ('SwaggerKindNormal t) = t
type instance SwaggerKindType 'SwaggerKindSchema = Schema
type instance SwaggerKindType 'SwaggerKindParamOtherSchema = ParamOtherSchema

data SwaggerType t where
  SwaggerString   :: SwaggerType t
  SwaggerNumber   :: SwaggerType t
  SwaggerInteger  :: SwaggerType t
  SwaggerBoolean  :: SwaggerType t
  SwaggerArray    :: SwaggerType t
  SwaggerFile     :: SwaggerType 'SwaggerKindParamOtherSchema
  SwaggerNull     :: SwaggerType 'SwaggerKindSchema
  SwaggerObject   :: SwaggerType 'SwaggerKindSchema
  deriving (Typeable)

deriving instance Eq (SwaggerType t)
deriving instance Show (SwaggerType t)

swaggerTypeConstr :: Data (SwaggerType t) => SwaggerType t -> Constr
swaggerTypeConstr t = mkConstr (dataTypeOf t) (show t) [] Prefix

swaggerTypeDataType :: {- Data (SwaggerType t) => -} SwaggerType t -> DataType
swaggerTypeDataType _ = mkDataType "Data.Swagger.SwaggerType" swaggerTypeConstrs

swaggerCommonTypes :: [SwaggerType k]
swaggerCommonTypes = [SwaggerString, SwaggerNumber, SwaggerInteger, SwaggerBoolean, SwaggerArray]

swaggerParamTypes :: [SwaggerType 'SwaggerKindParamOtherSchema]
swaggerParamTypes = swaggerCommonTypes ++ [SwaggerFile]

swaggerSchemaTypes :: [SwaggerType 'SwaggerKindSchema]
swaggerSchemaTypes = swaggerCommonTypes ++ [error "SwaggerFile is invalid SwaggerType Schema", SwaggerNull, SwaggerObject]

swaggerTypeConstrs :: [Constr]
swaggerTypeConstrs = map swaggerTypeConstr (swaggerCommonTypes :: [SwaggerType 'SwaggerKindSchema])
  ++ [swaggerTypeConstr SwaggerFile, swaggerTypeConstr SwaggerNull, swaggerTypeConstr SwaggerObject]

instance Typeable t => Data (SwaggerType ('SwaggerKindNormal t)) where
  gunfold = gunfoldEnum "SwaggerType" swaggerCommonTypes
  toConstr = swaggerTypeConstr
  dataTypeOf = swaggerTypeDataType

instance Data (SwaggerType 'SwaggerKindParamOtherSchema) where
  gunfold = gunfoldEnum "SwaggerType ParamOtherSchema" swaggerParamTypes
  toConstr = swaggerTypeConstr
  dataTypeOf = swaggerTypeDataType

instance Data (SwaggerType 'SwaggerKindSchema) where
  gunfold = gunfoldEnum "SwaggerType Schema" swaggerSchemaTypes
  toConstr = swaggerTypeConstr
  dataTypeOf = swaggerTypeDataType

data ParamLocation
  = -- | Parameters that are appended to the URL.
    -- For example, in @/items?id=###@, the query parameter is @id@.
    ParamQuery
    -- | Custom headers that are expected as part of the request.
  | ParamHeader
    -- | Used together with Path Templating, where the parameter value is actually part of the operation's URL.
    -- This does not include the host or base path of the API.
    -- For example, in @/items/{itemId}@, the path parameter is @itemId@.
  | ParamPath
    -- | Used to describe the payload of an HTTP request when either @application/x-www-form-urlencoded@
    -- or @multipart/form-data@ are used as the content type of the request
    -- (in Swagger's definition, the @consumes@ property of an operation).
    -- This is the only parameter type that can be used to send files, thus supporting the @'ParamFile'@ type.
    -- Since form parameters are sent in the payload, they cannot be declared together with a body parameter for the same operation.
    -- Form parameters have a different format based on the content-type used
    -- (for further details, consult <http://www.w3.org/TR/html401/interact/forms.html#h-17.13.4>).
  | ParamFormData
  deriving (Eq, Show, Generic, Data, Typeable)

type Format = Text

-- | Determines the format of the array.
data CollectionFormat t where
  -- Comma separated values: @foo,bar@.
  CollectionCSV :: CollectionFormat t
  -- Space separated values: @foo bar@.
  CollectionSSV :: CollectionFormat t
  -- Tab separated values: @foo\\tbar@.
  CollectionTSV :: CollectionFormat t
  -- Pipe separated values: @foo|bar@.
  CollectionPipes :: CollectionFormat t
  -- Corresponds to multiple parameter instances
  -- instead of multiple values for a single instance @foo=bar&foo=baz@.
  -- This is valid only for parameters in @'ParamQuery'@ or @'ParamFormData'@.
  CollectionMulti :: CollectionFormat 'SwaggerKindParamOtherSchema
  deriving (Typeable)

deriving instance Eq (CollectionFormat t)
deriving instance Show (CollectionFormat t)

collectionFormatConstr :: CollectionFormat t -> Constr
collectionFormatConstr cf = mkConstr collectionFormatDataType (show cf) [] Prefix

collectionFormatDataType :: DataType
collectionFormatDataType = mkDataType "Data.Swagger.CollectionFormat" $
  map collectionFormatConstr collectionCommonFormats

collectionCommonFormats :: [CollectionFormat t]
collectionCommonFormats = [ CollectionCSV, CollectionSSV, CollectionTSV, CollectionPipes ]

instance Data t => Data (CollectionFormat ('SwaggerKindNormal t)) where
  gunfold = gunfoldEnum "CollectionFormat" collectionCommonFormats
  toConstr = collectionFormatConstr
  dataTypeOf _ = collectionFormatDataType

deriving instance Data (CollectionFormat 'SwaggerKindParamOtherSchema)

type ParamName = Text

data Schema = Schema
  { _schemaTitle :: Maybe Text
  , _schemaDescription :: Maybe Text
  , _schemaRequired :: [ParamName]

  , _schemaAllOf :: Maybe [Referenced Schema]
  , _schemaProperties :: InsOrdHashMap Text (Referenced Schema)
  , _schemaAdditionalProperties :: Maybe (Referenced Schema)

  , _schemaDiscriminator :: Maybe Text
  , _schemaReadOnly :: Maybe Bool
  , _schemaXml :: Maybe Xml
  , _schemaExternalDocs :: Maybe ExternalDocs
  , _schemaExample :: Maybe Value

  , _schemaMaxProperties :: Maybe Integer
  , _schemaMinProperties :: Maybe Integer

  , _schemaParamSchema :: ParamSchema 'SwaggerKindSchema
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | A @'Schema'@ with an optional name.
-- This name can be used in references.
data NamedSchema = NamedSchema
  { _namedSchemaName :: Maybe Text
  , _namedSchemaSchema :: Schema
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Regex pattern for @string@ type.
type Pattern = Text

data ParamSchema (t :: SwaggerKind *) = ParamSchema
  { -- | Declares the value of the parameter that the server will use if none is provided,
    -- for example a @"count"@ to control the number of results per page might default to @100@
    -- if not supplied by the client in the request.
    -- (Note: "default" has no meaning for required parameters.)
    -- Unlike JSON Schema this value MUST conform to the defined type for this parameter.
    _paramSchemaDefault :: Maybe Value

  , _paramSchemaType :: SwaggerType t
  , _paramSchemaFormat :: Maybe Format
  , _paramSchemaItems :: Maybe (SwaggerItems t)
  , _paramSchemaMaximum :: Maybe Scientific
  , _paramSchemaExclusiveMaximum :: Maybe Bool
  , _paramSchemaMinimum :: Maybe Scientific
  , _paramSchemaExclusiveMinimum :: Maybe Bool
  , _paramSchemaMaxLength :: Maybe Integer
  , _paramSchemaMinLength :: Maybe Integer
  , _paramSchemaPattern :: Maybe Pattern
  , _paramSchemaMaxItems :: Maybe Integer
  , _paramSchemaMinItems :: Maybe Integer
  , _paramSchemaUniqueItems :: Maybe Bool
  , _paramSchemaEnum :: Maybe [Value]
  , _paramSchemaMultipleOf :: Maybe Scientific
  } deriving (Eq, Show, Generic, Typeable)

deriving instance (Typeable k, Data (SwaggerType k), Data (SwaggerItems k)) => Data (ParamSchema k)

data Xml = Xml
  { -- | Replaces the name of the element/attribute used for the described schema property.
    -- When defined within the @'SwaggerItems'@ (items), it will affect the name of the individual XML elements within the list.
    -- When defined alongside type being array (outside the items),
    -- it will affect the wrapping element and only if wrapped is true.
    -- If wrapped is false, it will be ignored.
    _xmlName :: Maybe Text

    -- | The URL of the namespace definition.
    -- Value SHOULD be in the form of a URL.
  , _xmlNamespace :: Maybe Text

    -- | The prefix to be used for the name.
  , _xmlPrefix :: Maybe Text

    -- | Declares whether the property definition translates to an attribute instead of an element.
    -- Default value is @False@.
  , _xmlAttribute :: Maybe Bool

    -- | MAY be used only for an array definition.
    -- Signifies whether the array is wrapped
    -- (for example, @\<books\>\<book/\>\<book/\>\</books\>@)
    -- or unwrapped (@\<book/\>\<book/\>@).
    -- Default value is @False@.
    -- The definition takes effect only when defined alongside type being array (outside the items).
  , _xmlWrapped :: Maybe Bool
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | A container for the expected responses of an operation.
-- The container maps a HTTP response code to the expected response.
-- It is not expected from the documentation to necessarily cover all possible HTTP response codes,
-- since they may not be known in advance.
-- However, it is expected from the documentation to cover a successful operation response and any known errors.
data Responses = Responses
  { -- | The documentation of responses other than the ones declared for specific HTTP response codes.
    -- It can be used to cover undeclared responses.
   _responsesDefault :: Maybe (Referenced Response)

    -- | Any HTTP status code can be used as the property name (one property per HTTP status code).
    -- Describes the expected response for those HTTP status codes.
  , _responsesResponses :: InsOrdHashMap HttpStatusCode (Referenced Response)
  } deriving (Eq, Show, Generic, Data, Typeable)

type HttpStatusCode = Int

-- | Describes a single response from an API Operation.
data Response = Response
  { -- | A short description of the response.
    -- GFM syntax can be used for rich text representation.
    _responseDescription :: Text

    -- | A definition of the response structure.
    -- It can be a primitive, an array or an object.
    -- If this field does not exist, it means no content is returned as part of the response.
    -- As an extension to the Schema Object, its root type value may also be "file".
    -- This SHOULD be accompanied by a relevant produces mime-type.
  , _responseSchema :: Maybe (Referenced Schema)

    -- | A list of headers that are sent with the response.
  , _responseHeaders :: InsOrdHashMap HeaderName Header

    -- | An example of the response message.
  , _responseExamples :: Maybe Example
  } deriving (Eq, Show, Generic, Data, Typeable)

instance IsString Response where
  fromString s = Response (fromString s) Nothing mempty Nothing

type HeaderName = Text

data Header = Header
  { -- | A short description of the header.
    _headerDescription :: Maybe Text

  , _headerParamSchema :: ParamSchema ('SwaggerKindNormal Header)
  } deriving (Eq, Show, Generic, Data, Typeable)

data Example = Example { getExample :: Map MediaType Value }
  deriving (Eq, Show, Generic, Typeable)

exampleConstr :: Constr
exampleConstr = mkConstr exampleDataType "Example" ["getExample"] Prefix

exampleDataType :: DataType
exampleDataType = mkDataType "Data.Swagger.Example" [exampleConstr]

instance Data Example where
  gunfold k z c = case constrIndex c of
    1 -> k (z (\m -> Example (Map.mapKeys fromString m)))
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type Example."
  toConstr (Example _) = exampleConstr
  dataTypeOf _ = exampleDataType

-- | The location of the API key.
data ApiKeyLocation
  = ApiKeyQuery
  | ApiKeyHeader
  deriving (Eq, Show, Generic, Data, Typeable)

data ApiKeyParams = ApiKeyParams
  { -- | The name of the header or query parameter to be used.
    _apiKeyName :: Text

    -- | The location of the API key.
  , _apiKeyIn :: ApiKeyLocation
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | The authorization URL to be used for OAuth2 flow. This SHOULD be in the form of a URL.
type AuthorizationURL = Text

-- | The token URL to be used for OAuth2 flow. This SHOULD be in the form of a URL.
type TokenURL = Text

data OAuth2Flow
  = OAuth2Implicit AuthorizationURL
  | OAuth2Password TokenURL
  | OAuth2Application TokenURL
  | OAuth2AccessCode AuthorizationURL TokenURL
  deriving (Eq, Show, Generic, Data, Typeable)

data OAuth2Params = OAuth2Params
  { -- | The flow used by the OAuth2 security scheme.
    _oauth2Flow :: OAuth2Flow

    -- | The available scopes for the OAuth2 security scheme.
  , _oauth2Scopes :: InsOrdHashMap Text Text
  } deriving (Eq, Show, Generic, Data, Typeable)

data SecuritySchemeType
  = SecuritySchemeBasic
  | SecuritySchemeApiKey ApiKeyParams
  | SecuritySchemeOAuth2 OAuth2Params
  deriving (Eq, Show, Generic, Data, Typeable)

data SecurityScheme = SecurityScheme
  { -- | The type of the security scheme.
    _securitySchemeType :: SecuritySchemeType

    -- | A short description for security scheme.
  , _securitySchemeDescription :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Lists the required security schemes to execute this operation.
-- The object can have multiple security schemes declared in it which are all required
-- (that is, there is a logical AND between the schemes).
newtype SecurityRequirement = SecurityRequirement
  { getSecurityRequirement :: InsOrdHashMap Text [Text]
  } deriving (Eq, Read, Show, Semigroup, Monoid, ToJSON, FromJSON, Data, Typeable)

-- | Tag name.
type TagName = Text

-- | Allows adding meta data to a single tag that is used by @Operation@.
-- It is not mandatory to have a @Tag@ per tag used there.
data Tag = Tag
  { -- | The name of the tag.
    _tagName :: TagName

    -- | A short description for the tag.
    -- GFM syntax can be used for rich text representation.
  , _tagDescription :: Maybe Text

    -- | Additional external documentation for this tag.
  , _tagExternalDocs :: Maybe ExternalDocs
  } deriving (Eq, Ord, Show, Generic, Data, Typeable)

instance IsString Tag where
  fromString s = Tag (fromString s) Nothing Nothing

-- | Allows referencing an external resource for extended documentation.
data ExternalDocs = ExternalDocs
  { -- | A short description of the target documentation.
    -- GFM syntax can be used for rich text representation.
    _externalDocsDescription :: Maybe Text

    -- | The URL for the target documentation.
  , _externalDocsUrl :: URL
  } deriving (Eq, Ord, Show, Generic, Data, Typeable)

-- | A simple object to allow referencing other definitions in the specification.
-- It can be used to reference parameters and responses that are defined at the top level for reuse.
newtype Reference = Reference { getReference :: Text }
  deriving (Eq, Show, Data, Typeable)

data Referenced a
  = Ref Reference
  | Inline a
  deriving (Eq, Show, Functor, Data, Typeable)

instance IsString a => IsString (Referenced a) where
  fromString = Inline . fromString

newtype URL = URL { getUrl :: Text } deriving (Eq, Ord, Show, ToJSON, FromJSON, Data, Typeable)

-- =======================================================================
-- Monoid instances
-- =======================================================================

instance Semigroup Swagger where
  (<>) = genericMappend
instance Monoid Swagger where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup Info where
  (<>) = genericMappend
instance Monoid Info where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup Contact where
  (<>) = genericMappend
instance Monoid Contact where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup PathItem where
  (<>) = genericMappend
instance Monoid PathItem where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup Schema where
  (<>) = genericMappend
instance Monoid Schema where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup (ParamSchema t) where
  (<>) = genericMappend
instance Monoid (ParamSchema t) where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup Param where
  (<>) = genericMappend
instance Monoid Param where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup ParamOtherSchema where
  (<>) = genericMappend
instance Monoid ParamOtherSchema where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup Header where
  (<>) = genericMappend
instance Monoid Header where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup Responses where
  (<>) = genericMappend
instance Monoid Responses where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup Response where
  (<>) = genericMappend
instance Monoid Response where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup ExternalDocs where
  (<>) = genericMappend
instance Monoid ExternalDocs where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup Operation where
  (<>) = genericMappend
instance Monoid Operation where
  mempty = genericMempty
  mappend = (<>)

instance Semigroup Example where
  (<>) = genericMappend
instance Monoid Example where
  mempty = genericMempty
  mappend = (<>)

-- =======================================================================
-- SwaggerMonoid helper instances
-- =======================================================================

instance SwaggerMonoid Info
instance SwaggerMonoid PathItem
instance SwaggerMonoid Schema
instance SwaggerMonoid (ParamSchema t)
instance SwaggerMonoid Param
instance SwaggerMonoid ParamOtherSchema
instance SwaggerMonoid Responses
instance SwaggerMonoid Response
instance SwaggerMonoid ExternalDocs
instance SwaggerMonoid Operation

instance SwaggerMonoid MimeList
deriving instance SwaggerMonoid URL

instance SwaggerMonoid (SwaggerType t) where
  swaggerMempty = SwaggerString
  swaggerMappend _ y = y

instance SwaggerMonoid ParamLocation where
  swaggerMempty = ParamQuery
  swaggerMappend _ y = y

instance OVERLAPPING_ SwaggerMonoid (InsOrdHashMap FilePath PathItem) where
  swaggerMempty = InsOrdHashMap.empty
  swaggerMappend = InsOrdHashMap.unionWith mappend

instance Monoid a => SwaggerMonoid (Referenced a) where
  swaggerMempty = Inline mempty
  swaggerMappend (Inline x) (Inline y) = Inline (mappend x y)
  swaggerMappend _ y = y

instance SwaggerMonoid ParamAnySchema where
  swaggerMempty = ParamOther swaggerMempty
  swaggerMappend (ParamBody x) (ParamBody y) = ParamBody (swaggerMappend x y)
  swaggerMappend (ParamOther x) (ParamOther y) = ParamOther (swaggerMappend x y)
  swaggerMappend _ y = y

-- =======================================================================
-- Simple Generic-based ToJSON instances
-- =======================================================================

instance ToJSON ParamLocation where
  toJSON = genericToJSON (jsonPrefix "Param")

instance ToJSON Info where
  toJSON = genericToJSON (jsonPrefix "Info")

instance ToJSON Contact where
  toJSON = genericToJSON (jsonPrefix "Contact")

instance ToJSON License where
  toJSON = genericToJSON (jsonPrefix "License")

instance ToJSON ApiKeyLocation where
  toJSON = genericToJSON (jsonPrefix "ApiKey")

instance ToJSON ApiKeyParams where
  toJSON = genericToJSON (jsonPrefix "apiKey")

instance ToJSON Scheme where
  toJSON = genericToJSON (jsonPrefix "")

instance ToJSON Tag where
  toJSON = genericToJSON (jsonPrefix "Tag")

instance ToJSON ExternalDocs where
  toJSON = genericToJSON (jsonPrefix "ExternalDocs")

instance ToJSON Xml where
  toJSON = genericToJSON (jsonPrefix "Xml")

-- =======================================================================
-- Simple Generic-based FromJSON instances
-- =======================================================================

instance FromJSON ParamLocation where
  parseJSON = genericParseJSON (jsonPrefix "Param")

instance FromJSON Info where
  parseJSON = genericParseJSON (jsonPrefix "Info")

instance FromJSON Contact where
  parseJSON = genericParseJSON (jsonPrefix "Contact")

instance FromJSON License where
  parseJSON = genericParseJSON (jsonPrefix "License")

instance FromJSON ApiKeyLocation where
  parseJSON = genericParseJSON (jsonPrefix "ApiKey")

instance FromJSON ApiKeyParams where
  parseJSON = genericParseJSON (jsonPrefix "apiKey")

instance FromJSON Scheme where
  parseJSON = genericParseJSON (jsonPrefix "")

instance FromJSON Tag where
  parseJSON = genericParseJSON (jsonPrefix "Tag")

instance FromJSON ExternalDocs where
  parseJSON = genericParseJSON (jsonPrefix "ExternalDocs")

-- =======================================================================
-- Manual ToJSON instances
-- =======================================================================

instance ToJSON OAuth2Flow where
  toJSON (OAuth2Implicit authUrl) = object
    [ "flow"             .= ("implicit" :: Text)
    , "authorizationUrl" .= authUrl ]
  toJSON (OAuth2Password tokenUrl) = object
    [ "flow"     .= ("password" :: Text)
    , "tokenUrl" .= tokenUrl ]
  toJSON (OAuth2Application tokenUrl) = object
    [ "flow"     .= ("application" :: Text)
    , "tokenUrl" .= tokenUrl ]
  toJSON (OAuth2AccessCode authUrl tokenUrl) = object
    [ "flow"             .= ("accessCode" :: Text)
    , "authorizationUrl" .= authUrl
    , "tokenUrl"         .= tokenUrl ]

instance ToJSON OAuth2Params where
  toJSON = sopSwaggerGenericToJSON
  DEFINE_TOENCODING

instance ToJSON SecuritySchemeType where
  toJSON SecuritySchemeBasic
      = object [ "type" .= ("basic" :: Text) ]
  toJSON (SecuritySchemeApiKey params)
      = toJSON params
    <+> object [ "type" .= ("apiKey" :: Text) ]
  toJSON (SecuritySchemeOAuth2 params)
      = toJSON params
    <+> object [ "type" .= ("oauth2" :: Text) ]

instance ToJSON Swagger where
  toJSON = sopSwaggerGenericToJSON
  DEFINE_TOENCODING

instance ToJSON SecurityScheme where
  toJSON = sopSwaggerGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Schema where
  toJSON = sopSwaggerGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Header where
  toJSON = sopSwaggerGenericToJSON
  DEFINE_TOENCODING

instance ToJSON (ParamSchema t) => ToJSON (SwaggerItems t) where
  toJSON (SwaggerItemsPrimitive fmt schema) = object
    [ "collectionFormat" .= fmt
    , "items"            .= schema ]
  toJSON (SwaggerItemsObject x) = object [ "items" .= x ]
  toJSON (SwaggerItemsArray  x) = object [ "items" .= x ]

instance ToJSON Host where
  toJSON (Host host mport) = toJSON $
    case mport of
      Nothing -> host
      Just port -> host ++ ":" ++ show port

instance ToJSON MimeList where
  toJSON (MimeList xs) = toJSON (map show xs)

instance ToJSON Param where
  toJSON = sopSwaggerGenericToJSON
  DEFINE_TOENCODING

instance ToJSON ParamAnySchema where
  toJSON (ParamBody s) = object [ "in" .= ("body" :: Text), "schema" .= s ]
  toJSON (ParamOther s) = toJSON s

instance ToJSON ParamOtherSchema where
  toJSON = sopSwaggerGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Responses where
  toJSON = sopSwaggerGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Response where
  toJSON = sopSwaggerGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Operation where
  toJSON = sopSwaggerGenericToJSON
  DEFINE_TOENCODING

instance ToJSON PathItem where
  toJSON = sopSwaggerGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Example where
  toJSON = toJSON . Map.mapKeys show . getExample

instance ToJSON Reference where
  toJSON (Reference ref) = object [ "$ref" .= ref ]

referencedToJSON :: ToJSON a => Text -> Referenced a -> Value
referencedToJSON prefix (Ref (Reference ref)) = object [ "$ref" .= (prefix <> ref) ]
referencedToJSON _ (Inline x) = toJSON x

instance ToJSON (Referenced Schema)   where toJSON = referencedToJSON "#/definitions/"
instance ToJSON (Referenced Param)    where toJSON = referencedToJSON "#/parameters/"
instance ToJSON (Referenced Response) where toJSON = referencedToJSON "#/responses/"

instance ToJSON (SwaggerType t) where
  toJSON SwaggerArray   = "array"
  toJSON SwaggerString  = "string"
  toJSON SwaggerInteger = "integer"
  toJSON SwaggerNumber  = "number"
  toJSON SwaggerBoolean = "boolean"
  toJSON SwaggerFile    = "file"
  toJSON SwaggerNull    = "null"
  toJSON SwaggerObject  = "object"

instance ToJSON (CollectionFormat t) where
  toJSON CollectionCSV   = "csv"
  toJSON CollectionSSV   = "ssv"
  toJSON CollectionTSV   = "tsv"
  toJSON CollectionPipes = "pipes"
  toJSON CollectionMulti = "multi"

instance ToJSON (ParamSchema k) where
  -- TODO: this is a bit fishy, why we need sub object only in `ToJSON`?
  toJSON = sopSwaggerGenericToJSONWithOpts $
      mkSwaggerAesonOptions "paramSchema" & saoSubObject ?~ "items"

-- =======================================================================
-- Manual FromJSON instances
-- =======================================================================

instance FromJSON OAuth2Flow where
  parseJSON (Object o) = do
    (flow :: Text) <- o .: "flow"
    case flow of
      "implicit"    -> OAuth2Implicit    <$> o .: "authorizationUrl"
      "password"    -> OAuth2Password    <$> o .: "tokenUrl"
      "application" -> OAuth2Application <$> o .: "tokenUrl"
      "accessCode"  -> OAuth2AccessCode
        <$> o .: "authorizationUrl"
        <*> o .: "tokenUrl"
      _ -> empty
  parseJSON _ = empty

instance FromJSON OAuth2Params where
  parseJSON = sopSwaggerGenericParseJSON

instance FromJSON SecuritySchemeType where
  parseJSON js@(Object o) = do
    (t :: Text) <- o .: "type"
    case t of
      "basic"  -> pure SecuritySchemeBasic
      "apiKey" -> SecuritySchemeApiKey <$> parseJSON js
      "oauth2" -> SecuritySchemeOAuth2 <$> parseJSON js
      _ -> empty
  parseJSON _ = empty

instance FromJSON Swagger where
  parseJSON = sopSwaggerGenericParseJSON

instance FromJSON SecurityScheme where
  parseJSON = sopSwaggerGenericParseJSON

instance FromJSON Schema where
  parseJSON = sopSwaggerGenericParseJSON

instance FromJSON Header where
  parseJSON = sopSwaggerGenericParseJSON

instance (FromJSON (CollectionFormat ('SwaggerKindNormal t)), FromJSON (ParamSchema ('SwaggerKindNormal t))) => FromJSON (SwaggerItems ('SwaggerKindNormal t)) where
  parseJSON = withObject "SwaggerItemsPrimitive" $ \o -> SwaggerItemsPrimitive
    <$> o .:? "collectionFormat"
    <*> (o .: "items" >>= parseJSON)

instance FromJSON (SwaggerItems 'SwaggerKindParamOtherSchema) where
  parseJSON = withObject "SwaggerItemsPrimitive" $ \o -> SwaggerItemsPrimitive
    <$> o .:? "collectionFormat"
    <*> ((o .: "items" >>= parseJSON) <|> fail ("foo" ++ show o))

instance FromJSON (SwaggerItems 'SwaggerKindSchema) where
  parseJSON js@(Object _) = SwaggerItemsObject <$> parseJSON js
  parseJSON js@(Array _)  = SwaggerItemsArray  <$> parseJSON js
  parseJSON _ = empty

instance FromJSON Host where
  parseJSON (String s) = case map Text.unpack $ Text.split (== ':') s of
    [host] -> return $ Host host Nothing
    [host, port] -> case readMaybe port of
      Nothing -> fail $ "Invalid port `" ++ port ++ "'"
      Just p -> return $ Host host (Just (fromInteger p))
    _ -> fail $ "Invalid host `" ++ Text.unpack s ++ "'"
  parseJSON _ = empty

instance FromJSON MimeList where
  parseJSON js = (MimeList . map fromString) <$> parseJSON js

instance FromJSON Param where
  parseJSON = sopSwaggerGenericParseJSON

instance FromJSON ParamAnySchema where
  parseJSON js@(Object o) = do
    (i :: Text) <- o .: "in"
    case i of
      "body" -> do
        schema <- o .: "schema"
        ParamBody <$> parseJSON schema
      _ -> ParamOther <$> parseJSON js
  parseJSON _ = empty

instance FromJSON ParamOtherSchema where
  parseJSON = sopSwaggerGenericParseJSON

instance FromJSON Responses where
  parseJSON (Object o) = Responses
    <$> o .:? "default"
    <*> (parseJSON (Object (HashMap.delete "default" o)))
  parseJSON _ = empty

instance FromJSON Example where
  parseJSON js = do
    m <- parseJSON js
    pure $ Example (Map.mapKeys fromString m)

instance FromJSON Response where
  parseJSON = sopSwaggerGenericParseJSON

instance FromJSON Operation where
  parseJSON = sopSwaggerGenericParseJSON

instance FromJSON PathItem where
  parseJSON = sopSwaggerGenericParseJSON

instance FromJSON Reference where
  parseJSON (Object o) = Reference <$> o .: "$ref"
  parseJSON _ = empty

referencedParseJSON :: FromJSON a => Text -> Value -> JSON.Parser (Referenced a)
referencedParseJSON prefix js@(Object o) = do
  ms <- o .:? "$ref"
  case ms of
    Nothing -> Inline <$> parseJSON js
    Just s  -> Ref <$> parseRef s
  where
    parseRef s = do
      case Text.stripPrefix prefix s of
        Nothing     -> fail $ "expected $ref of the form \"" <> Text.unpack prefix <> "*\", but got " <> show s
        Just suffix -> pure (Reference suffix)
referencedParseJSON _ _ = fail "referenceParseJSON: not an object"

instance FromJSON (Referenced Schema)   where parseJSON = referencedParseJSON "#/definitions/"
instance FromJSON (Referenced Param)    where parseJSON = referencedParseJSON "#/parameters/"
instance FromJSON (Referenced Response) where parseJSON = referencedParseJSON "#/responses/"

instance FromJSON Xml where
  parseJSON = genericParseJSON (jsonPrefix "xml")

instance FromJSON (SwaggerType 'SwaggerKindSchema) where
  parseJSON = parseOneOf [SwaggerString, SwaggerInteger, SwaggerNumber, SwaggerBoolean, SwaggerArray, SwaggerNull, SwaggerObject]

instance FromJSON (SwaggerType 'SwaggerKindParamOtherSchema) where
  parseJSON = parseOneOf [SwaggerString, SwaggerInteger, SwaggerNumber, SwaggerBoolean, SwaggerArray, SwaggerFile]

instance FromJSON (SwaggerType ('SwaggerKindNormal t)) where
  parseJSON = parseOneOf [SwaggerString, SwaggerInteger, SwaggerNumber, SwaggerBoolean, SwaggerArray]

instance FromJSON (CollectionFormat ('SwaggerKindNormal t)) where
  parseJSON = parseOneOf [CollectionCSV, CollectionSSV, CollectionTSV, CollectionPipes]

-- NOTE: There aren't collections of 'Schema'
--instance FromJSON (CollectionFormat (SwaggerKindSchema)) where
--  parseJSON = parseOneOf [CollectionCSV, CollectionSSV, CollectionTSV, CollectionPipes]

instance FromJSON (CollectionFormat 'SwaggerKindParamOtherSchema) where
  parseJSON = parseOneOf [CollectionCSV, CollectionSSV, CollectionTSV, CollectionPipes, CollectionMulti]

instance (FromJSON (SwaggerType ('SwaggerKindNormal t)), FromJSON (SwaggerItems ('SwaggerKindNormal t))) => FromJSON (ParamSchema ('SwaggerKindNormal t)) where
  parseJSON = sopSwaggerGenericParseJSON
instance FromJSON (ParamSchema 'SwaggerKindParamOtherSchema) where
  parseJSON = sopSwaggerGenericParseJSON
instance FromJSON (ParamSchema 'SwaggerKindSchema) where
  parseJSON = sopSwaggerGenericParseJSON

-------------------------------------------------------------------------------
-- TH splices
-------------------------------------------------------------------------------

deriveGeneric ''Header
deriveGeneric ''OAuth2Params
deriveGeneric ''Operation
deriveGeneric ''Param
deriveGeneric ''ParamOtherSchema
deriveGeneric ''PathItem
deriveGeneric ''Response
deriveGeneric ''Responses
deriveGeneric ''SecurityScheme
deriveGeneric ''Schema
deriveGeneric ''ParamSchema
deriveGeneric ''Swagger

instance HasSwaggerAesonOptions Header where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "header" & saoSubObject ?~ "paramSchema"
instance HasSwaggerAesonOptions OAuth2Params where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "oauth2" & saoSubObject ?~ "flow"
instance HasSwaggerAesonOptions Operation where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "operation"
instance HasSwaggerAesonOptions Param where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "param" & saoSubObject ?~ "schema"
instance HasSwaggerAesonOptions ParamOtherSchema where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "paramOtherSchema" & saoSubObject ?~ "paramSchema"
instance HasSwaggerAesonOptions PathItem where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "pathItem"
instance HasSwaggerAesonOptions Response where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "response"
instance HasSwaggerAesonOptions Responses where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "responses" & saoSubObject ?~ "responses"
instance HasSwaggerAesonOptions SecurityScheme where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "securityScheme" & saoSubObject ?~ "type"
instance HasSwaggerAesonOptions Schema where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "schema" & saoSubObject ?~ "paramSchema"
instance HasSwaggerAesonOptions Swagger where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "swagger" & saoAdditionalPairs .~ [("swagger", "2.0")]

instance HasSwaggerAesonOptions (ParamSchema ('SwaggerKindNormal t)) where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "paramSchema" & saoSubObject ?~ "items"
instance HasSwaggerAesonOptions (ParamSchema 'SwaggerKindParamOtherSchema) where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "paramSchema" & saoSubObject ?~ "items"
-- NOTE: Schema doesn't have 'items' sub object!
instance HasSwaggerAesonOptions (ParamSchema 'SwaggerKindSchema) where
  swaggerAesonOptions _ = mkSwaggerAesonOptions "paramSchema"

instance AesonDefaultValue (ParamSchema s)
instance AesonDefaultValue OAuth2Flow
instance AesonDefaultValue Responses
instance AesonDefaultValue ParamAnySchema
instance AesonDefaultValue SecuritySchemeType
instance AesonDefaultValue (SwaggerType a)
instance AesonDefaultValue MimeList where defaultValue = Just mempty
instance AesonDefaultValue Info
instance AesonDefaultValue ParamLocation
