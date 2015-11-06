module Data.Swagger.Internal where

import           Data.HashMap.Strict      (HashMap)
import           Data.Text                (Text)
import           Network                  (HostName, PortNumber)
import           Network.HTTP.Media       (MediaType)
import           Network.URL              (URL)

-- | This is the root document object for the API specification.
data Swagger = Swagger
  { -- | Provides metadata about the API.
    -- The metadata can be used by the clients if needed.
    swaggerInfo :: SwaggerInfo

    -- | The host (name or ip) serving the API. It MAY include a port.
    -- If the host is not included, the host serving the documentation is to be used (including the port).
  , swaggerHost :: Maybe SwaggerHost

    -- | The base path on which the API is served, which is relative to the host.
    -- If it is not included, the API is served directly under the host.
    -- The value MUST start with a leading slash (/).
  , swaggerBasePath :: Maybe FilePath

    -- | The transfer protocol of the API.
    -- If the schemes is not included, the default scheme to be used is the one used to access the Swagger definition itself.
  , swaggerSchemes :: Maybe [SwaggerScheme]

    -- | A list of MIME types the APIs can consume.
    -- This is global to all APIs but can be overridden on specific API calls.
  , swaggerConsumes :: [MediaType]

    -- | A list of MIME types the APIs can produce.
    -- This is global to all APIs but can be overridden on specific API calls. 
  , swaggerProduces :: [MediaType]

    -- | The available paths and operations for the API.
  , swaggerPaths :: SwaggerPaths

    -- | An object to hold data types produced and consumed by operations.
  , swaggerDefinitions :: [SwaggerDefinition]

    -- | An object to hold parameters that can be used across operations.
    -- This property does not define global parameters for all operations.
  , swaggerParameters :: [SwaggerParamDefinition]

    -- | An object to hold responses that can be used across operations.
    -- This property does not define global responses for all operations.
  , swaggerResponses :: [SwaggerResponseDefinition]

    -- | Security scheme definitions that can be used across the specification.
  , swaggerSecurityDefinitions :: [SwaggerSecurityDefinition]

    -- | A declaration of which security schemes are applied for the API as a whole.
    -- The list of values describes alternative security schemes that can be used
    -- (that is, there is a logical OR between the security requirements).
    -- Individual operations can override this definition.
  , swaggerSecurity :: [SwaggerSecurityRequirement]

    -- | A list of tags used by the specification with additional metadata.
    -- The order of the tags can be used to reflect on their order by the parsing tools.
    -- Not all tags that are used by the Operation Object must be declared.
    -- The tags that are not declared may be organized randomly or based on the tools' logic.
    -- Each tag name in the list MUST be unique.
  , swaggerTags :: [SwaggerTag]

    -- | Additional external documentation.
  , swaggerExternalDocs :: SwaggerExternalDocs
  } deriving (Show)

-- | The object provides metadata about the API.
-- The metadata can be used by the clients if needed,
-- and can be presented in the Swagger-UI for convenience.
data SwaggerInfo = SwaggerInfo
  { -- | The title of the application.
    swaggerInfoTitle :: Text

    -- | A short description of the application.
    -- GFM syntax can be used for rich text representation.
  , swaggerInfoDescription :: Maybe Text

    -- | The Terms of Service for the API.
  , swaggerInfoTermsOfService :: Maybe Text

    -- | The contact information for the exposed API.
  , swaggerInfoContact :: Maybe SwaggerContact

    -- | The license information for the exposed API.
  , swaggerInfoLicense :: Maybe SwaggerLicense

    -- | Provides the version of the application API
    -- (not to be confused with the specification version).
  , swaggerInfoVersion :: Text
  } deriving (Show)

-- | Contact information for the exposed API.
data SwaggerContact = SwaggerContact
  { -- | The identifying name of the contact person/organization.
    swaggerContactName  :: Maybe Text

    -- | The URL pointing to the contact information.
  , swaggerContactURL   :: Maybe URL

    -- | The email address of the contact person/organization.
  , swaggerContactEmail :: Maybe Text
  } deriving (Show)

-- | License information for the exposed API.
data SwaggerLicense = SwaggerLicense
  { -- | The license name used for the API.
    swaggerLicenseName :: Text

    -- | A URL to the license used for the API.
  , swaggerLicenseURL :: Maybe URL
  } deriving (Show)

-- | The host (name or ip) serving the API. It MAY include a port.
data SwaggerHost = SwaggerHost
  { swaggerHostName :: HostName         -- ^ Host name.
  , swaggerHostPort :: Maybe PortNumber -- ^ Optional port.
  } deriving (Show)

-- | The transfer protocol of the API.
data SwaggerScheme
  = Http
  | Https
  | Ws
  | Wss
  deriving (Show)

-- | The available paths and operations for the API.
data SwaggerPaths = SwaggerPaths
  { -- | Holds the relative paths to the individual endpoints.
    -- The path is appended to the @'swaggerBasePath'@ in order to construct the full URL.
    swaggerPathsMap         :: HashMap FilePath SwaggerPathItem

    -- | Allows extensions to the Swagger Schema.
  , swaggerPathsExtensions  :: [SwaggerExtension]
  } deriving (Show)

-- | Describes the operations available on a single path.
-- A @'SwaggerPathItem'@ may be empty, due to ACL constraints.
-- The path itself is still exposed to the documentation viewer
-- but they will not know which operations and parameters are available.
data SwaggerPathItem = SwaggerPathItem
  { -- | A definition of a GET operation on this path.
    swaggerPathItemGet :: Maybe SwaggerOperation

    -- | A definition of a PUT operation on this path.
  , swaggerPathItemPut :: Maybe SwaggerOperation

    -- | A definition of a POST operation on this path.
  , swaggerPathItemPost :: Maybe SwaggerOperation

    -- | A definition of a DELETE operation on this path.
  , swaggerPathItemDelete :: Maybe SwaggerOperation

    -- | A definition of a OPTIONS operation on this path.
  , swaggerPathItemOptions :: Maybe SwaggerOperation

    -- | A definition of a HEAD operation on this path.
  , swaggerPathItemHead :: Maybe SwaggerOperation

    -- | A definition of a PATCH operation on this path.
  , swaggerPathItemPatch :: Maybe SwaggerOperation

    -- | A list of parameters that are applicable for all the operations described under this path.
    -- These parameters can be overridden at the operation level, but cannot be removed there.
    -- The list MUST NOT include duplicated parameters.
    -- A unique parameter is defined by a combination of a name and location.
  , swaggerPathItemParameters :: [SwaggerParameter]
  } deriving (Show)

-- | Describes a single API operation on a path.
data SwaggerOperation = SwaggerOperation
  { -- | A list of tags for API documentation control.
    -- Tags can be used for logical grouping of operations by resources or any other qualifier.
    swaggerOperationTags :: [Text]

    -- | A short summary of what the operation does.
    -- For maximum readability in the swagger-ui, this field SHOULD be less than 120 characters.
  , swaggerOperationSummary :: Maybe Text

    -- | A verbose explanation of the operation behavior.
    -- GFM syntax can be used for rich text representation.
  , swaggerOperationDescription :: Maybe Text

    -- | Additional external documentation for this operation.
  , swaggerOperationExternalDocs :: Maybe SwaggerExternalDocs

    -- | Unique string used to identify the operation.
    -- The id MUST be unique among all operations described in the API.
    -- Tools and libraries MAY use the it to uniquely identify an operation,
    -- therefore, it is recommended to follow common programming naming conventions.
  , swaggerOperationOperationId :: Maybe Text

    -- | A list of MIME types the operation can consume.
    -- This overrides the @'swaggerConsumes'@.
    -- @Just []@ MAY be used to clear the global definition.
  , swaggerOperationConsumes :: Maybe [MediaType]

    -- | A list of MIME types the operation can produce.
    -- This overrides the @'swaggerProduces'@.
    -- @Just []@ MAY be used to clear the global definition.
  , swaggerOperationProduces :: Maybe [MediaType]

    -- | A list of parameters that are applicable for this operation.
    -- If a parameter is already defined at the @'SwaggerPathItem'@,
    -- the new definition will override it, but can never remove it.
    -- The list MUST NOT include duplicated parameters.
    -- A unique parameter is defined by a combination of a name and location.
  , swaggerOperationParameters :: [SwaggerParameter]

    -- | The list of possible responses as they are returned from executing this operation.
  , swaggerOperationResponses :: SwaggerResponses

    -- | The transfer protocol for the operation.
    -- The value overrides @'swaggerSchemes'@.
  , swaggerOperationSchemes :: Maybe [SwaggerScheme]

    -- | Declares this operation to be deprecated.
    -- Usage of the declared operation should be refrained.
    -- Default value is @False@.
  , swaggerOperationDeprecated :: Bool

    -- | A declaration of which security schemes are applied for this operation.
    -- The list of values describes alternative security schemes that can be used
    -- (that is, there is a logical OR between the security requirements).
    -- This definition overrides any declared top-level security.
    -- To remove a top-level security declaration, @Just []@ can be used.
  , swaggerOperationSecurity :: [SwaggerSecurityRequirement]
  } deriving (Show)

data SwaggerParameter = SwaggerParameter
  deriving (Show)

data SwaggerResponses = SwaggerResponses
  deriving (Show)

data SwaggerExtension = SwaggerExtension
  deriving (Show)

data SwaggerDefinition = SwaggerDefinition
  deriving (Show)

data SwaggerParamDefinition = SwaggerParamDefinition
  deriving (Show)

data SwaggerResponseDefinition = SwaggerResponseDefinition
  deriving (Show)

data SwaggerSecurityDefinition = SwaggerSecurityDefinition
  deriving (Show)

data SwaggerSecurityRequirement = SwaggerSecurityRequirement
  deriving (Show)

data SwaggerTag = SwaggerTag
  deriving (Show)

-- | Allows referencing an external resource for extended documentation.
data SwaggerExternalDocs = SwaggerExternalDocs
  { -- | A short description of the target documentation.
    -- GFM syntax can be used for rich text representation.
    swaggerExternalDocsDescription :: Maybe Text

    -- | The URL for the target documentation.
  , swaggerExternalDocsURL :: URL
  } deriving (Show)

