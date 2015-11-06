module Data.Swagger.Internal where

import Data.Text          (Text)
import Network            (HostName, PortNumber)
import Network.HTTP.Media (MediaType)

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

data SwaggerContact = SwaggerContact
  deriving (Show)

data SwaggerLicense = SwaggerLicense
  deriving (Show)

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
    swaggerPathsMap         :: [SwaggerPath]

    -- | Allows extensions to the Swagger Schema.
  , swaggerPathsExtensions  :: [SwaggerExtension]
  } deriving (Show)

data SwaggerPath = SwaggerPath
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

data SwaggerExternalDocs = SwaggerExternalDocs
  deriving (Show)

