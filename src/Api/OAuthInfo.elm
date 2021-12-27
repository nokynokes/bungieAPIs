module OAuthInfo exposing (oauthConfig, Configuration, authHttpsUrl, tokenHttpsUrl)

import Url exposing (Protocol(..), Url)
import Url.Builder as UrlBuilder
import OAuth
import OAuth.AuthorizationCode.PKCE as OAuth
import Http

type alias Configuration =
  { authorizationEndpoint : Url
  , tokenEndpoint : Url
  , clientId : String
  , scope : List String
  }

defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }

authHttpsUrl : Url
authHttpsUrl =
  { protocol = Https
  , host = "bungie.net"
  , path = "/en/OAuth/Authorize"
  , port_ = Nothing
  , query = Nothing
  , fragment = Nothing
  }

tokenHttpsUrl : Url
tokenHttpsUrl =
  { protocol = Https
  , host = "bungie.net"
  , path = "/Platform/App/OAuth/token/"
  , port_ = Nothing
  , query = Nothing
  , fragment = Nothing
  }



oauthConfig : Configuration
oauthConfig =
  { authorizationEndpoint =
      { defaultHttpsUrl | host = "bungie.net", path = "/en/OAuth/Authorize" }
  , tokenEndpoint =
      { defaultHttpsUrl | host = "bungie.net", path = "/Platform/App/OAuth/token/" }
  , clientId = "38809"
  , scope = []
  }
