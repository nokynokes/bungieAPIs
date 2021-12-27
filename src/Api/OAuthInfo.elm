module OAuthInfo exposing (oauthConfig, Configuration)

import Url exposing (Protocol(..), Url)
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

oauthConfig : Configuration
oauthConfig =
  { authorizationEndpoint =
      { defaultHttpsUrl | host = "bungie.net", path = "/en/OAuth/Authorize" }
  , tokenEndpoint =
      { defaultHttpsUrl | host = "bungie.net", path = "/platform/app/oauth/token/" }
  , clientId = "38809"
  , scope = ["ReadDestinyInventoryAndVault", "ReadBasicUserProfile", "MoveEquipDestinyItems", "ReadDestinyVendorsAndAdvisors"]
  }
