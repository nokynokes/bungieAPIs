port module Main exposing (main)

import Json.Decode as Decode exposing (Value)
import Base64.Encode as Base64
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (..)
import Html.Events exposing (onClick)
import Delay exposing (after)
import Url exposing (Url)
import Browser
import Debug
import Http
import Json.Decode as Json
import OAuthInfo exposing (oauthConfig, Configuration)
import OAuth
import OAuth.AuthorizationCode.PKCE as OAuth
import Url.Builder as UrlBuilder exposing (crossOrigin, string)


type alias Model =
  { redirectUri : Url
  , flow : Flow
  }

type Msg
  = ChangedUrl Url
  | ClickedLink Browser.UrlRequest
  | SignInRequested
  | GotRandomBytes (List Int)
  | AccessTokenRequested
  | GotAccessToken (Result Http.Error OAuth.AuthenticationSuccess)

type Flow
    = Idle
    | Authorized OAuth.AuthorizationCode OAuth.CodeVerifier
    | Authenticated OAuth.Token
    | Errored Error

type Error
    = ErrStateMismatch
    | ErrFailedToConvertBytes
    | ErrAuthorization OAuth.AuthorizationError
    | ErrAuthentication OAuth.AuthenticationError
    | ErrHTTPGetAccessToken
    | ErrHTTPGetUserInfo

init : Maybe { state : String, codeVerifier : OAuth.CodeVerifier } -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init maybeFlags url navKey =
   let
      redirectUri =
          { url | query = Nothing, fragment = Nothing }
      clearUrl =
          Navigation.replaceUrl navKey (Url.toString redirectUri)
   in
      case OAuth.parseCode url of
        OAuth.Error error -> ({ flow = Errored <| ErrAuthorization error, redirectUri = redirectUri }, clearUrl)
        OAuth.Empty -> ({ flow = Idle, redirectUri = redirectUri }, Cmd.none)
        OAuth.Success { code, state } ->
          case maybeFlags of
            Nothing -> ({ flow = Errored ErrStateMismatch, redirectUri = redirectUri }, clearUrl)
            Just flags ->
              if state /= Just flags.state then
                ({ flow = Errored ErrStateMismatch, redirectUri = redirectUri }, clearUrl)
              else
                ({ flow = Authorized code flags.codeVerifier, redirectUri = redirectUri }
                , Cmd.batch
                    [ after 750 AccessTokenRequested
                    , clearUrl
                    ]
                )



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (model.flow, msg) of
    (Idle, SignInRequested) -> (model, genRandomBytes <| cSTATE_SIZE + cCODE_VERIFIER_SIZE )
    (Idle, GotRandomBytes bytes) -> gotRandomBytes model bytes
    (Authorized code codeVerifier, AccessTokenRequested) -> (model, getAccessToken oauthConfig model.redirectUri code codeVerifier)
    (Authorized _ _, GotAccessToken authResponse) -> gotAccessToken model authResponse
    _ -> (model, Cmd.none)


view : Model -> Browser.Document Msg
view model =
  let
    html =
      case model.flow of
        Idle ->
          [ button
            [ onClick SignInRequested ]
            [ text "Sign in" ]
          ]
        Authorized _ _ ->
           [ span [] [ text "Authenticating..." ] ]
        Authenticated token ->
           [ span [] [ text <| "Success! token is: " ++ OAuth.tokenToString token ] ]
        Errored error ->
           [ span [] [ viewError error ] ]
  in
       { title = "Bungie NET Auth"
       , body = html
       }

viewError : Error -> Html Msg
viewError e =
    text <|
        case e of
            ErrStateMismatch ->
                "'state' doesn't match, the request has likely been forged by an adversary!"

            ErrFailedToConvertBytes ->
                "Unable to convert bytes to 'state' and 'codeVerifier', this is likely not your fault..."

            ErrAuthorization error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            ErrAuthentication error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            ErrHTTPGetAccessToken ->
                "Unable to retrieve token: HTTP request failed. CORS is likely disabled on the authorization server."

            ErrHTTPGetUserInfo ->
                "Unable to retrieve user info: HTTP request failed."





oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc

subscriptions : Model -> Sub Msg
subscriptions model =
  randomBytes GotRandomBytes


port genRandomBytes : Int -> Cmd msg

port randomBytes : (List Int -> msg) -> Sub msg

cSTATE_SIZE : Int
cSTATE_SIZE =
    8

-- Number of bytes making the 'code_verifier'
cCODE_VERIFIER_SIZE : Int
cCODE_VERIFIER_SIZE =
    32

toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


base64 : Bytes -> String
base64 =
    Base64.bytes >> Base64.encode

getAccessToken : Configuration -> Url -> OAuth.AuthorizationCode -> OAuth.CodeVerifier -> Cmd Msg
getAccessToken config redirectUri code codeVerifier =
  let
    authRequest =
      OAuth.makeTokenRequest GotAccessToken
        { credentials =
           { clientId = config.clientId
            , secret = Nothing
           }
        , code = code
        , codeVerifier = codeVerifier
        , url = config.tokenEndpoint
        , redirectUri = redirectUri
        }
  in
    Http.request <|
      { authRequest | headers = [ Http.header "Access-Control-Allow-Origin" "*" ] }
  -- Http.request <|
  --   OAuth.makeTokenRequest GotAccessToken
  --     { credentials =
  --       { clientId = config.clientId
  --       , secret = Nothing
  --       }
  --     , code = code
  --     , codeVerifier = codeVerifier
  --     , url = config.tokenEndpoint
  --     , redirectUri = redirectUri
  --     }
  -- Http.request <|
  --   { method = "POST",
  --   , url = Url.toString config.tokenEndpoint
  --   , headers = [ string "Content-Type" "application/x-www-form-urlencoded" ]
  --   , body =
  --   }
gotAccessToken : Model -> Result Http.Error OAuth.AuthenticationSuccess -> ( Model, Cmd Msg )
gotAccessToken model authResponse =
  case authResponse of
    Err (Http.BadBody body) ->
       case Json.decodeString OAuth.defaultAuthenticationErrorDecoder body of
         Ok err ->
           ({ model | flow = Errored <| ErrAuthentication err }, Cmd.none)
         _ -> ({ model | flow = Errored ErrHTTPGetAccessToken }, Cmd.none)
    Err _ -> ({ model | flow = Errored ErrHTTPGetAccessToken }, Cmd.none)
    Ok { token } -> ({ model | flow = Authenticated token }, Cmd.none)

gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    case convertBytes bytes of
        Nothing ->
            (model, Cmd.none)
        Just { state, codeVerifier } ->
            let
                -- https://www.bungie.net/en/oauth/authorize?client_id=12345&response_type=code&state=6i0mkLx79Hp91nzWVeHrzHG4
                authorization =
                  crossOrigin
                    "https://www.bungie.net"
                    [ "en","OAuth","Authorize" ]
                    [ string "client_id" oauthConfig.clientId, string "response_type" "code", string "state" state]
            in
            ( model, Navigation.load authorization)

convertBytes : List Int -> Maybe { state : String, codeVerifier : OAuth.CodeVerifier }
convertBytes bytes =
    if List.length bytes < (cSTATE_SIZE + cCODE_VERIFIER_SIZE) then
        Nothing
    else
        let
            state =
                bytes
                    |> List.take cSTATE_SIZE
                    |> toBytes
                    |> base64

            mCodeVerifier =
                bytes
                    |> List.drop cSTATE_SIZE
                    |> toBytes
                    |> OAuth.codeVerifierFromBytes
        in
        Maybe.map (\codeVerifier -> { state = state, codeVerifier = codeVerifier }) mCodeVerifier


main : Program (Maybe (List Int)) Model Msg
main = Browser.application
  { init = Maybe.andThen convertBytes >> init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlChange = ChangedUrl
  , onUrlRequest = ClickedLink
  }
