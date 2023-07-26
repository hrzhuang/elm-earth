module Main exposing (main)

import Dict
import Task

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events as Events
import Html
import Html.Attributes as Attrs
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Texture)

type alias Flags = ()

type alias TexturesLoading =
    { front : Maybe Texture
    , back : Maybe Texture
    , right : Maybe Texture
    , left : Maybe Texture
    , top : Maybe Texture
    , bottom : Maybe Texture
    }

type alias Textures =
    { front : Texture
    , back : Texture
    , right : Texture
    , left : Texture
    , top : Texture
    , bottom : Texture
    }

type alias LoadingModel =
    { windowWidth : Maybe Float
    , windowHeight : Maybe Float
    , textures : TexturesLoading
    }

type alias LoadedModel =
    { windowWidth : Float
    , windowHeight : Float
    , textures : Textures
    , rotation : Float -- in radians
    }

type Model
    = LoadingState LoadingModel
    | LoadedState LoadedModel
    | ErrorState

type CubeFace = Front | Back | Right | Left | Top | Bottom

cubeFaces : List CubeFace
cubeFaces = [Front, Back, Right, Left, Top, Bottom]

type Msg
    = GotWindowDimensions Float Float
    | LoadTextureResult CubeFace (Result Texture.Error Texture)
    | AnimationFrame Float -- time delta since last frame in milliseconds

main : Program Flags Model Msg
main = Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

{- ---
 - init
 - ---
 -}

gotViewport : Dom.Viewport -> Msg
gotViewport { viewport } = GotWindowDimensions viewport.width viewport.height

getWindowDimensions : Cmd Msg
getWindowDimensions = Task.perform gotViewport Dom.getViewport

cubeFaceTexturePath : CubeFace -> String
cubeFaceTexturePath face =
    let
        prefix = "textures/earth/"
        suffix = ".jpg"
    in case face of
        Front -> prefix ++ "front" ++ suffix
        Back -> prefix ++ "back" ++ suffix
        Right -> prefix ++ "right" ++ suffix
        Left -> prefix ++ "left" ++ suffix
        Top -> prefix ++ "top" ++ suffix
        Bottom -> prefix ++ "bottom" ++ suffix

loadCubeFaceTexture : CubeFace -> Cmd Msg
loadCubeFaceTexture face =
    let
        -- need to bind to name since record update syntax does not accept
        -- namespaced variables
        defaultOptions = Texture.defaultOptions
    in cubeFaceTexturePath face
        |> Texture.loadWith { defaultOptions | flipY = False }
        |> Task.attempt (LoadTextureResult face)

init : Flags -> (Model, Cmd Msg)
init () =
    let
        model = LoadingState
            { windowWidth = Nothing
            , windowHeight = Nothing
            , textures =
                { front = Nothing
                , back = Nothing
                , right = Nothing
                , left = Nothing
                , top = Nothing
                , bottom = Nothing
                }
            }
        cmd = Cmd.batch <|
            getWindowDimensions :: List.map loadCubeFaceTexture cubeFaces
    in (model, cmd)

{- ---
 - update
 - ---
 -}

setWindowDimensionsLoading : Float -> Float -> LoadingModel -> LoadingModel
setWindowDimensionsLoading width height loadingModel =
    { loadingModel | windowWidth = Just width, windowHeight = Just height }

setWindowDimensionsLoaded : Float -> Float -> LoadedModel -> LoadedModel
setWindowDimensionsLoaded width height loadedModel =
    { loadedModel | windowWidth = width , windowHeight = height }

setCubeFaceTexture : CubeFace -> Texture -> LoadingModel -> LoadingModel
setCubeFaceTexture face texture loadingModel =
    let
        updateTextures textures = case face of
            Front -> { textures | front = Just texture }
            Back -> { textures | back = Just texture }
            Right -> { textures | right = Just texture }
            Left -> { textures | left = Just texture }
            Top -> { textures | top = Just texture }
            Bottom -> { textures | bottom = Just texture }
    in { loadingModel | textures = updateTextures loadingModel.textures }

tryFinishLoadingTextures : TexturesLoading -> Maybe Textures
tryFinishLoadingTextures loading =
    -- need nested tuples because elm does not support tuples with more than
    -- 3 elements
    case ((loading.front, loading.back), (loading.right, loading.left),
            (loading.top, loading.bottom)) of
        ((Just front, Just back), (Just right, Just left),
                (Just top, Just bottom)) ->
            Just
                { front = front, back = back
                , right = right, left = left
                , top = top, bottom = bottom
                }
        _ -> Nothing

tryFinishLoading : LoadingModel -> Model
tryFinishLoading loadingModel =
    case (loadingModel.windowWidth, loadingModel.windowHeight,
            tryFinishLoadingTextures loadingModel.textures) of
        (Just windowWidth, Just windowHeight, Just textures) ->
            LoadedState
                { windowWidth = windowWidth
                , windowHeight = windowHeight
                , textures = textures
                , rotation = 0
                }
        _ -> LoadingState loadingModel

-- in radians per millisecond
rotationSpeed : Float
rotationSpeed =
    let
        secondsPerRotation = 8
    in 2*pi / 1000 / secondsPerRotation

-- "mod" rotation by 2*pi
wrapRotation : Float -> Float
wrapRotation rotation =
    rotation - (toFloat << floor) (rotation / (2*pi)) * (2*pi)

updateRotation : Float -> LoadedModel -> LoadedModel
updateRotation delta loadedModel =
    { loadedModel
    | rotation = loadedModel.rotation + delta * rotationSpeed |> wrapRotation
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case model of
    LoadingState loadingModel -> case msg of
        GotWindowDimensions width height ->
            ( setWindowDimensionsLoading width height loadingModel
                |> tryFinishLoading
            , Cmd.none
            )
        LoadTextureResult face result -> case result of
            Ok texture ->
                ( setCubeFaceTexture face texture loadingModel
                    |> tryFinishLoading
                , Cmd.none
                )
            Err _ -> (ErrorState, Cmd.none)
        AnimationFrame _ -> (ErrorState, Cmd.none)
    LoadedState loadedModel -> case msg of
        GotWindowDimensions width height ->
            ( LoadedState (setWindowDimensionsLoaded width height loadedModel)
            , Cmd.none
            )
        LoadTextureResult _ _ -> (ErrorState, Cmd.none)
        AnimationFrame delta ->
            ( LoadedState (updateRotation delta loadedModel)
            , Cmd.none
            )
    ErrorState -> (ErrorState, Cmd.none)

{- ---
 - subscriptions
 - ---
 -}

windowResized : Int -> Int -> Msg
windowResized newWidth newHeight =
    GotWindowDimensions (toFloat newWidth) (toFloat newHeight)

subscriptions : Model -> Sub Msg
subscriptions model = case model of
    LoadingState _ -> Events.onResize windowResized
    LoadedState _ -> Sub.batch
        [ Events.onAnimationFrameDelta AnimationFrame
        , Events.onResize windowResized
        ]
    ErrorState -> Sub.none

{- ---
 - view
 - ---
 -}

type alias Vertex = { pos : Vec3, textureCoord : Vec2 }

{- vertices
 - ---
 - b, f - back (-x), front (+x)
 - l, r - left (-y), right (+y)
 - b, t - bottom (-z), top (+z)
 - ---
 - names based on 3D graphs in calculus
 - ---
 - front/back - closer/farther from viewer
 - left/right - left/right from viewer's (not the cube's) perspective
 -}
frt = Vec3.normalize <| vec3 1 1 1
frb = Vec3.normalize <| vec3 1 1 -1
flt = Vec3.normalize <| vec3 1 -1 1
flb = Vec3.normalize <| vec3 1 -1 -1
brt = Vec3.normalize <| vec3 -1 1 1
brb = Vec3.normalize <| vec3 -1 1 -1
blt = Vec3.normalize <| vec3 -1 -1 1
blb = Vec3.normalize <| vec3 -1 -1 -1

type alias Quad = { tl : Vertex, tr : Vertex, bl : Vertex, br : Vertex }

quad : Vertex -> Vertex -> Vertex -> Vertex -> Quad
quad tl tr bl br = { tl = tl, tr = tr, bl = bl, br = br }

cubeFaceQuad : CubeFace -> Quad
cubeFaceQuad face =
    let
        makeQuad tl tr bl br =
            { tl = { pos = tl, textureCoord = vec2 0 0 }
            , tr = { pos = tr, textureCoord = vec2 1 0 }
            , bl = { pos = bl, textureCoord = vec2 0 1 }
            , br = { pos = br, textureCoord = vec2 1 1 }
            }
    in case face of
        Front -> makeQuad flt frt flb frb
        Back -> makeQuad brt blt brb blb
        Right -> makeQuad frt brt frb brb
        Left -> makeQuad blt flt blb flb
        Top -> makeQuad blt brt flt frt
        Bottom -> makeQuad flb frb blb brb

cubeFaceNormal : CubeFace -> Vec3
cubeFaceNormal face = case face of
    Front -> Vec3.i
    Back -> Vec3.negate Vec3.i
    Right -> Vec3.j
    Left -> Vec3.negate Vec3.j
    Top -> Vec3.k
    Bottom -> Vec3.negate Vec3.k

cubeEdgeLength : Float
cubeEdgeLength = Vec3.length <| Vec3.sub frt frb

mapToCubeFace : CubeFace -> Vec3 -> Vec3
mapToCubeFace face v =
    let
        normal = cubeFaceNormal face
        factor = cubeEdgeLength / 2 / Vec3.dot normal v
    in Vec3.scale factor v

posBetween : Vertex -> Vertex -> Vec3
posBetween v1 v2 = Vec3.normalize <| Vec3.add v1.pos v2.pos

textureCoordBetween : CubeFace -> Vertex -> Vertex -> Vec2
textureCoordBetween face v1 v2 =
    -- angle bisector theorem
    let
        weight1 = Vec3.length <| mapToCubeFace face v2.pos
        weight2 = Vec3.length <| mapToCubeFace face v1.pos
    in Vec2.scale (1 / (weight1 + weight2)) <| Vec2.add
        (Vec2.scale weight1 v1.textureCoord)
        (Vec2.scale weight2 v2.textureCoord)

vertexBetween : CubeFace -> Vertex -> Vertex -> Vertex
vertexBetween face v1 v2 =
    { pos = posBetween v1 v2
    , textureCoord = textureCoordBetween face v1 v2
    }

splitQuad : CubeFace -> Quad -> List Quad
splitQuad face { tl, tr, bl, br } =
    let
        t = vertexBetween face tl tr
        r = vertexBetween face tr br
        b = vertexBetween face bl br
        l = vertexBetween face tl bl
        c = vertexBetween face t b
    in [quad tl t l c, quad t tr c r, quad l c bl b, quad c r b br]

type alias Triangle = (Vertex, Vertex, Vertex)

quadTriangles : Quad -> List Triangle
quadTriangles { tl, tr, bl, br } = [(tl, tr, bl), (tr, bl, br)]

cubeFaceMesh : CubeFace -> Mesh Vertex
cubeFaceMesh face = cubeFaceQuad face
    |> List.singleton
    |> List.concatMap (splitQuad face)
    |> List.concatMap (splitQuad face)
    |> List.concatMap (splitQuad face)
    |> List.concatMap (splitQuad face)
    |> List.concatMap quadTriangles
    |> WebGL.triangles

type alias Uniforms =
    { rotation : Mat4
    , camera : Mat4
    , perspective : Mat4
    , texture : Texture
    , lightColor : Vec3
    , ambientBrightness : Float
    , lightPos : Vec3
    , cameraPos : Vec3
    , specularBrightness : Float
    }

type alias Varying = { vTextureCoord : Vec2, vPos : Vec3 }

cubeFaceTexture : Textures -> CubeFace -> Texture
cubeFaceTexture textures face = case face of
    Front -> textures.front
    Back -> textures.back
    Right -> textures.right
    Left -> textures.left
    Top -> textures.top
    Bottom -> textures.bottom

cameraPos : Vec3
cameraPos = vec3 3 3 3

white : Vec3
white = vec3 1 1 1

cubeFaceUniforms : LoadedModel -> CubeFace -> Uniforms
cubeFaceUniforms { windowWidth, windowHeight, textures, rotation } face =
    { rotation = Mat4.makeRotate rotation Vec3.k
    , camera = Mat4.makeLookAt cameraPos (vec3 0 0 0) (vec3 -1 -1 1)
    , perspective =
        Mat4.makePerspective 45 (windowWidth / windowHeight) 0.01 100
    , texture = cubeFaceTexture textures face
    , lightColor = white
    , ambientBrightness = 0.1
    , lightPos = vec3 0 10 10
    , cameraPos = cameraPos
    , specularBrightness = 0.5
    }

vertexShader : Shader Vertex Uniforms Varying
vertexShader = [glsl|
        attribute vec3 pos;
        attribute vec2 textureCoord;
        uniform mat4 rotation;
        uniform mat4 camera;
        uniform mat4 perspective;
        varying vec2 vTextureCoord;
        varying vec3 vPos;
        void main() {
            gl_Position = perspective * camera * rotation * vec4(pos, 1.0);
            vTextureCoord = textureCoord;
            vPos = mat3(rotation) * pos;
        }
    |]

fragmentShader : Shader {} Uniforms Varying
fragmentShader = [glsl|
        precision lowp float;
        uniform sampler2D texture;
        uniform vec3 lightColor;
        uniform float ambientBrightness;
        uniform vec3 lightPos;
        uniform vec3 cameraPos;
        uniform float specularBrightness;
        varying vec2 vTextureCoord;
        varying vec3 vPos;
        void main() {
            vec3 ambient = ambientBrightness * lightColor;
            vec3 lightDir = normalize(lightPos - vPos);
            // normal is same as position since unit sphere centered at origin
            vec3 normal = vPos;
            vec3 diffuse = max(dot(normal, lightDir), 0.0) * lightColor;
            vec3 cameraDir = normalize(cameraPos - vPos);
            vec3 reflectDir = reflect(-lightDir, normal);
            vec3 specular = specularBrightness
                * pow(max(dot(cameraDir, reflectDir), 0.0), 8.0)
                * lightColor;
            vec3 lighting = ambient + diffuse + specular;
            vec4 objColor = texture2D(texture, vTextureCoord);
            gl_FragColor = vec4(lighting, 1.0) * objColor;
        }
    |]

cubeFaceEntity : LoadedModel -> CubeFace -> Entity
cubeFaceEntity loadedModel face = WebGL.entity vertexShader fragmentShader
    (cubeFaceMesh face) (cubeFaceUniforms loadedModel face)

view : Model -> Document Msg
view model =
    let
        body = case model of
            LoadedState loadedModel ->
                let
                    width = round loadedModel.windowWidth
                    height = round loadedModel.windowHeight
                in List.map (cubeFaceEntity loadedModel) cubeFaces
                    |> WebGL.toHtml
                        [ Attrs.width (width * 2)
                        , Attrs.height (height * 2)
                        , Attrs.style "width" (String.fromInt width ++ "px")
                        , Attrs.style "height" (String.fromInt height ++ "px")
                        , Attrs.style "display" "block"
                        , Attrs.style "background-color" "black"
                        ]
                    |> List.singleton
            _ -> []
    in { title = "Earth", body = body }
