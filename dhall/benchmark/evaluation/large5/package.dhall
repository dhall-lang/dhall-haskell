let Base/render = ./src/base/render.dhall

let Configuration/global = ./src/configuration/global.dhall

in  { Configuration = { Global = Configuration/global }, Render = Base/render }
