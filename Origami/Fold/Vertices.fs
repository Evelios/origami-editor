[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Fold.Vertices

open Thoth.Json
open Math.Geometry

let create a : Vertices<'Coordinates> = a

let empty: Vertices<'Coordinates> =
    { Coordinates = []
      Vertices = []
      Faces = [] }

let decoder<'Coordinates> : Decoder<Vertices<'Coordinates>> =
    Decode.object (fun get ->
        let maybeList (name: string) (subDecoder: Decoder<'a>) : 'a list =
            get.Optional.Field name (Decode.list subDecoder)
            |> Option.defaultValue []

        { Coordinates = maybeList "vertices_coords" Point2D.decoder
          Vertices = maybeList "vertices_vertices" Decode.int
          Faces = maybeList "vertices_faces" (Decode.list Decode.int) })

let encode (vertices: Vertices<'Coordinates>) : JsonValue =
    let encodeList f list = List.map f list |> Encode.list

    let encodeListList f list =
        List.map (List.map f >> Encode.list) list
        |> Encode.list

    Encode.object [
        "vertices_coords", encodeList Point2D.encoder vertices.Coordinates
        "vertices_vertices", encodeList Encode.int vertices.Vertices
        "vertices_faces", encodeListList Encode.int vertices.Faces
    ]
