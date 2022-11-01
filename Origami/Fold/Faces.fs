[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Fold.Faces

open Thoth.Json.Net

let create a : Faces = a

let empty: Faces =
    { Vertices = []
      Edges = []
      Orders = [] }

let decoder<'Coordinates> (get: Decode.IGetters) : Faces =
    let field (name: string) (subDecoder: Decoder<'a>) (defaultValue: 'a) : 'a =
        get.Optional.Field name subDecoder
        |> Option.defaultValue defaultValue

    let decodeListListInt: Decoder<int list list> =
        Decode.int |> Decode.list |> Decode.list

    let decode3TupleList: Decoder<(int * int * int) list> =
        Decode.tuple3 Decode.int Decode.int Decode.int
        |> Decode.list

    { Vertices = field "faces_vertices" decodeListListInt []
      Edges = field "faces_edges" decodeListListInt []
      Orders = field "faceOrders" decode3TupleList [] }

let encode (faces: Faces) : (string * JsonValue) list =
    let encodeList f list = List.map f list |> Encode.list

    let encodeListList f list =
        List.map (List.map f >> Encode.list) list
        |> Encode.list

    let encodeTuple3 =
        Encode.tuple3 Encode.int Encode.int Encode.int

    [ if faces.Vertices <> [] then
          "faces_vertices", encodeListList Encode.int faces.Vertices

      if faces.Edges <> [] then
          "faces_edges", encodeListList Encode.int faces.Edges

      if faces.Orders <> [] then
          "faceOrders", encodeList encodeTuple3 faces.Orders ]
