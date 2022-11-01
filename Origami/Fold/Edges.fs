[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Fold.Edges

open Thoth.Json.Net

let create a : Edges = a

let empty: Edges =
    { Vertices = []
      Faces = []
      Assignment = []
      FoldAngle = []
      Length = []
      Orders = [] }


let decoder (get: Decode.IGetters) : Edges =
    let field (name: string) (subDecoder: Decoder<'a>) (defaultValue: 'a) : 'a =
        get.Optional.Field name subDecoder
        |> Option.defaultValue defaultValue

    let decode2TupleList: Decoder<(int * int) list> =
        Decode.tuple2 Decode.int Decode.int |> Decode.list

    let decode2TupleOptionList: Decoder<(int * int option) list> =
        Decode.tuple2 Decode.int (Decode.option Decode.int)
        |> Decode.list

    let decode3TupleList: Decoder<(int * int * int) list> =
        Decode.tuple3 Decode.int Decode.int Decode.int
        |> Decode.list

    let edgeAssignmentDecoder =
        Decode.string
        |> Decode.map EdgeAssignment.fromString
        |> Decode.list

    { Vertices = field "edges_vertices" decode2TupleList []
      Faces = field "edges_faces" decode2TupleOptionList []
      Assignment = field "edges_assignment" edgeAssignmentDecoder []
      FoldAngle = field "edges_foldAngle" (Decode.float |> Decode.list) []
      Length = field "edges_length" (Decode.float |> Decode.list) []
      Orders = field "edgeOrders" decode3TupleList [] }

let encode (edges: Edges) : (string * JsonValue) list =
    let encodeList f list = List.map f list |> Encode.list

    let encodeTuple2 =
        Encode.tuple2 Encode.int Encode.int

    let encodeTuple2Option =
        Encode.tuple2 Encode.int (Encode.option Encode.int)

    let encodeTuple3 =
        Encode.tuple3 Encode.int Encode.int Encode.int


    [ if edges.Vertices <> [] then
          "edges_vertices", encodeList encodeTuple2 edges.Vertices

      if edges.Faces <> [] then
          "edges_faces", encodeList encodeTuple2Option edges.Faces

      if edges.Assignment <> [] then
          "edges_assignment", encodeList (EdgeAssignment.toString >> Encode.string) edges.Assignment

      if edges.FoldAngle <> [] then
          "edges_foldAngle", encodeList Encode.float edges.FoldAngle

      if edges.Length <> [] then
          "edges_length", encodeList Encode.float edges.Length

      if edges.Orders <> [] then
          "edgeOrders", encodeList encodeTuple3 edges.Orders ]
