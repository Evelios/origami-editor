namespace Utilities.Extensions

open Utilities.Extensions

open Thoth.Json.Net

module Encode =
    //// Name, object, converter,  empty value
    let sparseObject (objects: (string * 'a * ('a -> JsonValue) * 'a) list) : JsonValue =
        let converter (name: string, obj: 'a, encoder: 'a -> JsonValue, empty: 'a) =
            if obj = empty then
                None
            else
                Some(name, encoder obj)

        List.filterMap converter objects |> Encode.object
