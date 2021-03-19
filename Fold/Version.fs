namespace Fold

open FSharp.Json


type Version = { major: int; minor: int; patch: int }

module Version =
    let Create major minor patch: Version =
        { major = major
          minor = minor
          patch = patch }

    let CreateWithMinor major minor: Version = Create major minor 0

    let CreateWithMajor major: Version = Create major 0 0

    let ToString ({ major = maj
                    minor = min
                    patch = patch }: Version)
                 : string =
        match (maj, min, patch) with
        | (_, 0, 0) -> maj.ToString()
        | (_, _, 0) -> maj.ToString() + "." + min.ToString()
        | _ ->
            maj.ToString()
            + "."
            + min.ToString()
            + "."
            + patch.ToString()

    let FromString (text: string): Version option =
        let versionNumbers =
            text.Split [| '.' |]
            |> Array.map StringConverter.ParseInt

        match versionNumbers with
        | [||] -> None
        | [| Some major |] -> Some(CreateWithMajor major)
        | [| Some major; Some minor |] -> Some(CreateWithMinor major minor)
        | [| Some major; Some minor; Some patch |] -> Some(Create major minor patch)
        | _ -> None

    /// Transform a version to and from the string representation
    type Transform() =
        interface ITypeTransform with
            member x.targetType() = (fun _ -> typeof<string>) ()
            member x.toTargetType value = ToString(value :?> Version) :> obj

            member x.fromTargetType value =
                value.ToString()
                |> FromString
                |> Option.defaultValue (CreateWithMajor 1) :> obj
