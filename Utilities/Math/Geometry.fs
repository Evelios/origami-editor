namespace Math.Geometry

open Math.Units

open Thoth.Json.Net

module Line2D =

    open Math.Geometry
    open Math.Units

    /// Get the two perpendicular directions
    let perpendicularDirection (line: Line2D<Meters, 'Coordinates>) : Direction2D<'Coordinates> option =
        Line2D.direction line
        |> Option.map (Direction2D.rotateBy (Angle.degrees 90.))

    let perpendicularThroughPoint
        (point: Point2D<Meters, 'Coordinates>)
        (line: Line2D<Meters, 'Coordinates>)
        : Line2D<Meters, 'Coordinates> =

        perpendicularDirection line
        |> Option.get
        |> (fun direction -> Line2D.atPointInDirection point (Vector2D.meters direction.X direction.Y))


module Point2D =
    let decoder<'Coordinates> : Decoder<Point2D<Meters, 'Coordinates>> =
        Decode.object (fun get ->
            match get.Required.Raw(Decode.list Decode.float) with
            | [ x; y ] -> Point2D.meters x y
            | [ _; _; _ ] -> failwith "3D Points are not currently supported"
            | _ -> failwith "Received an incorrect Point2D value")

    let encoder (point: Point2D<Meters, 'Coordinates>) : JsonValue =
        Encode.list [
            Length.inMeters point.X
            Length.inMeters point.Y
        ]
