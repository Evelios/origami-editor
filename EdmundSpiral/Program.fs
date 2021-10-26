open System.IO
open System.Reflection
open Geometry
open GeometrySvg

open SharpVG
open Utilities

module File =
    let getProgramPath =
        System.IO.Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
        + (string Path.DirectorySeparatorChar)

    let getTemporaryFileNameWithExt ext =
        Path.ChangeExtension(Path.GetTempFileName(), ext)

    let saveToFile name lines = File.WriteAllLines(name, [ lines ])


/// Create the spiral module adapted from John Edmund.
/// * The given value is the base length, the line between D and C.
/// * The control angle is theta D
/// * Point A is at the origin (0, 0).
/// * The polygon is created and given in the point order [A, B, C, D].
/// * thetaB = thetaC
/// * Corner A is a right angle, thetaA = 90deg
/// * thetaD' is the continued angle off of the line AD on the opposite side of angle D
///
///        side
///       B    C
///        ____
///  top  |    \  base
///       |_____\
///       A     D
///        side
///
/// Returns the length of the next base length as well as the spiral polygon.
let spiralModule (baseLength: float) (controlAngle: Angle) : Polygon2D * float =
    let lengthCD = baseLength
    let thetaD = controlAngle

    let thetaB =
        (Angle.inDegrees 135.<deg>) - (thetaD / 2.)

    let halfThetaD = thetaD / 2.
    let halfThetaB = thetaB / 2.

    let lengthAD =
        lengthCD * Angle.sin thetaB * Angle.cos halfThetaD
        / Angle.sin halfThetaB

    let lengthAB =
        lengthCD * Angle.sin thetaB * Angle.sin halfThetaD
        / Angle.sin halfThetaB

    let thetaD' = Angle.inDegrees 180.<deg> - controlAngle

    let pA = Point2D.origin
    let pB = Point2D.xy 0. lengthAB

    let pC =
        Point2D.xy lengthAD 0.
        + Vector2D.rTheta baseLength thetaD'

    let pD = Point2D.xy lengthAD 0.

    let polygon =
        Polygon2D.singleLoop [ pA; pB; pC; pD ]
        |> Polygon2D.rotateAround pD -thetaD'
        |> Polygon2D.translate (Point2D.toVector -pD)

    polygon, lengthAB


let spiral (baseLength: float) (controlAngle: Angle) (segments: int) : Polygon2D list =
    let rec spiralHelper prevModules len segCount =
        if segments <= 0 then
            prevModules
        else
            let polygon, nextLength = spiralModule len controlAngle
            
            
            spiralHelper (polygon :: prevModules) nextLength (segCount - 1)

    spiralHelper [] baseLength segments

[<EntryPoint>]
let main _ =
    let fileName =
        File.getProgramPath + "Edmund-Spiral.svg"

    let baseLength = 63.
    let controlAngle = Angle.inDegrees 78.<deg>


    let fullSpiral =
        spiral baseLength controlAngle
        |> Debug.log "Spiral"

    let boundingBox = Polygon2D.boundingBox fullSpiral 2

    Draw.polygon fullSpiral
    |> Svg.withSize (Area.ofFloats (2. * BoundingBox2D.width boundingBox, 2. * BoundingBox2D.height boundingBox))
    |> Svg.withViewBox (ViewBox.create Point.origin Area.full)
    |> Svg.toString
    |> File.saveToFile fileName

    printfn $"Created {fileName}"
    0
