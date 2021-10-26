open System.IO
open System.Reflection
open Geometry
open GeometrySvg

open SharpVG

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
/// * The diagonal line between B and D is an angle bisector to both
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
let spiralModule (baseLength: float) (controlAngle: Angle) : Polygon2D * float * Frame2D =
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

    let childOrigin =
        Point2D.rotateAround pD -thetaD' pA
        |> Point2D.translate (Point2D.toVector -pD)

    let thetaChild = thetaD - (Angle.inDegrees 90.<deg>)
    let childFrame = Frame2D.withAngle thetaChild childOrigin

    polygon, lengthAB, childFrame


let spiral (baseLength: float) (controlAngle: Angle) (segments: int) : Polygon2D list =
    let rec spiralHelper prevModules len parentFrame segCount =
        if segCount <= 0 then
            prevModules
        else
            let localPolygon, childLength, localChildFrame = spiralModule len controlAngle
            let childFrame = Frame2D.placeIn parentFrame localChildFrame
            let globalPolygon = Polygon2D.placeIn parentFrame localPolygon

            spiralHelper (globalPolygon :: prevModules) childLength childFrame (segCount - 1)

    spiralHelper [] baseLength Frame2D.atOrigin segments

[<EntryPoint>]
let main _ =
    let fileName =
        "/users/tommy/Pictures/Edmark-Spiral.svg"

    let style =
        { Stroke = Some(Name Colors.Black)
          StrokeWidth = Some(Length.ofInt 1)
          Fill = Some(Name Colors.LightGray)
          Opacity = None
          FillOpacity = None
          Name = Some("std") }

    let baseLength = 30.
    let controlAngle = Angle.inDegrees 82.<deg>
    let numSegments = 13


    let fullSpiral =
        spiral baseLength controlAngle numSegments

    let boundingBox =
        List.fold
            (fun bbox spiral -> BoundingBox2D.union (Polygon2D.boundingBox spiral) bbox)
            BoundingBox2D.empty
            fullSpiral

    List.map (Draw.polygon >> Element.withStyle style) fullSpiral
    |> Group.ofList
    |> Svg.ofGroup
    |> Svg.withSize (Area.ofFloats (2. * BoundingBox2D.width boundingBox, 2. * BoundingBox2D.height boundingBox))
    |> Svg.withViewBox (ViewBox.create Point.origin Area.full)
    |> Svg.toString
    |> File.saveToFile fileName
    
    printfn $"Created {fileName}"
    0
