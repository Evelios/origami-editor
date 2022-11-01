namespace Origami

open System
open Math.Geometry
open Math.Units

open Utilities.Collections

type EdgeAssignment =
    | Boundary
    | Mountain
    | Valley
    | Flat
    | Preview
    | Unassigned


[<CustomEquality>]
[<CustomComparison>]
type Edge<'Coordinates> =
    { Crease: LineSegment2D<Meters, 'Coordinates>
      Assignment: EdgeAssignment }

    (* Accessors *)

    member this.Line =
        Line2D.through this.Crease.Start this.Crease.Finish


    (* Interfaces *)

    interface IComparable<Edge<'Coordinates>> with
        member this.CompareTo(edge) = this.Comparison(edge)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Edge<'Coordinates> as edge -> this.Comparison(edge)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other: Edge<'Coordinates>) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other) =
        if this.Crease = other.Crease then
            this.Assignment < other.Assignment
        else
            this.Crease < other.Crease

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Edge<'Coordinates> as other ->
            this.Crease = other.Crease
            && this.Assignment = other.Assignment
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.Crease, this.Assignment)

type PointId = int

type Graph<'Coordinates> =
    private
        { Vertices: Map<PointId, Point2D<Meters, 'Coordinates>>
          Edges: Map<UnorderedTuple2<PointId>, EdgeAssignment> }

type GraphElement<'Coordinates> =
    | EdgeElement of Edge<'Coordinates>
    | VertexElement of Point2D<Meters, 'Coordinates>

[<RequireQualifiedAccess>]
type Axiom =
    | One
    | Two
    | Three

[<RequireQualifiedAccess>]
type AxiomAction<'Coordinates> =
    | One of Point2D<Meters, 'Coordinates> * Point2D<Meters, 'Coordinates>
    | Two of Point2D<Meters, 'Coordinates> * Point2D<Meters, 'Coordinates>
    | Three of Line2D<Meters, 'Coordinates> * Line2D<Meters, 'Coordinates>

type Label = int

type CreasePattern<'Coordinates> =
    { Bounds: BoundingBox2D<Meters, 'Coordinates>
      Graph: Graph<'Coordinates>
      Author: string
      Title: string
      Description: string }

type ReferenceFinderSolution<'Coordinates> =
    { Solution: CreasePattern<'Coordinates>
      CreasePatterns: CreasePattern<'Coordinates> seq
      Steps: AxiomAction<'Coordinates> seq
      Point: Point2D<Meters, 'Coordinates>
      Distance: Length
      Error: Percent }

type ReferenceFinderStep<'Coordinates> =
    private
        { Final: CreasePattern<'Coordinates>
          CreasePatterns: CreasePattern<'Coordinates> seq
          Steps: AxiomAction<'Coordinates> seq
          Depth: int }


type ReferenceFinder<'Coordinates> = private ReferenceFinder of Tree<ReferenceFinderStep<'Coordinates>>
