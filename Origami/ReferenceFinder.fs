namespace CreasePattern

open Geometry
open Utilities.Collections

type ReferenceFinderSolution =
    { Solution: CreasePattern
      CreasePatterns: CreasePattern list
      Steps: AxiomAction list
      Point: Point2D
      Distance: float
      Error: Percentage }

module ReferenceFinder =

    (* Types *)

    type Step =
        private
            { Final: CreasePattern
              CreasePatterns: CreasePattern list
              Steps: AxiomAction list
              Depth: int }

    type LookupTable = private LookupTable of Tree<Step>


    (* Builders *)

    /// Initialize the reference finder lookup table. This table can be queried to get a series of creases that would
    /// give the desired point on a sheet of paper.
    ///
    /// TODO: Create builders for different shapes of paper
    let init =
        let maxDepth = 3

        let boundingBox =
            BoundingBox2D.from (Point2D.xy 0. 0.) (Point2D.xy 1. 1.)

        let continuation step =
            CreasePattern.elements step.Final
            |> List.ofSeq
            |> Axiom.fromElements
            |> List.map
                (fun axiom ->
                    { Depth = step.Depth + 1
                      Steps = step.Steps @ [ axiom ]
                      CreasePatterns = step.CreasePatterns @ [ step.Final ]
                      Final = CreasePattern.performAxiom axiom step.Final })
            |> Seq.ofList

        Tree.fromInitializer
            { BaseCase =
                  { Final = CreasePattern.withBoundingBox boundingBox
                    CreasePatterns = []
                    Steps = []
                    Depth = 0 }
              Continuation = continuation
              Termination = (fun step -> step.Depth = maxDepth) }
        |> LookupTable


    (* Modifiers *)

    let private fold folder state (LookupTable referenceFinder) = Tree.fold folder state referenceFinder


    (* Queries *)

    /// Get the first crease pattern that creates a point close to the desired point
    /// TODO: only get UNIQUE edges from the axiom actions. This will reduce computation for tree branching on initializing and lookup
    let bestFoldSequenceTo target referenceFinder : ReferenceFinderSolution =
        let baseCase =
            { Solution = CreasePattern.empty
              CreasePatterns = []
              Steps = []
              Point = Point2D.origin
              Distance = infinity
              Error = Percentage.ofFloat infinity }

        fold
            (fun bestSolution step ->
                match CreasePattern.closestVertex target step.Final with
                | Some (point, matchDistance) when matchDistance < bestSolution.Distance ->
                    { Solution = step.Final
                      CreasePatterns = step.CreasePatterns
                      Steps = step.Steps
                      Point = point
                      Distance = matchDistance
                      Error =
                          matchDistance
                          / (max step.Final.Bounds.MaxX step.Final.Bounds.MaxY)
                          |> Percentage.ofFloat }
                | _ -> bestSolution)

            baseCase
            referenceFinder
