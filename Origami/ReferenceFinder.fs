[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.ReferenceFinder

open Utilities.Collections
open Math.Geometry
open Math.Units


// ---- Builders ---------------------------------------------------------------

/// Initialize the reference finder lookup table. This table can be queried to get a series of creases that would
/// give the desired point on a sheet of paper.
///
/// TODO: Create builders for different shapes of paper
let init<'Coordinates> : ReferenceFinder<'Coordinates> =
    let maxDepth = 3

    let boundingBox =
        BoundingBox2D.from (Point2D.meters 0. 0.) (Point2D.meters 1. 1.)

    let continuation (step: ReferenceFinderStep<'Coordinatesa>) =
        CreasePattern.elements step.Final
        |> Axiom.fromElements
        |> Seq.map (fun axiomAction ->
            (CreasePattern.performAxiom EdgeAssignment.Unassigned axiomAction step.Final, axiomAction))
        |> Seq.distinctBy fst
        |> Seq.map (fun (newCreasePattern, axiomAction) ->
            { Final = newCreasePattern
              Depth = step.Depth + 1
              Steps = Seq.append step.Steps (Seq.singleton axiomAction)
              CreasePatterns = Seq.append step.CreasePatterns (Seq.singleton step.Final) })

    Tree.fromInitializer
        { BaseCase =
            { Final = CreasePattern.withBoundingBox boundingBox
              CreasePatterns = []
              Steps = []
              Depth = 0 }
          Continuation = continuation
          Termination = (fun step -> step.Depth = maxDepth) }
    |> ReferenceFinder


// ---- Accessors --------------------------------------------------------------

let nodes (ReferenceFinder ref) = Tree.nodes ref


// ---- Modifiers --------------------------------------------------------------

let private fold folder state (ReferenceFinder referenceFinder) = Tree.fold folder state referenceFinder


// ---- Queries ----------------------------------------------------------------

/// Get the first crease pattern that creates a point close to the desired point
let bestFoldSequenceTo
    (target: Point2D<Meters, 'Coordinates>)
    (lookupTable: ReferenceFinder<'Coordinates>)
    : ReferenceFinderSolution<'Coordinates> =
    let baseCase: ReferenceFinderSolution<'Coordinates> =
        { Solution = CreasePattern.empty<'Coordinates>
          CreasePatterns = []
          Steps = []
          Point = Point2D.origin
          Distance = Quantity.infinity
          Error = Percent.create infinity }

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
                    / (max (BoundingBox2D.width step.Final.Bounds) (BoundingBox2D.height step.Final.Bounds))
                    |> Percent.create }
            | _ -> bestSolution)

        baseCase
        lookupTable

/// Get the first crease pattern that creates a point close to the desired point
let bestFoldSequencesTo count target referenceFinder : ReferenceFinderSolution<'Coordinates> array =
    let distance step = step.Distance

    fold
        (fun bestSolutions step ->
            let lastIndex =
                min (Array.length bestSolutions) (count - 1)

            let largestDistance =
                if Array.isEmpty bestSolutions then
                    Quantity.infinity
                else
                    bestSolutions[lastIndex - 1].Distance


            match CreasePattern.closestVertex target step.Final with
            | Some (point, matchDistance) when matchDistance < largestDistance ->
                if Array.exists (fun sol -> sol.Solution = step.Final) bestSolutions then
                    bestSolutions

                else
                    let newSolution =
                        { Solution = step.Final
                          CreasePatterns = step.CreasePatterns
                          Steps = step.Steps
                          Point = point
                          Distance = matchDistance
                          Error =
                            matchDistance
                            / (max (BoundingBox2D.width step.Final.Bounds) (BoundingBox2D.height step.Final.Bounds))
                            |> Percent.create }

                    let keepSolutions =
                        if Array.isEmpty bestSolutions then
                            Array.empty
                        else
                            bestSolutions[0 .. (lastIndex - 1)]

                    Array.append keepSolutions [| newSolution |]
                    |> Array.sortBy distance

            | _ -> bestSolutions)

        Array.empty
        referenceFinder
