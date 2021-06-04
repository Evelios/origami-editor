namespace CreasePattern

type Frame =
    { unit: Fold.LengthUnit
      creasePattern: CreasePattern }

module Frame =
    
    (* Builders *)
    
    let create : Frame =
        { unit = Fold.Unitless
          creasePattern = CreasePattern.create }
        
    let empty : Frame =
        { unit = Fold.Unitless
          creasePattern = CreasePattern.empty }

    (* Modifiers *)

    let setUnit unit frame = { frame with unit = unit }

    let fromFoldFrame (foldFrame: Fold.Frame) : Frame =
        { create with
              unit = foldFrame.unit
              creasePattern = CreasePattern.fromFoldValues foldFrame.vertices foldFrame.edges foldFrame.faces }
