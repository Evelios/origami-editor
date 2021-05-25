namespace CreasePattern

type Frame =
    { unit: Fold.LengthUnit
      creasePattern: CreasePattern }

module Frame =
    let create : Frame =
        { unit = Fold.Unitless
          creasePattern = CreasePattern.create }

   (* Modifiers *) 
    
    let setUnit unit frame = { frame with unit = unit }

    let fromFoldFrame (foldFrame: Fold.Frame) : Frame = { create with unit = foldFrame.unit }
