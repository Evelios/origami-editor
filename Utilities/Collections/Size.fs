namespace Utilities.Collections

type Size = { width: float; height: float }

module Size =

    let create width height = { width = width; height = height }

    let scale scale size =
        { width = size.width * scale
          height = size.height * scale }

    let normalizeBelowOne size = scale (1. / max size.width size.height) size
    
    let withMaxSize size =
        normalizeBelowOne >> scale size
        
