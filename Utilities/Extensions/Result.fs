module FSharp.Core.Extra.Result

/// <summary>If the input value is an Ok leaves it unchanged, otherwise maps the Error value and flattens the resulting nested Result.</summary>
/// <param name="binder">A function that takes the error and transforms it into a result.</param>
/// <param name="source">The source input value.</param>
/// <returns>A result of the output type of the binder.</returns>
let inline bindError (binder: 'Error->Result<'T,'Error2>) (source: Result<'T,'Error>) = match source with Ok v -> Ok v | Error e -> binder e