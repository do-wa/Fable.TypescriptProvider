// ts2fable 0.0.0
module rec f7
open System
open Fable.Core
open Fable.Core.JS

type SyntheticEvent = React.SyntheticEvent

type [<AllowNullLiteral>] EventHandler<'E when 'E :> SyntheticEvent<obj option>> =
    abstract bivarianceHack: ``event``: 'E -> unit
