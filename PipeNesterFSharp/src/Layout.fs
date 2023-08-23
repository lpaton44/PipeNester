open Feliz
open Fable.React

let layout =
   Html.div [
      props.children [
         Components.Header
         Html.slot
      ]

   ]
