namespace Nester
open Thoth.Json
open LookUp

type Order =
        {
          orderNumber: int
          order: string
        }
        static member Decoder =
            Decode.object (fun get ->
                  {
                    orderNumber = get.Required.Field "orderNumber" Decode.int
                    order = get.Required.Field "order" Decode.string
                  }
             )

        static member Encoder (order : Order) =
            Encode.object [
                "orderNumber", Encode.int order.orderNumber
                "order", Encode.string order.order
            ]


module NesterHelpers =

    let removeBracketsFromPair (pair: string List) =
        pair
        |> List.map (fun i ->
            i.Trim([|'('; ')'|]).Trim())


    let getDetails (orderString : string) =
       let orderItems =
           orderString.Trim([|'['; ']'; ' '|]).Split(';')
           |> Array.toList
       let itemDetails =
           orderItems
               |> List.map (fun item -> item.Trim().Split(',') |> Array.toList)
               |> List.map removeBracketsFromPair
       itemDetails

    let convertOrderListToString orderL =
        orderL
        |> List.map ( fun (code, quantity) ->
            $"({code}, {quantity})")
        |> String.concat ";"


    let checkIfFound order =
        match order with
        | Some value -> $"{value.orderNumber}"
        | None -> "No order found."

    let convertOrderToPipeList order =
       match order with
       | Some validOrder ->
            let details = getDetails validOrder.order
            let tuples =
                details
                |> List.map (fun item -> ( item[0], item[1] |> int))
            [
               for p, n in tuples do
                  let (diameter, socket) = itemCodeLookUpTable |> Map.find p
                  for _ in 1..n do
                     {
                         Diameter = PipeDiameter diameter
                         ParentIdO = None
                         ProductCode = p
                         Socket = socket
                     }
            ]
       | None -> []
