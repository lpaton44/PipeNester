namespace App

open Browser.Types
open Fable.Core
open Thoth.Fetch
open Thoth.Json
open Feliz
open Feliz.Router
open Fable.FontAwesome.Free
open Fable.FontAwesome

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

type Components =

    [<ReactComponent>]
    static member Header() =
        Html.header [
            prop.className "h-30 bg-teal-600 text-white "
            prop.children [
                Html.div [
                    prop.className "flex items-center"
                    prop.children [
                       Html.div [
                           prop.className "color-white mt-5 mr-2  ml-10"
                           prop.children [
                              Fa.i [
                                  Fa.Size Fa.ISize.Fa2x
                                  Fa.Solid.Truck ] []
                           ]
                       ]

                       Html.h1 [
                           prop.className "mt-5 text-3xl font-semibold"
                           prop.text "PipeNester"
                       ]
                    ]
                ]

                Html.div [
                    prop.className "items-bottom justify-end flex mr-10 mb-5"
                    prop.children[
                        Html.nav [
                            prop.className "mr-10 p-5 link text-white font-semibold text-xl"
                            prop.children [
                                Html.a [
                                    prop.onClick (fun _ -> Router.navigate "allOrders")
                                    prop.className "mr-10 hover:text-gray-500 cursor-pointer"
                                    prop.text "All Orders"
                                ]
                                Html.a [
                                    prop.onClick (fun _ -> Router.navigate "newOrder")
                                    prop.className "hover:text-gray-500 cursor-pointer"
                                    prop.text "Add Order"
                                ]
                            ]
                        ]

                    ]

                ]

            ]
        ]


    [<ReactComponent>]
    static member OrderForm () =

        let (orderItems, setOrderItems) = React.useState []
        let (addingItem, setAddingItem) = React.useState false
        let (editingItem, setEditingItem) = React.useState false

        let mutable itemDiameterRef = React.useRef None
        let mutable itemQuantityRef = React.useRef None
        let mutable orderNumberRef = React.useRef None
        let mutable orderRef = React.useRef List.empty

        let addNewItemHandler () =
            if (itemDiameterRef.current = None) || (itemQuantityRef.current = None) then
                ()
            else
                let newItem = [itemDiameterRef.current, itemQuantityRef.current]
                              |> List.map unbox<HTMLInputElement>
                              |> List.map (fun i -> i.value)

                let newItems = newItem@orderItems

                orderRef.current <- newItems
                itemDiameterRef.current <- None
                itemQuantityRef.current <- None
                setOrderItems newItems
                setAddingItem false

        let submitHandler () =
            let newOrderNumber = unbox<HTMLInputElement> orderNumberRef.current
            let newOrder = {
                orderNumber = newOrderNumber.value |> int
                order = orderItems |> String.concat "; "
            }

            promise {
                let url = sprintf "https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/pipes.json"
                return! Fetch.post (url, newOrder, caseStrategy = CamelCase)
            }


        let orderString =
            orderItems |> List.fold (fun acc i -> $"({i}) {acc}") ""

        Html.div [
            prop.className "flex justify-items-center"

            prop.children [
                Html.form [
                    prop.className "p-10 mx-10 flex w-full justify-items-center align-center"

                    prop.children [
                       Html.div [
                           prop.className "w-full"
                           (*prop.style [
                               style.borderColor color.red
                               style.borderWidth 1
                           ]*)
                           prop.children [
                               Html.div [
                                   prop.className "mx-5 mb-5 w-full justify-items-center align-center"
                                   prop.children [
                                       Html.label [
                                            prop.className "text-left text-2xl p-5 font-semibold"
                                            prop.text "Order Number:"
                                       ]
                                   ]
                               ]
                               Html.div [

                                   prop.children [
                                       Html.input [
                                          prop.id "orderNumber"
                                          prop.name "orderNumber"
                                          prop.type'.number
                                          prop.defaultValue ""
                                          prop.style [
                                              style.borderRadius 10
                                              style.borderColor color.gray
                                              style.borderWidth 1
                                          ]
                                          prop.className "ml-10 w-64 p-1"
                                          prop.ref orderNumberRef
                                      ]
                                   ]
                               ]
                               Html.div [
                                   prop.className "mt-10 mx-5 w-full"
                                   prop.children [
                                       Html.label [
                                            prop.className "text-left text-2xl p-5 font-semibold"
                                            prop.text $"Order Details:"
                                       ]
                                   ]
                               ]
                               Html.div [
                                   prop.className "mt-5"
                                   prop.children [
                                       Html.div [
                                           prop.children [
                                               Html.label [
                                                   prop.className "ml-20 mt-5 text-xl"
                                                   prop.text $"Current Order: {orderString}"
                                               ]
                                           ]
                                       ]
                                       Html.div [
                                           prop.className "mt-7 ml-10"
                                           prop.children [
                                               if (addingItem) then
                                                   Html.div [
                                                       prop.className "ml-5 mb-3"
                                                       prop.children [
                                                           Html.label [
                                                              prop.className "p-5 ml-1 text-large"
                                                              prop.text $"Diameter:"
                                                            ]
                                                           Html.input [
                                                              prop.id "diameter"
                                                              prop.name "diameter"
                                                              prop.defaultValue ""
                                                              prop.type'.number
                                                              prop.style [
                                                                  style.borderRadius 10
                                                                  style.borderColor color.gray
                                                                  style.borderWidth 1
                                                              ]
                                                              prop.className "ml-10 p-1"
                                                              prop.ref itemDiameterRef
                                                           ]
                                                       ]
                                                   ]

                                                   Html.div [
                                                       prop.className "ml-5 p-2 mb-10 "
                                                       prop.children [
                                                          Html.label [
                                                              prop.className "p-5 text-large"
                                                              prop.text $"Quantity:"
                                                          ]
                                                          Html.input [
                                                            prop.id "quantity"
                                                            prop.name "quantity"
                                                            prop.defaultValue ""
                                                            prop.type'.number
                                                            prop.style [
                                                                style.borderRadius 10
                                                                style.borderColor color.gray
                                                                style.borderWidth 1
                                                            ]
                                                            prop.className "ml-10 p-1"
                                                            prop.ref itemQuantityRef
                                                         ]
                                                       ]
                                                   ]


                                               ]
                                           ]

                                       ]

                                    ]
                               Html.div [
                                   prop.className "flex justify-end w-3/4 mr-20"
                                   prop.children [
                                       if (not addingItem) then
                                           Html.button [
                                               prop.className "cursor-pointer bg-teal-500 rounded p-3 text-large text-black hover:bg-teal-600"
                                               prop.text "Add Item to Order"
                                               prop.onClick (fun _ -> setAddingItem true)
                                           ]
                                   ]
                               ]
                               Html.div [
                                   prop.className "flex w-3/4 items-center justify-end mr-20 gap-5 mt-10 ml-10"
                                   prop.children [
                                       if addingItem then
                                           Html.button [
                                               prop.className "cursor-pointer bg-green-500 rounded p-3 px-4 text-large text-black hover:bg-green-600"
                                               prop.text "Add Item"
                                               prop.onClick (fun _ -> addNewItemHandler())
                                           ]

                                           Html.button [
                                                    prop.className "cursor-pointer bg-gray-300 rounded p-3 px-6 text-large text-black hover:bg-red-300"
                                                    prop.text "Close"
                                                    prop.onClick (fun _ -> setAddingItem false)

                                           ]

                                       else
                                           Html.button [
                                               prop.className "cursor-pointer bg-green-500 rounded p-3 text-large text-black hover:bg-green-600"
                                               prop.text "Save Order"
                                           ]
                                           Html.button [
                                                    prop.className "cursor-pointer bg-gray-300 rounded p-3 text-large text-black hover:bg-red-300"
                                                    prop.text "Cancel Order"
                                                    prop.onClick (fun _ -> Router.navigate("allOrders"))
                                           ]
                                   ]
                               ]
                           ]
                       ]
                    ]
                ]
            ]
        ]





    [<ReactComponent>]
    static member OrderList() =

        let orders, setOrders = React.useState Array.empty

        let getDataA () :JS.Promise<Order []> = // Setup API call inside component to keep all related calls together with the component
            promise {
                let url = sprintf "https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/pipes.json"
                return! Fetch.get (url, caseStrategy = CamelCase)
            }


        React.useEffectOnce (fun () ->
            let d = getDataA() // get the data from the API call (Promise)
            d.``then``setOrders// On promise return set order state to returned result
            |> ignore // ignore promise after state has been set as React.useEffect always needs to return a unit
        )


        let getListItems (orderString : string) =
            let removeSquareBrackets = orderString.Trim('[', ']').Trim()
            let splitItems = removeSquareBrackets.Split(';')
                             |> Array.toList
                             //|> List.map (fun i -> i.Trim('(', ')'))

            Html.ul [

                prop.children (splitItems
                |> List.map(fun item ->
                    Html.li [
                        prop.text $"{item}"
                        prop.onClick (fun _ -> Router.navigate(""))
                    ]
                ))
            ]

        Html.div [
            prop.className "px-20"
            prop.children [
                Html.h1 [
                    prop.className "text-center mb-10 text-xl underline font-bold"
                    prop.text "All Orders"
                ]
                Html.table [
                    prop.className "min-w-full py-2 text-center table-auto text-xl"
                    prop.children [
                        Html.thead [
                        prop.className "border-b font-semibold dark:border-neutral-500"
                        prop.children [
                            Html.tr [
                                Html.th [ prop.text "Order Number" ]
                                Html.th [ prop.text "Order Summary" ]
                                ]
                            ]
                        ]
                        Html.tbody [
                           prop.children (
                               orders
                               |> Array.toList
                               |> List.mapi (fun index orderItem ->
                                   Html.tr [
                                       prop.key orderItem.orderNumber
                                       prop.className (if index % 2 = 0 then "bg-gray-100" else "bg-white border-b dark:border-neutral-500")
                                       prop.children [
                                           Html.td [
                                               prop.text $"{orderItem.orderNumber}"
                                           ]
                                           Html.td [
                                               prop.children (getListItems orderItem.order)

                                           ]
                                       ]
                                   ]
                               )
                           )
                       ]
                    ]
                ]
            ]
        ]



    /// <summary>
    /// A React component that uses Feliz.Router
    /// to determine what to show based on the current URL
    /// </summary>
    [<ReactComponent>]
    static member Router() =
        let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
        React.router [
            router.onUrlChanged updateUrl
            router.children [
                Components.Header()

                match currentUrl with
                | [ "newOrder" ] -> Components.OrderForm()
                | ["allOrders"] -> Components.OrderList()
                | otherwise -> Html.h1 "Not found"
            ]
        ]
