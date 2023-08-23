namespace App

open Thoth.Fetch
open Thoth.Json
open Feliz
open Feliz.Router
open Fable.FontAwesome.Free
open Fable.FontAwesome

type Order =
        {
          id : string
          orderNumber : string
          order : string
        }

        static member Decoder =
            Decode.object (fun get ->
                {
                  id = get.Required.Field "id" Decode.string
                  orderNumber =  get.Required.Field "orderNumber" Decode.string
                  order = get.Required.Field "order" Decode.string
                }
            )

        static member Encoder (order : Order) =
            Encode.object [
                "id", Encode.string order.id
                "orderNumber", Encode.string order.orderNumber
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
                            prop.className "mr-20 link text-white text-xl hover:text-gray-500"
                            prop.text "All Orders"
                        ]
                        Html.nav [
                            prop.className "mr-20 link text-white text-xl hover:text-gray-500"
                            prop.text "New Order"
                        ]
                    ]

                ]

            ]
        ]


    [<ReactComponent>]
    static member OrderForm () =

        let orders =
            promise {
                let url = "https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/pipes.json"
                return! Fetch.tryGet(url, decoder = Order.Decoder)
                (*match response with
                | Ok data -> return data
                | Error error -> failwith <| printfn $"Error: {error.message}"*)
            }




        let (orderItems, setOrderItems) = React.useState([])
        let (addingItem, setAddingItem) = React.useState(false)
        let (editingItem, setEditingItem) = React.useState(false)

        let mutable itemDiameterRef = React.useRef("")
        let mutable itemQuantityRef = React.useRef("")
        let mutable orderNumberRef = React.useRef(None)
        let mutable orderRef = React.useRef([])

        let addNewItemHandler () =
            if (itemDiameterRef.current = "") || (itemQuantityRef.current = "") then
                ()
            else
                let newItem = [itemDiameterRef.current, itemQuantityRef.current]
                let newItems = newItem@orderItems

                orderRef.current <-  newItems
                itemDiameterRef.current <- ""
                itemQuantityRef.current <- ""
                setOrderItems newItems
                setAddingItem false

        Html.div [
            prop.className "items-center justify-center"

            prop.children [
                Html.form [
                    prop.children [
                       Html.p [
                           prop.children [
                               Html.label [
                                    prop.className "mt-20 text-xl p-5 font-semibold "
                                    prop.text "Order Number:"
                                ]
                               Html.input [
                                   prop.id "orderNumber"
                                   prop.name "orderNumber"
                                   prop.defaultValue ""
                                   prop.type'.text
                                   prop.style [
                                       style.borderRadius 10
                                       style.borderColor color.gray
                                       style.borderWidth 1
                                   ]
                                   prop.className "w-7/10 mb-10"
                               ]
                               Html.label [
                                   prop.className "mt-20 text-xl p-5 font-semibold"
                                   prop.text $"Order Details:"
                               ]
                               Html.input [
                                   prop.id "order"
                                   prop.name "order"
                                   prop.defaultValue ""
                                   prop.type'.text
                                   prop.style [
                                       style.borderRadius 10
                                       style.borderColor color.gray
                                       style.borderWidth 1
                                   ]
                                   prop.className "w-7/10 ml-2"
                               ]

                           ]
                       ]

                    ]
                ]
            ]
        ]




    (*[<ReactComponent>]
    static member OrderList() =
        let orders = [

        ]
        let listItems =
            orders |> List.map
                          (fun item ->
                             let order = item.order
                             Html.li [
                                 prop.text $"{order}"
                             ])
        Html.div [
            prop.children [
                Html.h1 [
                    prop.className "text-xl font-bold"
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
                            Html.tbody [
                                prop.children (
                                    orders
                                    |> List.mapi (fun index orderItem ->
                                        Html.tr [
                                            prop.key orderItem.id
                                            prop.className (if index % 2 = 0 then "bg-gray-100" else "bg-white border-b dark:border-neutral-500")
                                            prop.children [

                                            ]
                                        ]

                                    ))
                            ]
                        ]
                ]
            ]

        ]
    ]
    ]
*)
    /// <summary>
    /// The simplest possible React component.
    /// Shows a header with the text Hello World
    /// </summary>
    [<ReactComponent>]
    static member HelloWorld() = Html.h1 "Hello World"

    /// <summary>
    /// A stateful React component that maintains a counter
    /// </summary>
    [<ReactComponent>]
    static member Counter() =
        let (count, setCount) = React.useState(0)
        Html.div [
            Html.h1 count
            Html.button [
                prop.className "bg-orange-500 rounded-full p-2"
                prop.onClick (fun _ -> setCount(count + 1))
                prop.text "Increment"
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
                | otherwise -> Html.h1 "Not found"
            ]
        ]
